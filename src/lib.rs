use chrono::{DateTime, TimeDelta, Utc};
use ext_php_rs::{
    ffi::{_zval_struct, ZEND_INTERNAL_FUNCTION},
    flags::DataType,
    prelude::*,
    types::Zval,
    zend::{ExecuteData, ProcessGlobals, SapiGlobals},
};
use serde::Serialize;
use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    ffi::CStr,
    sync::{
        atomic::{AtomicU64, Ordering},
        OnceLock,
    },
    time::{Duration, Instant},
};
use tokio::{io::AsyncWriteExt as _, net::UnixStream, runtime::Runtime};

mod util;

// Global tokio runtime - reused across requests
static TOKIO_RT: OnceLock<Runtime> = OnceLock::new();

#[derive(Debug, Clone, Serialize)]
struct ProfileData {
    start_time: DateTime<Utc>,
    end_time: DateTime<Utc>,
    callstack: Vec<CallType>,
    duration: TimeDelta,
    url: Option<String>,
    method: Option<String>,
    response_code: i32,
    server: HashMap<String, RequestValue>,
    request: HashMap<String, RequestValue>,
    cookies: HashMap<String, RequestValue>,
    get: HashMap<String, RequestValue>,
    post: HashMap<String, RequestValue>,
}

#[derive(Debug, Clone)]
struct ActiveCall {
    stack_id: StackId,
    start_time: Instant,
}

#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
enum RequestValue {
    String(String),
    Bool(bool),
    Int(i64),
    Array(HashMap<String, RequestValue>),
}

#[derive(Debug, Clone)]
struct Request {
    start_time: DateTime<Utc>,
    method: Option<String>,
    url: Option<String>,
    server: HashMap<String, RequestValue>,
    request: HashMap<String, RequestValue>,
    cookies: HashMap<String, RequestValue>,
    get: HashMap<String, RequestValue>,
    post: HashMap<String, RequestValue>,
}

thread_local! {
    static ACTIVE_CALLS: RefCell<VecDeque<ActiveCall>> = RefCell::new(VecDeque::with_capacity(1024));
    static FUNCTION_CALLS: RefCell<Vec<CallType>> = RefCell::new(Vec::new());
    static REQUEST: RefCell<Option<Request>> = RefCell::new(None);
}

static STACK_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

type StackId = u64;

#[derive(Debug, Clone, Serialize)]
struct FunctionCall {
    name: &'static str,
    stack_id: u64,
    internal: bool,
    duration: Duration,
    parent_stack_id: u64,
    filename: Option<&'static str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    namespace: Option<&'static str>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    arguments: Vec<ArgumentType>,
}

#[derive(Debug, Clone, Serialize)]
struct MethodCall {
    name: &'static str,
    internal: bool,
    stack_id: u64,
    classname: &'static str,
    duration: Duration,
    parent_stack_id: u64,
    filename: Option<&'static str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    namespace: Option<&'static str>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    arguments: Vec<ArgumentType>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "lowercase", tag = "type")]
enum CallType {
    Function(FunctionCall),
    Method(MethodCall),
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "lowercase", tag = "type", content = "value")]
enum ArgumentType {
    Array(String),
    Bool(bool),
    Callable(String),
    Float(f64),
    Int(i64),
    Iterable(String),
    Mixed(String),
    Null,
    Object(String),
    String(String),
    Undefined,
    Void,
}

#[no_mangle]
pub extern "C" fn startup(_type: i32, _module: i32) -> i32 {
    register_observer();

    0
}

#[no_mangle]
pub extern "C" fn shutdown(_type: i32, _module: i32) -> i32 {
    0
}

#[no_mangle]
pub extern "C" fn request_startup(_type: i32, _module: i32) -> i32 {
    fn map_value_to_request_value(value: &_zval_struct) -> Option<RequestValue> {
        if let Some(value) = value.string() {
            Some(RequestValue::String(value))
        } else if let Some(value) = value.bool() {
            Some(RequestValue::Bool(value))
        } else if let Some(value) = value.long() {
            Some(RequestValue::Int(value))
        } else if let Some(value) = value.array() {
            Some(RequestValue::Array(
                value
                    .iter()
                    .filter_map(|(key, value)| {
                        Some((key.to_string(), map_value_to_request_value(value)?))
                    })
                    .collect(),
            ))
        } else {
            return None;
        }
    }

    REQUEST.with_borrow_mut(|request| {
        *request = Some(Request {
            start_time: chrono::Utc::now(),
            method: ProcessGlobals::get()
                .http_server_vars()
                .and_then(|vars| vars.get("REQUEST_METHOD"))
                .and_then(|method| (*method).string()),
            url: ProcessGlobals::get()
                .http_server_vars()
                .and_then(|vars| {
                    Some((
                        vars.get("SERVER_PROTOCOL")?.str()?,
                        vars.get("HTTP_HOST")?.str()?,
                        vars.get("REQUEST_URI")?.str()?,
                    ))
                })
                .map(|(protocol, host, uri)| {
                    format!(
                        "{}{host}{uri}",
                        if protocol.starts_with("HTTPS") {
                            "https://"
                        } else {
                            "http://"
                        }
                    )
                }),
            server: ProcessGlobals::get()
                .http_server_vars()
                .map(|vars| {
                    vars.iter()
                        .filter_map(|(key, value)| {
                            Some((key.to_string(), map_value_to_request_value(value)?))
                        })
                        .collect()
                })
                .unwrap_or_default(),
            request: ProcessGlobals::get()
                .http_request_vars()
                .map(|vars| {
                    vars.iter()
                        .filter_map(|(key, value)| {
                            Some((key.to_string(), map_value_to_request_value(value)?))
                        })
                        .collect()
                })
                .unwrap_or_default(),
            cookies: ProcessGlobals::get()
                .http_cookie_vars()
                .iter()
                .filter_map(|(key, value)| {
                    Some((key.to_string(), map_value_to_request_value(value)?))
                })
                .collect(),
            get: ProcessGlobals::get()
                .http_get_vars()
                .iter()
                .filter_map(|(key, value)| {
                    Some((key.to_string(), map_value_to_request_value(value)?))
                })
                .collect(),
            post: ProcessGlobals::get()
                .http_post_vars()
                .iter()
                .filter_map(|(key, value)| {
                    Some((key.to_string(), map_value_to_request_value(value)?))
                })
                .collect(),
        });
    });

    0
}

#[no_mangle]
pub extern "C" fn request_shutdown(_type: i32, _module: i32) -> i32 {
    let Some(request) = REQUEST.take() else {
        return 0;
    };

    let callstack = FUNCTION_CALLS.with(|calls| std::mem::take(&mut *calls.borrow_mut()));

    let end_time = chrono::Utc::now();
    let profile_data = ProfileData {
        start_time: request.start_time,
        end_time,
        callstack,
        duration: end_time.signed_duration_since(request.start_time),
        method: request.method,
        url: request.url,
        cookies: request.cookies,
        get: request.get,
        post: request.post,
        request: request.request,
        server: request.server,
        response_code: SapiGlobals::get().sapi_headers.http_response_code,
    };

    std::thread::spawn(move || {
        let rt = TOKIO_RT.get_or_init(|| {
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("Failed to create Tokio runtime")
        });

        rt.block_on(async {
            if let Ok(json) = serde_json::to_string(&profile_data) {
                if let Ok(mut stream) = UnixStream::connect("/tmp/php-tracking-daemon.sock").await {
                    let _ = stream.write_all(json.as_bytes()).await;
                    let _ = stream.flush().await;
                }
            }
        });
    });

    0
}

//
// observer
//

// Define function pointer types for Zend observer handlers
type ZendObserverFcallBeginHandler = extern "C" fn(*mut ExecuteData);
type ZendObserverFcallEndHandler =
    extern "C" fn(*mut ExecuteData, *mut ext_php_rs::ffi::zend_value);

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct zend_observer_fcall_handlers {
    pub begin: Option<ZendObserverFcallBeginHandler>,
    pub end: Option<ZendObserverFcallEndHandler>,
}

// External function bindings for the Zend API
extern "C" {
    pub fn zend_observer_fcall_register(
        callback: extern "C" fn(*mut ExecuteData) -> zend_observer_fcall_handlers,
    );
}

#[no_mangle]
extern "C" fn observer_begin(_: *mut ExecuteData) {
    ACTIVE_CALLS.with(|calls| {
        let mut calls = calls.borrow_mut();
        calls.push_back(ActiveCall {
            stack_id: STACK_ID_COUNTER.fetch_add(1, Ordering::Relaxed),
            start_time: Instant::now(),
        });
    });
}

#[no_mangle]
extern "C" fn observer_end(execute_data: *mut ExecuteData, _: *mut ext_php_rs::ffi::zend_value) {
    let Some((active_call, parent_stack_id)) = ACTIVE_CALLS.with(|calls| {
        let mut calls = calls.borrow_mut();
        if let Some(active_call) = calls.pop_back() {
            let parent_id = calls
                .back()
                .map(|active_call| active_call.stack_id)
                .unwrap_or(1);
            Some((active_call, parent_id))
        } else {
            None
        }
    }) else {
        return;
    };

    let elapsed = active_call.start_time.elapsed();

    let Some(execute_data_ref) = (unsafe { execute_data.as_ref() }) else {
        return;
    };

    let Some(func) = execute_data_ref.function() else {
        return;
    };

    let Some(name_ptr) = (unsafe { func.internal_function.function_name.as_ref() }) else {
        return;
    };

    let filename = unsafe {
        maybe!({
            let caller = execute_data_ref.prev_execute_data.as_ref()?;
            let op_array = caller.func.as_ref().map(|func| func.op_array)?;
            let filename = op_array.filename.as_ref()?;
            CStr::from_ptr(filename.val.as_ptr()).to_str().ok()
        })
    };

    let internal = unsafe { func.type_ } as u32 == ZEND_INTERNAL_FUNCTION;

    let Some(function_name) = (unsafe { CStr::from_ptr(name_ptr.val.as_ptr()).to_str().ok() })
    else {
        return;
    };

    let arguments = unsafe { get_function_arguments(execute_data) };

    let scope = unsafe { func.internal_function.scope.as_ref() }.and_then(|scope| scope.name());

    let call = if let Some(scope) = scope {
        let parts: Vec<&str> = scope.rsplitn(2, '\\').collect();
        let (classname, namespace) = match parts.as_slice() {
            [classname, namespace] => (*classname, Some(*namespace)),
            [classname] => (*classname, None),
            _ => return,
        };

        CallType::Method(MethodCall {
            classname,
            namespace,
            arguments,
            internal,
            filename,
            parent_stack_id,
            duration: elapsed,
            name: function_name,
            stack_id: active_call.stack_id,
        })
    } else {
        CallType::Function(FunctionCall {
            arguments,
            filename,
            internal,
            namespace: None,
            parent_stack_id,
            duration: elapsed,
            name: function_name,
            stack_id: active_call.stack_id,
        })
    };

    FUNCTION_CALLS.with(|calls| calls.borrow_mut().push(call));
}

#[no_mangle]
extern "C" fn observer_callback(_execute_data: *mut ExecuteData) -> zend_observer_fcall_handlers {
    zend_observer_fcall_handlers {
        begin: Some(observer_begin),
        end: Some(observer_end),
    }
}

#[no_mangle]
pub extern "C" fn register_observer() {
    unsafe {
        zend_observer_fcall_register(observer_callback);
    }
}

/// Initialize the module and setup function tracking
#[php_module]
pub fn get_module(module: ModuleBuilder) -> ModuleBuilder {
    module
        .startup_function(startup)
        .shutdown_function(shutdown)
        .request_startup_function(request_startup)
        .request_shutdown_function(request_shutdown)
}

unsafe fn get_function_arguments(execute_data: *mut ExecuteData) -> Vec<ArgumentType> {
    let mut args: Vec<ArgumentType> = Vec::new();

    let actual_num_args = (*execute_data).This.u2.num_args as usize;
    let stack_top = execute_data.offset(1) as *mut Zval;

    if stack_top.is_null() {
        return args;
    }

    for i in 0..actual_num_args {
        let arg_ptr = stack_top.offset(i as isize);

        if arg_ptr.is_null() {
            continue;
        }

        let Some(arg) = arg_ptr.as_ref() else {
            continue;
        };

        match arg.get_type() {
            DataType::String => {
                if let Some(s) = arg.string() {
                    args.push(ArgumentType::String(s));
                }
            }
            DataType::Long => {
                if let Some(i) = arg.long() {
                    args.push(ArgumentType::Int(i));
                }
            }
            DataType::Double => {
                if let Some(f) = arg.double() {
                    args.push(ArgumentType::Float(f));
                }
            }
            DataType::True | DataType::False | DataType::Bool => {
                if let Some(b) = arg.bool() {
                    args.push(ArgumentType::Bool(b));
                }
            }
            DataType::Iterable => {
                if let Some(i) = arg.iterable() {
                    args.push(ArgumentType::Iterable(format!("{:?}", i)));
                }
            }
            DataType::Array => {
                if let Some(a) = arg.array() {
                    args.push(ArgumentType::Array(format!("{:?}", a)));
                }
            }
            DataType::Undef => args.push(ArgumentType::Undefined),
            DataType::Null => args.push(ArgumentType::Null),
            DataType::Object(_) => {
                if let Some(o) = arg.object() {
                    args.push(ArgumentType::Object(format!("{:?}", o)));
                }
            }
            DataType::Resource => {}
            DataType::Reference => {}
            DataType::Callable => {
                if let Some(c) = arg.callable() {
                    args.push(ArgumentType::Callable(format!("{:?}", c)));
                }
            }
            DataType::ConstantExpression => {}
            DataType::Void => {
                args.push(ArgumentType::Void);
            }
            DataType::Mixed => {
                args.push(ArgumentType::Mixed(format!("{:?}", arg)));
            }
            DataType::Ptr => {}
            DataType::Indirect => {}
        }
    }

    args
}

#[allow(dead_code)]
fn print_call_tree(calls: &[CallType], parent_id: u64, level: usize) {
    for call in calls.iter().filter(|c| match c {
        CallType::Function(c) => c.parent_stack_id == parent_id,
        CallType::Method(c) => c.parent_stack_id == parent_id,
    }) {
        match call {
            CallType::Function(call) => {
                println!(
                    "{:indent$}└─ {}::{} ({}ms) [{}]",
                    "",
                    call.namespace.clone().unwrap_or_default(),
                    call.name,
                    call.duration.as_millis(),
                    call.filename.unwrap_or_default(),
                    indent = level * 2
                );
                print_call_tree(calls, call.stack_id, level + 1);
            }
            CallType::Method(call) => {
                println!(
                    "{:indent$}└─ {}{}::{} ({}ms) [{}]",
                    "",
                    call.namespace.clone().unwrap_or_default(),
                    call.classname,
                    call.name,
                    call.duration.as_millis(),
                    call.filename.unwrap_or_default(),
                    indent = level * 2
                );
                print_call_tree(calls, call.stack_id, level + 1);
            }
        }
    }
}
