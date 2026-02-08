use chrono::{DateTime, TimeDelta, Utc};
use ext_php_rs::{
    ffi::{_zval_struct, sapi_headers_struct, zend_ce_throwable, ZEND_INTERNAL_FUNCTION},
    flags::{DataType, IniEntryPermission},
    prelude::*,
    types::Zval,
    zend::{ClassEntry, ExecuteData, ExecutorGlobals, IniEntryDef, ProcessGlobals, SapiGlobals},
};
use serde::Serialize;
use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    ffi::CStr,
    io::Write as _,
    os::fd::AsRawFd as _,
    sync::atomic::{AtomicU64, Ordering},
    time::{Duration, Instant},
};

mod util;

const DAEMON_SOCKET_PATH: &str = "/tmp/php-tracking-daemon.sock";
/// 2 MB send buffer — large enough for most profile payloads to be written in
/// a single burst without blocking on the daemon to drain the receive side.
const SOCKET_SEND_BUF_SIZE: libc::c_int = 2 * 1024 * 1024;

#[derive(Debug, Serialize)]
struct ProfileData {
    application_id: String,
    start_time: DateTime<Utc>,
    end_time: DateTime<Utc>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    callstack: Vec<CallType>,
    duration: TimeDelta,
    memory_usage: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    method: Option<String>,
    response_code: i32,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    response_headers: HashMap<String, Option<String>>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    server: HashMap<String, RequestValue>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    request: HashMap<String, RequestValue>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    cookies: HashMap<String, RequestValue>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    get: HashMap<String, RequestValue>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    post: HashMap<String, RequestValue>,
}

#[derive(Debug)]
struct ActiveCall {
    stack_id: StackId,
    start_time: Instant,
    start_memory: usize,
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
enum RequestValue {
    String(String),
    Bool(bool),
    Int(i64),
    Float(f64),
    Array(HashMap<String, RequestValue>),
}

#[derive(Debug)]
struct Request {
    start_time: DateTime<Utc>,
    start_memory: usize,
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
    static DAEMON_CONNECTION: RefCell<Option<(u32, std::os::unix::net::UnixStream)>> = RefCell::new(None);
}

static STACK_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

type StackId = u64;

#[derive(Debug, Serialize)]
struct FunctionCall {
    name: String,
    stack_id: u64,
    internal: bool,
    duration: Duration,
    parent_stack_id: u64,
    memory_usage: i64,
    #[serde(skip_serializing_if = "Option::is_none")]
    filename: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    namespace: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    arguments: Vec<ArgumentType>,
    bubbles_exception: bool,
}

#[derive(Debug, Serialize)]
struct MethodCall {
    name: String,
    internal: bool,
    stack_id: u64,
    classname: String,
    duration: Duration,
    parent_stack_id: u64,
    memory_usage: i64,
    #[serde(skip_serializing_if = "Option::is_none")]
    filename: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    namespace: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    arguments: Vec<ArgumentType>,
    bubbles_exception: bool,
    is_exception: bool,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "lowercase", tag = "type")]
enum CallType {
    Function(FunctionCall),
    Method(MethodCall),
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
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
pub extern "C" fn startup(_type: i32, module_number: i32) -> i32 {
    register_observer();

    let ini_entries = vec![IniEntryDef::new(
        "php_tracking.application_id".to_owned(),
        "".to_owned(),
        &IniEntryPermission::System,
    )];
    IniEntryDef::register(ini_entries, module_number);

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
        } else if let Some(value) = value.double() {
            Some(RequestValue::Float(value))
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
            start_memory: unsafe { zend_memory_usage(false) },
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

    let ini_values = ExecutorGlobals::get().ini_values();
    let Some(application_id) = ini_values
        .get("php_tracking.application_id")
        .and_then(|v| v.as_ref())
        .filter(|v| !v.is_empty())
    else {
        eprintln!(
            "Failed to get valid application id from php ini file `php_tracking.application_id`"
        );
        return 0;
    };

    let callstack = FUNCTION_CALLS.with(|calls| std::mem::take(&mut *calls.borrow_mut()));

    let end_time = chrono::Utc::now();
    let end_memory = unsafe { zend_memory_usage(false) };
    let sapi_headers = SapiGlobals::get().sapi_headers;
    let profile_data = ProfileData {
        application_id: application_id.clone(),
        start_time: request.start_time,
        end_time,
        callstack,
        duration: end_time.signed_duration_since(request.start_time),
        memory_usage: end_memory.saturating_sub(request.start_memory),
        method: request.method,
        url: request.url,
        cookies: request.cookies,
        get: request.get,
        post: request.post,
        request: request.request,
        server: request.server,
        response_code: sapi_headers.http_response_code,
        response_headers: response_headers_from_sapi_headers(sapi_headers),
    };

    send_to_daemon(profile_data);

    0
}

/// Send profile data directly to the daemon over a persistent Unix socket.
///
/// The connection is kept in thread-local storage and reused across requests
/// within the same PHP-FPM worker. With a 2 MB `SO_SNDBUF`, most messages
/// land in kernel space in a single `write` syscall without blocking.
///
/// Uses PID tracking to safely re-create the connection after PHP-FPM forks.
fn send_to_daemon(data: ProfileData) {
    let mut json = match serde_json::to_vec(&data) {
        Ok(j) => j,
        Err(_) => return,
    };

    // Append newline so we can write everything in a single `write_all` call.
    // This avoids partial-write corruption where the JSON bytes are flushed
    // but the newline delimiter is not.
    json.push(b'\n');

    DAEMON_CONNECTION.with(|cell| {
        let mut slot = cell.borrow_mut();
        let pid = std::process::id();

        // After a fork the inherited fd is stale — reconnect.
        let needs_init = match *slot {
            Some((stored_pid, _)) => stored_pid != pid,
            None => true,
        };

        if needs_init {
            *slot = None;
        }

        // Fast path: reuse existing connection
        if let Some((_, ref mut stream)) = *slot {
            if stream.write_all(&json).is_ok() {
                return;
            }
            // Write failed — connection is broken
            *slot = None;
        }

        // Slow path: establish a new connection and retry
        if let Some(mut stream) = connect_to_daemon() {
            if stream.write_all(&json).is_ok() {
                *slot = Some((pid, stream));
            }
        }
    });
}

/// Open a new connection to the daemon with a large send buffer.
fn connect_to_daemon() -> Option<std::os::unix::net::UnixStream> {
    let stream = std::os::unix::net::UnixStream::connect(DAEMON_SOCKET_PATH).ok()?;

    // No write timeout — with the 2 MB SO_SNDBUF most writes complete
    // instantly. If the daemon is truly stuck the write blocks, which is
    // correct backpressure. If the daemon dies the OS delivers EPIPE.
    let _ = stream.set_write_timeout(None);

    // Enlarge the kernel send buffer so large payloads can be written in
    // one burst without ping-ponging with the daemon.
    set_send_buffer_size(&stream);

    Some(stream)
}

/// Ask the kernel to enlarge the send buffer on `stream`.
///
/// With the default ~8 KB buffer a 500 KB message requires dozens of
/// write-block-drain round-trips. A 2 MB buffer lets most messages land in
/// kernel space in a single `write` syscall.
fn set_send_buffer_size(stream: &std::os::unix::net::UnixStream) {
    unsafe {
        libc::setsockopt(
            stream.as_raw_fd(),
            libc::SOL_SOCKET,
            libc::SO_SNDBUF,
            &SOCKET_SEND_BUF_SIZE as *const libc::c_int as *const libc::c_void,
            std::mem::size_of::<libc::c_int>() as libc::socklen_t,
        );
    }
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

    pub fn zend_memory_usage(real_usage: bool) -> usize;
}

#[no_mangle]
extern "C" fn observer_begin(_: *mut ExecuteData) {
    let start_memory = unsafe { zend_memory_usage(false) };
    ACTIVE_CALLS.with(|calls| {
        let mut calls = calls.borrow_mut();
        calls.push_back(ActiveCall {
            stack_id: STACK_ID_COUNTER.fetch_add(1, Ordering::Relaxed),
            start_time: Instant::now(),
            start_memory,
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
    let end_memory = unsafe { zend_memory_usage(false) };
    let memory_usage = end_memory as i64 - active_call.start_memory as i64;

    let Some(execute_data_ref) = (unsafe { execute_data.as_ref() }) else {
        return;
    };

    let Some(func) = execute_data_ref.function() else {
        return;
    };

    let Some(name_ptr) = (unsafe { func.internal_function.function_name.as_ref() }) else {
        return;
    };

    let internal = unsafe { func.type_ } as u32 == ZEND_INTERNAL_FUNCTION;

    // Get filename from the caller's execute_data (prev_execute_data)
    let filename = execute_data_ref.previous().and_then(|prev| {
        prev.function().and_then(|prev_func| {
            let prev_internal = unsafe { prev_func.type_ } as u32 == ZEND_INTERNAL_FUNCTION;
            if prev_internal {
                None
            } else {
                unsafe { prev_func.op_array.filename.as_ref() }.and_then(|filename_str| unsafe {
                    CStr::from_ptr(filename_str.val.as_ptr())
                        .to_str()
                        .ok()
                        .map(|s| s.to_string())
                })
            }
        })
    });

    let Some(function_name) =
        (unsafe { CStr::from_ptr(name_ptr.val.as_ptr()).to_str().ok() }).map(|s| s.to_string())
    else {
        return;
    };

    let arguments = Vec::new();
    // let arguments = unsafe { get_function_arguments(execute_data) };

    let throwable: Option<&ClassEntry> = unsafe { zend_ce_throwable.as_ref() };
    let scope: Option<&ClassEntry> = unsafe { func.internal_function.scope.as_ref() };
    let scope_name = scope.and_then(|scope| scope.name()).map(|s| s.to_string());

    let is_exception = scope
        .zip(throwable)
        .is_some_and(|(scope, throwable)| scope.instance_of(throwable));

    let bubbles_exception = ExecutorGlobals::has_exception();

    let call = if let Some(scope_name) = scope_name {
        let parts: Vec<&str> = scope_name.rsplitn(2, '\\').collect();
        let (classname, namespace) = match parts.as_slice() {
            [classname, namespace] => ((*classname).to_string(), Some((*namespace).to_string())),
            [classname] => ((*classname).to_string(), None),
            _ => return,
        };

        CallType::Method(MethodCall {
            classname,
            namespace,
            arguments,
            internal,
            filename,
            is_exception,
            memory_usage,
            parent_stack_id,
            duration: elapsed,
            bubbles_exception,
            name: function_name,
            stack_id: active_call.stack_id,
        })
    } else {
        CallType::Function(FunctionCall {
            arguments,
            filename,
            internal,
            memory_usage,
            namespace: None,
            parent_stack_id,
            duration: elapsed,
            bubbles_exception,
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

#[php_module]
#[php(startup = "startup")]
pub fn get_module(module: ModuleBuilder) -> ModuleBuilder {
    module
        .shutdown_function(shutdown)
        .request_startup_function(request_startup)
        .request_shutdown_function(request_shutdown)
}

fn response_headers_from_sapi_headers(
    mut headers: sapi_headers_struct,
) -> HashMap<String, Option<String>> {
    headers
        .headers()
        .map(|header| {
            (
                header.name().to_string(),
                header.value().map(|value| value.to_string()),
            )
        })
        .collect::<HashMap<String, Option<String>>>()
}

#[allow(dead_code)]
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
                    "{:indent$}└─ {}::{} ({}ms) ({}mb) [{}]",
                    "",
                    call.namespace.clone().unwrap_or_default(),
                    call.name,
                    call.duration.as_millis(),
                    call.memory_usage,
                    call.filename.as_deref().unwrap_or_default(),
                    indent = level * 2
                );
                print_call_tree(calls, call.stack_id, level + 1);
            }
            CallType::Method(call) => {
                println!(
                    "{:indent$}└─ {}{}::{} ({}ms) ({}mb) [{}]",
                    "",
                    call.namespace.clone().unwrap_or_default(),
                    call.classname,
                    call.name,
                    call.duration.as_millis(),
                    call.memory_usage,
                    call.filename.as_deref().unwrap_or_default(),
                    indent = level * 2
                );
                print_call_tree(calls, call.stack_id, level + 1);
            }
        }
    }
}
