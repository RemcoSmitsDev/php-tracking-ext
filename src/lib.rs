use chrono::{DateTime, TimeDelta, Utc};
use ext_php_rs::{
    ffi::ZEND_INTERNAL_FUNCTION, flags::DataType, prelude::*, types::Zval, zend::ExecuteData,
};
use futures_util::{SinkExt as _, StreamExt as _};
use lazy_static::lazy_static;
use parking_lot::Mutex;
use serde::Serialize;
use std::{
    cell::RefCell,
    collections::VecDeque,
    ffi::CStr,
    sync::atomic::{AtomicU64, Ordering},
    time::{Duration, Instant},
};
use tokio::runtime::Runtime;
use tokio_tungstenite::{connect_async, tungstenite::Message};

mod util;

#[derive(Debug, Clone, Serialize)]
struct ProfileData {
    start_timestamp: DateTime<Utc>,
    end_timestamp: DateTime<Utc>,
    callstack: Vec<CallType>,
    duration: TimeDelta,
    url: String,
}

thread_local! {
    static ACTIVE_CALLS: RefCell<VecDeque<(StackId, Instant)>> = RefCell::new(VecDeque::with_capacity(1024));
    static THREAD_FUNCTION_CALLS: RefCell<Vec<CallType>> = RefCell::new(Vec::with_capacity(1000));
}

lazy_static! {
    static ref FUNCTION_CALLS: Mutex<Vec<CallType>> = Mutex::new(Vec::with_capacity(27000));
    static ref REQUEST_START_TIME: Mutex<Option<DateTime<Utc>>> = Mutex::new(None);
    static ref STACK_ID_COUNTER: AtomicU64 = AtomicU64::new(0);
    static ref PROFILE_TX: Mutex<Option<crossbeam_channel::Sender<ProfileData>>> = Mutex::new(None);
}

type StackId = u64;

#[derive(Debug, Clone, Serialize)]
struct FunctionCall {
    #[serde(skip_serializing_if = "Option::is_none")]
    line: Option<u32>,
    name: String,
    stack_id: u64,
    internal: bool,
    duration: Duration,
    parent_stack_id: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    filename: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    namespace: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    arguments: Vec<ArgumentType>,
}

#[derive(Debug, Clone, Serialize)]
struct MethodCall {
    #[serde(skip_serializing_if = "Option::is_none")]
    line: Option<u32>,
    name: String,
    internal: bool,
    stack_id: u64,
    classname: String,
    duration: Duration,
    parent_stack_id: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    filename: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    namespace: Option<String>,
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

fn flush_thread_function_calls() {
    THREAD_FUNCTION_CALLS.with(|calls| {
        let mut calls = calls.borrow_mut();
        if !calls.is_empty() {
            let mut global_calls = FUNCTION_CALLS.lock();
            global_calls.extend(calls.drain(..));
        }
    });
}

async fn maintain_websocket_connection(rx: crossbeam_channel::Receiver<ProfileData>) {
    const WS_URL: &str = "ws://localhost:3000/ws";

    match connect_async(WS_URL).await {
        Ok((ws_stream, _)) => {
            let (mut write, _read) = ws_stream.split();

            dbg!("[CONNECTED] to WS");

            while let Ok(profile_data) = rx.recv() {
                let data = serde_json::to_string(&profile_data).unwrap();
                if let Err(error) = write.send(Message::Text(data.into())).await {
                    eprintln!("Failed to send profile data: {:?}", error);
                }
            }
        }
        Err(error) => {
            eprintln!("Failed to connect to WebSocket: {:?}", error);
        }
    }
}

#[no_mangle]
pub extern "C" fn startup(_type: i32, _module: i32) -> i32 {
    register_observer();

    let (tx, rx) = crossbeam_channel::unbounded();

    *PROFILE_TX.lock() = Some(tx);

    std::thread::spawn(move || {
        let rt = Runtime::new().unwrap();
        rt.block_on(async move { maintain_websocket_connection(rx).await });
    });

    0
}

#[no_mangle]
pub extern "C" fn shutdown(_type: i32, _module: i32) -> i32 {
    let _ = PROFILE_TX.lock().take();

    0
}

#[no_mangle]
pub extern "C" fn request_startup(_type: i32, _module: i32) -> i32 {
    *REQUEST_START_TIME.lock() = Some(chrono::Utc::now());

    0
}

#[no_mangle]
pub extern "C" fn request_shutdown(_type: i32, _module: i32) -> i32 {
    let end_timestamp = chrono::Utc::now();

    let start_timestamp = { REQUEST_START_TIME.lock().take() };

    let Some(start_timestamp) = start_timestamp else {
        return 0;
    };

    flush_thread_function_calls();

    let calls = {
        let mut calls_guard = FUNCTION_CALLS.lock();
        std::mem::take(&mut *calls_guard)
    };

    dbg!(calls.len());

    if let Some(tx) = PROFILE_TX.lock().as_ref() {
        tx.send(ProfileData {
            start_timestamp,
            end_timestamp,
            callstack: calls,
            duration: end_timestamp.signed_duration_since(start_timestamp),
            url: "https://my-application.nl/api/v1/users".into(),
        })
        .unwrap_or_else(|_| {
            eprintln!("PHP tracking extension thread panicked, stopping profiler");
        });
    } else {
        dbg!("[PROFILE TX] dropped");
    }

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
        calls.push_back((
            STACK_ID_COUNTER.fetch_add(1, Ordering::Relaxed),
            Instant::now(),
        ));
    });
}

#[no_mangle]
extern "C" fn observer_end(execute_data: *mut ExecuteData, _: *mut ext_php_rs::ffi::zend_value) {
    let Some((stack_id, instant, parent_stack_id)) = ACTIVE_CALLS.with(|calls| {
        let mut calls = calls.borrow_mut();
        let current = calls.back().cloned();
        if let Some((id, inst)) = current {
            calls.pop_back();
            let parent_id = calls.back().map(|stack| stack.0).unwrap_or(0);
            Some((id, inst, parent_id))
        } else {
            None
        }
    }) else {
        return;
    };

    let elapsed = instant.elapsed();

    // reduce amount of calls reported
    // normally around 27.000 calls for a normal symfony application
    // if elapsed < Duration::from_millis(1) {
    //     // return;
    // }

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

    let function_name = unsafe {
        CStr::from_ptr(name_ptr.val.as_ptr())
            .to_string_lossy()
            .into_owned()
    };

    let (filename, line) = {
        if internal {
            (None, None)
        } else {
            let op_array = unsafe { func.op_array };
            let filename_str = (|| {
                let filename_ptr = unsafe { op_array.filename.as_ref() }?;
                let str_ptr = filename_ptr.val.as_ptr();
                if str_ptr.is_null() {
                    return None;
                }
                Some(unsafe { CStr::from_ptr(str_ptr).to_string_lossy().into_owned() })
            })();

            (filename_str, Some(op_array.line_start))
        }
    };

    // let arguments = unsafe { get_function_arguments(execute_data) };
    let arguments = Vec::default();

    let scope = unsafe { func.internal_function.scope.as_ref() }
        .and_then(|scope| scope.name())
        .map(|scope| scope.to_owned());

    let call = if let Some(scope) = scope {
        let parts: Vec<&str> = scope.rsplitn(2, '\\').collect();
        let (classname, namespace) = match parts.as_slice() {
            [classname, namespace] => (classname.to_string(), Some(namespace.to_string())),
            [classname] => (classname.to_string(), None),
            _ => return,
        };

        CallType::Method(MethodCall {
            line,
            stack_id,
            classname,
            namespace,
            arguments,
            internal,
            filename,
            parent_stack_id,
            duration: elapsed,
            name: function_name,
        })
    } else {
        CallType::Function(FunctionCall {
            line,
            stack_id,
            arguments,
            filename,
            internal,
            namespace: None,
            parent_stack_id,
            duration: elapsed,
            name: function_name,
        })
    };

    THREAD_FUNCTION_CALLS.with(|calls| {
        let mut calls = calls.borrow_mut();
        calls.push(call);

        // Periodically flush to the global FUNCTION_CALLS
        if calls.len() >= 500 {
            let mut global_calls = FUNCTION_CALLS.lock();
            global_calls.extend(calls.drain(..));
        }
    });
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
            DataType::Resource => todo!(),
            DataType::Reference => todo!(),
            DataType::Callable => {
                if let Some(c) = arg.callable() {
                    args.push(ArgumentType::Callable(format!("{:?}", c)));
                }
            }
            DataType::ConstantExpression => todo!(),
            DataType::Void => {
                args.push(ArgumentType::Void);
            }
            DataType::Mixed => {
                args.push(ArgumentType::Mixed(format!("{:?}", arg)));
            }
            DataType::Ptr => todo!(),
            DataType::Indirect => todo!(),
        }
    }

    args
}

#[cfg(debug_assertions)]
fn print_call_tree(calls: &[CallType], parent_id: u64, level: usize) {
    for call in calls.iter().filter(|c| match c {
        CallType::Function(c) => c.parent_stack_id == parent_id,
        CallType::Method(c) => c.parent_stack_id == parent_id,
    }) {
        match call {
            CallType::Function(call) => {
                println!(
                    "{:indent$}└─ {}::{} ({}ms) [{}:{}]",
                    "",
                    call.namespace.clone().unwrap_or_default(),
                    call.name,
                    call.duration.as_millis(),
                    call.filename.clone().unwrap_or_default(),
                    call.line.unwrap_or_default(),
                    indent = level * 2
                );
                print_call_tree(calls, call.stack_id, level + 1);
            }
            CallType::Method(call) => {
                println!(
                    "{:indent$}└─ {}{}::{} ({}ms) [{}:{}]",
                    "",
                    call.namespace.clone().unwrap_or_default(),
                    call.classname.clone(),
                    call.name,
                    call.duration.as_millis(),
                    call.filename.clone().unwrap_or_default(),
                    call.line.unwrap_or_default(),
                    indent = level * 2
                );
                print_call_tree(calls, call.stack_id, level + 1);
            }
        }
    }
}
