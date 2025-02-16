use chrono::{DateTime, Utc};
use ext_php_rs::ffi::ZEND_INTERNAL_FUNCTION;
use ext_php_rs::flags::DataType;
use ext_php_rs::types::Zval;
use ext_php_rs::{prelude::*, zend::ExecuteData};
use lazy_static::lazy_static;
use serde::Serialize;
use std::collections::VecDeque;
use std::ffi::CStr;
use std::sync::{
    atomic::{AtomicU64, Ordering},
    Mutex,
};
use std::time::{Duration, Instant};

lazy_static! {
    static ref FUNCTION_CALLS: Mutex<Vec<CallType>> = Mutex::new(Vec::new());
    static ref REQUEST_START_TIME: Mutex<Option<DateTime<Utc>>> = Mutex::new(None);
    static ref ACTIVE_CALLS: Mutex<VecDeque<(StackId, DateTime<Utc>, Instant)>> =
        Mutex::new(VecDeque::new());
    static ref STACK_ID_COUNTER: AtomicU64 = AtomicU64::new(0);
}

type StackId = u64;

#[derive(Debug, Clone, Serialize)]
struct FunctionCall {
    line: Option<u32>,
    name: String,
    stack_id: u64,
    internal: bool,
    duration: Duration,
    parent_stack_id: u64,
    timestamp: DateTime<Utc>,
    filename: Option<String>,
    namespace: Option<String>,
    arguments: Vec<ArgumentType>,
}

#[derive(Debug, Clone, Serialize)]
struct MethodCall {
    line: Option<u32>,
    name: String,
    internal: bool,
    stack_id: u64,
    classname: String,
    duration: Duration,
    parent_stack_id: u64,
    filename: Option<String>,
    timestamp: DateTime<Utc>,
    namespace: Option<String>,
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
pub extern "C" fn request_startup(_type: i32, _module: i32) -> i32 {
    *REQUEST_START_TIME.lock().unwrap() = Some(chrono::Utc::now());

    0
}

#[no_mangle]
pub extern "C" fn request_shutdown(_type: i32, _module: i32) -> i32 {
    use reqwest::blocking;

    let start_timestamp = REQUEST_START_TIME.lock().unwrap().clone().unwrap();
    let end_timestamp = chrono::Utc::now();

    let duration = end_timestamp.signed_duration_since(start_timestamp);
    let total_micros = duration.num_microseconds().unwrap();
    let seconds = total_micros / 1_000_000;
    let millis = (total_micros % 1_000_000) / 1_000;
    let micros = total_micros % 1_000;
    println!(
        "\nRequest completed in: {}s {}ms {}µs\nTotal function/method calls {:?}",
        seconds,
        millis,
        micros,
        STACK_ID_COUNTER.load(Ordering::SeqCst)
    );

    // Print call tree
    let calls = FUNCTION_CALLS.lock().unwrap();

    // Calculate total time in functions
    let duration: Duration = calls
        .iter()
        .map(|call| match call {
            CallType::Function(call) => call.duration,
            CallType::Method(call) => call.duration,
        })
        .sum();

    let client = blocking::Client::builder()
        .timeout(Duration::from_secs(2))
        .build()
        .unwrap();

    #[derive(Serialize)]
    struct CreateRequest {
        start_timestamp: DateTime<Utc>,
        end_timestamp: DateTime<Utc>,
        callstack: Vec<CallType>,
        duration: Duration,
        url: Option<String>,
    }

    let response = client
        .post("http://localhost:3000/requests")
        .json(&CreateRequest {
            start_timestamp,
            end_timestamp,
            duration,
            callstack: calls.clone(),
            url: Some("https://my-application.nl/api/v1/users".to_owned()),
        })
        .send()
        .unwrap();

    dbg!(response.status(), response.text());

    // if let Ok(mut file) = std::fs::OpenOptions::new()
    //     .write(true)
    //     .create(true)
    //     .truncate(true)
    //     .open("call_tree.json")
    // {
    //     serde_json::to_writer_pretty(&mut file, &calls.clone()).unwrap();
    // }

    if cfg!(debug_assertions) {
        println!("\nFunction call tree:");
        print_call_tree(&calls, 0, 0);
    }

    drop(calls);

    println!("\nTotal time in functions: {:?} ", duration);

    if let Ok(mut calls) = FUNCTION_CALLS.lock() {
        calls.clear();
    }
    if let Ok(mut call_stack) = ACTIVE_CALLS.lock() {
        call_stack.clear();
    }
    STACK_ID_COUNTER.store(0, Ordering::SeqCst);

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
    if let Ok(mut active_calls) = ACTIVE_CALLS.lock() {
        active_calls.push_back((
            STACK_ID_COUNTER.fetch_add(1, Ordering::SeqCst),
            chrono::Utc::now(),
            Instant::now(),
        ));
    }
}

#[no_mangle]
extern "C" fn observer_end(execute_data: *mut ExecuteData, _: *mut ext_php_rs::ffi::zend_value) {
    let Ok(Some((stack_id, start_time, instant))) =
        ACTIVE_CALLS.lock().map(|mut calls| calls.pop_back())
    else {
        return;
    };

    let elapsed = instant.elapsed();

    let Some(execute_data_ref) = (unsafe { execute_data.as_ref() }) else {
        return;
    };

    let Some(func) = execute_data_ref.function() else {
        return;
    };

    let Some(name_ptr) = (unsafe { func.internal_function.function_name.as_ref() }) else {
        return;
    };

    let (filename, line) = {
        let op_array = unsafe { func.op_array };
        if !op_array.filename.is_null() {
            (
                Some(unsafe {
                    CStr::from_ptr((*op_array.filename).val.as_ptr())
                        .to_string_lossy()
                        .into_owned()
                }),
                Some(op_array.line_start),
            )
        } else {
            (None, None)
        }
    };

    let function_name = unsafe {
        CStr::from_ptr(name_ptr.val.as_ptr())
            .to_string_lossy()
            .into_owned()
    };

    let arguments = unsafe { get_function_arguments(execute_data) };

    let scope = unsafe { func.internal_function.scope.as_ref() }
        .and_then(|scope| scope.name())
        .map(|scope| scope.to_owned());

    // Get parent stack ID
    let parent_stack_id = if let Ok(mut active_calls) = ACTIVE_CALLS.lock() {
        let id = active_calls.back().map(|stack| stack.0).unwrap_or(0);
        active_calls.pop_back();
        id
    } else {
        0
    };

    if let Some(scope) = scope {
        let parts: Vec<&str> = scope.rsplitn(2, '\\').collect();
        let (classname, namespace) = match parts.as_slice() {
            [classname, namespace] => (classname.to_string(), Some(namespace.to_string())),
            [classname] => (classname.to_string(), None),
            _ => return,
        };

        {
            let mut calls = FUNCTION_CALLS.lock().unwrap();
            calls.push(CallType::Method(MethodCall {
                line,
                stack_id,
                classname,
                namespace,
                arguments,
                filename,
                parent_stack_id,
                duration: elapsed,
                name: function_name,
                timestamp: start_time,
                internal: unsafe { func.type_ } as u32 == ZEND_INTERNAL_FUNCTION,
            }));
        }
    } else {
        {
            let mut calls = FUNCTION_CALLS.lock().unwrap();
            calls.push(CallType::Function(FunctionCall {
                line,
                stack_id,
                arguments,
                filename,
                namespace: None,
                parent_stack_id,
                duration: elapsed,
                name: function_name,
                timestamp: start_time,
                internal: unsafe { func.type_ } as u32 == ZEND_INTERNAL_FUNCTION,
            }));
        }
    }
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
    register_observer();

    module
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
