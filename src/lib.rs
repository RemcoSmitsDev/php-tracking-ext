use chrono::{DateTime, Utc};
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

type ZendExecuteFuncExternal = Option<unsafe extern "C" fn(*mut ExecuteData)>;
type ZendExecuteFuncInternal =
    Option<unsafe extern "C" fn(*mut ExecuteData, *mut ext_php_rs::ffi::zend_value)>;

extern "C" {
    static mut zend_execute_ex: ZendExecuteFuncExternal;
    static mut zend_execute_internal: ZendExecuteFuncInternal;
}

static mut ORIGINAL_EXECUTE_EX: ZendExecuteFuncExternal = None;
static mut ORIGINAL_EXECUTE_INTERNAL: ZendExecuteFuncInternal = None;

// Static storage for execution times and original execute_ex
lazy_static! {
    static ref FUNCTION_CALLS: Mutex<Vec<CallType>> = Mutex::new(Vec::new());
    static ref REQUEST_START_TIME: Mutex<Option<DateTime<Utc>>> = Mutex::new(None);
    static ref CALL_STACK: Mutex<VecDeque<StackId>> = Mutex::new(Default::default());
    static ref STACK_ID_COUNTER: AtomicU64 = AtomicU64::new(0);
}

type StackId = u64;

#[derive(Debug, Clone, Serialize)]
struct FunctionCall {
    line: Option<u32>,
    name: String,
    stack_id: u64,
    timestamp: i64,
    internal: bool,
    duration: Duration,
    parent_stack_id: u64,
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
    timestamp: i64,
    classname: String,
    duration: Duration,
    file: Option<String>,
    parent_stack_id: u64,
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
    String(String),
    Int(i64),
    Bool(bool),
    Object(String), // debug presentation
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

        let arg = &*arg_ptr;
        if let Some(arg) = arg.string() {
            args.push(ArgumentType::String(arg));
        } else if let Some(arg) = arg.long() {
            args.push(ArgumentType::Int(arg));
        } else if let Some(arg) = arg.bool() {
            args.push(ArgumentType::Bool(arg));
        } else {
            args.push(ArgumentType::Object(format!("{:?}", arg)));
        }
    }

    args
}

/// Custom execute_ex function for tracking
#[no_mangle]
pub unsafe extern "C" fn custom_execute_external(execute_data: *mut ExecuteData) {
    let stack_id = STACK_ID_COUNTER.fetch_add(1, Ordering::SeqCst);

    let arguments = get_function_arguments(execute_data);

    // Get parent stack ID and depth
    let parent_stack_id = {
        let mut call_stack = CALL_STACK.lock().unwrap();
        let parent_id = if call_stack.is_empty() {
            0
        } else {
            *call_stack.back().unwrap()
        };
        call_stack.push_back(stack_id);
        parent_id
    };

    let start_time = Instant::now();

    // Call original execute_ex
    unsafe {
        if let Some(original) = ORIGINAL_EXECUTE_EX {
            original(execute_data);
        }
    }

    let elapsed = start_time.elapsed();

    {
        let mut call_stack = CALL_STACK.lock().unwrap();
        call_stack.pop_back();
    }

    if execute_data.is_null() {
        return;
    }

    let Some(execute_data_ref) = (unsafe { execute_data.as_ref() }) else {
        return;
    };

    let Some(func) = execute_data_ref.function() else {
        return;
    };

    let Some(name_ptr) = func.internal_function.function_name.as_ref() else {
        return;
    };

    let (file, line) = {
        let op_array = func.op_array;
        let filename = if !op_array.filename.is_null() {
            Some(
                CStr::from_ptr((*op_array.filename).val.as_ptr())
                    .to_string_lossy()
                    .into_owned(),
            )
        } else {
            None
        };

        (filename, op_array.line_start)
    };

    let function_name = CStr::from_ptr(name_ptr.val.as_ptr())
        .to_string_lossy()
        .into_owned();

    let scope = func
        .internal_function
        .scope
        .as_ref()
        .and_then(|scope| scope.name())
        .map(|scope| scope.to_owned());

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
                file,
                stack_id,
                classname,
                arguments,
                namespace,
                parent_stack_id,
                internal: false,
                line: Some(line),
                duration: elapsed,
                name: function_name,
                timestamp: chrono::Utc::now().timestamp(),
            }));
        }
    } else {
        let parts: Vec<&str> = function_name.rsplitn(2, '\\').collect();
        let (function_name, namespace) = match parts.as_slice() {
            [function_name, namespace] => (function_name.to_string(), Some(namespace.to_string())),
            [function_name] => (function_name.to_string(), None),
            _ => return,
        };

        {
            let mut calls = FUNCTION_CALLS.lock().unwrap();
            calls.push(CallType::Function(FunctionCall {
                filename: file,
                stack_id,
                namespace,
                arguments,
                parent_stack_id,
                internal: false,
                line: Some(line),
                duration: elapsed,
                name: function_name,
                timestamp: chrono::Utc::now().timestamp(),
            }));
        }
    }
}

#[no_mangle]
extern "C" fn custom_execute_internal(
    execute_data: *mut ExecuteData,
    return_value: *mut ext_php_rs::ffi::zend_value,
) {
    let stack_id = STACK_ID_COUNTER.fetch_add(1, Ordering::SeqCst);

    if execute_data.is_null() {
        return;
    }

    let Some(execute_data_ref) = (unsafe { execute_data.as_ref() }) else {
        return;
    };

    let Some(func) = execute_data_ref.function() else {
        return;
    };

    let Some(name_ptr) = (unsafe { func.internal_function.function_name.as_ref() }) else {
        return;
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
    let parent_stack_id = {
        let mut call_stack = CALL_STACK.lock().unwrap();
        let parent_id = if call_stack.is_empty() {
            0
        } else {
            *call_stack.back().unwrap()
        };
        call_stack.push_back(stack_id);
        parent_id
    };

    let start_time = Instant::now();

    // Call original function
    unsafe {
        if let Some(original) = ORIGINAL_EXECUTE_INTERNAL {
            original(execute_data, return_value);
        }
    }

    let elapsed = start_time.elapsed();
    {
        let mut call_stack = CALL_STACK.lock().unwrap();
        call_stack.pop_back();
    }

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
                stack_id,
                classname,
                namespace,
                arguments,
                file: None,
                line: None,
                internal: true,
                parent_stack_id,
                duration: elapsed,
                name: function_name,
                timestamp: chrono::Utc::now().timestamp(),
            }));
        }
    } else {
        {
            let mut calls = FUNCTION_CALLS.lock().unwrap();
            calls.push(CallType::Function(FunctionCall {
                stack_id,
                arguments,
                line: None,
                filename: None,
                internal: true,
                namespace: None,
                parent_stack_id,
                duration: elapsed,
                name: function_name,
                timestamp: chrono::Utc::now().timestamp(),
            }));
        }
    }
}

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
                    call.file.clone().unwrap_or_default(),
                    call.line.unwrap_or_default(),
                    indent = level * 2
                );
                print_call_tree(calls, call.stack_id, level + 1);
            }
        }
    }
}

#[no_mangle]
pub extern "C" fn request_startup(_type: i32, _module: i32) -> i32 {
    *REQUEST_START_TIME.lock().unwrap() = Some(chrono::Utc::now());

    if let Ok(mut calls) = FUNCTION_CALLS.lock() {
        calls.clear();
    }
    if let Ok(mut call_stack) = CALL_STACK.lock() {
        call_stack.clear();
    }
    STACK_ID_COUNTER.store(0, Ordering::SeqCst);

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

    let client = blocking::Client::builder()
        .timeout(Duration::from_secs(2))
        .build()
        .unwrap();

    #[derive(Serialize)]
    struct CreateRequest {
        start_timestamp: DateTime<Utc>,
        end_timestamp: DateTime<Utc>,
        callstack: Vec<CallType>,
        url: Option<String>,
    }

    client
        .post("http://localhost:3000/requests")
        .json(&CreateRequest {
            start_timestamp,
            end_timestamp,
            callstack: calls.clone(),
            url: Some("https://my-application.nl/api/v1/users".to_owned()),
        })
        .send()
        .unwrap();

    // if let Ok(mut file) = std::fs::OpenOptions::new()
    //     .write(true)
    //     .create(true)
    //     .truncate(true)
    //     .open("call_tree.json")
    // {
    //     serde_json::to_writer_pretty(&mut file, &calls.clone()).unwrap();
    // }

    // if cfg!(debug_assertions) {
    //     println!("\nFunction call tree:");
    //     print_call_tree(&calls, 0, 0);
    // }

    // Calculate total time in functions
    let total_function_time: Duration = calls
        .iter()
        .map(|call| match call {
            CallType::Function(call) => call.duration,
            CallType::Method(call) => call.duration,
        })
        .sum();
    drop(calls);

    println!(
        "\nTotal time in functions: {:?} ({:.2}% of request)",
        total_function_time,
        (total_function_time.as_nanos() as f64 / duration.num_nanoseconds().unwrap() as f64)
            * 100.0
    );

    0
}

/// Initialize the module and setup function tracking
#[php_module]
pub fn get_module(module: ModuleBuilder) -> ModuleBuilder {
    unsafe {
        // Store original execute_ex and execute_internal
        ORIGINAL_EXECUTE_EX = zend_execute_ex;
        ORIGINAL_EXECUTE_INTERNAL = zend_execute_internal;

        // Override with our custom functions
        zend_execute_ex = Some(custom_execute_external);
        zend_execute_internal = Some(custom_execute_internal);
    }

    module
        .request_startup_function(request_startup)
        .request_shutdown_function(request_shutdown)
}
