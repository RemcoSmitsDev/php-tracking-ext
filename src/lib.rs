use ext_php_rs::{prelude::*, zend::ExecuteData};
use lazy_static::lazy_static;
use std::collections::VecDeque;
use std::ffi::CStr;
use std::sync::{
    atomic::{AtomicU64, Ordering},
    Mutex,
};
use std::time::{Duration, Instant};

// Define type for execute_ex function pointer
type ZendExecuteFunc = Option<unsafe extern "C" fn(*mut ExecuteData)>;

extern "C" {
    static mut zend_execute_ex: ZendExecuteFunc;
}

#[derive(Debug, Clone)]
struct FunctionCall {
    line: u32,
    name: String,
    file: String,
    stack_id: u64,
    duration: Duration,
    parent_stack_id: Option<u64>,
}

// Static storage for execution times and original execute_ex
lazy_static! {
    static ref FUNCTION_CALLS: Mutex<Vec<FunctionCall>> = Mutex::new(Vec::new());
    static ref ORIGINAL_EXECUTE_EX: Mutex<Option<ZendExecuteFunc>> = Mutex::new(None);
    static ref REQUEST_START_TIME: Mutex<Option<Instant>> = Mutex::new(None);
    static ref CALL_STACK: Mutex<VecDeque<(String, u64, Instant)>> = Mutex::new(Default::default());
    static ref STACK_ID_COUNTER: AtomicU64 = AtomicU64::new(0);
}

/// Custom execute_ex function for tracking
#[no_mangle]
pub unsafe extern "C" fn custom_execute_ex(execute_data: *mut ExecuteData) {
    if execute_data.is_null() {
        return;
    }

    let (function_name, file, line) = {
        let execute_data = &*execute_data;

        let Some(func) = execute_data.function() else {
            return;
        };

        let name = if let Some(name_ptr) = func.internal_function.function_name.as_ref() {
            let scope = if let Some(scope) = func.internal_function.scope.as_ref() {
                if !scope.name.is_null() {
                    format!(
                        "{}::",
                        CStr::from_ptr((*scope.name).val.as_ptr())
                            .to_string_lossy()
                            .into_owned()
                    )
                } else {
                    "".into()
                }
            } else {
                "".into()
            };

            let function = CStr::from_ptr(name_ptr.val.as_ptr())
                .to_string_lossy()
                .into_owned();

            format!("{}{}", scope, function)
        } else {
            "anonymous".to_string()
        };

        let (file, line) = {
            let op_array = func.op_array;
            let filename = if !op_array.filename.is_null() {
                CStr::from_ptr((*op_array.filename).val.as_ptr())
                    .to_string_lossy()
                    .into_owned()
            } else {
                "unknown".to_string()
            };

            (filename, op_array.line_start)
        };

        (name, file, line)
    };

    let stack_id = STACK_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
    let start_time = Instant::now();

    // Get parent stack ID and depth
    let parent_stack_id = {
        let mut call_stack = CALL_STACK.lock().unwrap();
        let parent_id = if call_stack.is_empty() {
            None
        } else {
            Some(call_stack.back().unwrap().1)
        };
        call_stack.push_back((function_name.clone(), stack_id, start_time));
        parent_id
    };

    // Call original execute_ex
    if let Some(Some(original)) = ORIGINAL_EXECUTE_EX
        .lock()
        .ok()
        .map(|guard| *guard)
        .flatten()
    {
        original(execute_data);
    }

    let elapsed = start_time.elapsed();

    {
        let mut call_stack = CALL_STACK.lock().unwrap();
        call_stack.pop_back();
    }

    // Store function call information
    if let Ok(mut calls) = FUNCTION_CALLS.lock() {
        calls.push(FunctionCall {
            name: function_name,
            duration: elapsed,
            stack_id,
            parent_stack_id,
            file,
            line,
        });
    }
}

fn print_call_tree(calls: &[FunctionCall], parent_id: Option<u64>, level: usize) {
    for call in calls.iter().filter(|c| c.parent_stack_id == parent_id) {
        println!(
            "{:indent$}└─ {} ({}ms) [{}:{}]",
            "",
            call.name,
            call.duration.as_millis(),
            call.file,
            call.line,
            indent = level * 2
        );
        print_call_tree(calls, Some(call.stack_id), level + 1);
    }
}

fn write_call_tree_to_file(
    calls: &[FunctionCall],
    parent_id: Option<u64>,
    level: usize,
    file: &mut std::fs::File,
) -> std::io::Result<()> {
    use std::io::Write;

    for call in calls.iter().filter(|c| c.parent_stack_id == parent_id) {
        writeln!(
            file,
            "{:indent$}└─ {} ({}ms) [{}:{}]",
            "",
            call.name,
            call.duration.as_millis(),
            call.file,
            call.line,
            indent = level * 2
        )?;
        write_call_tree_to_file(calls, Some(call.stack_id), level + 1, file)?;
    }
    Ok(())
}

#[no_mangle]
pub extern "C" fn request_startup(_type: i32, _module: i32) -> i32 {
    *REQUEST_START_TIME.lock().unwrap() = Some(Instant::now());

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
    let start = REQUEST_START_TIME.lock().unwrap().clone().unwrap();

    let duration = start.elapsed();
    println!(
        "\nRequest completed in: {:?} total stacks {:?}",
        duration,
        STACK_ID_COUNTER.load(Ordering::SeqCst)
    );

    // Print call tree
    let calls = FUNCTION_CALLS.lock().unwrap();

    if cfg!(debug_assertions) {
        println!("\nFunction call tree:");
        print_call_tree(&calls, None, 0);
    } else {
        if let Ok(mut file) = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open("/Users/remcosmits/Documents/code/php-tracking/call_tree.txt")
        {
            println!("\nFunction call tree:");
            write_call_tree_to_file(&calls, None, 0, &mut file).unwrap();
        }
    }

    // Calculate total time in functions
    let total_function_time: Duration = calls.iter().map(|call| call.duration).sum();
    drop(calls);

    println!(
        "\nTotal time in functions: {:?} ({:.4}% of request)",
        total_function_time,
        (total_function_time.as_nanos() as f64 / duration.as_nanos() as f64) * 100.0
    );

    0
}

/// Initialize the module and setup function tracking
#[php_module]
pub fn get_module(module: ModuleBuilder) -> ModuleBuilder {
    unsafe {
        // Store original execute_ex
        *ORIGINAL_EXECUTE_EX.lock().unwrap() = Some(zend_execute_ex);
        // Set our custom execute_ex
        zend_execute_ex = Some(custom_execute_ex);
    }

    module
        .request_startup_function(request_startup)
        .request_shutdown_function(request_shutdown)
}
