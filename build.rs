use std::env;
use std::process::Command;

fn main() {
    // Setup built crate
    let dst = std::path::Path::new(&env::var("OUT_DIR").unwrap()).join("built.rs");
    built::write_built_file_with_opts(&dst).expect("Failed to acquire build-time information");

    // Get PHP configuration
    let php_config = env::var("PHP_CONFIG").unwrap_or_else(|_| {
        Command::new("which")
            .arg("php-config")
            .output()
            .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_string())
            .unwrap_or_else(|_| "php-config".to_string())
    });

    println!("cargo:warning=Using php-config at: {}", php_config);

    // Get PHP include paths
    let output = Command::new(&php_config)
        .arg("--includes")
        .output()
        .expect("Failed to execute php-config");

    if !output.status.success() {
        panic!(
            "php-config --includes failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let includes = String::from_utf8_lossy(&output.stdout);

    // Add PHP include paths
    for include in includes.split_whitespace() {
        if include.starts_with("-I") {
            println!("cargo:rustc-link-search=native={}", &include[2..]);
        }
    }

    // Get PHP extension directory
    let ext_dir = Command::new(&php_config)
        .arg("--extension-dir")
        .output()
        .expect("Failed to get PHP extension directory");

    if !ext_dir.status.success() {
        panic!(
            "php-config --extension-dir failed: {}",
            String::from_utf8_lossy(&ext_dir.stderr)
        );
    }

    let ext_dir = String::from_utf8_lossy(&ext_dir.stdout).trim().to_string();
    println!("cargo:rustc-link-search=native={}", ext_dir);

    // Tell cargo to rerun this script if PHP_CONFIG changes
    println!("cargo:rerun-if-env-changed=PHP_CONFIG");
}
