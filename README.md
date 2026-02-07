# PHP Tracking Extension

A PHP extension written in Rust that automatically profiles every PHP request — capturing call stacks, timing, memory usage, request/response metadata — and sends the data to the [PHP Tracking Daemon](https://github.com/RemcoSmitsDev/php-tracking-daemon) over a Unix socket.

## Prerequisites

- **Rust** (stable toolchain)
- **PHP** (with `php-config` available on your `$PATH`)
- **[PHP Tracking Daemon](https://github.com/RemcoSmitsDev/php-tracking-daemon)** — must be running to receive profiling data

## Building

```sh
# Debug build
cargo build

# Release build (recommended for production)
cargo build --release
```

The compiled shared library will be at:

| Build   | Path                                         |
| ------- | -------------------------------------------- |
| Debug   | `target/debug/libphp_tracking_ext.dylib`     |
| Release | `target/release/libphp_tracking_ext.dylib`   |

> **Note:** On Linux the file extension will be `.so` instead of `.dylib`.

## Configuration

The extension is configured through `php.ini`. The following INI directive is available:

| Directive                        | Type   | Default | Changeable      |
| -------------------------------- | ------ | ------- | --------------- |
| `php_tracking.application_id`    | string | `""`    | `PHP_INI_SYSTEM` |

### `php_tracking.application_id`

The UUID of your application as registered in the PHP Tracking backend. This value is **required** — if it is empty or not set, the extension will silently skip profiling.

Because the permission level is `PHP_INI_SYSTEM`, it can only be set in `php.ini` (or equivalent system-level configuration). It cannot be changed at runtime with `ini_set()`.

### Example `php.ini` configuration

Add the following to your `php.ini` file:

```ini
; Load the extension
extension=/path/to/libphp_tracking_ext.dylib

; Set your application ID (required)
php_tracking.application_id = "your-application-uuid-here"
```

You can find the path to your active `php.ini` by running:

```sh
php --ini
```

## Usage

### Using `php.ini`

Once the extension is loaded and `php_tracking.application_id` is configured in your `php.ini`, every PHP request will be automatically profiled. No code changes are required.

### Using the command line

You can also load the extension and set the INI value directly from the command line, which is useful for testing:

```sh
php \
  -d extension=/path/to/libphp_tracking_ext.dylib \
  -d php_tracking.application_id="your-application-uuid-here" \
  your_script.php
```

## How It Works

1. On **module startup**, the extension registers the `php_tracking.application_id` INI entry and installs a Zend observer to intercept function/method calls.
2. On **request startup**, it captures the start time, memory baseline, and request metadata (`$_SERVER`, `$_GET`, `$_POST`, `$_COOKIE`, `$_REQUEST`).
3. During the request, the observer records every function and method call — including duration, memory usage, class name, file, namespace, arguments, and exception info.
4. On **request shutdown**, it bundles all collected data into a profile payload (JSON) and sends it asynchronously to the PHP Tracking Daemon via a Unix socket at `/tmp/php-tracking-daemon.sock`.

The daemon then compresses and forwards the data to the backend over WebSocket.

## Collected Data

Each profiled request includes:

- **Application ID** — from `php_tracking.application_id`
- **Timing** — request start/end timestamps and total duration
- **Memory** — peak memory usage delta
- **HTTP metadata** — URL, method, response code, response headers
- **Superglobals** — `$_SERVER`, `$_GET`, `$_POST`, `$_COOKIE`, `$_REQUEST`
- **Call stack** — every function and method call with:
  - Name, class, namespace, filename
  - Duration and memory usage
  - Exception propagation info
