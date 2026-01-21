# FFI Guide - Rust Crate Integration

## Table of Contents
- [Overview](#overview)
- [Bridge Workflow](#bridge-workflow)
- [Basic Usage](#basic-usage)
- [Supported Types](#supported-types)
- [Struct Methods and Field Access](#struct-methods-and-field-access)
- [Async Functions](#async-functions)
- [Error Handling](#error-handling)
- [Memory Management](#memory-management)
- [Manual Configuration](#manual-configuration)
- [Caching and Build Artifacts](#caching-and-build-artifacts)
- [Diagnostics and Troubleshooting](#diagnostics-and-troubleshooting)
- [Limitations](#limitations)

## Overview

OtterLang provides a transparent Foreign Function Interface (FFI) that loads Rust crates directly with `use rust:<crate>`. The bridge is generated on demand, so most users do not need to write Rust glue code.

When a crate is imported:

1. Discover its public API via `rustdoc` JSON (nightly toolchain required)
2. Generate a `cdylib` stub crate that translates between OtterLang values and Rust values
3. Compile that stub with Cargo and cache the result under `~/.otter_cache/ffi/<crate>-<hash>/`
4. Load the shared library at runtime and register exported functions in the interpreter and type checker

If rustdoc extraction fails, the bridge falls back to manual metadata defined in `crates/ffi/ffi/<crate>/bridge.yaml`.

## Bridge Workflow

Every `use rust:foo` import runs through the same pipeline:

1. Metadata lookup - The `BridgeSymbolRegistry` checks for overrides/hand-authored functions under `crates/ffi/ffi/foo/bridge.yaml` and builds the dependency specification
2. Rustdoc extraction - `cargo +nightly doc -Z unstable-options` produces a JSON description of the crate
3. Stub generation - Synthesizes a new cargo package named `otterffi_<crate>` that wraps parameters/returns and stores complex values in an internal handle table
4. Compilation - The stub is compiled once with `cargo build --release` and cached
5. Dynamic loading - The shared library is loaded with `libloading` and its exports are registered

## Basic Usage

### Import Syntax

```otter
use rust:rand

fn main:
    let number = rand.random_f64()
    println("Random: {}", number)
```

The first run may take a while because Cargo needs to fetch and build the crate plus the generated bridge. Later runs reuse the cached artifacts.

### Module Access and Aliases

Rust paths are flattened into dot-separated segments:

```otter
use rust:chrono as time

fn main:
    let now = time.Utc.now()
    println("UTC timestamp: {}", now)
```

## Supported Types

Transparent generation only keeps functions whose parameter and return types can be mapped to supported types. Unsupported types are skipped unless you add a manual definition.

### Primitive Mapping

| Rust type                                | OtterLang view | Notes |
|------------------------------------------|----------------|-------|
| `()`                                     | `unit`         | |
| `bool`                                   | `bool`         | |
| Signed integers (`i8` → `i64`)           | `i32`/`i64`    | Narrower widths are widened; `i128` becomes `i64` |
| Unsigned integers (`u8` → `usize`)       | `i32`/`i64`    | Values are interpreted as signed integers |
| Floating point (`f32`, `f64`)            | `f64`          | `f32` is widened |
| `&str`, `String`                         | `str`          | Copied through UTF-8 strings |
| `Vec<T>`                                 | `List(T)`      | When `T` is a supported primitive, converted to Otter list via JSON |
| `HashMap<String, V>`                     | `Map(Str, V)`  | When `V` is a supported type, converted to Otter map via JSON |
| `Option<T>`                              | `Option(T)`    | When `T` is a supported type, converted to nullable Otter type via JSON |

### Option and Result Helpers

Functions returning `Option<T>` or `Result<T, E>` have JSON helpers:

- `<path>_optjson` returns `{"some": true, "value": ...}` or `{"some": false}` (for `Option<T>` return types)
- `<path>_try` returns `{"ok": true, "value": ...}` or `{"ok": false, "error": "..."}` (for `Result<T, E>` and all other functions)

All functions have a `_try` variant that provides structured error handling, catching panics and returning error information in JSON format.

```otter
use rust:serde_json

fn main:
    let payload = "{\"name\":\"Otter\"}"
    let info = serde_json.from_str_try(payload)
    println(info)
```

### Opaque Handles

Complex types (structs, enums, iterators, etc.) are represented as 64-bit handles. The stub keeps the real Rust value inside an internal store and only exposes the integer ID to Otter.

```otter
use rust:nalgebra

fn main:
    let vec = nalgebra.Vector3.new(1.0, 2.0, 3.0)
    println("Handle: {}", vec)
```

Opaque handles remain alive until `otter_handle_release(handle)` is called. Handles use reference counting internally. There is also `otter_handle_clone(handle)` for manual reference-count bumps and `otter_handle_ref_count(handle)` to check the current reference count.

Handles are automatically registered with Otter's GC system when created and unregistered when released.

## Struct Methods and Field Access

The FFI system supports calling struct methods and accessing struct fields.

### Methods

Instance methods are automatically exported. The first parameter is always the opaque handle to the struct instance:

```otter
use rust:some_crate

fn main:
    let obj = some_crate.MyStruct.new()
    let result = some_crate.MyStruct.some_method(obj, arg1, arg2)
    some_crate.handle_release(obj)
```

Associated functions (like constructors) are also supported and don't require a handle parameter.

### Field Accessors

Public struct fields automatically get accessor functions:

```otter
use rust:some_crate

fn main:
    let obj = some_crate.MyStruct.new()
    let field_value = some_crate.MyStruct.my_field(obj)
    some_crate.handle_release(obj)
```

## Async Functions

When the extractor sees an `async fn` or a function returning a `Future`, it emits two helpers:

- `<path>_spawn` - accepts the original arguments, spawns the future, and returns an opaque handle to a `JoinHandle`
- `<path>_await` - accepts that handle, waits for completion, releases the JoinHandle, and yields the mapped output type

```otter
use rust:tokio

fn main:
    let join = tokio.time.sleep_spawn(1000)
    println("doing other work...")
    let _ = tokio.time.sleep_await(join)
```

## Error Handling

Rust panics are caught at the FFI boundary with improved error reporting:

1. The panic information is logged to stderr with the function name
2. The function returns the default zero value for the mapped type
3. The `_try` variant returns structured error information: `{"ok": false, "error": "panic in <function>: <details>"}`

Prefer the `_try` helpers for explicit error reporting.

## Memory Management

- Scalar values are copied across the boundary
- Strings are cloned; the stub allocates a `CString` and Otter copies it back
- Opaque handles refer to entries in the stub's `ffi_store`. Handles are automatically registered with Otter's GC system
- Helper APIs (e.g., `_await`) automatically consume the handle they accept via `ffi_store::take`

## Manual Configuration

### bridge.yaml Format

Add overrides or additional functions by creating `crates/ffi/ffi/<crate>/bridge.yaml`:

```yaml
dependency:
  name: serde_json
  version: "1.0"
  features: ["std"]
  default_features: true

functions:
  - name: serde_json.parse_from_file
    rust_path: serde_json::from_reader
    params: ["Opaque"]
    result: Str
    call:
      kind: direct
```

Fields:
- `dependency` mirrors Cargo dependency options
- `functions` is a list of exports. `name` becomes the Otter identifier
- `params`/`result` accept `unit`, `bool`, `i32`, `i64`, `f64`, `str`, or `opaque` (case-insensitive)
- `rust_path` is optional; when omitted the generator uses the exported name

### Call Templates

`call.kind` selects how the stub wraps the Rust call:

- `direct` (default) - calls the function and returns its value
- `result` - expects the function to return `Result<T, E>` and converts it into a JSON string
- `expr` - use a custom Rust expression via `call.expr` with placeholders `{0}`, `{1}`, etc.

Manual entries override auto-generated ones with the same `name`.

## Caching and Build Artifacts

- Bridge sources and build output live under `~/.otter_cache/ffi/`
- The cache key includes crate name, version, feature set, `default-features`, and path overrides
- Clear the cache by deleting the directory: `rm -rf ~/.otter_cache/ffi`
- Rustdoc JSONs are cached separately under `~/.otter_cache/ffi/rustdoc/<crate>/`

## Diagnostics and Troubleshooting

- Set `OTTER_LOG=debug` when running your program to see messages about cache hits, rustdoc generation, and Cargo builds
- The bridge requires the nightly toolchain for `rustdoc --output-format json`. Install it with `rustup toolchain install nightly`
- If Cargo fails to compile the stub, try running `cargo build --release --manifest-path ~/.otter_cache/ffi/<crate>-<hash>/Cargo.toml`
- "invalid opaque handle" means a handle was awaited or released twice
- "rustdoc JSON not found" typically indicates that the crate failed to build its documentation

## Limitations

1. Trait methods still need manual entries in `bridge.yaml` (inherent impl methods are supported)
2. Enum pattern matching, iterators, and macros are not exposed
3. Generics beyond basic scalar substitutions become `opaque` handles for complex cases
4. Building bridges requires Cargo and the nightly Rust toolchain
