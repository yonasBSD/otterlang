# FFI Guide – Rust Crate Integration

## Table of Contents
- [Overview](#overview)
- [Bridge Workflow](#bridge-workflow)
- [Basic Usage](#basic-usage)
  - [Import Syntax](#import-syntax)
  - [Module Access and Aliases](#module-access-and-aliases)
- [Supported Types](#supported-types)
  - [Primitive Mapping](#primitive-mapping)
  - [Option and Result Helpers](#option-and-result-helpers)
  - [Opaque Handles](#opaque-handles)
- [Async Functions](#async-functions)
- [Error Handling](#error-handling)
- [Memory Management](#memory-management)
- [Manual Configuration](#manual-configuration)
  - [bridge.yaml Format](#bridgeyaml-format)
  - [Call Templates](#call-templates)
- [Caching and Build Artifacts](#caching-and-build-artifacts)
- [Diagnostics and Troubleshooting](#diagnostics-and-troubleshooting)
- [Limitations and Roadmap](#limitations-and-roadmap)

OtterLang ships with a transparent Foreign Function Interface (FFI) that can load
Rust crates directly with `use rust:<crate>`. The bridge is generated on demand,
so most users do not have to write any Rust glue code.

## Overview

The current implementation focuses on calling public Rust *functions*. When a
crate is imported we:

1. Discover its public API via `rustdoc` JSON (nightly toolchain required).
2. Generate a `cdylib` stub crate that translates between OtterLang values and
   Rust values.
3. Compile that stub with Cargo and cache the result under
   `~/.otter_cache/ffi/<crate>-<hash>/`.
4. Load the shared library at runtime and register exported functions in the
   interpreter and type checker.

If rustdoc extraction fails, the bridge falls back to manual metadata defined in
`crates/ffi/ffi/<crate>/bridge.yaml`.

## Bridge Workflow

Every `use rust:foo` import runs through the same pipeline:

1. **Metadata lookup** – The `BridgeSymbolRegistry` checks for
overrides/hand-authored functions under `crates/ffi/ffi/foo/bridge.yaml` and
builds the dependency specification (version, features, path overrides).
2. **Rustdoc extraction** – `cargo +nightly doc -Z unstable-options` produces a
JSON description of the crate. We parse every public free function and drop ones
with unsupported signatures.
3. **Stub generation** – `crates/ffi` synthesizes a new cargo package named
`otterffi_<crate>`. The stub wraps string parameters/returns with `CString`/`String`, translates scalars, and stores complex values in an internal handle table.
4. **Compilation** – The stub is compiled once with `cargo build --release` and
stored with its target artifacts; subsequent runs reuse the cached build.
5. **Dynamic loading** – The shared library is loaded with `libloading` and its
exports are registered so the Otter program can call them like regular
functions.

## Basic Usage

### Import Syntax

```otter
use rust:rand

fn main:
    let number = rand.random_f64()
    println("Random: {}", number)
```

The first run may take a while because Cargo needs to fetch and build the crate
plus the generated bridge. Later runs reuse the cached artifacts.

### Module Access and Aliases

Rust paths are flattened into dot-separated segments:

```otter
use rust:chrono as time

fn main:
    let now = time.Utc.now()
    println("UTC timestamp: {}", now)
```

The example above maps `chrono::Utc::now` to `chrono.Utc.now`. You can alias a
crate (`as time`) to avoid repeating long prefixes.

## Supported Types

Transparent generation only keeps functions whose parameter and return types can
be mapped to one of the supported `TypeSpec` values. Unsupported types are
skipped (they will not appear in Otter) unless you add a manual definition.

### Primitive Mapping

| Rust type                                | OtterLang view | Notes |
|------------------------------------------|----------------|-------|
| `()`                                     | `unit`         | Returned as `()` |
| `bool`                                   | `bool`         | |
| Signed integers (`i8` → `i64`)           | `i32`/`i64`    | Narrower widths are widened; `i128` becomes `i64` |
| Unsigned integers (`u8` → `usize`)       | `i32`/`i64`    | Values are interpreted as signed integers |
| Floating point (`f32`, `f64`)            | `f64`          | `f32` is widened |
| `&str`, `String`                         | `str`          | Copied through UTF-8 strings |

### Option and Result Helpers

The direct export of a function returning `Option<T>` or `Result<T, E>` mirrors
whatever `T` maps to. In addition, the stub emits JSON helpers so callers can
introspect success/failure without panicking:

- `<path>.<fn>_optjson` returns `{"some": true, "value": ...}` or
  `{"some": false}`
- `<path>.<fn>_try` returns `{"ok": true, "value": ...}` or
  `{"ok": false, "error": "Debug string"}`

```otter
use rust:serde_json

fn main:
    let payload = "{\"name\":\"Otter\"}"
    let info = serde_json.from_str_try(payload)
    println(info)  # => {"ok":true,"value":{...}}
```

### Opaque Handles

Anything that does not map to a scalar (structs, enums, `Vec<T>`, iterators,
async handles, etc.) is represented as a 64-bit handle. The stub keeps the real
Rust value inside an internal store and only exposes the integer ID to Otter.

```otter
use rust:nalgebra

fn main:
    let vec = nalgebra.Vector3.new(1.0, 2.0, 3.0)  # returns opaque handle (i64)
    println("Handle: {}", vec)
```

Opaque handles remain alive until `otter_handle_release(handle)` is called from
Otter code (this helper is exported by every bridge). Forgetting to release a
handle keeps the Rust value in memory for the life of the process. There is also
`otter_handle_clone(handle)` for manual reference-count bumps when sharing the
same resource between multiple data structures.

## Async Functions

When the extractor sees an `async fn` or a function returning a `Future`, it
emits two helpers that run on a Tokio runtime embedded inside the stub:

- `<path>.<fn>_spawn` – accepts the original arguments, spawns the future, and
  returns an opaque handle to a `JoinHandle`
- `<path>.<fn>_await` – accepts that handle, waits for completion, releases the
  JoinHandle, and yields the mapped output type

```otter
use rust:tokio

fn main:
    let join = tokio.time.sleep_sleep_spawn(1000)  # handle
    println("doing other work...")
    let _ = tokio.time.sleep_sleep_await(join)
```

(Yes, the current naming duplicates the function segment; this is the behavior
of the generator today.)

## Error Handling

Rust panics are caught at the FFI boundary. If a panic occurs the stub prints no
additional output but returns the default zero value for the mapped type (for
strings that means a null pointer, for numbers it is 0). Prefer the `_try`
helpers for explicit error reporting, especially when dealing with crates that
return `Result`.

## Memory Management

- Scalar values are copied across the boundary.
- Strings are cloned; the stub allocates a `CString` and Otter copies it back
  into an Otter `str`.
- Opaque handles refer to entries in the stub’s `ffi_store`. Handles are simple
  `i64` values on the Otter side, so the runtime is not yet aware of when they
  should be dropped. Call `otter_handle_release(handle)` once you no longer need
  the resource to avoid leaking the underlying Rust value.
- Helper APIs (e.g., `_await`) automatically consume the handle they accept via
  `ffi_store::take`, so you must not reuse a handle after awaiting.

## Manual Configuration

### bridge.yaml Format

Automatic extraction only works when the signature is supported and rustdoc can
be produced. Add overrides or additional functions by creating
`crates/ffi/ffi/<crate>/bridge.yaml`:

```yaml
# crates/ffi/ffi/serde_json/bridge.yaml
dependency:
  name: serde_json
  version: "1.0"
  features: ["std"]
  default_features: true

functions:
  - name: serde_json.parse_from_file
    rust_path: serde_json::from_reader
    params: ["Opaque"]            # e.g. file handle
    result: Str
    call:
      kind: direct
```

Fields:
- `dependency` mirrors Cargo dependency options (`version`, `path`, `features`,
  `default_features`). The crate name defaults to the directory name.
- `functions` is a list of exports. `name` becomes the Otter identifier.
- `params`/`result` accept `unit`, `bool`, `i32`, `i64`, `f64`, `str`, or
  `opaque` (case-insensitive).
- `rust_path` is optional; when omitted the generator uses the exported name as
  `crate::path::to::function`.

### Call Templates

`call.kind` selects how the stub wraps the Rust call:

- `direct` (default) – calls the function and returns its value.
- `result` – expects the function to return `Result<T, E>` and converts it into a
  JSON string in the same format as `_try`.
- `expr` – use a custom Rust expression via `call.expr` with placeholders
  `{0}`, `{1}`, … for arguments.

Manual entries override auto-generated ones with the same `name`, so you can
patch individual signatures without losing the rest of the transparent surface.

## Caching and Build Artifacts

- Bridge sources and build output live under `~/.otter_cache/ffi/` (respects the
  platform-specific Otter cache root).
- The cache key includes crate name, version, feature set, `default-features`,
  and path overrides.
- Clear the cache by deleting the directory if you need a clean rebuild:
  `rm -rf ~/.otter_cache/ffi`.
- Rustdoc JSONs are cached separately under `~/.otter_cache/ffi/rustdoc/<crate>/`
  to avoid regenerating documentation repeatedly.

## Diagnostics and Troubleshooting

- Set `OTTER_LOG=debug` when running your program to see messages about cache
  hits, rustdoc generation, and Cargo builds.
- The bridge requires the nightly toolchain for `rustdoc --output-format json`.
  Install it with `rustup toolchain install nightly`. If nightly is missing the
  generator falls back to manual metadata only.
- If Cargo fails to compile the stub, try running `cargo build --release
  --manifest-path ~/.otter_cache/ffi/<crate>-<hash>/Cargo.toml` to reproduce the
  error directly.
- "invalid opaque handle" means a handle was awaited or released twice. Make
  sure to clone handles you intend to share and release each clone separately.
- "rustdoc JSON not found" typically indicates that the crate failed to build
  its documentation; re-run with `OTTER_LOG=debug` to inspect the Cargo output.

## Limitations and Roadmap

Current limitations of the transparent FFI:

1. Only free functions are exported today. Trait methods and inherent impl
   methods need manual entries.
2. Struct field access, enum pattern matching, iterators, and macros are not
   exposed.
3. Generics beyond basic scalar substitutions become `opaque` handles. There is
   no automatic deserialization to Otter compound types yet.
4. Memory for opaque handles stays allocated until
   `otter_handle_release(handle)` is called.
5. The async helper names include the function name twice
   (`foo.bar.baz.baz_spawn`). This may change in the future and should not be
   relied upon for stable APIs.
6. Building bridges requires Cargo and the nightly Rust toolchain.

Upcoming work includes better ownership tracking for opaque handles, smoother
naming for async helpers, struct field exposure, and streaming/iterator support.
