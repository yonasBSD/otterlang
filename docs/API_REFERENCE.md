# OtterLang API Reference

Complete API reference for OtterLang standard library functions and types.

> [!NOTE]
> Only the prelude primitives (`print`, `panic`, `len`, strings/lists/maps, enums/Option/Result, arithmetic) are injected automatically. Every other module documented here must be imported explicitly with `use module_name` (e.g. `use http`, `use json as js`) before its dotted members can be referenced.

## Table of Contents

- [Built-in Functions](#built-in-functions)
- [Module: `io` - Input/Output Operations](#module-io---inputoutput-operations)
- [Module: `math` - Mathematical Functions](#module-math---mathematical-functions)
- [Module: `time` - Time and Date Operations](#module-time---time-and-date-operations)
- [Module: `json` - JSON Processing](#module-json)
- [Module: `runtime` - Runtime Utilities](#module-runtime---runtime-utilities)
  - [Garbage Collection](#garbage-collection)
  - [Memory Management](#memory-management)
  - [Memory Profiling](#memory-profiling)
- [Module: `arena` - Memory Arenas](#module-arena---memory-arenas)
- [Module: `task` - Concurrent Task Execution](#module-task---concurrent-task-execution)
- [Type Definitions](#type-definitions)

## Built-in Functions

Core functions available in all OtterLang programs.

### `print(message: string) -> unit`

Prints a message to standard output.

**Parameters:**
- `message`: The string to print

**Example:**
```otter
print("Hello, World!")
```

### `println(message: string) -> unit`

Prints a message to standard output followed by a newline.

**Parameters:**
- `message`: The string to print

**Example:**
```otter
println("Hello, World!")
```

### `println() -> unit`

Prints a newline to standard output.

**Example:**
```otter
println()  # Just prints a newline
```

### `eprintln(message: string) -> unit`

Prints a message to standard error followed by a newline (useful for diagnostics).

**Example:**
```otter
eprintln("Something went wrong")
```

### `str(value: any) -> string`

Converts any value to its string representation.

**Parameters:**
- `value`: The value to convert

**Returns:** String representation of the value

**Example:**
```otter
answer = str(42)
println(f"Value: {answer}")
```

### `len(collection: array | string) -> int`

Returns the length of an array or string.

**Parameters:**
- `collection`: An array or string

**Returns:** The length as an integer

**Example:**
```otter
array_len = len([1, 2, 3])  # Returns 3
str_len = len("hello")      # Returns 5
```

### `cap(array: array) -> int`

Returns the capacity of an array.

**Parameters:**
- `array`: An array

**Returns:** The capacity as an integer

## Module: `io` - Input/Output Operations

Wrappers around the runtime I/O primitives (`src/runtime/stdlib/io.rs`). None of these functions are in the prelude, so `use io` is required.

### `read(path: string) -> string`

Loads the entire file at `path` into a string. Raises a runtime error if the file cannot be read.

### `write(path: string, data: string) -> bool`

Writes `data` into `path`, overwriting any existing file. Returns `true` on success.

### `copy(src: string, dst: string) -> bool`

Copies `src` to `dst`. Returns `true` when the operation succeeds.

### `lines(path: string) -> list<string>`

Returns all lines from the file as a list of strings (newline terminators removed).

### `buffer(data: string = "") -> Buffer`

Creates an in-memory buffer seeded with `data`. Buffers expose helper functions:

- `buffer_read(buf, n)` – returns up to `n` bytes from the buffer.
- `buffer_write(buf, bytes)` – appends bytes and returns `true` on success.
- `buffer_clear(buf)` – empties the buffer.
- `buffer_data(buf)` – returns the entire buffer contents as a string.

## Module: `math` - Mathematical Functions

Functions exposed once `use math` is invoked. All functions take/return `float` unless noted.

- `abs(x)` – absolute value.
- `sqrt(x)` – square root; panics on negative inputs.
- `pow(x, y)` – `x` raised to `y`.
- `exp(x)` / `log(x)` – natural exponential/logarithm.
- `sin(x)`, `cos(x)`, `tan(x)` – trigonometric helpers (radians).
- `atan2(y, x)` – four-quadrant arctangent.
- `floor(x)`, `ceil(x)`, `round(x)` – rounding helpers.
- `clamp(x, min, max)` – clamp value into `[min, max]`.
- `min(a, b)` / `max(a, b)` – pairwise extrema.
- `hypot(x, y)` – √(x² + y²).
- `lerp(a, b, t)` – linear interpolation.
- `randf()` – pseudo‑random float in [0, 1).
- `randi(max: int) -> int` – pseudo‑random integer in `[0, max)`.
- `mean(list<float>, len)` / `std(...)` / `sum(...)` – statistics helpers that operate over the first `len` elements of a float list.

## Module: `time` - Time and Date Operations

### `now_ms() -> int`

Returns the current time in milliseconds since Unix epoch.

**Returns:** Milliseconds since epoch as an integer

**Example:**
```otter
timestamp = now_ms()
```

### `sleep_ms(ms: int) -> unit`

Sleeps for the specified number of milliseconds.

**Parameters:**
- `ms`: Milliseconds to sleep

**Example:**
```otter
sleep_ms(1000)  # Sleep for 1 second
```

## Module: `json`

### `parse(json_str: string) -> dict | array | nil`

Parses a JSON string into a dictionary or array.

**Parameters:**
- `json_str`: A valid JSON string

**Returns:** Parsed value or `nil` on error

**Example:**
```otter
data = json.parse('{"name": "Otter", "age": 42}')
if data != nil:
    name = data["name"]
```

### `stringify(value: dict | array) -> string`

Converts a dictionary or array to a JSON string.

**Parameters:**
- `value`: A dictionary or array

**Returns:** JSON string representation

**Example:**
```otter
json_str = json.stringify({"key": "value"})
```

> [!NOTE]
> `stringify()` is specific to JSON serialization. For general-purpose conversions use the built-in `str()` helper described above (the old Pythonic alias relationship has been flipped: `stringify()` now simply calls `str()`).

## Module: `runtime` - Runtime Utilities

### Garbage Collection

#### `collect_garbage() -> int`

Manually triggers garbage collection.

**Returns:** Number of bytes freed

**Example:**
```otter
freed_bytes = runtime.collect_garbage()
println(f"Freed {freed_bytes} bytes")
```

#### `set_gc_strategy(strategy: string) -> unit`

Sets the garbage collection strategy.

**Parameters:**
- `strategy`: One of "rc", "marksweep", "hybrid", "none"

**Example:**
```otter
runtime.set_gc_strategy("marksweep")
```

### Memory Management

#### `gc.alloc(size: int) -> i64`

Allocates `size` bytes on the GC-managed heap and returns a pointer.

**Parameters:**
- `size`: Number of bytes to allocate

**Returns:** Pointer as an integer

**Example:**
```otter
ptr = gc.alloc(128)
```

#### `gc.add_root(ptr: i64) -> unit`

Registers the given pointer as a GC root.

**Parameters:**
- `ptr`: Pointer returned by `gc.alloc`

**Example:**
```otter
gc.add_root(ptr)
```

#### `gc.remove_root(ptr: i64) -> unit`

Removes a previously registered root pointer.

**Parameters:**
- `ptr`: Pointer previously added with `gc.add_root`

**Example:**
```otter
gc.remove_root(ptr)
```

### Memory Profiling

#### `memory_profiler_start() -> unit`

Starts memory profiling.

**Example:**
```otter
runtime.memory_profiler_start()
```

#### `memory_profiler_stop() -> unit`

Stops memory profiling.

**Example:**
```otter
runtime.memory_profiler_stop()
```

#### `memory_profiler_stats() -> string`

Returns memory profiling statistics as JSON.

**Returns:** JSON string with profiling statistics

**Example:**
```otter
stats = runtime.memory_profiler_stats()
```

#### `memory_profiler_leaks() -> string`

Detects and returns memory leaks as JSON.

**Returns:** JSON string with leak information

**Example:**
```otter
leaks = runtime.memory_profiler_leaks()
```

#### `gc.disable() -> bool`

Temporarily pauses automatic garbage collection.

**Returns:** Previous GC state

**Example:**
```otter
was_enabled = gc.disable()
# Perform operations that need GC disabled
if was_enabled:
    gc.enable()
```

#### `gc.enable() -> bool`

Re-enables automatic garbage collection.

**Returns:** Previous GC state

**Example:**
```otter
gc.enable()
```

#### `gc.is_enabled() -> bool`

Returns `true` if garbage collection is currently active.

**Returns:** Boolean indicating GC state

**Example:**
```otter
if not gc.is_enabled():
    gc.enable()
```

## Module: `arena` - Memory Arenas

Lightweight bump-allocated arenas for deterministic lifetimes. Arenas do not participate in GC; all allocations live until you reset or destroy the arena.

#### `arena.create(capacity: int = 65536) -> i64`

Creates a new arena with the requested capacity (default 64KB) and returns a handle.

**Parameters:**
- `capacity`: Initial capacity in bytes (default: 65536)

**Returns:** Arena handle as integer

**Example:**
```otter
arena_handle = arena.create(131072)  # 128KB arena
```

#### `arena.alloc(handle: i64, size: int, align: int = 8) -> i64`

Allocates raw bytes from the arena. Returns a pointer (as an integer) or `0` if there is not enough space.

**Parameters:**
- `handle`: Arena handle from `arena.create`
- `size`: Number of bytes to allocate
- `align`: Alignment requirement (default: 8)

**Returns:** Pointer as integer, or 0 on failure

**Example:**
```otter
ptr = arena.alloc(arena_handle, 128)
```

#### `arena.reset(handle: i64) -> bool`

Clears all allocations inside the arena so it can be reused.

**Parameters:**
- `handle`: Arena handle

**Returns:** `true` on success

**Example:**
```otter
arena.reset(arena_handle)
```

#### `arena.destroy(handle: i64) -> bool`

Destroys the arena and frees its backing memory.

**Parameters:**
- `handle`: Arena handle

**Returns:** `true` on success

**Example:**
```otter
arena.destroy(arena_handle)
```

## Module: `task` - Concurrent Task Execution

### `spawn(block: () -> T) -> Task<T>`

Spawns a concurrent task.

**Parameters:**
- `block`: A function block to execute concurrently

**Returns:** A Task handle

**Example:**
```otter
task = spawn:
    return compute_result()
```

### `await(task: Task<T>) -> T`

Waits for a task to complete and returns its result.

**Parameters:**
- `task`: A Task handle

**Returns:** The task's result

**Example:**
```otter
task = spawn:
    return 42
result = await task
```

## Type Definitions

### `Task<T>`

Represents a concurrent task that returns type T.

### Built-in Types

- `int`: 64-bit signed integer
- `float`: 64-bit floating point
- `bool`: Boolean (`true` or `false`)
- `string`: UTF-8 string
- `unit`: Unit type (void)
- `array<T>`: Dynamic array of type T
- `dict<K, V>`: Dictionary mapping K to V

See [Language Specification](./LANGUAGE_SPEC.md) for more details.
