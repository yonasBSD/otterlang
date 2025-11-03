# OtterLang 🦦

<p>
    <picture>
      <source media="(prefers-color-scheme: dark)" srcset="https://github.com/jonathanmagambo/otterlang/blob/main/image.png?raw=true" width="400">
      <img src="https://github.com/jonathanmagambo/otterlang/blob/main/image.png?raw=true" width="400" alt="OtterLang Logo" />
    </picture>
    <br>
    <strong>Simple like Python, fast with Rust, and everything in between.</strong>
</p>

[![Build Status](https://github.com/jonathanmagambo/otterlang/workflows/CI/badge.svg)](https://github.com/jonathanmagambo/otterlang/actions)
[![Discord](https://img.shields.io/badge/Discord-Join%20Server-5865F2?style=flat&logo=discord&logoColor=white)](https://discord.gg/y3b4QuvyFk)

An indentation-sensitive programming language with an LLVM backend. OtterLang compiles to native binaries with a focus on simplicity and performance.

## Why OtterLang?

| Feature | OtterLang | Nim |
|---------|-----------|-----|
| **Transparent Rust FFI** | ✅ Auto-extracts entire public API | ❌ Manual bindings required |
| **Indentation-based Syntax** | ✅ Clean, Python-like | ✅ Similar |
| **LLVM Backend** | ✅ Native code generation | ✅ Also uses LLVM |
| **Memory Management** | ✅ Automatic GC + profiling | ✅ GC available |
| **Zero-cost Abstractions** | ✅ Direct LLVM compilation | ✅ Good |
| **Type Inference** | ✅ Full inference | ✅ Advanced |
| **Async/Await** | ✅ Built-in task system | ✅ Async/await |
| **Cross-compilation** | ✅ WASM, embedded targets | ✅ Good support |
| **REPL** | ✅ Interactive development | ❌ Limited |
| **Code Formatting** | ✅ Built-in formatter | ✅ Built-in |
| **Memory Profiling** | ✅ Built-in profiler | ❌ External tools |
| **Rust Ecosystem Access** | ✅ Transparent bridging | ❌ Manual bindings |
| **Compilation Speed** | ✅ Fast (LLVM) | ✅ Fast |
| **Runtime Performance** | ✅ Near C speed (1.28x) | ✅ Near C speed |

## Quick Start

```bash
# Clone and setup
git clone https://github.com/jonathanmagambo/otterlang.git
cd otterlang
./setup.sh

# Create your first program
cat > hello.otter << 'EOF'
fn main:
    print("Hello from OtterLang!")
EOF

# Run it
otter run hello.otter
```

## Installation

### Prerequisites

Requires **LLVM 15**.

**macOS:**
```bash
brew install llvm@15
export LLVM_SYS_150_PREFIX=$(brew --prefix llvm@15)
export PATH="$LLVM_SYS_150_PREFIX/bin:$PATH"
```

**Ubuntu/Debian:**
```bash
sudo apt-get install -y llvm-15 llvm-15-dev clang-15
export LLVM_SYS_150_PREFIX=/usr/lib/llvm-15
```

**Manual Build:**
```bash
git clone https://github.com/jonathanmagambo/otterlang.git
cd otterlang
cargo build --release
cargo install --path . --bin otter
```

## Language Features

### Syntax

Indentation-based syntax (like Python):

```otter
fn greet(name: string) -> string:
    return f"Hello, {name}!"

fn main:
    message = greet("World")
    print(message)
    
    # Control flow
    if message.len() > 10:
        print("Long message")
    
    # Loops
    for i in 0..10:
        print(i)
```

### Transparent Rust FFI

Automatically use any Rust crate without manual configuration:

```otter
use rust:rand
use rust:serde_json

fn main:
    # Auto-extracted from rustdoc JSON
    let random = rand.random_f64()
    let data = json.from_str("{\"key\": \"value\"}")
    
    print(f"Random: {random}")
```

**Key advantages:**
- ✅ No manual bindings needed
- ✅ Automatic API extraction via rustdoc
- ✅ Memory management handled automatically
- ✅ Async/await support for Rust Futures
- ✅ Type checking integrated

See [docs/FFI_TRANSPARENT.md](docs/FFI_TRANSPARENT.md) for details.

### Standard Library

Built-in modules:
- `otter:math` - Mathematical functions
- `otter:io` - File I/O
- `otter:time` - Time utilities
- `otter:task` - Task-based concurrency
- `otter:rand` - Random numbers
- `otter:json` - JSON parsing
- `otter:net` - Networking
- `otter:http` - HTTP client/server

## Performance

Benchmarked against C and Rust (100M iterations):

| Language | Time | Relative to C |
|----------|------|---------------|
| C | 0.070s | 1.00x |
| Rust | 0.080s | 1.14x |
| **OtterLang** | **0.090s** | **1.28x** |

Run `examples/benchmark.sh` to test yourself.

## CLI Commands

```bash
otter run program.otter          # Run program
otter build program.otter -o out   # Build executable
otter fmt                          # Format code
otter repl                         # Start REPL
otter profile memory program.otter # Profile memory
```

## Examples

- `examples/advanced_pipeline.otter` - Complex computation
- `examples/task_benchmark.otter` - Task concurrency
- `examples/ffi_rand_demo.otter` - Transparent FFI
- `examples/ffi_rand_advanced.otter` - Advanced FFI usage

## Status

**Early Access (v0.1.0)** - Experimental, not production-ready.

### Known Limitations

- Type inference is limited (explicit types recommended)
- Module system has some limitations
- Windows support is experimental
- Requires LLVM 15 specifically

## Contributing

Contributions welcome! See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

MIT License - see [LICENSE](LICENSE).
