# Contributing to OtterLang

Thank you for your interest in contributing to OtterLang! This guide will help you get started with development setup, workflow expectations, and contribution guidelines.

## Development Setup

### Option 1: Nix Development Environment (Recommended)

The easiest way to get started is using the Nix development environment:

```bash
nix develop
cargo +nightly build --release
```

### Option 2: Manual Toolchain Setup

If you prefer to set up the toolchain manually, follow these steps:

#### Prerequisites

1. Install Rust nightly:
   ```bash
   rustup toolchain install nightly
   ```

2. Install LLVM 18 and set the `LLVM_SYS_181_PREFIX` environment variable

#### Platform-Specific Instructions

**macOS**

```bash
brew install llvm@18
export LLVM_SYS_181_PREFIX=$(brew --prefix llvm@18)
export LLVM_SYS_180_PREFIX=$LLVM_SYS_181_PREFIX
export PATH="$LLVM_SYS_181_PREFIX/bin:$PATH"
cargo +nightly build --release
```

**Ubuntu / Debian**

```bash
sudo apt-get install -y llvm-18 llvm-18-dev clang-18
export LLVM_SYS_181_PREFIX=/usr/lib/llvm-18
export LLVM_SYS_180_PREFIX=$LLVM_SYS_181_PREFIX
cargo +nightly build --release
```

**Windows**

```powershell
cargo install llvmenv --locked
llvmenv install 18.1
llvmenv global 18.1

$llvmPath = llvmenv prefix
$env:LLVM_SYS_181_PREFIX = $llvmPath
$env:LLVM_SYS_180_PREFIX = $llvmPath
$env:Path = "$llvmPath\bin;$env:Path"

rustup toolchain install nightly
rustup default nightly

cargo +nightly build --release
```

> **Note for Windows users:** Use the *x64 Native Tools Command Prompt for VS 2022*. It preloads the MSVC linker environment that plain PowerShell/CMD sessions lack.

## Building & Testing

Build the project:

```bash
cargo +nightly build --release
```

Run the test suite:

```bash
cargo test
```

## Workflow Expectations

### Code Style

- Run `cargo fmt` before committing
- Run `cargo clippy` (nightly) and resolve warnings
- Favor expressive names and small helpers over heavy commenting

### Commit Messages

Use concise prefixes following conventional commit format:

```
feat: add array indexing support
fix: resolve type inference bug
docs: update FFI documentation
refactor: simplify lexer tokenization
```

### Pull Requests

When submitting a pull request:

1. Keep PRs scoped to a single change
2. Add tests/docs alongside behavioral changes
3. Ensure CI is green before requesting review
4. Reference related issues when possible (e.g., `Fixes #123`)

## Areas for Contribution

We welcome contributions in the following areas:

- **Language Front-End**: Parser, type checker, diagnostics
- **Standard Library**: Modules and runtime utilities
- **FFI Bridge**: Type mapping, cache tooling, `bridge.yaml` configs
- **Tooling**: VS Code extension, LSP, formatter, REPL/TUI
- **Documentation**: Runnable examples (`examples/basic`, `examples/ffi`, `examples/benchmarks`)
- **Performance**: Profiling and error-message polish

### Custom FFI Bridges

Most crates work via automatic rustdoc extraction. For bespoke setups:

1. Create `ffi/<crate>/bridge.yaml`
2. Describe the dependency metadata and any overrides
3. Use `ffi/rand/bridge.yaml` as a reference

## Reporting Issues

When reporting issues, please include:

- Clear reproduction steps or a minimal failing example
- Expected vs actual behavior
- Environment details (OS, LLVM/clang version, Rust toolchain, `nix develop` vs manual)

## License

By contributing, you agree that your work will be released under the MIT License.
