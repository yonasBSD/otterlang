# OtterLang Roadmap

This document outlines the remaining work required for the v0.1 release, organized by subsystem and priority.

## Component Status

| Component | Key Files | Status | Notes |
|-----------|-----------|--------|-------|
| Language front-end | `crates/otterc_*`, `docs/LANGUAGE_SPEC.md` | Ready | Parser and type checker implemented. Async and pattern guard decisions pending before feature freeze. |
| Runtime + stdlib | `src/runtime/**/*`, `stdlib/otter/*` | Ready | Core features present. Channels, tasks, and GC require polish and documentation. |
| JIT & codegen | `src/codegen/llvm`, `src/runtime/jit/*` | Partial | Functional but hot-path optimizations stubbed. Adaptive managers exist but are rarely invoked. |
| Tooling | `src/cli.rs`, `crates/otterc_fmt`, `src/lsp`, `vscode-extension/` | Partial | End-to-end functionality works. CLI flags and formatter parity pending. |
| Documentation & release | `docs/*.md`, `.github/workflows` | Partial | Comprehensive documentation exists. Installation guide, GC/task documentation, and release automation required. |

## 1. Language & Type System

| Issue | Description | Relevant Files | Priority |
|-------|-------------|----------------|----------|
| `await` limited to direct calls | The grammar restricts `await` to call expressions only (`crates/otterc_parser/src/grammar.rs:440-500`), preventing awaiting of stored handles. Either generalize the grammar or emit appropriate diagnostics. | Parser, `crates/otterc_typecheck/src/checker.rs:2770-2805` | High |
| Traits/polymorphism | Implementation requires `trait` definitions, `impl` blocks, trait-qualified calls, and trait objects. Parser requires new productions and AST nodes (`crates/otterc_parser/src/grammar.rs`, `crates/otterc_ast`). Type checker must track trait tables, validate implementations, and resolve trait bounds and trait objects (`crates/otterc_typecheck/src/checker.rs`). Codegen and runtime require vtables and trait-object layouts for dynamic dispatch (`crates/otterc_codegen/src/llvm`, `crates/otterc_runtime/src/ffi`). Feature must be documented upon implementation. | Parser, type checker, codegen, runtime, documentation | High |
| Type checker modularity and diagnostics | `TypeCheckerWorkspace` snapshots module exports and diagnostics, maintains per-module type maps for incremental re-checking, and shares symbol tables across modules. Remaining work: integrate workspace into CLI and LSP, expand `use` resolution, and surface cross-file diagnostics in the editor. | Type checker, LSP | Medium |
| Pattern matching guards | `match_case` hard-codes `guard: None` (`crates/otterc_parser/src/grammar.rs:750-771`). Either add `case Foo if cond` parsing and type checking, or document as unsupported. | Parser, `crates/otterc_typecheck/src/checker.rs:1510-1600` | Medium |
| Iterator abstraction | `for` loops handle only ranges, lists, and strings (`crates/otterc_codegen/src/llvm/compiler/stmt.rs:296-371`). Comprehensions require list iterables (`crates/otterc_codegen/src/llvm/compiler/expr.rs:2227-2405`). Introduce an iterator trait or produce compile errors that hint at `range()` usage. | Codegen, documentation | High |
| Range expressions allocate eagerly | `start..end` expands to `otter_builtin_range_*` and creates a list immediately (`crates/otterc_runtime/src/stdlib/builtins.rs:472-510`). Document the memory impact or implement a lazy iterator. | Stdlib, documentation | Medium |

## 2. Code Generation, JIT, and Runtime Execution

| Issue | Description | Relevant Files | Priority |
|-------|-------------|----------------|----------|
| `await` drops values | `eval_await_expr` ignores return values (`crates/otterc_codegen/src/llvm/compiler/expr.rs:20-35`). Tasks must write into shared state. Either plumb return values through handles or document the limitation with examples. | Codegen, documentation | High |
| `spawn` context management | Captured variables are serialized via `SPAWN_CONTEXTS` (`crates/otterc_codegen/src/llvm/compiler/expr.rs:38-140`, `crates/otterc_runtime/src/stdlib/task.rs:28-74`) but are never freed if the task never consumes them. Implement explicit drop logic or GC hooks. | Codegen, task runtime | High |
| Hot JIT optimization incomplete | `JitExecutor::optimize_function` is a no-op (`crates/otterc_jit/src/executor.rs:37-67`). Adaptive concurrency and memory managers are instantiated but unused (`crates/otterc_jit/src/engine.rs:28-76`). Hot detector relies on thresholds without integration tests. Implement real recompilation paths or remove unused managers. | `crates/otterc_jit/src/*` | High |
| Iterator coverage | No iterator abstraction exists for maps or custom structs. All iteration funnels through `__otter_iter_array/string` FFI helpers. Add runtime traits or helper APIs to enable user-defined collections to participate in `for` loops and comprehensions. | Runtime FFI, documentation | Medium |
| Standard library surface audit | Multiple stdlib modules exhibit inconsistent naming and return conventions (`stdlib/otter/*.ot`, `crates/otterc_runtime/src/stdlib/*`). `io` and `fs` mix strings and `Option` for error reporting. `http` and `net` define separate response structs. `yaml` and `json` return untyped strings instead of typed values. `task` and `sync` expose incompatible handle types. Required: systematic pass to (a) define shared error/result structs, (b) standardize naming conventions (e.g., `new`, `open`, `close`), (c) document all exported symbols in `docs/API_REFERENCE.md`, and (d) integrate documentation into LSP and CLI for accurate signature completions. | Stdlib Otter files, `crates/otterc_runtime/src/stdlib` | Medium |
| Random number generation APIs | `stdlib/otter/rand.ot` is a thin wrapper around `crates/otterc_runtime/src/stdlib/rand.rs` and only exposes global functions (`seed`, `int`, `float`). No concept of independent RNG instances, cryptographic sources, or reproducible streams per task. Add RNG structs (e.g., `Random`), thread-safe seeding, OS entropy consumption, and documentation with tests demonstrating deterministic replay versus secure randomness. | `stdlib/otter/rand.ot`, `crates/otterc_runtime/src/stdlib/rand.rs` | Medium |
| Time and scheduling utilities | `stdlib/otter/time.ot` provides formatting and `sleep_ms` but lacks monotonic timers, duration arithmetic, or integration with the task runtime. Align `time` APIs with `crates/otterc_runtime/src/stdlib/time.rs`, expose Duration and Instant types, and ensure `task` helpers (timeouts, scheduling) reuse the same primitives to prevent users from accessing the runtime directly. | `stdlib/otter/time.ot`, `crates/otterc_runtime/src/stdlib/time.rs`, task runtime | Medium |
| FFI surface expansion | The FFI layer currently exposes only hand-written helpers. Complex Rust crates require manual symbol plumbing (`crates/otterc_runtime/src/ffi`, `crates/otterc_ffi/*`). To enable interoperation with additional libraries, implement a richer binding story: trait/object support, improved metadata in `crates/otterc_ffi/src/rust_stubgen.rs`, runtime loaders with versioning, and documentation demonstrating how to register new modules. | FFI crates, runtime FFI, documentation | High |
| Task runtime depth | The task runtime combines async helpers (`stdlib/otter/task.ot`) with a scheduler backed by `crates/otterc_runtime/src/task` and `crates/otterc_jit/src/concurrency/*`, but only exposes spawn/join/sleep/channel APIs. Required: per-task cancellation, timeout-aware waits, structured concurrency (scoped tasks), improved metrics and telemetry, and documentation with examples covering select, typed channels, and coordination with `sync` primitives. Implementation affects `crates/otterc_runtime/src/task/channel.rs`, `crates/otterc_runtime/src/task/mod.rs`, and CLI flags that toggle task instrumentation. | Task runtime, stdlib/task, CLI, documentation | High |

## 3. Memory Management & Garbage Collection

| Issue | Description | Relevant Files | Priority |
|-------|-------------|----------------|----------|
| GC strategy implementation gaps | `crates/otterc_runtime/src/memory/gc.rs` defines reference counting, mark-sweep, and generational collectors, but only mark-sweep is regularly exercised. Reference counting lacks cycle detection. Generational GC never promotes objects. `GcStrategy::None` is untested. Required: test coverage for each strategy, runtime metrics to surface collector statistics, and a mechanism to hot-swap strategies based on `GcConfig`. | Runtime memory subsystem | High |
| Arena allocator integration | The arena API (`crates/otterc_runtime/src/memory/arena.rs`, `crates/otterc_runtime/src/stdlib/gc.rs`) is disconnected from the rest of the allocator story. Otter programs cannot tie arenas to lifetimes or automatically reset them. Add high-level Otter APIs (`stdlib/otter/gc.ot`), ensure arenas respect GC roots, and document how arenas interact with FFI allocations. | Runtime memory, stdlib | Medium |
| Memory profiler visibility | `crates/otterc_runtime/src/memory/profiler.rs` can record allocations, but no user-facing tool surfaces the data. Integrate with `otter profile` and expose hooks in the CLI and LSP so developers can inspect object counts, GC pauses, and arena usage. | Memory profiler, CLI, tooling | Medium |

## 4. Tooling & Developer Experience

| Issue | Description | Relevant Files | Priority |
|-------|-------------|----------------|----------|
| CLI flag validation | Flags such as `--tasks`, `--tasks-debug`, and GC environment variables lack validation or help text beyond clap defaults (`src/cli.rs:16-120`). Add cohesive `--gc-*` and `--task-*` behavior and update `INSTALL.md` and `README.md`. | CLI, documentation | Medium |
| LSP capability gaps | The LSP hardcodes completions and snippets (`src/lsp/mod.rs:15-90`), rebuilds a fresh `TypeChecker` per request, and only processes the active document. Add a module-aware index, workspace symbol table, semantic tokens, and go-to-definition/reference support across files so the VS Code extension reflects real project structure. | LSP, type checker | Medium |
| Formatter and LSP parity | `crates/otterc_fmt` and VS Code grammar must remain synchronized with the current specification (spawn/await spacing, comprehension layout). Audit formatter output and update `vscode-extension/syntaxes/otterlang.tmLanguage.json`. | Formatter, VS Code extension | Medium |
| Profiler and test documentation | `otter profile` and `otter test` expose powerful tools, but documentation and sample outputs are missing. Extend `docs/TUTORIALS.md` and `docs/EXAMPLES.md` to explain profiler metrics, snapshot testing, and CI usage. | Documentation, tooling | Low |

## 5. Documentation, Examples, and Release Engineering

| Issue | Description | Relevant Files | Priority |
|-------|-------------|----------------|----------|
| Missing task/sync tutorials | Concurrency modules (`stdlib/otter/task.ot`, `stdlib/otter/sync.ot`) lack corresponding tutorials and examples. Add runnable demos under `examples/` (typed channels, wait groups, select) and reference them from `docs/TUTORIALS.md`. | Examples, documentation | Medium |
| GC and environment variable documentation | Neither `docs/LANGUAGE_SPEC.md` nor `docs/FFI_GUIDE.md` mention `OTTER_GC_STRATEGY`, arenas, or GC hooks. Add a dedicated GC section and environment variable reference. | Documentation | High |
| FFI exception bridge | The runtime exposes exception helpers (`crates/otterc_runtime/src/stdlib/exceptions.rs`), but FFI documentation does not describe their usage. Update `docs/FFI_GUIDE.md` and `docs/FFI_TRANSPARENT.md` with examples. | Documentation | Medium |
| Installation and packaging | No documented process exists for installing binaries, bundling the stdlib, or setting environment variables after a release. Expand `INSTALL.md` and `README.md` with platform-specific steps and ensure CI publishes archives. | Documentation, CI | High |
| Release automation | `.github/workflows/ci.yml` only runs tests (`.github/workflows/ci.yml:1-120`). Add workflows for tagging releases, building macOS/Linux/Windows artifacts, bundling stdlib, and publishing the VS Code extension. | CI, release | High |
| Roadmap visibility | Once blockers begin closing, mirror a short status summary in `README.md` so contributors are aware of remaining work for v0.1. | README | Low |

## Priority Summary

### High Priority
- `await` limited to direct calls
- Traits/polymorphism implementation
- Iterator abstraction
- `await` drops values
- `spawn` context management
- Hot JIT optimization incomplete
- FFI surface expansion
- Task runtime depth
- GC strategy implementation gaps
- GC and environment variable documentation
- Installation and packaging
- Release automation

### Medium Priority
- Type checker modularity and diagnostics
- Pattern matching guards
- Range expressions allocate eagerly
- Iterator coverage
- Standard library surface audit
- Random number generation APIs
- Time and scheduling utilities
- Arena allocator integration
- Memory profiler visibility
- CLI flag validation
- LSP capability gaps
- Formatter and LSP parity
- Missing task/sync tutorials
- FFI exception bridge

### Low Priority
- Profiler and test documentation
- Roadmap visibility

---

This document should be updated as gaps are closed or new blockers emerge to maintain alignment with the codebase and the path to v0.1.
