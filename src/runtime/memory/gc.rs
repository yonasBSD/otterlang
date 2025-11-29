//! Garbage collection implementations

use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use parking_lot::RwLock;

use crate::runtime::memory::config::GcStrategy;
use crate::runtime::memory::profiler::get_profiler;

/// Trait for garbage collection strategies
pub trait GcStrategyTrait: Send + Sync {
    /// Run garbage collection
    fn collect(&self) -> GcStats;

    /// Allocate memory
    fn alloc(&self, size: usize) -> Option<*mut u8>;

    /// Add a root object
    fn add_root(&self, ptr: usize);

    /// Remove a root object
    fn remove_root(&self, ptr: usize);

    /// Get the strategy name
    fn name(&self) -> &'static str;
}

/// Statistics from a garbage collection run
#[derive(Debug, Clone, Default)]
pub struct GcStats {
    /// Number of objects collected
    pub objects_collected: usize,
    /// Bytes freed
    pub bytes_freed: usize,
    /// Duration of GC in milliseconds
    pub duration_ms: u64,
}

/// Reference counting garbage collector
pub struct RcGC {
    // Reference counting is handled automatically by RcOtter
    // This GC just provides statistics
}

impl RcGC {
    pub fn new() -> Self {
        Self {}
    }
}

impl GcStrategyTrait for RcGC {
    fn collect(&self) -> GcStats {
        // Reference counting handles cleanup automatically
        // This is mainly for statistics
        GcStats {
            objects_collected: 0,
            bytes_freed: 0,
            duration_ms: 0,
        }
    }

    fn alloc(&self, size: usize) -> Option<*mut u8> {
        // Use system allocator
        unsafe {
            let layout = std::alloc::Layout::from_size_align(size, 8).ok()?;
            let ptr = std::alloc::alloc(layout);
            if ptr.is_null() { None } else { Some(ptr) }
        }
    }

    fn add_root(&self, _ptr: usize) {}

    fn remove_root(&self, _ptr: usize) {}

    fn name(&self) -> &'static str {
        "ReferenceCounting"
    }
}

impl Default for RcGC {
    fn default() -> Self {
        Self::new()
    }
}

/// Mark-and-sweep garbage collector
pub struct MarkSweepGC {
    roots: Arc<RwLock<HashSet<usize>>>, // Root object pointers
    objects: Arc<RwLock<HashMap<usize, ObjectInfo>>>,
}

#[derive(Debug, Clone)]
struct ObjectInfo {
    size: usize,
    references: Vec<usize>, // Pointers to other objects
}

impl MarkSweepGC {
    pub fn new() -> Self {
        Self {
            roots: Arc::new(RwLock::new(HashSet::new())),
            objects: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a root object (object that should not be collected)
    pub fn add_root(&self, ptr: usize) {
        self.roots.write().insert(ptr);
    }

    /// Remove a root object
    pub fn remove_root(&self, ptr: usize) {
        self.roots.write().remove(&ptr);
    }

    /// Register an object for GC tracking
    pub fn register_object(&self, ptr: usize, size: usize, references: Vec<usize>) {
        self.objects
            .write()
            .insert(ptr, ObjectInfo { size, references });
    }

    /// Unregister an object
    pub fn unregister_object(&self, ptr: usize) {
        self.objects.write().remove(&ptr);
    }

    /// Mark phase: mark all reachable objects
    fn mark(&self) -> HashSet<usize> {
        let mut marked = HashSet::new();
        let roots = self.roots.read().clone();
        let objects = self.objects.read().clone();

        let mut stack: Vec<usize> = roots.iter().copied().collect();

        while let Some(ptr) = stack.pop() {
            if marked.contains(&ptr) {
                continue;
            }

            marked.insert(ptr);

            if let Some(info) = objects.get(&ptr) {
                for &ref_ptr in &info.references {
                    if !marked.contains(&ref_ptr) {
                        stack.push(ref_ptr);
                    }
                }
            }
        }

        marked
    }

    /// Sweep phase: collect unmarked objects
    fn sweep(&self, marked: &HashSet<usize>) -> GcStats {
        let mut objects = self.objects.write();
        let mut objects_collected = 0;
        let mut bytes_freed = 0;

        let unmarked: Vec<usize> = objects
            .keys()
            .filter(|ptr| !marked.contains(ptr))
            .copied()
            .collect();

        for ptr in unmarked {
            if let Some(info) = objects.remove(&ptr) {
                objects_collected += 1;
                bytes_freed += info.size;

                // Record deallocation in profiler
                get_profiler().record_deallocation(ptr);
            }
        }

        GcStats {
            objects_collected,
            bytes_freed,
            duration_ms: 0, // Will be set by caller
        }
    }
}

impl GcStrategyTrait for MarkSweepGC {
    fn collect(&self) -> GcStats {
        let start = std::time::Instant::now();

        let marked = self.mark();
        let mut stats = self.sweep(&marked);

        stats.duration_ms = start.elapsed().as_millis() as u64;

        stats
    }

    fn alloc(&self, size: usize) -> Option<*mut u8> {
        // Use system allocator (MarkSweep tracks objects separately)
        unsafe {
            let layout = std::alloc::Layout::from_size_align(size, 8).ok()?;
            let ptr = std::alloc::alloc(layout);
            if ptr.is_null() { None } else { Some(ptr) }
        }
    }

    fn add_root(&self, ptr: usize) {
        MarkSweepGC::add_root(self, ptr);
    }

    fn remove_root(&self, ptr: usize) {
        MarkSweepGC::remove_root(self, ptr);
    }

    fn name(&self) -> &'static str {
        "MarkSweep"
    }
}

impl Default for MarkSweepGC {
    fn default() -> Self {
        Self::new()
    }
}

/// Generational GC: Nursery (Bump Pointer) + Old Gen (Mark-Sweep)
pub struct GenerationalGC {
    nursery: crate::runtime::memory::allocator::BumpAllocator,
    old_gen: MarkSweepGC,
    _nursery_size: usize,
    // Track objects allocated in nursery for minor GC
    nursery_objects: Arc<RwLock<HashMap<usize, ObjectInfo>>>,
}

impl GenerationalGC {
    pub fn new() -> Self {
        // Default nursery size: 2MB
        let nursery_size = 2 * 1024 * 1024;
        Self {
            nursery: crate::runtime::memory::allocator::BumpAllocator::new(nursery_size),
            old_gen: MarkSweepGC::new(),
            _nursery_size: nursery_size,
            nursery_objects: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Allocate memory in the nursery
    pub fn alloc(&self, size: usize) -> Option<*mut u8> {
        // Try to allocate in nursery
        if let Some(ptr) = self.nursery.alloc(size, 8) {
            return Some(ptr);
        }

        // Nursery full, trigger minor GC
        let _stats = self.collect_minor();

        // Try again after GC
        if let Some(ptr) = self.nursery.alloc(size, 8) {
            Some(ptr)
        } else {
            // Still failed, try to allocate directly in old gen (fallback)
            // For now, we just fail or trigger major GC
            let _stats = self.collect_major();
            self.nursery.alloc(size, 8)
        }
    }

    /// Minor GC: Collect nursery, promote survivors to old gen
    fn collect_minor(&self) -> GcStats {
        let start = std::time::Instant::now();
        let mut objects_collected = 0;
        let mut bytes_freed = 0;

        // 1. Identify roots pointing to nursery
        // In a real implementation, we'd filter roots. Here we check all roots.
        let roots = self.old_gen.roots.read();
        let nursery_objects = self.nursery_objects.read();
        
        // Find reachable objects in nursery
        let mut reachable = HashSet::new();
        let mut stack: Vec<usize> = roots.iter().copied().collect();

        while let Some(ptr) = stack.pop() {
            if reachable.contains(&ptr) {
                continue;
            }

            // If it's in nursery, mark it
            if nursery_objects.contains_key(&ptr) {
                reachable.insert(ptr);
                
                // Trace children
                if let Some(info) = nursery_objects.get(&ptr) {
                    for &ref_ptr in &info.references {
                        stack.push(ref_ptr);
                    }
                }
            } else {
                // If it's in old gen, we might need to trace into nursery
                // (This requires write barriers in full impl, simplified here)
                if let Some(info) = self.old_gen.objects.read().get(&ptr) {
                     for &ref_ptr in &info.references {
                        if !reachable.contains(&ref_ptr) {
                            stack.push(ref_ptr);
                        }
                    }
                }
            }
        }

        // 2. Promote survivors to old gen
        for ptr in reachable {
            if let Some(info) = nursery_objects.get(&ptr) {
                // Allocate in old gen
                // Note: In a real copying GC, we'd copy the memory content here.
                // Since we don't have direct memory access to the content structure easily here without unsafe casting,
                // we simulate promotion by registering in old gen.
                // Real impl would: memcpy(new_ptr, ptr, info.size)
                
                // For this simulation, we assume the pointer address stays valid (which isn't true for copying GC)
                // OR we just register it in old gen and "pretend" we moved it.
                // To be safer for this codebase, we'll just register it in old gen.
                self.old_gen.register_object(ptr, info.size, info.references.clone());
            }
        }

        // 3. Reset nursery
        // Calculate freed stats
        for (ptr, info) in nursery_objects.iter() {
            if !self.old_gen.objects.read().contains_key(ptr) {
                objects_collected += 1;
                bytes_freed += info.size;
                get_profiler().record_deallocation(*ptr);
            }
        }

        // Clear nursery tracking
        // Note: We don't actually reset the bump allocator here because we "promoted" by keeping pointers.
        // In a real copying GC, we would reset the allocator because we moved everything out.
        // self.nursery.reset(); 
        drop(nursery_objects);
        self.nursery_objects.write().clear();

        GcStats {
            objects_collected,
            bytes_freed,
            duration_ms: start.elapsed().as_millis() as u64,
        }
    }

    /// Major GC: Collect old gen
    fn collect_major(&self) -> GcStats {
        self.old_gen.collect()
    }
}

impl GcStrategyTrait for GenerationalGC {
    fn collect(&self) -> GcStats {
        // Default to minor GC
        self.collect_minor()
    }

    fn alloc(&self, size: usize) -> Option<*mut u8> {
        self.alloc(size)
    }

    fn add_root(&self, ptr: usize) {
        self.old_gen.add_root(ptr);
    }

    fn remove_root(&self, ptr: usize) {
        self.old_gen.remove_root(ptr);
    }

    fn name(&self) -> &'static str {
        "Generational"
    }
}

impl Default for GenerationalGC {
    fn default() -> Self {
        Self::new()
    }
}

/// GC manager that handles different strategies
pub struct GcManager {
    strategy: Arc<RwLock<Box<dyn GcStrategyTrait>>>,
    config: Arc<RwLock<crate::runtime::memory::config::GcConfig>>,
    gc_enabled: AtomicBool,
    disabled_bytes: AtomicUsize,
    disabled_bytes_limit: AtomicUsize,
}

impl GcManager {
    pub fn new(config: crate::runtime::memory::config::GcConfig) -> Self {
        let strategy: Box<dyn GcStrategyTrait> = match config.strategy {
            GcStrategy::ReferenceCounting => Box::new(RcGC::new()),
            GcStrategy::MarkSweep => Box::new(MarkSweepGC::new()),
            GcStrategy::Generational => Box::new(GenerationalGC::new()),
            GcStrategy::None => Box::new(NoOpGC),
        };

        let disabled_limit = config.disabled_heap_limit;
        Self {
            strategy: Arc::new(RwLock::new(strategy)),
            config: Arc::new(RwLock::new(config)),
            gc_enabled: AtomicBool::new(true),
            disabled_bytes: AtomicUsize::new(0),
            disabled_bytes_limit: AtomicUsize::new(disabled_limit),
        }
    }

    pub fn collect(&self) -> GcStats {
        if !self.is_enabled() {
            return GcStats::default();
        }
        self.strategy.read().collect()
    }

    pub fn alloc(&self, size: usize) -> Option<*mut u8> {
        let ptr = self.strategy.read().alloc(size);
        if ptr.is_some() && !self.is_enabled() {
            let total = self.disabled_bytes.fetch_add(size, Ordering::SeqCst) + size;
            let limit = self.disabled_bytes_limit.load(Ordering::SeqCst);
            if limit > 0 && total >= limit {
                self.enable();
                // Trigger a collection now that GC is re-enabled
                let _ = self.collect();
            }
        }
        ptr
    }

    pub fn add_root(&self, ptr: usize) {
        self.strategy.read().add_root(ptr);
    }

    pub fn remove_root(&self, ptr: usize) {
        self.strategy.read().remove_root(ptr);
    }

    pub fn set_strategy(&self, strategy: GcStrategy) {
        let new_strategy: Box<dyn GcStrategyTrait> = match strategy {
            GcStrategy::ReferenceCounting => Box::new(RcGC::new()),
            GcStrategy::MarkSweep => Box::new(MarkSweepGC::new()),
            GcStrategy::Generational => Box::new(GenerationalGC::new()),
            GcStrategy::None => Box::new(NoOpGC),
        };
        *self.strategy.write() = new_strategy;
        self.config.write().strategy = strategy;
    }

    pub fn config(&self) -> Arc<RwLock<crate::runtime::memory::config::GcConfig>> {
        self.config.clone()
    }

    pub fn enable(&self) -> bool {
        let previous = self.gc_enabled.swap(true, Ordering::SeqCst);
        if !previous {
            self.disabled_bytes.store(0, Ordering::SeqCst);
        }
        previous
    }

    pub fn disable(&self) -> bool {
        let previous = self.gc_enabled.swap(false, Ordering::SeqCst);
        self.disabled_bytes.store(0, Ordering::SeqCst);
        previous
    }

    pub fn is_enabled(&self) -> bool {
        self.gc_enabled.load(Ordering::SeqCst)
    }
}

/// No-op GC (for manual memory management)
struct NoOpGC;

impl GcStrategyTrait for NoOpGC {
    fn collect(&self) -> GcStats {
        GcStats::default()
    }

    fn alloc(&self, size: usize) -> Option<*mut u8> {
        unsafe {
            let layout = std::alloc::Layout::from_size_align(size, 8).ok()?;
            let ptr = std::alloc::alloc(layout);
            if ptr.is_null() { None } else { Some(ptr) }
        }
    }

    fn add_root(&self, _ptr: usize) {}

    fn remove_root(&self, _ptr: usize) {}

    fn name(&self) -> &'static str {
        "None"
    }
}

/// Global GC manager
static GLOBAL_GC: once_cell::sync::Lazy<GcManager> = once_cell::sync::Lazy::new(|| {
    let config = crate::runtime::memory::config::GcConfig::from_env();
    GcManager::new(config)
});

/// Get the global GC manager
pub fn get_gc() -> &'static GcManager {
    &GLOBAL_GC
}
