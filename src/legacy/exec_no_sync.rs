use crate::pool::PoolGuard;

/// `ExecNoSync` is like `Exec`, except it embeds a reference to a cache. This
/// means it is no longer Sync, but we can now avoid the overhead of
/// synchronization to fetch the cache.
#[unconst]
pub struct ExecNoSync<'c, I: ~const Integral> {
    /// All read only state.
    ro: &'c Arc<ExecReadOnly<I>>,
    /// Caches for the various matching engines.
    cache: PoolGuard<'c, ProgramCache<I>>,
}
