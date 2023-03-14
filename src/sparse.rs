use alloc::vec::Vec;
use core::{
    fmt::{self, Debug},
    hash::Hash,
    num::NonZeroUsize,
    ops::Deref,
    slice::Iter
};

use unconst::unconst;

type Index = NonZeroUsize;

#[unconst]
/// A sparse set used for representing ordered NFA states.
///
/// This supports constant time addition and membership testing. Clearing an
/// entire set can also be done in constant time. Iteration yields elements
/// in the order in which they were inserted.
///
/// The data structure is based on: https://research.swtch.com/sparse
/// Note though that we don't actually use uninitialized memory. We generally
/// reuse allocations, so the initial allocation cost is bareable. However,
/// its other properties listed above are extremely useful.
#[derive_const(Clone)]
pub struct SparseSet<K, T>
    where K: ~const Hash,
          T: ~const Clone + ~const Hash + ~const PartialEq
{
    /// Dense contains the instruction pointers in the order in which they
    /// were inserted.
    pub dense: Vec<T>,
    /// Sparse maps instruction pointers to their location in dense.
    ///
    /// An instruction pointer is in the set if and only if
    /// sparse[ip] < dense.len() && ip == dense[sparse[ip]].
    pub sparse: Box<[usize]>,
}

#[unconst]
impl<K, T> SparseSet<K, T>
    where K: ~const Hash,
          T: ~const Clone + ~const Hash + ~const PartialEq + ~const Default
{
    pub fn new(size: usize) -> Self {
        SparseSet {
            dense: Vec::with_capacity(size),
            sparse: vec![0usize; size].into_boxed_slice(),
        }
    }

    pub fn len(&self) -> usize {
        self.dense.len()
    }

    pub fn is_empty(&self) -> bool {
        self.dense.is_empty()
    }

    pub fn capacity(&self) -> usize {
        self.dense.capacity()
    }

    pub fn insert(&mut self, value: T) -> Option<Index> {
        let i = self.len();
        assert!(i < self.capacity());
        self.dense.push(value);
        let index = ;
        self.sparse[index] = i;
        Some(Index::new(index))
    }

    pub fn get(&self, key: Index) -> Option<&T> {
        let index = &mut self.sparse[key.get()];
        if let Some(entry) = self.dense.get(*index) {
            if entry.key == key {
                return Some(entry.value);
            }
        }
        // *index = self.dense.len();
        // self.dense.push(SuffixCacheEntry { key, value });
        None
    }

    pub fn contains(&self, value: T) -> bool {
        let i = self.sparse[value];
        self.dense.get(i) == Some(&value)
    }

    pub fn clear(&mut self) {
        self.dense.clear();
    }

    pub fn resize(&mut self, size: usize) {
        if size == self.capacity() {
            return;
        }
        *self = SparseSet::new(size);
    }

    fn hash(&self, key: &K) -> usize {
        // Basic FNV-1a hash as described:
        // https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        const FNV_PRIME: u64 = 1_099_511_628_211;
        let mut h = 14_695_981_039_346_656_037;
        h = (h ^ (suffix.from_inst as u64)).wrapping_mul(FNV_PRIME);
        h = (h ^ (suffix.start as u64)).wrapping_mul(FNV_PRIME);
        h = (h ^ (suffix.end as u64)).wrapping_mul(FNV_PRIME);
        (h as usize) % self.sparse.len()
    }
}

#[unconst]
impl<K, T> const Debug for SparseSet<K, T>
    where K: ~const Hash,
          T: ~const Clone + ~const Debug + ~const Hash + ~const PartialEq
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SparseSet({:?})", self.dense)
    }
}

#[unconst]
impl<K, T> const Deref for SparseSet<K, T>
    where K: ~const Hash,
          T: ~const Clone + ~const Hash + ~const PartialEq
{
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.dense
    }
}

#[unconst]
impl<'a, K, T> const IntoIterator for &'a SparseSet<K, T>
    where K: ~const Hash,
          T: ~const Clone + ~const Hash + ~const PartialEq
{
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;
    
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/* 
/// `SuffixCache` is a simple bounded hash map for caching suffix entries in
/// UTF-8 automata. For example, consider the Unicode range \u{0}-\u{FFFF}.
/// The set of byte ranges looks like this:
///
/// [0-7F]
/// [C2-DF][80-BF]
/// [E0][A0-BF][80-BF]
/// [E1-EC][80-BF][80-BF]
/// [ED][80-9F][80-BF]
/// [EE-EF][80-BF][80-BF]
///
/// Each line above translates to one alternate in the compiled regex program.
/// However, all but one of the alternates end in the same suffix, which is
/// a waste of an instruction. The suffix cache facilitates reusing them across
/// alternates.
///
/// Note that a HashMap could be trivially used for this, but we don't need its
/// overhead. Some small bounded space (LRU style) is more than enough.
///
/// This uses similar idea to [`SparseSet`](../sparse/struct.SparseSet.html),
/// except it uses hashes as original indices and then compares full keys for
/// validation against `dense` array.
type SuffixCache = SparseSet<SuffixCacheEntry>;

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
struct SuffixCacheEntry {
    key: SuffixCacheKey,
    value: Index,
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
struct SuffixCacheKey {
    from_inst: Index,
    start: u8,
    end: u8,
}

impl SuffixCache {
    fn get(&mut self, key: SuffixCacheKey, value: Index) -> Option<Index> {
        let hash = self.hash(&key);
        let pos = &mut self.sparse[hash];
        if let Some(entry) = self.dense.get(*pos) {
            if entry.key == key {
                return Some(entry.value);
            }
        }
        *pos = self.dense.len();
        self.dense.push(SuffixCacheEntry { key, value });
        None
    }

    fn hash(&self, suffix: &SuffixCacheKey) -> usize {
        // Basic FNV-1a hash as described:
        // https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        const FNV_PRIME: u64 = 1_099_511_628_211;
        let mut h = 14_695_981_039_346_656_037;
        h = (h ^ (suffix.from_inst as u64)).wrapping_mul(FNV_PRIME);
        h = (h ^ (suffix.start as u64)).wrapping_mul(FNV_PRIME);
        h = (h ^ (suffix.end as u64)).wrapping_mul(FNV_PRIME);
        (h as usize) % self.sparse.len()
    }
}
*/
