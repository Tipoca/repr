use alloc::vec::Vec;
use core::{
    fmt::{self, Debug},
    ops::Deref,
    slice::Iter
};

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
#[derive(Clone)]
pub struct SparseSet<T: Clone> {
    /// Dense contains the instruction pointers in the order in which they
    /// were inserted.
    pub dense: Vec<T>,
    /// Sparse maps instruction pointers to their location in dense.
    ///
    /// An instruction pointer is in the set if and only if
    /// sparse[ip] < dense.len() && ip == dense[sparse[ip]].
    pub sparse: Box<[usize]>,
}

impl<T: Clone> SparseSet<T> {
    pub fn new(size: usize) -> Self {
        SparseSet {
            dense: Vec::with_capacity(size),
            sparse: vec![T::default(); size].into_boxed_slice(),
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

    pub fn insert(&mut self, value: T) {
        let i = self.len();
        assert!(i < self.capacity());
        self.dense.push(value);
        self.sparse[value] = i;
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
}

impl<T: Clone + Debug> Debug for SparseSet<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SparseSet({:?})", self.dense)
    }
}

impl<T: Clone> Deref for SparseSet<T> {
    type Target = [usize];

    fn deref(&self) -> &Self::Target {
        &self.dense
    }
}

impl<'a, T: Clone> IntoIterator for &'a SparseSet<T> {
    type Item = &'a usize;
    type IntoIter = Iter<'a, usize>;
    
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
