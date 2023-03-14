//! <https://en.wikipedia.org/wiki/Boundary_(topology)>
//! <https://en.wikipedia.org/wiki/Partition_of_a_set>
//! <https://en.wikipedia.org/wiki/Sequence>

// This module contains an *internal* implementation of interval sets.
//
// The primary invariant that interval sets guards is canonical ordering. That
// is, every interval set contains an ordered sequence of intervals where
// no two intervals are overlapping or adjacent. While this invariant is
// occasionally broken within the implementation, it should be impossible for
// callers to observe it.
//
// Since case folding (as implemented below) breaks that invariant, we roll
// that into this API even though it is a little out of place in an otherwise
// generic interval set. (Hence the reason why the `unicode` module is imported
// here.)
//
// Some of the implementation complexity here is a result of me wanting to
// preserve the sequential representation without using additional memory.
// In many cases, we do use linear extra memory, but it is at most 2x and it
// is amortized. If we relaxed the memory requirements, this implementation
// could become much simpler. The extra memory is honestly probably OK, but
// character classes (especially of the Unicode variety) can become quite
// large, and it would be nice to keep regex compilation snappy even in debug
// builds. (In the past, I have been careless with this area of code and it has
// caused slow regex compilations in debug mode, so this isn't entirely
// unwarranted.)
//
// Tests on this are relegated to the public API of HIR in src/hir.rs.
// Tests for interval sets are written in src/hir.rs against the public API.
use core::{
    cmp::{max, min},
    iter::{IntoIterator, Step},
    ops::RangeInclusive
};

use unconst::unconst;

use crate::repr::Integral;

#[unconst]
// TODO(rnarkk) Does negative Interval (self.1 < self.0) have use case?
// TODO(rnarkk) check if I..I always yield valid characters
/// A character class, regardless of its character type, is represented by a
/// sequence of non-overlapping non-adjacent ranges of characters.
#[derive_const(Clone, Default, PartialEq, PartialOrd, Ord)]
#[derive(Copy, Debug, Eq)]
pub struct Interval<I: ~const Integral>(pub I, pub I);

#[unconst]
impl<I: ~const Integral> Interval<I> {
    pub const fn new(from: I, to: I) -> Self {
        if from <= to {
            Interval(from, to)
        } else {
            Interval(to, from)
        }
    }

    pub const fn full() -> Self {
        Interval(I::MIN, I::MAX)
    }
    
    /// Intersect this Interval with the given Interval and return the result.
    ///
    /// If the intersection is empty, then this returns `None`.
    pub const fn and(self, other: Self) -> Option<Self> {
        match (max(self.0, other.0), min(self.1, other.1)) {
            (from, to) if from <= to => Some(Self::new(from, to)),
            _ => None
        }
    }
    
    /// Union the given overlapping Interval into this Interval.
    ///
    /// If the two Seqs aren't contiguous, then this returns `None`.
    pub const fn or(self, other: Self) -> Option<Self> {
        match (max(self.0, other.0), min(self.1, other.1)) {
            (from, to) if from <= to.succ() => Some(Self::new(from, to)),
            _ => None
        }
    }
    
    /// Compute the symmetric difference the given Interval from this Interval. This
    /// returns the union of the two Seqs minus its intersection.
    pub const fn xor(self, other: Self) -> (Option<Self>, Option<Self>) {
        let or = match self.or(other) {
            None => return (Some(self.clone()), Some(other.clone())),
            Some(or) => or,
        };
        let and = match self.and(other) {
            None => return (Some(self.clone()), Some(other.clone())),
            Some(and) => and,
        };
        or.sub(and)
    }
    
    /// Subtract the given Interval from this Interval and return the resulting
    /// Seqs.
    ///
    /// If subtraction would result in an empty Interval, then no Seqs are
    /// returned.
    /// 
    /// other.0 <= self.0 <= self.1 <= other.1 (self <= other) => (None, None)
    /// self.0 <= other.0 <= other.1 <= self.1 (other <= self) => (lower, upper)
    /// self.0 <= other.0 <= self.1 <= other.1 => (lower, None)
    /// other.0 <= self.0 <= other.1 <= self.1 => (None, uppper)
    pub const fn sub(self, other: Self) -> (Option<Self>, Option<Self>) {
        if self.le(&other) {
            return (None, None);
        }
        if self.and(other).is_none() {
            return (Some(self.clone()), None);
        }
        let mut ret = (None, None);
        if self.0 < other.0 {
            ret.0 = Some(Self::new(self.0, other.0.pred()));
        }
        if other.1 < self.1 {
            let range = Self::new(other.1.succ(), self.1);
            if ret.0.is_none() {
                ret.0 = Some(range);
            } else {
                ret.1 = Some(range);
            }
        }
        ret
    }

    /// Negate this Interval.
    ///
    /// For all `a` where `a` is any element, if `a` is in this interval, then it will not be in this set after negation.
    pub const fn not(self) -> (Option<Self>, Option<Self>) {
        Self::full().sub(self)
    }

    // TODO(rnarkk) Why not simply `other.0 <= self.0 && self.1 <= other.1`
    /// a ⊆ b
    pub const fn le(&self, other: &Self) -> bool {
        (other.0 <= self.0 && self.0 <= other.1)
        && (other.0 <= self.1 && self.1 <= other.1)
    }

    /// i ∈ a
    pub fn has(&self, i: I) -> bool {
        if self.0 <= i && i <= self.1 {
            true
        } else {
            false
        }
    }

    pub const fn len(&self) -> usize {
        <I as Step>::steps_between(&self.0, &self.1).unwrap()
    }
}

#[unconst]
impl<I: ~const Integral> const IntoIterator for Interval<I> {
    type Item = I;
    type IntoIter = RangeInclusive<I>;

    fn into_iter(self) -> Self::IntoIter {
        self.0..=self.1
    }
}

impl Interval<char> {
    /// Returns true if and only if this character class will either match
    /// nothing or only ASCII bytes. Stated differently, this returns false
    /// if and only if this class contains a non-ASCII codepoint.
    pub fn is_all_ascii(&self) -> bool {
        self.1 <= '\x7F'
    }
}
