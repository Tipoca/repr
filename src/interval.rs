//! <https://en.wikipedia.org/wiki/Boundary_(topology)>
//! <https://en.wikipedia.org/wiki/Partition_of_a_set>
//! <https://en.wikipedia.org/wiki/Sequence>

use core::{
    cmp::{max, min},
    iter::{IntoIterator, Step},
    ops::RangeInclusive
};

use unconst::unconst;

use crate::repr::Repr::{self, Zero, Or};
use crate::traits::Integral;

#[unconst]
// TODO(rnarkk) Does negative Interval (self.1 < self.0) have use case?
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
    /// If the intersection is empty, then don't intersect them.
    pub const fn and(self, other: Self) -> Repr<I> {
        match (max(self.0, other.0), min(self.1, other.1)) {
            (from, to) if from <= to => Repr::Interval(Self::new(from, to)),
            _ => Repr::Interval(self).or(Repr::Interval(other))
        }
    }
    
    /// Union the given overlapping Interval into this Interval.
    ///
    /// If the two Seqs aren't contiguous, then don't union them.
    pub const fn or(self, other: Self) -> Repr<I> {
        if max(self.0, other.0) <= min(self.1, other.1).succ() {
            Repr::Interval(Self::new(min(self.0, other.0), max(self.1, other.1)))
        } else {
            Or(Box::new(Repr::Interval(self)), Box::new(Repr::Interval(other)))
        }
    }
    
    // /// Compute the symmetric difference the given Interval from this Interval. This
    // /// returns the union of the two Intervals minus its intersection.
    // pub const fn xor(self, other: Self) -> Repr<I> {
    //     let or = match self.or(other) {
    //         Or(lhs, rhs) => return Or(lhs, rhs),
    //         Repr::Interval(or) => or,
    //     };
    //     let and = match self.and(other) {
    //         Or(lhs, rhs) => return Or(lhs, rhs),
    //         Repr::Interval(and) => and,
    //     };
    //     or.sub(and)
    // }
    
    // /// Subtract the given Interval from this Interval and return the resulting
    // /// Seqs.
    // ///
    // /// If subtraction would result in an empty Interval, then no Seqs are
    // /// returned.
    // /// 
    // /// other.0 <= self.0 <= self.1 <= other.1 (self <= other) => Zero
    // /// self.0 <= other.0 <= other.1 <= self.1 (other <= self) => (lower, upper)
    // /// self.0 <= other.0 <= self.1 <= other.1 => (lower, None)
    // /// other.0 <= self.0 <= other.1 <= self.1 => (None, uppper)
    // pub const fn sub(self, other: Self) -> Repr<I> {
    //     if self.le(&other) {
    //         return Zero;
    //     }
    //     if self.contiguous(&other) {
    //         return Repr::Interval(self.clone());
    //     }
    //     let mut ret = (None, None);
    //     if self.0 < other.0 {
    //         ret.0 = Some(Self::new(self.0, other.0.pred()));
    //     }
    //     if other.1 < self.1 {
    //         let range = Self::new(other.1.succ(), self.1);
    //         if ret.0.is_none() {
    //             ret.0 = Some(range);
    //         } else {
    //             ret.1 = Some(range);
    //         }
    //     }
    //     ret
    // }

    /// 
    pub const fn contiguous(&self, other: &Self) -> bool {
        max(self.0, other.0) <= min(self.1, other.1)
    }

    // /// Negate this Interval.
    // ///
    // /// For all `a` where `a` is any element, if `a` is in this interval, then it will not be in this set after negation.
    // pub const fn not(self) -> Repr<I> {
    //     Self::full().sub(self)
    // }

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
