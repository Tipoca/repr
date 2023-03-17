//! <https://en.m.wikipedia.org/wiki/Partition_of_a_set>
//! <https://en.m.wikipedia.org/wiki/Covering_problems>
//! <https://en.m.wikipedia.org/wiki/Exact_cover>
//! 
//! <https://en.wikipedia.org/wiki/Interval_tree>
//! <https://en.wikipedia.org/wiki/Segment_tree>
//! <https://en.wikipedia.org/wiki/Range_tree>
//! <https://en.wikipedia.org/wiki/Range_query_(data_structures)>
//! <https://en.wikipedia.org/wiki/Cartesian_tree>
//! <https://en.wikipedia.org/wiki/Prefix_tree>
//! <https://en.wikipedia.org/wiki/Suffix_tree>
//! <https://en.wikipedia.org/wiki/Suffix_automaton>
//! <https://en.wikipedia.org/wiki/LCP_array>
//! 
//! TODO(rnarkk)
//! - Uniqueness

use core::iter::FusedIterator;

use unconst::unconst;

use crate::context::Context;
use crate::repr::Repr;
use crate::traits::Integral;

#[unconst]
/// An iterator over all non-overlapping successive leftmost-first ranges.
#[derive_const(Clone, PartialEq)]
#[derive(Debug, Eq)]
pub struct Partition<'c, I: ~const Integral> {
    context: &'c Context<I>,
    repr: Repr<I>,
    last_end: usize,
    last_match: Option<usize>,
}

#[unconst]
impl<'c, I: ~const Integral> Partition<'c, I> {
    pub const fn new(context: &Context<I>, repr: Repr<I>) -> Self {
        Partition { context, repr, last_end: 0, last_match: None }
    }

    /// Return the context being searched.
    pub const fn context(&self) -> &'c Context<I> {
        self.context
    }

    /// Return the underlying regex.
    pub const fn repr(&self) -> &Repr<I> {
        &self.repr
    }
}

#[unconst]
impl<'c, I: ~const Integral> Iterator for Partition<'c, I> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
        if self.last_end > self.context.len() {
            return None;
        }
        let (start, end) = match self.repr.find_at(self.context, self.last_end) {
            None => return None,
            Some((start, end)) => (start, end),
        };
        if start == end {
            // This is an empty match. To ensure we make progress, start
            // the next search at the smallest possible starting position
            // of the next match following this one.
            self.last_end = end + 1;
            // Don't accept empty matches immediately following a match.
            // Just move on to the next match.
            if Some(end) == self.last_match {
                return self.next();
            }
        } else {
            self.last_end = end;
        }
        self.last_match = Some(end);
        Some((start, end))
    }
}

#[unconst]
impl<'c, I: ~const Integral> FusedIterator for Partition<'c, I> {}

#[unconst]
/// Match represents a single match of a regex in a haystack.
///
/// The lifetime parameter `'c` refers to the lifetime of the matched text.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Match<'c, I: ~const Integral> {
    context: &'c Context<I>,
    start: usize,
    end: usize,
}

#[unconst]
impl<'c, I: ~const Integral> Match<'c, I> {
    /// Creates a new match from the given haystack and byte offsets.
    #[inline]
    pub const fn new(context: &'c Context<I>, start: usize, end: usize) -> Self
    {
        Match { context, start, end }
    }

    /// Returns the matched text.
    #[inline]
    pub const fn as_slice(&self) -> &'c [I] {
        &self.context[self.start..self.end]
    }
}
