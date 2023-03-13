use alloc::vec::IntoIter;
use core::{
    iter::{Enumerate, FusedIterator},
    slice::Iter
};

use unconst::unconst;

use crate::context::Context;
use crate::repr::{Repr, Integral};


#[unconst]
/// An iterator over all non-overlapping successive leftmost-first ranges.
///
/// The iterator yields a `Match` value. The iterator stops when no more
/// matches can be found.
///
/// `'r` is the lifetime of the compiled regular expression and `'c` is the
/// lifetime of the matched string.
#[derive_const(PartialEq)]
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


/// A set of matches returned by a regex set.
#[derive(Clone, Debug)]
pub struct SetMatches {
    matched_any: bool,
    matches: Vec<bool>,
}

impl SetMatches {
    /// Whether this set contains any matches.
    pub fn matched_any(&self) -> bool {
        self.matched_any
    }

    /// Whether the regex at the given index matched.
    ///
    /// The index for a regex is determined by its insertion order upon the
    /// initial construction of a `RegexSet`, starting at `0`.
    ///
    /// # Panics
    ///
    /// If `regex_index` is greater than or equal to `self.len()`.
    pub fn matched(&self, regex_index: usize) -> bool {
        self.matches[regex_index]
    }

    /// The total number of regexes in the set that created these matches.
    pub fn len(&self) -> usize {
        self.matches.len()
    }

    /// Returns an iterator over indexes in the regex that matched.
    ///
    /// This will always produces matches in ascending order of index, where
    /// the index corresponds to the index of the regex that matched with
    /// respect to its position when initially building the set.
    pub fn iter(&self) -> SetMatchesIter<'_> {
        SetMatchesIter((&*self.matches).into_iter().enumerate())
    }
}

impl IntoIterator for SetMatches {
    type IntoIter = SetMatchesIntoIter;
    type Item = usize;

    fn into_iter(self) -> Self::IntoIter {
        SetMatchesIntoIter(self.matches.into_iter().enumerate())
    }
}

impl<'a> IntoIterator for &'a SetMatches {
    type IntoIter = SetMatchesIter<'a>;
    type Item = usize;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// An owned iterator over the set of matches from a regex set.
///
/// This will always produces matches in ascending order of index, where the
/// index corresponds to the index of the regex that matched with respect to
/// its position when initially building the set.
#[derive(Debug)]
pub struct SetMatchesIntoIter(Enumerate<IntoIter<bool>>);

impl Iterator for SetMatchesIntoIter {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        loop {
            match self.0.next() {
                None => return None,
                Some((_, false)) => {}
                Some((i, true)) => return Some(i),
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl DoubleEndedIterator for SetMatchesIntoIter {
    fn next_back(&mut self) -> Option<usize> {
        loop {
            match self.0.next_back() {
                None => return None,
                Some((_, false)) => {}
                Some((i, true)) => return Some(i),
            }
        }
    }
}

impl FusedIterator for SetMatchesIntoIter {}

/// A borrowed iterator over the set of matches from a regex set.
///
/// The lifetime `'a` refers to the lifetime of a `SetMatches` value.
///
/// This will always produces matches in ascending order of index, where the
/// index corresponds to the index of the regex that matched with respect to
/// its position when initially building the set.
#[derive(Clone, Debug)]
pub struct SetMatchesIter<'a>(Enumerate<Iter<'a, bool>>);

impl<'a> Iterator for SetMatchesIter<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        loop {
            match self.0.next() {
                None => return None,
                Some((_, &false)) => {}
                Some((i, &true)) => return Some(i),
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a> DoubleEndedIterator for SetMatchesIter<'a> {
    fn next_back(&mut self) -> Option<usize> {
        loop {
            match self.0.next_back() {
                None => return None,
                Some((_, &false)) => {}
                Some((i, &true)) => return Some(i),
            }
        }
    }
}

impl<'a> FusedIterator for SetMatchesIter<'a> {}
