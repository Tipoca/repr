use core::iter::FusedIterator;

use unconst::unconst;

use crate::context::Context;
use crate::repr::{Repr, Integral};


#[unconst]
/// An iterator over all non-overlapping successive leftmost-first ranges.
#[derive(Debug)]
pub struct Partition<'c, I: ~const Integral> {
    repr: Repr<I>,
    context: &'c Context<I>,
    last_end: usize,
    last_match: Option<usize>,
}

#[unconst]
impl<'c, I: ~const Integral> Partition<'c, I> {
    /// Return the context being searched.
    pub fn context(&self) -> &'c Context<I> {
        self.context
    }

    /// Return the underlying regex.
    pub fn repr(&self) -> &Repr<I> {
        &self.repr
    }
}

#[unconst]
impl<'c, I: ~const Integral> Iterator for Partition<'c, I> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
        if self.last_end > self.context.as_ref().len() {
            return None;
        }
        let (s, e) = match self.repr.find_at(self.context, self.last_end) {
            None => return None,
            Some((s, e)) => (s, e),
        };
        if s == e {
            // This is an empty match. To ensure we make progress, start
            // the next search at the smallest possible starting position
            // of the next match following this one.
            self.last_end = e + 1;
            // Don't accept empty matches immediately following a match.
            // Just move on to the next match.
            if Some(e) == self.last_match {
                return self.next();
            }
        } else {
            self.last_end = e;
        }
        self.last_match = Some(e);
        Some((s, e))
    }
}

#[unconst]
impl<'c, I: ~const Integral> FusedIterator for Partition<'c, I> {}
