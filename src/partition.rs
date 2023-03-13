use core::iter::FusedIterator;

use unconst::unconst;

use crate::context::Context;
use crate::repr::{Repr, Integral};

#[unconst]
/// An iterator over all non-overlapping successive leftmost-first matches.
#[derive(Debug)]
pub struct Partition<'t, I: ~const Integral> {
    repr: Repr<I>,
    context: &'t Context<I>,
    last_end: usize,
    last_match: Option<usize>,
}

#[unconst]
impl<'t, I: ~const Integral> Partition<'t, I> {
    /// Return the context being searched.
    pub fn context(&self) -> &'t Context<I> {
        self.context
    }

    /// Return the underlying regex.
    pub fn repr(&self) -> &Repr<I> {
        &self.repr
    }
}

#[unconst]
impl<'t, I: ~const Integral> Iterator for Partition<'t, I> {
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
impl<'t, I: ~const Integral> FusedIterator for Partition<'t, I>
    // R::Text: 't + AsRef<[u8]>,
{}
