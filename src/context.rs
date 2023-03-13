use core::ops::Deref;

use unconst::unconst;

use crate::repr::{Zero, Integral};

use crate::regex::LiteralSearcher;
use crate::regex::InstZero;

#[unconst]
pub struct Context<I: ~const Integral>(Vec<I>);

#[unconst]
impl<I: ~const Integral> const Deref for Context<I> {
    type Target = Vec<I>;

    fn deref(&self) -> &Vec<I> {
        &self.0
    }
}

/// An abstraction over input used in the matching engines.
impl Context<char> {
    /// Return true if the given empty width instruction matches at the
    /// input position given.
    fn is_empty_match(&self, at: usize, empty: &InstZero) -> bool {
        match empty.look {
            Zero::StartLine => {
                let c = &self[at - 1];
                at == 0 || c == '\n'
            }
            Zero::EndLine => {
                let c = &self[at + 1];
                at == self.len() || c == '\n'
            }
            Zero::StartText => at == 0,
            Zero::EndText => at == self.len(),
            Zero::WordBoundary => {
                let (c1, c2) = (&self[at - 1], &self[at + 1]);
                c1.is_word_char() != c2.is_word_char()
            }
            Zero::NotWordBoundary => {
                let (c1, c2) = (&self[at - 1], &self[at + 1]);
                c1.is_word_char() == c2.is_word_char()
            }
            Zero::WordBoundaryAscii => {
                let (c1, c2) = (&self[at - 1], &self[at + 1]);
                c1.is_word_byte() != c2.is_word_byte()
            }
            Zero::NotWordBoundaryAscii => {
                let (c1, c2) = (&self[at - 1], &self[at + 1]);
                c1.is_word_byte() == c2.is_word_byte()
            }
            Zero::Any => unimplemented!()
        }
    }

    /// Scan the input for a matching prefix.
    fn prefix_at(
        &self,
        prefixes: &LiteralSearcher<I>,
        at: usize,
    ) -> Option<I>
    {
        prefixes.find(&self[at..]).map(|(s, _)| self[at + s])
    }
}
