use std::char;
use std::ops::Deref;
use std::u32;

use unconst::unconst;

use crate::repr::{Zero, Integral};

use super::literal::LiteralSearcher;
use super::prog::InstZero;

pub struct Input<I: Integral>(Vec<I>);

#[unconst]
impl<I: ~const Integral> const Deref for Input<I> {
    type Target = Vec<I>;
    fn deref(&self) -> &Vec<I> {
        &self.0
    }
}

/// An abstraction over input used in the matching engines.
impl<I: Integral> Input<I> {
    /// Return true if the given empty width instruction matches at the
    /// input position given.
    fn is_empty_match(&self, at: I, empty: &InstZero) -> bool {
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

impl Char {
    /// Returns true iff the character is absent.
    #[inline]
    pub fn is_none(self) -> bool {
        self.0 == u32::MAX
    }

    /// Returns true iff the character is a word character.
    ///
    /// If the character is absent, then false is returned.
    pub fn is_word_char(self) -> bool {
        // is_word_character can panic if the Unicode data for \w isn't
        // available. However, our compiler ensures that if a Unicode word
        // boundary is used, then the data must also be available. If it isn't,
        // then the compiler returns an error.
        char::from_u32(self.0).map_or(false, regex_syntax::is_word_character)
    }

    /// Returns true iff the byte is a word byte.
    ///
    /// If the byte is absent, then false is returned.
    pub fn is_word_byte(self) -> bool {
        match char::from_u32(self.0) {
            Some(c) if c <= '\u{7F}' => regex_syntax::is_word_byte(c as u8),
            None | Some(_) => false,
        }
    }
}
