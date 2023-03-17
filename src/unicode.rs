use core::char::from_u32;

use unconst::unconst;

use crate::context::Context;
use crate::interval::Interval;
use crate::repr::{Repr, Zero};
use crate::traits::Integral;

#[unconst]
impl const Integral for char {
    const MIN: Self = '\x00';
    const MAX: Self = '\u{10FFFF}';
    fn succ(self) -> Self {
        match self {
            '\u{D7FF}' => '\u{E000}',
            c => from_u32((c as u32).checked_add(1).unwrap()).unwrap(),
        }
    }
    fn pred(self) -> Self {
        match self {
            '\u{E000}' => '\u{D7FF}',
            c => from_u32((c as u32).checked_sub(1).unwrap()).unwrap(),
        }
    }
}

#[unconst]
impl Repr<char> {
    /// `.` expression that matches any character except for `\n`. To build an
    /// expression that matches any character, including `\n`, use the `any`
    /// method.
    pub const fn dot() -> Self {
        Self::Or(box Self::Interval(Interval('\0', '\x09')),
                 box Self::Interval(Interval('\x0B', '\u{10FFFF}')))
    }

    // /// `(?s).` expression that matches any character, including `\n`. To build an
    // /// expression that matches any character except for `\n`, then use the
    // /// `dot` method.
    // pub const fn any() -> Self {
    //     Self::Interval(Interval('\0', '\u{10FFFF}'))
    // }
}

/// An abstraction over input used in the matching engines.
impl Context<char> {
    /// Return true if the given empty width instruction matches at the
    /// input position given.
    pub fn yes(&self, index: usize, look: &Zero) -> bool {
        match look {
            Zero::StartLine => {
                let c = &self[index - 1];
                index == 0 || c == &'\n'
            }
            Zero::EndLine => {
                let c = &self[index + 1];
                index == self.len() || c == &'\n'
            }
            Zero::StartText => index == 0,
            Zero::EndText => index == self.len(),
            Zero::WordBoundary => {
                let (c1, c2) = (&self[index - 1], &self[index + 1]);
                is_word_char(c1) != is_word_char(c2)
            }
            Zero::NotWordBoundary => {
                let (c1, c2) = (&self[index - 1], &self[index + 1]);
                is_word_char(c1) == is_word_char(c2)
            }
            Zero::WordBoundaryAscii => {
                let (c1, c2) = (&self[index - 1], &self[index + 1]);
                is_word_byte(c1) != is_word_byte(c2)
            }
            Zero::NotWordBoundaryAscii => {
                let (c1, c2) = (&self[index - 1], &self[index + 1]);
                is_word_byte(c1) == is_word_byte(c2)
            }
            Zero::Any => unimplemented!()
        }
    }
}

#[unconst]
/// Returns true iff the character is a word character.
///
/// If the character is absent, then false is returned.
pub const fn is_word_char(c: &char) -> bool {
    // is_word_character can panic if the Unicode data for \w isn't
    // available. However, our compiler ensures that if a Unicode word
    // boundary is used, then the data must also be available. If it isn't,
    // then the compiler returns an error.
    regex_syntax::is_word_character(*c)
}

#[unconst]
/// Returns true iff the byte is a word byte.
///
/// If the byte is absent, then false is returned.
pub const fn is_word_byte(c: &char) -> bool {
    regex_syntax::is_word_byte(*c as u8)
}

pub const fn escape(c: char) -> char {
    match c {
        'b' => '\u{0008}',  // Backspace
        'f' => '\u{000C}',  // Form feed
        'n' => '\n',  // New line
        'r' => '\r',  // Carriage return
        't' => '\t',  // Tab
        'v' => '\u{000B}',  // Vertical tab
        '0' => '\0',  // Null character
        _ => panic!()
    }
}
