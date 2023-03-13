use core::char::from_u32;

use unconst::unconst;

use crate::repr::Integral;

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

// pub trait Char {
//     /// Returns true iff the character is absent.
//     #[inline]
//     pub fn is_none(self) -> bool {
//         self.0 == u32::MAX
//     }

//     /// Returns true iff the character is a word character.
//     ///
//     /// If the character is absent, then false is returned.
//     pub fn is_word_char(self) -> bool {
//         // is_word_character can panic if the Unicode data for \w isn't
//         // available. However, our compiler ensures that if a Unicode word
//         // boundary is used, then the data must also be available. If it isn't,
//         // then the compiler returns an error.
//         from_u32(self.0).map_or(false, regex_syntax::is_word_character)
//     }

//     /// Returns true iff the byte is a word byte.
//     ///
//     /// If the byte is absent, then false is returned.
//     pub fn is_word_byte(self) -> bool {
//         match from_u32(self.0) {
//             Some(c) if c <= '\u{7F}' => regex_syntax::is_word_byte(c as u8),
//             None | Some(_) => false,
//         }
//     }
// }
