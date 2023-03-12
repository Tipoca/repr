// pub mod ascii;
// pub mod perl;

use unconst::unconst;

use crate::repr::Repr;

/// `\0`
#[unconst]
pub const NUL: Repr<char> = Repr::from('\0');
/// `\t`
#[unconst]
pub const HT: Repr<char> = Repr::from('\t');
/// `\n`
#[unconst]
pub const LF: Repr<char> = Repr::from('\n');
/// `\v`
#[unconst]
pub const VT: Repr<char> = Repr::from('\u{000B}');
/// `\r`
#[unconst]
pub const CR: Repr<char> = Repr::from('\r');
/// ` `
#[unconst]
pub const SP: Repr<char> = Repr::from(' ');

#[unconst]
pub const DIGIT: Repr<char> = Repr::from('0'..'9');
#[unconst]
pub const WORD: Repr<char> = Repr::from('A'..'Z') | ('a'..'z');
