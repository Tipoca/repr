pub mod ascii;
pub mod perl;

use unconst::unconst;

use crate::repr::Repr;

/// `\0`
#[unconst]
pub const NUL: Repr<char> = Repr::from('\0');
/// `\a`, `\x07`
#[unconst]
pub const BEL: Repr<char> = Repr::from('\u{0007}');
/// `\t`, `\x09`
#[unconst]
pub const HT: Repr<char> = Repr::from('\t');
/// `\n`, `\x0A`
#[unconst]
pub const LF: Repr<char> = Repr::from('\n');
/// `\v`, `\x0B`
#[unconst]
pub const VT: Repr<char> = Repr::from('\u{000B}');
/// `\f`, `\x0C`
#[unconst]
pub const FF: Repr<char> = Repr::from('\u{000C}');
/// `\r`, `\x0D`
#[unconst]
pub const CR: Repr<char> = Repr::from('\r');
/// ` `, `\x20`
#[unconst]
pub const SP: Repr<char> = Repr::from(' ');
