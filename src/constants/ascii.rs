//! The available ASCII character classes.

use unconst::unconst;

use crate::constants::*;
use crate::repr::Repr;

#[unconst]
/// `[0-9A-Za-z]`, `[:alnum:]`
pub const ALNUM: Repr<char> = DIGIT | UPPER | LOWER;
#[unconst]
/// `[A-Za-z]`, `[:alpha:]`
pub const ALPHA: Repr<char> = UPPER | LOWER;
#[unconst]
/// `[\x00-\x7F]`, `[:ascii:]`
pub const ASCII: Repr<char> = Repr::from('\x00'..'\x7F');
#[unconst]
/// `[ \t]`, `[:blank:]`
pub const BLANK: Repr<char> = SP | HT;
#[unconst]
/// `[\x00-\x1F\x7F]`, `[:cntrl:]`
pub const CNTRL: Repr<char> = Repr::from('\x00'..'\x1F') | '\x7F';
#[unconst]
/// `[0-9]`, `[:digit:]`
pub const DIGIT: Repr<char> = Repr::from('0'..'9');
#[unconst]
/// `[!-~]`, `[:graph:]`
pub const GRAPH: Repr<char> = Repr::from('!'..'~');
#[unconst]
/// `[a-z]`, `[:lower:]`
pub const LOWER: Repr<char> = Repr::from('a'..'z');
#[unconst]
/// `[ -~]`, `[:print:]`
pub const PRINT: Repr<char> = Repr::from(' '..'~');
#[unconst]
/// `[!-/:-@\[-`{-~]`, `[:punct:]`
pub const PUNCT: Repr<char>
    = Repr::from('!'..'/') | (':'..'@') | ('['..'`') | ('{'..'~');
#[unconst]
/// `[\t\n\v\f\r ]`, `[:space:]`
pub const SPACE: Repr<char> = HT | LF | VT | FF | CR | SP;
#[unconst]
/// `[A-Z]`, `[:upper:]`
pub const UPPER: Repr<char> = Repr::from('A'..'Z');
#[unconst]
/// `[0-9A-Za-z_]`, `[:word:]`
pub const WORD: Repr<char> = ALNUM | SP;
/// `[0-9A-Fa-f]`, `[:xdigit:]`
pub const XDIGIT: Repr<char> = DIGIT | ('A'..'F') | ('a'..'f');
