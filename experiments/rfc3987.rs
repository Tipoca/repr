//! <https://www.rfc-editor.org/rfc/rfc3987>

use unconst::unconst;

pub use repr::{
    Repr,
    constants::{EMPTY, DIGIT, SPACE, WORD as ALPHA},
};

#[unconst]
pub const SUB_DELIMS: Repr<char>
    = EMPTY * '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=';
#[unconst]
pub const PCT_ENCODED: Repr<char> = '%' * HEXDIG * HEXDIG;
// pub const UCSCHAR: Repr<char>
//     = %xA0..D7FF | %xF900..FDCF | %xFDF0..FFEF
//     | %x10000..1FFFD | %x20000..2FFFD | %x30000..3FFFD
//     | %x40000..4FFFD | %x50000..5FFFD | %x60000..6FFFD
//     | %x70000..7FFFD | %x80000..8FFFD | %x90000..9FFFD
//     | %xA0000..AFFFD | %xB0000..BFFFD | %xC0000..CFFFD
//     | %xD0000..DFFFD | %xE1000..EFFFD;
#[unconst]
pub const UNRESERVED: Repr<char>
    = ALPHA | DIGIT | '-' | '.' | '_' | '~' | UCSCHAR;
#[unconst]
pub const USERINFO: Repr<char>
    = (UNRESERVED | PCT_ENCODED | SUB_DELIMS | ':').exp();
#[unconst]
pub const AUTHORITY: Repr<char> = (USERINFO * '@')? * ihost * (':' * port);
#[unconst]
pub const HIER_PART: Repr<char>
    = "//" * AUTHORITY * ipath_abempty
    | ipath_absolute
    | ipath_rootless
    | ipath_empty;
pub const SCHEME: Repr<char> = ALPHA * (ALPHA | DIGIT | '+' | '-' | '.').exp();
pub const IRI: Repr<char>
    = SCHEME * ':' * HIER_PART * ('?' * iquery)? * ('#' * ifragment)?;
