pub mod ascii;
pub mod perl;

use unconst::unconst;

use crate::repr::Repr;

#[unconst]
/// `\0`, 'null character'
pub const NUL: Repr<char> = Repr::from('\0');
#[unconst]
/// `\a`, `\x07`
pub const BEL: Repr<char> = Repr::from('\u{0007}');
#[unconst]
/// `\b`, `\x08`, 'backspace'
pub const BS: Repr<char> = Repr::from('\u{0008}');
#[unconst]
/// `\t`, `\x09`, 'tab'
pub const HT: Repr<char> = Repr::from('\t');
#[unconst]
/// `\n`, `\x0A`, 'line feed', 'new line'
pub const LF: Repr<char> = Repr::from('\n');
#[unconst]
/// `\v`, `\x0B`, 'vertical tab'
pub const VT: Repr<char> = Repr::from('\u{000B}');
#[unconst]
/// `\f`, `\x0C`, 'form feed'
pub const FF: Repr<char> = Repr::from('\u{000C}');
#[unconst]
/// `\r`, `\x0D`, 'carriage return'
pub const CR: Repr<char> = Repr::from('\r');
#[unconst]
/// ` `, `\x20`
pub const SP: Repr<char> = Repr::from(' ');

pub const fn escape(c: char) -> std::sync::LazyLock<Repr<char>> {
    match c {
        'b' => BS,
        'f' => FF,
        'n' => LF,
        'r' => CR,
        't' => HT,
        'v' => VT,
        '0' => NUL,
        _ => unimplemented!(),
    }
}
