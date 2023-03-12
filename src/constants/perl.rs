use unconst::unconst;

use crate::repr::Repr;

#[unconst]
/// `\d`
pub const DIGIT: Repr<char> = Repr::from('0'..'9');
#[unconst]
// /// `\s`
// pub const SPACE: Repr<char> = Repr::;
#[unconst]
/// `\w`
pub const WORD: Repr<char> = Repr::from('A'..'Z') | ('a'..'z');
