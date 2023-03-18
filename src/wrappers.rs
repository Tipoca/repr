use unconst::unconst;

use crate::repr::Repr;
use crate::traits::Integral;

#[unconst]
pub const fn one<I: ~const Integral>(i: I) -> Repr<I> {
    Repr::one(i)
}
