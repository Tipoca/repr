use unconst::unconst;

use crate::repr::Repr;
use crate::traits::Integral;

#[unconst]
pub const fn one<I: ~const Integral>(i: I) -> Repr<I> {
    Repr::one(i)
}

#[unconst]
pub const fn seq<I, M>(is: M) -> Repr<I>
    where I: ~const Integral,
          M: ~const IntoIterator<Item = I>
{
    Repr::seq(is)
}
