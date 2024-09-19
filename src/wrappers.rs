use unconst::unconst;

use crate::repr::Repr;
use crate::traits::Integral;

#[unconst]
pub const fn zero<I: ~const Integral>() -> Repr<I> {
    Repr::zero()
}

#[unconst]
pub const fn one<I: ~const Integral>() -> Repr<I> {
    Repr::one()
}

#[unconst]
pub const fn seq<I, M>(is: M) -> Repr<I>
where
    I: ~const Integral,
    M: ~const IntoIterator<Item = I>,
    M::IntoIter: ExactSizeIterator,
{
    Repr::seq(is)
}

#[unconst]
pub const fn interval<I: ~const Integral>(from: I, to: I) -> Repr<I> {
    Repr::interval(from, to)
}
