use core::ops::Range;

use unconst::unconst;

use crate::interval::Interval;
use crate::repr::Repr;
use crate::seq::Seq;
use crate::traits::Integral;

#[unconst]
impl<I: ~const Integral> const From<I> for Seq<I> {
    fn from(value: I) -> Self {
        Self::one(value)
    }
}

impl From<&str> for Seq<char> {
    fn from(value: &str) -> Self {
        Self::new(value.chars())
    }
}

#[unconst]
impl<I: ~const Integral> const From<Range<I>> for Interval<I> {
    fn from(range: Range<I>) -> Self {
        Interval(range.start, range.end)
    }
}

#[unconst]
impl<I: ~const Integral> const From<I> for Repr<I> {
    fn from(value: I) -> Repr<I> {
        Self::One(Seq::one(value))
    }
}

#[unconst]
impl<I: ~const Integral> const From<Range<I>> for Repr<I> {
    fn from(range: Range<I>) -> Self {
        Repr::Interval(range.into())
    }
}

#[unconst]
impl<I: ~const Integral, T: Into<Repr<I>>> const From<[T; 1]> for Repr<I> {
    fn from(value: [T; 1]) -> Repr<I> {
        value.into_iter().nth(0).unwrap().into().exp()
    }
}
