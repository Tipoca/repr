use core::ops::{BitOr, BitAnd, BitXor, Range, Mul, RangeFull, RangeFrom};

use unconst::unconst;

use crate::interval::Interval;
use crate::repr::{Repr, Integral};

#[unconst]
impl<I: ~const Integral> const BitAnd<Self> for Repr<I> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        self.and(rhs)
    }
}

#[unconst]
impl<I: ~const Integral> const BitAnd<I> for Repr<I> {
    type Output = Self;

    fn bitand(self, rhs: I) -> Self {
        self.and(Repr::One(rhs))
    }
}

// impl const BitAnd<Repr<char>> for &str {
//     type Output = Repr<char>;

//     fn bitand(self, rhs: Repr<char>) -> Self::Output {
//         rhs.clone().and(self)
//     }
// }

// impl const BitAnd<Range<u8>> for Repr<char> {
//     type Output = Self;

//     fn bitand(self, rhs: Range<u8>) -> Repr<char> {
//         self.and(rhs)
//     }
// }

#[unconst]
impl<I: ~const Integral> const BitAnd<Range<I>> for Repr<I> {
    type Output = Self;

    fn bitand(self, rhs: Range<I>) -> Self::Output {
        self.and(Repr::Interval(rhs.into()))
    }
}

#[unconst]
impl<I: ~const Integral, T: Into<Self>> const BitAnd<[T; 1]> for Repr<I> {
    type Output = Self;

    fn bitand(self, rhs: [T; 1]) -> Self::Output {
        self.and(Repr::from(rhs) * ..)
    }
}

#[unconst]
impl<I: ~const Integral> const BitOr<Self> for Repr<I> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        self.or(rhs)
    }
}

#[unconst]
impl<I: ~const Integral> const BitOr<I> for Repr<I> {
    type Output = Self;
    
    fn bitor(self, rhs: I) -> Self {
        self.or(Self::One(rhs))
    }
}

// impl const BitOr<&str> for Repr<char> {
//     type Output = Repr<char>;

//     fn bitor(self, rhs: &str) -> Repr<char> {
//         self.or(Self::One(rhs))
//     }
// }

// impl const BitOr<Repr<char>> for &str {
//     type Output = Repr<char>;

//     fn bitor(self, rhs: Repr<char>) -> Repr<char> {
//         Self::One(rhs).or(self)
//     }
// }

#[unconst]
impl<I: ~const Integral> const BitOr<Range<I>> for Repr<I> {
    type Output = Self;

    fn bitor(self, rhs: Range<I>) -> Self {
        self.or(Repr::Interval(rhs.into()))
    }
}

#[unconst]
impl<I: ~const Integral, T: Into<Self>> const BitOr<[T; 1]> for Repr<I> {
    type Output = Self;

    fn bitor(self, rhs: [T; 1]) -> Self {
        self.or(Repr::from(rhs) * ..)
    }
}

#[unconst]
impl<I: ~const Integral> const Mul<RangeFull> for Repr<I> {
    type Output = Self;

    fn mul(self, rhs: RangeFull) -> Self {
        Self::Exp(box self)
    }
}

#[unconst]
impl<I: ~const Integral> const Mul<RangeFrom<usize>> for Repr<I> {
    type Output = Self;

    fn mul(self, rhs: RangeFrom<usize>) -> Self {
        Self::Mul(box self.repeat(rhs.start), box Self::Exp(box self))
    }
}

#[unconst]
impl<I: ~const Integral> const BitAnd<Self> for Interval<I> {
    type Output = Option<Self>;

    fn bitand(self, rhs: Self) -> Self::Output {
        self.and(rhs)
    }
}

#[unconst]
impl<I: ~const Integral> const BitOr<Self> for Interval<I> {
    type Output = Option<Self>;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.or(rhs)
    }
}

#[unconst]
impl<I: ~const Integral> const BitXor<Self> for Interval<I> {
    type Output = (Option<Self>, Option<Self>);

    fn bitxor(self, rhs: Self) -> Self::Output {
        self.xor(rhs)
    }
}
