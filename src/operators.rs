use core::{
    clone::Clone,
    cmp::{PartialEq, PartialOrd, Ordering},
    fmt::{self, Debug},
    ops::{BitOr, BitAnd, BitXor, Range, Add, Mul, RangeFull, RangeFrom}
};

use unconst::unconst;

use crate::interval::Interval;
use crate::repr::Repr;
use crate::traits::Integral;

#[unconst]
impl<I: ~const Integral> const Debug for Repr<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Repr::True(_) => panic!("True variant cannot be cloned"),
            Repr::One(seq) => write!(f, "One({:?})", seq),
            Repr::Interval(interval) => write!(f, "Interval({:?})", interval),
            Repr::Mul(lhs, rhs) => write!(f, "Mul({:?}, {:?})", lhs, rhs),
            Repr::Or(lhs, rhs) => write!(f, "Or({:?}, {:?})", lhs, rhs),
            Repr::Inf(repr) => write!(f, "Inf({:?})", repr),
            Repr::Add(lhs, rhs) => write!(f, "Add({:?}, {:?})", lhs, rhs),
            Repr::And(lhs, rhs) => write!(f, "And({:?}, {:?})", lhs, rhs),
            _ => unimplemented!()
        }
    }
}

#[unconst]
impl<I: ~const Integral> const Clone for Repr<I> {
    fn clone(&self) -> Self {
        match self {
            Repr::True(_) => panic!("True variant cannot be cloned"),
            Repr::One(seq) => Repr::One(seq.clone()),
            Repr::Interval(interval) => Repr::Interval(interval.clone()),
            Repr::Mul(lhs, rhs) => Repr::Mul(lhs.clone(), rhs.clone()),
            Repr::Or(lhs, rhs) => Repr::Or(lhs.clone(), rhs.clone()),
            Repr::Inf(repr) => Repr::Inf(repr.clone()),
            Repr::Add(lhs, rhs) => Repr::Add(lhs.clone(), rhs.clone()),
            Repr::And(lhs, rhs) => Repr::And(lhs.clone(), rhs.clone()),
            _ => unimplemented!()
        }
    }
}

#[unconst]
impl<I: ~const Integral> const PartialEq for Repr<I> {
    fn eq(&self, other: &Self) -> bool {
        self.eq(other)
    }
}

#[unconst]
impl<I: ~const Integral> Eq for Repr<I> {}

#[unconst]
impl<I: ~const Integral> const PartialOrd for Repr<I> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else if self.le(other) {
            Some(Ordering::Less)
        } else {
            Some(Ordering::Greater)
        }
    }
}

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
        self.and(Repr::One(rhs.into()))
    }
}

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
        self.and(Repr::from(rhs).inf())
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
        self.or(Self::One(rhs.into()))
    }
}

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
        self.or(Repr::from(rhs).inf())
    }
}

#[unconst]
impl<I: ~const Integral> const Mul<Self> for Repr<I> {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        self.mul(other)
    }
}

#[unconst]
impl<I: ~const Integral> const Mul<RangeFull> for Repr<I> {
    type Output = Self;

    fn mul(self, _: RangeFull) -> Self {
        self.inf()
    }
}

#[unconst]
impl<I: ~const Integral> const Mul<RangeFrom<usize>> for Repr<I> {
    type Output = Self;

    fn mul(self, rhs: RangeFrom<usize>) -> Self {
        self.clone().rep(rhs.start).mul(self.inf())
    }
}

#[unconst]
impl<I: ~const Integral> const Add<Self> for Repr<I> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        self.add(other)
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
