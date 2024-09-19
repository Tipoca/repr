mod operators;
// mod pattern;

use alloc::{boxed::Box, vec};

use unconst::unconst;

use crate::interval::Interval;
use crate::seq::Seq;
use crate::traits::Integral;

#[unconst]
pub enum Repr<I: ~const Integral> {
    True(Box<dyn Fn(Seq<I>) -> bool>),
    // ⊥ (multiplicative disjunction unit)
    // False,
    /// 0 (additive disjuction unit)
    Zero,
    /// 1
    One,
    ///
    Seq(Seq<I>),
    ///
    Interval(Interval<I>),
    /// a ⊗ b (multiplicative conjunction/times)
    Mul(Box<Repr<I>>, Box<Repr<I>>),
    /// a ⊕ b (additive disjuction/plus)
    Or(Box<Repr<I>>, Box<Repr<I>>),
    /// νa (largest fixed point)
    Inf(Box<Repr<I>>),
    /// µa (smallest fixed point)
    Sup(Box<Repr<I>>),
    /// a ⅋ b (multiplicative disjunction/par)
    Add(Box<Repr<I>>, Box<Repr<I>>),
    /// a & b (additive conjunction/with)
    And(Box<Repr<I>>, Box<Repr<I>>),
    /// TODO
    Cap(Box<Repr<I>>),
}

#[unconst]
impl<I: ~const Integral> Repr<I> {
    pub const fn zero() -> Self {
        Repr::Zero
    }

    pub const fn one() -> Self {
        Repr::One
    }

    pub const fn seq<M>(is: M) -> Self
    where
        M: ~const IntoIterator<Item = I>,
        M::IntoIter: ExactSizeIterator,
    {
        let is = is.into_iter();
        if is.is_empty() {
            Repr::One
        } else {
            Repr::Seq(Seq::new(is))
        }
    }

    pub const fn interval(from: I, to: I) -> Repr<I> {
        Repr::Interval(Interval::new(from, to))
    }

    #[allow(clippy::should_implement_trait)]
    pub const fn mul(self, other: Self) -> Self {
        match (self, other) {
            (Repr::Zero, _) => Repr::Zero,
            (_, Repr::Zero) => Repr::Zero,
            (Repr::One, rhs) => rhs,
            (lhs, Repr::One) => lhs,
            (Repr::Seq(lhs), Repr::Seq(rhs)) => Repr::Seq(lhs.mul(rhs)),
            (Repr::Mul(llhs, lrhs), rhs) => {
                Repr::Mul(llhs, Box::new(Repr::Mul(lrhs, Box::new(rhs))))
            }
            (Repr::Inf(lhs), Repr::Inf(rhs)) if lhs.eq(&rhs) => Repr::Inf(lhs),
            (lhs, rhs) => Repr::Mul(Box::new(lhs), Box::new(rhs)),
        }
    }

    pub const fn or(self, other: Self) -> Self {
        match (self, other) {
            (lhs, rhs) if lhs.eq(&rhs) => lhs,
            (Repr::Zero, rhs) => rhs,
            (lhs, Repr::Zero) => lhs,
            (Repr::Interval(lhs), Repr::Interval(rhs)) => lhs.or(rhs),
            (Repr::Or(llhs, lrhs), rhs) => Repr::Or(llhs, Box::new(Repr::Or(lrhs, Box::new(rhs)))),
            (lhs, rhs) => Repr::Or(Box::new(lhs), Box::new(rhs)),
        }
    }

    pub const fn inf(self) -> Self {
        match self {
            Repr::Inf(repr) => Repr::Inf(repr),
            repr => Repr::Inf(Box::new(repr)),
        }
    }

    pub const fn sup(self) -> Self {
        Repr::Sup(Box::new(self))
    }

    #[allow(clippy::should_implement_trait)]
    pub const fn add(self, other: Self) -> Self {
        match (self, other) {
            (Repr::Add(llhs, lrhs), rhs) => {
                Repr::Add(llhs, Box::new(Repr::Add(lrhs, Box::new(rhs))))
            }
            (lhs, rhs) => Repr::Add(Box::new(lhs), Box::new(rhs)),
        }
    }

    pub const fn and(self, other: Self) -> Self {
        match (self, other) {
            (lhs, rhs) if lhs.eq(&rhs) => lhs,
            (Repr::Interval(lhs), Repr::Interval(rhs)) => lhs.and(rhs),
            (Repr::And(llhs, lrhs), rhs) => {
                Repr::And(llhs, Box::new(Repr::And(lrhs, Box::new(rhs))))
            }
            (lhs, rhs) => Repr::And(Box::new(lhs), Box::new(rhs)),
        }
    }

    pub const fn prod<M: ~const Iterator<Item = Self>>(reprs: M) -> Self {
        reprs.reduce(|acc, e| acc.mul(e)).unwrap()
    }

    pub const fn any<M: ~const Iterator<Item = Self>>(reprs: M) -> Self {
        reprs.reduce(|acc, e| acc.or(e)).unwrap()
    }

    pub const fn sum<M: ~const Iterator<Item = Self>>(reprs: M) -> Self {
        reprs.reduce(|acc, e| acc.add(e)).unwrap()
    }

    pub const fn all<M: ~const Iterator<Item = Self>>(reprs: M) -> Self {
        reprs.reduce(|acc, e| acc.and(e)).unwrap()
    }

    pub const fn rep(self, count: usize) -> Self {
        Self::prod(vec![self; count].into_iter())
    }

    pub const fn rev(self) -> Self {
        match self {
            Repr::Zero => Repr::Zero,
            Repr::Seq(seq) => Repr::Seq(seq.rev()),
            Repr::Interval(i) => Repr::Interval(i),
            Repr::Mul(lhs, rhs) => rhs.rev().mul(lhs.rev()),
            Repr::Or(lhs, rhs) => lhs.rev().or(rhs.rev()),
            Repr::Inf(repr) => repr.rev().inf(),
            Repr::Add(lhs, rhs) => lhs.rev().add(rhs.rev()),
            Repr::And(lhs, rhs) => lhs.rev().and(rhs.rev()),
            _ => unimplemented!(),
        }
    }

    pub const fn dual(self) -> Self {
        match self {
            // Self::Interval(i) => {
            // },
            Repr::Seq(repr) => Repr::Seq(repr),
            Repr::Mul(lhs, rhs) => lhs.dual().add(rhs.dual()),
            Repr::Or(lhs, rhs) => lhs.dual().and(rhs.dual()),
            Repr::Inf(repr) => repr.dual().sup(),
            Repr::Sup(repr) => repr.dual().inf(),
            Repr::Add(lhs, rhs) => lhs.dual().mul(rhs.dual()),
            Repr::And(lhs, rhs) => lhs.dual().or(rhs.dual()),
            _ => unimplemented!(),
        }
    }

    /// 𝜕, derivation, linear endofunctor
    pub const fn der(self, other: Repr<I>) -> Self {
        match self {
            Repr::Zero => Repr::Zero,
            Repr::One => Repr::Zero,
            Repr::Seq(seq) => {
                if Repr::Seq(seq).eq(&other) {
                    Repr::One
                } else {
                    Repr::Zero
                }
            }
            Repr::Mul(lhs, rhs) => lhs
                .clone()
                .der(other.clone())
                .mul(*rhs.clone())
                .or(lhs.mul(rhs.der(other))),
            Repr::Or(lhs, rhs) => lhs.der(other.clone()).or(rhs.der(other)),
            // Repr::And(lhs, rhs) => lhs.der(other.clone()).and(rhs.der(other)),
            _ => unimplemented!(),
        }
    }

    /// ε-production, nullable
    pub const fn is_nullable(&self) -> bool {
        match self {
            Repr::Zero => false,
            Repr::One => true,
            Repr::Seq(_) => false,
            Repr::Interval(_) => false,
            Repr::Mul(lhs, rhs) => lhs.is_nullable() && rhs.is_nullable(),
            Repr::Or(lhs, rhs) => lhs.is_nullable() || rhs.is_nullable(),
            Repr::Inf(_) => true,
            // Repr::And(lhs, rhs) => lhs.is_nullable() || rhs.is_nullable(),
            _ => false,
        }
    }

    pub const fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (Repr::Seq(lhs), Repr::Seq(rhs)) => lhs.eq(rhs),
            (Repr::Interval(lhs), Repr::Interval(rhs)) => lhs.le(rhs),
            (Repr::Or(llhs, lrhs), Repr::Or(rlhs, rrhs)) => {
                llhs.le(rlhs) && lrhs.le(rrhs) || llhs.le(rrhs) && lrhs.le(rlhs)
            }
            // TODO(rinarakaki)
            (lhs, Repr::Or(rlhs, rrhs)) => lhs.le(rlhs) || lhs.le(rrhs),
            (lhs, rhs) => panic!("le not implemented between {:?} {:?}", lhs, rhs),
        }
    }

    pub const fn len(&self) -> usize {
        match self {
            Repr::Seq(seq) => seq.len(),
            Repr::Interval(_) => 1,
            Repr::Mul(lhs, rhs) => lhs.len() + rhs.len(),
            Repr::Or(lhs, rhs) => lhs.len() + rhs.len(),
            _ => unimplemented!(),
        }
    }

    pub const fn lookahead(self, other: Self) -> Self {
        self.clone().and(self.mul(other))
    }

    pub const fn lookbehind(self, other: Self) -> Self {
        self.clone().and(other.mul(self))
    }

    pub const fn cap(self) -> Self {
        Repr::Cap(Box::new(self))
    }
}
