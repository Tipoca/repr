use alloc::{boxed::Box, vec};

use unconst::unconst;

use crate::interval::Interval;
use crate::seq::Seq;
use crate::traits::Integral;

#[unconst]
pub enum Repr<I: ~const Integral> {
    True(Box<dyn Fn(Seq<I>) -> bool>),
    /// 0 (additive disjuction unit)
    Zero,
    /// 
    One(Seq<I>),
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
    // Map(Box<Repr<I>>, Fn(Box<Repr<I>>), Fn(Box<Repr<I>>))
}

use Repr::{True, Zero, One, Mul, Or, Inf, Sup, Add, And};

#[unconst]
impl<I: ~const Integral> Repr<I> {
    pub const fn zero() -> Self {
        Zero
    }

    pub const fn one(i: I) -> Self {
        One(Seq::one(i))
    }

    pub const fn seq<M: ~const IntoIterator<Item = I>>(is: M) -> Self {
        One(Seq::new(is))
    }
    
    pub const fn interval(from: I, to: I) -> Repr<I> {
        Repr::Interval(Interval::new(from, to))
    }

    pub const fn mul(self, other: Self) -> Self {
        match (self, other) {
            (Zero, _) => Zero,
            (_, Zero) => Zero,
            (One(lhs), One(rhs)) => One(lhs.mul(rhs)),
            (Mul(llhs, lrhs), rhs) => Mul(llhs, Box::new(Mul(lrhs, Box::new(rhs)))),
            (Inf(lhs), Inf(rhs)) if lhs.eq(rhs) => Inf(lhs),
            (lhs, rhs) => Mul(Box::new(lhs), Box::new(rhs))
        }
    }
    
    pub const fn or(self, other: Self) -> Self {
        match (self, other) {
            (lhs, rhs) if lhs.eq(rhs) => lhs,
            (Zero, rhs) => rhs,
            (lhs, Zero) => lhs,
            (Self::Interval(lhs), Self::Interval(rhs)) => lhs.or(rhs),
            (Or(llhs, lrhs), rhs) => Or(llhs, Box::new(Or(lrhs, Box::new(rhs)))),
            (lhs, rhs) => Or(Box::new(lhs), Box::new(rhs))
        }
    }
    
    pub const fn inf(self) -> Self {
        Inf(Box::new(self))
    }
    
    pub const fn sup(self) -> Self {
        Sup(Box::new(self))
    }

    pub const fn add(self, other: Self) -> Self {
        match (self, other) {
            (Add(llhs, lrhs), rhs) => Add(llhs, Box::new(Add(lrhs, Box::new(rhs)))),
            (lhs, rhs) => Add(Box::new(lhs), Box::new(rhs))
        }
    }

    pub const fn and(self, other: Self) -> Self {
        match (self, other) {
            (lhs, rhs) if lhs.eq(rhs) => lhs,
            (Self::Interval(lhs), Self::Interval(rhs)) => lhs.and(rhs),
            (lhs, rhs) => And(Box::new(lhs), Box::new(rhs))
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
            Zero => Zero,
            One(seq) => One(seq.rev()),
            Self::Interval(i) => Self::Interval(i),
            Mul(lhs, rhs) => rhs.rev().mul(lhs.rev()),
            Or(lhs, rhs) => lhs.rev().or(rhs.rev()),
            Inf(repr) => repr.rev().inf(),
            Add(lhs, rhs) => lhs.rev().add(rhs.rev()),
            And(lhs, rhs) => lhs.rev().and(rhs.rev()),
            _ => unimplemented!()
        }
    }
    
    pub const fn dual(self) -> Self {
        match self {
            // Self::Interval(i) => {
            // },
            One(repr) => One(repr),
            Mul(lhs, rhs) => lhs.dual().add(rhs.dual()),
            Or(lhs, rhs) => lhs.dual().and(rhs.dual()),
            Inf(repr) => repr.dual().sup(),
            Sup(repr) => repr.dual().inf(),
            Add(lhs, rhs) => lhs.dual().mul(rhs.dual()),
            And(lhs, rhs) => lhs.dual().or(rhs.dual()),
            _ => unimplemented!()
        }
    }

    /// 𝜕, derivation, linear endofunctor
    pub const fn der(self, seq: Seq<I>) -> Self {
        match self {
            One(_seq) => unimplemented!(),
            Mul(lhs, rhs) => lhs.clone().der(seq.clone()).mul(*rhs.clone())
                                .or(lhs.mul(rhs.der(seq))),
            Or(lhs, rhs) => lhs.der(seq.clone()).or(rhs.der(seq)),
            And(lhs, rhs) => lhs.der(seq.clone()).and(rhs.der(seq)),
            _ => unimplemented!()
        }
    }
 
    /// ε-production, nullable
    pub const fn null(&self) -> bool {
        match self {
            Zero => false,
            One(seq) => seq.null(),
            Self::Interval(_) => false,
            Mul(lhs, rhs) => lhs.null() && rhs.null(),
            Or(lhs, rhs) => lhs.null() || rhs.null(),
            Inf(_) => true,
            // And(lhs, rhs) => lhs.null() || rhs.null(),
            _ => false
        }
    }
    
    pub const fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (True(_), True(_)) => panic!("True variant is uncomparable"),
            (Zero, Zero) => true,
            (One(lhs), One(rhs)) => lhs.eq(rhs),
            (Self::Interval(lhs), Self::Interval(rhs)) => lhs.eq(rhs),
            (Mul(llhs, lrhs), Mul(rlhs, rrhs)) => llhs.eq(rlhs) && lrhs.eq(rrhs),
            (Or(llhs, lrhs), Or(rlhs, rrhs))
                => llhs.eq(rlhs) && lrhs.eq(rrhs)
                || llhs.eq(rrhs) && lrhs.eq(rlhs),
            (Inf(lhs), Inf(rhs)) => lhs.eq(rhs),
            (Add(llhs, lrhs), Add(rlhs, rrhs)) => llhs.eq(rlhs) && lrhs.eq(rrhs),
            (And(llhs, lrhs), And(rlhs, rrhs))
                => llhs.eq(rlhs) && lrhs.eq(rrhs)
                || llhs.eq(rrhs) && lrhs.eq(rlhs),
            // TODO(rnarkk)
            _ => false
        }
    }
    
    pub const fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (One(lhs), One(rhs)) => lhs.eq(rhs),
            (Self::Interval(lhs), Self::Interval(rhs)) => lhs.le(rhs),
            (Or(llhs, lrhs), Or(rlhs, rrhs))
                => llhs.le(rlhs) && lrhs.le(rrhs)
                || llhs.le(rrhs) && lrhs.le(rlhs),
            // TODO(rnarkk)
            (lhs, Or(rlhs, rrhs)) => lhs.le(rlhs) || lhs.le(rrhs),
            (lhs, rhs) => panic!("le not implemented between {:?} {:?}", lhs, rhs)
        }
    }

    pub const fn len(&self) -> usize {
        match self {
            One(seq) => seq.len(),
            Self::Interval(_) => 1,
            Mul(lhs, rhs) => lhs.len() + rhs.len(),
            Or(lhs, rhs) => lhs.len() + rhs.len(),
            _ => unimplemented!()
        }
    }

    pub const fn lookahead(self, other: Self) -> Self {
        self.clone().and(self.mul(other))
    }

    pub const fn lookbehind(self, other: Self) -> Self {
        self.clone().and(other.mul(self))
    }

    // #[inline(always)]
    // pub const fn map<F: FnMut(Self) -> Self>(self, mut f: F) -> Self {
    //     match self {
    //         // One(seq) => One(f(seq)),
    //         Mul(lhs, rhs) => f(*lhs).mul(f(*rhs)),
    //         Or(lhs, rhs) => f(*lhs).or(f(*rhs)),
    //         _ => unimplemented!()
    //     }
    // }
}

#[unconst]
impl<I: ~const Integral> Repr<I> {
    pub const fn is_always_utf8(&self) -> bool {
        unimplemented!()
    }

    pub const fn is_all_assertions(&self) -> bool {
        unimplemented!()
    }

    // pub const fn is_anchored_start(&self) -> bool {
    //     match self {
    //         Self::Zero(Zero::StartText) => true,
    //         _ => false
    //     }
    // }

    // pub const fn is_anchored_end(&self) -> bool {
    //     match self {
    //         Self::Zero(Zero::EndText) => true,
    //         _ => false
    //     }
    // }

    pub const fn is_line_anchored_start(&self) -> bool {
        unimplemented!()
    }

    pub const fn is_line_anchored_end(&self) -> bool {
        unimplemented!()
    }

    pub const fn is_any_anchored_start(&self) -> bool {
        unimplemented!()
    }

    pub const fn is_any_anchored_end(&self) -> bool {
        unimplemented!()
    }

    pub const fn is_literal(&self) -> bool {
        unimplemented!()
    }

    pub const fn is_alternation_literal(&self) -> bool {
        match self {
            One(_) => true,
            Or(lhs, rhs)
                => lhs.is_alternation_literal() && rhs.is_alternation_literal(),
            _ => false
        }
    }
}

/*
/// An anchor assertion. An anchor assertion match always has zero length.
/// The high-level intermediate representation for an anchor assertion.
///
/// A matching anchor assertion is always zero-length.
/// 
/// A word boundary assertion, which may or may not be Unicode aware. A
/// word boundary assertion match always has zero length.
/// The high-level intermediate representation for a word-boundary assertion.
///
/// A matching word boundary assertion is always zero-length.
#[unconst]
#[derive_const(Clone, Default, PartialEq)]
#[derive(Copy, Debug, Eq)]
pub enum Zero {
    #[default]
    Any,
    /// `^`,  `(?m:^)`
    /// Match the beginning of a line or the beginning of text. Specifically,
    /// this matches at the starting position of the input, or at the position
    /// immediately following a `\n` character.
    StartLine,
    /// `$`, `(?m:$)`
    /// Match the end of a line or the end of text. Specifically,
    /// this matches at the end position of the input, or at the position
    /// immediately preceding a `\n` character.
    EndLine,
    /// `\A`
    /// Match the beginning of text. Specifically, this matches at the starting
    /// position of the input.
    StartText,
    /// `\z`
    /// Match the end of text. Specifically, this matches at the ending
    /// position of the input.
    EndText,
    /// `\b`, `(?-u:\b)`
    /// Match a Unicode-aware word boundary. That is, this matches a position
    /// where the left adjacent character and right adjacent character
    /// correspond to a word and non-word or a non-word and word character.
    WordBoundary,
    /// `\B`, `(?-u:\B)`
    /// Match a Unicode-aware negation of a word boundary.
    NotWordBoundary,
    /// Match an ASCII-only word boundary. That is, this matches a position
    /// where the left adjacent character and right adjacent character
    /// correspond to a word and non-word or a non-word and word character.
    WordBoundaryAscii,
    /// Match an ASCII-only negation of a word boundary.
    NotWordBoundaryAscii,
}
*/
