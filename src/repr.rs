use alloc::{boxed::Box, vec};
use core::fmt::Debug;

use unconst::unconst;

use crate::interval::Interval;
use crate::seq::Seq;
use crate::traits::Integral;

#[unconst]
#[derive_const(Clone, PartialEq)]
#[derive(Debug, Eq)]
pub enum Repr<I: ~const Integral> {
    True(Box<dyn Fn(Seq<I>) -> bool>),
    Zero(Zero),
    One(Seq<I>),
    Interval(Interval<I>),
    /// a ⊗ b (multiplicative conjunction/times)
    Mul(Box<Repr<I>>, Box<Repr<I>>),
    /// a ⊕ b (additive disjuction/plus)
    Or(Box<Repr<I>>, Box<Repr<I>>),
    /// a ⊸ b (linear implication)
    Div(Box<Repr<I>>, Box<Repr<I>>),
    /// νa (largest fixed point)
    Inf(Box<Repr<I>>),
    /// µa (smallest fixed point)
    Sup(Box<Repr<I>>),
    Not(Box<Repr<I>>),
    /// a ⅋ b (multiplicative disjunction/par)
    Add(Box<Repr<I>>, Box<Repr<I>>),
    /// a & b (additive conjunction/with)
    And(Box<Repr<I>>, Box<Repr<I>>),
    // Map(Box<Repr<I>>, Fn(Box<Repr<I>>), Fn(Box<Repr<I>>))
}

use Repr::{One, Mul, Or, Div, Inf, Sup, Add, And};

#[unconst]
impl<I: ~const Integral> Repr<I> {
    pub const fn zero() -> Self {
        Self::Zero(Default::default())
    }

    pub const fn one(i: I) -> Self {
        One(Seq::one(i))
    }

    pub const fn seq<M: ~const IntoIterator<Item = I>>(is: M) -> Self {
        One(Seq::new(is))
    }

    pub const fn mul(self, other: Self) -> Self {
        match (self, other) {
            (One(lhs), One(rhs)) => One(lhs.mul(rhs)),
            (Mul(llhs, lrhs), rhs) => Mul(llhs, Box::new(Mul(lrhs, Box::new(rhs)))),
            (lhs, rhs) => Mul(Box::new(lhs), Box::new(rhs))
        }
    }
    
    pub const fn or(self, other: Self) -> Self {
        match (self, other) {
            (lhs, rhs) if lhs == rhs => lhs,
            (Or(llhs, lrhs), rhs) => Or(llhs, Box::new(Or(lrhs, Box::new(rhs)))),
            (lhs, rhs) => Or(Box::new(lhs), Box::new(rhs))
        }
    }
    
//     pub const fn xor(self, other: Self) -> Self {
//         Xor(Box::new(self), Box::new(other))
//     }
    
    pub const fn add(self, other: Self) -> Self {
        Add(Box::new(self), Box::new(other))
    }
    
    pub const fn div(self, other: Self) -> Self {
        Div(Box::new(self), Box::new(other))
    }

    pub const fn inf(self) -> Self {
        Inf(Box::new(self))
    }
    
    pub const fn sup(self) -> Self {
        Sup(Box::new(self))
    }

    pub const fn and(self, other: Self) -> Self {
        And(Box::new(self), Box::new(other))
    }
    
    pub const fn le(&self, other: &Self) -> bool {
        match other {
            Or(lhs, rhs) => self.le(lhs) || self.le(rhs),
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
    
    pub const fn rev(self) -> Self {
        match self {
            Self::Zero(zero) => Self::Zero(zero),
            One(seq) => One(seq.rev()),
            Self::Interval(i) => Self::Interval(i),
            Mul(lhs, rhs) => rhs.rev().mul(lhs.rev()),
            Or(lhs, rhs) => lhs.rev().or(rhs.rev()),
            // Div(lhs, rhs) => ,
            Inf(repr) => repr.rev().inf(),
            // Not => ,
            Add(lhs, rhs) => lhs.rev().add(rhs.rev()),
            And(lhs, rhs) => lhs.rev().and(rhs.rev()),
            _ => unimplemented!()
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

    /*
    TODO(anarkk) Make this inductive.

    For example, while `a*` will report `true`, `()+` will not, even though `()` matches the empty string and one or more occurrences of something that matches the empty string will always match the empty string. In order to get the inductive definition, see the corresponding method on
    [`Hir`](struct.Hir.html).
    */
    /// ε-production, nullable
    pub const fn null(&self) -> bool {
        match self {
            Self::Zero(_) => true,
            One(seq) => seq == &Seq::empty(),
            Mul(lhs, rhs) => lhs.null() && rhs.null(),
            Or(lhs, rhs) => lhs.null() || rhs.null(),
            Inf(_) => true,
            // And(lhs, rhs) => lhs.null() || rhs.null(),
            _ => false
        }
    }

    pub const fn len(&self) -> usize {
        match self {
            One(_) => 1,
            Mul(lhs, rhs) => lhs.len() + rhs.len(),
            Or(lhs, rhs) => lhs.len() + rhs.len(),
            _ => unimplemented!()
        }
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

    pub const fn is_anchored_start(&self) -> bool {
        match self {
            Self::Zero(Zero::StartText) => true,
            _ => false
        }
    }

    pub const fn is_anchored_end(&self) -> bool {
        match self {
            Self::Zero(Zero::EndText) => true,
            _ => false
        }
    }

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
