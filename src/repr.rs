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
    True,
    Zero(Zero),
    One(Seq<I>),
    Interval(Interval<I>),
    /// a ⊗ b (multiplicative conjunction/times)
    Mul(Box<Repr<I>>, Box<Repr<I>>),
    /// a ⊕ b (additive disjuction/plus)
    Or(Box<Repr<I>>, Box<Repr<I>>),
    // Xor(Box<Repr<I>>, Box<Repr<I>>),
    // Sub(Box<Repr<I>>, Box<Repr<<I>>),
    /// a ⊸ b (linear implication)
    Div(Box<Repr<I>>, Box<Repr<I>>),
    Exp(Box<Repr<I>>),
    Not(Box<Repr<I>>),
    /// a ⅋ b (multiplicative disjunction/par)
    Add(Box<Repr<I>>, Box<Repr<I>>),
    /// a & b (additive conjunction/with)
    And(Box<Repr<I>>, Box<Repr<I>>),
    // Map(Box<Repr<I>>, Fn(Box<Repr<I>>), Fn(Box<Repr<I>>))
}

use Repr::{One, Mul, Or, Div, Exp, Add, And};

#[unconst]
impl<I: ~const Integral> Repr<I> {
    pub const fn zero() -> Self {
        Self::Zero(Default::default())
    }

    pub const fn one(i: I) -> Self {
        One(Seq::one(i))
    }

    
    pub const fn mul(self, other: Self) -> Self {
        match (self, other) {
            (One(lhs), One(rhs)) => One(lhs.mul(rhs)),
            (lhs, rhs) => Or(box lhs, box rhs)
        }
    }
    
    pub const fn or(self, other: Self) -> Self {
        Or(box self, box other)
    }
    
//     pub const fn xor(self, other: Self) -> Self {
//         Xor(box self, box other)
//     }
    
    pub const fn add(self, other: Self) -> Self {
        Add(box self, box other)
    }
    
    pub const fn div(self, other: Self) -> Self {
        Div(box self, box other)
    }
    
    pub const fn exp(self) -> Self {
        Exp(box self)
    }

    pub const fn and(self, other: Self) -> Self {
        And(box self, box other)
    }
    
    pub const fn le(&self, _other: &Self) -> bool {
        match self {
            // Or(lhs, rhs) => other == lhs || other == rhs,
            _ => unimplemented!()
        }
    }

    pub const fn dual(self) -> Self {
        match self {
            // Self::Interval(i) => {
            // },
            Mul(lhs, rhs) => Add(box lhs.dual(), box rhs.dual()),
            Or(lhs, rhs) => And(box lhs.dual(), box rhs.dual()),
            Add(lhs, rhs) => Mul(box lhs.dual(), box rhs.dual()),
            And(lhs, rhs) => Or(box lhs.dual(), box rhs.dual()),
            _ => unimplemented!()
        }
    }
    
    pub const fn rev(self) -> Self {
        match self {
            Self::Zero(zero) => Self::Zero(zero),
            One(i) => One(i.rev()),
            Self::Interval(i) => Self::Interval(i),
            Mul(lhs, rhs) => Mul(box rhs.rev(), box lhs.rev()),
            Or(lhs, rhs) => Or(box lhs.rev(), box rhs.rev()),
            // Div(lhs, rhs) => ,
            Exp(repr) => Exp(box repr.rev()),
            // Not => ,
            Add(lhs, rhs) => Add(box lhs.rev(), box rhs.rev()),
            And(lhs, rhs) => And(box lhs.rev(), box rhs.rev()),
            _ => unimplemented!()
        }
    }

    pub const fn prod<M: ~const Iterator<Item = Self>>(reprs: M) -> Self {
        reprs.reduce(|acc, e| Mul(box acc, box e)).unwrap()
    }

    pub const fn any<M: ~const Iterator<Item = Self>>(reprs: M) -> Self {
        reprs.reduce(|acc, e| Or(box acc, box e)).unwrap()
    }

    pub const fn sum<M: ~const Iterator<Item = Self>>(reprs: M) -> Self {
        reprs.reduce(|acc, e| Add(box acc, box e)).unwrap()
    }

    pub const fn all<M: ~const Iterator<Item = Self>>(reprs: M) -> Self {
        reprs.reduce(|acc, e| And(box acc, box e)).unwrap()
    }

    pub const fn rep(self, count: usize) -> Self {
        Self::prod(vec![self; count].into_iter())
    }

    pub const fn der(self, seq: Seq<I>) -> Self {
        match self {
            One(_seq) => unimplemented!(),
            Mul(lhs, rhs)
                => Or(box Mul(box lhs.clone().der(seq.clone()), rhs.clone()),
                      box Mul(lhs, box rhs.der(seq))),
            Or(lhs, rhs) => Or(box lhs.der(seq.clone()), box rhs.der(seq)),
            And(lhs, rhs) => And(box lhs.der(seq.clone()), box rhs.der(seq)),
            _ => unimplemented!()
        }
    }

    /// Returns true if and only if this can match the empty string.
    ///
    /// Note that this is not defined inductively. For example, while `a*`
    /// will report `true`, `()+` will not, even though `()` matches the empty
    /// string and one or more occurrences of something that matches the empty
    /// string will always match the empty string. In order to get the
    /// inductive definition, see the corresponding method on
    /// [`Hir`](struct.Hir.html).
    pub const fn nullable(&self) -> bool {
        match self {
            Self::Zero(_) => true,
            One(seq) => seq == &Seq::empty(),
            // Mul(lhs, rhs) => lhs.nullable() && rhs.nullable(),
            Or(lhs, rhs) => lhs.nullable() || rhs.nullable(),
            And(lhs, rhs) => lhs.nullable() || rhs.nullable(),
            Exp(_) => true,
            _ => false
        }
    }
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
