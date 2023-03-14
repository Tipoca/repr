use alloc::boxed::Box;
use core::{
    fmt::Debug,
    iter::Step,
    marker::Destruct,
    panic::{UnwindSafe, RefUnwindSafe}
};

use unconst::unconst;

use crate::interval::Interval;
use crate::seq::Seq;

#[unconst]
#[derive_const(Clone, Debug)]
#[derive(Eq, PartialEq)]
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

#[unconst]
impl<I: ~const Integral> Repr<I> {
    pub const fn zero() -> Self {
        Self::Zero(Default::default())
    }

    pub const fn one(i: I) -> Self {
        Self::One(Seq::one(i))
    }

    
    pub const fn mul(self, other: Self) -> Self {
        match (self, other) {
            (Self::One(lhs), Self::One(rhs)) => Self::One(lhs.mul(rhs)),
            (lhs, rhs) => Self::Or(box lhs, box rhs)
        }
    }
    
    pub const fn or(self, other: Self) -> Self {
        Self::Or(box self, box other)
    }
    
//     pub const fn xor(self, other: Self) -> Self {
//         Self::Xor(box self, box other)
//     }
    
    pub const fn add(self, other: Self) -> Self {
        Self::Add(box self, box other)
    }
    
    pub const fn div(self, other: Self) -> Self {
        Self::Div(box self, box other)
    }
    
    pub const fn exp(self) -> Self {
        Self::Exp(box self)
    }

    pub const fn and(self, other: Self) -> Self {
        Self::And(box self, box other)
    }
    
    pub const fn le(&self, other: &Self) -> bool {
        match self {
            // Self::Or(lhs, rhs) => other == lhs || other == rhs,
            _ => unimplemented!()
        }
    }

    pub const fn dual(self) -> Self {
        match self {
            // Self::Interval(i) => {

            // },
            Self::Mul(lhs, rhs) => Self::Add(box lhs.dual(), box rhs.dual()),
            Self::Or(lhs, rhs) => Self::And(box lhs.dual(), box rhs.dual()),
            Self::Add(lhs, rhs) => Self::Mul(box lhs.dual(), box rhs.dual()),
            Self::And(lhs, rhs) => Self::Or(box lhs.dual(), box rhs.dual()),
            _ => unimplemented!()
        }
    }
    
    pub const fn rev(self) -> Self {
        match self {
            Self::Zero(zero) => Self::Zero(zero),
            Self::One(i) => Self::One(i.rev()),
            Self::Interval(i) => Self::Interval(i),
            Self::Mul(lhs, rhs) => Self::Mul(box rhs.rev(), box lhs.rev()),
            Self::Or(lhs, rhs) => Self::Or(box lhs.rev(), box rhs.rev()),
            // Self::Div(lhs, rhs) => ,
            Self::Exp(repr) => Self::Exp(box repr.rev()),
            // Self::Not => ,
            Self::Add(lhs, rhs) => Self::Add(box lhs.rev(), box rhs.rev()),
            Self::And(lhs, rhs) => Self::And(box lhs.rev(), box rhs.rev()),
            _ => unimplemented!()
        }
    }

    pub const fn prod<M: ~const Iterator<Item = Self>>(reprs: M) -> Self {
        reprs.reduce(|acc, e| Repr::Mul(box acc, box e)).unwrap()
    }

    pub const fn any<M: ~const Iterator<Item = Self>>(reprs: M) -> Self {
        reprs.reduce(|acc, e| Repr::Or(box acc, box e)).unwrap()
    }

    pub const fn sum<M: ~const Iterator<Item = Self>>(reprs: M) -> Self {
        reprs.reduce(|acc, e| Repr::Add(box acc, box e)).unwrap()
    }

    pub const fn all<M: ~const Iterator<Item = Self>>(reprs: M) -> Self {
        reprs.reduce(|acc, e| Repr::And(box acc, box e)).unwrap()
    }

    pub const fn rep(self, count: usize) -> Self {
        Self::prod(vec![self; count].into_iter())
    }

    pub const fn der(self, seq: Seq<I>) -> Self {
        match self {
            Self::One(seq) => unimplemented!(),
            Self::Mul(lhs, rhs)
                => Self::Or(box Self::Mul(box lhs.der(seq), rhs),
                            box Self::Mul(lhs, box rhs.der(seq))),
            Self::Or(lhs, rhs) => Self::Or(box lhs.der(seq), box rhs.der(seq)),
            Self::And(lhs, rhs) => Self::And(box lhs.der(seq),
                                             box rhs.der(seq)),
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
            Self::One(seq) => seq == &Seq::empty(),
            // Self::Mul(lhs, rhs) => lhs.nullable() && rhs.nullable(),
            Self::Or(lhs, rhs) => lhs.nullable() || rhs.nullable(),
            Self::And(lhs, rhs) => lhs.nullable() || rhs.nullable(),
            Self::Exp(_) => true,
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
            Repr::One(_) => true,
            Repr::Or(lhs, rhs)
                => lhs.is_alternation_literal() && rhs.is_alternation_literal(),
            _ => false
        }
    }
}

/// - `Copy` + `Clone`: possibility of `!` exponentiation
/// - `PartialEq` + `Eq`: decidability
#[unconst]
#[const_trait]
pub trait Integral: Copy + ~const Clone
                    + ~const PartialEq + Eq
                    + ~const PartialOrd + ~const Ord
                    + Step
                    + ~const Destruct
                    + Debug
                    + Sync + Send + RefUnwindSafe + UnwindSafe
{
    const MIN: Self;
    const MAX: Self;
    fn succ(self) -> Self;
    fn pred(self) -> Self;
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
#[derive_const(Default)]
#[derive(Clone, Debug, Eq, PartialEq)]
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
