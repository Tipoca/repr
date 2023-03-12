use alloc::{
    boxed::Box,
//     vec::Vec
};
use core::{
    fmt::Debug,
    // iter::IntoIterator,
    marker::Destruct,
};

use unconst::unconst;

use crate::interval::Interval;

#[unconst]
// TODO(rnarkk) Interval (class) as `or` for char, &str as `and` for char?
#[derive_const(Clone, Debug)]
#[derive(Eq, PartialEq)]
pub enum Repr<I: ~const Integral> {
    Zero(Zero),
    One(I),  // TODO(rnarkk)  Interval(I, I)
    Interval(Interval<I>),  // TODO(rnarkk)
    /// a & b (additive conjunction/with)
    Mul(Box<Repr<I>>, Box<Repr<I>>),
    /// a ⊕ b (additive disjuction/plus)
    Or(Box<Repr<I>>, Box<Repr<I>>),
    // Xor(Box<Repr<I>>, Box<Repr<I>>),
    // Sub(Box<Repr<I>>, Interval<I>),  // TODO(rnarkk)
    /// a ⊸ b (linear implication)
    Div(Box<Repr<I>>, Box<Repr<I>>),
    Exp(Box<Repr<I>>, Range),
    Not(Box<Repr<I>>),
    Rev(Box<Repr<I>>),
    /// a ⅋ b (multiplicative disjunction/par)
    Add(Box<Repr<I>>, Box<Repr<I>>),
    /// a ⊗ b (multiplicative conjunction/times)
    And(Box<Repr<I>>, Box<Repr<I>>),
    // Map(Box<Repr<I>>, Fn(Box<Repr<I>>), Fn(Box<Repr<I>>))
}

#[unconst]
impl<I: ~const Integral> Repr<I> {
    // pub const fn new<const N: usize>(seqs: [Interval<I>; N]) -> Self {

    // }

    pub const fn empty() -> Self {
        Self::Zero(Default::default())
    }

    pub const fn one(i: I) -> Self {
        Self::One(i)
    }
    
    pub const fn not(self) -> Self {
        Self::Not(box self)
    }
    
    pub const fn and(self, other: Self) -> Self {
        Self::And(box self, box other)
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
    
    pub const fn exp(self, range: Range) -> Self {
        Self::Exp(box self, range)
    }
    
    pub const fn rev(self) -> Self {
        Self::Rev(box self)
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
}

#[unconst]
impl Repr<char> {
    /// `.` expression that matches any character except for `\n`. To build an
    /// expression that matches any character, including `\n`, use the `any`
    /// method.
    pub const fn dot() -> Self {
        Self::Or(box Self::Interval(Interval('\0', '\x09')),
                 box Self::Interval(Interval('\x0B', '\u{10FFFF}')))
    }

    // /// `(?s).` expression that matches any character, including `\n`. To build an
    // /// expression that matches any character except for `\n`, then use the
    // /// `dot` method.
    // pub const fn any() -> Self {
    //     Self::Interval(Interval('\0', '\u{10FFFF}'))
    // }
}

/*
#[unconst]
pub struct Iter<'a, I: ~const Integral> {
    repr: &'a Repr<I>,
    left: 
}
*/

// impl<I: ~const Integral> const IntoIterator for Repr<I> {
//     type Item = I;
//     type IntoIter: IntoIter<'a, I>;

//     fn into_iter(self) -> Self::IntoIter {
//         let mut iter = Vec::new();
//         match self {
//             _ => unimplemented!()
//         }
//     }
// }

/// - `Copy` + `Clone`: possibility of `!` exponentiation
/// - `PartialEq` + `Eq`: decidability
#[unconst]
#[const_trait]
pub trait Integral: Copy + ~const Clone
                    + ~const PartialEq + Eq
                    + ~const PartialOrd + ~const Ord
                    + ~const Destruct
                    + Debug
{
    // type S: ~const IntoIterator<Item = Self>;
    const MIN: Self;
    const MAX: Self;
    fn succ(self) -> Self;
    fn pred(self) -> Self;
    // // (rnarkk) use this in crate::literal
    // fn as_bytes(self, reverse: bool) -> &'static [u8];
}

#[unconst]
impl const Integral for char {
    // type S = Str<'a>;
    const MIN: Self = '\x00';
    const MAX: Self = '\u{10FFFF}';
    fn succ(self) -> Self {
        match self {
            '\u{D7FF}' => '\u{E000}',
            c => char::from_u32((c as u32).checked_add(1).unwrap()).unwrap(),
        }
    }
    fn pred(self) -> Self {
        match self {
            '\u{E000}' => '\u{D7FF}',
            c => char::from_u32((c as u32).checked_sub(1).unwrap()).unwrap(),
        }
    }
    // fn as_bytes(self, reverse: bool) -> &'static [u8] {
    //     let mut buf = [0u8; 4];
    //     let len = self.encode_utf8(&mut buf).len();
    //     let buf = &mut buf[..len];
    //     if reverse {
    //         buf.reverse();
    //     }
    //     buf
    // }
}

pub struct Str<'a>(&'a str);

impl<'a> IntoIterator for Str<'a> {
    type Item = char;
    type IntoIter = core::str::Chars<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.chars()
    }
}

// 24bit
#[unconst]
#[derive_const(Clone, PartialEq, PartialOrd, Ord)]
#[derive(Copy, Debug, Eq)]
pub enum Range {
    Empty,
    From(usize),
    To(usize),
    // TODO(rnarkk) validate 0 <= 1
    // TODO(rnarkk) if this is (0, 0), need to ignore it or treat as zero sized match any way? For now, ignore if (0, 0)
    Full(usize, usize),
}

#[unconst]
impl Range {
    /// Returns true if and only if this repetition operator makes it possible
    /// to match the empty string.
    ///
    /// Note that this is not defined inductively. For example, while `a*`
    /// will report `true`, `()+` will not, even though `()` matches the empty
    /// string and one or more occurrences of something that matches the empty
    /// string will always match the empty string. In order to get the
    /// inductive definition, see the corresponding method on
    /// [`Hir`](struct.Hir.html).
    pub const fn is_match_empty(&self) -> bool {
        match self {
            Range::Empty => true,
            Range::To(_) => true,
            Range::From(n) => n == &0,
            Range::Full(n, _) => n == &0,
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
