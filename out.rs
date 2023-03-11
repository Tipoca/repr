#![feature(prelude_import)]
/*!
# Usage

```text
use repr::{Repr, consts::DIGIT};
let re = DIGIT * 4 & '-' & DIGIT * 2 & '-' & DIGIT * 2;
assert!(re.is_match("2014-01-01"));
```

```text
let re = (D * 4)["year"] & '-' & (D * 2)["month"] & '-' & (D * 2)["day"];
let before = "2012-03-14, 2013-01-01 and 2014-07-05";
let after = re.replace_all(before, "$m/$d/$y");
assert_eq!(after, "03/14/2012, 01/01/2013 and 07/05/2014");
```

```text
use repr::{Repr, consts::DIGIT};
let wh = WORD | '-';
let re = (wh | '.') * 1.. & '@' & (wh * 1.. & '.') * 1.. & wh * 2..4;
```
*/
#![feature(pattern)]
#![feature(once_cell)]
#![feature(const_trait_impl)]
#![feature(box_syntax)]
#![feature(try_trait_v2)]
#![feature(derive_const)]
#![feature(const_try)]
#![feature(const_for)]
#![feature(const_box)]
#![feature(const_cmp)]
#![feature(const_discriminant)]
#![feature(const_clone)]
#![feature(const_reverse)]
#![feature(const_slice_index)]
#![feature(const_mut_refs)]
#![feature(const_option)]
#![feature(const_refs_to_cell)]
#![feature(const_heap)]
#![feature(const_convert)]
#![feature(core_intrinsics)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
extern crate alloc;
mod conversions {
    use core::ops::{self, RangeBounds, Bound};
    use unconst::unconst;
    use crate::repr::{Repr, Seq, Range, Integral};
    impl<I: Integral> From<I> for Repr<I> {
        fn from(value: I) -> Repr<I> {
            Self::One(value)
        }
    }
    impl<I: Integral> From<ops::Range<I>> for Seq<I> {
        fn from(range: ops::Range<I>) -> Self {
            Seq(range.start, range.end)
        }
    }
    impl<I: Integral> From<ops::Range<I>> for Repr<I> {
        fn from(range: ops::Range<I>) -> Self {
            Repr::Seq(range.into())
        }
    }
    impl<R: RangeBounds<usize>> From<R> for Range {
        fn from(range: R) -> Self {
            use Bound::*;
            match (range.start_bound(), range.end_bound()) {
                (Unbounded, Unbounded) => Range::Empty,
                (Unbounded, Excluded(end)) => Range::To(end.clone()),
                (Included(start), Unbounded) => Range::From(start.clone()),
                (Included(start), Excluded(end)) => {
                    Range::Full(start.clone(), end.clone())
                }
                _ => {
                    ::core::panicking::panic_fmt(
                        format_args!("Try m..n instead of m..=n."),
                    )
                }
            }
        }
    }
}
mod interval {
    //! TODO(rnarkk) Refactor and relocate them in crate::repr
    //! <https://en.wikipedia.org/wiki/Interval_arithmetic>
    //! <https://en.wikipedia.org/wiki/Boundary_(topology)>
    //! <https://en.wikipedia.org/wiki/Partition_of_a_set>
    //! <https://en.wikipedia.org/wiki/Sequence>
}
mod operators {
    use core::ops::{BitOr, BitAnd, BitXor, Range, Mul, RangeBounds};
    use unconst::unconst;
    use crate::repr::{Repr, Seq, Integral};
    impl<I: Integral> BitAnd<Self> for Repr<I> {
        type Output = Self;
        fn bitand(self, rhs: Self) -> Self {
            self.and(rhs)
        }
    }
    impl<I: Integral> BitAnd<I> for Repr<I> {
        type Output = Self;
        fn bitand(self, rhs: I) -> Self {
            self.and(Repr::One(rhs))
        }
    }
    impl<I: Integral> BitAnd<Range<I>> for Repr<I> {
        type Output = Self;
        fn bitand(self, rhs: Range<I>) -> Self::Output {
            self.and(Repr::Seq(rhs.into()))
        }
    }
    impl<I: Integral> BitOr<Self> for Repr<I> {
        type Output = Self;
        fn bitor(self, rhs: Self) -> Self {
            self.or(rhs)
        }
    }
    impl<I: Integral> BitOr<I> for Repr<I> {
        type Output = Self;
        fn bitor(self, rhs: I) -> Self {
            self.or(Self::One(rhs))
        }
    }
    impl<I: Integral> BitOr<Range<I>> for Repr<I> {
        type Output = Self;
        fn bitor(self, rhs: Range<I>) -> Self {
            self.or(Repr::Seq(rhs.into()))
        }
    }
    impl<I: Integral> BitAnd<Self> for Seq<I> {
        type Output = Option<Self>;
        fn bitand(self, rhs: Self) -> Self::Output {
            self.and(rhs)
        }
    }
    impl<I: Integral> BitOr<Self> for Seq<I> {
        type Output = Option<Self>;
        fn bitor(self, rhs: Self) -> Self::Output {
            self.or(rhs)
        }
    }
    impl<I: Integral> BitXor<Self> for Seq<I> {
        type Output = (Option<Self>, Option<Self>);
        fn bitxor(self, rhs: Self) -> Self::Output {
            self.xor(rhs)
        }
    }
}
mod wrappers {
    pub const fn empty() {}
}
pub mod char {
    use crate::repr::Repr;
    pub trait CharExt: Into<Repr<char>> {
        fn and(self, rhs: Self) -> Repr<char> {
            self.into() & rhs.into()
        }
        fn or(self, rhs: Self) -> Repr<char> {
            self.into() | rhs.into()
        }
    }
    impl CharExt for char {}
    pub const fn escape(c: char) -> char {
        match c {
            'b' => '\u{0008}',
            'f' => '\u{000C}',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => '\u{000B}',
            '0' => '\0',
            _ => ::core::panicking::panic("explicit panic"),
        }
    }
}
pub mod consts {
    use unconst::unconst;
    use crate::repr::Repr;
    /// `\0`
    pub const NUL: std::sync::LazyLock<Repr<char>> = std::sync::LazyLock::new(|| Repr::from(
        '\0',
    ));
    /// `\t`
    pub const HT: std::sync::LazyLock<Repr<char>> = std::sync::LazyLock::new(|| Repr::from(
        '\t',
    ));
    /// `\n`
    pub const LF: std::sync::LazyLock<Repr<char>> = std::sync::LazyLock::new(|| Repr::from(
        '\n',
    ));
    /// `\v`
    pub const VT: std::sync::LazyLock<Repr<char>> = std::sync::LazyLock::new(|| Repr::from(
        '\u{000B}',
    ));
    /// `\r`
    pub const CR: std::sync::LazyLock<Repr<char>> = std::sync::LazyLock::new(|| Repr::from(
        '\r',
    ));
    /// ` `
    pub const SP: std::sync::LazyLock<Repr<char>> = std::sync::LazyLock::new(|| Repr::from(
        ' ',
    ));
    pub const DIGIT: std::sync::LazyLock<Repr<char>> = std::sync::LazyLock::new(|| Repr::from(
        '0'..'9',
    ));
    pub const WORD: std::sync::LazyLock<Repr<char>> = std::sync::LazyLock::new(|| {
        Repr::from('A'..'Z') | ('a'..'z')
    });
}
pub mod macros {
    //! Macro definitions.
}
pub mod repr {
    use alloc::{boxed::Box, vec::Vec};
    use core::{
        cmp::{max, min},
        fmt::Debug, marker::Destruct,
    };
    use unconst::unconst;
    pub enum Repr<I: Integral> {
        Zero(Zero),
        One(I),
        Seq(Seq<I>),
        Not(Box<Repr<I>>),
        Or(Box<Repr<I>>, Box<Repr<I>>),
        And(Box<Repr<I>>, Box<Repr<I>>),
        Xor(Box<Repr<I>>, Box<Repr<I>>),
        Add(Box<Repr<I>>, Box<Repr<I>>),
        Sub(Box<Repr<I>>, Seq<I>),
        Exp(Box<Repr<I>>, Range),
    }
    #[automatically_derived]
    impl<I: Integral> ::core::marker::StructuralEq for Repr<I> {}
    #[automatically_derived]
    impl<I: ::core::cmp::Eq + Integral> ::core::cmp::Eq for Repr<I> {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<Zero>;
            let _: ::core::cmp::AssertParamIsEq<I>;
            let _: ::core::cmp::AssertParamIsEq<Seq<I>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Repr<I>>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Repr<I>>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Repr<I>>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Repr<I>>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Repr<I>>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Repr<I>>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Repr<I>>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Repr<I>>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Repr<I>>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Repr<I>>>;
            let _: ::core::cmp::AssertParamIsEq<Seq<I>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Repr<I>>>;
            let _: ::core::cmp::AssertParamIsEq<Range>;
        }
    }
    #[automatically_derived]
    impl<I: Integral> ::core::marker::StructuralPartialEq for Repr<I> {}
    #[automatically_derived]
    impl<I: ::core::cmp::PartialEq + Integral> ::core::cmp::PartialEq for Repr<I> {
        #[inline]
        fn eq(&self, other: &Repr<I>) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
                && match (self, other) {
                    (Repr::Zero(__self_0), Repr::Zero(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    (Repr::One(__self_0), Repr::One(__arg1_0)) => *__self_0 == *__arg1_0,
                    (Repr::Seq(__self_0), Repr::Seq(__arg1_0)) => *__self_0 == *__arg1_0,
                    (Repr::Not(__self_0), Repr::Not(__arg1_0)) => *__self_0 == *__arg1_0,
                    (Repr::Or(__self_0, __self_1), Repr::Or(__arg1_0, __arg1_1)) => {
                        *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1
                    }
                    (Repr::And(__self_0, __self_1), Repr::And(__arg1_0, __arg1_1)) => {
                        *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1
                    }
                    (Repr::Xor(__self_0, __self_1), Repr::Xor(__arg1_0, __arg1_1)) => {
                        *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1
                    }
                    (Repr::Add(__self_0, __self_1), Repr::Add(__arg1_0, __arg1_1)) => {
                        *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1
                    }
                    (Repr::Sub(__self_0, __self_1), Repr::Sub(__arg1_0, __arg1_1)) => {
                        *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1
                    }
                    (Repr::Exp(__self_0, __self_1), Repr::Exp(__arg1_0, __arg1_1)) => {
                        *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1
                    }
                    _ => unsafe { ::core::intrinsics::unreachable() }
                }
        }
    }
    #[automatically_derived]
    impl<I: ::core::clone::Clone + Integral> ::core::clone::Clone for Repr<I> {
        #[inline]
        fn clone(&self) -> Repr<I> {
            match self {
                Repr::Zero(__self_0) => Repr::Zero(::core::clone::Clone::clone(__self_0)),
                Repr::One(__self_0) => Repr::One(::core::clone::Clone::clone(__self_0)),
                Repr::Seq(__self_0) => Repr::Seq(::core::clone::Clone::clone(__self_0)),
                Repr::Not(__self_0) => Repr::Not(::core::clone::Clone::clone(__self_0)),
                Repr::Or(__self_0, __self_1) => {
                    Repr::Or(
                        ::core::clone::Clone::clone(__self_0),
                        ::core::clone::Clone::clone(__self_1),
                    )
                }
                Repr::And(__self_0, __self_1) => {
                    Repr::And(
                        ::core::clone::Clone::clone(__self_0),
                        ::core::clone::Clone::clone(__self_1),
                    )
                }
                Repr::Xor(__self_0, __self_1) => {
                    Repr::Xor(
                        ::core::clone::Clone::clone(__self_0),
                        ::core::clone::Clone::clone(__self_1),
                    )
                }
                Repr::Add(__self_0, __self_1) => {
                    Repr::Add(
                        ::core::clone::Clone::clone(__self_0),
                        ::core::clone::Clone::clone(__self_1),
                    )
                }
                Repr::Sub(__self_0, __self_1) => {
                    Repr::Sub(
                        ::core::clone::Clone::clone(__self_0),
                        ::core::clone::Clone::clone(__self_1),
                    )
                }
                Repr::Exp(__self_0, __self_1) => {
                    Repr::Exp(
                        ::core::clone::Clone::clone(__self_0),
                        ::core::clone::Clone::clone(__self_1),
                    )
                }
            }
        }
    }
    impl<I: Integral> Repr<I> {
        pub fn empty() -> Self {
            Self::Zero(Default::default())
        }
        pub fn not(self) -> Self {
            Self::Not(box self)
        }
        pub fn and(self, other: Self) -> Self {
            Self::And(box self, box other)
        }
        pub fn or(self, other: Self) -> Self {
            Self::Or(box self, box other)
        }
        pub fn xor(self, other: Self) -> Self {
            Self::Xor(box self, box other)
        }
        pub fn add(self, other: Self) -> Self {
            Self::Add(box self, box other)
        }
        pub fn sub(self, seq: Seq<I>) -> Self {
            Self::Sub(box self, seq)
        }
        pub fn mul(self, range: Range) -> Self {
            Self::Exp(box self, range)
        }
    }
    impl Repr<char> {
        /// `.` expression that matches any character except for `\n`. To build an
        /// expression that matches any character, including `\n`, use the `any`
        /// method.
        pub fn dot() -> Self {
            Self::Or(
                box Self::Seq(Seq('\0', '\x09')),
                box Self::Seq(Seq('\x0B', '\u{10FFFF}')),
            )
        }
        /// `(?s).` expression that matches any character, including `\n`. To build an
        /// expression that matches any character except for `\n`, then use the
        /// `dot` method.
        pub fn any() -> Self {
            Self::Seq(Seq('\0', '\u{10FFFF}'))
        }
    }
    pub struct Seq<I: Integral>(pub I, pub I);
    #[automatically_derived]
    impl<I: ::core::marker::Copy + Integral> ::core::marker::Copy for Seq<I> {}
    #[automatically_derived]
    impl<I: Integral> ::core::marker::StructuralEq for Seq<I> {}
    #[automatically_derived]
    impl<I: ::core::cmp::Eq + Integral> ::core::cmp::Eq for Seq<I> {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<I>;
        }
    }
    #[automatically_derived]
    impl<I: ::core::clone::Clone + Integral> ::core::clone::Clone for Seq<I> {
        #[inline]
        fn clone(&self) -> Seq<I> {
            Seq(
                ::core::clone::Clone::clone(&self.0),
                ::core::clone::Clone::clone(&self.1),
            )
        }
    }
    #[automatically_derived]
    impl<I: ::core::default::Default + Integral> ::core::default::Default for Seq<I> {
        #[inline]
        fn default() -> Seq<I> {
            Seq(::core::default::Default::default(), ::core::default::Default::default())
        }
    }
    #[automatically_derived]
    impl<I: Integral> ::core::marker::StructuralPartialEq for Seq<I> {}
    #[automatically_derived]
    impl<I: ::core::cmp::PartialEq + Integral> ::core::cmp::PartialEq for Seq<I> {
        #[inline]
        fn eq(&self, other: &Seq<I>) -> bool {
            self.0 == other.0 && self.1 == other.1
        }
    }
    #[automatically_derived]
    impl<I: ::core::cmp::PartialOrd + Integral> ::core::cmp::PartialOrd for Seq<I> {
        #[inline]
        fn partial_cmp(
            &self,
            other: &Seq<I>,
        ) -> ::core::option::Option<::core::cmp::Ordering> {
            match ::core::cmp::PartialOrd::partial_cmp(&self.0, &other.0) {
                ::core::option::Option::Some(::core::cmp::Ordering::Equal) => {
                    ::core::cmp::PartialOrd::partial_cmp(&self.1, &other.1)
                }
                cmp => cmp,
            }
        }
    }
    #[automatically_derived]
    impl<I: ::core::cmp::Ord + Integral> ::core::cmp::Ord for Seq<I> {
        #[inline]
        fn cmp(&self, other: &Seq<I>) -> ::core::cmp::Ordering {
            match ::core::cmp::Ord::cmp(&self.0, &other.0) {
                ::core::cmp::Ordering::Equal => ::core::cmp::Ord::cmp(&self.1, &other.1),
                cmp => cmp,
            }
        }
    }
    impl<I: Integral> Seq<I> {
        pub fn new(from: I, to: I) -> Self {
            if from <= to { Seq(from, to) } else { Seq(to, from) }
        }
        /// Intersect this Seq with the given Seq and return the result.
        ///
        /// If the intersection is empty, then this returns `None`.
        pub fn and(self, other: Self) -> Option<Self> {
            match (max(self.0, other.0), min(self.1, other.1)) {
                (from, to) if from <= to => Some(Self::new(from, to)),
                _ => None,
            }
        }
        /// Union the given overlapping Seq into this Seq.
        ///
        /// If the two Seqs aren't contiguous, then this returns `None`.
        pub fn or(self, other: Self) -> Option<Self> {
            match (max(self.0, other.0), min(self.1, other.1)) {
                (from, to) if from <= to.succ() => Some(Self::new(from, to)),
                _ => None,
            }
        }
        /// Compute the symmetric difference the given Seq from this Seq. This
        /// returns the union of the two Seqs minus its intersection.
        pub fn xor(self, other: Self) -> (Option<Self>, Option<Self>) {
            let or = match self.or(other) {
                None => return (Some(self.clone()), Some(other.clone())),
                Some(or) => or,
            };
            let and = match self.and(other) {
                None => return (Some(self.clone()), Some(other.clone())),
                Some(and) => and,
            };
            or.sub(and)
        }
        /// Subtract the given Seq from this Seq and return the resulting
        /// Seqs.
        ///
        /// If subtraction would result in an empty Seq, then no Seqs are
        /// returned.
        ///
        /// other.0 <= self.0 <= self.1 <= other.1 (self <= other) => (None, None)
        /// self.0 <= other.0 <= other.1 <= self.1 (other <= self) => (lower, upper)
        /// self.0 <= other.0 <= self.1 <= other.1 => (lower, None)
        /// other.0 <= self.0 <= other.1 <= self.1 => (None, uppper)
        pub fn sub(self, other: Self) -> (Option<Self>, Option<Self>) {
            if self.le(&other) {
                return (None, None);
            }
            if self.and(other).is_none() {
                return (Some(self.clone()), None);
            }
            let mut ret = (None, None);
            if self.0 < other.0 {
                ret.0 = Some(Self::new(self.0, other.0.pred()));
            }
            if other.1 < self.1 {
                let range = Self::new(other.1.succ(), self.1);
                if ret.0.is_none() {
                    ret.0 = Some(range);
                } else {
                    ret.1 = Some(range);
                }
            }
            ret
        }
        /// Returns true if and only if this range is a subset of the other range.
        pub fn le(&self, other: &Self) -> bool {
            (other.0 <= self.0 && self.0 <= other.1)
                && (other.0 <= self.1 && self.1 <= other.1)
        }
    }
    impl Seq<char> {
        /// Returns true if and only if this character class will either match
        /// nothing or only ASCII bytes. Stated differently, this returns false
        /// if and only if this class contains a non-ASCII codepoint.
        pub fn is_all_ascii(&self) -> bool {
            self.1 <= '\x7F'
        }
    }
    impl Debug for Seq<char> {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            let start = if !self.0.is_whitespace() && !self.0.is_control() {
                self.0.to_string()
            } else {
                {
                    let res = ::alloc::fmt::format(
                        format_args!("0x{0:X}", self.0 as u32),
                    );
                    res
                }
            };
            let end = if !self.1.is_whitespace() && !self.1.is_control() {
                self.1.to_string()
            } else {
                {
                    let res = ::alloc::fmt::format(
                        format_args!("0x{0:X}", self.1 as u32),
                    );
                    res
                }
            };
            f.debug_struct("Seq<char>").field("0", &start).field("1", &end).finish()
        }
    }
    /// A single character, where a character is either
    /// defined by a Unicode scalar value or an arbitrary byte. Unicode characters
    /// are preferred whenever possible. In particular, a `Byte` variant is only
    /// ever produced when it could match invalid UTF-8.
    /// ==========================================================================
    /// Type of characters. A character is either
    /// defined by a Unicode scalar value or a byte. Unicode characters are used
    /// by default, while bytes are used when Unicode mode (via the `u` flag) is
    /// disabled.
    ///
    /// A character class, regardless of its character type, is represented by a
    /// sequence of non-overlapping non-adjacent ranges of characters.
    ///
    /// Note that unlike [`Literal`](enum.Literal.html), a `Bytes` variant may
    /// be produced even when it exclusively matches valid UTF-8. This is because
    /// a `Bytes` variant represents an intention by the author of the regular
    /// expression to disable Unicode mode, which in turn impacts the semantics of
    /// case insensitive matching. For example, `(?i)k` and `(?i-u)k` will not
    /// match the same set of strings.
    pub trait Integral: Copy + Clone + Debug + PartialEq + Eq + PartialOrd + Ord + Destruct {
        const MIN: Self;
        const MAX: Self;
        fn succ(self) -> Self;
        fn pred(self) -> Self;
        fn as_bytes(self, reverse: bool) -> &'static [u8];
    }
    /// Unicode scalar values
    impl Integral for char {
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
        fn as_bytes(self, reverse: bool) -> &'static [u8] {
            let mut buf = [0u8; 4];
            let len = self.encode_utf8(&mut buf).len();
            let buf = &mut buf[..len];
            if reverse {
                buf.reverse();
            }
            &buf
        }
    }
    pub struct Str<'a>(&'a str);
    impl<'a> IntoIterator for Str<'a> {
        type Item = char;
        type IntoIter = core::str::Chars<'a>;
        fn into_iter(self) -> Self::IntoIter {
            self.0.chars()
        }
    }
    pub enum Range {
        Empty,
        From(usize),
        To(usize),
        Full(usize, usize),
    }
    #[automatically_derived]
    impl ::core::marker::Copy for Range {}
    #[automatically_derived]
    impl ::core::fmt::Debug for Range {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Range::Empty => ::core::fmt::Formatter::write_str(f, "Empty"),
                Range::From(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "From",
                        &__self_0,
                    )
                }
                Range::To(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "To", &__self_0)
                }
                Range::Full(__self_0, __self_1) => {
                    ::core::fmt::Formatter::debug_tuple_field2_finish(
                        f,
                        "Full",
                        __self_0,
                        &__self_1,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for Range {}
    #[automatically_derived]
    impl ::core::cmp::Eq for Range {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<usize>;
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Range {
        #[inline]
        fn clone(&self) -> Range {
            match self {
                Range::Empty => Range::Empty,
                Range::From(__self_0) => {
                    Range::From(::core::clone::Clone::clone(__self_0))
                }
                Range::To(__self_0) => Range::To(::core::clone::Clone::clone(__self_0)),
                Range::Full(__self_0, __self_1) => {
                    Range::Full(
                        ::core::clone::Clone::clone(__self_0),
                        ::core::clone::Clone::clone(__self_1),
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Range {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Range {
        #[inline]
        fn eq(&self, other: &Range) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
                && match (self, other) {
                    (Range::From(__self_0), Range::From(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    (Range::To(__self_0), Range::To(__arg1_0)) => *__self_0 == *__arg1_0,
                    (
                        Range::Full(__self_0, __self_1),
                        Range::Full(__arg1_0, __arg1_1),
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl ::core::cmp::PartialOrd for Range {
        #[inline]
        fn partial_cmp(
            &self,
            other: &Range,
        ) -> ::core::option::Option<::core::cmp::Ordering> {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            match (self, other) {
                (Range::From(__self_0), Range::From(__arg1_0)) => {
                    ::core::cmp::PartialOrd::partial_cmp(__self_0, __arg1_0)
                }
                (Range::To(__self_0), Range::To(__arg1_0)) => {
                    ::core::cmp::PartialOrd::partial_cmp(__self_0, __arg1_0)
                }
                (Range::Full(__self_0, __self_1), Range::Full(__arg1_0, __arg1_1)) => {
                    match ::core::cmp::PartialOrd::partial_cmp(__self_0, __arg1_0) {
                        ::core::option::Option::Some(::core::cmp::Ordering::Equal) => {
                            ::core::cmp::PartialOrd::partial_cmp(__self_1, __arg1_1)
                        }
                        cmp => cmp,
                    }
                }
                _ => ::core::cmp::PartialOrd::partial_cmp(&__self_tag, &__arg1_tag),
            }
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Ord for Range {
        #[inline]
        fn cmp(&self, other: &Range) -> ::core::cmp::Ordering {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            match ::core::cmp::Ord::cmp(&__self_tag, &__arg1_tag) {
                ::core::cmp::Ordering::Equal => {
                    match (self, other) {
                        (Range::From(__self_0), Range::From(__arg1_0)) => {
                            ::core::cmp::Ord::cmp(__self_0, __arg1_0)
                        }
                        (Range::To(__self_0), Range::To(__arg1_0)) => {
                            ::core::cmp::Ord::cmp(__self_0, __arg1_0)
                        }
                        (
                            Range::Full(__self_0, __self_1),
                            Range::Full(__arg1_0, __arg1_1),
                        ) => {
                            match ::core::cmp::Ord::cmp(__self_0, __arg1_0) {
                                ::core::cmp::Ordering::Equal => {
                                    ::core::cmp::Ord::cmp(__self_1, __arg1_1)
                                }
                                cmp => cmp,
                            }
                        }
                        _ => ::core::cmp::Ordering::Equal,
                    }
                }
                cmp => cmp,
            }
        }
    }
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
        pub fn is_match_empty(&self) -> bool {
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
    pub enum Zero {
        #[default]
        Any,
        /// Match the beginning of a line or the beginning of text. Specifically,
        /// this matches at the starting position of the input, or at the position
        /// immediately following a `\n` character.
        StartLine,
        /// Match the end of a line or the end of text. Specifically,
        /// this matches at the end position of the input, or at the position
        /// immediately preceding a `\n` character.
        EndLine,
        /// Match the beginning of text. Specifically, this matches at the starting
        /// position of the input.
        StartText,
        /// Match the end of text. Specifically, this matches at the ending
        /// position of the input.
        EndText,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Zero {
        #[inline]
        fn clone(&self) -> Zero {
            match self {
                Zero::Any => Zero::Any,
                Zero::StartLine => Zero::StartLine,
                Zero::EndLine => Zero::EndLine,
                Zero::StartText => Zero::StartText,
                Zero::EndText => Zero::EndText,
            }
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Zero {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    Zero::Any => "Any",
                    Zero::StartLine => "StartLine",
                    Zero::EndLine => "EndLine",
                    Zero::StartText => "StartText",
                    Zero::EndText => "EndText",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for Zero {}
    #[automatically_derived]
    impl ::core::cmp::Eq for Zero {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Zero {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Zero {
        #[inline]
        fn eq(&self, other: &Zero) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    #[automatically_derived]
    impl ::core::default::Default for Zero {
        #[inline]
        fn default() -> Zero {
            Self::Any
        }
    }
}
pub use consts::{DIGIT, WORD};
pub use repr::{Repr, Seq, Range};
