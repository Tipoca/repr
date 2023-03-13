use alloc::vec::Vec;
use core::ops::Deref;

use unconst::unconst;

use crate::repr::Integral;

#[unconst]
#[derive_const(Clone, Debug, PartialEq, PartialOrd, Ord)]
#[derive(Eq)]
pub struct Seq<I: ~const Integral>(Vec<I>);

#[unconst]
impl<I: ~const Integral> Seq<I> {
    pub const fn empty() -> Self {
        Seq(Vec::new())
    }

    pub const fn new<M: ~const Iterator<Item = I>>(is: M) -> Self {
        Seq(is.collect())
    }

    pub const fn one(i: I) -> Self {
        Seq(vec![i])
    }

    pub const fn mul(self, other: Self) -> Self {
        self.0.extend(other);
        self
    }

    pub const fn rev(self) -> Self {
        Seq(self.0.into_iter().rev().collect())
    }
}

#[unconst]
impl<I: ~const Integral> const AsRef<[I]> for Seq<I> {
    fn as_ref(&self) -> &[I] {
        &self.0
    }
}

#[unconst]
impl<I: ~const Integral> const Deref for Seq<I> {
    type Target = Vec<I>;
    fn deref(&self) -> &Vec<I> {
        &self.0
    }
}

#[unconst]
impl<I: ~const Integral> const IntoIterator for Seq<I> {
    type Item = I;
    type IntoIter = <Vec<I> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
