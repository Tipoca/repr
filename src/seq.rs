use alloc::{vec, vec::Vec};
use core::ops::Deref;

use unconst::unconst;

use crate::traits::Integral;

#[unconst]
#[derive_const(Clone, PartialEq, PartialOrd, Ord)]
#[derive(Debug, Eq)]
pub struct Seq<I: ~const Integral>(Vec<I>);

#[unconst]
impl<I: ~const Integral> Seq<I> {
    pub const fn empty() -> Self {
        Seq(Vec::new())
    }

    pub const fn new<M: ~const IntoIterator<Item = I>>(is: M) -> Self {
        Seq(is.into_iter().collect())
    }

    pub const fn one(i: I) -> Self {
        Seq(vec![i])
    }

    #[allow(clippy::should_implement_trait)]
    pub const fn mul(mut self, other: Self) -> Self {
        self.0.extend(other);
        self
    }

    pub const fn rev(self) -> Self {
        Seq(self.0.into_iter().rev().collect())
    }

    pub const fn null(&self) -> bool {
        self.0.is_empty()
    }

    pub const fn len(&self) -> usize {
        self.0.len()
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
