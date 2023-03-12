use alloc::{
    vec::Vec
};

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
