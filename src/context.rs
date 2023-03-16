use alloc::vec::Vec;
use core::ops::Deref;

use unconst::unconst;

use crate::traits::Integral;

#[unconst]
#[derive_const(PartialEq)]
#[derive(Debug, Eq)]
pub struct Context<I: ~const Integral>(Vec<I>);

#[unconst]
impl<I: ~const Integral> const Deref for Context<I> {
    type Target = Vec<I>;

    fn deref(&self) -> &Vec<I> {
        &self.0
    }
}
