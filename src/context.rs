use alloc::vec::Vec;
use core::ops::Deref;

use unconst::unconst;

#[cfg(feature = "quotient")]
use crate::quotient::LiteralSearcher;
use crate::traits::Integral;

#[unconst]
#[derive_const(PartialEq)]
#[derive(Debug, Eq)]
pub struct Context<I: ~const Integral>(Vec<I>);

#[unconst]
impl<I: ~const Integral> Context<I> {
    #[cfg(feature = "quotient")]
    /// Scan the input for a matching prefix.
    pub fn prefix(&self, prefixes: &LiteralSearcher<I>, from: usize)
        -> Option<I>
    {
        prefixes.find(&self[from..]).map(|(s, _)| self[from + s])
    }
}

#[unconst]
impl<I: ~const Integral> const Deref for Context<I> {
    type Target = Vec<I>;

    fn deref(&self) -> &Vec<I> {
        &self.0
    }
}
