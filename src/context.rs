use core::ops::Deref;

use unconst::unconst;

use crate::repr::Integral;

#[unconst]
pub struct Context<I: ~const Integral>(Vec<I>);

#[unconst]
impl<I: ~const Integral> const Deref for Context<I> {
    type Target = Vec<I>;

    fn deref(&self) -> &Vec<I> {
        &self.0
    }
}
