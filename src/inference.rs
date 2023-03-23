use unconst::unconst;

use crate::repr::Repr;
use crate::traits::Integral;

#[unconst]
pub struct In<I: ~const Integral>([I], Repr<I>);
  
match self {
    (i, One(seq)) if i.is(seq) => ,
}
