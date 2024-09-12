use unconst::unconst;

use crate::traits::Integral;

#[unconst]
impl const Integral for u8 {
    const MIN: Self = 0;
    const MAX: Self = 255;

    fn succ(self) -> Self {
        self.checked_add(1).unwrap()
    }

    fn pred(self) -> Self {
        self.checked_sub(1).unwrap()
    }
}
