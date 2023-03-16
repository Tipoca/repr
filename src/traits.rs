use core::{
    fmt::Debug,
    iter::Step,
    marker::Destruct,
    panic::{UnwindSafe, RefUnwindSafe}
};

/// - `Copy` + `Clone`: possibility of `!` exponentiation
/// - `PartialEq` + `Eq`: decidability
#[unconst]
#[const_trait]
pub trait Integral: Copy + ~const Clone
                    + ~const PartialEq + Eq
                    + ~const PartialOrd + ~const Ord
                    + Step
                    + ~const Destruct
                    + Debug
                    + Sync + Send + RefUnwindSafe + UnwindSafe
{
    const MIN: Self;
    const MAX: Self;
    fn succ(self) -> Self;
    fn pred(self) -> Self;
}
