// #![no_std]
#![feature(pattern)]
#![feature(box_syntax)]
#![feature(once_cell)]
#![feature(step_trait)]
#![feature(stmt_expr_attributes)]
// #![feature(negative_impls)]
// #![feature(specialization)]
#![feature(derive_const)]
#![feature(const_trait_impl)]
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
// #![feature(const_iter)]

extern crate alloc; 

mod backtrack;
mod compile;
mod context;
mod conversions;
mod exec;
mod interval;
mod operators;
mod partition;
mod pattern;
mod pikevm;
mod pool;
mod process;
mod seq;
mod sparse;
mod unicode;
mod wrappers;

pub mod char;
pub mod constants;
pub mod macros;
pub mod quotient;
pub mod traits;
pub mod repr;

pub use constants::perl::{DIGIT, WORD};
pub use context::Context;
pub use interval::Interval;
pub use partition::Partition;
pub use crate::repr::{Repr, Zero};
pub use seq::Seq;
pub use traits::Integral;
