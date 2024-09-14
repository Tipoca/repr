// #![no_std]
#![feature(pattern)]
#![feature(step_trait)]
#![feature(stmt_expr_attributes)]
#![feature(negative_impls)]
// #![feature(specialization)]
#![feature(derive_const)]
#![feature(const_trait_impl)]
#![feature(const_try)]
#![feature(const_for)]
#![feature(const_box)]
#![feature(const_mut_refs)]
#![feature(const_option)]
#![feature(const_refs_to_cell)]
#![feature(const_heap)]
// #![feature(const_iter)]

extern crate alloc;

mod bytes;
mod context;
mod conversion;
mod functor;
mod interval;
mod operators;
// mod partition;
// mod pattern;
// mod pool;
// mod process;
mod regex;
mod seq;
// mod sparse;
mod string;

pub mod constants;
pub mod macros;
// #[cfg(feature = "quotient")]
// pub mod quotient;
pub mod repr;
pub mod traits;
pub mod wrappers;

pub use constants::perl::{DIGIT, WORD};
pub use context::Context;
pub use interval::Interval;
// pub use partition::Partition;
pub use crate::repr::Repr::{self, *};
pub use seq::Seq;
pub use traits::Integral;
