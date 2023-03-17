use unconst::unconst;

use crate::context::Context;
use crate::partition::Partition;
use crate::repr::Repr::{self, *};
use crate::traits::Integral;

#[unconst]
/// A backtracking matching engine.
#[derive(Debug)]
pub struct Process<'c, 'm, I: ~const Integral> {
    repr: Repr<I>,
    context: &'c Context<I>,
    matches: &'m mut [bool],
}

#[unconst]
impl<'c, 'm, I: ~const Integral> Process<'c, 'm, I> {
    pub const fn step(&self, index: usize, slice: &[I], partition: &mut Partition<I>) -> Option<usize> {
        match self.repr {
            True => {
                return Some(index);
            }
            Zero(zero) => {
                if self.context.yes(zero) {
                    return Some(slice);
                } else {
                    return None;
                }
            }
            One(seq) => {
                let slice = &self.context[index..index + seq.len()];
                if seq.as_slice() == slice {
                    return Some(index + seq.len());
                } else {
                    return None;
                }
            }
            Interval(interval) => {
                if interval.has(self.context[index]) {
                    return Some(index + 1);
                } else {
                    return None;
                }
            }
            Mul(lhs, rhs) => {
                lhs.step(slice, partition)
                .and_then(|slice| rhs.step(slice, partition))
            }
            Or(lhs, rhs) => {
                lhs.step(slice, &mut partition.clone())
                .or_else(|| rhs.step(slice, partition))
            }
            // Add(lhs, rhs) => {

            // }
            _ => unimplemented!()
        }
    }
}
