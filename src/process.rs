use unconst::unconst;

// use crate::context::Context;
use crate::partition::Partition;
use crate::repr::Repr::{self, *};
use crate::traits::Integral;

#[unconst]
impl<I: ~const Integral> Repr<I> {
    pub const fn step(&self, slice: &[I], partition: &mut Partition<I>) -> Option<&[I]> {
        match self {
            True => {
            //     if slot < self.matches.len() {
            //         self.matches[slot] = true;
            //     }
                return Some(&[]);
            }
            Zero(zero) => {
                if slice.is_empty_match(zero) {
                    return Some(slice);
                } else {
                    return None;
                }
            }
            One(seq) => {
                let slice = &slice[0..seq.len()];
                if seq.as_slice() == slice {
                    return Some(slice);
                } else {
                    return None;
                }
            }
            Interval(interval) => {
                if interval.has(slice[0]) {
                    return Some(&slice[1..]);
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
            _ => unimplemented!()
        }
    }
}
