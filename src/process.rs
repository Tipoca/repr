//! <https://en.wikipedia.org/wiki/Process_calculus>
//! <https://en.wikipedia.org/wiki/Communicating_sequential_processes>
//! <https://en.wikipedia.org/wiki/Calculus_of_communicating_systems>

use unconst::unconst;

use crate::context::Context;
use crate::partition::Partition;
use crate::repr::Repr::{self, *};
use crate::traits::Integral;

#[unconst]
#[derive(Debug)]
pub struct Process<'c, 'm, I: ~const Integral> {
    repr: Repr<I>,
    context: &'c Context<I>,
    matches: &'m mut [bool],
}

#[unconst]
pub const fn next<I>(
    repr: &Repr<I>, mut slice: &[I], partition: &mut Partition<I>)
    where I: ~const Integral
{
    match repr {
        // True => {
        //     return Some(index);
        // }
        // Zero(zero) => {
        //     if zero.yes(slice) {
        //         return Some(0);
        //     } else {
        //         return None;
        //     }
        // }
        One(seq) => {
            let slice = &slice[..seq.len()];
            if seq.as_slice() == slice {
                slice = &slice[seq.len()..];
            }
        }
        Interval(interval) => {
            if interval.has(slice[1]) {
                slice = &slice[1..];
            }
        }
        Mul(lhs, rhs) => {
            next(lhs, slice, partition);
            next(rhs, slice, partition);
        }
        Or(lhs, rhs) => {
            next(lhs, slice, &mut partition.clone());
            next(rhs, slice, &mut partition)
        }
        // Add(lhs, rhs) => {

        // }
        _ => unimplemented!()
    }
}
