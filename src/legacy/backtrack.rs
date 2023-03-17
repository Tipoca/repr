/*!
This is the backtracking matching engine. It has the same exact capability
as the full NFA simulation, except it is artificially restricted to small
regexes on small inputs because of its memory requirements.

In particular, this is a *bounded* backtracking engine. It retains worst
case linear time by keeping track of the states that it has visited (using a
bitmap). Namely, once a state is visited, it is never visited again. Since a
state is keyed by `(instruction index, input index)`, we have that its time
complexity is `O(mn)` (i.e., linear in the size of the search text).

The backtracking engine can beat out the NFA simulation on small
regexes/inputs because it doesn't have to keep track of multiple copies of
the capture groups. In benchmarks, the backtracking engine is roughly twice
as fast as the full NFA simulation. Note though that its performance doesn't
scale, even if you're willing to live with the memory requirements. Namely,
the bitset has to be zeroed on each execution, which becomes quite expensive
on large bitsets.
*/

use unconst::unconst;

/// Sets the matching engine to use a bounded backtracking engine no
/// matter what optimizations are possible.
///
/// One must use this with care, since the bounded backtracking engine
/// uses memory proportion to `len(regex) * len(context)`.
///
/// This overrides whatever was previously set via the `automatic` or
/// `nfa` methods.

use crate::traits::Integral;

type Bits = u32;

const BIT_SIZE: usize = 32;
const MAX_SIZE_BYTES: usize = 256 * (1 << 10); // 256 KB

/// Returns true iff the given regex and input should be executed by this
/// engine with reasonable memory usage.
pub fn should_exec(num_insts: usize, text_len: usize) -> bool {
    // Total memory usage in bytes is determined by:
    //
    //   ((len(insts) * (len(input) + 1) + bits - 1) / bits) * (size_of(u32))
    //
    // The actual limit picked is pretty much a heuristic.
    // See: https://github.com/rust-lang/regex/issues/215
    let size = ((num_insts * (text_len + 1) + BIT_SIZE - 1) / BIT_SIZE) * 4;
    size <= MAX_SIZE_BYTES
}

#[unconst]
/// A backtracking matching engine.
#[derive(Debug)]
pub struct Bounded<'c, 'a, 'm, I: ~const Integral> {
    prog: Program<I>,
    context: &'c Context<I>,
    matches: &'m mut [bool],
    cache: &'a mut Cache<I>,
}
