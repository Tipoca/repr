//! TODO(rnarkk) Refactor and relocate them in crate::repr
//! <https://en.wikipedia.org/wiki/Interval_arithmetic>
//! <https://en.wikipedia.org/wiki/Boundary_(topology)>
//! <https://en.wikipedia.org/wiki/Partition_of_a_set>
//! <https://en.wikipedia.org/wiki/Sequence>

// This module contains an *internal* implementation of interval sets.
//
// The primary invariant that interval sets guards is canonical ordering. That
// is, every interval set contains an ordered sequence of intervals where
// no two intervals are overlapping or adjacent. While this invariant is
// occasionally broken within the implementation, it should be impossible for
// callers to observe it.
//
// Since case folding (as implemented below) breaks that invariant, we roll
// that into this API even though it is a little out of place in an otherwise
// generic interval set. (Hence the reason why the `unicode` module is imported
// here.)
//
// Some of the implementation complexity here is a result of me wanting to
// preserve the sequential representation without using additional memory.
// In many cases, we do use linear extra memory, but it is at most 2x and it
// is amortized. If we relaxed the memory requirements, this implementation
// could become much simpler. The extra memory is honestly probably OK, but
// character classes (especially of the Unicode variety) can become quite
// large, and it would be nice to keep regex compilation snappy even in debug
// builds. (In the past, I have been careless with this area of code and it has
// caused slow regex compilations in debug mode, so this isn't entirely
// unwarranted.)
//
// Tests on this are relegated to the public API of HIR in src/hir.rs.
// Tests for interval sets are written in src/hir.rs against the public API.
