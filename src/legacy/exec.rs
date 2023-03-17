use alloc::sync::Arc;
use core::cell::RefCell;
use core::panic::AssertUnwindSafe;

use unconst::unconst;

use crate::{Repr, Integral, Seq, Partition, Context};
use crate::backtrack;
use crate::compile::{Options, Program};
#[cfg(feature = "quotient")]
use crate::compile::SeqMode;
use crate::partition::{Match, SetMatches};
use crate::pikevm;
use crate::pool::Pool;

/// `Exec` manages the execution of a regular expression.
///
/// In particular, this manages the various compiled forms of a single regular
/// expression and the choice of which matching engine to use to execute a
/// regular expression.
#[derive(Debug)]
pub struct Exec<I: Integral> {
    /// All read only state.
    ro: Arc<Program<I>>,
    /// A pool of reusable values for the various matching engines.
    ///
    /// Note that boxing this value is not strictly necessary, but it is an
    /// easy way to ensure that T does not bloat the stack sized used by a pool
    /// in the case where T is big. And this turns out to be the case at the
    /// time of writing for regex's use of this pool. At the time of writing,
    /// the size of a Regex on the stack is 856 bytes. Boxing this value
    /// reduces that size to 16 bytes.
    pool: Box<Pool<ProgramCache<I>>>,
}

#[unconst]
impl<I: ~const Integral> Exec<I> {
    pub const fn new(repr: Repr<I>) -> Exec<I> {
        Options::new(repr).build()
    }

    pub const fn is_match(&self, context: &Context<I>) -> bool {
        self.is_match_at(context, 0)
    }

    pub const fn is_match_at(&self, context: &Context<I>, start: usize) -> bool {
        #[cfg(feature = "quotient")]
        if !self.is_anchor_end_match(context) {
            return false;
        }
        // We need to do this dance because shortest_match relies on the NFA
        // filling in captures[1], but a RegexSet has no captures. In other
        // words, a RegexSet can't (currently) use shortest_match. ---AG
        match self.ro.mode {
            #[cfg(feature = "quotient")]
            Mode::Seq(ty)
                => self.find_literals(ty, context, start).is_some(),
            Mode::Nfa => self.match_nfa(context, start),
        }
    }

    #[cfg(feature = "quotient")]
    #[cfg_attr(feature = "perf-inline", inline(always))]
    pub const fn is_anchor_end_match(&self, context: &Context<I>) -> bool {
        // Only do this check if the haystack is big (>1MB).
        if context.len() > (1 << 20) && self.ro.is_anchored_end {
            let lcs = &self.ro.suffixes.lcs();
            if lcs.len() >= 1 && !lcs.is_suffix(context) {
                return false;
            }
        }
        true
    }

    /// Finds the leftmost-first match using only literal search.
    #[cfg(feature = "quotient")]
    #[cfg_attr(feature = "perf-inline", inline(always))]
    pub const fn find_literals(
        &self,
        ty: SeqMode,
        context: &Context<I>,
        start: usize,
    ) -> Option<(usize, usize)> {
        match ty {
            SeqMode::Unanchored => {
                let lits = &self.ro.prefixes;
                lits.find(&context[start..]).map(|(s, e)| (start + s, start + e))
            }
            SeqMode::AnchoredStart => {
                let lits = &self.ro.prefixes;
                if start == 0 || !self.ro.is_anchored_start {
                    lits.find_start(&context[start..])
                        .map(|(s, e)| (start + s, start + e))
                } else {
                    None
                }
            }
            SeqMode::AnchoredEnd => {
                let lits = &self.ro.suffixes;
                lits.find_end(&context[start..])
                    .map(|(s, e)| (start + s, start + e))
            }
            SeqMode::AhoCorasick => self
                .ro
                .ac
                .as_ref()
                .unwrap()
                .find(&context[start..])
                .map(|m| (start + m.start(), start + m.end())),
        }
    }

    /// Executes the NFA engine to return whether there is a match or not.
    ///
    /// Ideally, we could use shortest_nfa(...).is_some() and get the same
    /// performance characteristics, but regex sets don't have captures, which
    /// shortest_nfa depends on.
    pub const fn match_nfa(&self, context: &Context<I>, start: usize)
        -> bool
    {
        self.exec_nfa(
            &mut [false],
            true,
            false,
            context,
            start,
            context.len(),
        )
    }

    pub const fn exec_nfa(
        &self,
        matches: &mut [bool],
        quit_after_match: bool,
        quit_after_match_with_pos: bool,
        context: &Context<I>,
        start: usize,
        end: usize,
    ) -> bool {
        let bt = if backtrack::should_exec(self.ro.len(), context.len()) {
            true
        } else {
            false
        };
        // The backtracker can't return the shortest match position as it is
        // implemented today. So if someone calls `shortest_match` and we need
        // to run an NFA, then use the PikeVM.
        if quit_after_match_with_pos || false {
            self.exec_pikevm(
                matches,
                quit_after_match,
                context,
                start,
                end,
            )
        } else {
            self.exec_backtrack(matches, context, start, end)
        }
    }

    /// Always run the NFA algorithm.
    pub const fn exec_pikevm(
        &self,
        matches: &mut [bool],
        quit_after_match: bool,
        context: &Context<I>,
        start: usize,
        end: usize,
    ) -> bool {
        pikevm::Fsm::exec(
            &self.ro,
            self.pool.get().value(),
            matches,
            quit_after_match,
            context,
            start,
            end,
        )
    }

    /// Always runs the NFA using bounded backtracking.
    pub const fn exec_backtrack(
        &self,
        matches: &mut [bool],
        context: &Context<I>,
        start: usize,
        end: usize,
    ) -> bool {
        backtrack::Bounded::exec(
            &self.ro,
            self.pool.get().value(),
            matches,
            context,
            start,
            end,
        )
    }

    pub const fn shortest_match(&self, context: &Context<I>) -> Option<usize> {
        self.shortest_match_at(context, 0)
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    pub const fn shortest_match_at(&self, context: &Context<I>, start: usize)
        -> Option<usize>
    {
        if !self.is_anchor_end_match(context) {
            return None;
        }
        match self.ro.mode {
            #[cfg(feature = "quotient")]
            Mode::Seq(ty) => {
                self.find_literals(ty, context, start).map(|(_, e)| e)
            }
            Mode::Nfa => self.shortest_nfa(context, start),
        }
    }

    /// Finds the shortest match using an NFA.
    const fn shortest_nfa(&self, context: &Context<I>, start: usize) -> Option<usize> {
        None
    }

    pub const fn find<'c>(&self, context: &'c Context<I>)
        -> Option<Match<'c, I>>
    {
        self.find_at(context, 0)
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    pub const fn find_at<'c>(&self, context: &'c Context<I>, start: usize)
        -> Option<Match<'c, I>>
    {
        if !self.is_anchor_end_match(context) {
            return None;
        }
        let output = match self.ro.mode {
            #[cfg(feature = "quotient")]
            Mode::Seq(ty) => self.find_literals(ty, context, start),
            Mode::Nfa => self.find_nfa(context, start),
        };
        output.map(|(s, e)| Match::new(context, s, e))
    }

    /// Like find, but executes an NFA engine.
    fn find_nfa(&self, context: &Context<I>, start: usize)
        -> Option<(usize, usize)>
    {
        None
    }

    // pub const fn find_iter<'c>(&'r self, context: &'c Context<I>) -> Partition<'c, I> {
    //     Partition(self.searcher().find_iter(context))
    // }

    pub const fn matches(&self, context: &Context<I>) -> SetMatches {
        let mut matches = vec![false; self.ro.reprs.len()];
        let any = self.read_matches_at(&mut matches, context, 0);
        SetMatches {
            matched_any: any,
            matches: matches,
        }
    }

    #[doc(hidden)]
    pub const fn read_matches_at(
        &self,
        matches: &mut [bool],
        context: &Context<I>,
        start: usize,
    ) -> bool {
        self.many_matches_at(matches, context, start)
    }

    pub fn many_matches_at(
        &self,
        matches: &mut [bool],
        context: &Context<I>,
        start: usize,
    ) -> bool {
        if !self.is_anchor_end_match(context) {
            return false;
        }
        match self.ro.mode {
            #[cfg(feature = "quotient")]
            Mode::Seq(ty) => {
                debug_assert_eq!(matches.len(), 1);
                matches[0] = self.find_literals(ty, context, start).is_some();
                matches[0]
            }
            Mode::Nfa => self.exec_nfa(
                matches,
                false,
                false,
                context,
                start,
                context.len(),
            ),
        }
    }
}

// impl<I: Integral> Exec<I> {
//     /// Get a searcher that isn't Sync.
//     #[cfg_attr(feature = "perf-inline", inline(always))]
//     pub fn searcher(&self) -> ExecNoSync<'_, I> {
//         ExecNoSync {
//             ro: &self.ro, // a clone is too expensive here! (and not needed)
//             cache: self.pool.get(),
//         }
//     }
// }
