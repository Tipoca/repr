use alloc::sync::Arc;
use core::cell::RefCell;
use core::panic::AssertUnwindSafe;

use unconst::unconst;

use crate::{Repr, Integral, Seq, Partition, Context};
use crate::backtrack;
use crate::compile::{Program, Mode};
#[cfg(feature = "derivative")]
use crate::compile::SeqMode;
use crate::options::Options;
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
    /// Create a new regex set from an iterator of strings:
    ///
    /// ```rust
    /// # use regex::RegexSet;
    /// let set = RegexSet::new(&[r"\w+", r"\d+"]).unwrap();
    /// assert!(set.is_match("foo"));
    /// ```
    pub const fn new(repr: Repr<I>) -> Exec<I> {
        Options::new(repr).build()
    }

    /// Returns true if and only if one of the regexes in this set matches
    /// the text given.
    ///
    /// This method should be preferred if you only need to test whether any
    /// of the regexes in the set should match, but don't care about *which*
    /// regexes matched. This is because the underlying matching engine will
    /// quit immediately after seeing the first match instead of continuing to
    /// find all matches.
    ///
    /// Note that as with searches using `Regex`, the expression is unanchored
    /// by default. That is, if the regex does not start with `^` or `\A`, or
    /// end with `$` or `\z`, then it is permitted to match anywhere in the
    /// text.
    ///
    /// # Example
    ///
    /// Tests whether a set matches some text:
    ///
    /// ```rust
    /// # use regex::RegexSet;
    /// let set = RegexSet::new(&[r"\w+", r"\d+"]).unwrap();
    /// assert!(set.is_match("foo"));
    /// assert!(!set.is_match("☃"));
    /// ```
    /// 
    /// Test if some text contains at least one word with exactly 13
    /// Unicode word characters:
    ///
    /// ```rust
    /// # use regex::Regex;
    /// # fn main() {
    /// let text = "I categorically deny having triskaidekaphobia.";
    /// assert!(Regex::new(r"\b\w{13}\b").unwrap().is_match(text));
    /// # }
    pub const fn is_match(&self, context: &Context<I>) -> bool {
        self.is_match_at(context, 0)
    }

    /// Returns the same as is_match, but starts the search at the given
    /// offset.
    /// 
    /// For single regular expressions, this is equivalent to calling
    /// shortest_match(...).is_some().
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    pub const fn is_match_at(&self, context: &Context<I>, start: usize) -> bool {
        if !self.is_anchor_end_match(context) {
            return false;
        }
        // We need to do this dance because shortest_match relies on the NFA
        // filling in captures[1], but a RegexSet has no captures. In other
        // words, a RegexSet can't (currently) use shortest_match. ---AG
        match self.ro.mode {
            #[cfg(feature = "derivative")]
            Mode::Seq(ty)
                => self.find_literals(ty, context, start).is_some(),
            Mode::Nfa => self.match_nfa(context, start),
        }
    }

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
    #[cfg(feature = "derivative")]
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

    /// Returns the end location of a match in the text given.
    ///
    /// This method may have the same performance characteristics as
    /// `is_match`, except it provides an end location for a match. In
    /// particular, the location returned *may be shorter* than the proper end
    /// of the leftmost-first match.
    ///
    /// # Example
    ///
    /// Typically, `a+` would match the entire first sequence of `a` in some
    /// text, but `shortest_match` can give up as soon as it sees the first
    /// `a`.
    ///
    /// ```rust
    /// # use regex::Regex;
    /// # fn main() {
    /// let text = "aaaaa";
    /// let pos = Regex::new(r"a+").unwrap().shortest_match(text);
    /// assert_eq!(pos, Some(1));
    /// # }
    /// ```
    pub const fn shortest_match(&self, context: &Context<I>) -> Option<usize> {
        self.shortest_match_at(context, 0)
    }

    /// Returns the end of a match location, possibly occurring before the
    /// end location of the correct leftmost-first match.
    /// ================================================================
    /// Returns the same as shortest_match, but starts the search at the given
    /// offset.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    #[cfg_attr(feature = "perf-inline", inline(always))]
    pub const fn shortest_match_at(&self, context: &Context<I>, start: usize)
        -> Option<usize>
    {
        if !self.is_anchor_end_match(context) {
            return None;
        }
        match self.ro.mode {
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

    /// Returns the start and end byte range of the leftmost-first match in
    /// `text`. If no match exists, then `None` is returned.
    ///
    /// Note that this should only be used if you want to discover the position
    /// of the match. Testing the existence of a match is faster if you use
    /// `is_match`.
    ///
    /// # Example
    ///
    /// Find the start and end location of the first word with exactly 13
    /// Unicode word characters:
    ///
    /// ```rust
    /// # use regex::Regex;
    /// # fn main() {
    /// let text = "I categorically deny having triskaidekaphobia.";
    /// let mat = Regex::new(r"\b\w{13}\b").unwrap().find(text).unwrap();
    /// assert_eq!(mat.start(), 2);
    /// assert_eq!(mat.end(), 15);
    /// # }
    /// ```
    pub const fn find<'c>(&self, context: &'c Context<I>)
        -> Option<Match<'c, I>>
    {
        self.find_at(context, 0)
    }

    /// Finds the start and end location of the leftmost-first match, starting
    /// at the given location.
    /// ========================================================
    /// Returns the same as find, but starts the search at the given
    /// offset.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    #[cfg_attr(feature = "perf-inline", inline(always))]
    pub const fn find_at<'c>(&self, context: &'c Context<I>, start: usize)
        -> Option<Match<'c, I>>
    {
        if !self.is_anchor_end_match(context) {
            return None;
        }
        let output = match self.ro.mode {
            #[cfg(feature = "derivative")]
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

    // /// Returns an iterator for each successive non-overlapping match in
    // /// `text`, returning the start and end byte indices with respect to
    // /// `text`.
    // ///
    // /// # Example
    // ///
    // /// Find the start and end location of every word with exactly 13 Unicode
    // /// word characters:
    // ///
    // /// ```rust
    // /// # use regex::Regex;
    // /// # fn main() {
    // /// let text = "Retroactively relinquishing remunerations is reprehensible.";
    // /// for mat in Regex::new(r"\b\w{13}\b").unwrap().find_iter(text) {
    // ///     println!("{:?}", mat);
    // /// }
    // /// # }
    // /// ```
    // pub const fn find_iter<'c>(&'r self, context: &'c Context<I>) -> Partition<'c, I> {
    //     Partition(self.searcher().find_iter(context))
    // }

        /// Returns the set of regular expressions that match in the given text.
    ///
    /// The set returned contains the index of each regular expression that
    /// matches in the given text. The index is in correspondence with the
    /// order of regular expressions given to `RegexSet`'s constructor.
    ///
    /// The set can also be used to iterate over the matched indices.
    ///
    /// Note that as with searches using `Regex`, the expression is unanchored
    /// by default. That is, if the regex does not start with `^` or `\A`, or
    /// end with `$` or `\z`, then it is permitted to match anywhere in the
    /// text.
    ///
    /// # Example
    ///
    /// Tests which regular expressions match the given text:
    ///
    /// ```rust
    /// # use regex::RegexSet;
    /// let set = RegexSet::new(&[
    ///     r"\w+",
    ///     r"\d+",
    ///     r"\pL+",
    ///     r"foo",
    ///     r"bar",
    ///     r"barfoo",
    ///     r"foobar",
    /// ]).unwrap();
    /// let matches: Vec<_> = set.matches("foobar").into_iter().collect();
    /// assert_eq!(matches, vec![0, 2, 3, 4, 6]);
    ///
    /// // You can also test whether a particular regex matched:
    /// let matches = set.matches("foobar");
    /// assert!(!matches.matched(5));
    /// assert!(matches.matched(6));
    /// ```
    pub const fn matches(&self, context: &Context<I>) -> SetMatches {
        let mut matches = vec![false; self.ro.reprs.len()];
        let any = self.read_matches_at(&mut matches, context, 0);
        SetMatches {
            matched_any: any,
            matches: matches,
        }
    }

    /// Returns the same as matches, but starts the search at the given
    /// offset and stores the matches into the slice given.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    ///
    /// `matches` must have a length that is at least the number of regexes
    /// in this set.
    ///
    /// This method returns true if and only if at least one member of
    /// `matches` is true after executing the set against `text`.
    #[doc(hidden)]
    pub const fn read_matches_at(
        &self,
        matches: &mut [bool],
        context: &Context<I>,
        start: usize,
    ) -> bool {
        self.many_matches_at(matches, context, start)
    }

    /// Finds which regular expressions match the given text.
    ///
    /// `matches` should have length equal to the number of regexes being
    /// searched.
    ///
    /// This is only useful when one wants to know which regexes in a set
    /// match some text.
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
            #[cfg(feature = "derivative")]
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

#[unconst]
impl<I: ~const Integral> Clone for Exec<I> {
    fn clone(&self) -> Exec<I> {
        let pool = new_pool(&self.ro);
        Exec { ro: self.ro.clone(), pool }
    }
}

#[unconst]
pub fn new_pool<I>(prog: &Arc<Program<I>>) -> Box<Pool<ProgramCache<I>>>
    where I: ~const Integral
{
    let prog = prog.clone();
    Box::new(Pool::new(Box::new(move || {
        AssertUnwindSafe(RefCell::new(ProgramCacheInner::new(&prog)))
    })))
}

#[unconst]
/// `ProgramCache` maintains reusable allocations for each matching engine
/// available to a particular program.
///
/// We declare this as unwind safe since it's a cache that's only used for
/// performance purposes. If a panic occurs, it is (or should be) always safe
/// to continue using the same regex object.
pub type ProgramCache<I: ~const Integral>
    = AssertUnwindSafe<RefCell<ProgramCacheInner<I>>>;

#[derive(Debug)]
pub struct ProgramCacheInner<I: Integral> {
    pub pikevm: pikevm::Cache,
    pub backtrack: backtrack::Cache<I>,
}

#[unconst]
impl<I: ~const Integral> ProgramCacheInner<I> {
    fn new(ro: &Program<I>) -> Self {
        ProgramCacheInner {
            pikevm: pikevm::Cache::new(&ro),
            backtrack: backtrack::Cache::new(&ro),
        }
    }
}
