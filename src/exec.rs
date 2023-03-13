use alloc::sync::Arc;
use core::cell::RefCell;
use core::panic::AssertUnwindSafe;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, MatchKind};
use unconst::unconst;

use crate::{Repr, Integral, Seq, Partition, Context};
use crate::backtrack;
use crate::compile::Compiler;
use crate::derivative::{Literals, LiteralSearcher};
use crate::options::Options;
use crate::partition::Match;
use crate::pool::Pool;
use crate::program::Program;

// use super::pikevm;

/// A compiled regular expression for matching Unicode strings.
///
/// It is represented as either a sequence of bytecode instructions (dynamic)
/// or as a specialized Rust function (native). It can be used to search, split
/// or replace text. All searching is done with an implicit `.*?` at the
/// beginning and end of an expression. To force an expression to match the
/// whole string (or a prefix or a suffix), you must use an anchor like `^` or
/// `$` (or `\A` and `\z`).
///
/// While this crate will handle Unicode strings (whether in the regular
/// expression or in the search text), all positions returned are **byte
/// indices**. Every byte index is guaranteed to be at a Unicode code point
/// boundary.
///
/// The lifetimes `'r` and `'t` in this crate correspond to the lifetime of a
/// compiled regular expression and text to search, respectively.
///
/// The only methods that allocate new strings are the string replacement
/// methods. All other methods (searching and splitting) return borrowed
/// pointers into the string given.
///
/// # Examples
///
/// Find the location of a US phone number:
///
/// ```rust
/// # use regex::Regex;
/// let re = Regex::new("[0-9]{3}-[0-9]{3}-[0-9]{4}").unwrap();
/// let mat = re.find("phone: 111-222-3333").unwrap();
/// assert_eq!((mat.start(), mat.end()), (7, 19));
/// ```
///
/// # Using the `std::str::pattern` methods with `Regex`
///
/// > **Note**: This section requires that this crate is compiled with the
/// > `pattern` Cargo feature enabled, which **requires nightly Rust**.
///
/// Since `Regex` implements `Pattern`, you can use regexes with methods
/// defined on `&str`. For example, `is_match`, `find`, `find_iter`
/// and `split` can be replaced with `str::contains`, `str::find`,
/// `str::match_indices` and `str::split`.
///
/// Here are some examples:
///
/// ```rust,ignore
/// # use regex::Regex;
/// let re = Regex::new(r"\d+").unwrap();
/// let haystack = "a111b222c";
///
/// assert!(haystack.contains(&re));
/// assert_eq!(haystack.find(&re), Some(1));
/// assert_eq!(haystack.match_indices(&re).collect::<Vec<_>>(),
///            vec![(1, "111"), (5, "222")]);
/// assert_eq!(haystack.split(&re).collect::<Vec<_>>(), vec!["a", "b", "c"]);
/// ```
/// =====================================================
/// Match multiple (possibly overlapping) regular expressions in a single scan.
///
/// A regex set corresponds to the union of two or more regular expressions.
/// That is, a regex set will match text where at least one of its
/// constituent regular expressions matches. A regex set as its formulated here
/// provides a touch more power: it will also report *which* regular
/// expressions in the set match. Indeed, this is the key difference between
/// regex sets and a single `Regex` with many alternates, since only one
/// alternate can match at a time.
///
/// For example, consider regular expressions to match email addresses and
/// domains: `[a-z]+@[a-z]+\.(com|org|net)` and `[a-z]+\.(com|org|net)`. If a
/// regex set is constructed from those regexes, then searching the text
/// `foo@example.com` will report both regexes as matching. Of course, one
/// could accomplish this by compiling each regex on its own and doing two
/// searches over the text. The key advantage of using a regex set is that it
/// will report the matching regexes using a *single pass through the text*.
/// If one has hundreds or thousands of regexes to match repeatedly (like a URL
/// router for a complex web application or a user agent matcher), then a regex
/// set can realize huge performance gains.
///
/// # Example
///
/// This shows how the above two regexes (for matching email addresses and
/// domains) might work:
///
/// ```rust
/// # use regex::RegexSet;
/// let set = RegexSet::new(&[
///     r"[a-z]+@[a-z]+\.(com|org|net)",
///     r"[a-z]+\.(com|org|net)",
/// ]).unwrap();
///
/// // Ask whether any regexes in the set match.
/// assert!(set.is_match("foo@example.com"));
///
/// // Identify which regexes in the set match.
/// let matches: Vec<_> = set.matches("foo@example.com").into_iter().collect();
/// assert_eq!(vec![0, 1], matches);
///
/// // Try again, but with text that only matches one of the regexes.
/// let matches: Vec<_> = set.matches("example.com").into_iter().collect();
/// assert_eq!(vec![1], matches);
///
/// // Try again, but with text that doesn't match any regex in the set.
/// let matches: Vec<_> = set.matches("example").into_iter().collect();
/// assert!(matches.is_empty());
/// ```
///
/// Note that it would be possible to adapt the above example to using `Regex`
/// with an expression like:
///
/// ```text
/// (?P<email>[a-z]+@(?P<email_domain>[a-z]+[.](com|org|net)))|(?P<domain>[a-z]+[.](com|org|net))
/// ```
///
/// After a match, one could then inspect the capture groups to figure out
/// which alternates matched. The problem is that it is hard to make this
/// approach scale when there are many regexes since the overlap between each
/// alternate isn't always obvious to reason about.
///
/// # Limitations
///
/// Regex sets are limited to answering the following two questions:
///
/// 1. Does any regex in the set match?
/// 2. If so, which regexes in the set match?
///
/// As with the main [`Regex`][crate::Regex] type, it is cheaper to ask (1)
/// instead of (2) since the matching engines can stop after the first match
/// is found.
///
/// You cannot directly extract [`Match`][crate::Match] or
/// [`Captures`][crate::Captures] objects from a regex set. If you need these
/// operations, the recommended approach is to compile each pattern in the set
/// independently and scan the exact same input a second time with those
/// independently compiled patterns:
///
/// ```rust
/// use regex::{Regex, RegexSet};
///
/// let patterns = ["foo", "bar"];
/// // Both patterns will match different ranges of this string.
/// let text = "barfoo";
///
/// // Compile a set matching any of our patterns.
/// let set = RegexSet::new(&patterns).unwrap();
/// // Compile each pattern independently.
/// let regexes: Vec<_> = set.patterns().iter()
///     .map(|pat| Regex::new(pat).unwrap())
///     .collect();
///
/// // Match against the whole set first and identify the individual
/// // matching patterns.
/// let matches: Vec<&str> = set.matches(text).into_iter()
///     // Dereference the match index to get the corresponding
///     // compiled pattern.
///     .map(|match_idx| &regexes[match_idx])
///     // To get match locations or any other info, we then have to search
///     // the exact same text again, using our separately-compiled pattern.
///     .map(|pat| pat.find(text).unwrap().as_str())
///     .collect();
///
/// // Matches arrive in the order the constituent patterns were declared,
/// // not the order they appear in the input.
/// assert_eq!(vec!["foo", "bar"], matches);
/// ```
///
/// # Performance
///
/// A `RegexSet` has the same performance characteristics as `Regex`. Namely,
/// search takes `O(mn)` time, where `m` is proportional to the size of the
/// regex set and `n` is proportional to the length of the search text.
/// 
/// =====================================================
/// 
/// `Exec` manages the execution of a regular expression.
///
/// In particular, this manages the various compiled forms of a single regular
/// expression and the choice of which matching engine to use to execute a
/// regular expression.
#[derive(Debug)]
pub struct Exec<I: Integral> {
    /// All read only state.
    ro: Arc<ExecReadOnly<I>>,
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

/// `ExecReadOnly` comprises all read only state for a regex. Namely, all such
/// state is determined at compile time and never changes during search.
struct ExecReadOnly<I: Integral> {
    /// A compiled program that is used in the NFA simulation and backtracking.
    /// It can be byte-based or Unicode codepoint based.
    ///
    /// N.B. It is not possibly to make this byte-based from the public API.
    /// It is only used for testing byte based programs in the NFA simulations.
    nfa: Program<I>,
    /// A set of suffix literals extracted from the regex.
    ///
    /// Prefix literals are stored on the `Program`, since they are used inside
    /// the matching engines.
    suffixes: LiteralSearcher<I>,
    /// An Aho-Corasick automaton with leftmost-first match semantics.
    ///
    /// This is only set when the entire regex is a simple unanchored
    /// alternation of literals. We could probably use it more circumstances,
    /// but this is already hacky enough in this architecture.
    ///
    /// N.B. We use u32 as a state ID representation under the assumption that
    /// if we were to exhaust the ID space, we probably would have long
    /// surpassed the compilation size limit.
    #[cfg(feature = "perf-literal")]
    ac: Option<AhoCorasick<u32>>,
    /// match_type encodes as much upfront knowledge about how we're going to
    /// execute a search as possible.
    match_type: MatchType,
}

#[unconst]
impl<I: ~const Integral> Exec<I> {
    /// Create a new regex set with the given regular expressions.
    ///
    /// This takes an iterator of `S`, where `S` is something that can produce
    /// a `&str`. If any of the strings in the iterator are not valid regular
    /// expressions, then an error is returned.
    ///
    /// # Example
    ///
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

    /// Returns true if and only if there is a match for the regex in the
    /// string given.
    ///
    /// It is recommended to use this method if all you need to do is test
    /// a match, since the underlying matching engine may be able to do less
    /// work.
    ///
    /// # Example
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
    /// ```
    /// ===================================================
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
    pub const fn is_match(&self, context: &Context<I>) -> bool {
        self.is_match_at(context, 0)
    }

    /// Returns true if and only if the regex matches text.
    ///
    /// For single regular expressions, this is equivalent to calling
    /// shortest_match(...).is_some().
    /// ==============================================================
    /// Returns the same as is_match, but starts the search at the given
    /// offset.
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
        match self.ro.match_type {
            #[cfg(feature = "perf-literal")]
            MatchType::Seq(ty)
                => self.find_literals(ty, context, start).is_some(),
            MatchType::Nfa => self.match_nfa(context, start),
            MatchType::Nothing => false,
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    pub const fn is_anchor_end_match(&self, context: &Context<I>) -> bool {
        // Only do this check if the haystack is big (>1MB).
        if context.len() > (1 << 20) && &self.ro.nfa.is_anchored_end {
            let lcs = &self.ro.suffixes.lcs();
            if lcs.len() >= 1 && !lcs.is_suffix(context) {
                return false;
            }
        }
        true
    }

    /// Finds the leftmost-first match using only literal search.
    #[cfg(feature = "perf-literal")]
    #[cfg_attr(feature = "perf-inline", inline(always))]
    pub const fn find_literals(
        &self,
        ty: MatchSeqType,
        context: &Context<I>,
        start: usize,
    ) -> Option<(usize, usize)> {
        use self::MatchSeqType::*;
        match ty {
            Unanchored => {
                let lits = &self.ro.nfa.prefixes;
                lits.find(&context[start..]).map(|(s, e)| (start + s, start + e))
            }
            AnchoredStart => {
                let lits = &self.ro.nfa.prefixes;
                if start == 0 || !self.ro.nfa.is_anchored_start {
                    lits.find_start(&context[start..])
                        .map(|(s, e)| (start + s, start + e))
                } else {
                    None
                }
            }
            AnchoredEnd => {
                let lits = &self.ro.suffixes;
                lits.find_end(&context[start..])
                    .map(|(s, e)| (start + s, start + e))
            }
            AhoCorasick => self
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
        let bt = if backtrack::should_exec(self.ro.nfa.len(), context.len()) {
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
            &self.ro.nfa,
            self.cache.value(),
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
            &self.ro.nfa,
            self.cache.value(),
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
        match self.ro.match_type {
            MatchType::Seq(ty) => {
                self.find_literals(ty, context, start).map(|(_, e)| e)
            }
            MatchType::Nfa => self.shortest_nfa(context, start),
            MatchType::Nothing => None,
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
        let output = match self.ro.match_type {
            #[cfg(feature = "perf-literal")]
            MatchType::Seq(ty) => self.find_literals(ty, context, start),
            MatchType::Nfa => self.find_nfa(context, start),
            MatchType::Nothing => None,
        };
        output.map(|(s, e)| Match::new(context, s, e))
    }

    /// Like find, but executes an NFA engine.
    fn find_nfa(&self, context: &Context<I>, start: usize)
        -> Option<(usize, usize)>
    {
        None
    }

    /// Returns an iterator for each successive non-overlapping match in
    /// `text`, returning the start and end byte indices with respect to
    /// `text`.
    ///
    /// # Example
    ///
    /// Find the start and end location of every word with exactly 13 Unicode
    /// word characters:
    ///
    /// ```rust
    /// # use regex::Regex;
    /// # fn main() {
    /// let text = "Retroactively relinquishing remunerations is reprehensible.";
    /// for mat in Regex::new(r"\b\w{13}\b").unwrap().find_iter(text) {
    ///     println!("{:?}", mat);
    /// }
    /// # }
    /// ```
    pub const fn find_iter<'c>(&'r self, context: &'c Context<I>) -> Partition<'c, I> {
        Partition(self.searcher().find_iter(context))
    }

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
        let mut matches = vec![false; self.0.regex_strings().len()];
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
        use self::MatchType::*;
        if !self.is_anchor_end_match(context) {
            return false;
        }
        match self.ro.match_type {
            #[cfg(feature = "perf-literal")]
            Seq(ty) => {
                debug_assert_eq!(matches.len(), 1);
                matches[0] = self.find_literals(ty, context, start).is_some();
                matches[0]
            }
            Nfa => self.exec_nfa(
                matches,
                false,
                false,
                context,
                start,
                context.len(),
            ),
            Nothing => false,
        }
    }
}

/// Facilitates the construction of an executor by exposing various knobs
/// to control how a regex is executed and what kinds of resources it's
/// permitted to use.
// `ExecBuilder` is only public via the `internal` module, so avoid deriving
// `Debug`.
#[allow(missing_debug_implementations)]
pub struct ExecBuilder<I: Integral> {
    options: Options<I>,
    match_type: Option<MatchType>,
}

#[unconst]
/// Parsed represents a set of parsed regular expressions and their detected
/// literals.
struct Parsed<I: ~const Integral> {
    reprs: Vec<Repr<I>>,
    prefixes: Literals<I>,
    suffixes: Literals<I>,
}

#[unconst]
impl<I: ~const Integral> ExecBuilder<I> {
    /// Create a regex execution builder.
    ///
    /// This uses default settings for everything except the regex itself,
    /// which must be provided. Further knobs can be set by calling methods,
    /// and then finally, `build` to actually create the executor.
    /// ==============
    /// Like new, but compiles the union of the given regular expressions.
    ///
    /// Note that when compiling 2 or more regular expressions, capture groups
    /// are completely unsupported. (This means both `find` and `captures`
    /// won't work.)
    pub const fn new(options: Options<I>) -> Self {
        ExecBuilder {
            options,
            match_type: None,
        }
    }

    /// Parse the current set of patterns into their AST and extract literals.
    fn parse(&self) -> Parsed<I> {
        let mut prefixes = Some(Literals::empty());
        let mut suffixes = Some(Literals::empty());
        let is_set = true;
        // If we're compiling a regex set and that set has any anchored
        // expressions, then disable all literal optimizations.
        let mut reprs = Vec::new();
        let mut current = &self.options.repr;
        while let Repr::Add(lhs, rhs) = current {
            if !repr.is_anchored_start() && repr.is_any_anchored_start() {
                // Partial anchors unfortunately make it hard to use
                // prefixes, so disable them.
                prefixes = None;
            } else if is_set && repr.is_anchored_start() {
                // Regex sets with anchors do not go well with literal
                // optimizations.
                prefixes = None;
            }
            prefixes = prefixes.and_then(|mut prefixes| {
                if !prefixes.union_prefixes(&repr) {
                    None
                } else {
                    Some(prefixes)
                }
            });

            if !repr.is_anchored_end() && repr.is_any_anchored_end() {
                // Partial anchors unfortunately make it hard to use
                // suffixes, so disable them.
                suffixes = None;
            } else if is_set && repr.is_anchored_end() {
                // Regex sets with anchors do not go well with literal
                // optimizations.
                suffixes = None;
            }
            suffixes = suffixes.and_then(|mut suffixes| {
                if !suffixes.union_suffixes(&repr) {
                    None
                } else {
                    Some(suffixes)
                }
            });
            exprs.push(repr);
        }
        Parsed {
            reprs,
            prefixes: prefixes.unwrap_or_else(Literals::empty),
            suffixes: suffixes.unwrap_or_else(Literals::empty),
        }
    }

    /// Build an executor that can run a regular expression.
    pub fn build(self) -> Exec<I> {
        let parsed = self.parse();
        let mut nfa = Compiler::new()
            .size_limit(self.options.size_limit)
            .compile(&parsed.reprs);

        #[cfg(feature = "perf-literal")]
        let ac = self.build_aho_corasick(&parsed);
        nfa.prefixes = LiteralSearcher::prefixes(parsed.prefixes);

        let mut ro = ExecReadOnly {
            nfa,
            suffixes: LiteralSearcher::suffixes(parsed.suffixes),
            #[cfg(feature = "perf-literal")]
            ac,
            match_type: MatchType::Nothing,
        };
        ro.match_type = ro.choose_match_type(self.match_type);

        let ro = Arc::new(ro);
        let pool = ExecReadOnly::new_pool(&ro);
        Exec { ro, pool }
    }

    #[cfg(feature = "perf-literal")]
    fn build_aho_corasick(&self, parsed: &Parsed<I>) -> Option<AhoCorasick<u32>> {
        if parsed.reprs.len() != 1 {
            return None;
        }
        let lits = match or_constants(&parsed.reprs[0]) {
            None => return None,
            Some(lits) => lits,
        };
        // If we have a small number of literals, then let Teddy handle
        // things (see literal/mod.rs).
        if lits.len() <= 32 {
            return None;
        }
        Some(
            AhoCorasickBuilder::new()
                .match_kind(MatchKind::LeftmostFirst)
                .auto_configure(&lits)
                .build_with_size::<u32, _, _>(&lits)
                // This should never happen because we'd long exceed the
                // compilation limit for regexes first.
                .expect("AC automaton too big"),
        )
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

impl<I: Integral> Clone for Exec<I> {
    fn clone(&self) -> Exec<I> {
        let pool = ExecReadOnly::new_pool(&self.ro);
        Exec { ro: self.ro.clone(), pool }
    }
}

impl<I: Integral> ExecReadOnly<I> {
    fn choose_match_type(&self, hint: Option<MatchType>) -> MatchType {
        if let Some(MatchType::Nfa) = hint {
            return hint.unwrap();
        }
        // If the NFA is empty, then we'll never match anything.
        if self.nfa.insts.is_empty() {
            return MatchType::Nothing;
        }
        if let Some(literalty) = self.choose_literal_match_type() {
            return literalty;
        }
        // We're so totally hosed.
        MatchType::Nfa
    }

    /// If a plain literal scan can be used, then a corresponding literal
    /// search type is returned.
    fn choose_literal_match_type(&self) -> Option<MatchType> {
        fn imp<I: Integral>(ro: &ExecReadOnly<I>) -> Option<MatchType> {
            // If our set of prefixes is complete, then we can use it to find
            // a match in lieu of a regex engine. This doesn't quite work well
            // in the presence of multiple regexes, so only do it when there's
            // one.
            //
            // TODO(burntsushi): Also, don't try to match literals if the regex
            // is partially anchored. We could technically do it, but we'd need
            // to create two sets of literals: all of them and then the subset
            // that aren't anchored. We would then only search for all of them
            // when at the beginning of the input and use the subset in all
            // other cases.
            if ro.res.len() != 1 {
                return None;
            }
            if ro.ac.is_some() {
                return Some(MatchType::Seq(
                    MatchSeqType::AhoCorasick,
                ));
            }
            if ro.nfa.prefixes.complete() {
                return if ro.nfa.is_anchored_start {
                    Some(MatchType::Seq(MatchSeqType::AnchoredStart))
                } else {
                    Some(MatchType::Seq(MatchSeqType::Unanchored))
                };
            }
            if ro.suffixes.complete() {
                return if ro.nfa.is_anchored_end {
                    Some(MatchType::Seq(MatchSeqType::AnchoredEnd))
                } else {
                    // This case shouldn't happen. When the regex isn't
                    // anchored, then complete prefixes should imply complete
                    // suffixes.
                    Some(MatchType::Seq(MatchSeqType::Unanchored))
                };
            }
            None
        }

        imp(self)
    }

    fn new_pool(ro: &Arc<ExecReadOnly<I>>) -> Box<Pool<ProgramCache<I>>> {
        let ro = ro.clone();
        Box::new(Pool::new(Box::new(move || {
            AssertUnwindSafe(RefCell::new(ProgramCacheInner::new(&ro)))
        })))
    }
}

#[derive(Clone, Copy, Debug)]
enum MatchType {
    /// A single or multiple literal search. This is only used when the regex
    /// can be decomposed into a literal search.
    #[cfg(feature = "perf-literal")]
    Seq(MatchSeqType),
    /// An NFA variant.
    Nfa,
    /// No match is ever possible, so don't ever try to search.
    Nothing,
}

#[derive(Clone, Copy, Debug)]
#[cfg(feature = "perf-literal")]
enum MatchSeqType {
    /// Match literals anywhere in text.
    Unanchored,
    /// Match literals only at the start of text.
    AnchoredStart,
    /// Match literals only at the end of text.
    AnchoredEnd,
    /// Use an Aho-Corasick automaton. This requires `ac` to be Some on
    /// ExecReadOnly.
    AhoCorasick,
}

/// `ProgramCache` maintains reusable allocations for each matching engine
/// available to a particular program.
///
/// We declare this as unwind safe since it's a cache that's only used for
/// performance purposes. If a panic occurs, it is (or should be) always safe
/// to continue using the same regex object.
pub type ProgramCache<I: Integral>
    = AssertUnwindSafe<RefCell<ProgramCacheInner<I>>>;

#[derive(Debug)]
pub struct ProgramCacheInner<I: Integral> {
    // pub pikevm: pikevm::Cache,
    pub backtrack: backtrack::Cache<I>,
}

impl<I: Integral> ProgramCacheInner<I> {
    fn new(ro: &ExecReadOnly<I>) -> Self {
        ProgramCacheInner {
            // pikevm: pikevm::Cache::new(&ro.nfa),
            backtrack: backtrack::Cache::new(&ro.nfa),
        }
    }
}

/// Alternation literals checks if the given HIR is a simple alternation of
/// literals, and if so, returns them. Otherwise, this returns None.
#[cfg(feature = "perf-literal")]
fn or_constants<I: Integral>(repr: &Repr<I>) -> Option<Vec<Vec<u8>>> {
    // This is pretty hacky, but basically, if `is_alternation_literal` is
    // true, then we can make several assumptions about the structure of our
    // HIR. This is what justifies the `unreachable!` statements below.
    //
    // This code should be refactored once we overhaul this crate's
    // optimization pipeline, because this is a terribly inflexible way to go
    // about things.

    if !repr.is_alternation_literal() {
        return None;
    }
    let mut constants = Vec::new();
    let mut current = repr;
    // One literal isn't worth it.
    while let Repr::Or(lhs, rhs) = current {
        let mut constant = Seq::empty();
        match *lhs {
            Repr::One(ref seq) => constant = constant.mul(seq),
            Repr::And(ref exprs) => {
                for e in exprs {
                    match *e {
                        Repr::One(ref x) => constant = constant.mul(x),
                        _ => unreachable!("expected literal, got {:?}", e),
                    }
                }
            }
            _ => unreachable!("expected literal or concat, got {:?}", lhs),
        }
        constants.push(constant);
    }

    Some(constants)
}
