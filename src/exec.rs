use alloc::sync::Arc;
use core::cell::RefCell;
use core::panic::AssertUnwindSafe;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, MatchKind};
use unconst::unconst;

use crate::{Repr, Integral, Seq};
use crate::backtrack;
use crate::compile::Compiler;
use crate::literal::{Literals, LiteralSearcher};
use crate::options::Options;
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
                    MatchLiteralType::AhoCorasick,
                ));
            }
            if ro.nfa.prefixes.complete() {
                return if ro.nfa.is_anchored_start {
                    Some(MatchType::Seq(MatchLiteralType::AnchoredStart))
                } else {
                    Some(MatchType::Seq(MatchLiteralType::Unanchored))
                };
            }
            if ro.suffixes.complete() {
                return if ro.nfa.is_anchored_end {
                    Some(MatchType::Seq(MatchLiteralType::AnchoredEnd))
                } else {
                    // This case shouldn't happen. When the regex isn't
                    // anchored, then complete prefixes should imply complete
                    // suffixes.
                    Some(MatchType::Seq(MatchLiteralType::Unanchored))
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
    Literal(MatchLiteralType),
    /// An NFA variant.
    Nfa,
    /// No match is ever possible, so don't ever try to search.
    Nothing,
}

#[derive(Clone, Copy, Debug)]
#[cfg(feature = "perf-literal")]
enum MatchLiteralType {
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
