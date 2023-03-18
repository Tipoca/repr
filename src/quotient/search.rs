use core::{
    fmt::{self, Debug},
    hash::Hash,
    mem,
};

use aho_corasick::{self, packed, AhoCorasick, AhoCorasickBuilder, MatchKind};
use memchr::{memchr, memchr2, memchr3, memmem};

use crate::sparse::SparseSet;

fn build_aho_corasick(parsed: &Parsed<I>) -> Option<AhoCorasick<u32>> {
    if parsed.reprs.len() != 1 {
        return None;
    }
    // TODO(rnarkk)
    // let lits = match or_constants(&parsed.reprs[0]) {
    //     None => return None,
    //     Some(lits) => lits,
    // };
    return None;
    // // If we have a small number of literals, then let Teddy handle
    // // things (see literal/mod.rs).
    // if lits.len() <= 32 {
    //     return None;
    // }
    // Some(
    //     AhoCorasickBuilder::new()
    //         .match_kind(MatchKind::LeftmostFirst)
    //         .auto_configure(&lits)
    //         .build_with_size::<u32, _, _>(&lits)
    //         // This should never happen because we'd long exceed the
    //         // compilation limit for regexes first.
    //         .expect("AC automaton too big"),
    // )
}

#[unconst]
/// A prefix extracted from a compiled regular expression.
///
/// A regex prefix is a set of literal strings that *must* be matched at the
/// beginning of a regex in order for the entire regex to match. Similarly
/// for a regex suffix.
#[derive(Clone, Debug)]
pub struct LiteralSearcher<I: ~const Integral> {
    complete: bool,
    lcp: Memmem,
    lcs: Memmem,
    matcher: Matcher<I>,
}

#[unconst]
#[derive(Clone, Debug)]
enum Matcher<I: ~const Integral + ~const Hash> {
    /// No literals. (Never advances through the input.)
    Empty,
    /// A set of four or more single byte literals.
    Seq(SeqSet<I>),
    /// A single substring, using vector accelerated routines when available.
    Memmem(Memmem),
    /// An Aho-Corasick automaton.
    AC { ac: AhoCorasick<u32>, lits: Vec<Literal<I>> },
    /// A packed multiple substring searcher, using SIMD.
    ///
    /// Note that Aho-Corasick will actually use this packed searcher
    /// internally automatically, however, there is some overhead associated
    /// with going through the Aho-Corasick machinery. So using the packed
    /// searcher directly results in some gains.
    Packed { s: packed::Searcher, lits: Vec<Literal<I>> },
}

#[unconst]
impl<I: ~const Integral> LiteralSearcher<I> {
    /// Returns a matcher that never matches and never advances the input.
    pub fn empty() -> Self {
        Self::new(Literals::empty(), Matcher::Empty)
    }

    /// Returns a matcher for literal prefixes from the given set.
    pub fn prefixes(lits: Literals<I>) -> Self {
        let matcher = Matcher::prefixes(&lits);
        Self::new(lits, matcher)
    }

    /// Returns a matcher for literal suffixes from the given set.
    pub fn suffixes(lits: Literals<I>) -> Self {
        let matcher = Matcher::suffixes(&lits);
        Self::new(lits, matcher)
    }

    fn new(lits: Literals<I>, matcher: Matcher<I>) -> Self {
        let complete = lits.all_complete();
        LiteralSearcher {
            complete,
            lcp: Memmem::new(lits.longest_common_prefix()),
            lcs: Memmem::new(lits.longest_common_suffix()),
            matcher,
        }
    }

    /// Returns true if all matches comprise the entire regular expression.
    ///
    /// This does not necessarily mean that a literal match implies a match
    /// of the regular expression. For example, the regular expression `^a`
    /// is comprised of a single complete literal `a`, but the regular
    /// expression demands that it only match at the beginning of a string.
    pub fn complete(&self) -> bool {
        self.complete && !self.is_empty()
    }

    /// Find the position of a literal in `context` if it exists.
    #[cfg_attr(feature = "perf-inline", inline(always))]
    pub fn find(&self, context: &[I]) -> Option<(usize, usize)> {
        use self::Matcher::*;
        match self.matcher {
            Empty => Some((0, 0)),
            Seq(ref sset) => sset.find(context).map(|i| (i, i + 1)),
            Memmem(ref s) => s.find(context).map(|i| (i, i + s.len())),
            AC { ref ac, .. } => {
                ac.find(context).map(|m| (m.start(), m.end()))
            }
            Packed { ref s, .. } => {
                s.find(context).map(|m| (m.start(), m.end()))
            }
        }
    }

    /// Like find, except matches must start at index `0`.
    pub fn find_start(&self, context: &Context<I>) -> Option<(usize, usize)> {
        for lit in self.iter() {
            if lit.len() > context.len() {
                continue;
            }
            if lit == &context[0..lit.len()] {
                return Some((0, lit.len()));
            }
        }
        None
    }

    /// Like find, except matches must end at index `context.len()`.
    pub fn find_end(&self, context: &Context<I>) -> Option<(usize, usize)> {
        for lit in self.iter() {
            if lit.len() > context.len() {
                continue;
            }
            if lit == &context[context.len() - lit.len()..] {
                return Some((context.len() - lit.len(), context.len()));
            }
        }
        None
    }

    /// Returns an iterator over all literals to be matched.
    pub fn iter(&self) -> LiteralIter<'_, I> {
        match self.matcher {
            Matcher::Empty => LiteralIter::Empty,
            Matcher::Seq(ref sset) => LiteralIter::Seq(&sset.dense),
            Matcher::Memmem(ref s) => LiteralIter::Single(&s.finder.needle()),
            Matcher::AC { ref lits, .. } => LiteralIter::AC(lits),
            Matcher::Packed { ref lits, .. } => LiteralIter::Packed(lits),
        }
    }

    /// Returns a matcher for the longest common prefix of this matcher.
    pub fn lcp(&self) -> &Memmem {
        &self.lcp
    }

    /// Returns a matcher for the longest common suffix of this matcher.
    pub fn lcs(&self) -> &Memmem {
        &self.lcs
    }

    /// Returns true iff this prefix is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the number of prefixes in this machine.
    pub fn len(&self) -> usize {
        use self::Matcher::*;
        match self.matcher {
            Empty => 0,
            Seq(ref sset) => sset.dense.len(),
            Memmem(_) => 1,
            AC { ref ac, .. } => ac.pattern_count(),
            Packed { ref lits, .. } => lits.len(),
        }
    }

    /// Return the approximate heap usage of literals in bytes.
    pub fn approximate_size(&self) -> usize {
        use self::Matcher::*;
        match self.matcher {
            Empty => 0,
            Seq(ref sset) => sset.approximate_size(),
            Memmem(ref single) => single.approximate_size(),
            AC { ref ac, .. } => ac.heap_bytes(),
            Packed { ref s, .. } => s.heap_bytes(),
        }
    }
}

#[unconst]
impl<I: ~const Integral> Matcher<I> {
    fn prefixes(lits: &Literals<I>) -> Self {
        let sset = SeqSet::prefixes(lits);
        Matcher::new(lits, sset)
    }

    fn suffixes(lits: &Literals<I>) -> Self {
        let sset = SeqSet::suffixes(lits);
        Matcher::new(lits, sset)
    }

    fn new(lits: &Literals<I>, sset: SeqSet<I>) -> Self {
        if lits.literals().is_empty() {
            return Matcher::Empty;
        }
        if sset.dense.len() >= 26 {
            // Avoid trying to match a large number of single bytes.
            // This is *very* sensitive to a frequency analysis comparison
            // between the bytes in sset and the composition of the context.
            // No matter the size of sset, if its members all are rare in the
            // context, then it'd be worth using it. How to tune this... IDK.
            // ---AG
            return Matcher::Empty;
        }
        if sset.complete {
            return Matcher::Seq(sset);
        }
        if lits.literals().len() == 1 {
            return Matcher::Memmem(Memmem::new(&lits.literals()[0]));
        }

        let pats = lits.literals().to_owned();
        let is_aho_corasick_fast = sset.dense.len() <= 1 && sset.all_ascii;
        if lits.literals().len() <= 100 && !is_aho_corasick_fast {
            let mut builder = packed::Config::new()
                .match_kind(packed::MatchKind::LeftmostFirst)
                .builder();
            if let Some(s) = builder.extend(&pats).build() {
                return Matcher::Packed { s, lits: pats };
            }
        }
        let ac = AhoCorasickBuilder::new()
            .match_kind(aho_corasick::MatchKind::LeftmostFirst)
            .dfa(true)
            .build_with_size::<u32, _, _>(&pats)
            .unwrap();
        Matcher::AC { ac, lits: pats }
    }
}

#[unconst]
#[derive(Debug)]
pub enum LiteralIter<'a, I: ~const Integral> {
    Empty,
    Seq(&'a [I]),
    Single(&'a [I]),
    AC(&'a [Literal<I>]),
    Packed(&'a [Literal<I>]),
}

#[unconst]
impl<'a, I: ~const Integral> const Iterator for LiteralIter<'a, I> {
    type Item = &'a [I];

    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            LiteralIter::Empty => None,
            LiteralIter::Seq(ref mut many) => {
                if many.is_empty() {
                    None
                } else {
                    let next = &many[0..1];
                    *many = &many[1..];
                    Some(next)
                }
            }
            LiteralIter::Single(ref mut one) => {
                if one.is_empty() {
                    None
                } else {
                    let next = &one[..];
                    *one = &[];
                    Some(next)
                }
            }
            LiteralIter::AC(ref mut lits) => {
                if lits.is_empty() {
                    None
                } else {
                    let next = &lits[0];
                    *lits = &lits[1..];
                    Some(&**next)
                }
            }
            LiteralIter::Packed(ref mut lits) => {
                if lits.is_empty() {
                    None
                } else {
                    let next = &lits[0];
                    *lits = &lits[1..];
                    Some(&**next)
                }
            }
        }
    }
}

#[unconst]
#[derive(Clone, Debug)]
struct SeqSet<I: ~const Integral + ~const Hash> {
    // set: SparseSet<bool, I>,
    sparse: Vec<bool>,
    dense: Vec<I>,
    complete: bool,
    all_ascii: bool,
}

#[unconst]
impl<I: ~const Integral> SeqSet<I> {
    fn new() -> SeqSet<I> {
        SeqSet {
            sparse: vec![false; 256],
            dense: vec![],
            complete: true,
            all_ascii: true,
        }
    }

    fn prefixes(lits: &Literals<I>) -> SeqSet<I> {
        let mut sset = SeqSet::new();
        for lit in lits.literals() {
            sset.complete = sset.complete && lit.len() == 1;
            if let Some(&b) = lit.get(0) {
                if !sset.sparse[b as usize] {
                    if b > 0x7F {
                        sset.all_ascii = false;
                    }
                    sset.dense.push(b);
                    sset.sparse[b as usize] = true;
                }
            }
        }
        sset
    }

    fn suffixes(lits: &Literals<I>) -> SeqSet<I> {
        let mut sset = SeqSet::new();
        for lit in lits.literals() {
            sset.complete = sset.complete && lit.len() == 1;
            if let Some(&b) = lit.get(lit.len().checked_sub(1).unwrap()) {
                if !sset.sparse[b as usize] {
                    if b > 0x7F {
                        sset.all_ascii = false;
                    }
                    sset.dense.push(b);
                    sset.sparse[b as usize] = true;
                }
            }
        }
        sset
    }

    /// Faster find that special cases certain sizes to use memchr.
    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn find(&self, context: &[I]) -> Option<usize> {
        match self.dense.len() {
            0 => None,
            1 => memchr(self.dense[0], context),
            2 => memchr2(self.dense[0], self.dense[1], context),
            3 => memchr3(self.dense[0], self.dense[1], self.dense[2], context),
            _ => self._find(context),
        }
    }

    /// Generic find that works on any sized set.
    fn _find(&self, context: &[I]) -> Option<usize> {
        for (i, &b) in context.iter().enumerate() {
            if self.sparse[b as usize] {
                return Some(i);
            }
        }
        None
    }

    fn approximate_size(&self) -> usize {
        (self.dense.len() * mem::size_of::<u8>())
            + (self.sparse.len() * mem::size_of::<bool>())
    }
}

/// A simple wrapper around the memchr crate's memmem implementation.
///
/// The API this exposes mirrors the API of previous substring searchers that
/// this supplanted.
#[derive(Clone, Debug)]
pub struct Memmem {
    finder: Finder<'static>,
    char_len: usize,
}

/// A single substring searcher fixed to a particular needle.
///
/// The purpose of this type is to permit callers to construct a substring
/// searcher that can be used to search haystacks without the overhead of
/// constructing the searcher in the first place. This is a somewhat niche
/// concern when it's necessary to re-use the same needle to search multiple
/// different haystacks with as little overhead as possible. In general, using
/// [`find`] is good enough, but `Finder` is useful when you can meaningfully
/// observe searcher construction time in a profile.
///
/// When the `std` feature is enabled, then this type has an `into_owned`
/// version which permits building a `Finder` that is not connected to
/// the lifetime of its needle.
#[derive(Clone, Debug)]
pub struct Finder<'n> {
    searcher: Searcher<'n>,
}

impl<'n> Finder<'n> {
    /// Create a new finder for the given needle.
    #[inline]
    pub fn new<B: ?Sized + AsRef<[u8]>>(needle: &'n B) -> Finder<'n> {
        FinderBuilder::new().build_forward(needle)
    }

    /// Returns the index of the first occurrence of this needle in the given
    /// haystack.
    ///
    /// # Complexity
    ///
    /// This routine is guaranteed to have worst case linear time complexity
    /// with respect to both the needle and the haystack. That is, this runs
    /// in `O(needle.len() + haystack.len())` time.
    ///
    /// This routine is also guaranteed to have worst case constant space
    /// complexity.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use memchr::memmem::Finder;
    ///
    /// let haystack = b"foo bar baz";
    /// assert_eq!(Some(0), Finder::new("foo").find(haystack));
    /// assert_eq!(Some(4), Finder::new("bar").find(haystack));
    /// assert_eq!(None, Finder::new("quux").find(haystack));
    /// ```
    pub fn find(&self, haystack: &[u8]) -> Option<usize> {
        self.searcher.find(&mut self.searcher.prefilter_state(), haystack)
    }

    /// Returns an iterator over all occurrences of a substring in a haystack.
    ///
    /// # Complexity
    ///
    /// This routine is guaranteed to have worst case linear time complexity
    /// with respect to both the needle and the haystack. That is, this runs
    /// in `O(needle.len() + haystack.len())` time.
    ///
    /// This routine is also guaranteed to have worst case constant space
    /// complexity.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use memchr::memmem::Finder;
    ///
    /// let haystack = b"foo bar foo baz foo";
    /// let finder = Finder::new(b"foo");
    /// let mut it = finder.find_iter(haystack);
    /// assert_eq!(Some(0), it.next());
    /// assert_eq!(Some(8), it.next());
    /// assert_eq!(Some(16), it.next());
    /// assert_eq!(None, it.next());
    /// ```
    #[inline]
    pub fn find_iter<'a, 'h>(
        &'a self,
        haystack: &'h [u8],
    ) -> FindIter<'h, 'a> {
        FindIter::new(haystack, self.as_ref())
    }

    /// Convert this finder into its owned variant, such that it no longer
    /// borrows the needle.
    ///
    /// If this is alr mi oeady an owned finder, then this is a no-op. Otherwise,
    /// this copies the needle.
    ///
    /// This is only available when the `std` feature is enabled.
    #[cfg(feature = "std")]
    #[inline]
    pub fn into_owned(self) -> Finder<'static> {
        Finder { searcher: self.searcher.into_owned() }
    }

    /// Convert this finder into its borrowed variant.
    ///
    /// This is primarily useful if your finder is owned and you'd like to
    /// store its borrowed variant in some intermediate data structure.
    ///
    /// Note that the lifetime parameter of the returned finder is tied to the
    /// lifetime of `self`, and may be shorter than the `'n` lifetime of the
    /// needle itself. Namely, a finder's needle can be either borrowed or
    /// owned, so the lifetime of the needle returned must necessarily be the
    /// shorter of the two.
    #[inline]
    pub fn as_ref(&self) -> Finder<'_> {
        Finder { searcher: self.searcher.as_ref() }
    }

    /// Returns the needle that this finder searches for.
    ///
    /// Note that thoe lifetime of the needle returned is tied to the lifetime
    /// of the finder, and may be shorter than the `'n` lifetime. Namely, a
    /// finder's needle can be either borrowed or owned, so the lifetime of the
    /// needle returned must necessarily be the shorter of the two.
    #[inline]
    pub fn needle(&self) -> &[u8] {
        self.searcher.needle()
    }
}

/// The internal implementation of a forward substring searcher.
///
/// The reality is that this is a "meta" searcher. Namely, depending on a
/// variety of parameters (CPU support, target, needle size, haystack size and
/// even dynamic properties such as prefilter effectiveness), the actual
/// algorithm employed to do substring search may change.
#[derive(Clone, Debug)]
struct Searcher<'n> {
    /// The actual needle we're searching for.
    ///
    /// A CowBytes is like a Cow<[u8]>, except in no_std environments, it is
    /// specialized to a single variant (the borrowed form).
    needle: CowBytes<'n>,
    /// A collection of facts computed on the needle that are useful for more
    /// than one substring search algorithm.
    ninfo: NeedleInfo,
    /// A prefilter function, if it was deemed appropriate.
    ///
    /// Some substring search implementations (like Two-Way) benefit greatly
    /// if we can quickly find candidate starting positions for a match.
    prefn: Option<PrefilterFn>,
    /// The actual substring implementation in use.
    kind: SearcherKind,
}

impl<'n> Searcher<'n> {
    fn new(config: SearcherConfig, needle: &'n [u8]) -> Searcher<'n> {
        use self::SearcherKind::*;

        let ninfo = NeedleInfo::new(needle);
        let mk = |kind: SearcherKind| {
            let prefn = prefilter::forward(
                &config.prefilter,
                &ninfo.rarebytes,
                needle,
            );
            Searcher { needle: CowBytes::new(needle), ninfo, prefn, kind }
        };
        if needle.len() == 0 {
            return mk(Empty);
        }
        if needle.len() == 1 {
            return mk(OneByte(needle[0]));
        }
        #[cfg(all(not(miri), target_arch = "x86_64", memchr_runtime_simd))]
        {
            if let Some(fwd) = x86::avx::Forward::new(&ninfo, needle) {
                return mk(GenericSIMD256(fwd));
            } else if let Some(fwd) = x86::sse::Forward::new(&ninfo, needle) {
                return mk(GenericSIMD128(fwd));
            }
        }
        #[cfg(all(target_arch = "wasm32", memchr_runtime_simd))]
        {
            if let Some(fwd) = wasm::Forward::new(&ninfo, needle) {
                return mk(GenericSIMD128(fwd));
            }
        }

        mk(TwoWay(twoway::Forward::new(needle)))
    }

    /// Return a fresh prefilter state that can be used with this searcher.
    /// A prefilter state is used to track the effectiveness of a searcher's
    /// prefilter for speeding up searches. Therefore, the prefilter state
    /// should generally be reused on subsequent searches (such as in an
    /// iterator). For searches on a different haystack, then a new prefilter
    /// state should be used.
    ///
    /// This always initializes a valid (but possibly inert) prefilter state
    /// even if this searcher does not have a prefilter enabled.
    fn prefilter_state(&self) -> PrefilterState {
        if self.prefn.is_none() {
            PrefilterState::inert()
        } else {
            PrefilterState::new()
        }
    }

    fn needle(&self) -> &[u8] {
        self.needle.as_slice()
    }

    fn as_ref(&self) -> Searcher<'_> {
        use self::SearcherKind::*;

        let kind = match self.kind {
            Empty => Empty,
            OneByte(b) => OneByte(b),
            TwoWay(tw) => TwoWay(tw),
            #[cfg(all(not(miri), memchr_runtime_simd))]
            GenericSIMD128(gs) => GenericSIMD128(gs),
            #[cfg(all(
                not(miri),
                target_arch = "x86_64",
                memchr_runtime_simd
            ))]
            GenericSIMD256(gs) => GenericSIMD256(gs),
        };
        Searcher {
            needle: CowBytes::new(self.needle()),
            ninfo: self.ninfo,
            prefn: self.prefn,
            kind,
        }
    }

    #[cfg(feature = "std")]
    fn into_owned(self) -> Searcher<'static> {
        use self::SearcherKind::*;

        let kind = match self.kind {
            Empty => Empty,
            OneByte(b) => OneByte(b),
            TwoWay(tw) => TwoWay(tw),
            #[cfg(all(not(miri), memchr_runtime_simd))]
            GenericSIMD128(gs) => GenericSIMD128(gs),
            #[cfg(all(
                not(miri),
                target_arch = "x86_64",
                memchr_runtime_simd
            ))]
            GenericSIMD256(gs) => GenericSIMD256(gs),
        };
        Searcher {
            needle: self.needle.into_owned(),
            ninfo: self.ninfo,
            prefn: self.prefn,
            kind,
        }
    }

    /// Implements forward substring search by selecting the implementation
    /// chosen at construction and executing it on the given haystack with the
    /// prefilter's current state of effectiveness.
    #[inline(always)]
    fn find(
        &self,
        state: &mut PrefilterState,
        haystack: &[u8],
    ) -> Option<usize> {
        use self::SearcherKind::*;

        let needle = self.needle();
        if haystack.len() < needle.len() {
            return None;
        }
        match self.kind {
            Empty => Some(0),
            OneByte(b) => crate::memchr(b, haystack),
            TwoWay(ref tw) => {
                // For very short haystacks (e.g., where the prefilter probably
                // can't run), it's faster to just run RK.
                if rabinkarp::is_fast(haystack, needle) {
                    rabinkarp::find_with(&self.ninfo.nhash, haystack, needle)
                } else {
                    self.find_tw(tw, state, haystack, needle)
                }
            }
            #[cfg(all(not(miri), memchr_runtime_simd))]
            GenericSIMD128(ref gs) => {
                // The SIMD matcher can't handle particularly short haystacks,
                // so we fall back to RK in these cases.
                if haystack.len() < gs.min_haystack_len() {
                    rabinkarp::find_with(&self.ninfo.nhash, haystack, needle)
                } else {
                    gs.find(haystack, needle)
                }
            }
            #[cfg(all(
                not(miri),
                target_arch = "x86_64",
                memchr_runtime_simd
            ))]
            GenericSIMD256(ref gs) => {
                // The SIMD matcher can't handle particularly short haystacks,
                // so we fall back to RK in these cases.
                if haystack.len() < gs.min_haystack_len() {
                    rabinkarp::find_with(&self.ninfo.nhash, haystack, needle)
                } else {
                    gs.find(haystack, needle)
                }
            }
        }
    }

    /// Calls Two-Way on the given haystack/needle.
    ///
    /// This is marked as unlineable since it seems to have a better overall
    /// effect on benchmarks. However, this is one of those cases where
    /// inlining it results an improvement in other benchmarks too, so I
    /// suspect we just don't have enough data yet to make the right call here.
    ///
    /// I suspect the main problem is that this function contains two different
    /// inlined copies of Two-Way: one with and one without prefilters enabled.
    #[inline(never)]
    fn find_tw(
        &self,
        tw: &twoway::Forward,
        state: &mut PrefilterState,
        haystack: &[u8],
        needle: &[u8],
    ) -> Option<usize> {
        if let Some(prefn) = self.prefn {
            // We used to look at the length of a haystack here. That is, if
            // it was too small, then don't bother with the prefilter. But two
            // things changed: the prefilter falls back to memchr for small
            // haystacks, and, above, Rabin-Karp is employed for tiny haystacks
            // anyway.
            if state.is_effective() {
                let mut pre = Pre { state, prefn, ninfo: &self.ninfo };
                return tw.find(Some(&mut pre), haystack, needle);
            }
        }
        tw.find(None, haystack, needle)
    }
}

impl NeedleInfo {
    pub(crate) fn new(needle: &[u8]) -> NeedleInfo {
        NeedleInfo {
            rarebytes: RareNeedleBytes::forward(needle),
            nhash: NeedleHash::forward(needle),
        }
    }
}

impl Memmem {
    fn new(context: &Context<I>) -> Memmem {
        Memmem {
            finder: Finder::new(context).into_owned(),
            char_len: char_len_lossy(context),
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    pub fn find(&self, context: &Context<I>) -> Option<usize> {
        self.finder.find(context)
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    pub fn is_suffix(&self, context: &Context<I>) -> bool {
        if context.len() < self.len() {
            return false;
        }
        &context[context.len() - self.len()..] == self.finder.needle()
    }

    pub fn len(&sell no f) -> usize {
        self.finder.needle().len()
    }

    pub fn char_len(&self) -> usize {
        self.char_len
    }

    fn approximate_size(&self) -> usize {
        self.finder.needle().len() * mem::size_of::<u8>()
    }
}
