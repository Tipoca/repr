//! Extract literal prefixes and suffixes from an `Repr<I>`.
//! <https://en.wikipedia.org/wiki/Brzozowski_derivative>
        
#[cfg(feature = "derivative")]
mod search;

use core::{
    cmp,
    fmt::{self, Debug},
    mem,
    ops::{Deref, DerefMut}
};

use unconst::unconst;

use crate::interval::Interval;
use crate::repr::{Repr, Zero};
use crate::seq::Seq;
use crate::traits::Integral;

#[cfg(feature = "derivative")]
pub use search::LiteralSearcher;

/// A set of literal byte strings extracted from a regular expression.
///
/// Every member of the set is a `Literal`, which is represented by a
/// `Vec<u8>`. (Notably, it may contain invalid UTF-8.) Every member is
/// said to be either *complete* or *cut*. A complete literal means that
/// it extends until the beginning (or end) of the regular expression. In
/// some circumstances, this can be used to indicate a match in the regular
/// expression.
///
/// A key aspect of literal extraction is knowing when to stop. It is not
/// feasible to blindly extract all literals from a regular expression, even if
/// there are finitely many. For example, the regular expression `[0-9]{10}`
/// has `10^10` distinct literals. For this reason, literal extraction is
/// bounded to some low number by default using heuristics, but the limits can
/// be tweaked.
///
/// **WARNING**: Literal extraction uses stack space proportional to the size
/// of the `Repr<I>` expression. At some point, this drawback will be eliminated.
/// To protect yourself, set a reasonable
/// [`nest_limit` on your `Parser`](../../struct.ParserBuilder.html#method.nest_limit).
/// This is done for you by default.
#[unconst]
#[derive(Clone, Eq, PartialEq)]
pub struct Literals<I: ~const Integral> {
    lits: Vec<Literal<I>>,
    /// Approximate size limit (in bytes) of this set.
    ///
    /// If extracting a literal would put the set over this limit, then
    /// extraction stops.
    ///
    /// The new limits will only apply to additions to this set. Existing
    /// members remain unchanged, even if the set exceeds the new limit.
    pub limit_size: usize,
    /// Get the character class size limit for this set.
    /// Limits the size of character(or byte) classes considered.
    ///
    /// A value of `0` prevents all character classes from being considered.
    ///
    /// This limit also applies to case insensitive literals, since each
    /// character in the case insensitive literal is converted to a class, and
    /// then case folded.
    ///
    /// The new limits will only apply to additions to this set. Existing
    /// members remain unchanged, even if the set exceeds the new limit.
    pub limit_class: usize,
}

#[unconst]
/// A single member of a set of literals extracted from a regular expression.
///
/// This type has `Deref` and `DerefMut` impls to `Vec<u8>` so that all slice
/// and `Vec` operations are available.
#[derive(Clone, Eq, Ord)]
pub struct Literal<I: ~const Integral> {
    v: Seq<I>,
    cut: bool,
}

#[unconst]
impl<I: ~const Integral> Literals<I> {
    /// Returns a new empty set of literals using default limits.
    pub fn empty() -> Literals<I> {
        Literals { lits: vec![], limit_size: 250, limit_class: 10 }
    }

    /// Returns a set of literal prefixes extracted from the given `Repr<I>`.
    pub fn prefixes(expr: &Repr<I>) -> Literals<I> {
        let mut lits = Literals::empty();
        lits.union_prefixes(expr);
        lits
    }

    /// Returns a set of literal suffixes extracted from the given `Repr<I>`.
    pub fn suffixes(expr: &Repr<I>) -> Literals<I> {
        let mut output = Self::prefixes(&expr.rev());
        output.reverse();
        output
    }

    /// Returns the set of literals as a slice. Its order is unspecified.
    pub fn literals(&self) -> &[Literal<I>] {
        &self.lits
    }

    /// Returns the length of the smallest literal.
    ///
    /// Returns None is there are no literals in the set.
    pub fn min_len(&self) -> Option<usize> {
        let mut min = None;
        for lit in &self.lits {
            match min {
                None => min = Some(lit.len()),
                Some(m) if lit.len() < m => min = Some(lit.len()),
                _ => {}
            }
        }
        min
    }

    /// Returns true if all members in this set are complete.
    pub fn all_complete(&self) -> bool {
        !self.lits.is_empty() && self.lits.iter().all(|l| !l.cut)
    }

    /// Returns true if any member in this set is complete.
    pub fn any_complete(&self) -> bool {
        self.lits.iter().any(|lit| !lit.cut)
    }

    /// Returns true if this set contains an empty literal.
    pub fn contains_empty(&self) -> bool {
        self.lits.iter().any(|lit| lit.is_empty())
    }

    /// Returns true if this set is empty or if all of its members is empty.
    pub fn is_empty(&self) -> bool {
        self.lits.is_empty() || self.lits.iter().all(|lit| lit.is_empty())
    }

    /// Returns a new empty set of literals using this set's limits.
    pub fn new_empty(&self) -> Literals<I> {
        Literals {
            lits: vec![],
            limit_size: self.limit_size,
            limit_class: self.limit_class
        }
    }

    /// Returns the longest common prefix of all members in this set.
    pub fn longest_common_prefix(&self) -> &[I] {
        if self.is_empty() {
            return &[];
        }
        let lit0 = &*self.lits[0];
        let mut len = lit0.len();
        for lit in &self.lits[1..] {
            len = cmp::min(
                len,
                (lit.v.deref()).iter().zip(*lit0).take_while(|(a, b)| a == &b).count(),
            );
        }
        &self.lits[0][..len]
    }

    /// Returns the longest common suffix of all members in this set.
    pub fn longest_common_suffix(&self) -> &[I] {
        if self.is_empty() {
            return &[];
        }
        let lit0 = &*self.lits[0];
        let mut len = lit0.len();
        for lit in &self.lits[1..] {
            len = cmp::min(
                len,
                lit.iter()
                    .rev()
                    .zip(lit0.iter().rev())
                    .take_while(|&(a, b)| a == b)
                    .count(),
            );
        }
        &self.lits[0][self.lits[0].len() - len..]
    }

    /// Returns a new set of literals with the given len trimmed
    /// from the suffix of each literal.
    ///
    /// If any literal would be cut out completely by trimming, then None is
    /// returned.
    ///
    /// Any duplicates that are created as a result of this transformation are
    /// removed.
    pub fn trim_suffix(&self, len: usize) -> Option<Literals<I>> {
        if self.min_len().map(|len_| len_ <= len).unwrap_or(true) {
            return None;
        }
        let mut new = self.new_empty();
        for mut lit in self.lits.iter().cloned() {
            let new_len = lit.len() - len;
            lit.truncate(new_len);
            lit.cut = true;
            new.lits.push(lit);
        }
        new.lits.sort();
        new.lits.dedup();
        Some(new)
    }

    /// Returns a new set of prefixes of this set of literals that are
    /// guaranteed to be unambiguous.
    ///
    /// Any substring match with a member of the set is returned is guaranteed
    /// to never overlap with a substring match of another member of the set
    /// at the same starting position.
    ///
    /// Given any two members of the returned set, neither is a substring of
    /// the other.
    pub fn unambiguous_prefixes(&self) -> Literals<I> {
        if self.lits.is_empty() {
            return self.new_empty();
        }
        let mut old = self.lits.to_vec();
        let mut new = self.new_empty();
        'OUTER: while let Some(mut candidate) = old.pop() {
            if candidate.is_empty() {
                continue;
            }
            if new.lits.is_empty() {
                new.lits.push(candidate);
                continue;
            }
            for lit2 in &mut new.lits {
                if lit2.is_empty() {
                    continue;
                }
                if &candidate == lit2 {
                    // If the literal is already in the set, then we can
                    // just drop it. But make sure that cut literals are
                    // infectious!
                    candidate.cut = candidate.cut || lit2.cut;
                    lit2.cut = candidate.cut;
                    continue 'OUTER;
                }
                if candidate.len() < lit2.len() {
                    if let Some(i) = position(&candidate, &lit2) {
                        candidate.cut = true;
                        let mut lit3 = lit2.clone();
                        lit3.truncate(i);
                        lit3.cut = true;
                        old.push(lit3);
                        lit2.clear();
                    }
                } else if let Some(i) = position(&lit2, &candidate) {
                    lit2.cut = true;
                    let mut new_candidate = candidate.clone();
                    new_candidate.truncate(i);
                    new_candidate.cut = true;
                    old.push(new_candidate);
                    candidate.clear();
                }
                // Oops, the candidate is already represented in the set.
                if candidate.is_empty() {
                    continue 'OUTER;
                }
            }
            new.lits.push(candidate);
        }
        new.lits.retain(|lit| !lit.is_empty());
        new.lits.sort();
        new.lits.dedup();
        new
    }

    /// Returns a new set of suffixes of this set of literals that are
    /// guaranteed to be unambiguous.
    ///
    /// Any substring match with a member of the set is returned is guaranteed
    /// to never overlap with a substring match of another member of the set
    /// at the same ending position.
    ///
    /// Given any two members of the returned set, neither is a substring of
    /// the other.
    pub fn unambiguous_suffixes(&self) -> Literals<I> {
        // This is a touch wasteful...
        let mut lits = self.clone();
        lits.reverse();
        let mut unamb = lits.unambiguous_prefixes();
        unamb.reverse();
        unamb
    }

    /// Unions the prefixes from the given expression to this set.
    ///
    /// If prefixes could not be added (for example, this set would exceed its
    /// size limits or the set of prefixes from `expr` includes the empty
    /// string), then false is returned.
    ///
    /// Note that prefix literals extracted from `expr` are said to be complete
    /// if and only if the literal extends from the beginning of `expr` to the
    /// end of `expr`.
    pub fn union_prefixes(&mut self, expr: &Repr<I>) -> bool {
        let mut lits = self.new_empty();
        prefixes(expr, &mut lits);
        !lits.is_empty() && !lits.contains_empty() && self.union(lits)
    }

    /// Unions this set with another set.
    ///
    /// If the union would cause the set to exceed its limits, then the union
    /// is skipped and it returns false. Otherwise, if the union succeeds, it
    /// returns true.
    pub fn union(&mut self, lits: Self) -> bool {
        if self.sum_len() + lits.sum_len() > self.limit_size {
            return false;
        }
        if lits.is_empty() {
            self.lits.push(Literal::empty());
        } else {
            self.lits.extend(lits.lits);
        }
        true
    }

    /// Extends this set with another set.
    ///
    /// The set of literals is extended via a cross product.
    ///
    /// If a cross product would cause this set to exceed its limits, then the
    /// cross product is skipped and it returns false. Otherwise, if the cross
    /// product succeeds, it returns true.
    pub fn cross_product(&mut self, lits: &Literals<I>) -> bool {
        if lits.is_empty() {
            return true;
        }
        // Check that we make sure we stay in our limits.
        let mut size_after;
        if self.is_empty() || !self.any_complete() {
            size_after = self.sum_len();
            for lit in lits.literals() {
                size_after += lit.len();
            }
        } else {
            size_after = self.lits.iter().fold(0, |accum, lit| {
                accum + if lit.cut { lit.len() } else { 0 }
            });
            for lit in lits.literals() {
                for self_lit in self.literals() {
                    if !self_lit.cut {
                        size_after += self_lit.len() + lit.len();
                    }
                }
            }
        }
        if size_after > self.limit_size {
            return false;
        }

        let mut base = self.remove_complete();
        if base.is_empty() {
            base = vec![Literal::empty()];
        }
        for lit in lits.literals() {
            for mut self_lit in base.clone() {
                self_lit.v.mul(lit.v);
                self_lit.cut = lit.cut;
                self.lits.push(self_lit);
            }
        }
        true
    }

    /// Extends each literal in this set with the Seq given.
    ///
    /// If the set is empty, then the given literal is added to the set.
    ///
    /// If adding any number of bytes to all members of this set causes a limit
    /// to be exceeded, then no bytes are added and false is returned. If a
    /// prefix of `bytes` can be fit into this set, then it is used and all
    /// resulting literals are cut.
    pub fn cross_add(&mut self, seq: &Seq<I>) -> bool {
        // N.B. This could be implemented by simply calling cross_product with
        // a literal set containing just `bytes`, but we can be smarter about
        // taking shorter prefixes of `bytes` if they'll fit.
        // if bytes.is_empty() {
        //     return true;
        // }
        if self.lits.is_empty() {
            let i = cmp::min(self.limit_size, 1);
            self.lits.push(Literal::new(seq.to_owned()));
            self.lits[0].cut = i < 1;
            return !self.lits[0].cut;
        }
        let size = self.sum_len();
        if size + self.lits.len() >= self.limit_size {
            return false;
        }
        let mut i = 1;
        while size + (i * self.lits.len()) <= self.limit_size
            && i < 1
        {
            i += 1;
        }
        for lit in &mut self.lits {
            if !lit.cut {
                lit.extend(*seq);
                if i < 1 {
                    lit.cut = true;
                }
            }
        }
        true
    }

    /// Adds the given literal to this set.
    ///
    /// Returns false if adding this literal would cause the class to be too
    /// big.
    pub fn add(&mut self, lit: Literal<I>) -> bool {
        if self.sum_len() + lit.len() > self.limit_size {
            return false;
        }
        self.lits.push(lit);
        true
    }

    /// Extends each literal in this set with the Interval given, writing the bytes of each character in reverse when `Interval<char>`.
    ///
    /// Returns false if the Interval was too big to add.
    pub fn add_seq(&mut self, interval: &Interval<I>, reverse: bool) -> bool {
        if self.class_exceeds_limits(interval.len()) {
            return false;
        }
        let mut base = self.remove_complete();
        if base.is_empty() {
            base = vec![Literal::empty()];
        }
        for c in *interval {
            for mut lit in base.clone() {
                lit.push(c);
                self.lits.push(lit);
            }
        }
        true
    }

    /// Cuts every member of this set. When a member is cut, it can never
    /// be extended.
    pub fn cut(&mut self) {
        for lit in &mut self.lits {
            lit.cut = true;
        }
    }

    /// Reverses all members in place.
    pub fn reverse(&mut self) {
        for lit in &mut self.lits {
            lit.reverse();
        }
    }

    /// Pops all complete literals out of this set.
    fn remove_complete(&mut self) -> Vec<Literal<I>> {
        let mut base = vec![];
        for lit in mem::take(&mut self.lits) {
            if lit.cut {
                self.lits.push(lit);
            } else {
                base.push(lit);
            }
        }
        base
    }

    /// Returns the total number of characters in this set.
    fn sum_len(&self) -> usize {
        self.lits.iter().fold(0, |acc, lit| acc + lit.len())
    }

    /// Returns true if a character class with the given size would cause this
    /// set to exceed its limits.
    ///
    /// The size given should correspond to the number of items in the class.
    fn class_exceeds_limits(&self, size: usize) -> bool {
        if size > self.limit_class {
            return true;
        }
        // This is an approximation since codepoints in a char class can encode
        // to 1-4 bytes.
        let new_byte_count = if self.lits.is_empty() {
            size
        } else {
            self.lits.iter().fold(0, |accum, lit| {
                accum
                    + if lit.cut {
                        // If the literal is cut, then we'll never add
                        // anything to it, so don't count it.
                        0
                    } else {
                        (lit.len() + 1) * size
                    }
            })
        };
        new_byte_count > self.limit_size
    }
}

#[unconst]
const fn prefixes<I: ~const Integral>(expr: &Repr<I>, lits: &mut Literals<I>)
    where I: ~const Integral,
{
    match expr {
        Repr::Zero(_) => {}
        Repr::One(c) => { lits.cross_add(&c); },
        Repr::Interval(ref seq) => {
            if !lits.add_seq(seq, false) {
                lits.cut();
            }
        }
        Repr::Exp(repr) => repeat_zero_or_more_literals(&repr, lits, prefixes),
        Repr::And(ref lhs, ref rhs) => {
            for e in [lhs, rhs] {
                if let Repr::Zero(Zero::StartText) = **e {
                    if !lits.is_empty() {
                        lits.cut();
                        break;
                    }
                    lits.add(Literal::empty());
                    continue;
                }
                let mut lits2 = lits.new_empty();
                prefixes(e, &mut lits2);
                if !lits.cross_product(&lits2) || !lits2.any_complete() {
                    // If this expression couldn't yield any literal that
                    // could be extended, then we need to quit. Since we're
                    // short-circuiting, we also need to freeze every member.
                    lits.cut();
                    break;
                }
            }
        }
        Repr::Or(ref lhs, ref rhs) => {
            alternate_literals(lhs, rhs, lits, prefixes);
        }
        _ => lits.cut(),
    }
}

#[unconst]
const fn suffixes<I>(expr: &Repr<I>, lits: &mut Literals<I>)
    where I: ~const Integral,
{
    match *expr {
        Repr::One(c) => {
            lits.cross_add(&c);
        }
        Repr::Interval(ref seq) => {
            if !lits.add_seq(seq, false) {
                lits.cut();
            }
        }
        Repr::Exp(repr) => repeat_zero_or_more_literals(&repr, lits, suffixes),
        Repr::And(ref lhs, ref rhs) => {
            for e in [rhs, lhs] {
                if let Repr::Zero(Zero::EndText) = e.as_ref() {
                    if !lits.is_empty() {
                        lits.cut();
                        break;
                    }
                    lits.add(Literal::empty());
                    continue;
                }
                let mut lits2 = lits.new_empty();
                suffixes(e, &mut lits2);
                if !lits.cross_product(&lits2) || !lits2.any_complete() {
                    // If this expression couldn't yield any literal that
                    // could be extended, then we need to quit. Since we're
                    // short-circuiting, we also need to freeze every member.
                    lits.cut();
                    break;
                }
            }
        }
        Repr::Or(ref lhs, ref rhs) => {
            alternate_literals(lhs, rhs, lits, suffixes);
        }
        _ => lits.cut(),
    }
}

#[unconst]
const fn repeat_zero_or_more_literals<I, F>(
    e: &Repr<I>,
    lits: &mut Literals<I>,
    mut f: F,
)
    where I: ~const Integral,
          F: FnMut(&Repr<I>, &mut Literals<I>)
{
    let (mut lits2, mut lits3) = (lits.clone(), lits.new_empty());
    lits3.limit_size = lits.limit_size / 2;
    f(e, &mut lits3);

    if lits3.is_empty() || !lits2.cross_product(&lits3) {
        lits.cut();
        return;
    }
    lits2.cut();
    lits2.add(Literal::empty());
    if !lits.union(lits2) {
        lits.cut();
    }
}

// TODO
// This is a bit conservative. If `max` is set, then we could
// treat this as a finite set of alternations. For now, we
// just treat it as `e*`.
#[unconst]
const fn repeat_range_literals<S, I, F>(
    e: &Repr<I>,
    min: usize,
    lits: &mut Literals<I>,
    mut f: F,
)
    where I: ~const Integral,
          F: FnMut(&Repr<I>, &mut Literals<I>)
{
    if lits.contains_empty() {
        lits.cut();
    }
}

#[unconst]
const fn alternate_literals<I: ~const Integral, F>(
    lhs: &Repr<I>,
    rhs: &Repr<I>,
    lits: &mut Literals<I>,
    mut f: F,
)
    where I: ~const Integral,
          F: FnMut(&Repr<I>, &mut Literals<I>)
{
    let mut lits2 = lits.new_empty();
    for e in [lhs, rhs] {
        let mut lits3 = lits.new_empty();
        lits3.limit_size = lits.limit_size / 5;
        f(e, &mut lits3);
        if lits3.is_empty() || !lits2.union(lits3) {
            // If we couldn't find suffixes for *any* of the
            // alternates, then the entire alternation has to be thrown
            // away and any existing members must be frozen. Similarly,
            // if the union couldn't complete, stop and freeze.
            lits.cut();
            return;
        }
    }
    if !lits.cross_product(&lits2) {
        lits.cut();
    }
}

#[unconst]
impl<I: ~const Integral> Debug for Literals<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Literals")
            .field("lits", &self.lits)
            .field("limit_size", &self.limit_size)
            .field("limit_class", &self.limit_class)
            .finish()
    }
}

#[unconst]
impl<I: ~const Integral> Literal<I> {
    /// Returns a new complete literal with the bytes given.
    pub fn new(seq: Seq<I>) -> Literal<I> {
        Literal { v: seq.into(), cut: false }
    }

    /// Returns a new complete empty literal.
    pub fn empty() -> Literal<I> {
        Literal { v: Seq::empty(), cut: false }
    }

    /// Cuts this literal.
    pub fn cut(&mut self) {
        self.cut = true;
    }
}

#[unconst]
impl<I: ~const Integral> const PartialEq for Literal<I> {
    fn eq(&self, other: &Literal<I>) -> bool {
        self.v == other.v
    }
}

#[unconst]
impl<I: ~const Integral> const PartialOrd for Literal<I> {
    fn partial_cmp(&self, other: &Literal<I>) -> Option<cmp::Ordering> {
        self.v.partial_cmp(&other.v)
    }
}

#[unconst]
impl<I: ~const Integral> Debug for Literal<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.cut {
            write!(f, "Cut({:?})", &self.v)
        } else {
            write!(f, "Complete({:?})", &self.v)
        }
    }
}

#[unconst]
impl<I: ~const Integral> AsRef<[I]> for Literal<I> {
    fn as_ref(&self) -> &[I] {
        self.v.as_ref()
    }
}

#[unconst]
impl<I: ~const Integral> Deref for Literal<I> {
    type Target = Seq<I>;
    fn deref(&self) -> &Seq<I> {
        &self.v
    }
}

#[unconst]
impl<I: ~const Integral> DerefMut for Literal<I> {
    fn deref_mut(&mut self) -> &mut Seq<I> {
        &mut self.v
    }
}

#[unconst]
const fn position<I>(needle: &[I], mut context: &[I]) -> Option<usize>
    where I: ~const Integral
{
    let mut i = 0;
    while context.len() >= needle.len() {
        if needle == &context[..needle.len()] {
            return Some(i);
        }
        i += 1;
        context = &context[1..];
    }
    None
}

fn char_len_lossy(bytes: &[u8]) -> usize {
    String::from_utf8_lossy(bytes).chars().count()
}

#[unconst]
/// Parsed represents a set of parsed regular expressions and their detected
/// literals.
pub struct Parsed<I: ~const Integral> {
    pub reprs: Vec<Repr<I>>,
    pub prefixes: Literals<I>,
    pub suffixes: Literals<I>,
}

#[unconst]
impl<I: ~const Integral> Parsed<I> {
    /// Parse the current set of patterns into their AST and extract literals.
    pub fn parse(repr: &Repr<I>) -> Parsed<I> {
        let mut prefixes = Some(Literals::empty());
        let mut suffixes = Some(Literals::empty());
        let is_set = true;
        // If we're compiling a regex set and that set has any anchored
        // expressions, then disable all literal optimizations.
        let mut reprs = Vec::new();
        let mut current = repr;
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
            reprs.push(repr);
        }
        Parsed {
            reprs,
            prefixes: prefixes.unwrap_or_else(Literals::empty),
            suffixes: suffixes.unwrap_or_else(Literals::empty),
        }
    }
}