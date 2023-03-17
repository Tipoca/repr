//! Extract literal prefixes and suffixes from an `Repr<I>`.
//! <https://en.wikipedia.org/wiki/Brzozowski_derivative>
        
#[cfg(feature = "quotient")]
mod search;

use alloc::vec::Vec;
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

#[cfg(feature = "quotient")]
pub use search::LiteralSearcher;

/// A set of Seqs extracted from a Repr.
///
/// Every member of the set is a `Literal`, which is represented by a
/// `Seq<I>`. Every member is said to be either *complete* or *cut*. A complete literal means that it extends until the beginning (or end) of the regular expression. In some circumstances, this can be used to indicate a match in the regular expression.
///
/// A key aspect of literal extraction is knowing when to stop. It is not
/// feasible to blindly extract all literals from a regular expression, even if
/// there are finitely many. For example, the regular expression `[0-9]{10}`
/// has `10^10` distinct literals. For this reason, literal extraction is
/// bounded to some low number by default using heuristics, but the limits can
/// be tweaked.
#[unconst]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Set<I: ~const Integral>(Vec<Literal<I>>);
/*
This limit also applies to case insensitive literals, since each
character in the case insensitive literal is converted to a class, and
then case folded.
*/

#[unconst]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Literal<I: ~const Integral> {
    seq: Seq<I>,
    cut: bool,
}

#[unconst]
impl<I: ~const Integral> Set<I> {
    /// Returns a new empty set of literals using default limits.
    pub fn empty() -> Set<I> {
        Set(Vec::new())
    }

    /// Returns a set of literal prefixes extracted from the given `Repr<I>`.
    pub fn prefixes(expr: &Repr<I>) -> Set<I> {
        let mut set = Set::empty();
        set.union_prefixes(expr);
        set
    }

    /// Returns a set of literal suffixes extracted from the given `Repr<I>`.
    pub fn suffixes(expr: &Repr<I>) -> Set<I> {
        let mut output = Self::prefixes(&expr.rev());
        output.reverse();
        output
    }

    /// Returns the length of the smallest literal.
    ///
    /// Returns None is there are no literals in the set.
    pub fn min_len(&self) -> Option<usize> {
        let mut min = None;
        for lit in &self.0 {
            match min {
                None => min = Some(lit.seq.len()),
                Some(m) if lit.len() < m => min = Some(lit.len()),
                _ => {}
            }
        }
        min
    }

    /// Returns true if all members in this set are complete.
    pub fn all_complete(&self) -> bool {
        !self.0.is_empty() && self.0.iter().all(|seq| !seq.cut)
    }

    /// Returns true if any member in this set is complete.
    pub fn any_complete(&self) -> bool {
        self.0.iter().any(|lit| !lit.cut)
    }

    /// Returns true if this set contains an empty literal.
    pub fn contains_empty(&self) -> bool {
        self.0.iter().any(|lit| lit.is_empty())
    }

    /// Returns true if this set is empty or if all of its members is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty() || self.0.iter().all(|lit| lit.is_empty())
    }

    /// Returns the longest common prefix of all members in this set.
    pub fn longest_common_prefix(&self) -> &[I] {
        if self.is_empty() {
            return &[];
        }
        let lit0 = &*self.0[0];
        let mut len = lit0.len();
        for lit in &self.0[1..] {
            len = cmp::min(
                len,
                (lit.seq.deref()).iter().zip(*lit0).take_while(|(a, b)| a == &b).count(),
            );
        }
        &self.0[0][..len]
    }

    /// Returns the longest common suffix of all members in this set.
    pub fn longest_common_suffix(&self) -> &[I] {
        if self.is_empty() {
            return &[];
        }
        let lit0 = &*self.0[0];
        let mut len = lit0.len();
        for lit in &self.0[1..] {
            len = cmp::min(
                len,
                lit.iter()
                    .rev()
                    .zip(lit0.iter().rev())
                    .take_while(|&(a, b)| a == b)
                    .count(),
            );
        }
        &self.0[0][self.0[0].len() - len..]
    }

    /// Returns a new set of literals with the given len trimmed
    /// from the suffix of each literal.
    ///
    /// If any literal would be cut out completely by trimming, then None is
    /// returned.
    ///
    /// Any duplicates that are created as a result of this transformation are
    /// removed.
    pub fn trim_suffix(&self, len: usize) -> Option<Set<I>> {
        if self.min_len().map(|len_| len_ <= len).unwrap_or(true) {
            return None;
        }
        let mut new = Self::empty();
        for mut lit in self.0.iter().cloned() {
            let new_len = lit.len() - len;
            lit.truncate(new_len);
            lit.cut = true;
            new.0.push(lit);
        }
        new.0.sort();
        new.0.dedup();
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
    pub fn unambiguous_prefixes(&self) -> Set<I> {
        if self.0.is_empty() {
            return Self::empty();
        }
        let mut old = self.0.to_vec();
        let mut new = Self::empty();
        'OUTER: while let Some(mut candidate) = old.pop() {
            if candidate.is_empty() {
                continue;
            }
            if new.0.is_empty() {
                new.0.push(candidate);
                continue;
            }
            for lit2 in &mut new.0 {
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
            new.0.push(candidate);
        }
        new.0.retain(|lit| !lit.is_empty());
        new.0.sort();
        new.0.dedup();
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
    pub fn unambiguous_suffixes(&self) -> Set<I> {
        // This is a touch wasteful...
        let mut set = self.clone();
        set.reverse();
        let mut unamb = set.unambiguous_prefixes();
        unamb.reverse();
        unamb
    }

    /// Unions the prefixes from the given expression to this set.
    ///
    /// If prefixes could not be added (for example, the set of prefixes from `expr` includes the empty
    /// string), then false is returned.
    ///
    /// Note that prefix literals extracted from `expr` are said to be complete
    /// if and only if the literal extends from the beginning of `expr` to the
    /// end of `expr`.
    pub fn union_prefixes(&mut self, expr: &Repr<I>) {
        let mut set = Self::empty();
        prefixes(expr, &mut set);
        if !set.is_empty() && !set.contains_empty() {
            self.union(set);
        }
    }

    /// Unions this set with another set.
    pub fn union(&mut self, other: Self) {
        if !other.is_empty() {
            self.0.extend(other.0);
        } else {
            // TODO(rnarkk) experiment
            // self.0.push(Literal::empty());
            panic!("other is empty");
        }
    }

    /// Extends this set with another set.
    ///
    /// The set of literals is extended via a cross product.
    pub fn cross_product(&mut self, other: &Self) {
        assert!(!other.is_empty());
        if other.is_empty() {
            return;
        }
        let mut base = self.remove_complete();
        if base.is_empty() {
            base = vec![Literal::empty()];
        }
        for seq in other.0 {
            for mut self_lit in base.clone() {
                self_lit.seq.mul(seq.seq);
                self_lit.cut = seq.cut;
                self.0.push(self_lit);
            }
        }
    }

    /// Extends each literal in this set with the Seq given.
    ///
    /// If the set is empty, then the given literal is added to the set.
    ///
    ///  If a prefix of `bytes` can be fit into this set, then it is used and all
    /// resulting literals are cut.
    pub fn cross_add(&mut self, seq: &Seq<I>) {
        // N.B. This could be implemented by simply calling cross_product with
        // a literal set containing just `bytes`, but we can be smarter about
        // taking shorter prefixes of `bytes` if they'll fit.
        // if bytes.is_empty() {
        //     return true;
        // }
        if self.0.is_empty() {
            self.0.push(Literal::new(seq.to_owned()));
            self.0[0].cut = false;
            // TODO(rnarkk)
            // return !self.0[0].cut;
        }
        for seq in &mut self.0 {
            if !seq.cut {
                seq.extend(*seq);
            }
        }
    }

    pub fn add(&mut self, seq: Literal<I>) {
        self.0.push(seq);
    }

    /// Extends each literal in this set with the Interval given, writing the bytes of each character in reverse when `Interval<char>`.
    pub fn add_interval(&mut self, interval: &Interval<I>) {
        let mut base = self.remove_complete();
        if base.is_empty() {
            base = vec![Literal::empty()];
        }
        for c in *interval {
            for mut lit in base.clone() {
                lit.push(c);
                self.0.push(lit);
            }
        }
    }

    /// Cuts every member of this set. When a member is cut, it can never
    /// be extended.
    pub fn cut(&mut self) {
        for lit in &mut self.0 {
            lit.cut = true;
        }
    }

    /// Reverses all members in place.
    pub fn reverse(&mut self) {
        for lit in &mut self.0 {
            lit.reverse();
        }
    }

    /// Pops all 'complete' literals out of this set.
    fn remove_complete(&mut self) -> Vec<Literal<I>> {
        let mut output = Vec::new();
        for seq in mem::take(&mut self.0) {
            if seq.cut {
                self.0.push(seq);
            } else {
                output.push(seq);
            }
        }
        output
    }
}

#[unconst]
const fn prefixes<I: ~const Integral>(expr: &Repr<I>, set: &mut Set<I>)
    where I: ~const Integral,
{
    match expr {
        Repr::Zero(_) => {}
        Repr::One(seq) => set.cross_add(&seq),
        Repr::Interval(ref interval) => set.add_interval(interval),
        Repr::Or(ref lhs, ref rhs) => {
            let mut lits2 = Set::empty();
            for e in [lhs, rhs] {
                let mut lits3 = Set::empty();
                prefixes(e, &mut lits3);
                if lits3.is_empty() || !lits2.union(lits3) {
                    // If we couldn't find suffixes for *any* of the
                    // alternates, then the entire alternation has to be thrown
                    // away and any existing members must be frozen. Similarly,
                    // if the union couldn't complete, stop and freeze.
                    set.cut();
                }
            }
            if !set.cross_product(&lits2) {
                set.cut();
            }
        }
        Repr::Exp(repr) => {
            let (mut lits2, mut lits3) = (set.clone(), Set::empty());
            prefixes(repr, &mut lits3);

            if lits3.is_empty() || !lits2.cross_product(&lits3) {
                set.cut();
                return;
            }
            lits2.cut();
            lits2.add(Literal::empty());
            set.union(lits2);
        },
        Repr::And(ref lhs, ref rhs) => {
            for e in [lhs, rhs] {
                if let Repr::Zero(Zero::StartText) = **e {
                    if !set.is_empty() {
                        set.cut();
                        break;
                    }
                    set.add(Literal::empty());
                    continue;
                }
                let mut lits2 = Set::empty();
                prefixes(e, &mut lits2);
                if !set.cross_product(&lits2) || !lits2.any_complete() {
                    // If this expression couldn't yield any literal that
                    // could be extended, then we need to quit. Since we're
                    // short-circuiting, we also need to freeze every member.
                    set.cut();
                    break;
                }
            }
        }
        _ => set.cut(),
    }
}

#[unconst]
impl<I: ~const Integral> Literal<I> {
    /// Returns a new complete literal with the bytes given.
    pub fn new(seq: Seq<I>) -> Literal<I> {
        Literal { seq: seq.into(), cut: false }
    }

    /// Returns a new complete empty literal.
    pub fn empty() -> Literal<I> {
        Literal { seq: Seq::empty(), cut: false }
    }
}

#[unconst]
impl<I: ~const Integral> Deref for Literal<I> {
    type Target = Seq<I>;
    fn deref(&self) -> &Seq<I> {
        &self.seq
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
/// Parsed represents a flattened Repr and their detected seqs.
pub struct Parsed<I: ~const Integral> {
    pub reprs: Vec<Repr<I>>,
    pub prefixes: Set<I>,
    pub suffixes: Set<I>,
}

#[unconst]
impl<I: ~const Integral> Parsed<I> {
    /// Parse the current set of patterns into their AST and extract seqs.
    pub fn parse(repr: &Repr<I>) -> Parsed<I> {
        let mut prefixes = Some(Set::empty());
        let mut suffixes = Some(Set::empty());
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
            } else if repr.is_anchored_start() {
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
            } else if repr.is_anchored_end() {
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
            prefixes: prefixes.unwrap_or_else(Set::empty),
            suffixes: suffixes.unwrap_or_else(Set::empty),
        }
    }
}
