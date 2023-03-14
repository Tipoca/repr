use alloc::vec::Vec;
use core::{
    fmt::{self, Debug},
    mem::size_of,
    ops::Deref,
    slice
};

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, MatchKind};
use unconst::unconst;

use crate::derivative::{LiteralSearcher, Parsed};
use crate::interval::Interval;
use crate::options::Options;
use crate::repr::{Repr, Integral, Zero};
use crate::seq::Seq;
use crate::sparse::SparseSet;

/// `Index` represents the index of an instruction in a regex program.
pub type Index = usize;

#[unconst]
/// Program is a sequence of instructions and various facts about those
/// instructions.
/// 
/// `Program` comprises all read only state for a regex. Namely, all such
/// state is determined at compile time and never changes during search.
/// 
/// A compiled program that is used in the NFA simulation and backtracking.
#[derive(Clone)]
pub struct Program<I: ~const Integral> {
    /// A sequence of instructions that represents an NFA.
    pub insts: Vec<Inst<I>>,
    /// Pointers to each Match instruction in the sequence.
    ///
    /// This is always length 1 unless this program represents a regex set.
    pub matches: Vec<Index>,
    /// A pointer to the start instruction. This can vary depending on how
    /// the program was compiled. For example, programs for use with the DFA
    /// engine have a `.*?` inserted at the beginning of unanchored regular
    /// expressions. The actual starting point of the program is after the
    /// `.*?`.
    pub start: Index,
    /// Whether the regex must match from the start of the input.
    pub is_anchored_start: bool,
    /// Whether the regex must match at the end of the input.
    pub is_anchored_end: bool,
    /// Whether this program contains a Unicode word boundary instruction.
    pub has_unicode_word_boundary: bool,
    /// A possibly empty machine for very quickly matching prefix literals.
    pub prefixes: LiteralSearcher<I>,
    /// A possibly empty machine for very quickly matching suffixe literals.
    pub suffixes: LiteralSearcher<I>,
    #[cfg(feature = "perf-literal")]
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
    pub ac: Option<AhoCorasick<u32>>,
    /// A limit on the size of the cache that the DFA is allowed to use while
    /// matching.
    ///
    /// The cache limit specifies approximately how much space we're willing to
    /// give to the state cache. Once the state cache exceeds the size, it is
    /// wiped and all states must be re-computed.
    ///
    /// Note that this value does not impact correctness. It can be set to 0
    /// and the DFA will run just fine. (It will only ever store exactly one
    /// state in the cache, and will likely run very slowly, but it will work.)
    ///
    /// Also note that this limit is *per thread of execution*. That is,
    /// if the same regex is used to search text across multiple threads
    /// simultaneously, then the DFA cache is not shared. Instead, copies are
    /// made.
    pub dfa_size_limit: usize,
    /// mode encodes as much upfront knowledge about how we're going to
    /// execute a search as possible.
    pub mode: Mode,
}

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    /// A single or multiple literal search. This is only used when the regex
    /// can be decomposed into a literal search.
    #[cfg(feature = "perf-literal")]
    Seq(SeqMode),
    /// An NFA variant.
    Nfa,
}

#[derive(Clone, Copy, Debug)]
#[cfg(feature = "perf-literal")]
pub enum SeqMode {
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

#[unconst]
impl<I: ~const Integral> Program<I> {
    /// Various options can be set before calling `compile` on an expression.
    pub const fn new(options: &Options<I>) -> Self {
        let parsed = Parsed::parse(&options.repr);
        let compiler = Compiler::new(options);
        let mut output = compiler.compile(&parsed.reprs);
        output.prefixes = LiteralSearcher::prefixes(parsed.prefixes);
        output.suffixes = LiteralSearcher::suffixes(parsed.suffixes);
        #[cfg(feature = "perf-literal")]
        output.ac = output.build_aho_corasick(&parsed);
        output.choose_mode();
        output
    }

    /// Creates an empty instruction sequence. Fields are given default
    /// values.
    pub fn default() -> Self {
        Program {
            insts: vec![],
            matches: vec![],
            start: 0,
            // byte_classes: vec![0; 256],
            is_anchored_start: false,
            is_anchored_end: false,
            has_unicode_word_boundary: false,
            prefixes: LiteralSearcher::empty(),
            suffixes: LiteralSearcher::empty(),
            dfa_size_limit: 2 * (1 << 20),
            #[cfg(feature = "perf-literal")]
            ac: Default::default(),
            mode: Mode::Nfa,
        }
    }

    #[cfg(feature = "perf-literal")]
    fn build_aho_corasick(&self, parsed: &Parsed<I>) -> Option<AhoCorasick<u32>> {
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

    /// If a plain literal scan can be used, then a corresponding literal
    /// search type is returned.
    fn choose_mode(&mut self) {
        if self.ac.is_some() {
            self.mode = Mode::Seq(SeqMode::AhoCorasick);
        }
        if self.prefixes.complete() {
            if self.is_anchored_start {
                self.mode = Mode::Seq(SeqMode::AnchoredStart);
            } else {
                self.mode = Mode::Seq(SeqMode::Unanchored);
            };
        }
        if self.suffixes.complete() {
            return if self.is_anchored_end {
                self.mode = Mode::Seq(SeqMode::AnchoredEnd);
            } else {
                // This case shouldn't happen. When the regex isn't
                // anchored, then complete prefixes should imply complete
                // suffixes.
                self.mode = Mode::Seq(SeqMode::Unanchored);
            };
        }
    }

    /// Return true if and only if an execution engine at instruction `pc` will
    /// always lead to a match.
    pub fn leads_to_match(&self, pc: usize) -> bool {
        if self.matches.len() > 1 {
            // If we have a regex set, then we have more than one ending
            // state, so leading to one of those states is generally
            // meaningless.
            return false;
        }
        match self[pc] {
            Inst::Match(_) => true,
            _ => false,
        }
    }

    /// Return the approximate heap usage of this instruction sequence in
    /// bytes.
    pub fn approximate_size(&self) -> usize {
        // The only instruction that uses heap space is Ranges (for
        // Unicode codepoint programs) to store non-overlapping codepoint
        // ranges. To keep this operation constant time, we ignore them.
        (self.len() * size_of::<Inst<I>>())
            + (self.matches.len() * size_of::<Index>())
            + (256 * size_of::<u8>())
            + self.prefixes.approximate_size()
    }
}

// /// Alternation literals checks if the given Repr is a simple alternation of
// /// literals, and if so, returns them. Otherwise, this returns None.
// #[cfg(feature = "perf-literal")]
// fn or_constants<I: Integral>(repr: &Repr<I>) -> Option<Vec<Vec<u8>>> {
//     // This is pretty hacky, but basically, if `is_alternation_literal` is
//     // true, then we can make several assumptions about the structure of our
//     // Repr. This is what justifies the `unreachable!` statements below.
//     //
//     // This code should be refactored once we overhaul this crate's
//     // optimization pipeline, because this is a terribly inflexible way to go
//     // about things.

//     if !repr.is_alternation_literal() {
//         return None;
//     }
//     let mut constants = Vec::new();
//     let mut current = repr;
//     // One literal isn't worth it.
//     while let Repr::Or(lhs, rhs) = current {
//         let mut constant = Seq::empty();
//         match lhs {
//             Repr::One(ref seq) => constant = constant.mul(seq),
//             Repr::And(ref exprs) => {
//                 for e in exprs {
//                     match *e {
//                         Repr::One(ref x) => constant = constant.mul(x),
//                         _ => unreachable!("expected literal, got {:?}", e),
//                     }
//                 }
//             }
//             _ => unreachable!("expected literal or concat, got {:?}", lhs),
//         }
//         constants.push(constant);
//     }

//     Some(constants)
// }

#[unconst]
impl<I: ~const Integral> Deref for Program<I> {
    type Target = [Inst<I>];

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn deref(&self) -> &Self::Target {
        &*self.insts
    }
}

#[unconst]
impl<I: ~const Integral> Debug for Program<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn with_goto(cur: usize, goto: usize, fmtd: String) -> String {
            if goto == cur + 1 {
                fmtd
            } else {
                format!("{} (goto: {})", fmtd, goto)
            }
        }

        fn visible_byte(b: u8) -> String {
            use std::ascii::escape_default;
            let escaped = escape_default(b).collect::<Vec<u8>>();
            String::from_utf8_lossy(&escaped).into_owned()
        }

        for (pc, inst) in self.iter().enumerate() {
            match *inst {
                Inst::Match(slot) => write!(f, "{:04} Match({:?})", pc, slot)?,
                Inst::Or { goto1, goto2 } => {
                    write!(f, "{:04} Or({}, {})", pc, goto1, goto2)?;
                }
                Inst::Zero { goto, zero } => {
                    let s = format!("{:?}", zero);
                    write!(f, "{:04} {}", pc, with_goto(pc, goto, s))?;
                }
                Inst::One { goto, seq } => {
                    let s = format!("{:?}", seq);
                    write!(f, "{:04} {}", pc, with_goto(pc, goto, s))?;
                }
                Inst::Interval { goto, interval } => {
                    let ranges = format!("{:?}-{:?}", interval.0, interval.1);
                    write!(f, "{:04} {}", pc, with_goto(pc, goto, ranges))?;
                }
            }
            if pc == self.start {
                write!(f, " (start)")?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<'a, I: Integral> IntoIterator for &'a Program<I> {
    type Item = &'a Inst<I>;
    type IntoIter = slice::Iter<'a, Inst<I>>;
    
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<I: Integral> Inst<I> {
    /// Returns true if and only if this is a match instruction.
    pub fn is_match(&self) -> bool {
        match *self {
            Inst::Match(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
struct Patch {
    hole: Hole,
    entry: Index,
}

#[derive(Debug)]
enum Hole {
    None,
    One(Index),
    Many(Vec<Hole>),
}

impl Hole {
    fn dup_one(self) -> (Self, Self) {
        match self {
            Hole::One(i) => (Hole::One(i), Hole::One(i)),
            _ => unreachable!("must be called on single hole")
        }
    }
}

#[derive(Clone, Debug)]
enum MaybeInst<I: Integral> {
    Compiled(Inst<I>),
    Zero(Zero),
    One(Seq<I>),
    Interval(Interval<I>),
    Or,
    Split1(Index),
    Split2(Index),
}

// Endofunctor? Effect?
/// Inst is an instruction code in a Regex program.
#[derive(Clone, Debug)]
pub enum Inst<I: Integral> {
    /// Match indicates that the program has reached a match state.
    ///
    /// The number in the match corresponds to the Nth logical regular
    /// expression in this program. This index is always 0 for normal regex
    /// programs. Values greater than 0 appear when compiling regex sets, and
    /// each match instruction gets its own unique value. The value corresponds
    /// to the Nth regex in the set.
    Match(usize),
    /// Representation of the `Zero` instruction.
    /// Zero represents a zero-width assertion in a regex program. A
    /// zero-width assertion does not consume any of the input text.
    Zero {
        /// The next location to execute in the program if this instruction
        /// succeeds.
        goto: Index,
        /// The type of zero-width assertion to check.
        zero: Zero,
    },
    /// Representation of the Char instruction.
    /// Char requires the regex program to match the character in InstOne at
    /// the current position in the input.
    One {
        /// The next location to execute in the program if this instruction
        /// succeeds.
        goto: Index,
        /// The character to test.
        seq: Seq<I>,
    },
    /// Representation of the Ranges instruction.
    /// Ranges requires the regex program to match the character at the current
    /// position in the input with one of the ranges specified in InstInterval.
    Interval  {
        /// The next location to execute in the program if this instruction
        /// succeeds.
        goto: Index,
        /// The set of Unicode scalar value ranges to test.
        interval: Interval<I>
    },
    /// Representation of the Or instruction.
    /// Or causes the program to diverge to one of two paths in the
    /// program, preferring goto1.
    Or {
        /// The first instruction to try. A match resulting from following goto1
        /// has precedence over a match resulting from following goto2.
        goto1: Index,
        /// The second instruction to try. A match resulting from following goto1
        /// has precedence over a match resulting from following goto2.
        goto2: Index,
    },
}

impl<I: Integral> MaybeInst<I> {
    fn fill(&mut self, goto: Index) {
        let maybeinst = match *self {
            Self::Zero(zero) => Inst::Zero { goto, zero },
            Self::One(seq) => Inst::One { goto, seq },
            Self::Interval(interval) => Inst::Interval { goto, interval },
            Self::Or => Self::Split1(goto),
            Self::Split1(goto1) => Inst::Or { goto1, goto2: goto },
            Self::Split2(goto2) => Inst::Or { goto1: goto, goto2 },
            _ => unreachable!(
                "not all instructions were compiled! \
                 found uncompiled instruction: {:?}",
                self
            ),
        };
        *self = maybeinst;
    }

    fn fill_split(&mut self, goto1: Index, goto2: Index) {
        let filled = match *self {
            Self::Or => Inst::Or { goto1, goto2 },
            _ => unreachable!(
                "must be called on Or instruction, \
                 instead it was called on: {:?}",
                self
            ),
        };
        *self = Self::Compiled(filled);
    }

    fn half_fill_split_goto1(&mut self, goto1: Index) {
        let half_filled = match *self {
            Self::Or => goto1,
            _ => unreachable!(
                "must be called on Or instruction, \
                 instead it was called on: {:?}",
                self
            ),
        };
        *self = Self::Split1(half_filled);
    }

    fn half_fill_split_goto2(&mut self, goto2: Index) {
        let half_filled = match *self {
            Self::Or => goto2,
            _ => unreachable!(
                "must be called on Or instruction, \
                 instead it was called on: {:?}",
                self
            ),
        };
        *self = Self::Split2(half_filled);
    }

    fn unwrap(self) -> Inst<I> {
        match self {
            Self::Compiled(inst) => inst,
            _ => unreachable!(
                "must be called on a compiled instruction, \
                 instead it was called on: {:?}",
                self
            ),
        }
    }
}

/// A compiler translates a `Repr` to a sequence of instructions. The sequence of instructions represents an NFA.
pub struct Compiler<I: Integral> {
    insts: Vec<MaybeInst<I>>,
    compiled: Program<I>,
    size_limit: usize,
    suffix_cache: SuffixCache,
    /*
    This keeps track of extra bytes allocated while compiling the regex
    program. Currently, this corresponds to two things.
    1. First is the heap memory allocated by Unicode character classes ('InstInterval').
    2. Second is a "fake" amount of memory used by empty sub-expressions, so that enough empty sub-expressions will ultimately trigger the compiler to bail because of a size limit restriction. (That empty sub-expressions don't
    add to heap memory usage is more-or-less an implementation detail.) In
    the second case, if we don't bail, then an excessively large repetition
    on an empty sub-expression can result in the compiler using a very large
    amount of CPU time.
    */
    extra_inst_bytes: usize,
}

#[unconst]
impl<I: ~const Integral> Compiler<I> {
    /// Create a new regular expression compiler.
    ///
    /// Various options can be set before calling `compile` on an expression.
    pub const fn new(options: &Options<I>) -> Self {
        Compiler {
            insts: Vec::new(),
            compiled: Program::default(),
            size_limit: options.size_limit,
            suffix_cache: SuffixCache::new(1000),
            extra_inst_bytes: 0,
        }
    }

    /// Compile a regular expression given its AST.
    ///
    /// The compiler is guaranteed to succeed unless the program exceeds the
    /// specified size limit. If the size limit is exceeded, then compilation
    /// stops and returns an error.
    pub fn compile(mut self, exprs: &[Repr<I>]) -> Program<I> {
        if exprs.len() == 1 {
            self.compile_one(&exprs[0])
        } else {
            self.compile_many(exprs)
        }
    }

    fn compile_one(mut self, expr: &Repr<I>) -> Program<I> {
        // If we're compiling a forward DFA and we aren't anchored, then
        // add a `.*?` before the first capture group.
        // Other matching engines handle this by baking the logic into the
        // matching engine itself.
        let mut dotstar_patch = Patch { hole: Hole::None, entry: 0 };
        self.compiled.is_anchored_start = expr.is_anchored_start();
        self.compiled.is_anchored_end = expr.is_anchored_end();
        let patch = self.c(expr).unwrap_or_else(|| self.next_inst());
        self.compiled.start = patch.entry;
        self.fill_to_next(patch.hole);
        self.compiled.matches = vec![self.insts.len()];
        self.push_compiled(Inst::Match(0));
        self.compile_finish()
    }

    fn compile_many(mut self, exprs: &[Repr<I>]) -> Program<I> {
        debug_assert!(exprs.len() > 1);

        self.compiled.is_anchored_start =
            exprs.iter().all(|e| e.is_anchored_start());
        self.compiled.is_anchored_end =
            exprs.iter().all(|e| e.is_anchored_end());
        let mut dotstar_patch = Patch { hole: Hole::None, entry: 0 };
        self.compiled.start = 0; // first instruction is always split
        self.fill_to_next(dotstar_patch.hole);

        let mut prev_hole = Hole::None;
        for (i, expr) in exprs[0..exprs.len() - 1].iter().enumerate() {
            self.fill_to_next(prev_hole);
            let split = self.push_split_hole();
            let Patch { hole, entry } =
                self.c(expr).unwrap_or_else(|| self.next_inst());
            self.fill_to_next(hole);
            self.compiled.matches.push(self.insts.len());
            self.push_compiled(Inst::Match(i));
            prev_hole = self.fill_split(split, Some(entry), None);
        }
        let i = exprs.len() - 1;
        let Patch { hole, entry } =
            self.c(&exprs[i]).unwrap_or_else(|| self.next_inst());
        self.fill(prev_hole, entry);
        self.fill_to_next(hole);
        self.compiled.matches.push(self.insts.len());
        self.push_compiled(Inst::Match(i));
        self.compile_finish()
    }

    fn compile_finish(mut self) -> Program<I> {
        self.compiled.insts =
            self.insts.into_iter().map(|inst| inst.unwrap()).collect();
        Ok(self.compiled)
    }

    /**
    Compile expr into self.insts, returning a patch on success,
    or an error if we run out of memory.

    All of the c_* methods of the compiler share the contract outlined
    here.

    The main thing that a c_* method does is mutate `self.insts`
    to add a list of mostly compiled instructions required to execute
    the given expression. `self.insts` contains MaybeInsts rather than
    Insts because there is some backpatching required.

    The `Patch` value returned by each c_* method provides metadata
    about the compiled instructions emitted to `self.insts`. The
    `entry` member of the patch refers to the first instruction
    (the entry point), while the `hole` member contains zero or
    more offsets to partial instructions that need to be backpatched.
    The c_* routine can't know where its list of instructions are going to
    jump to after execution, so it is up to the caller to patch
    these jumps to point to the right place. So compiling some
    expression, e, we would end up with a situation that looked like:

    ```text
    self.insts = [ ..., i1, i2, ..., iexit1, ..., iexitn, ...]
                        ^              ^             ^
                        |                \         /
                      entry                \     /
                                            hole
    ```

    To compile two expressions, e1 and e2, concatenated together we
    would do:

    ```ignore
    let patch1 = self.c(e1);
    let patch2 = self.c(e2);
    ```

    while leaves us with a situation that looks like

    ```text
    self.insts = [ ..., i1, ..., iexit1, ..., i2, ..., iexit2 ]
                        ^        ^            ^        ^
                        |        |            |        |
                   entry1        hole1   entry2        hole2
    ```

    Then to merge the two patches together into one we would backpatch
    hole1 with entry2 and return a new patch that enters at entry1
    and has hole2 for a hole. In fact, if you look at the c_mul
    method you will see that it does exactly this, though it handles
    a list of expressions rather than just the two that we use for
    an example.

    Ok(None) is returned when an expression is compiled to no
    instruction, and so no patch.entry value makes sense.
    */
    fn c(&mut self, expr: &Repr<I>) -> Patch {
        self.check_size();
        match *expr {
            Repr::Zero(Zero::Any) => self.c_empty(),
            Repr::One(seq) => self.c_one(seq),
            Repr::Interval(interval) => self.c_interval(interval),
            // Repr::Zero(Zero::StartLine) if self.compiled.is_reverse => {
            //     self.byte_classes.set_range(b'\n', b'\n');
            //     self.c_zero(prog::Zero::EndLine)
            // }
            // Repr::Zero(Zero::StartLine) => {
            //     self.byte_classes.set_range(b'\n', b'\n');
            //     self.c_zero(prog::Zero::StartLine)
            // }
            // Repr::Zero(Zero::EndLine) if self.compiled.is_reverse => {
            //     self.byte_classes.set_range(b'\n', b'\n');
            //     self.c_zero(prog::Zero::StartLine)
            // }
            // Repr::Zero(Zero::EndLine) => {
            //     self.byte_classes.set_range(b'\n', b'\n');
            //     self.c_zero(prog::Zero::EndLine)
            // }
            // Repr::Zero(Zero::StartText) if self.compiled.is_reverse => {
            //     self.c_zero(prog::Zero::EndText)
            // }
            // Repr::Zero(Zero::StartText) => {
            //     self.c_zero(prog::Zero::StartText)
            // }
            // Repr::Zero(Zero::EndText) if self.compiled.is_reverse => {
            //     self.c_zero(prog::Zero::StartText)
            // }
            // Repr::Zero(Zero::EndText) => {
            //     self.c_zero(prog::Zero::EndText)
            // }
            // Repr::Zero(Zero::Unicode) => {
            //     if !cfg!(feature = "unicode-perl") {
            //         return Err(Error::Syntax(
            //             "Unicode word boundaries are unavailable when \
            //              the unicode-perl feature is disabled"
            //                 .to_string(),
            //         ));
            //     }
            //     self.compiled.has_unicode_word_boundary = true;
            //     self.byte_classes.set_word_boundary();
            //     // We also make sure that all ASCII bytes are in a different
            //     // class from non-ASCII bytes. Otherwise, it's possible for
            //     // ASCII bytes to get lumped into the same class as non-ASCII
            //     // bytes. This in turn may cause the lazy DFA to falsely start
            //     // when it sees an ASCII byte that maps to a byte class with
            //     // non-ASCII bytes. This ensures that never happens.
            //     self.byte_classes.set_range(0, 0x7F);
            //     self.c_zero(prog::Zero::WordBoundary)
            // }
            // Repr::Zero(Zero::UnicodeNegate) => {
            //     if !cfg!(feature = "unicode-perl") {
            //         return Err(Error::Syntax(
            //             "Unicode word boundaries are unavailable when \
            //              the unicode-perl feature is disabled"
            //                 .to_string(),
            //         ));
            //     }
            //     self.compiled.has_unicode_word_boundary = true;
            //     self.byte_classes.set_word_boundary();
            //     // See comments above for why we set the ASCII range here.
            //     self.byte_classes.set_range(0, 0x7F);
            //     self.c_zero(prog::Zero::NotWordBoundary)
            // }
            // Repr::Zero(Zero::Ascii) => {
            //     self.byte_classes.set_word_boundary();
            //     self.c_zero(prog::Zero::WordBoundaryAscii)
            // }
            // Repr::Zero(Zero::AsciiNegate) => {
            //     self.byte_classes.set_word_boundary();
            //     self.c_zero(prog::Zero::NotWordBoundaryAscii)
            // }
            Repr::Mul(ref lhs, ref rhs) => self.c_mul(lhs, rhs),
            Repr::Or(ref lhs, ref rhs) => self.c_or(lhs, rhs),
            Repr::Exp(ref repr) => self.c_exp(repr),
            _ => unimplemented!()
        }
    }

    fn c_empty(&mut self) -> Option<Patch> {
        // See: https://github.com/rust-lang/regex/security/advisories/GHSA-m5pq-gvj9-9vr8
        // See: CVE-2022-24713
        //
        // Since 'empty' sub-expressions don't increase the size of
        // the actual compiled object, we "fake" an increase in its
        // size so that our 'check_size_limit' routine will eventually
        // stop compilation if there are too many empty sub-expressions
        // (e.g., via a large repetition).
        self.extra_inst_bytes += size_of::<Inst<I>>();
        None
    }

    fn c_full(&mut self) -> Patch {
        self.c(&Repr::Exp(box Repr::Interval(Interval::full())))
    }

    fn c_one(&mut self, seq: Seq<I>) -> Patch {
        let hole = self.push_hole(MaybeInst::One(seq));
        Patch { hole, entry: self.insts.len() - 1 }
    }

    fn c_interval(&mut self, seq: Interval<I>) -> Patch {
        let hole = if seq.0 == seq.1 {
            self.push_hole(MaybeInst::One(Seq::one(seq.0)))
        } else {
            self.extra_inst_bytes += size_of::<I>() * 2;
            self.push_hole(MaybeInst::Interval(seq))
        };
        Patch { hole, entry: self.insts.len() - 1 }
    }

    fn c_zero(&mut self, look: Zero) -> Patch {
        let hole = self.push_hole(MaybeInst::Zero(look));
        Patch { hole, entry: self.insts.len() - 1 }
    }

    fn c_mul(&mut self, lhs: Repr<I>, rhs: Repr<I>) -> Patch {
        let Patch { mut hole, entry } = if let Some(p) = self.c(&lhs) {
            p
        } else if let Some(p) = self.c(&rhs) {
            p
        };
        if let Some(p) = self.c(&lhs) {
            self.fill(hole, p.entry);
            hole = p.hole;
        }
        if let Some(p) = self.c(&rhs) {
            self.fill(hole, p.entry);
            hole = p.hole;
        }
        Patch { hole, entry }
    }

    fn c_or(&mut self, lhs: &Repr<I>, rhs: &Repr<I>) -> Patch {
        // Initial entry point is always the first split.
        let first_split_entry = self.insts.len();

        // Save up all of the holes from each alternate. They will all get
        // patched to point to the same location.
        let mut holes = Vec::new();

        // true indicates that the hole is a split where we want to fill
        // the second branch.
        let mut prev_hole = (Hole::None, false);
        if prev_hole.1 {
            let next = self.insts.len();
            self.fill_split(prev_hole.0, None, Some(next));
        } else {
            self.fill_to_next(prev_hole.0);
        }
        let split = self.push_split_hole();
        if let Some(Patch { hole, entry }) = self.c(lhs) {
            holes.push(hole);
            prev_hole = (self.fill_split(split, Some(entry), None), false);
        } else {
            let (split1, split2) = split.dup_one();
            holes.push(split1);
            prev_hole = (split2, true);
        }
        if let Some(Patch { hole, entry }) = self.c(&rhs) {
            holes.push(hole);
            if prev_hole.1 {
                self.fill_split(prev_hole.0, None, Some(entry));
            } else {
                self.fill(prev_hole.0, entry);
            }
        } else {
            // We ignore prev_hole.1. When it's true, it means we have two
            // empty branches both pushing prev_hole.0 into holes, so both
            // branches will go to the same place anyway.
            holes.push(prev_hole.0);
        }
        Patch { hole: Hole::Many(holes), entry: first_split_entry }
    }

    fn c_exp(&mut self, repr: &Repr<I>) -> Option<Patch> {
        let split_entry = self.insts.len();
        let split = self.push_split_hole();
        let Patch { hole: hole_rep, entry: entry_rep } = match self.c(repr) {
            Some(p) => p,
            None => return self.pop_split_hole(),
        };
        self.fill(hole_rep, split_entry);
        let split_hole = self.fill_split(split, Some(entry_rep), None);
        Some(Patch { hole: split_hole, entry: split_entry })
    }

    fn c_repeat_zero_or_one(&mut self, expr: &Repr<I>) -> Option<Patch> {
        let split_entry = self.insts.len();
        let split = self.push_split_hole();
        let Patch { hole: hole_rep, entry: entry_rep } = match self.c(expr) {
            Some(p) => p,
            None => return self.pop_split_hole(),
        };
        let split_hole = self.fill_split(split, Some(entry_rep), None);
        let holes = vec![hole_rep, split_hole];
        Some(Patch { hole: Hole::Many(holes), entry: split_entry })
    }

    // fn c_repeat_range(
    //     &mut self,
    //     expr: &Repr<I>,
    //     min: u32,
    //     max: u32,
    // ) -> Option<Patch> {
    //     let (min, max) = (u32_to_usize(min), u32_to_usize(max));
    //     debug_assert!(min <= max);
    //     let patch_concat = self.c_mul(iter::repeat(expr).take(min));
    //     if min == max {
    //         return Ok(patch_concat);
    //     }
    //     // Same reasoning as in c_repeat_range_min_or_more (we know that min <
    //     // max at this point).
    //     let patch_concat = patch_concat.unwrap_or_else(|| self.next_inst());
    //     let initial_entry = patch_concat.entry;
    //     // It is much simpler to compile, e.g., `a{2,5}` as:
    //     //
    //     //     aaa?a?a?
    //     //
    //     // But you end up with a sequence of instructions like this:
    //     //
    //     //     0: 'a'
    //     //     1: 'a',
    //     //     2: split(3, 4)
    //     //     3: 'a'
    //     //     4: split(5, 6)
    //     //     5: 'a'
    //     //     6: split(7, 8)
    //     //     7: 'a'
    //     //     8: MATCH
    //     //
    //     // This is *incredibly* inefficient because the splits end
    //     // up forming a chain, which has to be resolved everything a
    //     // transition is followed.
    //     let mut holes = Vec::new();
    //     let mut prev_hole = patch_concat.hole;
    //     for _ in min..max {
    //         self.fill_to_next(prev_hole);
    //         let split = self.push_split_hole();
    //         let Patch { hole, entry } = match self.c(expr)? {
    //             Some(p) => p,
    //             None => return self.pop_split_hole(),
    //         };
    //         prev_hole = hole;
    //         holes.push(self.fill_split(split, Some(entry), None));
    //     }
    //     holes.push(prev_hole);
    //     Ok(Some(Patch { hole: Hole::Many(holes), entry: initial_entry }))
    // }

    /// Can be used as a default value for the c_* functions when the call to
    /// c_function is followed by inserting at least one instruction that is
    /// always executed after the ones written by the c* function.
    fn next_inst(&self) -> Patch {
        Patch { hole: Hole::None, entry: self.insts.len() }
    }

    fn fill(&mut self, hole: Hole, goto: Index) {
        match hole {
            Hole::None => {}
            Hole::One(i) => {
                self.insts[i].fill(goto);
            }
            Hole::Many(holes) => {
                for hole in holes {
                    self.fill(hole, goto);
                }
            }
        }
    }

    fn fill_to_next(&mut self, hole: Hole) {
        let next = self.insts.len();
        self.fill(hole, next);
    }

    fn fill_split(&mut self, hole: Hole, goto1: Option<Index>,
                  goto2: Option<Index>,
    ) -> Hole {
        match hole {
            Hole::None => Hole::None,
            Hole::One(pc) => match (goto1, goto2) {
                (Some(goto1), Some(goto2)) => {
                    self.insts[pc].fill_split(goto1, goto2);
                    Hole::None
                }
                (Some(goto1), None) => {
                    self.insts[pc].half_fill_split_goto1(goto1);
                    Hole::One(pc)
                }
                (None, Some(goto2)) => {
                    self.insts[pc].half_fill_split_goto2(goto2);
                    Hole::One(pc)
                }
                (None, None) => unreachable!(
                    "at least one of the split holes must be filled"
                ),
            },
            Hole::Many(holes) => {
                let mut new_holes = Vec::new();
                for hole in holes {
                    new_holes.push(self.fill_split(hole, goto1, goto2));
                }
                if new_holes.is_empty() {
                    Hole::None
                } else if new_holes.len() == 1 {
                    new_holes.pop().unwrap()
                } else {
                    Hole::Many(new_holes)
                }
            }
        }
    }

    fn push_compiled(&mut self, inst: Inst<I>) {
        self.insts.push(MaybeInst::Compiled(inst));
    }

    fn push_hole(&mut self, inst: MaybeInst<I>) -> Hole {
        matches!(inst, MaybeInst::Zero { .. } | MaybeInst::One { .. } | MaybeInst::Interval { .. });
        let hole = self.insts.len();
        self.insts.push(inst);
        Hole::One(hole)
    }

    fn push_split_hole(&mut self) -> Hole {
        let hole = self.insts.len();
        self.insts.push(MaybeInst::Or);
        Hole::One(hole)
    }

    fn pop_split_hole(&mut self) -> Option<Patch> {
        self.insts.pop();
        None
    }

    fn check_size(&self) {
        let size =
            self.extra_inst_bytes + (self.insts.len() * size_of::<Inst<I>>());
        if size > self.size_limit {
            panic!("Size limit exceeds");
        }
    }
}

/// `SuffixCache` is a simple bounded hash map for caching suffix entries in
/// UTF-8 automata. For example, consider the Unicode range \u{0}-\u{FFFF}.
/// The set of byte ranges looks like this:
///
/// [0-7F]
/// [C2-DF][80-BF]
/// [E0][A0-BF][80-BF]
/// [E1-EC][80-BF][80-BF]
/// [ED][80-9F][80-BF]
/// [EE-EF][80-BF][80-BF]
///
/// Each line above translates to one alternate in the compiled regex program.
/// However, all but one of the alternates end in the same suffix, which is
/// a waste of an instruction. The suffix cache facilitates reusing them across
/// alternates.
///
/// Note that a HashMap could be trivially used for this, but we don't need its
/// overhead. Some small bounded space (LRU style) is more than enough.
///
/// This uses similar idea to [`SparseSet`](../sparse/struct.SparseSet.html),
/// except it uses hashes as original indices and then compares full keys for
/// validation against `dense` array.
type SuffixCache = SparseSet<SuffixCacheEntry>;

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
struct SuffixCacheEntry {
    key: SuffixCacheKey,
    pc: Index,
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
struct SuffixCacheKey {
    from_inst: Index,
    start: u8,
    end: u8,
}

impl SuffixCache {
    fn get(&mut self, key: SuffixCacheKey, pc: Index) -> Option<Index> {
        let hash = self.hash(&key);
        let pos = &mut self.sparse[hash];
        if let Some(entry) = self.dense.get(*pos) {
            if entry.key == key {
                return Some(entry.pc);
            }
        }
        *pos = self.dense.len();
        self.dense.push(SuffixCacheEntry { key, pc });
        None
    }

    fn hash(&self, suffix: &SuffixCacheKey) -> usize {
        // Basic FNV-1a hash as described:
        // https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        const FNV_PRIME: u64 = 1_099_511_628_211;
        let mut h = 14_695_981_039_346_656_037;
        h = (h ^ (suffix.from_inst as u64)).wrapping_mul(FNV_PRIME);
        h = (h ^ (suffix.start as u64)).wrapping_mul(FNV_PRIME);
        h = (h ^ (suffix.end as u64)).wrapping_mul(FNV_PRIME);
        (h as usize) % self.sparse.len()
    }
}

fn u32_to_usize(n: u32) -> usize {
    // In case usize is less than 32 bits, we need to guard against overflow.
    // On most platforms this compiles to nothing.
    // TODO Use `std::convert::TryFrom` once it's stable.
    if (n as u64) > (::std::usize::MAX as u64) {
        panic!("BUG: {} is too big to be pointer sized", n)
    }
    n as usize
}
