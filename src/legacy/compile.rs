use alloc::{
    sync::Arc,
    vec::Vec
};
use core::{
    fmt::{self, Debug},
    mem::size_of,
    ops::Deref,
    slice
};

use unconst::unconst;

#[cfg(feature = "quotient")]
use crate::quotient::LiteralSearcher;
use crate::quotient::Parsed;
use crate::exec::{Exec, new_pool};
use crate::interval::Interval;
use crate::repr::{Repr, Zero};
use crate::seq::Seq;
use crate::traits::Integral;

#[unconst]
/// A builder can be used to configure how the regex is built, for example, by
/// setting the default flags (which can be overridden in the expression
/// itself) or setting various limits.
/// The set of user configurable options for compiling zero or more regexes.
#[derive(Clone)]
#[allow(missing_docs)]
pub struct Options<I: ~const Integral> {
    pub repr: Repr<I>,
    /// Set the approximate size limit of the compiled regular expression.
    ///
    /// This roughly corresponds to the number of bytes occupied by a single
    /// compiled program. If the program exceeds this number, then a
    /// compilation error is returned.
    pub size_limit: usize,
    /// Set the approximate size of the cache used by the DFA.
    ///
    /// This roughly corresponds to the number of bytes that the DFA will
    /// use while searching.
    ///
    /// Note that this is a *per thread* limit. There is no way to set a global
    /// limit. In particular, if a regex is used from multiple threads
    /// simultaneously, then each thread may use up to the number of bytes
    /// specified here.
    pub dfa_size_limit: usize,
    /// Set the nesting limit for this parser.
    ///
    /// The nesting limit controls how deep the abstract syntax tree is allowed
    /// to be. If the AST exceeds the given limit (e.g., with too many nested
    /// groups), then an error is returned by the parser.
    ///
    /// The purpose of this limit is to act as a heuristic to prevent stack
    /// overflow for consumers that do structural induction on an `Ast` using
    /// explicit recursion. While this crate never does this (instead using
    /// constant stack space and moving the call stack to the heap), other
    /// crates may.
    ///
    /// This limit is not checked until the entire Ast is parsed. Therefore,
    /// if callers want to put a limit on the amount of heap space used, then
    /// they should impose a limit on the length, in bytes, of the concrete
    /// pattern string. In particular, this is viable since this parser
    /// implementation will limit itself to heap space proportional to the
    /// length of the pattern string.
    ///
    /// Note that a nest limit of `0` will return a nest limit error for most
    /// patterns but not all. For example, a nest limit of `0` permits `a` but
    /// not `ab`, since `ab` requires a concatenation, which results in a nest
    /// depth of `1`. In general, a nest limit is not something that manifests
    /// in an obvious way in the concrete syntax, therefore, it should not be
    /// used in a granular way.
    pub nest_limit: u32,
    /// Set the value for the multi-line matching (`m`) flag.
    ///
    /// When enabled, `^` matches the beginning of lines and `$` matches the
    /// end of lines.
    ///
    /// By default, they match beginning/end of the input.
    pub multi_line: bool,
    /// Set the value for the any character (`s`) flag, where in `.` matches
    /// anything when `s` is set and matches anything except for new line when
    /// it is not set (the default).
    ///
    /// N.B. "matches anything" means "any byte" when Unicode is disabled and
    /// means "any valid UTF-8 encoding of any Unicode scalar value" when
    /// Unicode is enabled.
    /// ======
    /// N.B. "matches anything" means "any byte" for `regex::bytes::RegexSet`
    /// expressions and means "any Unicode scalar value" for `regex::RegexSet`
    /// expressions.
    pub dot_matches_new_line: bool,
    /// Set the value for the greedy swap (`U`) flag.
    ///
    /// When enabled, a pattern like `a*` is lazy (tries to find shortest
    /// match) and `a*?` is greedy (tries to find longest match).
    ///
    /// By default, `a*` is greedy and `a*?` is lazy.
    pub swap_greed: bool,
    /// Set the value for the ignore whitespace (`x`) flag.
    ///
    /// When enabled, whitespace such as new lines and spaces will be ignored
    /// between expressions of the pattern, and `#` can be used to start a
    /// comment until the next new line.
    pub ignore_whitespace: bool,
}

#[unconst]
impl<I: ~const Integral> Options<I> {
    /// Create a new regular expression builder with the given pattern.
    ///
    /// If the pattern is invalid, then an error will be returned when
    /// `build` is called.
    pub const fn new(repr: Repr<I>) -> Self {
        Options {
            repr,
            size_limit: 10 * (1 << 20),
            dfa_size_limit: 2 * (1 << 20),
            nest_limit: 250,
            multi_line: false,
            dot_matches_new_line: false,
            swap_greed: false,
            ignore_whitespace: false,
        }
    }

    /// Consume the builder and compile the regular expression.
    ///
    /// Note that calling `as_str` on the resulting `Regex` will produce the
    /// pattern given to `new` verbatim. Notably, it will not incorporate any
    /// of the flags set on this builder.
    pub fn build(self) -> Exec<I> {
        let mut nfa = Program::new(&self);
        let ro = Arc::new(nfa);
        let pool = new_pool(&ro);
        Exec { ro, pool }
    }
}

/// `Index` represents the index of an instruction in a regex program.
pub type Index = usize;

#[unconst]
#[derive(Clone)]
pub struct Program<I: ~const Integral> {
    /// A sequence of instructions that represents an NFA.
    pub insts: Vec<Inst<I>>,
    /// Pointers to each True instruction in the sequence.
    ///
    /// This is always length 1 unless this program represents a regex set.
    pub matches: Vec<Index>,
    /// A pointer to the start instruction. This can vary depending on how
    /// the program was compiled. For example, programs for use with the DFA
    /// engine have a `.*?` inserted at the beginning of unanchored regular
    /// expressions. The actual starting point of the program is after the
    /// `.*?`.
    pub start: Index,
    #[cfg(feature = "quotient")]
    /// A possibly empty machine for very quickly matching prefix literals.
    pub prefixes: LiteralSearcher<I>,
    #[cfg(feature = "quotient")]
    /// A possibly empty machine for very quickly matching suffixe literals.
    pub suffixes: LiteralSearcher<I>,
    #[cfg(feature = "quotient")]
    /// An Aho-Corasick automaton with leftmost-first match semantics.
    ///
    /// This is only set when the entire regex is a simple unanchored
    /// alternation of literals. We could probably use it more circumstances,
    /// but this is already hacky enough in this architecture.
    ///
    /// N.B. We use u32 as a state ID representation under the assumption that
    /// if we were to exhaust the ID space, we probably would have long
    /// surpassed the compilation size limit.
    pub ac: Option<AhoCorasick<u32>>,
}

#[unconst]
impl<I: ~const Integral> Program<I> {
    /// Various options can be set before calling `compile` on an expression.
    pub const fn new(options: &Options<I>) -> Self {
        let parsed = Parsed::parse(&options.repr);
        let compiler = Compiler::new(options);
        let mut output = compiler.compile(&parsed.reprs);
        #[cfg(feature = "quotient")]
        output.derivative();
        output
    }

    #[cfg(feature = "quotient")]
    fn derivative(&mut self) {
        self.prefixes = LiteralSearcher::prefixes(parsed.prefixes);
        self.suffixes = LiteralSearcher::suffixes(parsed.suffixes);
        self.ac = self.build_aho_corasick(&parsed);
        self.choose_mode();
    }

    /// Creates an empty instruction sequence. Fields are given default
    /// values.
    pub fn default() -> Self {
        Program {
            #[cfg(feature = "quotient")]
            prefixes: LiteralSearcher::empty(),
            #[cfg(feature = "quotient")]
            suffixes: LiteralSearcher::empty(),
            #[cfg(feature = "quotient")]
            ac: Default::default(),
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
            Inst::True(_) => true,
            _ => false,
        }
    }
}

// /// Alternation literals checks if the given Repr is a simple alternation of
// /// literals, and if so, returns them. Otherwise, this returns None.
// #[cfg(feature = "quotient")]
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
//     while let Or(lhs, rhs) = current {
//         let mut constant = Seq::empty();
//         match lhs {
//             One(ref seq) => constant = constant.mul(seq),
//             And(ref reprs) => {
//                 for e in reprs {
//                     match *e {
//                         One(ref x) => constant = constant.mul(x),
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

impl<I: Integral> Inst<I> {
    /// Returns true if and only if this is a match instruction.
    pub fn is_match(&self) -> bool {
        match *self {
            Inst::True(_) => true,
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

/// A compiler translates a `Repr` to a sequence of instructions. The sequence of instructions represents an NFA.
pub struct Compiler<I: Integral> {
    insts: Vec<MaybeInst<I>>,
    compiled: Program<I>,
}

#[unconst]
impl<I: ~const Integral> Compiler<I> {
    pub const fn new(options: &Options<I>) -> Self {
        Compiler {
            insts: Vec::new(),
            compiled: Program::default(),
        }
    }

    /// Compile a regular expression given its AST.
    ///
    /// The compiler is guaranteed to succeed unless the program exceeds the
    /// specified size limit. If the size limit is exceeded, then compilation
    /// stops and returns an error.
    pub fn compile(mut self, reprs: &[Repr<I>]) -> Program<I> {
        if reprs.len() == 1 {
            self.compile_one(&reprs[0])
        } else {
            self.compile_many(reprs)
        }
    }

    fn compile_one(mut self, repr: &Repr<I>) -> Program<I> {
        let patch = self.c(repr).unwrap_or_else(|| self.next_inst());
        self.compiled.start = patch.entry;
        self.fill_to_next(patch.hole);
        self.compiled.matches = vec![self.insts.len()];
        self.push_compiled(Inst::True(0));
        self.compile_finish()
    }

    fn compile_many(mut self, reprs: &[Repr<I>]) -> Program<I> {
        debug_assert!(reprs.len() > 1);
        let mut dotstar_patch = Patch { hole: Hole::None, entry: 0 };
        self.compiled.start = 0; // first instruction is always split
        self.fill_to_next(dotstar_patch.hole);

        let mut prev_hole = Hole::None;
        for (i, repr) in reprs[0..reprs.len() - 1].iter().enumerate() {
            self.fill_to_next(prev_hole);
            let split = self.push_split_hole();
            let Patch { hole, entry } =
                self.c(repr).unwrap_or_else(|| self.next_inst());
            self.fill_to_next(hole);
            self.compiled.matches.push(self.insts.len());
            self.push_compiled(Inst::True(i));
            prev_hole = self.fill_split(split, Some(entry), None);
        }
        let i = reprs.len() - 1;
        let Patch { hole, entry } =
            self.c(&reprs[i]).unwrap_or_else(|| self.next_inst());
        self.fill(prev_hole, entry);
        self.fill_to_next(hole);
        self.compiled.matches.push(self.insts.len());
        self.push_compiled(Inst::True(i));
        self.compile_finish()
    }

    fn compile_finish(mut self) -> Program<I> {
        self.compiled.insts =
            self.insts.into_iter().map(|inst| inst.unwrap()).collect();
        Ok(self.compiled)
    }

    /**
    Compile repr into self.insts, returning a patch on success,
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
    fn c(&mut self, repr: &Repr<I>) -> Patch {
        self.check_size();
        match *repr {
            Repr::Zero(Zero::Any) => self.c_empty(),
            One(seq) => self.c_one(seq),
            Repr::Interval(interval) => self.c_interval(interval),
            Repr::Zero(Zero::StartLine) if self.compiled.is_reverse => {
                self.byte_classes.set_range(b'\n', b'\n');
                self.c_zero(prog::Zero::EndLine)
            }
            Repr::Zero(Zero::StartLine) => {
                self.byte_classes.set_range(b'\n', b'\n');
                self.c_zero(prog::Zero::StartLine)
            }
            Repr::Zero(Zero::EndLine) if self.compiled.is_reverse => {
                self.byte_classes.set_range(b'\n', b'\n');
                self.c_zero(prog::Zero::StartLine)
            }
            Repr::Zero(Zero::EndLine) => {
                self.byte_classes.set_range(b'\n', b'\n');
                self.c_zero(prog::Zero::EndLine)
            }
            Repr::Zero(Zero::StartText) if self.compiled.is_reverse => {
                self.c_zero(prog::Zero::EndText)
            }
            Repr::Zero(Zero::StartText) => {
                self.c_zero(prog::Zero::StartText)
            }
            Repr::Zero(Zero::EndText) if self.compiled.is_reverse => {
                self.c_zero(prog::Zero::StartText)
            }
            Repr::Zero(Zero::EndText) => {
                self.c_zero(prog::Zero::EndText)
            }
            Repr::Zero(Zero::Unicode) => {
                if !cfg!(feature = "unicode-perl") {
                    return Err(Error::Syntax(
                        "Unicode word boundaries are unavailable when \
                         the unicode-perl feature is disabled"
                            .to_string(),
                    ));
                }
                self.compiled.has_unicode_word_boundary = true;
                self.byte_classes.set_word_boundary();
                // We also make sure that all ASCII bytes are in a different
                // class from non-ASCII bytes. Otherwise, it's possible for
                // ASCII bytes to get lumped into the same class as non-ASCII
                // bytes. This in turn may cause the lazy DFA to falsely start
                // when it sees an ASCII byte that maps to a byte class with
                // non-ASCII bytes. This ensures that never happens.
                self.byte_classes.set_range(0, 0x7F);
                self.c_zero(prog::Zero::WordBoundary)
            }
            Repr::Zero(Zero::UnicodeNegate) => {
                if !cfg!(feature = "unicode-perl") {
                    return Err(Error::Syntax(
                        "Unicode word boundaries are unavailable when \
                         the unicode-perl feature is disabled"
                            .to_string(),
                    ));
                }
                self.compiled.has_unicode_word_boundary = true;
                self.byte_classes.set_word_boundary();
                // See comments above for why we set the ASCII range here.
                self.byte_classes.set_range(0, 0x7F);
                self.c_zero(prog::Zero::NotWordBoundary)
            }
            Repr::Zero(Zero::Ascii) => {
                self.byte_classes.set_word_boundary();
                self.c_zero(prog::Zero::WordBoundaryAscii)
            }
            Repr::Zero(Zero::AsciiNegate) => {
                self.byte_classes.set_word_boundary();
                self.c_zero(prog::Zero::NotWordBoundaryAscii)
            }
            Mul(ref lhs, ref rhs) => self.c_mul(lhs, rhs),
            Or(ref lhs, ref rhs) => self.c_or(lhs, rhs),
            Inf(ref repr) => self.c_exp(repr),
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
        self.c(&Inf(Box::new(Repr::Interval(Interval::full()))))
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

    fn c_repeat_zero_or_one(&mut self, repr: &Repr<I>) -> Option<Patch> {
        let split_entry = self.insts.len();
        let split = self.push_split_hole();
        let Patch { hole: hole_rep, entry: entry_rep } = match self.c(repr) {
            Some(p) => p,
            None => return self.pop_split_hole(),
        };
        let split_hole = self.fill_split(split, Some(entry_rep), None);
        let holes = vec![hole_rep, split_hole];
        Some(Patch { hole: Hole::Many(holes), entry: split_entry })
    }

    fn c_repeat_range(
        &mut self,
        repr: &Repr<I>,
        min: u32,
        max: u32,
    ) -> Option<Patch> {
        let (min, max) = (u32_to_usize(min), u32_to_usize(max));
        debug_assert!(min <= max);
        let patch_concat = self.c_mul(iter::repeat(repr).take(min));
        if min == max {
            return Ok(patch_concat);
        }
        // Same reasoning as in c_repeat_range_min_or_more (we know that min <
        // max at this point).
        let patch_concat = patch_concat.unwrap_or_else(|| self.next_inst());
        let initial_entry = patch_concat.entry;
        // It is much simpler to compile, e.g., `a{2,5}` as:
        //
        //     aaa?a?a?
        //
        // But you end up with a sequence of instructions like this:
        //
        //     0: 'a'
        //     1: 'a',
        //     2: split(3, 4)
        //     3: 'a'
        //     4: split(5, 6)
        //     5: 'a'
        //     6: split(7, 8)
        //     7: 'a'
        //     8: MATCH
        //
        // This is *incredibly* inefficient because the splits end
        // up forming a chain, which has to be resolved everything a
        // transition is followed.
        let mut holes = Vec::new();
        let mut prev_hole = patch_concat.hole;
        for _ in min..max {
            self.fill_to_next(prev_hole);
            let split = self.push_split_hole();
            let Patch { hole, entry } = match self.c(repr)? {
                Some(p) => p,
                None => return self.pop_split_hole(),
            };
            prev_hole = hole;
            holes.push(self.fill_split(split, Some(entry), None));
        }
        holes.push(prev_hole);
        Ok(Some(Patch { hole: Hole::Many(holes), entry: initial_entry }))
    }

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

fn u32_to_usize(n: u32) -> usize {
    // In case usize is less than 32 bits, we need to guard against overflow.
    // On most platforms this compiles to nothing.
    // TODO Use `std::convert::TryFrom` once it's stable.
    if (n as u64) > (::std::usize::MAX as u64) {
        panic!("BUG: {} is too big to be pointer sized", n)
    }
    n as usize
}
