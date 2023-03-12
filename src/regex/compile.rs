use std::iter;
use std::result;

use regex_syntax::utf8::Utf8Sequences;

use crate::interval::Interval;
use crate::repr::{Repr, Range, Integral, Zero};

use super::prog::{
    Inst, InstOne, InstZero, InstPtr, InstInterval, InstSplit, Program,
};

use super::Error;

type Result = result::Result<Patch, Error>;
type ResultOrEmpty = result::Result<Option<Patch>, Error>;

#[derive(Debug)]
struct Patch {
    hole: Hole,
    entry: InstPtr,
}

/// A compiler translates a regular expression AST to a sequence of
/// instructions. The sequence of instructions represents an NFA.
// `Compiler` is only public via the `internal` module, so avoid deriving
// `Debug`.
#[allow(missing_debug_implementations)]
pub struct Compiler<I: Integral> {
    insts: Vec<MaybeInst<I>>,
    compiled: Program<I>,
    num_exprs: usize,
    size_limit: usize,
    suffix_cache: SuffixCache,
    utf8_seqs: Option<Utf8Sequences>,
    // This keeps track of extra bytes allocated while compiling the regex
    // program. Currently, this corresponds to two things. First is the heap
    // memory allocated by Unicode character classes ('InstInterval'). Second is
    // a "fake" amount of memory used by empty sub-expressions, so that enough
    // empty sub-expressions will ultimately trigger the compiler to bail
    // because of a size limit restriction. (That empty sub-expressions don't
    // add to heap memory usage is more-or-less an implementation detail.) In
    // the second case, if we don't bail, then an excessively large repetition
    // on an empty sub-expression can result in the compiler using a very large
    // amount of CPU time.
    extra_inst_bytes: usize,
}

impl<I: Integral> Compiler<I> {
    /// Create a new regular expression compiler.
    ///
    /// Various options can be set before calling `compile` on an expression.
    pub fn new() -> Self {
        Compiler {
            insts: vec![],
            compiled: Program::new(),
            num_exprs: 0,
            size_limit: 10 * (1 << 20),
            suffix_cache: SuffixCache::new(1000),
            utf8_seqs: Some(Utf8Sequences::new('\x00', '\x00')),
            extra_inst_bytes: 0,
        }
    }

    /// The size of the resulting program is limited by size_limit. If
    /// the program approximately exceeds the given size (in bytes), then
    /// compilation will stop and return an error.
    pub fn size_limit(mut self, size_limit: usize) -> Self {
        self.size_limit = size_limit;
        self
    }

    /// When set, the machine returned is suitable for use in the DFA matching
    /// engine.
    ///
    /// In particular, this ensures that if the regex is not anchored in the
    /// beginning, then a preceding `.*?` is included in the program. (The NFA
    /// based engines handle the preceding `.*?` explicitly, which is difficult
    /// or impossible in the DFA engine.)
    pub fn dfa(mut self, yes: bool) -> Self {
        self.compiled.is_dfa = yes;
        self
    }

    /// When set, the machine returned is suitable for matching text in
    /// reverse. In particular, all concatenations are flipped.
    pub fn reverse(mut self, yes: bool) -> Self {
        self.compiled.is_reverse = yes;
        self
    }

    /// Compile a regular expression given its AST.
    ///
    /// The compiler is guaranteed to succeed unless the program exceeds the
    /// specified size limit. If the size limit is exceeded, then compilation
    /// stops and returns an error.
    pub fn compile(mut self, exprs: &[Repr<I>]) -> result::Result<Program<I>, Error> {
        self.num_exprs = exprs.len();
        if exprs.len() == 1 {
            self.compile_one(&exprs[0])
        } else {
            self.compile_many(exprs)
        }
    }

    fn compile_one(mut self, expr: &Repr<I>) -> result::Result<Program<I>, Error> {
        // If we're compiling a forward DFA and we aren't anchored, then
        // add a `.*?` before the first capture group.
        // Other matching engines handle this by baking the logic into the
        // matching engine itself.
        let mut dotstar_patch = Patch { hole: Hole::None, entry: 0 };
        self.compiled.is_anchored_start = expr.is_anchored_start();
        self.compiled.is_anchored_end = expr.is_anchored_end();
        if self.compiled.needs_dotstar() {
            dotstar_patch = self.c_dotstar()?;
            self.compiled.start = dotstar_patch.entry;
        }
        let patch =
            self.c_capture(expr)?.unwrap_or_else(|| self.next_inst());
        if self.compiled.needs_dotstar() {
            self.fill(dotstar_patch.hole, patch.entry);
        } else {
            self.compiled.start = patch.entry;
        }
        self.fill_to_next(patch.hole);
        self.compiled.matches = vec![self.insts.len()];
        self.push_compiled(Inst::Match(0));
        self.compile_finish()
    }

    fn compile_many(
        mut self,
        exprs: &[Repr<I>],
    ) -> result::Result<Program<I>, Error> {
        debug_assert!(exprs.len() > 1);

        self.compiled.is_anchored_start =
            exprs.iter().all(|e| e.is_anchored_start());
        self.compiled.is_anchored_end =
            exprs.iter().all(|e| e.is_anchored_end());
        let mut dotstar_patch = Patch { hole: Hole::None, entry: 0 };
        if self.compiled.needs_dotstar() {
            dotstar_patch = self.c_dotstar()?;
            self.compiled.start = dotstar_patch.entry;
        } else {
            self.compiled.start = 0; // first instruction is always split
        }
        self.fill_to_next(dotstar_patch.hole);

        let mut prev_hole = Hole::None;
        for (i, expr) in exprs[0..exprs.len() - 1].iter().enumerate() {
            self.fill_to_next(prev_hole);
            let split = self.push_split_hole();
            let Patch { hole, entry } =
                self.c_capture(expr)?.unwrap_or_else(|| self.next_inst());
            self.fill_to_next(hole);
            self.compiled.matches.push(self.insts.len());
            self.push_compiled(Inst::Match(i));
            prev_hole = self.fill_split(split, Some(entry), None);
        }
        let i = exprs.len() - 1;
        let Patch { hole, entry } =
            self.c_capture(&exprs[i])?.unwrap_or_else(|| self.next_inst());
        self.fill(prev_hole, entry);
        self.fill_to_next(hole);
        self.compiled.matches.push(self.insts.len());
        self.push_compiled(Inst::Match(i));
        self.compile_finish()
    }

    fn compile_finish(mut self) -> result::Result<Program<I>, Error> {
        self.compiled.insts =
            self.insts.into_iter().map(|inst| inst.unwrap()).collect();
        Ok(self.compiled)
    }

    /// Compile expr into self.insts, returning a patch on success,
    /// or an error if we run out of memory.
    ///
    /// All of the c_* methods of the compiler share the contract outlined
    /// here.
    ///
    /// The main thing that a c_* method does is mutate `self.insts`
    /// to add a list of mostly compiled instructions required to execute
    /// the given expression. `self.insts` contains MaybeInsts rather than
    /// Insts because there is some backpatching required.
    ///
    /// The `Patch` value returned by each c_* method provides metadata
    /// about the compiled instructions emitted to `self.insts`. The
    /// `entry` member of the patch refers to the first instruction
    /// (the entry point), while the `hole` member contains zero or
    /// more offsets to partial instructions that need to be backpatched.
    /// The c_* routine can't know where its list of instructions are going to
    /// jump to after execution, so it is up to the caller to patch
    /// these jumps to point to the right place. So compiling some
    /// expression, e, we would end up with a situation that looked like:
    ///
    /// ```text
    /// self.insts = [ ..., i1, i2, ..., iexit1, ..., iexitn, ...]
    ///                     ^              ^             ^
    ///                     |                \         /
    ///                   entry                \     /
    ///                                         hole
    /// ```
    ///
    /// To compile two expressions, e1 and e2, concatenated together we
    /// would do:
    ///
    /// ```ignore
    /// let patch1 = self.c(e1);
    /// let patch2 = self.c(e2);
    /// ```
    ///
    /// while leaves us with a situation that looks like
    ///
    /// ```text
    /// self.insts = [ ..., i1, ..., iexit1, ..., i2, ..., iexit2 ]
    ///                     ^        ^            ^        ^
    ///                     |        |            |        |
    ///                entry1        hole1   entry2        hole2
    /// ```
    ///
    /// Then to merge the two patches together into one we would backpatch
    /// hole1 with entry2 and return a new patch that enters at entry1
    /// and has hole2 for a hole. In fact, if you look at the c_concat
    /// method you will see that it does exactly this, though it handles
    /// a list of expressions rather than just the two that we use for
    /// an example.
    ///
    /// Ok(None) is returned when an expression is compiled to no
    /// instruction, and so no patch.entry value makes sense.
    fn c(&mut self, expr: &Repr<I>) -> ResultOrEmpty {
        self.check_size()?;
        match *expr {
            Repr::Zero(Zero::Any) => self.c_empty(),
            Repr::One(c) => self.c_char(c),
            Repr::Interval(seq) => self.c_class(seq),
            // Anchor(hir::Anchor::StartLine) if self.compiled.is_reverse => {
            //     self.byte_classes.set_range(b'\n', b'\n');
            //     self.c_empty_look(prog::Zero::EndLine)
            // }
            // Anchor(hir::Anchor::StartLine) => {
            //     self.byte_classes.set_range(b'\n', b'\n');
            //     self.c_empty_look(prog::Zero::StartLine)
            // }
            // Anchor(hir::Anchor::EndLine) if self.compiled.is_reverse => {
            //     self.byte_classes.set_range(b'\n', b'\n');
            //     self.c_empty_look(prog::Zero::StartLine)
            // }
            // Anchor(hir::Anchor::EndLine) => {
            //     self.byte_classes.set_range(b'\n', b'\n');
            //     self.c_empty_look(prog::Zero::EndLine)
            // }
            // Anchor(hir::Anchor::StartText) if self.compiled.is_reverse => {
            //     self.c_empty_look(prog::Zero::EndText)
            // }
            // Anchor(hir::Anchor::StartText) => {
            //     self.c_empty_look(prog::Zero::StartText)
            // }
            // Anchor(hir::Anchor::EndText) if self.compiled.is_reverse => {
            //     self.c_empty_look(prog::Zero::StartText)
            // }
            // Anchor(hir::Anchor::EndText) => {
            //     self.c_empty_look(prog::Zero::EndText)
            // }
            // WordBoundary(hir::WordBoundary::Unicode) => {
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
            //     self.c_empty_look(prog::Zero::WordBoundary)
            // }
            // WordBoundary(hir::WordBoundary::UnicodeNegate) => {
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
            //     self.c_empty_look(prog::Zero::NotWordBoundary)
            // }
            // WordBoundary(hir::WordBoundary::Ascii) => {
            //     self.byte_classes.set_word_boundary();
            //     self.c_empty_look(prog::Zero::WordBoundaryAscii)
            // }
            // WordBoundary(hir::WordBoundary::AsciiNegate) => {
            //     self.byte_classes.set_word_boundary();
            //     self.c_empty_look(prog::Zero::NotWordBoundaryAscii)
            // }
            Repr::And(ref lhs, ref rhs) => {
                if self.compiled.is_reverse {
                    self.c_concat(rhs, lhs)
                } else {
                    self.c_concat(lhs, rhs)
                }
            }
            Repr::Or(ref lhs, ref rhs) => self.c_alternate(lhs, rhs),
            Repr::Exp(ref repr, ref rep) => self.c_repeat(repr, rep),
            _ => unimplemented!()
        }
    }

    fn c_empty(&mut self) -> ResultOrEmpty {
        // See: https://github.com/rust-lang/regex/security/advisories/GHSA-m5pq-gvj9-9vr8
        // See: CVE-2022-24713
        //
        // Since 'empty' sub-expressions don't increase the size of
        // the actual compiled object, we "fake" an increase in its
        // size so that our 'check_size_limit' routine will eventually
        // stop compilation if there are too many empty sub-expressions
        // (e.g., via a large repetition).
        self.extra_inst_bytes += std::mem::size_of::<Inst<I>>();
        Ok(None)
    }

    fn c_capture(&mut self, expr: &Repr<I>) -> ResultOrEmpty {
        // Don't ever compile Save instructions for regex sets because
        // they are never used. They are also never used in DFA programs
        // because DFAs can't handle captures.
        self.c(expr)
    }

    fn c_dotstar(&mut self) -> Result {
        Ok(self.c(&Repr::Exp(Box::new(Repr::any()), Range::From(0)))?.unwrap())
    }

    fn c_char(&mut self, c: I) -> ResultOrEmpty {
        let hole = self.push_hole(InstHole::One(c));
        Ok(Some(Patch { hole, entry: self.insts.len() - 1 }))
    }

    fn c_class(&mut self, seq: Interval<I>) -> ResultOrEmpty {
        use std::mem::size_of;

        let hole = if seq.0 == seq.1 {
            self.push_hole(InstHole::One(seq.0))
        } else {
            self.extra_inst_bytes += size_of::<I>() * 2;
            self.push_hole(InstHole::Interval(seq))
        };
        Ok(Some(Patch { hole, entry: self.insts.len() - 1 }))
    }

    fn c_empty_look(&mut self, look: Zero) -> ResultOrEmpty {
        let hole = self.push_hole(InstHole::Zero(look));
        Ok(Some(Patch { hole, entry: self.insts.len() - 1 }))
    }

    fn c_concat(&mut self, lhs: Repr<I>, rhs: Repr<I>) -> ResultOrEmpty
    {
        let Patch { mut hole, entry } = if let Some(p) = self.c(&lhs)? {
            p
        } else if let Some(p) = self.c(&rhs)? {
            p
        };
        if let Some(p) = self.c(&lhs)? {
            self.fill(hole, p.entry);
            hole = p.hole;
        }
        if let Some(p) = self.c(&rhs)? {
            self.fill(hole, p.entry);
            hole = p.hole;
        }
        Ok(Some(Patch { hole, entry }))
    }

    fn c_alternate(&mut self, lhs: &Repr<I>, rhs: &Repr<I>) -> ResultOrEmpty {
        // Initial entry point is always the first split.
        let first_split_entry = self.insts.len();

        // Save up all of the holes from each alternate. They will all get
        // patched to point to the same location.
        let mut holes = vec![];

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
        if let Some(Patch { hole, entry }) = self.c(lhs)? {
            holes.push(hole);
            prev_hole = (self.fill_split(split, Some(entry), None), false);
        } else {
            let (split1, split2) = split.dup_one();
            holes.push(split1);
            prev_hole = (split2, true);
        }
        if let Some(Patch { hole, entry }) = self.c(&rhs)? {
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
        Ok(Some(Patch { hole: Hole::Many(holes), entry: first_split_entry }))
    }

    fn c_repeat(&mut self, rep: &Repr<I>, range: Range) -> ResultOrEmpty {
        match range {
            Range::Full(0, 1) => self.c_repeat_zero_or_one(&rep),
            Range::From(0) => self.c_repeat_zero_or_more(&rep),
            Range::From(1) => self.c_repeat_one_or_more(&rep),
            Range::Full(n, m) if n == m => self.c_repeat_range(&rep, n, n),
            Range::From(m) => self.c_repeat_range_min_or_more(&rep, m),
            Range::Full(n, m) => self.c_repeat_range(&rep, n, m)
        }
    }

    fn c_repeat_zero_or_one(&mut self, expr: &Repr<I>) -> ResultOrEmpty {
        let split_entry = self.insts.len();
        let split = self.push_split_hole();
        let Patch { hole: hole_rep, entry: entry_rep } = match self.c(expr)? {
            Some(p) => p,
            None => return self.pop_split_hole(),
        };
        let split_hole = self.fill_split(split, Some(entry_rep), None);
        let holes = vec![hole_rep, split_hole];
        Ok(Some(Patch { hole: Hole::Many(holes), entry: split_entry }))
    }

    fn c_repeat_zero_or_more(&mut self, expr: &Repr<I>) -> ResultOrEmpty {
        let split_entry = self.insts.len();
        let split = self.push_split_hole();
        let Patch { hole: hole_rep, entry: entry_rep } = match self.c(expr)? {
            Some(p) => p,
            None => return self.pop_split_hole(),
        };

        self.fill(hole_rep, split_entry);
        let split_hole = self.fill_split(split, Some(entry_rep), None);
        Ok(Some(Patch { hole: split_hole, entry: split_entry }))
    }

    fn c_repeat_one_or_more(&mut self, expr: &Repr<I>) -> ResultOrEmpty {
        let Patch { hole: hole_rep, entry: entry_rep } = match self.c(expr)? {
            Some(p) => p,
            None => return Ok(None),
        };
        self.fill_to_next(hole_rep);
        let split = self.push_split_hole();

        let split_hole = self.fill_split(split, Some(entry_rep), None);
        Ok(Some(Patch { hole: split_hole, entry: entry_rep }))
    }

    fn c_repeat_range_min_or_more(
        &mut self,
        expr: &Repr<I>,
        min: u32,
    ) -> ResultOrEmpty {
        let min = u32_to_usize(min);
        // Using next_inst() is ok, because we can't return it (concat would
        // have to return Some(_) while c_repeat_range_min_or_more returns
        // None).
        // let mut slf = self;
        // for i in 0..min {
        //     slf = slf.c_concat(expr.clone(), rhs)
        // }
        let patch_concat = self
            .c_concat(iter::repeat(expr).take(min))?
            .unwrap_or_else(|| self.next_inst());
        if let Some(patch_rep) = self.c_repeat_zero_or_more(expr)? {
            self.fill(patch_concat.hole, patch_rep.entry);
            Ok(Some(Patch { hole: patch_rep.hole, entry: patch_concat.entry }))
        } else {
            Ok(None)
        }
    }

    fn c_repeat_range(
        &mut self,
        expr: &Repr<I>,
        min: u32,
        max: u32,
    ) -> ResultOrEmpty {
        let (min, max) = (u32_to_usize(min), u32_to_usize(max));
        debug_assert!(min <= max);
        let patch_concat = self.c_concat(iter::repeat(expr).take(min))?;
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
        let mut holes = vec![];
        let mut prev_hole = patch_concat.hole;
        for _ in min..max {
            self.fill_to_next(prev_hole);
            let split = self.push_split_hole();
            let Patch { hole, entry } = match self.c(expr)? {
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

    fn fill(&mut self, hole: Hole, goto: InstPtr) {
        match hole {
            Hole::None => {}
            Hole::One(pc) => {
                self.insts[pc].fill(goto);
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

    fn fill_split(
        &mut self,
        hole: Hole,
        goto1: Option<InstPtr>,
        goto2: Option<InstPtr>,
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
                let mut new_holes = vec![];
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

    fn push_hole(&mut self, inst: InstHole<I>) -> Hole {
        let hole = self.insts.len();
        self.insts.push(MaybeInst::Uncompiled(inst));
        Hole::One(hole)
    }

    fn push_split_hole(&mut self) -> Hole {
        let hole = self.insts.len();
        self.insts.push(MaybeInst::Split);
        Hole::One(hole)
    }

    fn pop_split_hole(&mut self) -> ResultOrEmpty {
        self.insts.pop();
        Ok(None)
    }

    fn check_size(&self) -> result::Result<(), Error> {
        use std::mem::size_of;

        let size =
            self.extra_inst_bytes + (self.insts.len() * size_of::<Inst<I>>());
        if size > self.size_limit {
            Err(Error::CompiledTooBig(self.size_limit))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
enum Hole {
    None,
    One(InstPtr),
    Many(Vec<Hole>),
}

impl Hole {
    fn dup_one(self) -> (Self, Self) {
        match self {
            Hole::One(pc) => (Hole::One(pc), Hole::One(pc)),
            Hole::None | Hole::Many(_) =>
                unreachable!("must be called on single hole")
        }
    }
}

#[derive(Clone)]
enum MaybeInst<I: Integral> {
    Compiled(Inst<I>),
    Uncompiled(InstHole<I>),
    Split,
    Split1(InstPtr),
    Split2(InstPtr),
}

impl<I: Integral> MaybeInst<I> {
    fn fill(&mut self, goto: InstPtr) {
        let maybeinst = match *self {
            MaybeInst::Split => MaybeInst::Split1(goto),
            MaybeInst::Uncompiled(ref inst) => {
                MaybeInst::Compiled(inst.fill(goto))
            }
            MaybeInst::Split1(goto1) => {
                MaybeInst::Compiled(Inst::Split(InstSplit {
                    goto1,
                    goto2: goto,
                }))
            }
            MaybeInst::Split2(goto2) => {
                MaybeInst::Compiled(Inst::Split(InstSplit {
                    goto1: goto,
                    goto2,
                }))
            }
            _ => unreachable!(
                "not all instructions were compiled! \
                 found uncompiled instruction: {:?}",
                self
            ),
        };
        *self = maybeinst;
    }

    fn fill_split(&mut self, goto1: InstPtr, goto2: InstPtr) {
        let filled = match *self {
            MaybeInst::Split => Inst::Split(InstSplit { goto1, goto2 }),
            _ => unreachable!(
                "must be called on Split instruction, \
                 instead it was called on: {:?}",
                self
            ),
        };
        *self = MaybeInst::Compiled(filled);
    }

    fn half_fill_split_goto1(&mut self, goto1: InstPtr) {
        let half_filled = match *self {
            MaybeInst::Split => goto1,
            _ => unreachable!(
                "must be called on Split instruction, \
                 instead it was called on: {:?}",
                self
            ),
        };
        *self = MaybeInst::Split1(half_filled);
    }

    fn half_fill_split_goto2(&mut self, goto2: InstPtr) {
        let half_filled = match *self {
            MaybeInst::Split => goto2,
            _ => unreachable!(
                "must be called on Split instruction, \
                 instead it was called on: {:?}",
                self
            ),
        };
        *self = MaybeInst::Split2(half_filled);
    }

    fn unwrap(self) -> Inst<I> {
        match self {
            MaybeInst::Compiled(inst) => inst,
            _ => unreachable!(
                "must be called on a compiled instruction, \
                 instead it was called on: {:?}",
                self
            ),
        }
    }
}

#[derive(Clone)]
enum InstHole<I: Integral> {
    Zero(Zero),
    One(I),
    Interval(Interval<I>),
}

impl<I: Integral> InstHole<I> {
    fn fill(&self, goto: InstPtr) -> Inst<I> {
        match *self {
            InstHole::Zero(look) => Inst::Zero(InstZero { goto, look }),
            InstHole::One(c) => Inst::One(InstOne { goto, c }),
            InstHole::Interval(ref seq) => Inst::Interval(InstInterval { goto, seq }),
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
#[derive(Debug)]
struct SuffixCache {
    sparse: Box<[usize]>,
    dense: Vec<SuffixCacheEntry>,
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
struct SuffixCacheEntry {
    key: SuffixCacheKey,
    pc: InstPtr,
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
struct SuffixCacheKey {
    from_inst: InstPtr,
    start: u8,
    end: u8,
}

impl SuffixCache {
    fn new(size: usize) -> Self {
        SuffixCache {
            sparse: vec![0usize; size].into(),
            dense: Vec::with_capacity(size),
        }
    }

    fn get(&mut self, key: SuffixCacheKey, pc: InstPtr) -> Option<InstPtr> {
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

    fn clear(&mut self) {
        self.dense.clear();
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
