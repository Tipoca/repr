use core::fmt::{self, Debug};
use core::mem;
use core::ops::Deref;
use core::slice;

use unconst::unconst;

use crate::derivative::LiteralSearcher;
use crate::repr::{Integral};

/// `Index` represents the index of an instruction in a regex program.
pub type Index = usize;

/// Program is a sequence of instructions and various facts about those
/// instructions.
#[derive(Clone)]
pub struct Program<I: Integral> {
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
}

#[unconst]
impl<I: ~const Integral> Program<I> {
    /// Creates an empty instruction sequence. Fields are given default
    /// values.
    pub fn new() -> Self {
        Program {
            insts: vec![],
            matches: vec![],
            start: 0,
            // byte_classes: vec![0; 256],
            is_anchored_start: false,
            is_anchored_end: false,
            has_unicode_word_boundary: false,
            prefixes: LiteralSearcher::empty(),
            dfa_size_limit: 2 * (1 << 20),
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
        (self.len() * mem::size_of::<Inst<I>>())
            + (self.matches.len() * mem::size_of::<Index>())
            + (256 * mem::size_of::<u8>())
            + self.prefixes.approximate_size()
    }
}

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
                Inst::Split { goto1, goto2 } => {
                    write!(f, "{:04} Split({}, {})", pc, goto1, goto2)?;
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
