/*!
This module implements the Pike VM. That is, it guarantees linear time
search of a regex on any text with memory use proportional to the size of
the regex.

It is equal in power to the backtracking engine in this crate, except the
backtracking engine is typically faster on small regexes/texts at the
expense of a bigger memory footprint.

It can do more than the DFA can (specifically, record capture locations
and execute Unicode word boundary assertions), but at a slower speed.
Specifically, the Pike VM executes a DFA implicitly by repeatedly expanding
epsilon transitions. That is, the Pike VM engine can be in multiple states
at once where as the DFA is only ever in one state at a time.

Therefore, the Pike VM is generally treated as the fallback when the other
matching engines either aren't feasible to run or are insufficient.
*/

use core::mem;

use unconst::unconst;

use crate::repr::Integral;
use crate::context::Context;
use crate::exec::ProgramCache;
use crate::program::{Index, Program, Inst};
use crate::sparse::SparseSet;

/// An NFA simulation matching engine.
#[derive(Debug)]
pub struct Fsm<'r, I: Integral> {
    /// The sequence of opcodes (among other things) that is actually executed.
    ///
    /// The program may be byte oriented or Unicode codepoint oriented.
    prog: &'r Program<I>,
    /// An explicit stack used for following epsilon transitions. (This is
    /// borrowed from the cache.)
    stack: &'r mut Vec<Index>,
    /// The context to search.
    context: Context<I>,
}

/// A cached allocation that can be reused on each execution.
#[derive(Clone, Debug)]
pub struct Cache {
    /// A pair of ordered sets of opcodes (each opcode is an NFA state) for tracking NFA states.
    clist: SparseSet,
    nlist: SparseSet,
    /// An explicit stack used for following epsilon transitions.
    /// A representation of an explicit stack frame when following epsilon
    /// transitions. This is used to avoid recursion.
    /// Follow transitions at the given instruction pointer.
    stack: Vec<Index>,
}

#[unconst]
impl Cache {
    /// Create a new allocation used by the NFA machine to record execution
    /// and captures.
    pub const fn new<I: Integral>(_prog: &Program<I>) -> Self {
        Cache {
            clist: SparseSet::new(0),
            nlist: SparseSet::new(0),
            stack: Vec::new()
        }
    }
}

#[unconst]
impl<'r, I: ~const Integral> Fsm<'r, I> {
    /// Execute the NFA matching engine.
    ///
    /// If there's a match, `exec` returns `true` and populates the given
    /// captures accordingly.
    pub fn exec(
        prog: &'r Program<I>,
        cache: &ProgramCache<I>,
        matches: &mut [bool],
        quit_after_match: bool,
        context: Context<I>,
        start: usize,
        end: usize,
    ) -> bool {
        let mut cache = cache.borrow_mut();
        let cache = &mut cache.pikevm;
        cache.clist.resize(prog.len());
        cache.nlist.resize(prog.len());
        let at = context[start];
        Fsm { prog, stack: &mut cache.stack, context }.exec_(
            &mut cache.clist,
            &mut cache.nlist,
            matches,
            quit_after_match,
            at,
            end,
        )
    }

    fn exec_(
        &mut self,
        mut clist: &mut SparseSet,
        mut nlist: &mut SparseSet,
        matches: &mut [bool],
        quit_after_match: bool,
        mut at: I,
        end: usize,
    ) -> bool {
        let mut matched = false;
        let mut all_matched = false;
        clist.clear();
        nlist.clear();
        'LOOP: loop {
            if clist.is_empty() {
                // Three ways to bail out when our current set of threads is
                // empty.
                //
                // 1. We have a match---so we're done exploring any possible
                //    alternatives. Time to quit. (We can't do this if we're
                //    looking for matches for multiple regexes, unless we know
                //    they all matched.)
                //
                // 2. If the expression starts with a '^' we can terminate as
                //    soon as the last thread dies.
                if (matched && matches.len() <= 1) || all_matched {
                    break;
                }

                // 3. If there's a literal prefix for the program, try to
                //    jump ahead quickly. If it can't be found, then we can
                //    bail out early.
                if !self.prog.prefixes.is_empty() {
                    at = match self.context.prefix_at(&self.prog.prefixes, at) {
                        None => break,
                        Some(at) => at,
                    };
                }
            }

            // This simulates a preceding '.*?' for every regex by adding
            // a state starting at the current position in the input for the
            // beginning of the program only if we don't already have a match.
            if clist.is_empty()
                || (!self.prog.is_anchored_start && !all_matched)
            {
                self.add(&mut clist, 0, at);
            }
            // The previous call to "add" actually inspects the position just
            // before the current character. For stepping through the machine,
            // we can to look at the current character, so we advance the
            // input.
            let at_next = self.context[at + 1];
            for i in 0..clist.len() {
                let ip = clist[i];
                if self.step(&mut nlist, matches, ip, at, at_next) {
                    matched = true;
                    all_matched = all_matched || matches.iter().all(|&b| b);
                    if quit_after_match {
                        // If we only care if a match occurs (not its
                        // position), then we can quit right now.
                        break 'LOOP;
                    }
                    if self.prog.matches.len() == 1 {
                        // We don't need to check the rest of the threads
                        // in this set because we've matched something
                        // ("leftmost-first"). However, we still need to check
                        // threads in the next set to support things like
                        // greedy matching.
                        //
                        // This is only true on normal regexes. For regex sets,
                        // we need to mush on to observe other matches.
                        break;
                    }
                }
            }
            if at.pos() >= end {
                break;
            }
            at = at_next;
            mem::swap(clist, nlist);
            nlist.clear();
        }
        matched
    }

    /// Step through the input, one token (byte or codepoint) at a time.
    ///
    /// nlist is the set of states that will be processed on the next token
    /// in the input.
    ///
    /// caps is the set of captures passed by the caller of the NFA. They are
    /// written to only when a match state is visited.
    ///
    /// thread_caps is the set of captures set for the current NFA state, ip.
    ///
    /// at and at_next are the current and next positions in the input. at or
    /// at_next may be EOF.
    fn step(
        &mut self,
        nlist: &mut SparseSet,
        matches: &mut [bool],
        ip: usize,
        at: I,
        at_next: I,
    ) -> bool {
        match self.prog[ip] {
            Inst::Match(match_slot) => {
                if match_slot < matches.len() {
                    matches[match_slot] = true;
                }
                true
            }
            Inst::One(ref inst) => {
                if inst.c == at.char() {
                    self.add(nlist, inst.goto, at_next);
                }
                false
            }
            Inst::Interval(ref inst) => {
                if inst.matches(at.char()) {
                    self.add(nlist, inst.goto, at_next);
                }
                false
            }
            Inst::Zero(_) | Inst::Split(_) => false,
        }
    }

    /// Follows epsilon transitions and adds them for processing to nlist,
    /// starting at and including ip.
    fn add(&mut self, nlist: &mut SparseSet, ip: Index, at: I) {
        self.stack.push(ip);
        while let Some(ip) = self.stack.pop() {
            self.add_step(nlist, ip, at);
        }
    }

    /// A helper function for add that avoids excessive pushing to the stack.
    fn add_step(&mut self, nlist: &mut SparseSet, mut ip: usize, at: I) {
        // Instead of pushing and popping to the stack, we mutate ip as we
        // traverse the set of states. We only push to the stack when we
        // absolutely need recursion (restoring captures or following a
        // branch).
        loop {
            // Don't visit states we've already added.
            if nlist.contains(ip) {
                return;
            }
            nlist.insert(ip);
            match self.prog[ip] {
                Inst::Zero { goto, look } => {
                    if self.context.is_empty_match(at, look) {
                        ip = goto;
                    }
                }
                Inst::Split { goto1, goto2 } => {
                    self.stack.push(goto2);
                    ip = goto1;
                }
                Inst::Match(_) | Inst::One(_) | Inst::Interval(_) => {
                    return;
                }
            }
        }
    }
}
