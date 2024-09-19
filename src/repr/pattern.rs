use core::str::pattern::{Pattern, Searcher, SearchStep};

use crate::partition::Partition;
use crate::repr::Repr;

#[derive(Debug)]
pub struct RegexSearcher<'c> {
    context: &'c str,
    it: Partition<'c, char>,
    last_step_end: usize,
    next_match: Option<(usize, usize)>,
}

impl<'c> Pattern<'c> for &'c Repr<char> {
    type Searcher = RegexSearcher<'c>;

    fn into_searcher(self, context: &'c str) -> Self::Searcher {
        RegexSearcher {
            context,
            it: self.find_iter(context),
            last_step_end: 0,
            next_match: None,
        }
    }
}

unsafe impl<'c> Searcher<'c> for RegexSearcher<'c> {
    #[inline]
    fn haystack(&self) -> &'c str {
        self.context
    }

    #[inline]
    fn next(&mut self) -> SearchStep {
        if let Some((s, e)) = self.next_match {
            self.next_match = None;
            self.last_step_end = e;
            return SearchStep::Match(s, e);
        }
        match self.it.next() {
            None => {
                if self.last_step_end < self.context.len() {
                    let last = self.last_step_end;
                    self.last_step_end = self.context.len();
                    SearchStep::Reject(last, self.context.len())
                } else {
                    SearchStep::Done
                }
            }
            Some((start, end)) => {
                if start == self.last_step_end {
                    self.last_step_end = end;
                    SearchStep::Match(start, end)
                } else {
                    self.next_match = Some((start, end));
                    let last = self.last_step_end;
                    self.last_step_end = start;
                    SearchStep::Reject(last, start)
                }
            }
        }
    }
}

