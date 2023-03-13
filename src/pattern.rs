use core::str::pattern::{Pattern, Searcher, SearchStep};

use unconst::unconst;

use crate::partition::Partition;
use crate::repr::{Repr, Integral};

#[unconst]
#[derive(Debug)]
pub struct RegexSearcher<'c, I: ~const Integral> {
    haystack: &'c str,
    it: Partition<'c, I>,
    last_step_end: usize,
    next_match: Option<(usize, usize)>,
}

#[unconst]
impl<'c, I: ~const Integral> Pattern<'c> for &'c Repr<I> {
    type Searcher = RegexSearcher<'c, I>;

    fn into_searcher(self, haystack: &'c str) -> RegexSearcher<'c, I> {
        RegexSearcher {
            haystack,
            it: self.find_iter(haystack),
            last_step_end: 0,
            next_match: None,
        }
    }
}

// unsafe impl<'r, 't> Searcher<'t> for RegexSearcher<'r, 't> {
//     #[inline]
//     fn haystack(&self) -> &'t str {
//         self.haystack
//     }

//     #[inline]
//     fn next(&mut self) -> SearchStep {
//         if let Some((s, e)) = self.next_match {
//             self.next_match = None;
//             self.last_step_end = e;
//             return SearchStep::Match(s, e);
//         }
//         match self.it.next() {
//             None => {
//                 if self.last_step_end < self.haystack().len() {
//                     let last = self.last_step_end;
//                     self.last_step_end = self.haystack().len();
//                     SearchStep::Reject(last, self.haystack().len())
//                 } else {
//                     SearchStep::Done
//                 }
//             }
//             Some(m) => {
//                 let (s, e) = (m.start(), m.end());
//                 if s == self.last_step_end {
//                     self.last_step_end = e;
//                     SearchStep::Match(s, e)
//                 } else {
//                     self.next_match = Some((s, e));
//                     let last = self.last_step_end;
//                     self.last_step_end = s;
//                     SearchStep::Reject(last, s)
//                 }
//             }
//         }
//     }
// }

