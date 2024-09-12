use regex::Regex;
use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Hir, Repetition};
use unconst::unconst;

use crate::Repr;

#[unconst]
impl Repr<u8> {
    pub fn to_regex(&self) -> Regex {
        Regex::new(&format!("{}", self.to_regex_hir())).unwrap()
    }

    pub fn to_regex_hir(&self) -> Hir {
        match self {
            Repr::One(s) => Hir::literal(s.to_vec().into_boxed_slice()),
            Repr::Interval(i) => Hir::class(Class::Bytes(ClassBytes::new([ClassBytesRange::new(
                i.0, i.1,
            )]))),
            Repr::Mul(lhs, rhs) => Hir::concat(vec![lhs.to_regex_hir(), rhs.to_regex_hir()]),
            Repr::Or(lhs, rhs) => Hir::alternation(vec![lhs.to_regex_hir(), rhs.to_regex_hir()]),
            Repr::Inf(r) => Hir::repetition(Repetition {
                min: 0,
                max: None,
                greedy: true,
                sub: Box::new(r.to_regex_hir()),
            }),
            _ => unimplemented!(),
        }
    }
}
