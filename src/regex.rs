use regex::{Captures, Regex};
use unconst::unconst;

use crate::{wrappers::one, Repr};

#[unconst]
impl Repr<char> {
    pub fn captures<'h>(&self, haystack: &'h str) -> Option<Captures<'h>> {
        self.to_regex().captures(haystack)
    }

    pub fn to_regex(&self) -> Regex {
        Regex::new(&format!("^{}$", self.to_regex_string())).unwrap()
    }

    pub fn to_regex_string(&self) -> String {
        match self {
            Repr::Seq(s) => match s.as_ref() {
                ['+']
                | ['*']
                | ['?']
                | ['.']
                | ['(']
                | [')']
                | ['[']
                | [']']
                | ['{']
                | ['}']
                | ['^']
                | ['$']
                | ['|']
                | ['\\'] => format!("\\{}", s[0]),
                _ => s.iter().collect(),
            },
            Repr::Interval(i) => format!("[{}-{}]", i.0, i.1),
            Repr::Mul(lhs, rhs) => format!("{}{}", lhs.to_regex_string(), rhs.to_regex_string()),
            Repr::Or(lhs, rhs) if lhs.as_ref() == &one() => {
                format!("({})?", rhs.to_regex_string())
            }
            Repr::Or(lhs, rhs) => format!("({}|{})", lhs.to_regex_string(), rhs.to_regex_string()),
            Repr::Inf(r) => format!("{}*", r.to_regex_string()),
            Repr::Cap(r) => format!("({})", r.to_regex_string()),
            _ => unimplemented!(),
        }
    }
}
