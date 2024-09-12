use regex::Regex;
use unconst::unconst;

use crate::Repr;

#[unconst]
impl Repr<char> {
    pub fn to_regex(&self) -> Regex {
        Regex::new(&self.to_regex_string()).unwrap()
    }

    pub fn to_regex_string(&self) -> String {
        match self {
            Repr::One(s) => s.iter().collect(),
            Repr::Interval(i) => format!("[{}-{}]", i.0, i.1),
            Repr::Mul(lhs, rhs) => format!("{}{}", lhs.to_regex_string(), rhs.to_regex_string()),
            Repr::Or(lhs, rhs) => format!("{}|{}", lhs.to_regex_string(), rhs.to_regex_string()),
            Repr::Inf(r) => format!("{}*", r.to_regex_string()),
            Repr::Cap(r) => format!("({})", r.to_regex_string()),
            _ => unimplemented!(),
        }
    }
}
