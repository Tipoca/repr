// #[cfg(test)]
// mod tests {
//     use std::fmt;

//     use super::{escape_bytes, Literal, Literals};
//     use crate::repr::Repr;

//     // To make test failures easier to read.
//     #[derive(Debug, Eq, PartialEq)]
//     struct Bytes(Vec<ULiteral>);
//     #[derive(Debug, Eq, PartialEq)]
//     struct Unicode(Vec<ULiteral>);

//     fn escape_lits(blits: &[Literal]) -> Vec<ULiteral> {
//         let mut ulits = vec![];
//         for blit in blits {
//             ulits
//                 .push(ULiteral { v: escape_bytes(&blit), cut: blit.cut });
//         }
//         ulits
//     }

//     fn create_lits<I: IntoIterator<Item = Literal>>(it: I) -> Literals {
//         Literals {
//             lits: it.into_iter().collect(),
//             limit_size: 0,
//             limit_class: 0,
//         }
//     }

//     // Needs to be pub for 1.3?
//     #[derive(Clone, Eq, PartialEq)]
//     pub struct ULiteral {
//         v: String,
//         cut: bool,
//     }

//     impl ULiteral {
//         fn is_cut(&self) -> bool {
//             self.cut
//         }
//     }

//     impl Debug for ULiteral {
//         fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//             if self.cut {
//                 write!(f, "Cut({})", self.v)
//             } else {
//                 write!(f, "Complete({})", self.v)
//             }
//         }
//     }

//     impl PartialEq<Literal> for ULiteral {
//         fn eq(&self, other: &Literal) -> bool {
//             self.v.as_bytes() == &*other.v && self.cut == other.cut
//         }
//     }

//     impl PartialEq<ULiteral> for Literal {
//         fn eq(&self, other: &ULiteral) -> bool {
//             &*self.v == other.v.as_bytes() && self.cut == other.cut
//         }
//     }

//     #[allow(non_snake_case)]
//     fn C(s: &'static str) -> ULiteral {
//         ULiteral { v: s.to_owned(), cut: true }
//     }
//     #[allow(non_snake_case)]
//     fn M(s: &'static str) -> ULiteral {
//         ULiteral { v: s.to_owned(), cut: false }
//     }

//     macro_rules! assert_lit_eq {
//         ($which:ident, $got_lits:expr, $($expected_lit:expr),*) => {{
//             let expected: Vec<ULiteral> = vec![$($expected_lit),*];
//             let lits = $got_lits;
//             assert_eq!(
//                 $which(expected.clone()),
//                 $which(escape_lits(lits.literals())));
//             assert_eq!(
//                 !expected.is_empty() && expected.iter().all(|l| !l.cut),
//                 lits.all_complete());
//             assert_eq!(
//                 expected.iter().any(|l| !l.cut),
//                 lits.any_complete());
//         }};
//     }

//     macro_rules! test_lit {
//         ($name:ident, $which:ident, $re:expr) => {
//             test_lit!($name, $which, $re,);
//         };
//         ($name:ident, $which:ident, $re:expr, $($lit:expr),*) => {
//             #[test]
//             fn $name() {
//                 let expr = ParserBuilder::new()
//                     .build()
//                     .parse($re)
//                     .unwrap();
//                 let lits = Literals::$which(&expr);
//                 assert_lit_eq!(Unicode, lits, $($lit),*);

//                 let expr = ParserBuilder::new()
//                     .allow_invalid_utf8(true)
//                     .unicode(false)
//                     .build()
//                     .parse($re)
//                     .unwrap();
//                 let lits = Literals::$which(&expr);
//                 assert_lit_eq!(Bytes, lits, $($lit),*);
//             }
//         };
//     }

//     // ************************************************************************
//     // Tests for prefix literal extraction.
//     // ************************************************************************

//     // Elementary tests.
//     test_lit!(pfx_one_lit1, prefixes, "a", M("a"));
//     test_lit!(pfx_one_lit2, prefixes, "abc", M("abc"));
//     test_lit!(pfx_one_lit3, prefixes, "(?u)☃", M("\\xe2\\x98\\x83"));
//     #[cfg(feature = "unicode-case")]
//     test_lit!(pfx_one_lit4, prefixes, "(?ui)☃", M("\\xe2\\x98\\x83"));
//     test_lit!(pfx_class1, prefixes, "[1-4]", M("1"), M("2"), M("3"), M("4"));
//     test_lit!(
//         pfx_class2,
//         prefixes,
//         "(?u)[☃Ⅰ]",
//         M("\\xe2\\x85\\xa0"),
//         M("\\xe2\\x98\\x83")
//     );
//     #[cfg(feature = "unicode-case")]
//     test_lit!(
//         pfx_class3,
//         prefixes,
//         "(?ui)[☃Ⅰ]",
//         M("\\xe2\\x85\\xa0"),
//         M("\\xe2\\x85\\xb0"),
//         M("\\xe2\\x98\\x83")
//     );
//     test_lit!(pfx_one_lit_casei1, prefixes, "(?i-u)a", M("A"), M("a"));
//     test_lit!(
//         pfx_one_lit_casei2,
//         prefixes,
//         "(?i-u)abc",
//         M("ABC"),
//         M("aBC"),
//         M("AbC"),
//         M("abC"),
//         M("ABc"),
//         M("aBc"),
//         M("Abc"),
//         M("abc")
//     );
//     test_lit!(pfx_group1, prefixes, "(a)", M("a"));
//     test_lit!(pfx_rep_zero_or_one1, prefixes, "a?");
//     test_lit!(pfx_rep_zero_or_one2, prefixes, "(?:abc)?");
//     test_lit!(pfx_rep_zero_or_one_cat1, prefixes, "ab?", C("ab"), M("a"));
//     // FIXME: This should return [M("a"), M("ab")] because of the non-greedy
//     // repetition. As a work-around, we rewrite ab?? as ab*?, and thus we get
//     // a cut literal.
//     test_lit!(pfx_rep_zero_or_one_cat2, prefixes, "ab??", C("ab"), M("a"));
//     test_lit!(pfx_rep_zero_or_more1, prefixes, "a*");
//     test_lit!(pfx_rep_zero_or_more2, prefixes, "(?:abc)*");
//     test_lit!(pfx_rep_one_or_more1, prefixes, "a+", C("a"));
//     test_lit!(pfx_rep_one_or_more2, prefixes, "(?:abc)+", C("abc"));
//     test_lit!(pfx_rep_nested_one_or_more, prefixes, "(?:a+)+", C("a"));
//     test_lit!(pfx_rep_range1, prefixes, "a{0}");
//     test_lit!(pfx_rep_range2, prefixes, "a{0,}");
//     test_lit!(pfx_rep_range3, prefixes, "a{0,1}");
//     test_lit!(pfx_rep_range4, prefixes, "a{1}", M("a"));
//     test_lit!(pfx_rep_range5, prefixes, "a{2}", M("aa"));
//     test_lit!(pfx_rep_range6, prefixes, "a{1,2}", C("a"));
//     test_lit!(pfx_rep_range7, prefixes, "a{2,3}", C("aa"));

//     // Test regexes with concatenations.
//     test_lit!(pfx_cat1, prefixes, "(?:a)(?:b)", M("ab"));
//     test_lit!(pfx_cat2, prefixes, "[ab]z", M("az"), M("bz"));
//     test_lit!(
//         pfx_cat3,
//         prefixes,
//         "(?i-u)[ab]z",
//         M("AZ"),
//         M("BZ"),
//         M("aZ"),
//         M("bZ"),
//         M("Az"),
//         M("Bz"),
//         M("az"),
//         M("bz")
//     );
//     test_lit!(
//         pfx_cat4,
//         prefixes,
//         "[ab][yz]",
//         M("ay"),
//         M("by"),
//         M("az"),
//         M("bz")
//     );
//     test_lit!(pfx_cat5, prefixes, "a*b", C("a"), M("b"));
//     test_lit!(pfx_cat6, prefixes, "a*b*c", C("a"), C("b"), M("c"));
//     test_lit!(pfx_cat7, prefixes, "a*b*c+", C("a"), C("b"), C("c"));
//     test_lit!(pfx_cat8, prefixes, "a*b+c", C("a"), C("b"));
//     test_lit!(pfx_cat9, prefixes, "a*b+c*", C("a"), C("b"));
//     test_lit!(pfx_cat10, prefixes, "ab*", C("ab"), M("a"));
//     test_lit!(pfx_cat11, prefixes, "ab*c", C("ab"), M("ac"));
//     test_lit!(pfx_cat12, prefixes, "ab+", C("ab"));
//     test_lit!(pfx_cat13, prefixes, "ab+c", C("ab"));
//     test_lit!(pfx_cat14, prefixes, "a^", C("a"));
//     test_lit!(pfx_cat15, prefixes, "$a");
//     test_lit!(pfx_cat16, prefixes, r"ab*c", C("ab"), M("ac"));
//     test_lit!(pfx_cat17, prefixes, r"ab+c", C("ab"));
//     test_lit!(pfx_cat18, prefixes, r"z*azb", C("z"), M("azb"));
//     test_lit!(pfx_cat19, prefixes, "a.z", C("a"));

//     // Test regexes with alternations.
//     test_lit!(pfx_alt1, prefixes, "a|b", M("a"), M("b"));
//     test_lit!(pfx_alt2, prefixes, "[1-3]|b", M("1"), M("2"), M("3"), M("b"));
//     test_lit!(pfx_alt3, prefixes, "y(?:a|b)z", M("yaz"), M("ybz"));
//     test_lit!(pfx_alt4, prefixes, "a|b*");
//     test_lit!(pfx_alt5, prefixes, "a|b+", M("a"), C("b"));
//     test_lit!(pfx_alt6, prefixes, "a|(?:b|c*)");
//     test_lit!(
//         pfx_alt7,
//         prefixes,
//         "(a|b)*c|(a|ab)*c",
//         C("a"),
//         C("b"),
//         M("c"),
//         C("a"),
//         C("ab"),
//         M("c")
//     );
//     test_lit!(pfx_alt8, prefixes, "a*b|c", C("a"), M("b"), M("c"));

//     // Test regexes with empty assertions.
//     test_lit!(pfx_empty1, prefixes, "^a", M("a"));
//     test_lit!(pfx_empty2, prefixes, "a${2}", C("a"));
//     test_lit!(pfx_empty3, prefixes, "^abc", M("abc"));
//     test_lit!(pfx_empty4, prefixes, "(?:^abc)|(?:^z)", M("abc"), M("z"));

//     // Make sure some curious regexes have no prefixes.
//     test_lit!(pfx_nothing1, prefixes, ".");
//     test_lit!(pfx_nothing2, prefixes, "(?s).");
//     test_lit!(pfx_nothing3, prefixes, "^");
//     test_lit!(pfx_nothing4, prefixes, "$");
//     test_lit!(pfx_nothing6, prefixes, "(?m)$");
//     test_lit!(pfx_nothing7, prefixes, r"\b");
//     test_lit!(pfx_nothing8, prefixes, r"\B");

//     // Test a few regexes that defeat any prefix literal detection.
//     test_lit!(pfx_defeated1, prefixes, ".a");
//     test_lit!(pfx_defeated2, prefixes, "(?s).a");
//     test_lit!(pfx_defeated3, prefixes, "a*b*c*");
//     test_lit!(pfx_defeated4, prefixes, "a|.");
//     test_lit!(pfx_defeated5, prefixes, ".|a");
//     test_lit!(pfx_defeated6, prefixes, "a|^");
//     test_lit!(pfx_defeated7, prefixes, ".(?:a(?:b)(?:c))");
//     test_lit!(pfx_defeated8, prefixes, "$a");
//     test_lit!(pfx_defeated9, prefixes, "(?m)$a");
//     test_lit!(pfx_defeated10, prefixes, r"\ba");
//     test_lit!(pfx_defeated11, prefixes, r"\Ba");
//     test_lit!(pfx_defeated12, prefixes, "^*a");
//     test_lit!(pfx_defeated13, prefixes, "^+a");

//     test_lit!(
//         pfx_crazy1,
//         prefixes,
//         r"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]",
//         C("Mo\\'"),
//         C("Mu\\'"),
//         C("Moam"),
//         C("Muam")
//     );

//     // ************************************************************************
//     // Tests for quiting prefix literal search.
//     // ************************************************************************

//     macro_rules! test_exhausted {
//         ($name:ident, $which:ident, $re:expr) => {
//             test_exhausted!($name, $which, $re,);
//         };
//         ($name:ident, $which:ident, $re:expr, $($lit:expr),*) => {
//             #[test]
//             fn $name() {
//                 let expr = ParserBuilder::new()
//                     .build()
//                     .parse($re)
//                     .unwrap();
//                 let mut lits = Literals {
//                     lits: vec![],
//                     limit_size: 20,
//                     limit_class: 10,
//                 };
//                 $which(&mut lits, &expr);
//                 assert_lit_eq!(Unicode, lits, $($lit),*);

//                 let expr = ParserBuilder::new()
//                     .allow_invalid_utf8(true)
//                     .unicode(false)
//                     .build()
//                     .parse($re)
//                     .unwrap();
//                 let mut lits = Literals {
//                     lits: vec![],
//                     limit_size: 20,
//                     limit_class: 10,
//                 };
//                 $which(&mut lits, &expr);
//                 assert_lit_eq!(Bytes, lits, $($lit),*);
//             }
//         };
//     }

//     // These test use a much lower limit than the default so that we can
//     // write test cases of reasonable size.
//     test_exhausted!(pfx_exhausted1, prefixes, "[a-z]");
//     test_exhausted!(pfx_exhausted2, prefixes, "[a-z]*A");
//     test_exhausted!(pfx_exhausted3, prefixes, "A[a-z]Z", C("A"));
//     test_exhausted!(
//         pfx_exhausted4,
//         prefixes,
//         "(?i-u)foobar",
//         C("FO"),
//         C("fO"),
//         C("Fo"),
//         C("fo")
//     );
//     test_exhausted!(
//         pfx_exhausted5,
//         prefixes,
//         "(?:ab){100}",
//         C("abababababababababab")
//     );
//     test_exhausted!(
//         pfx_exhausted6,
//         prefixes,
//         "(?:(?:ab){100})*cd",
//         C("ababababab"),
//         M("cd")
//     );
//     test_exhausted!(
//         pfx_exhausted7,
//         prefixes,
//         "z(?:(?:ab){100})*cd",
//         C("zababababab"),
//         M("zcd")
//     );
//     test_exhausted!(
//         pfx_exhausted8,
//         prefixes,
//         "aaaaaaaaaaaaaaaaaaaaz",
//         C("aaaaaaaaaaaaaaaaaaaa")
//     );

//     // ************************************************************************
//     // Tests for suffix literal extraction.
//     // ************************************************************************

//     // Elementary tests.
//     test_lit!(sfx_one_lit1, suffixes, "a", M("a"));
//     test_lit!(sfx_one_lit2, suffixes, "abc", M("abc"));
//     test_lit!(sfx_one_lit3, suffixes, "(?u)☃", M("\\xe2\\x98\\x83"));
//     #[cfg(feature = "unicode-case")]
//     test_lit!(sfx_one_lit4, suffixes, "(?ui)☃", M("\\xe2\\x98\\x83"));
//     test_lit!(sfx_class1, suffixes, "[1-4]", M("1"), M("2"), M("3"), M("4"));
//     test_lit!(
//         sfx_class2,
//         suffixes,
//         "(?u)[☃Ⅰ]",
//         M("\\xe2\\x85\\xa0"),
//         M("\\xe2\\x98\\x83")
//     );
//     #[cfg(feature = "unicode-case")]
//     test_lit!(
//         sfx_class3,
//         suffixes,
//         "(?ui)[☃Ⅰ]",
//         M("\\xe2\\x85\\xa0"),
//         M("\\xe2\\x85\\xb0"),
//         M("\\xe2\\x98\\x83")
//     );
//     test_lit!(sfx_one_lit_casei1, suffixes, "(?i-u)a", M("A"), M("a"));
//     test_lit!(
//         sfx_one_lit_casei2,
//         suffixes,
//         "(?i-u)abc",
//         M("ABC"),
//         M("ABc"),
//         M("AbC"),
//         M("Abc"),
//         M("aBC"),
//         M("aBc"),
//         M("abC"),
//         M("abc")
//     );
//     test_lit!(sfx_group1, suffixes, "(a)", M("a"));
//     test_lit!(sfx_rep_zero_or_one1, suffixes, "a?");
//     test_lit!(sfx_rep_zero_or_one2, suffixes, "(?:abc)?");
//     test_lit!(sfx_rep_zero_or_more1, suffixes, "a*");
//     test_lit!(sfx_rep_zero_or_more2, suffixes, "(?:abc)*");
//     test_lit!(sfx_rep_one_or_more1, suffixes, "a+", C("a"));
//     test_lit!(sfx_rep_one_or_more2, suffixes, "(?:abc)+", C("abc"));
//     test_lit!(sfx_rep_nested_one_or_more, suffixes, "(?:a+)+", C("a"));
//     test_lit!(sfx_rep_range1, suffixes, "a{0}");
//     test_lit!(sfx_rep_range2, suffixes, "a{0,}");
//     test_lit!(sfx_rep_range3, suffixes, "a{0,1}");
//     test_lit!(sfx_rep_range4, suffixes, "a{1}", M("a"));
//     test_lit!(sfx_rep_range5, suffixes, "a{2}", M("aa"));
//     test_lit!(sfx_rep_range6, suffixes, "a{1,2}", C("a"));
//     test_lit!(sfx_rep_range7, suffixes, "a{2,3}", C("aa"));

//     // Test regexes with concatenations.
//     test_lit!(sfx_cat1, suffixes, "(?:a)(?:b)", M("ab"));
//     test_lit!(sfx_cat2, suffixes, "[ab]z", M("az"), M("bz"));
//     test_lit!(
//         sfx_cat3,
//         suffixes,
//         "(?i-u)[ab]z",
//         M("AZ"),
//         M("Az"),
//         M("BZ"),
//         M("Bz"),
//         M("aZ"),
//         M("az"),
//         M("bZ"),
//         M("bz")
//     );
//     test_lit!(
//         sfx_cat4,
//         suffixes,
//         "[ab][yz]",
//         M("ay"),
//         M("az"),
//         M("by"),
//         M("bz")
//     );
//     test_lit!(sfx_cat5, suffixes, "a*b", C("ab"), M("b"));
//     test_lit!(sfx_cat6, suffixes, "a*b*c", C("bc"), C("ac"), M("c"));
//     test_lit!(sfx_cat7, suffixes, "a*b*c+", C("c"));
//     test_lit!(sfx_cat8, suffixes, "a*b+c", C("bc"));
//     test_lit!(sfx_cat9, suffixes, "a*b+c*", C("c"), C("b"));
//     test_lit!(sfx_cat10, suffixes, "ab*", C("b"), M("a"));
//     test_lit!(sfx_cat11, suffixes, "ab*c", C("bc"), M("ac"));
//     test_lit!(sfx_cat12, suffixes, "ab+", C("b"));
//     test_lit!(sfx_cat13, suffixes, "ab+c", C("bc"));
//     test_lit!(sfx_cat14, suffixes, "a^");
//     test_lit!(sfx_cat15, suffixes, "$a", C("a"));
//     test_lit!(sfx_cat16, suffixes, r"ab*c", C("bc"), M("ac"));
//     test_lit!(sfx_cat17, suffixes, r"ab+c", C("bc"));
//     test_lit!(sfx_cat18, suffixes, r"z*azb", C("zazb"), M("azb"));
//     test_lit!(sfx_cat19, suffixes, "a.z", C("z"));

//     // Test regexes with alternations.
//     test_lit!(sfx_alt1, suffixes, "a|b", M("a"), M("b"));
//     test_lit!(sfx_alt2, suffixes, "[1-3]|b", M("1"), M("2"), M("3"), M("b"));
//     test_lit!(sfx_alt3, suffixes, "y(?:a|b)z", M("yaz"), M("ybz"));
//     test_lit!(sfx_alt4, suffixes, "a|b*");
//     test_lit!(sfx_alt5, suffixes, "a|b+", M("a"), C("b"));
//     test_lit!(sfx_alt6, suffixes, "a|(?:b|c*)");
//     test_lit!(
//         sfx_alt7,
//         suffixes,
//         "(a|b)*c|(a|ab)*c",
//         C("ac"),
//         C("bc"),
//         M("c"),
//         C("ac"),
//         C("abc"),
//         M("c")
//     );
//     test_lit!(sfx_alt8, suffixes, "a*b|c", C("ab"), M("b"), M("c"));

//     // Test regexes with empty assertions.
//     test_lit!(sfx_empty1, suffixes, "a$", M("a"));
//     test_lit!(sfx_empty2, suffixes, "${2}a", C("a"));

//     // Make sure some curious regexes have no suffixes.
//     test_lit!(sfx_nothing1, suffixes, ".");
//     test_lit!(sfx_nothing2, suffixes, "(?s).");
//     test_lit!(sfx_nothing3, suffixes, "^");
//     test_lit!(sfx_nothing4, suffixes, "$");
//     test_lit!(sfx_nothing6, suffixes, "(?m)$");
//     test_lit!(sfx_nothing7, suffixes, r"\b");
//     test_lit!(sfx_nothing8, suffixes, r"\B");

//     // Test a few regexes that defeat any suffix literal detection.
//     test_lit!(sfx_defeated1, suffixes, "a.");
//     test_lit!(sfx_defeated2, suffixes, "(?s)a.");
//     test_lit!(sfx_defeated3, suffixes, "a*b*c*");
//     test_lit!(sfx_defeated4, suffixes, "a|.");
//     test_lit!(sfx_defeated5, suffixes, ".|a");
//     test_lit!(sfx_defeated6, suffixes, "a|^");
//     test_lit!(sfx_defeated7, suffixes, "(?:a(?:b)(?:c)).");
//     test_lit!(sfx_defeated8, suffixes, "a^");
//     test_lit!(sfx_defeated9, suffixes, "(?m)a$");
//     test_lit!(sfx_defeated10, suffixes, r"a\b");
//     test_lit!(sfx_defeated11, suffixes, r"a\B");
//     test_lit!(sfx_defeated12, suffixes, "a^*");
//     test_lit!(sfx_defeated13, suffixes, "a^+");

//     // These test use a much lower limit than the default so that we can
//     // write test cases of reasonable size.
//     test_exhausted!(sfx_exhausted1, suffixes, "[a-z]");
//     test_exhausted!(sfx_exhausted2, suffixes, "A[a-z]*");
//     test_exhausted!(sfx_exhausted3, suffixes, "A[a-z]Z", C("Z"));
//     test_exhausted!(
//         sfx_exhausted4,
//         suffixes,
//         "(?i-u)foobar",
//         C("AR"),
//         C("Ar"),
//         C("aR"),
//         C("ar")
//     );
//     test_exhausted!(
//         sfx_exhausted5,
//         suffixes,
//         "(?:ab){100}",
//         C("abababababababababab")
//     );
//     test_exhausted!(
//         sfx_exhausted6,
//         suffixes,
//         "cd(?:(?:ab){100})*",
//         C("ababababab"),
//         M("cd")
//     );
//     test_exhausted!(
//         sfx_exhausted7,
//         suffixes,
//         "cd(?:(?:ab){100})*z",
//         C("abababababz"),
//         M("cdz")
//     );
//     test_exhausted!(
//         sfx_exhausted8,
//         suffixes,
//         "zaaaaaaaaaaaaaaaaaaaa",
//         C("aaaaaaaaaaaaaaaaaaaa")
//     );

//     // ************************************************************************
//     // Tests for generating unambiguous literal sets.
//     // ************************************************************************

//     macro_rules! test_unamb {
//         ($name:ident, $given:expr, $expected:expr) => {
//             #[test]
//             fn $name() {
//                 let given: Vec<Literal> = $given
//                     .into_iter()
//                     .map(|ul| {
//                         let cut = ul.cut;
//                         Literal { v: ul.v.into_bytes(), cut: cut }
//                     })
//                     .collect();
//                 let lits = create_lits(given);
//                 let got = lits.unambiguous_prefixes();
//                 assert_eq!($expected, escape_lits(got.literals()));
//             }
//         };
//     }

//     test_unamb!(unambiguous1, vec![M("z"), M("azb")], vec![C("a"), C("z")]);
//     test_unamb!(
//         unambiguous2,
//         vec![M("zaaaaaa"), M("aa")],
//         vec![C("aa"), C("z")]
//     );
//     test_unamb!(
//         unambiguous3,
//         vec![M("Sherlock"), M("Watson")],
//         vec![M("Sherlock"), M("Watson")]
//     );
//     test_unamb!(unambiguous4, vec![M("abc"), M("bc")], vec![C("a"), C("bc")]);
//     test_unamb!(unambiguous5, vec![M("bc"), M("abc")], vec![C("a"), C("bc")]);
//     test_unamb!(unambiguous6, vec![M("a"), M("aa")], vec![C("a")]);
//     test_unamb!(unambiguous7, vec![M("aa"), M("a")], vec![C("a")]);
//     test_unamb!(unambiguous8, vec![M("ab"), M("a")], vec![C("a")]);
//     test_unamb!(
//         unambiguous9,
//         vec![M("ac"), M("bc"), M("c"), M("ac"), M("abc"), M("c")],
//         vec![C("a"), C("b"), C("c")]
//     );
//     test_unamb!(
//         unambiguous10,
//         vec![M("Mo'"), M("Mu'"), M("Mo"), M("Mu")],
//         vec![C("Mo"), C("Mu")]
//     );
//     test_unamb!(
//         unambiguous11,
//         vec![M("zazb"), M("azb")],
//         vec![C("a"), C("z")]
//     );
//     test_unamb!(unambiguous12, vec![M("foo"), C("foo")], vec![C("foo")]);
//     test_unamb!(
//         unambiguous13,
//         vec![M("ABCX"), M("CDAX"), M("BCX")],
//         vec![C("A"), C("BCX"), C("CD")]
//     );
//     test_unamb!(
//         unambiguous14,
//         vec![M("IMGX"), M("MVIX"), M("MGX"), M("DSX")],
//         vec![M("DSX"), C("I"), C("MGX"), C("MV")]
//     );
//     test_unamb!(
//         unambiguous15,
//         vec![M("IMG_"), M("MG_"), M("CIMG")],
//         vec![C("C"), C("I"), C("MG_")]
//     );

//     // ************************************************************************
//     // Tests for suffix trimming.
//     // ************************************************************************
//     macro_rules! test_trim {
//         ($name:ident, $trim:expr, $given:expr, $expected:expr) => {
//             #[test]
//             fn $name() {
//                 let given: Vec<Literal> = $given
//                     .into_iter()
//                     .map(|ul| {
//                         let cut = ul.cut;
//                         Literal { v: ul.v.into_bytes(), cut: cut }
//                     })
//                     .collect();
//                 let lits = create_lits(given);
//                 let got = lits.trim_suffix($trim).unwrap();
//                 assert_eq!($expected, escape_lits(got.literals()));
//             }
//         };
//     }

//     test_trim!(trim1, 1, vec![M("ab"), M("yz")], vec![C("a"), C("y")]);
//     test_trim!(trim2, 1, vec![M("abc"), M("abd")], vec![C("ab")]);
//     test_trim!(trim3, 2, vec![M("abc"), M("abd")], vec![C("a")]);
//     test_trim!(trim4, 2, vec![M("abc"), M("ghij")], vec![C("a"), C("gh")]);

//     // ************************************************************************
//     // Tests for longest common prefix.
//     // ************************************************************************

//     macro_rules! test_lcp {
//         ($name:ident, $given:expr, $expected:expr) => {
//             #[test]
//             fn $name() {
//                 let given: Vec<Literal> = $given
//                     .into_iter()
//                     .map(|s: &str| Literal {
//                         v: s.to_owned().into_bytes(),
//                         cut: false,
//                     })
//                     .collect();
//                 let lits = create_lits(given);
//                 let got = lits.longest_common_prefix();
//                 assert_eq!($expected, escape_bytes(got));
//             }
//         };
//     }

//     test_lcp!(lcp1, vec!["a"], "a");
//     test_lcp!(lcp2, vec![], "");
//     test_lcp!(lcp3, vec!["a", "b"], "");
//     test_lcp!(lcp4, vec!["ab", "ab"], "ab");
//     test_lcp!(lcp5, vec!["ab", "a"], "a");
//     test_lcp!(lcp6, vec!["a", "ab"], "a");
//     test_lcp!(lcp7, vec!["ab", "b"], "");
//     test_lcp!(lcp8, vec!["b", "ab"], "");
//     test_lcp!(lcp9, vec!["foobar", "foobaz"], "fooba");
//     test_lcp!(lcp10, vec!["foobar", "foobaz", "a"], "");
//     test_lcp!(lcp11, vec!["a", "foobar", "foobaz"], "");
//     test_lcp!(lcp12, vec!["foo", "flub", "flab", "floo"], "f");

//     // ************************************************************************
//     // Tests for longest common suffix.
//     // ************************************************************************

//     macro_rules! test_lcs {
//         ($name:ident, $given:expr, $expected:expr) => {
//             #[test]
//             fn $name() {
//                 let given: Vec<Literal> = $given
//                     .into_iter()
//                     .map(|s: &str| Literal {
//                         v: s.to_owned().into_bytes(),
//                         cut: false,
//                     })
//                     .collect();
//                 let lits = create_lits(given);
//                 let got = lits.longest_common_suffix();
//                 assert_eq!($expected, escape_bytes(got));
//             }
//         };
//     }

//     test_lcs!(lcs1, vec!["a"], "a");
//     test_lcs!(lcs2, vec![], "");
//     test_lcs!(lcs3, vec!["a", "b"], "");
//     test_lcs!(lcs4, vec!["ab", "ab"], "ab");
//     test_lcs!(lcs5, vec!["ab", "a"], "");
//     test_lcs!(lcs6, vec!["a", "ab"], "");
//     test_lcs!(lcs7, vec!["ab", "b"], "b");
//     test_lcs!(lcs8, vec!["b", "ab"], "b");
//     test_lcs!(lcs9, vec!["barfoo", "bazfoo"], "foo");
//     test_lcs!(lcs10, vec!["barfoo", "bazfoo", "a"], "");
//     test_lcs!(lcs11, vec!["a", "barfoo", "bazfoo"], "");
//     test_lcs!(lcs12, vec!["flub", "bub", "boob", "dub"], "b");
// }
