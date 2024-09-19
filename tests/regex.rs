use repr::wrappers::seq;

macro_rules! assert_regex_eq {
    ($expr:expr, $rhs:literal) => {
        assert_eq!($expr.to_regex_string(), $rhs)
    };
}

#[test]
fn to_regex_string() {
    assert_regex_eq!(seq(['a']), "a");
    assert_regex_eq!(seq(['a']).mul(seq(['b'])), "ab");
    assert_regex_eq!(seq(['a']).mul(seq(['b'])).mul(seq(['c'])), "abc");
    assert_regex_eq!(seq(['a', 'b']), "ab");
    assert_regex_eq!(seq(['a', 'b', 'c']), "abc");
    assert_regex_eq!(seq(['a']).or(seq(['b'])), "(a|b)");
    assert_regex_eq!(seq(['a']).or(seq(['b'])).or(seq(['c'])), "(a|(b|c))");
    assert_regex_eq!(seq(['a']).mul(seq(['b', 'c'])), "abc");
    assert_regex_eq!(seq(['a']).mul(seq(['b', 'c', 'd'])), "abcd");
    assert_regex_eq!(seq(['a', 'b']).mul(seq(['c'])), "abc");
    assert_regex_eq!(seq(['a', 'b', 'c']).mul(seq(['d'])), "abcd");
    assert_regex_eq!(seq(['a']).inf(), "a*");
    assert_regex_eq!(seq(['a']).cap(), "(a)");
    assert_regex_eq!(seq(['a']).mul(seq(['b'])).cap(), "(ab)");
    assert_regex_eq!(seq(['a']).inf().cap(), "(a*)");
}

#[test]
fn escape() {
    assert_regex_eq!(seq(['+']), "\\+");
    assert_regex_eq!(seq(['*']), "\\*");
    assert_regex_eq!(seq(['?']), "\\?");
    assert_regex_eq!(seq(['.']), "\\.");
    assert_regex_eq!(seq(['(']), "\\(");
    assert_regex_eq!(seq([')']), "\\)");
    assert_regex_eq!(seq(['[']), "\\[");
    assert_regex_eq!(seq([']']), "\\]");
    assert_regex_eq!(seq(['{']), "\\{");
    assert_regex_eq!(seq(['}']), "\\}");
    assert_regex_eq!(seq(['^']), "\\^");
    assert_regex_eq!(seq(['$']), "\\$");
    assert_regex_eq!(seq(['|']), "\\|");
    assert_regex_eq!(seq(['\\']), "\\\\");
}

macro_rules! assert_captures {
    ($expr:expr, $rhs:literal) => {
        assert!($expr.captures($rhs).is_some());
    };
}

macro_rules! assert_not_captures {
    ($expr:expr, $rhs:literal) => {
        assert!($expr.captures($rhs).is_none());
    };
}

#[test]
fn captures() {
    assert_captures!(seq(['a']), "a");
    assert_not_captures!(seq(['a']), "aa");
    assert_captures!(seq(['a']).mul(seq(['b'])), "ab");
    assert_captures!(seq(['a']).mul(seq(['b'])).mul(seq(['c'])), "abc");
    assert_captures!(seq(['a', 'b']), "ab");
    assert_captures!(seq(['a', 'b', 'c']), "abc");
    assert_captures!(seq(['a']).or(seq(['b'])), "a");
    assert_captures!(seq(['a']).or(seq(['b'])), "b");
    assert_captures!(seq(['a']).or(seq(['b'])).or(seq(['c'])), "a");
    assert_captures!(seq(['a']).or(seq(['b'])).or(seq(['c'])), "b");
    assert_captures!(seq(['a']).or(seq(['b'])).or(seq(['c'])), "c");
    assert_captures!(seq(['a']).mul(seq(['b', 'c'])), "abc");
    assert_captures!(seq(['a']).mul(seq(['b', 'c', 'd'])), "abcd");
    assert_captures!(seq(['a', 'b']).mul(seq(['c'])), "abc");
    assert_captures!(seq(['a', 'b', 'c']).mul(seq(['d'])), "abcd");
    assert_captures!(seq(['a']).inf(), "");
    assert_captures!(seq(['a']).inf(), "a");
    assert_captures!(seq(['a']).inf(), "aa");
    assert_captures!(seq(['a']).inf(), "aaa");
    assert_captures!(seq(['a']).cap(), "a");
    assert_captures!(seq(['a']).mul(seq(['b'])).cap(), "ab");
    assert_captures!(seq(['a']).inf().cap(), "a");
}
