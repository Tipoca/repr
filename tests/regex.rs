use repr::{
    wrappers::{empty, interval, one, seq},
    Repr,
};

macro_rules! assert_regex_eq {
    ($expr:expr, $rhs:literal) => {
        assert_eq!($expr.to_regex_string(), $rhs)
    };
}

#[test]
fn to_regex_string() {
    assert_regex_eq!(one('a'), "a");
    assert_regex_eq!(one('a').mul(one('b')), "ab");
    assert_regex_eq!(one('a').mul(one('b')).mul(one('c')), "abc");
    assert_regex_eq!(seq(['a', 'b']), "ab");
    assert_regex_eq!(seq(['a', 'b', 'c']), "abc");
    assert_regex_eq!(one('a').or(one('b')), "(a|b)");
    assert_regex_eq!(one('a').or(one('b')).or(one('c')), "(a|(b|c))");
    assert_regex_eq!(one('a').mul(seq(['b', 'c'])), "abc");
    assert_regex_eq!(one('a').mul(seq(['b', 'c', 'd'])), "abcd");
    assert_regex_eq!(seq(['a', 'b']).mul(one('c')), "abc");
    assert_regex_eq!(seq(['a', 'b', 'c']).mul(one('d')), "abcd");
    assert_regex_eq!(one('a').inf(), "a*");
    assert_regex_eq!(one('a').cap(), "(a)");
    assert_regex_eq!(one('a').mul(one('b')).cap(), "(ab)");
    assert_regex_eq!(one('a').inf().cap(), "(a*)");
}

#[test]
fn escape() {
    assert_regex_eq!(one('+'), "\\+");
    assert_regex_eq!(one('*'), "\\*");
    assert_regex_eq!(one('?'), "\\?");
    assert_regex_eq!(one('.'), "\\.");
    assert_regex_eq!(one('('), "\\(");
    assert_regex_eq!(one(')'), "\\)");
    assert_regex_eq!(one('['), "\\[");
    assert_regex_eq!(one(']'), "\\]");
    assert_regex_eq!(one('{'), "\\{");
    assert_regex_eq!(one('}'), "\\}");
    assert_regex_eq!(one('^'), "\\^");
    assert_regex_eq!(one('$'), "\\$");
    assert_regex_eq!(one('|'), "\\|");
    assert_regex_eq!(one('\\'), "\\\\");
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
    assert_captures!(one('a'), "a");
    assert_not_captures!(one('a'), "aa");
    assert_captures!(one('a').mul(one('b')), "ab");
    assert_captures!(one('a').mul(one('b')).mul(one('c')), "abc");
    assert_captures!(seq(['a', 'b']), "ab");
    assert_captures!(seq(['a', 'b', 'c']), "abc");
    assert_captures!(one('a').or(one('b')), "a");
    assert_captures!(one('a').or(one('b')), "b");
    assert_captures!(one('a').or(one('b')).or(one('c')), "a");
    assert_captures!(one('a').or(one('b')).or(one('c')), "b");
    assert_captures!(one('a').or(one('b')).or(one('c')), "c");
    assert_captures!(one('a').mul(seq(['b', 'c'])), "abc");
    assert_captures!(one('a').mul(seq(['b', 'c', 'd'])), "abcd");
    assert_captures!(seq(['a', 'b']).mul(one('c')), "abc");
    assert_captures!(seq(['a', 'b', 'c']).mul(one('d')), "abcd");
    assert_captures!(one('a').inf(), "");
    assert_captures!(one('a').inf(), "a");
    assert_captures!(one('a').inf(), "aa");
    assert_captures!(one('a').inf(), "aaa");
    assert_captures!(one('a').cap(), "a");
    assert_captures!(one('a').mul(one('b')).cap(), "ab");
    assert_captures!(one('a').inf().cap(), "a");
}
