use repr::wrappers::{one, seq};

macro_rules! assert_regex_eq {
    ($expr:expr, $rhs:literal) => {
        assert_eq!($expr.to_regex_string(), $rhs)
    };
}

#[test]
fn main() {
    assert_regex_eq!(one('a'), "a");
    assert_regex_eq!(one('a').mul(one('b')), "ab");
    assert_regex_eq!(one('a').mul(one('b')).mul(one('c')), "abc");
    assert_regex_eq!(seq(['a', 'b']), "ab");
    assert_regex_eq!(seq(['a', 'b', 'c']), "abc");
    assert_regex_eq!(one('a').or(one('b')), "a|b");
    assert_regex_eq!(one('a').or(one('b')).or(one('c')), "a|b|c");
    assert_regex_eq!(one('a').mul(seq(['b', 'c'])), "abc");
    assert_regex_eq!(one('a').mul(seq(['b', 'c', 'd'])), "abcd");
    assert_regex_eq!(seq(['a', 'b']).mul(one('c')), "abc");
    assert_regex_eq!(seq(['a', 'b', 'c']).mul(one('d')), "abcd");
    assert_regex_eq!(one('a').inf(), "a*");
    assert_regex_eq!(one('a').cap(), "(a)");
    assert_regex_eq!(one('a').mul(one('b')).cap(), "(ab)");
    assert_regex_eq!(one('a').inf().cap(), "(a*)");
}
