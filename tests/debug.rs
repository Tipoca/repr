use repr::wrappers::one;

macro_rules! debug {
    ($expr:expr, $rhs:literal) => {
        assert_eq!(format!("{:?}", $expr), $rhs)
    }
}

#[test]
fn debug() {
    debug!(one('a'), "One(Seq(['a']))");
    debug!(one('a').mul(one('b')), "One(Seq(['a', 'b']))");
    debug!(one('a').or(one('b')), "Or(One(Seq(['a'])), One(Seq(['b'])))");
    debug!(one('a').inf(), "Inf(One(Seq(['a'])))");
}
