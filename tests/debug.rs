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
}
