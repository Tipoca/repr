use repr::wrappers::seq;

macro_rules! debug {
    ($expr:expr, $rhs:literal) => {
        assert_eq!(format!("{:?}", $expr), $rhs)
    };
}

#[test]
fn debug() {
    debug!(seq(['a']), "One(Seq(['a']))");
    debug!(seq(['a']).mul(seq(['b'])), "One(Seq(['a', 'b']))");
    debug!(
        seq(['a']).or(seq(['b'])),
        "Or(One(Seq(['a'])), One(Seq(['b'])))"
    );
    debug!(seq(['a']).inf(), "Inf(One(Seq(['a'])))");
}
