use repr::wrappers::seq;

macro_rules! debug {
    ($expr:expr, $rhs:literal) => {
        assert_eq!(format!("{:?}", $expr), $rhs)
    };
}

#[test]
fn debug() {
    debug!(seq(['a']), "Seq(Seq(['a']))");
    debug!(seq(['a']).mul(seq(['b'])), "Seq(Seq(['a', 'b']))");
    debug!(
        seq(['a']).or(seq(['b'])),
        "Or(Seq(Seq(['a'])), Seq(Seq(['b'])))"
    );
    debug!(seq(['a']).inf(), "Inf(Seq(Seq(['a'])))");
}
