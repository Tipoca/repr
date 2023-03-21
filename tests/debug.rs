use repr::wrappers::one;

macro_rules! debug {
    ($expr:expr) => { format!("{:?}", $expr) }
}

#[test]
fn debug() {
    assert!(debug!(one('a')), "One(['a'])");
}
