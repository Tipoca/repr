use repr::wrappers::interval;

#[test]
fn or() {
    assert_eq!(interval('a', 'f').or(interval('g', 'z')), interval('a', 'z'));
}

#[test]
fn and() {
    assert_eq!(interval('a', 'm').and(interval('g', 'z')), interval('g', 'm'));
}
