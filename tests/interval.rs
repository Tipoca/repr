use repr::wrappers::interval;

#[test]
fn reflexivity() {
    assert_eq!(interval('a', 'f').or(interval('g', 'z')), interval('a', 'z'));
}
