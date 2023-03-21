use repr::wrappers::interval;

#[test]
fn normal() {
    assert_eq!(interval('a', 'f').or(interval('g', 'z')), interval('a', 'z'));
}
