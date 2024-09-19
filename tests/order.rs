use repr::wrappers::seq;

#[test]
fn inclusion() {
    assert!(seq(['a']).le(&seq(['a']).or(seq(['b']))));
    assert!(seq(['b']).le(&seq(['a']).or(seq(['b']))));
    assert!(seq(['a'])
        .or(seq(['b']))
        .le(&seq(['a']).or(seq(['b'])).or(seq(['c']))));
    // TODO(rinarakaki)
    // assert!(seq(['b']).or(seq(['c'])).le(&seq(['a']).or(seq(['b'])).or(seq(['c']))));
}

#[test]
fn le_reflexivity() {
    assert!(seq(['a']).le(&seq(['a'])));
}
