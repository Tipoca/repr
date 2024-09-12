use repr::wrappers::one;

#[test]
fn inclusion() {
    assert!(one('a').le(&one('a').or(one('b'))));
    assert!(one('b').le(&one('a').or(one('b'))));
    assert!(one('a')
        .or(one('b'))
        .le(&one('a').or(one('b')).or(one('c'))));
    // TODO(rnarkk)
    // assert!(one('b').or(one('c')).le(&one('a').or(one('b')).or(one('c'))));
}

#[test]
fn le_reflexivity() {
    assert!(one('a').le(&one('a')));
}
