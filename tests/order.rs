use repr::wrappers::one;

#[test]
fn inclusion() {
    assert!(one('a').le(&one('a').or(one('b'))));
    assert!(one('b').le(&one('a').or(one('b'))));
}

#[test]
fn le_reflexivity() {
    assert!(one('a').le(&one('a')));
}
