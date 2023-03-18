use repr::{
    Repr,
    wrappers::one
};

#[test]
fn reflexivity() {
    assert_eq!(one('a'), one('a'));
}

#[test]
fn or_associativity() {
    assert_eq!(one('a').or(one('b').or(one('c'))), one('a').or(one('b')).or(one('c')));
}

#[test]
fn or_idempotence() {
    assert_eq!(one('a').or(one('a')), one('a'));
}

/*
#[test]
fn mul_unit() {
    assert_eq!();
}
*/
