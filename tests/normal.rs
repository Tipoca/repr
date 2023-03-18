use repr::{
    Repr,
    wrappers::one
};

#[test]
fn or_associativity() {
    assert_eq!(one('a').or(one('b').or(one('c'))), one('a').or(one('b')).or(one('c')));
}

/*
#[test]
fn mul_unit() {
    assert_eq!();
}
*/
