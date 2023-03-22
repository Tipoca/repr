use repr::wrappers::{zero, one, seq};

#[test]
fn reflexivity() {
    assert_eq!(one('a'), one('a'));
    assert_eq!(one('a').mul(one('b')), one('a').mul(one('b')));
    assert_eq!(one('a').or(one('b')), one('a').or(one('b')));
}

#[test]
fn mul_linearity() {
    assert_eq!(one('a').mul(one('b')), seq(['a', 'b']));
    assert_eq!(one('a').mul(one('b')).mul(one('c')), seq(['a', 'b', 'c']));
    assert_eq!(one('a').mul(one('b')).mul(one('c').mul(one('d'))), seq(['a', 'b', 'c', 'd']));
}

#[test]
fn mul_unit() {
    assert_eq!(one('a').mul(seq([])), one('a'));
    assert_eq!(seq([]).mul(one('a')), one('a'));
}

#[test]
fn mul_non_commutativity() {
    assert_ne!(one('a').mul(one('b')), one('b').mul(one('a')));
}

#[test]
fn mul_non_idempotence() {
    assert_ne!(one('a').mul(one('a')), one('a'));
}

#[test]
fn mul_associativity() {
    assert_eq!(one('a').mul(one('b').mul(one('c'))),
               one('a').mul(one('b')).mul(one('c')));
}

#[test]
fn or_unit() {
    assert_eq!(one('a').or(zero()), one('a'));
    assert_eq!(zero().or(one('a')), one('a'));
}

#[test]
fn or_associativity() {
    assert_eq!(one('a').or(one('b').or(one('c'))),
               one('a').or(one('b')).or(one('c')));
}

#[test]
fn or_idempotence() {
    assert_eq!(one('a').or(one('a')), one('a'));
}

/*
#[test]
fn add_idempotence() {
    assert_eq!(one('a').add(one('a')), one('a'));
}
*/

#[test]
fn add_associativity() {
    assert_eq!(one('a').add(one('b').add(one('c'))),
               one('a').add(one('b')).add(one('c')));
}

#[test]
fn and_idempotence() {
    assert_eq!(one('a').and(one('a')), one('a'));
}

#[test]
fn dual() {
    
}

#[test]
fn dual_involution() {
    assert_eq!(one('a').dual().dual(), one('a'));
    assert_eq!(one('a').mul(one('b')).dual().dual(), one('a').mul(one('b')));
    assert_eq!(one('a').or(one('b')).dual().dual(), one('a').or(one('b')));
}
