use repr::wrappers::{one, seq};

#[test]
fn reflexivity() {
    assert_eq!(one('a'), one('a'));
    assert_eq!(one('a').mul(one('b')), one('a').mul(one('b')));
    assert_eq!(one('a').or(one('b')), one('a').or(one('b')));
}

#[test]
fn mul_unit() {
    assert_eq!(one('a').mul(seq([])), one('a'));
    assert_eq!(seq([]).mul(one('a')), one('a'));
}

#[test]
fn mul_linearity() {
    assert_eq!(one('a').mul(one('b')), seq(['a', 'b']));
}

#[test]
fn mul_associativity() {
    assert_eq!(one('a').mul(one('b').mul(one('c'))),
               one('a').mul(one('b')).mul(one('c')));
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

#[test]
fn dual() {
    
}

#[test]
fn dual_involution() {
    assert_eq!(one('a').dual(), one('a'));
    assert_eq!(one('a').mul(one('b')).dual(), one('a').mul(one('b')));
    assert_eq!(one('a').or(one('b')).dual(), one('a').or(one('b')));
}








}
