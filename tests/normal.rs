use repr::wrappers::{one, seq, zero};

#[test]
fn reflexivity() {
    assert_eq!(one('a'), one('a'));
    assert_eq!(one('a').mul(one('b')), one('a').mul(one('b')));
    assert_eq!(one('a').or(one('b')), one('a').or(one('b')));
    assert_eq!(one('a').inf(), one('a').inf());
    assert_eq!(one('a').add(one('b')), one('a').add(one('b')));
    assert_eq!(one('a').and(one('b')), one('a').and(one('b')));
}

#[test]
fn mul_linearity() {
    assert_eq!(one('a').mul(one('b')), seq(['a', 'b']));
    assert_eq!(one('a').mul(one('b')).mul(one('c')), seq(['a', 'b', 'c']));
    assert_eq!(
        one('a').mul(one('b')).mul(one('c').mul(one('d'))),
        seq(['a', 'b', 'c', 'd'])
    );
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
    assert_eq!(
        one('a').mul(one('b').mul(one('c'))),
        one('a').mul(one('b')).mul(one('c'))
    );
}

#[test]
fn or_unit() {
    assert_eq!(one('a').or(zero()), one('a'));
    assert_eq!(zero().or(one('a')), one('a'));
}

/// a ⊕ b = b ⊕ a
#[test]
fn or_commutativity() {
    assert_eq!(one('a').or(one('b')), one('b').or(one('a')));
}

/// a ⊕ (b ⊕ c) = (a ⊕ b) ⊕ c
#[test]
fn or_associativity() {
    assert_eq!(
        one('a').or(one('b').or(one('c'))),
        one('a').or(one('b')).or(one('c'))
    );
}

/// a ⊕ a = a
#[test]
fn or_idempotence() {
    assert_eq!(one('a').or(one('a')), one('a'));
}

#[test]
fn inf_idempotence() {
    assert_eq!(one('a').inf().inf(), one('a').inf());
}

#[test]
fn add_non_commutativity() {
    assert_ne!(one('a').add(one('b')), one('b').add(one('a')));
}

#[test]
fn add_non_idempotence() {
    assert_ne!(one('a').add(one('a')), one('a'));
}

#[test]
fn add_associativity() {
    assert_eq!(
        one('a').add(one('b').add(one('c'))),
        one('a').add(one('b')).add(one('c'))
    );
}

#[test]
fn and_commutativity() {
    assert_eq!(one('a').and(one('b')), one('b').and(one('a')));
}

#[test]
fn and_associativity() {
    assert_eq!(
        one('a').and(one('b').and(one('c'))),
        one('a').and(one('b')).and(one('c'))
    );
}

#[test]
fn and_idempotence() {
    assert_eq!(one('a').and(one('a')), one('a'));
}

#[test]
fn dual() {
    assert_eq!(one('a').inf().dual(), one('a').dual().sup());
}

#[test]
fn dual_involution() {
    assert_eq!(one('a').dual().dual(), one('a'));
    assert_eq!(one('a').mul(one('b')).dual().dual(), one('a').mul(one('b')));
    assert_eq!(one('a').or(one('b')).dual().dual(), one('a').or(one('b')));
}
