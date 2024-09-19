use repr::wrappers::{empty, seq, zero};

#[test]
fn reflexivity() {
    assert_eq!(seq(['a']), seq(['a']));
    assert_eq!(seq(['a']).mul(seq(['b'])), seq(['a']).mul(seq(['b'])));
    assert_eq!(seq(['a']).or(seq(['b'])), seq(['a']).or(seq(['b'])));
    assert_eq!(seq(['a']).inf(), seq(['a']).inf());
    assert_eq!(seq(['a']).add(seq(['b'])), seq(['a']).add(seq(['b'])));
    assert_eq!(seq(['a']).and(seq(['b'])), seq(['a']).and(seq(['b'])));
}

#[test]
fn mul_linearity() {
    assert_eq!(seq(['a']).mul(seq(['b'])), seq(['a', 'b']));
    assert_eq!(
        seq(['a']).mul(seq(['b'])).mul(seq(['c'])),
        seq(['a', 'b', 'c'])
    );
    assert_eq!(
        seq(['a']).mul(seq(['b'])).mul(seq(['c']).mul(seq(['d']))),
        seq(['a', 'b', 'c', 'd'])
    );
}

#[test]
fn mul_unit() {
    assert_eq!(seq(['a']).mul(empty()), seq(['a']));
    assert_eq!(empty().mul(seq(['a'])), seq(['a']));
}

#[test]
fn mul_non_commutativity() {
    assert_ne!(seq(['a']).mul(seq(['b'])), seq(['b']).mul(seq(['a'])));
}

#[test]
fn mul_non_idempotence() {
    assert_ne!(seq(['a']).mul(seq(['a'])), seq(['a']));
}

#[test]
fn mul_associativity() {
    assert_eq!(
        seq(['a']).mul(seq(['b']).mul(seq(['c']))),
        seq(['a']).mul(seq(['b'])).mul(seq(['c']))
    );
}

#[test]
fn or_unit() {
    assert_eq!(seq(['a']).or(zero()), seq(['a']));
    assert_eq!(zero().or(seq(['a'])), seq(['a']));
}

/// a ⊕ b = b ⊕ a
#[test]
fn or_commutativity() {
    assert_eq!(seq(['a']).or(seq(['b'])), seq(['b']).or(seq(['a'])));
}

/// a ⊕ (b ⊕ c) = (a ⊕ b) ⊕ c
#[test]
fn or_associativity() {
    assert_eq!(
        seq(['a']).or(seq(['b']).or(seq(['c']))),
        seq(['a']).or(seq(['b'])).or(seq(['c']))
    );
}

/// a ⊕ a = a
#[test]
fn or_idempotence() {
    assert_eq!(seq(['a']).or(seq(['a'])), seq(['a']));
}

#[test]
fn inf_idempotence() {
    assert_eq!(seq(['a']).inf().inf(), seq(['a']).inf());
}

#[test]
fn add_non_commutativity() {
    assert_ne!(seq(['a']).add(seq(['b'])), seq(['b']).add(seq(['a'])));
}

#[test]
fn add_non_idempotence() {
    assert_ne!(seq(['a']).add(seq(['a'])), seq(['a']));
}

#[test]
fn add_associativity() {
    assert_eq!(
        seq(['a']).add(seq(['b']).add(seq(['c']))),
        seq(['a']).add(seq(['b'])).add(seq(['c']))
    );
}

#[test]
fn and_commutativity() {
    assert_eq!(seq(['a']).and(seq(['b'])), seq(['b']).and(seq(['a'])));
}

#[test]
fn and_associativity() {
    assert_eq!(
        seq(['a']).and(seq(['b']).and(seq(['c']))),
        seq(['a']).and(seq(['b'])).and(seq(['c']))
    );
}

#[test]
fn and_idempotence() {
    assert_eq!(seq(['a']).and(seq(['a'])), seq(['a']));
}

#[test]
fn dual() {
    assert_eq!(seq(['a']).inf().dual(), seq(['a']).dual().sup());
}

#[test]
fn dual_involution() {
    assert_eq!(seq(['a']).dual().dual(), seq(['a']));
    assert_eq!(
        seq(['a']).mul(seq(['b'])).dual().dual(),
        seq(['a']).mul(seq(['b']))
    );
    assert_eq!(
        seq(['a']).or(seq(['b'])).dual().dual(),
        seq(['a']).or(seq(['b']))
    );
}
