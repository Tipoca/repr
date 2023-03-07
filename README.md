**TODO**

- Can `ignore` be represented by Map?
- Can flags, infos and assertions be represented by Map?
- Derivatives and lookaheads

**`Map`**

- Capturing/grouping

**`Zero`**

- `Zero` acts as both an additive and multiplicative unit?

**`Div`** (quotients)

- Case insensitivity

**Comparisons**

| name | regex | linear logic | repr |
| - | - | - | - |
| concatenation | `ab` | `a` & `b` (additive conjunction) | `And(a, b)` |
| alternation | `a\|b` | `a` ⊕ `b` (additive disjuction) | `Or(a, b)` |
| kleen star | `a*` | `a` | `Exp(a, Range::From(0))` |
| optional | `a?` | `a` | `Exp(a, Range::Full(0, 1))` |
| repetition | `a{n,m}` | `a` | `Exp(a, Range::Full(n, m))` |
| class | `[a-z]` | - | `Seq(a, z)` |
| negation | `[^a-z]` | - | `Not(a)` |
| negative lookahead | | | |

**Targets**

```rust
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Flag {
    /// `i`
    CaseInsensitive,
    /// `m`
    MultiLine,
    /// `s`
    DotMatchesNewLine,
    /// `U`
    SwapGreed,
    /// `u`
    Unicode,
    /// `x`
    IgnoreWhitespace,
}
```

- Recouse non-greedy pattern to _
