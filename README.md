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

**`Rev`** (?)

**Comparisons**

| regex | linear logic | repr | op |
| - | - | - | - |
| (empty/ε) | | `Zero` | |
| `a` | `a` | `One(a)` | |
| `ab`/`a` · `b` (concatenation) | `a` & `b` (additive conjunction/with) | `Mul(a, b)` | `*` |
| `a\|b` (alternation) | `a` ⊕ `b` (additive disjuction/plus) | `Or(a, b)` | `\|` |
| `a*` (kleen star) | `a` | `Exp(a, Range::From(0))` |
| `a?` () | `a` | `Exp(a, Range::Full(0, 1))` |
| `a{n,m}` (repetition) | `a` | `Exp(a, Range::Full(n, m))` |
| `[a-z]` (class) | - | `Seq(a, z)` | `..` |
| `[^a-z]` (negation) | - | `Not(a)` | `!` |
| (negative lookahead) | | | |
| reverse? | | `Rev(a)` | `-` |
| quotient | | `Div(a, b)` | `/` |
| RegexSet | `a` ⅋ `b` (multiplicative disjunction/par) | `Add(a, b)` | `+` |
| intersection | `a` ⊗ `b` (multiplicative conjunction/times) | `And(a, b)` | `&` |

**Rules**

- `Rev(One(a))` = `One(a)`
- `Rev(Mul(a, b))` = `Mul(Rev(b), Rev(a))`

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
