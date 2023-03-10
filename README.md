**TODO**

- Interpretation of `match`, is it a judgement `a : A` or divisible (quotient is equal to zero) `a / b`?
- `ignore`, or lookahead/behind
- Derivatives
- `One` → `True` or sorts

**`Or`**

- Can we optimise it by assuring its lhs and rhs always have no common prefix?

**`Map`**

- The functional equivalent to grouping and capturing. 

**`Rem`** (partial match)

**`Sh`** (shift position?)

**Comparisons**

| regex | linear logic | repr | op | len |
| - | - | - | - | - |
| a ∈ L (match) | `a` : `A` (judgement) | | |
| ∅ | 0 | `Zero` | | |
| `a` | `a` | `One(a)` | | len(a) |
| ε (empty) | `*`, ⊤ | `One('')` | | 0 |
| `ab`/`a` · `b` (concatenation) | `a` & `b` (additive conjunction/with) | `Mul(a, b)` | `*` | len(a) + len(b) |
| `a\|b` (alternation) | `a` ⊕ `b` (additive disjuction/plus) | `Or(a, b)` | `\|` | max(len(a), len(b))
| `a*` (kleen star) | `!a` (of course) | `Exp(a)` | |
| `a*?` (non greedy) | `?a` (why not) | `Exp(a)` | |
| `a?` () | `a` | `Or(Zero, a)` | |
| `a{n,m}` (repetition) | `a` | `Or(Mul(a, Mul(a, ..)), Or(..))` | |
| `[a-z]` (class) | | `Seq(a, z)` | `..` | |
| `[^a-z]` (negation) | | `Neg(a)` | `-` | |
| `a`<sup>†</sup> (reverse) | right law vs left law | `Rev(a)` | | len(a) |
| `a` / `b` (right quotient) | `a` ⊸ `b` | `Div(a, b)` | `/` | len(a) - len(b) |
| `a` \ `b` (left quotient) | | `Div(a, b)` | `/` | |
| RegexSet | `a` ⅋ `b` (multiplicative disjunction/par) | `Add(a, b)` | `+` | |
| `a` ∩ `b` (intersection) | `a` ⊗ `b` (multiplicative conjunction/times) | `And(a, b)` | `&` | |
| `a(?=b)` (positive lookahead) | | `And(a, b)` | | |
| `a(?!b)` (negative lookahead) | | `And(a, Not(b))` | | |
| `(?<=a)b` (positive lookbehind) | | `And(a, b)` | | |
| `(?<!a)b` (negative lookbehind) | | `And(a, b)` | | |

**Laws**

| linear logic | repr | title |
| - | - | - |
| | Or(a, Or(b, c)) = Or(Or(a, b), c) | Or-associativity |
| | |
| | Or(a, a) = a | Or-idempotence |
| a ⊕ 0 = 0 ⊕ a = a| Or(a, Zero) = Or(Zero, a) = a | Zero, Or-unit |
| a & ⊤ = ⊤ & a = a | Mul(a, One('')) = Mul(One(''), a) = a | One(''), Mul-unit |
| TODO a & 0 = 0 & a = 0 | Mul(a, Zero) = Mul(Zero, a) = Zero | Zero, Mul-zero |
| | Mul(Or(a, b), c) = Or(Mul(a, c), Mul(b, c)) | right distributivity |
| | Mul(a, Or(b, c)) = Or(Mul(a, b), Mul(a, c)) | left distributivity |
| | Rev(One(a)) = One(a) | |
| | Rev(Mul(a, b)) = Mul(Rev(b), Rev(a)) | |
| | Mul(One(a), One(b)) = One(ab) | |

**Flags (TODO)**
- `i`, CaseInsensitive
- `m`, MultiLine
- `s`, DotMatchesNewLine
- `U`, SwapGreed
- `u`, Unicode
- `x`, IgnoreWhitespace
```

- Recouse non-greedy pattern to _
