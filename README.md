**TODO**

- Arbitrary one character `.` vs `Seq(MIN, MAX)` vs `⊤` (meaning we don't define such token)
- Interpretation of `match`, is it a judgement `a : A` or a test that one is divisible by another (quotient is equal to zero) `a / b`?
- `ignore`, or lookahead/behind
- Derivatives
- `One` → `True` or sorts

**`Or`**

- Can we optimise it by assuring its lhs and rhs always have no common prefix?

**`Map`**

- The functional equivalent to grouping and capturing. 

**`Xor`**

- lookahead/behind, multiple futures, communication and discard

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
| `a*` (kleen star) | `!a` (of course, exponential conjunction) | `Exp(a)` | |
| `a*?` (non greedy) | `?a` (why not, exponential disjunction) | `Exp(a)` | |
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

**about symbols**

Symbols are gropued and assigned primarily by positive/negative distinciton. They are corresponding to whether computation exits or rather continues; though concatenation `&`/`Mul`/`*` has conjunctive meaning, computation doesn't complete/exit at not satisfying one criterion for there are still different ways of partition to try (backtracking). Though RegexSet `⅋`/`Add`/`+` has disjunctive meaning, computation doesn't complete/exit at satisfying one criterion to return which regex has match. On the other hand, alternation `⊕`/`Or`/`|` and intersection `⊗`/`And`/`&` early-break, hence order-dependent. When I add `Map` variant to `Repr` to execute arbitrary functions, this order-dependency suddenly becomes to matter for those arbitrary functions can have effects, for example, modification/replacement of input string.

| | additive | multiplicative | exponential |
| - | - | - | - |
| positive/external | ⊕ 0	| ⊗ 1	| ! |
| negative/internal |	& ⊤	| ⅋ ⊥ | ? |	

**Laws**

| linear logic | repr | title |
| - | - | - |
| | Or(a, Or(b, c)) = Or(Or(a, b), c) | Or-associativity |
| | |
| | Or(a, a) = a | Or-idempotence |
| a ⊕ 0 = 0 ⊕ a = a| Or(a, Zero) = Or(Zero, a) = a | Zero, Or-unit |
| a & ⊤ = ⊤ & a = a | Mul(a, One('')) = Mul(One(''), a) = a | One(''), Mul-unit |
| | Mul(Or(a, b), c) = Or(Mul(a, c), Mul(b, c)) | right distributivity |
| | Mul(a, Or(b, c)) = Or(Mul(a, b), Mul(a, c)) | left distributivity |
| | Rev(One(a)) = One(a) | |
| | Rev(Mul(a, b)) = Mul(Rev(b), Rev(a)) | |
| | Mul(One(a), One(b)) = One(ab) | |

exp(a + b) = exp(a) * exp(b)

**Derivative (TODO)**

- d(Zero) = Zero
- d(Or(a, b)) = Or(d(a), d(b))
- d(Mul(a, b)) = Or(Mul(d(a), b), Mul(a, d(b))  *
- d(Exp(a)) = Mul(d(a), Exp(a))
- a : D(a)
- 

**Flags (TODO)**
- `i`, CaseInsensitive
- `m`, MultiLine
- `s`, DotMatchesNewLine
- `U`, SwapGreed
- `u`, Unicode
- `x`, IgnoreWhitespace

- Recouse non-greedy pattern to _

**Interpretations**

- By regarding matching as an assignment of occurrences of strings to each part of an expression, regular expressions are resource (limited occurrences of strings) consumption (exclusive assignment/matching of them).
