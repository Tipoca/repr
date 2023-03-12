**Caveats**

- Regular-expression-as-linear-logic interpretation is not fully general as it has decidability condition. For example, RegexSet-as-multiplicative-disjunction interpretaiton differs from ordinal resource interpretation in that when regexset has match we know where the match comes from (which regex(es) of regexset have(has) match).
- The assignment of the symbol groups (`Mul`/`*`, `Or`/`|`) and (`Add`/`+`, `And`/`&`) may be exchanged with one another in the future.
- TODO Partition of a context, equality between contexts

**TODO**

- Arbitrary one character `.` vs `Interval(MIN, MAX)` vs `⊤` (meaning we don't define such token)
- Interpretation of `match`, is it a judgement `a : A` or a test that one is divisible by another (quotient is equal to zero) `a / b`?
- `ignore`, or lookahead/behind
- Derivatives
- Rename `One` → `Seq` or sorts
- Equational reasoning
- Induction
- `True(I, Box<Repr<I>>)` (assertions, dependent types)
- Fixed points
- Spherical conic (tennis ball)

**`Or`**

- Can we optimise it by assuring its lhs and rhs always have no common prefix?

**`Map`**

- The functional equivalent to grouping and capturing. 

**`Xor`**

- lookahead/behind, multiple futures, communication and discard
- Split, subspace

**`Rem`** (partial match)

**`Sh`** (shift position?)

**Comparisons**

| regex | linear logic | repr | type theory/<br/>category theory | len |
| - | - | - | - | - |
| a ∈ L (match) | `a` : `A` (judgement) | | |
| ∅ | 0 | `Zero` | | |
| `a` | `a` | `One(a)` | | len(a) |
| ε (empty) | `*`, ⊤ | - | | 0 |
| `ab`/`a` · `b` (concatenation) | `a` & `b` (additive conjunction/with) | `Mul(a, b)`/`a * b` | × (product) | len(a) + len(b) |
| `a\|b` (alternation) | `a` ⊕ `b` (additive disjuction/plus) | `Or(a, b)`/`a \| b` | + (coproduct) | max(len(a), len(b))
| `a*` (kleen star) | `!a` (of course, exponential conjunction) | `Exp(a)` | □ (necessity) |
| `a*?` (non greedy) | `?a` (why not, exponential disjunction) | `Exp(a)` | ◊ (possibility) |
| `a?` () | `a` | `Or(Zero, a)` | |
| `a{n,m}` (repetition) | `a` | `Or(Mul(a, Mul(a, ..)), Or(..))` | |
| `[a-z]` (class) | | `Interval(a, z)`/`a..z` | | |
| `[^a-z]` (negation) | TODO this is complementary op | `Neg(a)`/`-a` | | |
| `a`<sup>†</sup> (reverse) | right law vs left law | `Rev(a)` | | len(a) |
| `a` / `b` (right quotient) | `a` ⊸ `b` | `Div(a, b)`/`a / b` | | len(a) - len(b) |
| `a` \ `b` (left quotient) | | `Div(a, b)` | | |
| RegexSet | `a` ⅋ `b` (multiplicative disjunction/par) | `Add(a, b)`/`a + b` | ⊕ (direct sum) | |
| `a` ∩ `b` (intersection) | `a` ⊗ `b` (multiplicative conjunction/times) | `And(a, b)`/`a & b` | ⊗ (tensor product) | |
| `a(?=b)` (positive lookahead) | | `And(a, b)` | | |
| `a(?!b)` (negative lookahead) | | `And(a, Not(b))` | | |
| `(?<=a)b` (positive lookbehind) | | `And(a, b)` | | |
| `(?<!a)b` (negative lookbehind) | | `And(a, b)` | | |

**about symbols**

Symbols are grouped and assigned primarily by positive/negative distinciton. They are corresponding to whether computation exits or rather continues; though concatenation `&`/`Mul`/`*` has conjunctive meaning, computation doesn't complete/exit at not satisfying one criterion for there are still different ways of partition to try (backtracking). Though RegexSet `⅋`/`Add`/`+` has disjunctive meaning, computation doesn't complete/exit at satisfying one criterion to return which regex has match. On the other hand, alternation `⊕`/`Or`/`|` and intersection `⊗`/`And`/`&` early-break, hence order-dependent. When I add `Map` variant to `Repr` to execute arbitrary functions, this order-dependency suddenly becomes to matter for those arbitrary functions can have effects, for example, modification/replacement of input string.

| | additive | multiplicative | exponential |
| - | - | - | - |
| positive/extensional | ⊕ 0 | ⊗ 1 | ! |
| negative/intensional | & ⊤ | ⅋ ⊥ | ? |

**Laws**

TODO:

- `Seq::empty()` - can be empty because negative
- `Interval::full()` - can't be empty because positive

| linear logic/quantale | repr | title |
| - | - | - |
| | Or(a, Or(b, c)) = Or(Or(a, b), c) | Or-associativity |
| | |
| a ⊕ a = a | Or(a, a) = a | Or-idempotence |
| a ⊕ 0 = 0 ⊕ a = a| Or(a, Zero) = Or(Zero, a) = a | Zero, Or-unit |
| a & ⊤ = ⊤ & a = a | Mul(a, One('')) = Mul(One(''), a) = a | One(''), Mul-unit |
| (a ⊕ b) & c = (a & c) ⊕ (b & c) | Mul(Or(a, b), c) = Or(Mul(a, c), Mul(b, c)) | right distributivity |
| a & (b ⊕ c) = (a & b) ⊕ (a & c) | Mul(a, Or(b, c)) = Or(Mul(a, b), Mul(a, c)) | left distributivity |
| a<sup>†</sup> = a | Rev(One(a)) = One(a) | |
| (a & b)<sup>†</sup> = (b<sup>†</sup>) & (a<sup>†</sup>)| Rev(Mul(a, b)) = Mul(Rev(b), Rev(a)) | |
| | Mul(One(a), One(b)) = One(ab) | |

Relationship among additive, multiplicative and exponential

- exp(a + b) = exp(a) * exp(b)

Linearity (which)

- f(a + b) = f(a) + f(b)
- (a → b) + (b → a)
- functions take only one argument

**Derivative (TODO)**

- d(Zero) = Zero
- d(Or(a, b)) = Or(d(a), d(b))
- d(Mul(a, b)) = Or(Mul(d(a), b), Mul(a, d(b))  *
- d(Exp(a)) = Mul(d(a), Exp(a))
- a : D(a)
- 

**True**

- And(True, a) -> 

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

**Algorithms (TODO)**

- Bit-pararell
- aho-corasick
- memchr

**Semantics (TODO)**

- Coherent space
