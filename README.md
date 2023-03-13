**TODO**

- Interpretation of `match`, is it a judgement `a : A` or a test that one is divisible by another (quotient is equal to zero) `a / b`?
- Representation of the four units
- TODO Partition of a context, equality between contexts
- Equational reasoning
- Induction
- `True(I, Box<Repr<I>>)` (assertions, dependent types)
- Fixed points
- Spherical conic (tennis ball)

**`Map`**

- The functional equivalent to grouping and capturing.

**`Xor`**

- lookahead/behind, multiple futures, communication and discard, `ignore` combinator
- Split, subspace

**`Add` and `And`**

- Are they better to be `Vec<Repr<I>>` instead of linked list?
- Should they be normalised so they always have (((0, 1), 2), ..) structures? 

**`Rem`** (partial match)

**`Sh`** (shift position?)

**Comparisons**

| regular expressions/<br/>set theory | linear logic/<br/>modal logic | repr | type theory/<br/>category theory | len |
| - | - | - | - | - |
| a ∈ L (match) | `a` : `A` (judgement) | | |
| ∅ | 0 | `Zero` | | |
| `a` | `a` | `One(a)` | | len(a) |
| ε (empty)/{ε} | 1 | `Seq([])` | \*/1 | 0 |
| `.` | | `Interval(MIN, MAX)` | | 1 |
| `ab`/`a` · `b` (concatenation) | `a` ⊗ `b` (multiplicative conjunction/times) | `Mul(a, b)`/`a * b` | ⊗ (tensor product) | len(a) + len(b) |
| `a\|b` (alternation) | `a` ⊕ `b` (additive disjuction/plus) | `Or(a, b)`/`a \| b` | + (coproduct) | max(len(a), len(b))
| `a*` (kleen star) | `!a` (exponential conjunction/of course),<br/>□ (necessity) | `Exp(a)` | μ, fixed point/trace, comonad |
| `a*?` (non greedy) | `?a` (exponential disjunction/why not),<br/>◊ (possibility) | `Exp(a)` | ν, monad |
| `a?` () | `a` | `Or(Zero, a)` | |
| `a{n,m}` (repetition) | `a` | `Or(Mul(a, Mul(a, ..)), Or(..))` | |
| `[a-z]` (class) | | `Interval(a, z)`/`a..z` | | |
| `[^a-z]` (negation) | TODO this is complementary op | `Neg(a)`/`-a` | | |
| `a`<sup>†</sup> (reverse) | right law vs left law | `Rev(a)` | | len(a) |
| `a` / `b` (right quotient) | `a` ⊸ `b` | `Div(a, b)`/`a / b` | | len(a) - len(b) |
| `a` \ `b` (left quotient) | | `Div(a, b)` | | |
| RegexSet | `a` ⅋ `b` (multiplicative disjunction/par) | `Add(a, b)`/`a + b` | ⊕ (direct sum) | |
| `a` ∩ `b` (intersection) | `a` & `b` (additive conjunction/with) | `And(a, b)`/`a & b` | × (product) | |
| `a(?=b)` (positive lookahead) | | `And(a, b)` | | |
| `a(?!b)` (negative lookahead) | | `And(a, Not(b))` | | |
| `(?<=a)b` (positive lookbehind) | | `And(a, b)` | | |
| `(?<!a)b` (negative lookbehind) | | `And(a, b)` | | |

**about symbols**

Symbols are grouped and assigned primarily by additive/multiplicative distinciton. They are corresponding to whether computation exits or rather continues; though concatenation `⊗`/`Mul`/`*` has conjunctive meaning, computation doesn't complete/exit at not satisfying one criterion for there are still different ways of partition to try (backtracking). Though RegexSet `⅋`/`Add`/`+` has disjunctive meaning, computation doesn't complete/exit at satisfying one criterion to return which regex has match. On the other hand, alternation `⊕`/`Or`/`|` and intersection `&`/`And`/`&` early-break, hence order-dependent. When I add `Map` variant to `Repr` to execute arbitrary functions, this order-dependency suddenly becomes to matter for those arbitrary functions can have effects, for example, modification/replacement of input string. (*Effects are additive.*)

| | additive | multiplicative | exponential |
| - | - | - | - |
| positive/extensional | ⊕ 0 | ⊗ 1 | ! |
| negative/intensional | & ⊤ | ⅋ ⊥ | ? |

**Laws**

TODO:

- `I::EMPTY` ε
- `Seq::empty()` - can be empty because negative
- `Interval::full()` - can't be empty because positive

| linear logic/quantale | repr | title |
| - | - | - |
| | Or(a, Or(b, c)) = Or(Or(a, b), c) | Or-associativity |
| | |
| a ⊕ a = a | Or(a, a) = a | Or-idempotence |
| a ⊕ 0 = 0 ⊕ a = a| Or(a, Zero) = Or(Zero, a) = a | Zero, Or-unit |
| a ⊗ 1 = 1 ⊗ a = a | Mul(a, One('')) = Mul(One(''), a) = a | One(''), Mul-unit |
| a ⊗ (b ⊕ c) = (a ⊗ b) ⊕ (a ⊗ c) | Mul(a, Or(b, c)) = Or(Mul(a, b), Mul(a, c)) | right-distributivity |
| (a ⊕ b) ⊗ c = (a ⊗ c) ⊕ (b ⊗ c) | Mul(Or(a, b), c) = Or(Mul(a, c), Mul(b, c)) | left-distributivity |
| a<sup>†</sup> = a | Rev(One(a)) = One(a) | |
| (a & b)<sup>†</sup> = (b<sup>†</sup>) & (a<sup>†</sup>)| Rev(Mul(a, b)) = Mul(Rev(b), Rev(a)) | |
| | Mul(One(a), One(b)) = One(ab) | |
| a ⅋ (b & c) = (a ⅋ b) & (a ⅋ c) | Add(a, And(b, c)) = And(Add(a, b), Add(a, c)) | right-distributivity |
| (a & b) ⅋ c = (a ⅋ c) & (b ⅋ c) | Add(And(a, b), c) = And(Add(a, c), Add(b, c)) | left-distributivity |

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

**Let's not say 'communication'**

- local ↔︎ global
- context solving

![Drawing Hands](https://upload.wikimedia.org/wikipedia/en/b/ba/DrawingHands.jpg)

**Algorithms (TODO)**

- Bit-pararell
- aho-corasick
- memchr

**Semantics (TODO)**

- Coherent space
