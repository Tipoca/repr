**Kind**

- Decidable
- Non-commutative/orderd
- Linear/non-deterministic (⊇ regular/deterministic)

**TODO**

- TextStart and TextEnd as left / right units
- Interpretation of `match`, is it a judgement `a : A` or a test that one is divisible by another (quotient is equal to zero) `a / b`? Proof search
- Representation of the four units
- Intensional/extensional commutativity/idempotence
- a.le(b) → a.and(b) = a, a.add(b) = b 
- Equational reasoning, bisimulation
- Induction
- True(Seq\<I\>, Box\<Repr\<I\>\>), assertions, dependent types, backreference
- action a.P, a ⊙ P, guard, scalar product
- Normalisation
- characteristic function/morphism
- parameterise some laws as features
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

| regular&nbsp;expressions&nbsp;/<br/>set theory | linear logic | repr | type theory&nbsp;/<br/>category&nbsp;theory | len | process calculus | probability&nbsp;theory&nbsp;/<br/>learning&nbsp;theory | quantum&nbsp;theory |
| - | - | - | - | - | - | - | - |
| a ∈ L (match) | | | a : A (judgement) | |
| ∅ | 0 | | | | nil, STOP |
| | ⊤ | True | | | | 
| a | a | One(Seq(a)) | | len(a) | (sequential composition, prefix) |
| ε (empty) ∈ {ε} | 1 | Seq(\[\]) | \* : 1 | 0 | SKIP | 
| . | | Interval(MIN, MAX) | | 1 |
| ab/a · b (concatenation) | a ⊗ b (multiplicative conjunction/times) | Mul(a, b) | ⊗ (tensor product) | len(a) + len(b) | P \|\|\| Q (interleaving) |
| a\|b (alternation) | a ⊕ b (additive disjuction/plus) | Or(a, b) | + (coproduct) | max(len(a), len(b)) | (deterministic choice) |
| a* (kleen star),<br/>..\|aa\|a\|ε | !a (exponential conjunction/of course),<br/>νX.1 & a & (X ⊗ X) | Inf(a) | ν, fixed point/trace, comonad, final coalgebra | | (replication) |
| a*? (non greedy),<br/>ε\|a\|aa\|.. | ?a (exponential disjunction/why not),<br/>µX.⊥ ⊕ a ⊕ (X ⅋ X) | Sup(a) | μ, monad, initial algebra | |
| a? | a + 1 | Or(Zero, a) | |
| a{n,m} (repetition) | `a` | Or(Mul(a, Mul(a, ..)), Or(..)) | |
| \[a-z\] (class) | | Interval(a, z) | | |
| `[^a-z]` (negation) | TODO this is complementary op | `Neg(a)`/`-a` | | |
| a<sup>†</sup> (reverse) | right law vs left law | a.rev() | | len(a) |
| a / b (right quotient) | a ⊸ b | Div(a, b) | | len(a) - len(b) | (hiding) |
| a \ b (left quotient) | | `Div(a, b)` | | | (hiding) |
| RegexSet | a ⅋ b (multiplicative disjunction/par) | Add(a, b) | ⊕ (direct sum) | | (nondeterministic choice) |
| a ∩ b (intersection) | a & b (additive conjunction/with) | And(a, b) | × (product) | | (interface parallel) |
| `a(?=b)` (positive lookahead) | | And(a, b) | | |
| `a(?!b)` (negative lookahead) | | And(a, Not(b)) | | |
| `(?<=a)b` (positive lookbehind) | | And(a, b) | | |
| `(?<!a)b` (negative lookbehind) | | And(a, b) | | |
| a ⊆ b | a ≤ b (≃ a = b ⅋ a < b) | a.le(b) |

**about symbols**

Symbols are grouped and assigned primarily by additive/multiplicative distinciton. They are corresponding to whether computation exits or rather continues; though concatenation `⊗`/`Mul`/`*` has conjunctive meaning, computation doesn't complete/exit at not satisfying one criterion for there are still different ways of partition to try (backtracking). Though RegexSet `⅋`/`Add`/`+` has disjunctive meaning, computation doesn't complete/exit at satisfying one criterion to return which regex has match. On the other hand, alternation `⊕`/`Or`/`|` and intersection `&`/`And`/`&` early-break, hence order-dependent. When I add `Map` variant to `Repr` to execute arbitrary functions, this order-dependency suddenly becomes to matter for those arbitrary functions can have effects, for example, modification/replacement of input string. (*Effects are additive.*)

| | additive | multiplicative | exponential |
| - | - | - | - |
| positive/extensional | ⊕ 0 | ⊗ 1 | ! |
| negative/intensional | & ⊤ | ⅋ ⊥ | ? |

**Laws/Coherence**

TODO:

- Seq::empty(), ε - can be empty because negative
- Interval::full() - can't be empty because positive

| regular expressions | linear logic/quantale | repr | title |
| - | - | - | - |
| a \| (b \| c) = (a \| b) \| c | a ⊕ (b ⊕ c) = (a ⊕ b) ⊕ c | Or(a, Or(b, c)) = Or(Or(a, b), c) | Or-associativity |
| | | |
| a \| a = a | a ⊕ a = a | Or(a, a) = a | Or-idempotence |
| | a ⊕ 0 = 0 ⊕ a = a| Or(a, Zero) = Or(Zero, a) = a | Zero, Or-unit |
| a · ε = ε · a = a | a ⊗ 1 = 1 ⊗ a = a | Mul(a, One('')) = Mul(One(''), a) = a | One(''), Mul-unit |
| a · (b \| c) | a ⊗ (b ⊕ c) = (a ⊗ b) ⊕ (a ⊗ c) | Mul(a, Or(b, c)) = Or(Mul(a, b), Mul(a, c)) | right-distributivity |
| | (a ⊕ b) ⊗ c = (a ⊗ c) ⊕ (b ⊗ c) | Mul(Or(a, b), c) = Or(Mul(a, c), Mul(b, c)) | left-distributivity |
| ε<sup>†</sup> = ε | | | |
| | (a & b)<sup>†</sup> = (b<sup>†</sup>) & (a<sup>†</sup>)| Rev(Mul(a, b)) = Mul(Rev(b), Rev(a)) | |
| | | Mul(One(a), One(b)) = One(ab) | |
| | a ⅋ (b & c) = (a ⅋ b) & (a ⅋ c) | Add(a, And(b, c)) = And(Add(a, b), Add(a, c)) | right-distributivity |
| | (a & b) ⅋ c = (a ⅋ c) & (b ⅋ c) | Add(And(a, b), c) = And(Add(a, c), Add(b, c)) | left-distributivity |
| | a ⅋ a = a | Add(a, a) = a | Add-idempotence |
| | a & a = a | And(a, a) = a | And-idempotence |

Relationship among additive, multiplicative and exponential

- exp(a + b) = exp(a) * exp(b)

Linearity (which)

- f(a + b) = f(a) + f(b)
- (a → b) + (b → a)  (non-constructive)
- functions take only one argument

**Derivative (TODO)**

- d(Zero) = Zero
- d(Or(a, b)) = Or(d(a), d(b))
- d(Mul(a, b)) = Or(Mul(d(a), b), Mul(a, d(b))  *
- d(Inf(a)) = Mul(d(a), Inf(a))
- a : D(a)
- 

**True**

- And(True, a) -> 

**Stream processor**

- νX.μY. (A → Y) + B × X + 1

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
- context solving/fitting/providing with each other

![Drawing Hands](https://upload.wikimedia.org/wikipedia/en/b/ba/DrawingHands.jpg)

**Algorithms (TODO)**

- Bit-pararell
- aho-corasick
- Boyer–Moore
- memchr

**Semantics (TODO)**

- Coherent space
