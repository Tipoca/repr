# About

By regarding matching as an assignment of occurrences of strings to each part of an expression, regular expressions are resource (limited occurrences of strings) consumption (exclusive assignment/matching of them).

**Kind**

- Decidable (or only complete?)
- Orderd/non-commutative
- Linear/non-deterministic (⊇ regular/deterministic)
- Unitless/promonoidal

**TODO**

- Is there such as selective/projective mul to concatenate only one side of And?
- Representation of the four units, or do we need them? (Decidability is affecting)
  - 1	as Seq::empty(), 𝟏 ≡ νX.X
  - 𝟎 ≡ μX.X
  - ⊤ as Inf(Interval::full())
  - ⊥ as a & b when a ≠ b (?A)
- One as Seq(\['a'\]) vs Interval('a', 'a')
- Relatipnship between nominality, input-ness/string, and function arguments/abstraction, the Integral trait and the De Bruijn index
- TextStart and TextEnd as left / right units
- Interpretation of `match`, is it a judgement `a : A` or a test that one is divisible by another (quotient is equal to zero) `a / b`, one (string) is contained by another (regex) a → b? Proof search
- Two types of negations for each positive connectives: rev and div for Seq, not and sub for Interval
- a.le(b) → a.and(b) = a, a.add(b) = b
- Equational reasoning, bisimulation
- Induction
- True(Seq\<I\>, Box\<Repr\<I\>\>), assertions, dependent types, backreference
- action a.P, a ⊙ P, guard, scalar product
- Normalisation vs equality rules
- characteristic function/morphism
- weighted limit, weighted colimit, end, coend
- parameterise some laws as features
- Spherical conic (tennis ball)

**`Xor`**

- lookahead/behind, multiple futures, communication and discard, `ignore` combinator
- Split, subspace

## Correspondence

| regular&nbsp;expressions&nbsp;/<br/>set theory | linear logic | repr | type theory&nbsp;/<br/>category&nbsp;theory&nbsp;/<br/>coalgebra | len | process calculus | probability&nbsp;theory&nbsp;/<br/>learning&nbsp;theory | quantum&nbsp;theory |
| - | - | - | - | - | - | - | - |
| a ∈ L (match) | | | a : A (judgement) | |
| $∅$ | $0$ (additive falsity) | Zero | 𝟎 (empty type) | | nil, STOP |
| | $⊤$ (additive truth) | True | | | |
| a | a | One(Seq(a)) | | len(a) | (sequential composition, prefix) |
| $ε$ (empty) ∈ {ε} | $1$ (multiplicative truth) | Seq(\[\]) | \* : 𝟏 (unit type) | 0 | SKIP |
| . | | Interval(MIN, MAX) | | 1 |
| ab / $a · b$ (concatenation) | $a ⊗ b$ (multiplicative conjunction/times) | Mul(a, b) | $a ⊗ b$ (tensor product) | len(a) + len(b) | P \|\|\| Q (interleaving) |
| a\|b (alternation),<br/>$a ∪ b$ (union) | $a ⊕ b$ (additive disjuction/plus) | Or(a, b) | $a + b$ (coproduct) | max(len(a), len(b)) | (deterministic choice) |
| a* (kleen star),<br/>..\|aa\|a\|ε | | | $μX.𝟏 + (L(a) ⊗ X)$ | | |
| TODO | $!a$ (exponential conjunction/of course),<br/>νX.1 & a & (X ⊗ X) | Inf(a) | ν (fixed point/trace, comonad, final coalgebra) | | (replication) |
| a*? (non greedy) | | | | |
| TODO | $?a$ (exponential disjunction/why not),<br/>$µX.⊥ ⊕ a ⊕ (X ⅋ X)$ | Sup(a) | μ (monad, initial algebra) | |
| a? | a + 1 | Or(Zero, a) | |
| a{n,m} (repetition) | a | Or(Mul(a, Mul(a, ..)), Or(..)) | |
| \[a-z\] (class) | | Interval(a, z) | | |
| `[^a-z]` (negation) | TODO this is complementary op | `Neg(a)`/`-a` | | |
| a<sup>†</sup> (reverse) | right law vs left law | a.rev() | | len(a) |
| $a / b$ (right quotient) | $a ⊸ b$ | Div(a, b) | | len(a) - len(b) | (hiding) |
| a \ b (left quotient) | | `Div(a, b)` | | | (hiding) |
| RegexSet | a ⅋ b (multiplicative disjunction/par) | Add(a, b) | $a ⊕ b$ (direct sum) | | (nondeterministic choice) |
| $a ∩ b$ (intersection) | a & b (additive conjunction/with) | And(a, b) | $a × b$ (product) | | (interface parallel) |
| `a(?=b)` (positive lookahead) | | And(a, b) | | |
| `a(?!b)` (negative lookahead) | | And(a, Not(b)) | | |
| `(?<=a)b` (positive lookbehind) | | And(a, b) | | |
| `(?<!a)b` (negative lookbehind) | | And(a, b) | | |
| $a ⊆ b, a ≤ b$ (containmemt) | $a ≤ b (≃ a = b ⅋ a < b)$ | a.le(b) |
| | a<sup>⊥</sup> (dual) | a.dual() |
| a = b (equality) | | | a = b (identity type) |

### About symbols

Symbols are grouped and assigned primarily by additive/multiplicative distinciton. They are corresponding to whether computation exits or rather continues; though concatenation `⊗`/`Mul`/`*` has conjunctive meaning, computation doesn't complete/exit at not satisfying one criterion for there are still different ways of partition to try (backtracking). Though RegexSet `⅋`/`Add`/`+` has disjunctive meaning, computation doesn't complete/exit at satisfying one criterion to return which regex has match. On the other hand, alternation `⊕`/`Or`/`|` and intersection `&`/`And`/`&` early-break, hence order-dependent. When I add `Map` variant to `Repr` to execute arbitrary functions, this order-dependency suddenly becomes to matter for those arbitrary functions can have effects, for example, modification/replacement of input string. (*Effects are additive.*)

| | additive | multiplicative | exponential |
| - | - | - | - |
| positive/extensional | $⊕$ $0$ Or | $⊗$ $1$ Mul | ! |
| negative/intensional | & ⊤ And | ⅋ ⊥ Add | ? |

## Properties/Coherence

- All connectives are associative
- Additive connectives are commutative and idempotent

TODO:

- Seq::empty(), ε - can be empty because negative
- Interval::full() - can't be empty because positive

| name | regular expressions | linear logic | type theory | repr |
| - | - | - | - | - |
| or-associativity | $a \| (b \| c) = (a \| b) \| c$ | $a ⊕ (b ⊕ c) = (a ⊕ b) ⊕ c$ | | Or(a, Or(b, c)) = Or(Or(a, b), c) |
| | | | |
| or-idempotence | $a \| a = a$ | $a ⊕ a = a$ | | Or(a, a) = a |
| or-unit | | $a ⊕ 0 = 0 ⊕ a = a$ | | Or(a, Zero) = Or(Zero, a) = a |
| mul-unit | $a · ε = ε · a = a$ | $a ⊗ 1 = 1 ⊗ a = a$ | | Mul(a, One('')) = Mul(One(''), a) = a |
| right-distributivity| $a · (b \| c) = (a · b) \| (a · c)$ | $a ⊗ (b ⊕ c) = (a ⊗ b) ⊕ (a ⊗ c)$ | | Mul(a, Or(b, c)) = Or(Mul(a, b), Mul(a, c)) |
| left-distributivity | $(a \| b) · c = (a · c) \| (b · c)$ | $(a ⊕ b) ⊗ c = (a ⊗ c) ⊕ (b ⊗ c)$ | | Mul(Or(a, b), c) = Or(Mul(a, c), Mul(b, c)) |
| | $ε^† = ε$ | | | |
| | | (a & b)<sup>†</sup> = (b<sup>†</sup>) & (a<sup>†</sup>)| | Rev(Mul(a, b)) = Mul(Rev(b), Rev(a)) |
| | | | | Mul(One(a), One(b)) = One(ab) |
| right-distributivity | | a ⅋ (b & c) = (a ⅋ b) & (a ⅋ c) | | Add(a, And(b, c)) = And(Add(a, b), Add(a, c)) |
| left-distributivity | | $(a \& b) ⅋ c = (a ⅋ c) \& (b ⅋ c)$ | | Add(And(a, b), c) = And(Add(a, c), Add(b, c)) |
| and-idempotence | | $a \& a = a$ | | And(a, a) = a |
| exponential | $(a \& b)* = a* · b*$ | $!(A \& B) ≣ !A ⊗ !B$ | | | |
| exponential | $(a \| b)? = a? ⅋ b?$ | $?(A ⊕ B) ≣ ?A ⅋ ?B$ | | | |

Relationship among additive, multiplicative and exponential

- exp(a + b) = exp(a) * exp(b)

Linearity (which)

- f(a + b) = f(a) + f(b)
- (a → b) + (b → a)  (non-constructive)
- functions take only one argument
- presheaves of modules

**𝜕, derivation**

| math | repr |
| - | - |
| $𝜕(a ⊗ b) ≃ 𝜕(a) ⊗ b + a ⊗ 𝜕(b)$ | der(Mul(a, b)) = Or(Mul(der(a), b), Mul(a, d(b))) |

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

**Algorithms (TODO)**

- Bit-pararell
- aho-corasick
- Boyer–Moore
- memchr

**Semantics (TODO)**

- Coherent space

**References**

- Completeness for Categories of Generalized Automata, Guido Boccali, Andrea Laretto, Fosco Loregian, Stefano Luneia, 2023 [[arxiv](https://arxiv.org/abs/2303.03867)]
- Beyond Initial Algebras and Final Coalgebras, Ezra Schoen, Jade Master, Clemens Kupke, 2023 [[arxiv](https://arxiv.org/abs/2303.02065)]
- The Functional Machine Calculus, Willem Heijltjes, 2023 [[arxiv](https://arxiv.org/abs/2212.08177)]
- The Functional Machine Calculus II: Semantics, Chris Barrett, Willem Heijltjes, Guy McCusker, 2023 [[arxiv](https://arxiv.org/abs/2211.13140)]
- Differential 2-Rigs, Fosco Loregian, Todd Trimble, 2022 [[arxiv](https://arxiv.org/abs/2103.00938)]
- On the Pre- and Promonoidal Structure of Spacetime, James Hefford, Aleks Kissinger, 2022 [[arxiv](https://arxiv.org/abs/2206.09678)]
- Diagrammatic Differentiation for Quantum Machine Learning, Alexis Toumi, Richie Yeung, Giovanni de Felice, 2021 [[arxiv](https://arxiv.org/abs/2103.07960)]
- Coend Calculus, Fosco Loregian, 2020 [[arxiv](https://arxiv.org/abs/1501.02503)]
- Proof Equivalence in MLL Is PSPACE-Complete, Willem Heijltjes, Robin Houston, 2016 [[arxiv](https://arxiv.org/abs/1510.06178)]
- Linear Logic Without Units, Robin Houston, 2013 [[arxiv](https://arxiv.org/abs/1305.2231)]
- Imperative Programs as Proofs via Game Semantics, Martin Churchill, Jim Laird, Guy McCusker, 2013 [[arxiv](https://arxiv.org/abs/1307.2004)]
- Regular Expression Containment as a Proof Search Problem, Vladimir Komendantsky, 2011 [[inria](https://hal.inria.fr/inria-00614042)]
