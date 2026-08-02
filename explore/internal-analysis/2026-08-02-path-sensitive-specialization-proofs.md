# Path-sensitive specialization proofs

## Scope

This note records the lessons from specializing `String.slice` when its caller
has already established that the start offset is non-negative and the end offset
is the start offset plus one.

## What the optimization needed

The generic `String.slice` implementation checks that the offsets form an
ascending sequence before selecting its fast branch. For
`String.slice offset (offset + 1) source`, both parts of that check are available
at the call site:

- the surrounding true branch establishes `0 <= offset`;
- the semantics of `int_add` establish `offset <= offset + 1`.

Bottom-up reduction alone cannot combine these facts. It sees the inner bounds
check without the condition that controls entry into the branch. The reducer
therefore now specializes each conditional branch under the value assumed for
the condition. In the true branch it can reuse ascending-order pairs from the
outer condition and prove an additional pair when the right side is the left
side plus non-negative integer literals.

The proof is intentionally conservative. It requires the shared operand to be
known to decode as an integer, accepts only facts implied by a true ascending
integer check, and declines negative or unknown addends. Failure to prove a
predicate preserves the original expression.

## Learnings

### Optimization context is part of expression meaning

An expression can be irreducible in isolation and constant inside a particular
control-flow path. Consequently, caches and rewrite APIs must distinguish
context-free reductions from reductions made under assumptions. A specialized
result must not be reused on a path where its assumptions do not hold.

### Builtin semantics should be modeled explicitly

Syntactic similarity is not enough to prove arithmetic relationships. The proof
for `x <= x + c` depends on `int_add`, on `c >= 0`, and on `x` being a valid
integer value. Modeling failure behavior is as important as modeling the common
integer case because Pine builtins can return non-boolean sentinel values for
invalid inputs.

### Small proof rules compose into useful specializations

The optimization does not need a general theorem prover. It needs three small
capabilities: extract order facts from a successful predicate, derive a new
order fact from a builtin rule, and replace a predicate only after every required
pair is proven. These rules remove the whole generic bounds branch after normal
reduction and inlining expose the relevant expression.

### Negative tests define the soundness boundary

The focused reducer snapshots include both the successful `offset + 1` case and
a negative-addend case that must remain unchanged. Such paired tests document
not only what the optimizer recognizes, but also what it must refuse to infer.
The sequential-IR snapshot then verifies that the proof removes the intended
runtime branch in the compiled Elm program.

## A scalable proof system

Further specializations should use a dedicated, immutable proof context rather
than adding unrelated pattern checks to branch reduction:

1. **Normalize facts.** Represent boolean values, integer validity, equality,
   and integer order as explicit fact records. Canonicalize commutative builtin
   inputs and integer affine forms before storing facts.
2. **Separate extraction from derivation.** Predicate extractors translate a
   known branch condition into seed facts. Builtin-specific transfer rules derive
   facts such as monotonicity, ranges, lengths, and tag shapes.
3. **Track provenance.** Every derived fact should reference its assumptions and
   rule. Provenance supports diagnostics, prevents accidental context-free cache
   reuse, and makes proof failures explainable in snapshots.
4. **Use path-local immutable contexts.** Fork the context at conditionals, add
   the condition or its negation, and merge only facts valid on every incoming
   path. Include a stable context identity in memoization keys.
5. **Provide a query interface.** Rewriters should ask questions such as
   `IsTrue(predicate)`, `IsInteger(expression)`, or
   `Compare(left, right)`, without knowing which rules established the answer.
6. **Bound inference.** Use work queues, rule budgets, and compact normalized
   domains so proof search remains predictable. Unknown must always be a normal
   result, never a reason to guess.
7. **Validate rules independently.** Give each builtin rule focused positive,
   negative, malformed-input, and boundary snapshots. Keep end-to-end IR and
   application snapshots for interactions between inlining, reduction, and
   code generation.

This structure lets new proofs be added as local semantic rules while keeping
path handling, caching, diagnostics, and soundness policy centralized.
