# 2026-04-25 Lambda lifting sibling-capture defect — postmortem and prevention

## The defect (one paragraph)

`LambdaLifting.LiftLocalFunction` and `LambdaLifting.LiftNamedLambda` rewrote
references to *sibling* let-bound functions in two cooperating but incompatible
ways:

1. The free-variable computation **excluded** every sibling whose name was in
   `localFunctionLiftedNames` (the set of let-bound functions that would be
   lifted out of the same let-block). The intent: "the sibling will be lifted,
   so the surrounding lifted function does not need to capture it."
2. `SubstituteVariableReferences` then **replaced** every reference to such a
   sibling with the bare lifted top-level name (e.g. `inner` → `compute__lifted__inner_1`).

That pair is correct only when the sibling has *no external captures*. As soon
as the sibling captures something (e.g. `inner _ = Just (sl, "doc")` where `sl`
is bound in the enclosing scope), the let-binding produced by step (1) is

```
inner = compute__lifted__inner_1 sl       -- partial application
```

and a sibling call site `inner name` should evaluate that partial application
applied once more to `name`. The substitution in step (2) instead rewrote the
call site to

```
compute__lifted__inner_1 name             -- bypasses the partial application
```

so `compute__lifted__inner_1` was invoked with `name` (the original argument)
where it expected `sl` (the captured value). Pine has no runtime type checking,
so the function body silently operated on the wrong-typed value and produced
plausible-looking but corrupted output (the user-visible symptom was an empty
`TextDocumentReferencesResponse []` from the language service).

The fix classifies each sibling as "substitutable" only if it has no external
captures (computed as a *greatest* fixed point so that mutually-recursive
sibling functions that have no other external captures remain substitutable
together). Siblings with external captures are now captured normally through
the let binding.

## Why the bug stayed hidden

Several otherwise-strong test layers each missed it for an understandable but
addressable reason:

| Layer                                                | What it checked                                          | Why it missed the defect                                                                                                                                              |
| ---------------------------------------------------- | -------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `LambdaLiftingTests` (e.g. `Two_local_functions_…`)  | Exact textual equality of the *lifted source*            | Tests covered only the no-capture case, where substitute-with-bare-name happens to be observationally equivalent to capture-via-let-binding.                          |
| End-to-end Elm interpreter tests                     | Functional behavior of the language service              | The language-service Elm code path was exercised, but the bug only fires once a sibling captures a complex value of a particular shape; the existing scenarios didn't trigger it on the interpreter side. The compiled-VM side did, but the test only flagged "result differs" without explaining why. |
| Pine VM execution                                    | Runtime evaluation                                       | Pine is dynamically typed at the value level (lists/blobs). Calling a function with a wrongly-typed first argument is not an error — the body just produces nonsense. |
| Differential test (interpreter vs compiled VM)       | Result equality                                          | This *did* catch the symptom — that is how we noticed something was wrong — but the diagnosis required ~2 days of bisection because the symptom (an empty list) was far from the root cause (a bad rewrite of a single call site). |

## Could a stricter interpreter have caught it?

**Yes — with caveats.** The bug is a classic "dynamic-typing-eats-the-error"
story. Several runtime invariants would have surfaced it immediately:

### 1. Arity / call-shape checking on lifted functions

After lambda lifting, every `Application` whose head is a top-level function
named `*__lifted__*` has a known fully-saturated arity (`captures.Count +
originalParams.Count`). If the interpreter (or, better, a *post-lifting
validation pass*; see below) enforced "either fully saturated or wrapped in a
single let-binding partial application", the substituted call site would have
failed loudly: `compute__lifted__inner_1` is declared as a 2-ary function but
the bad call site applies it to 1 argument and uses the result as a
`LocationInFile`.

This is cheap to check (one pass over the lowered AST). A few subtleties:

- **Currying.** Pine encodes Elm functions as curried; a "1-of-2 application"
  *should* produce a function value, not be an error. The check has to look at
  the *use* of the application, not the application itself. Equivalently:
  enforce that every lifted function is referenced only via the let-binding
  partial application that the lifter generates, never as a bare name in an
  expression that is not itself the let-binding's RHS. This is a syntactic
  invariant, not a runtime one.

- **Self-reference exception.** Inside a lifted function's body, the
  *self*-substitution produces an unsaturated reference to the lifted name.
  That is fine: it is consumed by an enclosing `Application` that supplies the
  remaining arguments. The check needs to be "every reference to a lifted name
  ends up as the head of a fully-saturated application by the time it
  syntactically leaves the lifted function or the surrounding let-binding".

### 2. Runtime parameter-type tagging

If function values carried a structural fingerprint of their first parameter
(or their declared first-parameter type from the source) and the interpreter
checked it on application, the call

```text
compute__lifted__inner_1 "x"      -- expects LocationInFile, got String
```

would fail with a clear "expected `LocationInFile`, got `String`" message. The
runtime cost of the check is small (one comparison per application) but the
ergonomic cost — adding type tags to every value — is large and runs counter
to Pine's deliberate uniform-value design. **Verdict:** would catch it, but the
cost is disproportionate.

### 3. Reference-interpreter cross-check on every emission

The pre-existing differential strategy (run the Elm interpreter on the same
program and compare results) *did* catch the symptom. What it failed to do was
*localize* it. Adding the cross-check at a finer granularity — for every
top-level declaration, evaluate one canonical witness via both paths and
compare — would have flagged precisely the lifted helper that diverged. This
is the cheapest catch-strategy to add and the one most aligned with how the
codebase already tests itself.

## Could a validation pass on lowered syntax have caught it?

**Yes, and this is the recommended primary defense.** The defect is a pure
*syntactic* invariant violation in the output of `LambdaLifting.LiftLambdas`.
It does not require running the program to detect. Concretely, a validator
that walked the lifted module and verified the following invariants would have
rejected the buggy output:

### Invariant L1 — every lifted function is referenced exactly through its let-binding partial application

For every lifted top-level function `M.f__lifted__b_n` produced from a
let-bound function `b` in `M.f`'s body:

- The let-block where `b` was originally declared must contain a 0-argument
  let-binding `b = M.f__lifted__b_n c1 c2 … ck` whose RHS is a partial
  application supplying exactly the captures that `M.f__lifted__b_n` declared.
- *No other* expression in the lifted module — neither in `M.f` nor in any
  other lifted function — may reference `M.f__lifted__b_n` directly, except
  for the *self* reference inside `M.f__lifted__b_n`'s own body and inside
  any sibling `M.f__lifted__b'_n'` that is permitted by the
  "sibling-without-external-captures" rule.

The buggy lifter violated this: a sibling lifted function's body referenced
`M.f__lifted__b_n` in a position where its captures were not supplied.

### Invariant L2 — capture parameters of every lifted function exactly equal the lifted body's free variables modulo lambda params and substitutable-sibling lifted names

If the lifter produces `M.f__lifted__b_n cap1 cap2 lam1 lam2 = body`, then

```
freeVars(body) ∖ {cap1, cap2, lam1, lam2} ⊆ S
```

where `S` is the set of sibling lifted names that have been classified as
substitutable. Anything else is, by construction, a name that the body cannot
resolve.

The buggy lifter violated this in the *opposite* direction: the body of one
lifted function contained a substituted reference to a sibling lifted name
that should not have been in `S` (because that sibling had captures), yet the
free variables of the body did not include the sibling under its original
name either. The validator would have rejected the substituted body as
referring to a name that was neither captured nor a substitutable sibling.

### Invariant L3 — saturation at every call site of a lifted name

Every `Application` whose head (after walking through `Application` chains
left-spine) is a `FunctionOrValue` referring to a lifted name must supply
exactly `captureCount + lambdaArity` arguments along that spine.

This is a stronger version of L1 and is also a pure syntactic check.

### What would such a pass cost?

- Implementation: ~150–250 lines in a new `LambdaLiftingValidator.cs`. One
  recursive walk of the lifted module, threading the lifted-functions
  metadata that `LiftLambdas` already produces.
- Runtime: O(size of lowered module). Negligible compared to the rest of
  compilation.
- False positives: zero, if the invariants are stated as above. Each
  invariant is a property the lifter is *meant* to maintain by construction;
  the validator just refuses to trust the lifter.
- Failure mode: when a future change to lambda lifting (or any subsequent
  pass that rewrites lifted bodies) breaks an invariant, the failure happens
  at compile time with a precise pointer to the offending name and call site,
  not as a silent miscompilation surfacing a week later in an unrelated
  language-service feature.

## Recommended next steps (in order of cost/benefit)

1. **Add `LambdaLiftingValidator`** that enforces L1, L2, L3 on the output of
   `LambdaLifting.LiftLambdas` and is run automatically (gated by an assert in
   debug builds, or always-on if cheap) at the end of the lifting stage.
   This single pass would have caught the present defect at the moment it was
   introduced.

2. **Extend the existing `LambdaLiftingTests` to cover the captured-sibling
   case** with both:
   - a syntactic expected-output test (mirroring
     `Two_local_functions_where_one_invokes_the_other`), and
   - a *semantic* test that compiles the lifted program and the original
     program and asserts they produce the same value on a chosen input.

   The synthetic regression probes added alongside this fix
   (`Probe_two_letFns_inner_returns_just_pair`,
   `Probe_two_letFns_inner_returns_plain`, etc.) are the semantic tests; they
   should be retained as-is.

3. **Promote the differential interpreter-vs-VM check from end-to-end tests
   into the per-declaration test harness** for hand-picked complex
   declarations. The differential check is the cheapest defense against any
   class of compiler defect that produces well-typed-but-wrong output and
   should run for every nontrivial declaration in the elm-in-elm
   self-bootstrap.

4. **Document the lifter's invariants in `LambdaLifting.cs`** so the next
   modification of `freeVariables`/`substitutions` is forced to consider
   them.

Items 1 and 4 are the biggest wins per unit of effort and would jointly turn
this whole class of defect (silent miscompilation from incorrectly-rewritten
sibling references) into a compile-time error in the future.
