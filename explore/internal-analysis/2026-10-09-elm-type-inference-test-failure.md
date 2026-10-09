# Why the lazy Project / getType test fails in Pine

## Conclusion

**This is a defect in Pine's compiler-generated implementation of generic
`Basics.compare`, exposed by VM inlining.** Its list-comparison fallback compares
unequal elements as signed integers rather than recursively as Elm comparables.
For module names such as `["A"]` and `["B"]`, that produces the wrong ordering.
A dictionary lookup consequently misses a key that is actually present.

The displayed `RangeNotFound` is misleading: in this reproduction it is
constructed by the **test helper before calling `Elm.TypeInference.getType`**.
It does not demonstrate broken Project threading or missing inferred ranges.

The bundled Elm `Basics.compareList` and the native C# comparison leaf both
implement recursive comparison correctly. The defect is in the generated Pine
fallback in `CoreBasics.cs`, not those implementations. Merely changing the
upstream test or swapping its parser dependency would not address the cause.

The investigation below describes the original defect at the pinned repository
revision. The follow-up implementation and regression results are recorded at
the end of this document.

## Versions and reproduction

Investigated against:

- Pine repository commit
  [`34d5791629fd84216613f3342bc686a0ec08694c`](https://github.com/Viir/super-duper-disco/tree/34d5791629fd84216613f3342bc686a0ec08694c),
  with the CLI built from source using .NET SDK `10.0.401`.
- Upstream project revision
  [`aee4a2fc8fd7f8380e97007fb1296115b0e5ca7f`](https://github.com/Janiczek/elm-syntax-type-inference/tree/aee4a2fc8fd7f8380e97007fb1296115b0e5ca7f).
- Screenshot seed `3082049773` and fuzz count `100`.

The isolated reproduction is:

```sh
pine elm test \
  "https://github.com/Janiczek/elm-syntax-type-inference/tree/aee4a2fc8fd7f8380e97007fb1296115b0e5ca7f" \
  --filter "getType threads the updated Project" \
  --seed 3082049773 --fuzz 100
```

It failed with the same `Err { declarationNames = [], details = RangeNotFound,
moduleName = ["A"] }` as the screenshot. This is a `Test.test`, not a fuzz test;
the seed is retained for reproducibility, not because a random input causes it.

For a control, the unchanged pinned upstream project's existing
`npm run test:unit -- --seed 3082049773 --fuzz 100` command passed **all 559 tests**
using its configured Elm `0.19.2` and `elm-test 0.19.2-1`.
That control uses the upstream toolchain and dependency resolver, not Pine's
substitutions; it is not by itself a compiler-only differential. The direct
comparison experiments below provide that isolation.

## What actually fails

The fixture has an import chain `Main -> B -> A`, plus an unrelated broken
module. `A` declares `value : Int`, with body `1`. The test queries `A.value`
first, then queries `Main.main` using the Project returned by the first query.
It checks both results with `Expect.ok`.

Sources:
[fixture](https://github.com/Janiczek/elm-syntax-type-inference/blob/aee4a2fc8fd7f8380e97007fb1296115b0e5ca7f/tests/TypeInferenceTests.elm#L5300-L5343),
[test](https://github.com/Janiczek/elm-syntax-type-inference/blob/aee4a2fc8fd7f8380e97007fb1296115b0e5ca7f/tests/TypeInferenceTests.elm#L5451-L5466).

There are two possible producers of this error:

1. The test's `lazyGetType` helper synthesizes `RangeNotFound` when
   `lazyDeclRange` cannot find the declaration.
2. The library's `lookupRange` reports it when an inferred table lacks the
   requested range.

Instrumenting those two branches in a temporary source copy identified
**the first branch**. Further instrumentation showed:

| Observation inside the failing helper | Result |
| --- | --- |
| `Dict.keys files` | `[["A"],["B"],["Broken"],["Main"]]` |
| `Dict.get ["A"] files` | `Nothing` |

Thus declaration-name filtering and range lookup are not even reached for A.
The test helper maps an unsuccessful module dictionary lookup to the generic
`RangeNotFound` error.

Sources:
[declaration lookup](https://github.com/Janiczek/elm-syntax-type-inference/blob/aee4a2fc8fd7f8380e97007fb1296115b0e5ca7f/tests/TypeInferenceTests.elm#L5364-L5389),
[helper error](https://github.com/Janiczek/elm-syntax-type-inference/blob/aee4a2fc8fd7f8380e97007fb1296115b0e5ca7f/tests/TypeInferenceTests.elm#L5405-L5418),
[library error](https://github.com/Janiczek/elm-syntax-type-inference/blob/aee4a2fc8fd7f8380e97007fb1296115b0e5ca7f/src/Elm/TypeInference.elm#L815-L829).

`parseModules` builds this dictionary by parsing each source and inserting the
File under its parsed module name. These keys are `List String`, not integer
keys. A smaller test retaining just that parsing/insertion/lookup sequence also
failed: literal `["A"]` was not retrievable, although retrieving the keys
enumerated by `Dict.keys` succeeded. A singleton parsed A dictionary and
literal-only dictionary controls passed. These controls initially suggest a
representation problem, but the comparator differential below identifies a
more fundamental discrepancy between execution paths.

Source:
[parseModules](https://github.com/Janiczek/elm-syntax-type-inference/blob/aee4a2fc8fd7f8380e97007fb1296115b0e5ca7f/tests/Tests/Elm/TypeInference/Helpers.elm#L151-L169).

## Root cause: incompatible comparison implementations

Pine has multiple relevant implementations of core comparison:

| Implementation | Unequal list elements |
| --- | --- |
| Bundled Elm `Basics.compareList` | Recursively calls generic `compare` |
| Native `CoreBasicsPrecompiledLeaves.CompareLists` | Recursively calls `BasicsCompare` |
| Generated `CoreBasics.CompareListRecursive` | Calls `int_is_sorted_asc` on the elements |

The third implementation is wrong for a generic `List comparable`.

In `CoreBasics.cs`, `CompareListRecursive` first checks structural equality of
the heads. For unequal heads, its `headALessThanB` expression applies
`BuiltinIntIsSortedAsc(headA, headB)`. It returns `LT` only if that succeeds as
true; otherwise it returns `GT`.

An Elm string is a tagged Pine list, not a signed-integer blob.
`int_is_sorted_asc` returns an empty list for malformed integer inputs, so
unequal string heads take the non-true branch **in either direction**.
Consequently, both `["A"]` versus `["B"]` and the reverse can compare as `GT`.
Equality still gives `EQ`, masking the problem in equality-only checks.

Sources:
[incorrect generated comparison](https://github.com/Viir/super-duper-disco/blob/34d5791629fd84216613f3342bc686a0ec08694c/implement/Pine.Core/Elm/ElmCompilerInDotnet/CoreLibraryModule/CoreBasics.cs#L4143-L4209),
[integer builtin](https://github.com/Viir/super-duper-disco/blob/34d5791629fd84216613f3342bc686a0ec08694c/implement/Pine.Core/BuiltinFunction.cs#L356-L382),
[integer decoding](https://github.com/Viir/super-duper-disco/blob/34d5791629fd84216613f3342bc686a0ec08694c/implement/Pine.Core/BuiltinFunction.cs#L653-L669),
[correct bundled comparison](https://github.com/Viir/super-duper-disco/blob/34d5791629fd84216613f3342bc686a0ec08694c/implement/Pine.Core/Elm/elm-in-elm/elm-kernel-modules/Basics.elm#L597-L622),
[correct native comparison](https://github.com/Viir/super-duper-disco/blob/34d5791629fd84216613f3342bc686a0ec08694c/implement/Pine.Core/Elm/ElmCompilerInDotnet/PrecompiledLeaves/CoreBasicsPrecompiledLeaves.cs#L342-L386).

### Direct proof, without parsing or intermediate-VM compilation

Using canonical encoded string lists, I evaluated
`CoreBasics.Compare_InnerBodyExpression()` with
`DirectInterpreter.WithoutEvalCaching`, then invoked the native
`CoreBasicsPrecompiledLeaves.CompareLeafDelegate` on the same environment
`[[], left, right]`.

| Inputs | Expected | Generated expression, direct interpreter | Native leaf |
| --- | --- | --- | --- |
| `["A"]`, `["B"]` | `LT` | **`GT`** | `LT` |
| `["B"]`, `["A"]` | `GT` | `GT` | `GT` |
| `["A"]`, `["A"]` | `EQ` | `EQ` | `EQ` |

This removes the Elm parser, state monad, dictionary implementation, deferred
string slicing, invocation cache, and sequential-IR optimizer from the
experiment. The generated expression itself has incorrect semantics.

As a separate Elm-level probe, default Pine execution returned `GT` for
`compare ["A"] [String.slice 7 8 "module B exposing (fromA)"]`, and `GT` for
the reverse. The expected pair is `LT`/`GT`.

### Why optimization exposes it

An uninlined comparison call can dispatch to the correct native leaf. An
inlined call can instead execute the incorrect generated fallback.
The VM's `SkipInlining` predicate does not automatically exclude registered
precompiled leaves, although its separate direct-invocation policy does.

The following differentials were executed with the same bundled substitutions:

| Experiment | Result |
| --- | --- |
| Reduced parsed-module dictionary test, default VM | Failed |
| Same reduced test, `skipInlineForExpression: _ => true` | Passed |
| Same reduced test, prevent inlining only `CoreBasics.Compare_InnerBodyExpression()` | Passed |
| Original failing scenario in the diagnostic source copy, all VM inlining disabled | Passed |

These establish that keeping the comparison call boundary avoids this failure.
They do not require changing parsed module names or library package versions.
The precise sequence of specialized frames in the original run was not traced;
the direct semantic mismatch and these inlining differentials are sufficient
to identify the faulty fallback and a verified workaround.

Sources:
[comparison body and leaf identity](https://github.com/Viir/super-duper-disco/blob/34d5791629fd84216613f3342bc686a0ec08694c/implement/Pine.Core/Elm/ElmCompilerInDotnet/CoreLibraryModule/CoreBasics.cs#L1131-L1173),
[VM inlining policy](https://github.com/Viir/super-duper-disco/blob/34d5791629fd84216613f3342bc686a0ec08694c/implement/Pine.Core/Interpreter/IntermediateVM/PineVM.cs#L404-L435),
[native dictionary lookup](https://github.com/Viir/super-duper-disco/blob/34d5791629fd84216613f3342bc686a0ec08694c/implement/Pine.Core/Elm/ElmCompilerInDotnet/PrecompiledLeaves/CoreDictPrecompiledLeaves.cs#L261-L301).

Dictionary insertion/search require the same total ordering. A correct native
insertion can place A to the left of a larger module name, while an incorrectly
inlined lookup orders A as greater and searches right. Walking the tree to
collect its keys still sees A. This explains the apparently contradictory
`Dict.keys`/`Dict.get` results.

It also explains why many tests can pass: integer-list ordering is supported,
equal heads skip the bad ordering operation, and native-leaf execution masks
the fallback defect. The existing comparison effectiveness fixture specifically
exercises `List Int`; it does not cover this `List String` discrepancy.

Source:
[existing comparison fixture](https://github.com/Viir/super-duper-disco/blob/34d5791629fd84216613f3342bc686a0ec08694c/implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/PrecompiledLeaves/BasicsPrecompiledLeavesEffectivenessTests.cs#L29-L49).

## How to fix it

### Permanent fix

Correct `CoreBasics.CompareListRecursive` so each pair of heads is compared
using **generic, recursive Elm comparison**, and tails are compared only when
that result is `EQ`. Preserve lexicographic empty-list and prefix ordering.
Do not retain the integer-only operation for string, float, character, tuple,
or nested-list elements.

Use runtime Pine recursion for the generated comparator. Simply expanding
`Internal_Compare` recursively while constructing C# expressions would recurse
without a finite construction boundary. Similarly, referencing the lazily
encoded comparison body while that same body is being initialized risks a
self-initialization cycle. The generated recursion/environment layout must
provide a finite construction and a genuine generic comparison call.

The required invariant is that the generated Pine implementation, bundled Elm
implementation, and native leaf agree for every supported comparable value,
regardless of whether a leaf is available or a call is inlined. Native
acceleration must not be necessary for correctness.

### Short-term containment

Prevent VM inlining of the generated `Basics.compare` body while retaining its
registered native leaf. Selective protection passed the reduced dictionary
reproducer; disabling all inlining also passed the original failing scenario.
Prefer selective protection over a global performance regression.

This is **not a complete fix**: a direct interpreter or a VM without the
native leaf still evaluates the broken generated comparator. Nor should
`RangeNotFound` be ignored or the test weakened.

### Regression coverage and acceptance

Add comparison and dictionary regressions covering:

- Both ordering directions and equality for `List String`, including parsed
  module-name keys and dynamically sliced strings.
- Nested comparable lists, tuples, characters, and floats, plus empty lists
  and proper prefixes.
- Dictionary insertion in different orders, followed by retrieval of every
  stored key using independently constructed equal keys.
- Generated-expression direct interpretation versus native-leaf results, and
  intermediate-VM execution with leaves enabled/disabled and inlining
  enabled/disabled. Checking only the native path would miss this defect again.

Then run the original filtered test and the full pinned 559-test project under
Pine. For C# regressions, use the repository's existing Microsoft.Testing.Platform
runner (`dotnet run` in the test project, not a `dotnet test --filter` command).
Format changed C# with `dotnet format` when implementing the fix.

Improving the upstream helper's diagnostic to distinguish a missing module from
a missing declaration/range would make future failures clearer, but would not
repair Pine's comparison semantics.

## Implemented fix and focused regressions

`CoreBasics.CompareListRecursive` now invokes a generic comparison body for
each pair of heads. That body receives its own encoded expression through
`envFunctions`, so recursion occurs at execution time rather than during C#
expression construction. The list loop carries both its own encoded body and
the generic comparator; its tail calls preserve those captures.

The public comparison body's environment remains `[[], left, right]`, preserving
the native leaf's calling convention. The fix does not disable inlining or
depend on native acceleration.

The focused float-head regression also exposed a coupled equality issue:
equal cross-products from differently encoded numeric values must compare as
`EQ`, not `LT`. The integer comparison helper now checks equality before the
non-strict sortedness builtin, allowing lexicographic comparison to proceed to
later elements when float heads are numerically equal.

The inclusive operators `<=` and `>=` now also accept the comparator's `EQ`
result when the operands are not structurally equal. A follow-up regression
checks equal but differently encoded scalar numbers and list elements in both
directions, with all four native-leaf/inlining configurations. It verifies
inclusive comparisons are true and strict comparisons are false; all four
cases failed before the inclusive-operator correction.

`CoreBasicsComparisonTests` covers both ordering directions, equality, empty
lists and prefixes, shared prefixes, nested lists, tuple elements, floats, and
integers. It compares direct interpretation and the native delegate against
the same expected result, and executes the generated comparison under all four
native-leaf/inlining configurations. A compiled Elm dictionary regression
checks independently constructed module-name keys, a missing key, and forward
and reverse insertion orders under the same four configurations. The tests
exposed incorrect comparisons and dictionary lookup before the fix.

Validation completed:

- All **19 focused regression cases** passed after the fix.
- All **434 selected core Basics, dictionary, and native-leaf cases**, including
  the focused regressions, passed.
- Changed C# was formatted using `dotnet format`.
- The original pinned upstream test passed under default Pine execution with
  seed `3082049773` and fuzz count `100` (one test, zero failures).
- The full pinned upstream suite passed after the final correction with the
  same seed and fuzz count: **559 passed, zero failed**.
- Independent read-only review identified the inclusive-operator issue described
  above and confirmed its correction, with no further high-confidence findings.
  Automated code review was unavailable because of a model-registry error;
  CodeQL skipped analysis because its database was too large. These automated
  checks therefore do not constitute completed review/security coverage.

The generated recursive helper is not separately registered as a native leaf.
Adding acceleration for that helper can be considered separately; correctness
now holds without it. This change does not attempt to repair unrelated scalar
character ordering.
