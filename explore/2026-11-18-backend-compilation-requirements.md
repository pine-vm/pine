# 2026-11-18 Backend Compilation Requirements

Backend compilation is transforming from Pine expressions into a representation that enables higher runtime efficiency on large workloads.

With compilation, we pay a one-time cost for a transformation that is then amortized over multiple evaluations of the same expressions.

## Implementation Requirements

Here we focus mostly on the properties of the outputs of such compilation.

But, while optimizing the outputs, there are also constraints on the implementation:

+ Must be cache friendly: Inputs to expensive transformations should be designed to enable good reuse with simple identity keys for caching.
+ Must be easy to port to a language without mutations or side-effects, like Elm.

## Output Targets

We maintain the output in form of a sequential IR that is then run in an interpreter loop, mostly as implemented already today in `Runtime.IntermediateVM.PineVM`.
Most of the requirements specified today will reference that representation.

Mid-term, we will compile to WASM. That might happen based on the already existing sequential IR.

As a temporary solution, we will compile also to C#. Also in this case, that might be based on the already existing sequential IR.

While compatibility with these future targets informs design, WASM or C# emission are not deliverables here. The old C# emission implementation present in this repository is outdated and its limitations must not influence design here.

## Scope and Background

While the input to backend compilation are Pine expressions, and the backend compiler must not know about frontend parts, we use mostly Elm programs to model requirements. While the Pine language is fixed, the co-optimization of the Elm compiler to emit shapes that are more amenable to code analysis and optimization is in scope here.

In 2026, we switched the Elm compiler to use embedding as literals where possible (no recursion) for function references, since the embedding makes the static analysis easier. However, we will also require analysis to cover optimization that will require data flow analysis across recursive groups.

The transformations we require here demand the search for proofs and corresponding inference capabilities.

The CFG Model and its functions need not to remain the same. It might be necessary to redesign the CFG completely to meet these requirements.

Some of the proofs might require the introduction of a rich value model that supports operations like disjunction and conjunction and nesting.

Transformations that are currently wired anywhere else on the path of compilation from Pine expressions (e.g. inlining before graph model) might turn out to be detrimental. These can be removed as long as there are no regressions in work counters already tracked in the regression tests.
Commit 269443e8053a1c2cc7ff989c455960fd8a4c2781 is baseline for these work counters.

### Eval Instruction Kinds as Proxies for Runtime Efficiency

Instructions of type `Eval_Binary` or `Eval_Multi` are generally more expensive at runtime, for two reasons:

+ Dynamic dispatch, searching the target.
+ Without knowledge about recipient interface, we must build the complete environment value, causing heap allocations.

## Review: Clarifications Needed Before Implementation

The examples below are useful capability tests, but are not yet a complete implementation handoff. In particular, eliminating an instruction kind is not by itself a runtime-performance guarantee. Resolve the following decisions before treating this document as an acceptance specification.

| Area | Gap or ambiguity | Required clarification |
| --- | --- | --- |
| Backend boundary | Elm examples can accidentally become frontend-specific pattern requirements. | Require proofs from Pine expressions and explicit environment constraints, not Elm names, source annotations, or knowledge of Elm closure layouts unavailable in the input. Pin the Elm compiler revision used to generate each fixture. |
| Semantic equivalence | Output shape says nothing about incorrect results or erased failing computations. | Define equivalence for all inputs admitted by the specialization constraints, including Pine errors and termination behavior. Preserve evaluation of potentially failing or diverging arguments unless proven safe to remove. Distinguish semantic equivalence from intentional changes in resource-limit accounting. |
| Meaning of instantiation | It is unclear whether specialization fixes an entire closure value, its code, its captured values, or some combination. | Specify the facts required to select a specialized callee, how captures remain runtime inputs, and when guards and a generic fallback are required. Facts must remain valid at every call boundary and recursive back-edge. |
| Code covered by an assertion | "All code produced" could mean the root body, code observed during one run, every generic variant in a cache, or transitively reachable specialized callees. | Adopt an explicit reachability rule, such as the acceptance-test contract below, including lazy compilation, guarded branches, recursion, and helper/interpreter escape paths. |
| Policies and budgets | "With the right policy" can permit arbitrarily large code expansion or analysis work. | Specify independent controls for inlining, instantiation, analysis effort, and code size; production defaults; deterministic exhaustion behavior; and how tests disable inlining without disabling specialization. A budget exit must preserve correctness, not pretend that a proof succeeded. |
| Cache identity | "Simple identity keys" does not define validity or lifetime. | Define ownership of interned identities, structural-equality behavior for independently loaded equivalent inputs, and which constraints, policies, target options, and compiler revisions partition caches. No reuse across incompatible contexts; profile changes must not silently reuse stale output. |
| Functional portability | "Easy to port" has no observable acceptance criterion. | Identify the analysis/rewriting core that must admit explicit immutable inputs and outputs, separating it from interning, mutable caches, scheduling, and target-specific execution infrastructure. |
| Regression baseline | A commit alone does not pin the workloads, inputs, settings, or meaning of work counters. | Record the fixture corpus, dependency revisions, entry constraints, policy settings, cache state, and counter definitions at the baseline. State whether each bound is per fixture or aggregate and how explicitly approved tradeoffs are recorded. Do not equate fewer VM instructions with less total work. |

## Output Requirements

The compilation must support configuring policy to allow an application to control the degree of transformations such as inlining and instantiation.

In production, the policies for such transformation are typically informed by profiling, which happens asynchronously on a separate instance.
For the regression tests verifying the compilation capabilities, we configure these policies extreme enough to support verifying internal capabilities by proxy of checking the produced output code.
This means, if the input source code reaches a certain level of complexity, limits for policies in these tests might need to exceed what would be a good default (absent profiling results) for production configurations.

Instead of testing data flow analysis and inference capabilities directly, most requirements use a higher level, asserting properties on the produced output code.

### Efficient Static Dispatch and Known Interfaces for Recursive Functions

For calls whose target is known under the declared entry-environment constraints, the output must provide static dispatch and a known callee interface, including for self-recursive and mutually recursive calls. Recursion must not force a return to generic evaluation or construction of a complete Pine environment value.

The dispatch requirements describe the produced code and its execution only. They do not prescribe an instruction kind, a representation of function references, a linking mechanism, or a compilation algorithm.

#### Required Output and Runtime Behavior

+ Each such call identifies its compiled callee and argument interface statically, including the applicable specialization when specialization assumptions are established. On execution, the call must not discover the target by parsing or comparing encoded expressions, searching an expression-to-code map, or scanning specialization candidates.
+ After the required compiled code is available, dispatch must not perform expression decoding, analysis, or compilation. The cost of selecting an already established target must not grow with the number of compiled functions, the size of their encoded expressions, or recursion depth. Ordinary stack-frame management is not target discovery.
+ The known interface must permit passing the runtime values needed by the callee without allocating Pine list instances solely to assemble a full environment and then project its arguments. This also applies to nested environment paths, recursive-function references, and dynamic closure captures. Replacing the full environment with unnecessary partial environment lists does not satisfy the requirement.
+ No reconstruction of encoded function bodies, recursive-function groups, or closure environments may be needed merely to identify a known recursive callee. Values that the program actually returns, stores, or otherwise uses as data remain subject to normal semantic requirements.
+ These properties must hold with inlining disabled and for non-tail recursive calls through distinct compiled stack frames. Tail-call conversion may satisfy eligible tail calls, but cannot be the only case supported.
+ Apply the same requirements to recursive calls in the parser, converter, and `allocate` workloads below, and to calls through the higher-order recursive fixture after instantiation establishes their targets. A genuinely unknown target is not covered by the static-call requirement; a target observed only during profiling is not thereby known.

Static dispatch and avoidance of environment allocations are separate acceptance conditions: a fixed-target instruction that still constructs the full environment is insufficient, as is allocation-free argument forwarding followed by generic target discovery. Merely eliminating the names `Eval_Binary` and `Eval_Multi` does not demonstrate either property.

#### Source Example: Self-Recursion and Mutual Recursion

```Elm
module RecursiveDispatch exposing (backward, forward, repeat)


repeat : Int -> Int -> Int
repeat remaining value =
    if remaining <= 0 then
        value

    else
        repeat (remaining - 1) (value + 2) + 1


forward : Int -> Int -> Int
forward remaining value =
    if remaining <= 0 then
        value

    else
        backward (remaining - 1) (value + 2) + 1


backward : Int -> Int -> Int
backward remaining value =
    if remaining <= 0 then
        value

    else
        forward (remaining - 1) (value + 5) + 2
```

All recursive calls are non-tail because their results are used by an addition. With inlining disabled, `repeat` must call its own compiled target, `forward` must call the compiled target for `backward`, and `backward` must call the compiled target for `forward`. Their interfaces must carry the changing integer arguments without packaging them into Pine environment lists.

Use the compiled functions with runtime-supplied arguments, not separate constant-folded expressions for each application:

| Application | Expected result | Purpose |
| --- | --- | --- |
| `repeat 0 10` | `10` | Self-recursion base case. |
| `repeat 4 10` | `22` | Multiple non-tail self-calls. |
| `forward 1 10` | `13` | One call to the other function. |
| `forward 2 10` | `20` | Complete mutual-recursion cycle. |
| `forward 3 10` | `23` | Re-enter the cycle with changed arguments. |
| `forward 4 10` | `30` | Multiple complete cycles. |
| `backward 1 10` | `17` | Enter the mutual group at the other function. |
| `backward 3 10` | `27` | Different operation order through the cycle. |
| `forward (-1) 10` | `10` | Nonpositive-depth base case. |

#### Acceptance Checks

+ Inspect every recursive call site in the reachable compiled bodies, including recursive back-edges, and verify its static target and known interface. Check both the output and execution; a sample run or an opcode-name check alone is insufficient.
+ With inlining disabled, retain the separate recursive function bodies and exercise non-tail call boundaries. Verify the results above and increasing depths within the configured stack limit against unoptimized evaluation.
+ For these integer-only functions, require zero Pine list-instance allocations for environment or argument packaging on the recursive edges. Attribute allocations inside dispatch helpers as well as caller instructions; do not infer this property solely from the absence of a list-building opcode. This does not require zero total runtime allocations or prohibit storage for stack frames and integer values.
+ Repeat calls with different depths and starting values through the same warmed compiled code. Increasing depth or changing these runtime integers must not require new code variants or target discovery.
+ For the indirect higher-order example below, additionally verify static dispatch and no environment-packaging lists on the specialized forwarding and callback-call edges while `offset` remains a runtime capture. Repeated calls with different offsets must use the correct invocation's capture without rebuilding a full closure environment merely to make the call.

#### Complement: Known Return Interfaces

The known invocation interface must also cover the return value. The product of expression and environment constraint determines the provable return shape, including alternatives and their conditionally present components. This knowledge must come from Pine inputs, not Elm type information unavailable to the backend.

+ Return the varying components in multiple slots of the already allocated stack, flattening nested structures and omitting fixed tags, field names, and other fixed items. Do not construct the canonical value, intermediate lists, or a heap-allocated result envelope merely to cross the invocation boundary. This must work across separate compiled frames, including recursive calls, with inlining disabled.
+ A consumer that immediately deconstructs the result must read those slots directly. Branch-specific payload slots are meaningful only in their corresponding alternative: after selecting `Nothing`, no consumer may read its dummy payload slots. Their contents must not affect canonical reconstruction, equality, or persisted results. Returning or forwarding slots must preserve their values despite reuse of callee stack storage.
+ The representation must include enough information to compose the exact canonical Pine value on demand: nested structure, fixed components, conditional components, and finite maps from compact tags to larger canonical values. Materialize only the subtree that actually escapes or is otherwise required in canonical form; direct consumers must not pay for this reconstruction.
+ Specialized consumers, including the persisted eval-cache writer, must consume components without first assembling a canonical result or an intermediate result object. Persist varying components and compact discriminants rather than redundant fixed structure or inactive payloads. Reloading must recover the same return interface without transient canonical trees; cache identity and format compatibility must account for the applicable shape and value maps. Allocations intrinsic to I/O are not prohibited.

For a finite value map, direct deconstruction emits equality checks on the compact discriminant. When the mapped canonical value is required, use a single IR value-map instruction containing an arbitrary number of key/value entries, not a chain of branches. This is the value-producing counterpart of `Switch_Jump_If_Equal_Const`: matched cases yield values without jumps; an unmatched key requires exactly one explicit outcome, either `Literal` for a fallback value or `JumpOffset` for a fallback path. The choice of compact tags and the inference, linking, and persistence algorithms remain unspecified.

#### Return Examples: Literal Scanning

These fixtures come from [StringParsing.elm](../implement/elm-in-elm/pine-elm-syntax/src/ElmSyntax/Concrete/Parser/StringParsing.elm), at revision `47343a952dc1610da69fa2c4b5d9ebc024724137`. The double-quoted scanner is reproduced in full:

```Elm
findDoubleQuotedLiteralRunEnd : String -> Int -> ( Int, LiteralRunBoundary )
findDoubleQuotedLiteralRunEnd source offset =
    case String.left 1 (String.dropLeft offset source) of
        "\"" ->
            ( offset, LiteralRunTermination )

        "" ->
            ( offset, LiteralRunUnterminated )

        "\\" ->
            ( offset, LiteralRunBackslash )

        "\n" ->
            ( offset, LiteralRunNewlineLF )

        "\u{000D}" ->
            if String.left 1 (String.dropLeft (offset + 1) source) == "\n" then
                ( offset, LiteralRunNewlineCRLF )

            else
                ( offset, LiteralRunNewlineCR )

        _ ->
            findDoubleQuotedLiteralRunEnd source (offset + 1)


findLiteralRunEnd : LiteralTermination -> String -> Int -> ( Int, LiteralRunBoundary )
findLiteralRunEnd termination source offset =
    case termination of
        SingleQuoteTermination ->
            findSingleQuotedLiteralRunEnd source offset

        DoubleQuoteTermination ->
            findDoubleQuotedLiteralRunEnd source offset

        TripleQuoteTermination ->
            findTripleQuotedLiteralRunEnd source offset
```

Both functions must return **two slots**: the end offset and a compact boundary discriminant covering the six constructors above. The discriminant maps to the canonical `LiteralRunBoundary` value only on demand; no tuple or choice wrapper is needed by a caller that branches directly on the boundary. `findLiteralRunEnd` must preserve this interface through its selected helper, including when `termination` is a runtime input.

The same source contains the following Unicode scanner; this excerpt omits the other hexadecimal arms and the recursive helper's body:

```Elm
scanUnicodeEscapeDigits : String -> Int -> Maybe ( Int, Int )
scanUnicodeEscapeDigits source offset =
    case String.left 1 (String.dropLeft offset source) of
        "0" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 0)

        -- The other hexadecimal arms likewise seed the helper with 1 through 15.
        _ ->
            Nothing


scanUnicodeEscapeDigitsHelp : String -> Int -> Int -> ( Int, Int )
```

The helper consumes subsequent hexadecimal digits, accumulating `value * 16 + digit`, and returns `( endOffset, value )` at the first non-hexadecimal character. Require **three slots** for `scanUnicodeEscapeDigits`: a `Nothing`/`Just` discriminant, the end offset, and the code point. This flattens the tuple nested inside `Just`. The fixed `"<Choice>"` marker occupies no slot; in the `Nothing` case only the discriminant is read, and the other two slots contain irrelevant dummy values.

Existing consumers in `TokensFromString.elm` and `FromString.elm` branch on `Just ( digitsEndOffset, codePoint )` or `Nothing`. They must obtain these components without constructing either the tuple or the enclosing choice.

#### Return Examples: Closed Records and Nested Choices

Use this synthetic source to exercise both shapes without depending on parser details:

```Elm
coordinates : Int -> Int -> { alfa : Int, beta : Int }
coordinates alfa beta =
    { alfa = alfa, beta = beta }


optionalCoordinates : Int -> Int -> Maybe { alfa : Int, beta : Int }
optionalCoordinates alfa beta =
    if alfa < 0 then
        Nothing

    else
        Just (coordinates alfa beta)
```

`coordinates` requires **two slots**, `alfa` and `beta`. In schematic canonical Pine notation its result is `["<Record>", "alfa", alfaValue, "beta", betaValue]`: the fixed record marker and field names take no slots. `optionalCoordinates` requires **three slots**, a choice discriminant and those two field values, omitting both fixed markers and the names even inside `Just`. Its canonical alternatives are `["<Choice>", "Nothing"]` and `["<Choice>", "Just", ["<Record>", "alfa", alfaValue, "beta", betaValue]]`. As with the Unicode scanner, a `Nothing` consumer must not request the payload slots.

#### Return-Interface Acceptance Checks

+ With inlining disabled, inspect the return layouts and exercise separate producer/consumer frames. Require the exact slot counts above, with no return-packaging Pine lists, child arrays, or replacement heap envelopes when a consumer only deconstructs or persists the result. Check allocations in helpers as well as instruction counters.
+ Exercise all six literal boundaries, all three termination modes, nonzero starting offsets, and recursive scans. For Unicode, include `"41}"` at offset `0` yielding `Just ( 2, 65 )`, `"x"` yielding `Nothing`, and `"x41}"` at offset `1` yielding `Just ( 3, 65 )`. Use runtime inputs and compare against unoptimized evaluation.
+ For records, use multiple runtime integers and both optional branches. Test consumers that read one field, both fields, only the choice tag, and consumers that let the whole value or a nested subtree escape. Canonical materialization must match unoptimized values exactly, and inactive dummy slots must be unobservable.
+ Round-trip each shape through the persisted eval cache, covering cold execution and warm cache hits with direct deconstruction and canonical escape. Verify omission of fixed and inactive components, compact tag storage, and no intermediate canonical allocations. Exercise both explicit unmatched outcomes of the value-map instruction.

### Elm Syntax Parser

[ElmSyntax/Concrete/Parser/FromString.elm](https://github.com/pine-vm/pine/blob/c5e7036a434b6e002443f36667e0e694b06201ee/implement/elm-in-elm/pine-elm-syntax/src/ElmSyntax/Concrete/Parser/FromString.elm) does not use higher order functions.

All sequential IR produced for running `parseFile` must not contain any instruction of kind `Eval_Binary` or `Eval_Multi`.

### Elm Syntax Conversion from Concrete to Abstract

[ElmSyntax/Abstract/ConvertFromConcrete.elm](https://github.com/pine-vm/pine/blob/c5e7036a434b6e002443f36667e0e694b06201ee/implement/elm-in-elm/pine-elm-syntax/src/ElmSyntax/Abstract/ConvertFromConcrete.elm) does not use higher order functions.

All sequential IR produced for running `fromFile` must not contain any instruction of kind `Eval_Binary` or `Eval_Multi`

### Transforming `List.member` on Literal Set to Map

For the source function:

```Elm
local : String -> String
local name =
    if List.member name (String.words "do if in NaN int for new try var let null true eval byte char goto long case else this void with enum false final float short break catch throw while class const super yield double native throws delete return switch typeof export import public static boolean default finally extends package private Infinity abstract volatile function continue debugger undefined arguments transient interface protected instanceof implements synchronized F2 F3 F4 F5 F6 F7 F8 F9 A2 A3 A4 A5 A6 A7 A8 A9") then
        "_" ++ name

    else
        name
```

(https://github.com/pine-vm/pine/blob/c5e7036a434b6e002443f36667e0e694b06201ee/implement/elm-in-elm/elm-make-ecmascript/src/ElmMake/JavaScriptNames.elm#L6-L12)

Compilation must not emit any invocations or loops, but use a single instruction like `Switch_Jump_If_Equal_Const` for the main map.

+ Threshold for item count should be configurable in the compilation API.
+ This optimization must not break when `local` is used in another function.

### Eliminating Higher-Order Functions via Instantiation

[ElmMake/CyclicDeclarations.elm/allocate](https://github.com/pine-vm/pine/blob/c5e7036a434b6e002443f36667e0e694b06201ee/implement/elm-in-elm/elm-make-ecmascript/src/ElmMake/CyclicDeclarations.elm#L20-L95) uses higher order functions.

With the right policy compilation must eliminate all cases of higher-order so that no instruction of kind `Eval_Binary` or `Eval_Multi` is found in all code produced to run `allocate`

This includes instantiation of `foldResult` and instantiation inside `DependencyGraph.order`

### Acceptance-Test Contract for Eliminating Generic Eval

The following is a proposed precise interpretation for the no-generic-`Eval` requirements, including the synthetic program below.

+ Compile from pinned Pine expressions with explicitly declared entry-environment constraints. Ordinary data arguments remain runtime inputs; test results must not come from specializing on the numeric or textual inputs used for verification.
+ Inspect the transitive closure of compiled variants reachable from the constrained entry point for **all** inputs satisfying those constraints, not just instructions executed by a sample input. Traverse direct calls, both feasible branches, and recursive edges with cycle detection. Include callees compiled lazily; a root-body-only scan is insufficient.
+ No reachable variant may contain `Eval_Binary` or `Eval_Multi`, or transfer the same unresolved application into an interpreter, runtime expression parser, native helper, or another opcode. A known call must become a statically resolved compiled call, a loop, or inlined code. Renaming or hiding generic evaluation does not satisfy the requirement.
+ Generic variants retained for unrelated callers or environments outside the declared constraints need not satisfy this assertion. If a generic fallback is reachable for an admissible input, it is part of the inspected closure and cannot be excluded merely because the measured run did not take it.
+ Compare results and relevant errors against unoptimized Pine evaluation on representative inputs. Structural assertions complement, rather than replace, semantic checks and runtime measurements.
+ Exercise both an inlining-enabled policy and a policy disabling inlining while retaining instantiation. For the latter, verify that the fixture still has distinct compiled caller/callee bodies and executed non-tail call boundaries; otherwise the test has not demonstrated inter-frame propagation.

The parser and concrete-to-abstract conversion fixtures also need input corpora covering successful and failing parses, small and large files, and the admitted environment constraints. Absence of higher-order functions in Elm source is not itself proof that every encoded Pine call target is recoverable.

### Indirect Higher-Order Functions Across Recursive SCCs and Stack Frames

This synthetic Elm module exercises instantiation where a callback is forwarded rather than immediately applied. An SCC is a strongly connected component of the call graph.

```Elm
module IndirectHigherOrder exposing (entry)


entry : Int -> Int -> Int -> Int -> Int
entry offset depth relayDepth value =
    first (\item -> item + offset) depth relayDepth value


first : (Int -> Int) -> Int -> Int -> Int -> Int
first fn remaining relayDepth value =
    if remaining <= 0 then
        relay fn relayDepth value

    else
        second fn (remaining - 1) relayDepth (value + 1) + 1


second : (Int -> Int) -> Int -> Int -> Int -> Int
second fn remaining relayDepth value =
    if remaining <= 0 then
        relay fn relayDepth value

    else
        third fn (remaining - 1) relayDepth (value + 2) + 1


third : (Int -> Int) -> Int -> Int -> Int -> Int
third fn remaining relayDepth value =
    if remaining <= 0 then
        relay fn relayDepth value

    else
        first fn (remaining - 1) relayDepth (value + 3) + 1


relay : (Int -> Int) -> Int -> Int -> Int
relay fn remaining value =
    if remaining <= 0 then
        apply fn value + 10

    else
        relay fn (remaining - 1) value + 1


apply : (Int -> Int) -> Int -> Int
apply fn value =
    fn value
```

The named-function reference graph contains the mutually recursive SCC `{ first, second, third }`, a separate recursive SCC `{ relay }`, and the nonrecursive helper `{ apply }`. Only `apply` invokes `fn`; the other four functions forward it. The callback has known code but captures the runtime value `offset`.

The additions after calls deliberately make the forwarding chain non-tail. With inlining disabled, `depth >= 3` and `relayDepth > 0` make the function value pass through multiple compiled stack frames, including a complete trip around the three-function cycle and then a different recursive SCC, before it is applied. Optimizing only self-recursion, only one SCC, or only immediately applied callbacks is insufficient.

#### Required Output and Runtime Behavior

+ With a policy allowing instantiation, satisfy the no-generic-`Eval` acceptance-test contract for `entry`, including every reachable specialized member of both recursive SCCs and `apply`.
+ Meet this requirement with inlining disabled, not merely by unrolling the cycle or copying the entire chain into the entry point. Keep the recursive forwarding calls as separate compiled calls in this capability test; the post-call additions ensure that tail-call conversion alone cannot satisfy it.
+ Specialize on the known callback code and necessary structural facts, while passing `offset`, `depth`, `relayDepth`, and `value` dynamically. The same compiled specialization must work with different captured offsets without recompiling or selecting a separate specialization for each offset.
+ Reach a finite set of specializations for the recursive call contexts in this fixture. Increasing runtime depth, relay depth, or the number of different captured offsets must not create additional variants of this fixture's code or re-run analysis during warmed execution.
+ In the no-inlining run, confirm that application in `apply` calls the specialized callback through a statically resolved compiled target, using the capture belonging to that invocation. Do not substitute a previously observed callback result or closure environment.

Use runtime-supplied arguments, not separate constant-folded entry expressions. Representative semantic checks are:

| Application | Expected result | Purpose |
| --- | --- | --- |
| `entry 7 0 2 10` | `29` | Base case in `first`, then recursive relay. |
| `entry 7 1 2 10` | `31` | Base case in `second`. |
| `entry 7 2 2 10` | `34` | Base case in `third`. |
| `entry 7 3 2 10` | `38` | Complete trip around the mutual cycle. |
| `entry 7 4 2 10` | `40` | Re-enter the cycle before crossing SCCs. |
| `entry 100 3 2 10` | `131` | Same callback code, different runtime capture. |
| `entry 7 3 0 10` | `36` | Skip relay recursion without changing specialization. |
| `entry 7 (-1) (-1) 10` | `27` | Both nonpositive-depth base cases. |

Also run changing inputs repeatedly through the same warmed compiler/VM caches; compilation must remain reusable without mixing captures. Cover multiple complete cycles and larger relay depths within the configured stack limit, not only the values in this table.

#### Required Variants

| Variant | Requirement |
| --- | --- |
| Two different known callbacks reaching the same recursive functions | Instantiate both contexts, including their transitive callees, without confusing callback code or captures. Include a callback with a different operation, not just a different offset. |
| Callback chosen by a runtime branch from two known functions | Eliminate generic `Eval` through finite guarded dispatch or equivalent specialized control flow. Inspect both branches and preserve the relationship between each code target and its captures. |
| Callback packed into a record, tuple, or custom-type constructor before forwarding | Propagate the necessary nested value facts through construction, projection, and a branch join across the same non-inlined call chain. |
| A helper returns the callback before it is forwarded or applied | Track returned function values across compiled return/call boundaries, not only argument positions. Include a partially applied function with a runtime capture. |
| Curried callbacks applied in separate stages | Cover partial application, full application, and application of a returned function. Resolve each known application stage without generic `Eval`, and avoid allocating intermediate closures when they do not escape. |
| Forwarding permutes arguments or replaces the callback on a recursive edge | Track the actual value at each parameter position. Cover a finite set of known replacement functions without incorrectly assuming the initial callback remains invariant. |
| Nested or interleaved calls using the same callback code with different captures | Keep invocation environments independent even when compiled code and specialization metadata are shared. |
| Callback code truly unknown under the entry constraints | Preserve a correct generic path or sound guarded specialization. This negative-control case does not require eliminating generic `Eval`; profiling observations alone are not a proof of a fixed target. |

### Additional Runtime-Performance Requirements to Specify

These are proposed additions, not claims about current compiler capabilities. Each accepted case needs a pinned fixture, policy, input-size series, and numeric acceptance limits before implementation handoff. Keep permissive capability tests separate from tests of production-default policies: a compiler that succeeds only with effectively unlimited instantiation is not necessarily suitable for production.

| Case | Required behavior and measurement |
| --- | --- |
| Long-running direct and mutual tail recursion | Require bounded active stack depth for tail-recursive cycles, including cycles between different known functions and across specialized call targets. Define which shapes are mandatory. Check that removing generic `Eval` does not replace a loop with growing stack frames. The deliberately non-tail synthetic fixture above is a separate requirement. |
| Closure and argument forwarding | Measure per-call allocations, environment/list construction, argument projection, and materialization. A known callback with a dynamic capture should not require rebuilding its encoded code or full closure value at every hop when it does not escape. Include an escaping closure that must still retain its captures correctly. |
| Finite callback choices and guard selection | Bound the runtime cost of dispatch as the number of variants grows. Avoid scanning every specialization or repeatedly comparing large closure environments on hot recursive edges when the target is already established. |
| Hot loops over large values | Cover map/fold/filter-style traversal, shared or repeatedly projected nested data, and large strings/blobs. Bound allocations and bytes copied as well as instruction counts; avoid repeated full materialization, reconstruction, hashing, or equality walks hidden inside a single instruction. |
| Literal membership | Test sizes below, at, and above the configured threshold; empty/singleton sets; duplicates; hits and misses; long keys; and hash collisions where hashing is used. Require exact Pine equality, including collision checks, and hoist `String.words`/table construction out of execution. Distinguish initialization and output-building work from the membership lookup itself. Measure scaling with set size and key length rather than assuming one switch instruction means constant total work. |
| Interprocedural constant-set propagation | Test `local` as a non-inlined callee, with several callers and with the set forwarded through helpers. Specify whether the threshold counts source entries or distinct values and what representation/cost is allowed below it. A surrounding caller's loops must not violate a lookup-local no-loop assertion. |
| Shared arguments and branches | Prevent inlining or specialization from duplicating expensive argument evaluation, list construction, or repeated projections. Include branch joins, repeated uses, and arguments that can fail; preserve strictness where semantically required. |
| Specialization growth and analysis termination | Stress many callback combinations, distinct captures, growing closure environments, and recursive changes in known facts. Set bounds on analysis work, compile latency, variant count, generated code size, and retained cache memory. On budget exhaustion, retain correct execution with an observable reason for the missed optimization; never loop indefinitely while searching for a proof. |
| Cold, warm, and changing workloads | Measure fresh-process startup, first compilation, warm execution with invocation-result caching disabled, and end-to-end execution with normal caches. Include equivalent independently loaded expressions, profile changes, cache eviction, and low-reuse workloads. Report compilation cost separately so moving work to compilation cannot hide a startup regression. |
| Realistic parser/compiler workloads | Supplement tiny functions with the pinned parser, converter, and `allocate` workloads over increasing successful and failing inputs. Record elapsed time, allocation/GC behavior, peak retained memory, and stack depth alongside work counters. Include rare branches and error paths so optimizing the common path does not leave accidental generic dispatch elsewhere in the required closure. |
| Cancellation and resource limits | Preserve cancellation responsiveness and enforce configured limits after inlining, direct-call conversion, and tail-loop conversion. Specify accounting for optimized-away invocations; document intentional counter changes instead of silently removing a safety limit. |

For each performance fixture, the handoff should record:

+ Exact source/Pine input identity, dependencies, entry constraints, runtime input generator, compiler/target settings, policy limits, and cache/instrumentation configuration.
+ Structural assertions and semantic/error checks, plus numeric limits for deterministic counters such as instructions, invocations, list builds/items, and any direct-interpreter or deferred-materialization work. Include cached derivation work separately; do not hide it by changing the reporting category.
+ Limits for compilation work, reachable variant/code size, allocations/bytes copied, peak live memory, and stack depth where applicable. Specify scaling bounds over the input-size series, not just one small-input snapshot.
+ A reproducible timing protocol stating runtime/build configuration, hardware class, warmup, sample count, summary statistic, and tolerated variance. Use deterministic work bounds for stable regression gates and timing/allocation measurements to detect costs those counters omit.
+ For compilation amortization, a baseline comparison using `T_compile + N * T_execute`, with cold initialization accounted for explicitly. State the minimum expected reuse count and acceptable break-even point rather than assuming that compilation cost is always negligible.

