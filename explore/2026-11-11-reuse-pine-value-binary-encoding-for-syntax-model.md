# 2026-11-11 Reuse Pine Value Binary Encoding for Syntax Model

In commit 772152dba7aee711da8a7ce69bf09df7042931ad we introduced an efficient binary encoding for the syntax model of a frontend language.

Could we simplify the overall design by reusing a more generic version of the existing `Pine.Core.Encodings.ValueEncodingBinaryDeterministic`?

How much runtime overhead would we pay for the Pine value encoding and decoding if we made `Pine.Core.Encodings.ValueEncodingBinaryDeterministic` generic enough to support such other tree structures? Is it possible to use such a generic common implementation without additional heap allocations when encoding and decoding Pine values?

Goals:

+ Do not cause more heap allocations for Pine value use case compared to the existing specialized encoding implementation.
+ Support the deterministic ordering also for the syntax model nodes, since we need stable hashes for those too.

Following is our first idea for a design:

+ Introduce a type parameter for the tree node type in `ValueEncodingBinaryDeterministic`
+ To support the ordering by depth in encoding, use a delegate `Func<NodeType, int> getDepth` as parameter to encoding.
+ Use the `Eval` entry type in `ValueEncodingBinaryDeterministic` to model syntax node constructors.
  + For every node type use a specific encoded expression value in `Eval`, via hash reference. Decoding that syntax model configures the generic decoding function to reject any other type of expression in eval. The arguments needed to build the syntax node can follow as an inline list in the environment part of the `Eval` node. This way, the only additional type needed for intermediate results is `Array<NodeType>`.
+ The base model is still the Pine value. The Pine values constructed by the node constructor expressions follow the model from [ElmSyntax/Abstract/Expression.elm](https://github.com/pine-vm/pine/blob/e176af605bbbd7561a849c873e673a48626b4351/implement/elm-in-elm/pine-elm-syntax/src/ElmSyntax/Abstract/Expression.elm#L16-L37) encoded as specified in [elm-value-encoding.md](https://github.com/pine-vm/pine/blob/e176af605bbbd7561a849c873e673a48626b4351/guide/elm-value-encoding.md)
+ No Pine value is directly instantiated. Instead, the existing parameters `resolveExternalReference` and `evaluate` are replaced with a more flexible model, returning `NodeType` and not requiring the modelling of the expression as Pine value instance at runtime anymore.
+ For testing, decode to a Pine value, convert to `ElmValue` and assert equality based on that representation.


## Copilot Did Some Research and Answers

Response from Copilot follows:

> **Yes—a shared codec is feasible, and it can preserve the existing Pine-value allocation costs.** But the `idea record` needs a few refinements. Genericity itself is not the main efficiency risk; the constructor representation and intermediate decoding values are.
> 
> ## Answers to the runtime questions
> 
> | Question | Answer |
> |---|---|
> | Can generic encoding/decoding avoid additional Pine-value heap allocations? | **Yes**, provided that specialization uses `PineValue` directly, without wrapper nodes, boxing, newly captured delegates, or per-node descriptors. |
> | Does calling `Func<NodeType, int>` allocate? | **Not per invocation.** Creating delegates or captured closures can allocate. |
> | Can CPU overhead also be zero? | Possibly after JIT optimization, but **not guaranteed**. Delegates can introduce indirect calls and inhibit inlining. A constrained struct adapter offers a better opportunity for specialization without boxing. |
> | How much slower would it be? | No defensible percentage follows from the design alone. That requires benchmarking the actual generic implementation against the current one. |
> | Does avoiding intermediate `PineValue` instances eliminate syntax-decoding overhead? | **No.** Argument arrays, decoding frames, constructor dispatch and success-result wrappers can still allocate or consume CPU. |
> 
> ### The existing decoder exposes specific allocation traps
> 
> At `the Eval branch`, every Eval currently creates:
> 
> - A pending-frame object.
> - A two-element argument array.
> 
> A nonempty environment list creates another frame and array. The existing `successful Result representation` is also a heap object.
> 
> A straightforward generic translation would preserve those costs—not reduce them to “only the constructor-argument array.” They are avoidable through inline frame fields, value-type frames and an allocation-free success-return convention.
> 
> These syntax-specific intermediates need not affect the ordinary Pine blob/list path.
> 
> ## Important design corrections
> 
> ### 1. Depth ordering is unnecessary here
> 
> The current `deterministic encoder` uses ordered traversal and backward references. It does **not** sort by depth. Depth grouping belongs to `the classic encoder`.
> 
> Therefore, no `getDepth` parameter is needed to preserve its determinism. Stable constructor identities, fixed field order, deterministic collection order and structural deduplication are sufficient. Depth alone would not establish a deterministic ordering anyway.
> 
> ### 2. A node type parameter is insufficient by itself
> 
> The encoder also needs allocation-free access to a node’s representation:
> 
> - Blob payload.
> - List length and indexed children.
> - Eval constructor identity and environment arguments.
> 
> The syntax decoder needs intermediate representations for constructor handles, blobs and lists—not just completed syntax nodes. `NodeType = Expression` cannot represent all of those. A typed union can, but its storage costs must remain out of the Pine specialization.
> 
> ### 3. Eval has significant wire overhead
> 
> Using the existing tag sizes, **before encoding any arguments**:
> 
> | Constructor representation | Bytes |
> |---|---:|
> | Eval + external constructor hash reference + inline list header | **48** |
> | Eval + internal reference to an earlier constructor reference + list header | **20** |
> 
> The current syntax format uses a **one-byte constructor tag**. Completed-node references also grow from five to eight bytes.
> 
> These are format calculations, not measured throughput results. They suggest the Eval envelope may matter more than generic dispatch, particularly for small nodes. Reuse constructor references rather than emitting the 32-byte hash repeatedly.
> 
> ### 4. Elm-value equality is not a complete preservation test
> 
> The current syntax model stores precomputed Pine values independently of literal text/numbers. Existing `tests explicitly require preserving unusual supplied values`.
> 
> The proposed Elm representation does not carry those fields. Consequently, it cannot reproduce every currently supported .NET syntax object unless you:
> 
> - Include those fields in the encoding, or
> - Intentionally narrow the preservation contract.
> 
> Decoding through Pine and comparing `ElmValue` is a useful semantic oracle, but retain native round-trip and reference-sharing tests too.
> 
> ## Recommendation
> 
> Extract a **shared traversal/reference codec with a statically typed adapter**, keeping the Pine specialization wrapper-free. Treat adopting Eval for syntax as a separate format decision.
> 
> Before replacing the syntax codec, compare:
> 
> - Allocated **bytes and object counts**, including cold and warmed runs.
> - Encode/decode time and encoded size.
> - Shared versus separately allocated equal subtrees.
> - Large literal streaming—the current `allocation test` protects against payload-sized temporary arrays.
> 
> **Bottom line:** allocation-neutral reuse for Pine is achievable. Equivalent efficiency to the dedicated syntax codec is not established, and the proposed Eval envelope introduces concrete costs that should be measured before committing to it.
> 
> *Source inspection only; no prototype or throughput measurements were performed.*


> About the eval overhead specifically.
> Reduce the heap allocation for result by switching to a ValueResult type, following the .NET pattern of ValueTask, ValueTuple.
> 
> Also explain, how could we further reduce overhead for the eval decoding?


> ## Follow-up: Reducing Eval decoding overhead
> 
> The Eval callback now returns `ValueResult<string, PineValue>`, a readonly
> struct with a discriminator and typed success/error payloads. This removes the
> per-Eval `Result.Ok` or `Result.Err` wrapper allocation without boxing payloads.
> An uninitialized result throws rather than silently becoming a success or a
> rejection. The outer `DecodeRoot` still returns the existing `Result` once per
> decode; the wire format, evaluation order, rejection details and completed-Eval
> reference reuse are unchanged.
> 
> Further opportunities, not implemented in this change:
> 
> 1. **Remove the two-element Eval array.** Keep the decoded expression directly
>    in the pending Eval frame; when the environment completes, pass it directly
>    to the callback. Lists still need their own element storage.
> 2. **Store pending frames as structs in an explicit stack.** This removes the
>    per-Eval/per-list frame object, but the stack's backing storage still allocates
>    or grows. Preserve iterative decoding for deeply nested inputs. Mutating a
>    struct returned by `Stack<T>.Peek` only changes a copy, so use explicit
>    write-back or indexed/ref access to the actual frame storage. Clear popped
>    reference fields to avoid retaining completed trees.
> 3. **Avoid materializing a Pine list for constructor arguments.** In the
>    proposed generic syntax decoder, an inline environment list can instead
>    produce its argument array directly, or feed a fixed-arity constructor
>    accumulator. Reuse final child-array storage where its element type permits.
>    The environment's address remains referenceable: skipping its representation
>    needs a restricted profile or equivalent lazy materialization. A general
>    evaluator may retain or return the environment, so blindly pooling its array
>    or exposing only a borrowed span would change the contract.
> 4. **Resolve constructors to reusable handles.** For the syntax profile, resolve
>    whitelisted expression hashes to stable constructor identifiers and dispatch
>    directly instead of constructing/interpreting expression Pine values.
>    Earlier constructor references can use internal references. Keep this policy
>    explicit: the general callbacks need not be pure or return stable instances.
> 5. **Do not memoize arbitrary Eval calls by their arguments.** Distinct Eval
>    entries must still invoke the callback even if their arguments are equal;
>    only internal references to a completed Eval reuse its result. A separate
>    pure-constructor profile can impose stronger guarantees.
> 
> These changes target result, frame, argument-array and dispatch overhead
> separately. They do not remove decoded-node allocations, the reference table,
> the Eval wire envelope or the need to benchmark total allocated bytes and CPU
> time. Structs avoid wrapper objects but can increase copying and stack storage.
