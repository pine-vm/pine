# Allocation-Neutral Binary Tree Encodings via Pine

Use `Pine.Core.Encodings.PineTreeBinaryEncoding` for both the wire
format and reference traversal. Define a canonical mapping from the retained
tree model to Pine values; that mapping need not be materialized by the codec.
The Elm syntax implementation is a complete example.

## Constructor contract

For each retained constructor, define a real Pine expression `C` that constructs
its canonical value from a shallow environment. Encode:

```text
Eval(External256(hash(ExpressionEncoding.EncodeExpressionAsValue(C))),
     List(field0, field1, ...))
```

Here `hash` is based on the canonical representation as specified in [pine-value-encoding-and-content-addressing.md](pine-value-encoding-and-content-addressing.md).
Store constructor hashes as static compiler data and whitelist them on decoding.
`StartExternalEval` emits each distinct hash once per encoder scope. Later Evals
use an eight-byte backward reference to that external-reference entry, saving
28 bytes per repeated constructor. Hash sharing is by all 32 content bytes,
independent of the retained-node comparer and without allocating a key object
per hash. Constructor programs and their hashes do not change.

Frontend-independent encoding specializations live in
`Pine.Core.Encodings.ValueEncodingExpressions`.
`BlobPadBytesTo4` consumes `[blob]` and returns a blob with three zero bytes
prepended to every input byte, preserving all values from 0 through 255. Its
static `BlobPadBytesTo4Hash` identifies that exact program without initializing
it. This is a byte transformation, not a string or frontend constructor.
Encoders using it must require an **output blob of at least 64 bytes** to
amortize Eval overhead. Additional content-only restrictions, such as an ASCII
gate in a string encoder, are permitted; cache-key encodings must use a fixed,
deterministic selection policy.

Flatten according to the **retained model**, not its canonical Pine shape.
An application uses `[function, argument0, argument1, ...]`. A constructor with multiple variable-size components needs explicit counts or retained subobjects:
`[moduleCount, module0, ..., name, pattern0, ...]`, or a separately constructed qualified-name object that is already retained in the model.
Never introduce a record/list wrapper just to mirror the canonical representation.

## Shared codec contract

`TreeEncoder<TItem>` writes list/blob headers, padding, relative references and
external Evals. Visit fields in fixed order, use structural keys, and call
`TryReference` before emitting a new retained node. Dictionaries must never
determine traversal order. No depth sorting is required.
`StartBlob(length)`, `BlobChunk(bytes)` and `EndBlob(length)` support bounded-buffer
streaming: emit exactly the declared payload length between the header and final
alignment padding. Text conversion belongs in the model-specific codec, not the
shared tree writer.

`TreeDecoder<TItem>.Read<TNode, TAdapter>` uses a struct
`IDecoding<TNode, TItem>` adapter, avoiding interface boxing and captured
delegates. `TItem` may be `object` to share strings, qualified names, native nodes
and Pine payloads in the same reference table. All retained items are reference
types. Read numeric fields from inline blob spans directly into value types;
do not box them into that table.

Set `HasExternalEvaluation` to dispatch the expression hash through
`EvaluateExternal(ref decoder, hash)`. Consume `ReadInlineListLength`, typed child
reads and `ReadInlineBlob` directly. Allocate only the final nodes and their
owned arrays; no encoded expression, environment array, list wrapper, pending
Eval object or success wrapper is needed. Enforce arity, component counts,
field types, scalar validity, constructor whitelist and a suitable `MaximumDepth`.
For an unreferenceable inline field Eval, `ReadInlineExternalEval` reads its
whitelisted external expression hash (including shared hash references); the
adapter then consumes and validates the environment itself. For example, the
AST string adapter recognizes a padded blob inside its own string constructor
without allocating an intermediate blob. See its
[deterministic string policy](elm-abstract-syntax-binary-encoding.md).
Recursive adapters must also check available execution stack: a fixed nesting
limit alone cannot protect smaller OS thread stacks. The shared reader rejects
specialized reads when `RuntimeHelpers.TryEnsureSufficientExecutionStack` fails,
rather than allowing a process-fatal stack overflow.

This is a **restricted profile**: constructor positions admit a direct external
hash or a backward reference to an already-read external-reference entry.
The shared reader tracks those entries separately from materialized retained
nodes, passing an immutable source-backed hash slice to `EvaluateExternal` in
both cases. The external hash is complete before its Eval's environment is
read, so nested Evals may reuse an enclosing constructor immediately.
Inline environments and raw scalar blobs remain unreferenceable. Constructor
hash entries are not retained nodes and cannot substitute for a typed child.
Completed retained nodes remain referenceable. Ordinary Pine
decoding instead materializes every normal argument and preserves arbitrary Eval
callbacks, including their invocation order and rejection details.

Old direct-hash Evals remain readable. Re-encoding with shared hashes changes
binary bytes and binary-derived content addresses; regenerate dependent
persistent keys. Ordinary canonical blob/list encoding is unchanged.

Dispose every scope, including on failure. Scratch reference/frame buffers may
be pooled, but must be cleared; arrays retained by results must never be returned
to a pool. Keep input memory immutable while retained blob slices are in use.
The shared reader scans the valid wire prefix for reference targets before
decoding, so unreferenced nodes need no scratch entry. This scan neither resolves
references nor evaluates expressions; actual reads still validate input.
**Internal dictionaries/indexes do allocate on the heap.** Encoder dictionary
objects, buckets and entries allocate on first use and growth. The decoder keeps
up to twelve distinct reference targets inline; larger indexes rent heap arrays.
Reuse eliminates many warmed-run allocations, not their backing storage or all
cold/concurrent growth costs. Bound reusable dictionaries and report these costs
separately from retained results.
Constructor-hash lookup has its own bounded reusable dictionary with inline
256-bit keys; its first-use/growth storage is additional scratch, not retained
constructor-program allocation.

Flattening removes temporary objects and their potential reference entries.
Keeping an already-retained subobject can instead amortize its object and index
cost across repeated uses. A deeper constructor boundary can enable reuse of
strings/qualified names/collections, but adds reference bookkeeping and may retain
more scratch capacity. Choose that boundary from measured sharing and allocation
costs, not just canonical tree shape. See the
[Elm AST implementation log](../explore/2026-11-14-reuse-pine-value-binary-encoding-implementation-log.md)
for concrete cold and warmed dictionary/index measurements.

## Required evidence

For concrete examples of every variant and boundary shape, assert equality of:
`specializedDecode(bytes) -> independent model-to-Elm encoding` and
`PineDecode(bytes, hash resolver, ordinary interpreter Eval) -> ElmValueEncoding`.
Also assert native round trips, exact cached payload preservation, sharing,
determinism, embedding and malformed-input rejection.

Compare the original and shared codecs in the same runtime using
`GC.GetAllocatedBytesForCurrentThread`. Prepare inputs and delegates outside the
measurement, warm explicitly, and report exact totals, repetitions and per-call
ranges. Measure first use separately in fresh processes. Include tiny, wide,
deep, shared and large streaming payloads. Pooling can remove steady-state
overhead, not retained-result allocations or all initialization costs; report
wire-size and lifetime tradeoffs rather than claiming an unmeasured universal zero.
