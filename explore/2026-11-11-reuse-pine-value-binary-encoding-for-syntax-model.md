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
