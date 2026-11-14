# Binary Elm syntax and SCC storage

`Pine.Core.Elm.Syntax.Abstract.ElmSyntaxAbstractBinary` is independent of
canonicalization, compilation, and SCC models. It encodes expressions, type
annotations, and declarations, including local bindings and patterns.

## Syntax sessions and embedding

`Encode` accepts either a `Stream` or an `Action<ReadOnlySpan<byte>>`, and a single
expression, declaration, or counted declaration batch. The stream remains open.
`ComputeHash(Declaration)` streams exactly the standalone declaration encoding
into SHA-256, with weak, identity-keyed memoization of immutable declarations.
It does not build an intermediate byte array or JSON document.

For multiple roots sharing references, use one encoder:

```csharp
using (var encoder = new ElmSyntaxAbstractBinary.Encoder(stream.Write))
{
    encoder.WriteExpression(expression);
    encoder.WriteDeclarations(declarations);
    encoder.WriteTypeAnnotation(annotation);
}
```

Writes are synchronous; `Flush` does not end the reference scope. Disposal clears
and releases reusable reference dictionaries. Strings stream ASCII or canonical
big-endian UTF-32 through a bounded stack buffer. Arbitrary-precision integers
use canonical Pine signed-magnitude blobs and pooled span storage for large
magnitudes.

Read those roots with the corresponding decoder methods:

```csharp
using var decoder = new ElmSyntaxAbstractBinary.Decoder(encodedMemory);
var expression = decoder.ReadExpression();
var declarations = decoder.ReadDeclarations();
var annotation = decoder.ReadTypeAnnotation();
var consumed = decoder.BytesRead;
```

The enclosing format determines the root kinds. Decoding consumes only those
roots, not trailing enclosing bytes. Start the decoder at the embedded sequence's
first byte and advance the enclosing reader by `BytesRead`. The standalone
`DecodeExpression`, `DecodeDeclaration`, and `DecodeDeclarations` helpers also
report their consumed extent. A new encoder/decoder starts an independent scope.

## Syntax wire format: Pine constructors, no version header

Syntax uses the shared deterministic Pine entry format. All tags and lengths
are big-endian Int32 values. A blob is tag `1`, byte length, bytes and padding to
a multiple of four. A list is tag `3`, item count and children. An internal
reference is tag `4` followed by an Int32 offset relative to the position after
its tag; the target must be the start of a completed retained entry.

Each syntax constructor is tag `6` (Eval), tag `5` (external expression), its
32-byte Pine expression hash, then an inline list environment. Constructor hashes
are static compiler data; there is no AST version ID. The previous `EAS` formats
are rejected. Regenerate stored syntax and dependent content addresses.

`ElmSyntaxAbstractBinary.Constructors.cs` defines the canonical Pine expressions.
They produce the Elm model in `elm-in-elm/pine-elm-syntax/src/ElmSyntax/Abstract`,
using the canonical [Elm value encoding](elm-value-encoding.md). Its
`ResolveConstructorExpression` permits ordinary Pine decoding and interpretation;
these expression representations are initialized only when that resolver is used,
never by the specialized syntax route. The generated constructor hashes are
verified against those expressions by the Elm-value equivalence tests.

The resolver also admits the reusable
`Pine.Core.Elm.ElmValueEncodingExpressions.StringFromAscii` expression. Its
environment is `[asciiBlob]`, with one byte per character (0 through 127), and
its result is the same canonical Elm string as the UTF-32 constructor produces.
Other codecs can use this encoded expression and its static
`StringFromAsciiHash` without depending on the AST model. The specialized
decoder validates ASCII and constructs the retained .NET string directly;
ordinary Pine interpretation expands the ASCII bytes into canonical UTF-32.

The encoder uses ASCII exactly when **every UTF-16 code unit is at most 127**,
including the empty string, NUL and DEL. Otherwise it encodes the entire string
as UTF-32, without replacement or normalization. This content-only policy has
no size threshold and does not depend on culture, platform, pooling, prior
encodings or heap sharing. It is deterministic within a Pine release and is
part of the persistence/hash contract. Reference reuse applies to strings in
either representation. Previously stored UTF-32 strings remain decodable,
but re-encoding ASCII strings now changes bytes and dependent declaration/SCC
content addresses; regenerate persistent keys across this encoding change.

Environments follow the retained .NET model, not the canonical value shape:

| Constructor | Shallow environment |
| --- | --- |
| Application | function, arguments... |
| LambdaExpression | body, patterns... |
| CaseExpression | scrutinee, retained cases... |
| LetExpression | body, retained let declarations... |
| Typed | moduleCount, module components..., name, type arguments... |
| NamedPattern | retained qualified name, cached tag value, patterns... |
| Qualified name | name, module components... |
| RecordPattern | fieldCount, field names..., cached field values... |
| String | raw ASCII blob when all characters are ASCII; otherwise raw UTF-32 blob |
| Literal | source scalar/string, cached Pine value |

Record fields, setters, function parts, signatures and other retained aggregate
objects have their own constructors. Variable-length tails allocate directly into
their final typed arrays. No temporary canonical lists, records or argument arrays
are constructed. Float literals/patterns retain their exact numerator and
denominator; the canonical Elm projection normalizes the rational.

Cached Pine values are additional payload fields, preserved exactly rather than
regenerated or included in the Elm projection. Their blobs/lists use the ordinary
shared reader with Pine factories and the same widened reference table. Strings,
qualified names, expressions and annotations can be reused across session roots.
Structural deduplication and fixed field order make bytes independent of prior
heap sharing.

Constructor expressions, inline environments and raw scalar fields are deliberately
not referenceable in the specialized profile. References to them, unknown hashes,
wrong retained types, invalid arities/counts/scalars and nesting beyond 512 reader
levels fail with `InvalidDataException`. Available execution stack is checked as
well, so smaller thread stacks can reject nesting below that limit safely.
Completed nodes remain referenceable.
Dispose decoders to return cleared scratch buffers. Retained Pine blobs own slices
of immutable input memory, not copies.

See [the reusable strategy spec](binary-tree-encoding-via-pine.md) and the
[implementation log and allocation measurements](../explore/2026-11-14-reuse-pine-value-binary-encoding-implementation-log.md).

## Prepared interpreter syntax

`Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreterPreparedBinary` adds an
opt-in codec for `ElmSyntaxInterpreter.Prepared`, `PreparedDeclaration`,
`PreparedExpression`, and `PreparedSourceDeclaration`. It does not replace
`ElmSyntaxInterpreterPreparedJson`, `PreparedSourceDeclaration.Codec`, or the
existing JSON-derived preparation cache keys.

The stream/action `Encode` overloads leave streams open. Standalone
`DecodePrepared`, `DecodeDeclaration`, `DecodeExpression`, and
`DecodeSourceDeclaration` report consumed bytes for embedding. `ComputeHash`
streams exactly the standalone prepared declaration bytes into SHA-256.
`SourceDeclarationCodec` is an `ICacheCodec<PreparedSourceDeclaration>` adapter
that requires a complete entry and rejects trailing bytes.

The prepared encoder/decoder sessions can also write/read abstract declarations
using `WriteSyntaxDeclaration`/`ReadSyntaxDeclaration`. All roots share one
reference scope. Patterns, signatures, type annotations, names, strings and
cached Pine values use the existing abstract codec; prepared expression bodies
and dispatch plans have their own constructors. The same deterministic ASCII
policy applies to all strings.

Prepared constructors use the same Eval/external-hash/inline-environment
envelope, without a separate version header. Their canonical ordinary-Pine
result is a choice named `Prepared` plus the constructor name, with one argument
containing the shallow environment list. `ResolveConstructorExpression`
resolves these constructors together with the abstract and ASCII constructors.
Static hashes are checked against ordinary interpretation by the equivalence
tests; the specialized route never builds those canonical trees.

| Constructor | Shallow environment |
| --- | --- |
| App | qualified key, prepared declaration, repeated in ordinal key order |
| SourceDeclaration | original abstract declaration, retained prepared declaration |
| Implementation | name, prepared body, patterns... |
| ValueLiteral | exact evaluated Pine value |
| LambdaExpression | prepared body, patterns... |
| LetExpression | prepared body, prepared local declarations... |
| CaseExpression | prepared scrutinee, retained dispatch plan |
| CaseDispatch | dispatch segments in their original order |
| ConstantValuePatterns | retained constant cases... |
| ConstantCase | source pattern, exact matching Pine value, prepared body |

Decoding reconstructs the retained prepared model directly; it never prepares
the source again. This preserves reduced literals, arbitrary raw Pine payloads,
cached record-field values, and constant/pattern/discard dispatch segments.
Source/prepared pairs retain both bodies, even when they differ.

Content-based prepared-expression reuse makes bytes independent of list
identity or prior heap sharing. Structural hashes are weakly memoized by
expression identity to avoid repeatedly hashing deep subtrees; dictionary
iteration and memoization do not determine the wire order. Inputs must remain
immutable, as with the abstract codec.

Structural reuse can merge equivalent prepared expression instances. Treat a
decoded source/prepared pair as a shared template: call `Rebind` before attaching
identity-keyed source observers to obtain fresh expression occurrences and the
current binding identities. Source paths are derived from the retained model,
not serialized separately.

Malformed counts, duplicate app keys, unknown constructors, wrong retained
types, invalid references, and excessive nesting fail explicitly. The same
512-level and available-stack checks and immutable-input ownership contract
apply as for abstract syntax. This new format is not a JSON compatibility reader;
switching persistence to it requires regenerating/versioning affected keys.

## SCC blobs

`Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits.SccEncodingBinary` is a
separate compiler-owned format. Each blob starts with `45 53 43 01` (`ESC`,
version 1), then a one-byte kind: value `1` or type `2`.

All maps are counted and sorted by ordinal symbol name. Member and intrinsic-role
keys are length-prefixed UTF-8 strings; qualified dependency keys store their
namespace-component list and unqualified name separately, preserving component
boundaries without string splitting. A value SCC contains member references, value
dependency references, and type dependency references, in that order. A type SCC
contains member references, type dependency references, and intrinsic-role
strings, in that order.

Each external reference is exactly 32 bytes. Member bodies and dependency
graphs are never embedded. Default member IDs are the SHA-256 standalone binary
declaration hashes; dependency IDs are the referenced SCC's binary fingerprint.
An SCC fingerprint is SHA-256 of exactly this default SCC blob. Changing this
format intentionally changes the old JSON-derived fingerprints and compiler
names; old content addresses must not be reused as binary addresses.

Encoding callbacks can store referenced declarations/SCCs and return stable
caller-defined 32-byte IDs. Decoding requires separate declaration and SCC
resolvers, trusts their ID-to-content mapping, preserves their supplied
instances, validates member/dependency kinds and SCC invariants, and reports
consumed bytes for embedding. Unresolved references fail explicitly rather than
producing partial graphs. Repeated IDs should resolve to the same retained
instances. Declaration blobs may be stored individually or loaded from batches
that share internal expression/annotation references.
