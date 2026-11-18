# Binary Elm syntax and SCC storage

`Pine.Core.Elm.Syntax.Abstract.ElmSyntaxAbstractBinary` is independent of
canonicalization, compilation, and SCC models. It replaces the former syntax
JSON serializer and encodes complete files, module/import/exposing metadata,
expressions, type annotations, declarations, local bindings and patterns.

## Syntax sessions and embedding

`Encode` accepts either a `Stream` or an `Action<ReadOnlySpan<byte>>`, and a single
file, expression, declaration, or counted declaration batch. The stream remains open.
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
`DecodeFile`, `DecodeExpression`, `DecodeDeclaration`, and `DecodeDeclarations` helpers also
report their consumed extent. A new encoder/decoder starts an independent scope.

Sessions also expose `WriteFile`/`ReadFile`, `WriteModule`/`ReadModule`,
`WriteImport`/`ReadImport`, `WriteExposing`/`ReadExposing`,
`WriteTopLevelExpose`/`ReadTopLevelExpose`, and `WritePattern`/`ReadPattern`.
File encoding shares retained syntax across all declarations. Module variants,
import order, explicit exposing order, and null versus empty optional values
are preserved, not normalized.

## Syntax wire format: Pine constructors, no version header

Syntax uses the shared deterministic Pine entry format. All tags and lengths
are big-endian Int32 values. A blob is tag `1`, byte length, bytes and padding to
a multiple of four. A list is tag `3`, item count and children. An internal
reference is tag `4` followed by an Int32 offset relative to the position after
its tag; the target must be the start of a completed retained entry.

Each syntax constructor is tag `6` (Eval), an expression reference, then an
inline list environment. The first occurrence of a constructor hash is tag `5`
(external expression) and its 32-byte Pine hash; later occurrences use an
eight-byte backward reference to that entry, saving 28 bytes each. Constructor hashes
are static compiler data; there is no AST version ID. The previous `EAS` formats
are rejected. Regenerate stored syntax and dependent content addresses.
Earlier direct-hash Evals remain readable, but shared-hash re-encoding changes
binary hashes and dependent declaration/SCC/prepared-cache content addresses.

`ElmSyntaxAbstractBinary.Constructors.cs` defines the canonical Pine expressions.
They produce the Elm model in `elm-in-elm/pine-elm-syntax/src/ElmSyntax/Abstract`,
using the canonical [Elm value encoding](elm-value-encoding.md). Its
`ResolveConstructorExpression` permits ordinary Pine decoding and interpretation;
these expression representations are initialized only when that resolver is used,
never by the specialized syntax route. The generated constructor hashes are
verified against those expressions by the Elm-value equivalence tests.

The resolver also admits the frontend-independent
`Pine.Core.Encodings.ValueEncodingExpressions.BlobPadBytesTo4` expression and
its static `BlobPadBytesTo4Hash`. This expression consumes `[blob]` and returns
a blob with three zero bytes prepended to each input byte, not an Elm string.
The AST codec wraps this Eval in its existing string constructor. The
specialized decoder constructs the retained .NET string directly; ordinary
Pine interpretation performs the byte padding and then the string construction.
All input byte values are valid for the padding expression and decode as
U+0000 through U+00FF in this string context, although the encoder chooses the
specialization only for ASCII.

The encoder uses the padding Eval exactly when **every UTF-16 code unit is at
most 127 and the resulting blob is at least 64 bytes** (at least 16 characters).
This includes sufficiently long strings containing NUL or DEL. Shorter strings,
including the empty string, and all non-ASCII strings use raw UTF-32, without
replacement or normalization. The 64-byte threshold amortizes Eval overhead.
This content-only policy does not depend on culture, platform, pooling, prior
encodings or heap sharing. It is deterministic within a Pine release and is
part of the persistence/hash contract. Reference reuse applies to strings in
either representation. Previously stored raw UTF-32 strings remain decodable.
The former combined ASCII-to-Elm-string expression is no longer admitted;
regenerate its stored encodings and dependent declaration/SCC/prepared-cache
content addresses across this encoding change.

Environments follow the retained .NET model, not the canonical value shape:

| Constructor | Shallow environment |
| --- | --- |
| File | module, importCount, imports..., declarations... |
| DefaultModuleData | exposing list, module name components... |
| EffectModuleData | exposing list, optional command, optional subscription, module name components... |
| Import | optional alias, optional exposing list, module name components... |
| ModuleName | alias components... |
| Explicit | exposed items... |
| ExposedType | name, exposesConstructors (integer 0 or 1) |
| Application | function, arguments... |
| LambdaExpression | body, patterns... |
| CaseExpression | scrutinee, retained cases... |
| LetExpression | body, retained let declarations... |
| Typed | moduleCount, module components..., name, type arguments... |
| NamedPattern | retained qualified name, cached tag value, patterns... |
| Qualified name | name, module components... |
| RecordPattern | fieldCount, field names..., cached field values... |
| String | padding Eval over a raw ASCII blob when ASCII and output is at least 64 bytes; otherwise raw UTF-32 blob |
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

Constructor hash entries may be referenced in later Eval constructor positions,
including inside the first Eval's environment. The specialized shared reader
passes the same source-backed hash slice to the adapter without materializing
constructor expression objects. Those entries cannot substitute for retained
syntax children. Inline environments and raw scalar fields remain deliberately
unreferenceable. Invalid references, unknown hashes,
wrong retained types, invalid arities/counts/scalars and nesting beyond 512 reader
levels fail with `InvalidDataException`. Available execution stack is checked as
well, so smaller thread stacks can reject nesting below that limit safely.
Completed nodes remain referenceable.
Dispose decoders to return cleared scratch buffers. Retained Pine blobs own slices
of immutable input memory, not copies.

See [the reusable strategy spec](binary-tree-encoding-via-pine.md) and the
[implementation log and allocation measurements](../explore/2026-11-14-reuse-pine-value-binary-encoding-implementation-log.md).

## Prepared interpreter syntax

`Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreterPreparedBinary` encodes
`ElmSyntaxInterpreter.Prepared`, `PreparedDeclaration`, `PreparedExpression`,
and `PreparedSourceDeclaration`. It replaces prepared JSON serialization and
backs `PreparedSourceDeclaration.Codec`, including persistent test-discovery
preparation caches.

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
resolves these constructors together with the abstract constructors and generic
blob-padding expression.
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
Large string hashes are also weakly memoized, avoiding repeated scans of shared
source/prepared shader payloads. Cache and compiler-document encoding use pooled
scratch buffers, but returned bytes always own their storage; decoders never
retain pooled scratch.

Structural reuse can merge equivalent prepared expression instances. Treat a
decoded source/prepared pair as a shared template: call `Rebind` before attaching
identity-keyed source observers to obtain fresh expression occurrences and the
current binding identities. Source paths are derived from the retained model,
not serialized separately.

Malformed counts, duplicate app keys, unknown constructors, wrong retained
types, invalid references, and excessive nesting fail explicitly. The same
512-level and available-stack checks and immutable-input ownership contract
apply as for abstract syntax. Optional payload kinds are validated, including
the distinction between `Nothing` and a malformed `Just Nothing`.

## Migration and runtime costs

The syntax and prepared JSON APIs and their legacy JSON fallback are removed.
There is no automatic conversion of old persisted JSON. Preparation storage now
uses the `prepared-declarations-v3-binary` subtree. `PreparedDeclarationInput`
hashes the UTF-8 bytes `pine-unreduced-prepared-declaration-v3-binary`, a NUL
byte, then the 32-byte standalone binary syntax digest, using SHA-256. The final
digest is weakly memoized by immutable syntax identity; warm lookups neither
serialize syntax nor allocate payloads.

Canonicalization's SCC member tie-breaker uses ordinal binary digest order
instead of rendering syntax as JSON. `CompilerModelJson` remains a compiler-model
document format, but its source-declaration leaves are base64 binary syntax,
with complete-entry validation. Its general Pine/integer/name JSON converters
live in `Pine.Core.Json`, not the syntax codec. Old compiler-model documents with
JSON source declarations must also be regenerated.

Specialized decoding avoids JSON reflection and temporary canonical Pine trees.
Reference dictionaries reuse their bounded storage and perform one hash lookup
per new entry. Streaming hashes avoid intermediate byte arrays; cache encoding
allocates its owned output rather than a growing series of payload-sized arrays.
These are not guarantees that every tree is smaller or faster: constructor
envelopes can outweigh JSON for short or mostly distinct nodes. Warmed
source/prepared probes on three large compiler declarations showed lower
encoding/decoding time and allocations than JSON; synthetic wide/repeated/deep
trees and a million-character ASCII shader were checked separately. Performance
still depends on tree shape, sharing, cache warmth and the host runtime.

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
