# Binary Elm abstract syntax and SCC storage

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

Disposal flushes the pooled output buffer; `Flush` makes buffered bytes visible
without ending the reference scope. Large strings are transcoded in bounded
UTF-8 chunks; arbitrary-precision integers use spans with pooled storage for
large values.

Read those roots with the corresponding decoder methods:

```csharp
var decoder = new ElmSyntaxAbstractBinary.Decoder(encodedMemory);
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

## Syntax wire format, version 1

Each session begins with the four bytes `45 41 53 01` (`EAS`, version 1).
There is no padding. Integers and lengths are big-endian. Collection and UTF-8
byte lengths are nonnegative signed 32-bit integers. Strings use strict UTF-8.
Arbitrary-precision integers are a byte length followed by minimal signed,
big-endian two's-complement bytes (zero occupies one byte). Floating-point
patterns store their IEEE 754 bits as 64 bits; float expressions store their
integer numerator and denominator separately.

Each node starts with a one-byte tag. Fields follow their model-constructor
order, except that a function's optional signature starts with byte `0` (absent)
or `1` (present). Lists start with their element count. Namespaced identifiers
store namespace-component lists and then the unqualified name.

| Category | Tags, in order starting at 1 |
| --- | --- |
| Expression | UnitExpr, StringLiteral, CharLiteral, IntegerLiteral, FloatLiteral, Negation, ListExpr, Identifier, IfBlock, PrefixOperator, Application, OperatorApplication, TupledExpression, LambdaExpression, CaseExpression, LetExpression, RecordExpr, RecordAccess, RecordAccessFunction, RecordUpdateExpression, GLSLExpression |
| Type annotation | GenericType, Typed, Unit, Tupled, Record, GenericRecord, FunctionTypeAnnotation |
| Declaration | FunctionDeclaration, ChoiceTypeDeclaration, AliasDeclaration, PortDeclaration, InfixDeclaration |
| Pattern | AllPattern, VarPattern, UnitPattern, CharPattern, StringPattern, IntPattern, FloatPattern, TuplePattern, RecordPattern, UnConsPattern, ListPattern, NamedPattern, AsPattern |
| Let declaration | LetFunction, LetDestructuring |
| Pine value | BlobValue, ListValue |

Infix directions occupy one byte: left `0`, right `1`, non-associative `2`.
Precomputed Pine values are preserved exactly, not regenerated from literals or
names. A Pine blob is its byte length and bytes; a Pine list is its count and
encoded children.

Expressions, type annotations, and Pine values have separate internal-reference
tables. Tag `0` in one of these categories is followed by a nonnegative 32-bit
table index. Every fully decoded non-reference node is appended to its
category's table, after its children; reference entries do not append another
entry. Forward/self references and indices in another category are invalid.
The encoder deduplicates structural equals, not just identical objects, so equal
syntax encodes identically regardless of prior heap sharing. Tables span every
root in a session, allowing different declarations to reuse the same expression
and annotation instances on decoding.

Malformed input, unknown versions/tags, invalid lengths/references, and nesting
deeper than 512 levels fail with `InvalidDataException`. Pine blobs retain input
memory slices; callers must keep that memory immutable while decoded syntax is
in use.

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
