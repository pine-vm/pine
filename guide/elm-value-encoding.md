# Elm Value Encoding

## Authority and Scope

This document is the normative specification of Elm values encoded as [Pine values](pine-language.md#value-model). **MUST**, **MUST NOT**, and **MAY** express requirements, prohibitions, and permitted alternatives.

The specification applies only to the **public interchange encoding at interfaces selected by the integrating application**, such as parameters and return values of a compilation entry point. A compiler, interpreter, host adapter, or other producer or consumer of those interfaces MUST adhere to this specification.

Internal code often uses different representations. This document does not prescribe the representation of compiler intermediates, interpreter objects, private function parameters, or private return values. An implementation MAY specialize, unbox, or otherwise change internal representations, provided that values crossing the selected interfaces conform to this document.

Function values and their currying or closure representation are outside this document's scope. An integrating application exposing function-valued inputs or outputs MUST also choose a compatible compiler or interpreter calling convention.

This specification defines representations, not package algorithms. Numerical approximations, including those used by square root and trigonometric functions, belong to the package versions selected by the application.

## Notation

The following notation describes actual Pine values, not textual serialization:

| Notation | Meaning |
| --- | --- |
| `Blob(04 2A)` | A blob containing the hexadecimal bytes `04` and `2A`, in that order |
| `Blob()` | An empty blob |
| `[v0, v1, ...]` | A Pine list containing the listed Pine values |
| `Text(s)` | A blob containing the Unicode scalar values of `s`, each as four bytes in big-endian order, without a byte-order mark |
| `Int(n)` | The Pine integer representing `n` |
| `Encode(v)` | Recursive encoding of an Elm value according to this document |

`Text(s)` is used for metadata and string payloads. It is not, by itself, the encoding of an Elm `String`.

Values are immutable. Storage sharing and object identity MUST NOT change the interpretation of an encoded value.

## Integers

An Elm `Int` MUST be represented as a [Pine integer](pine-language.md), without an additional wrapper.

## Characters and Strings

An Elm `Char` MUST be a blob containing exactly one Unicode scalar value as four bytes in big-endian order. Surrogate code points and values outside `U+0000` through `U+10FFFF` are not characters.

An Elm `String` MUST use this choice wrapper:

```text
[Text("<Choice>"), Text("String"), Text(stringContents)]
```

The payload contains decoded characters, not Elm source escape sequences.

| Elm value | Encoding |
| --- | --- |
| `'A'` | `Blob(00 00 00 41)` |
| `'\u{1F600}'` | `Blob(00 01 F6 00)` |
| `""` | `[Text("<Choice>"), Text("String"), Blob()]` |
| `"A"` | `[Text("<Choice>"), Text("String"), Blob(00 00 00 41)]` |
| `"A\u{1F600}"` | `[Text("<Choice>"), Text("String"), Blob(00 00 00 41 00 01 F6 00)]` |

## Booleans

The `Bool` constructors have special encodings:

| Elm value | Encoding |
| --- | --- |
| `True` | `Blob(04)` |
| `False` | `Blob(02)` |

These one-byte blobs are not the signed integer encodings of `1` and `0`. Booleans MUST NOT be emitted using the general choice wrapper.

## Lists, Tuples, and Unit

An Elm list MUST be a Pine list of recursively encoded elements, preserving their order. A tuple MUST be a Pine list of recursively encoded components, preserving their position. Unit MUST be the empty Pine list.

| Elm value | Encoding |
| --- | --- |
| `[]` | `[]` |
| `[1, 2]` | `[Int(1), Int(2)]` |
| `(1, True)` | `[Int(1), Blob(04)]` |
| `(1, 2, 3)` | `[Int(1), Int(2), Int(3)]` |
| `()` | `[]` |

List length does not identify a tuple, and the empty list does not distinguish unit from an empty Elm list. The integrating application's interface supplies those distinctions where needed.

## Choice Values

Except for the primitive representations explicitly specified in this document, an Elm choice value MUST use this flat layout:

```text
[Text("<Choice>"), Text(constructorName), Encode(arg0), Encode(arg1), ...]
```

`constructorName` is the constructor's declared, unqualified name, not a generated identifier or a module-qualified function name. Constructor arguments MUST retain declaration order. A constructor with no arguments has exactly two items. Argument at zero-based position `N` occupies list index `N + 2`.

| Elm value | Encoding |
| --- | --- |
| `Nothing` | `[Text("<Choice>"), Text("Nothing")]` |
| `Just 42` | `[Text("<Choice>"), Text("Just"), Int(42)]` |
| `Ok "A"` | `[Text("<Choice>"), Text("Ok"), Encode("A")]` |
| `LT` | `[Text("<Choice>"), Text("LT")]` |

For a declaration `type Message = Pair Int String`, `Pair 1 "A"` is:

```text
[Text("<Choice>"), Text("Pair"), Int(1), Encode("A")]
```

The encoding does not add a nominal type identity to the constructor name. Constructor lookup during compilation or interpretation remains subject to Elm name resolution.

## Records

An Elm record MUST use this flat layout:

```text
[Text("<Record>"), Text(fieldName0), Encode(fieldValue0),
                   Text(fieldName1), Encode(fieldValue1), ...]
```

Field names MUST be unique and sorted in ordinal lexicographic order, not locale-sensitive order. Ordinal order compares the UTF-16 code units of the decoded names; their wire encoding is still `Text`. Field values MUST remain paired with their names. The first item is the record marker, so the list has an odd number of items. At zero-based sorted field position `N`, the name is at index `2*N + 1` and the value at index `2*N + 2`.

The marker contains characters that are not legal in an Elm constructor name.

| Elm value | Encoding |
| --- | --- |
| `{}` | `[Text("<Record>")]` |
| `{ b = 2, a = 1 }` | `[Text("<Record>"), Text("a"), Int(1), Text("b"), Int(2)]` |
| `{ a = 1, b = 2 }` | The same encoding as the preceding record |

Record type aliases do not introduce an additional value wrapper. Record constructor argument order is a source-language calling-convention concern; it MUST NOT change this sorted interchange layout.

## Float Values

Elm `Float` values use arbitrary-precision rational components, with explicit nonfinite cases. The wrapper is:

```text
[Text("<Choice>"), Text("Elm_Float"), Int(numerator), Int(denominator)]
```

For a finite value, the denominator MUST be positive and the value is the exact quotient of numerator and denominator. Equivalent fractions need not be reduced to lowest terms. The sign belongs to the numerator; a finite zero has numerator zero and does not carry a negative-zero distinction.

An integral finite `Float` MAY instead use `Int(n)`. Consumers of a `Float` interface MUST interpret that form as the exact rational `n / 1`. A wrapper with denominator one is also valid. This permitted integer form does not make a nonintegral float an integer.

Nonfinite values retain the rational-component convention used by `Basics`:

| Numerator | Denominator | Meaning |
| --- | --- | --- |
| Positive | `0` | Positive infinity |
| Negative | `0` | Negative infinity |
| `0` | `0` | NaN |

There is no requirement to replace a nonzero infinity numerator with `1` or `-1`. A consumer MUST NOT discard its sign or reinterpret every zero-denominator value as NaN.

| Value | Encoding |
| --- | --- |
| Exact `1 / 2` | `[Text("<Choice>"), Text("Elm_Float"), Int(1), Int(2)]` |
| Exact `-3 / 2` | `[Text("<Choice>"), Text("Elm_Float"), Int(-3), Int(2)]` |
| Integral float `2` | `Int(2)` or `[Text("<Choice>"), Text("Elm_Float"), Int(2), Int(1)]` |
| Positive infinity | `[Text("<Choice>"), Text("Elm_Float"), Int(1), Int(0)]` |
| Negative infinity | `[Text("<Choice>"), Text("Elm_Float"), Int(-1), Int(0)]` |
| NaN | `[Text("<Choice>"), Text("Elm_Float"), Int(0), Int(0)]` |

This document fixes the interpretation of these representations, not the package-specific algorithm that produces a rational approximation.

## Bytes

An Elm `Bytes.Bytes` value MUST use this wrapper:

```text
[Text("<Choice>"), Text("Elm_Bytes"), Blob(rawBytes)]
```

The payload contains exactly the byte sequence, in order, with no added length, terminator, text encoding, or byte-order mark. The empty byte sequence uses `Blob()`.

For example, the three bytes `00`, `80`, and `FF` are:

```text
[Text("<Choice>"), Text("Elm_Bytes"), Blob(00 80 FF)]
```

The wrapper distinguishes a bytes value from a character or integer blob with the same payload bytes.

## Library-Defined Values

Representations of `Array`, `Dict`, `Set`, and other package-defined data structures belong to the selected library implementations. This specification does not fix their tree shapes, balancing rules, or private tags. Their public Elm components use the primitive and composite encodings above when exposed by the library.

## Conformance

Implementations MUST produce and consume the shapes specified here at the application's selected interchange interfaces, including values nested within lists, records, tuples, and choices.

The encoding is not a universal, self-describing serialization of Elm types. A consumer MUST NOT infer distinctions such as list versus tuple solely from a matching list shape.

If an interface rejects a value because its representation is invalid, it MUST report a failure rather than silently replacing it with zero, an empty value, or another successful result. The application MAY constrain an interface to a subset of Elm values, but MUST make that restriction explicit.

Conformance examples MUST compare actual Pine blobs and lists, not merely their display text. They MUST cover primitive encodings, nested composites, record field ordering, integral and nonintegral floats, all three nonfinite cases, and empty and nonempty bytes. Where alternative rational representations are permitted, decoded-value comparisons MUST account for that equivalence.
