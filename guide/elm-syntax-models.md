# Elm Syntax Models

To serve various applications working with Elm syntax, the Pine project implements the following Elm syntax models:

+ Concrete syntax model, namespace current `SyntaxModel`, future `ElmSyntaxConcrete`
+ Abstract syntax model, namespace `ElmSyntaxAbstract`
+ stil4m/elm-syntax version 7.*, namespace `Stil4mElmSyntax7`


## Concrete Syntax Model

The concrete syntax model contains:

+ Locations and ranges of tokens.
+ Trivia, like comments.
+ Redundant parens.
+ Concrete renderings of literals.

Modeling these details enables applications like code formatting to round-trip and preserve aspects of source code as users expect.

For example, the `elm-format` subcommand is designed to be consistent with the `elm-format` tool by Aaron VonderHaar. [`avh4/elm-format`](https://github.com/avh4/elm-format) preserves redundant parens, for example, in some expressions and type annotations.

## Abstract Syntax Model

The abstract syntax model is optimized for use in an interpreter or as input for compilation into lower-level languages. It lives in the namespace `Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract`.

Compared to the concrete model, it deliberately drops everything that only matters for round-tripping source text:

+ No locations or ranges.
+ No trivia like whitespace or comments.
+ No redundant parentheses (`ParenthesizedExpression` / `ParenthesizedPattern` are unwrapped during conversion).
+ Normalized literals (see below).

A big advantage of this model is that it is much more cache-friendly. Because the records carry no source positions, two syntactically identical fragments from different locations are *value-equal* and share a hash code. This, in turn, helps with uses such as the compilation and interpretation of programs, where subtrees are frequently used as cache keys or deduplicated.

### Value equality

Every node implements structural value equality (and a matching `GetHashCode`). For records that hold lists or module names, equality compares the elements in order rather than by reference. This is verified extensively by `ElmSyntaxAbstractTests.ModelTests`.

### Normalized literals

Literals are stored in a canonical, already-decoded form, so consumers never have to re-parse source text:

+ Integer literals are stored as a `System.Numerics.BigInteger`. Decimal, hexadecimal (`0x...`) and negative renderings all collapse to the same value, and the separate concrete hex/int pattern variants are merged.
+ Float literals are stored as an exact rational (numerator / denominator `BigInteger` pair), normalizing decimal and scientific (`1.0e3`) renderings.
+ String and char literals are stored with all escape sequences (e.g. `\n`, `\t`, `\\`, `\u{XXXX}`) already decoded into the actual characters.

### Precomputed `PineValue` instances

The model is designed for an interpreter that operates over `PineValue`. To avoid repeating conversions at runtime, some nodes cache a `PineValue` alongside the human-readable value:

+ Integer literals additionally hold the `PineValue` produced by `IntegerEncoding`, so an interpreter can reuse that instance instead of re-encoding the number. When such a literal is used as a `Float`, the composite rational structure happens to contain the same value as its numerator, so the cached instance is useful there too.
+ Record access, the record-access function (`.field`), and record update (record setters) hold the `PineValue` string-encoding of the field name (in addition to the original field-name string). The interpreter compares these `PineValue` field names directly when scanning a record for a matching entry, which keeps that scan efficient because no per-access string-to-`PineValue` conversion is needed.

### Converting from the concrete model

Use `ConvertFromConcrete` to obtain an abstract tree from a parsed concrete `SyntaxModel` tree. It exposes entry points for each major syntactic category (`FromFile`, `FromModule`, `FromImport`, `FromExposing`, `FromDeclaration`, `FromTypeAnnotation`, `FromPattern`, `FromExpression`). The converter performs the paren unwrapping, literal normalization, leading-dot stripping for record-access functions, and `PineValue` precomputation described above. Its behavior — including the literal normalizations — is covered by `ElmSyntaxAbstractTests.ConvertFromConcreteTests`.
