# Elm format record expression colon-token exploration

At Elm camp I learned from Simon Lydell about this behavior in [`avh4/elm-format`](https://github.com/avh4/elm-format)

Following is the research done by GitHub Copilot:

----

Date: 2026-08-30

## Question

Determine how AVH4 elm-format and the Elm 0.19.1 compiler handle a single colon
where an equals sign is required in record expressions and record update
expressions, and identify a safe compatibility boundary for Pine's parser.

## Reference tools

| Tool | Source | Version output | SHA-256 |
| --- | --- | --- | --- |
| AVH4 elm-format | [0.8.7 Linux x64 release](https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-linux-x64.tgz) | `elm-format 0.8.7` | Executable: `9acdd1006b9e4720f48cdbbb12f16262625c2a56145d56239f9c7b9a50ed0db4`; downloaded archive: `44344c7b6f838dc5d9495dfe4253280a698c2251ee8cfa29b6d1a032b6efb13b` |
| Elm compiler | [Elm 0.19.1 Linux release](https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz) | `0.19.1` | Executable: `f8f12a61a61f64ac71a85d57284cc4d14fb81f1cbebb8b150839d9731034092e`; downloaded gzip: `e44af52bb27f725a973478e589d990a6428e115fe1bb14f03833134d6c0f155c` |

The elm-format `0.8.7` tag resolves to
[`b5cca4c26b473dab06e5d73b98148637e4770d45`](https://github.com/avh4/elm-format/tree/b5cca4c26b473dab06e5d73b98148637e4770d45).
The Elm compiler `0.19.1` tag resolves to
[`c9aefb6230f5e0bda03205ab0499f6e4af924495`](https://github.com/elm/compiler/tree/c9aefb6230f5e0bda03205ab0499f6e4af924495).

Each elm-format experiment copied the source to a temporary file and ran:

```console
elm-format Experiment.elm --yes
```

Each compiler experiment used a temporary Elm application with `elm/core`
1.0.5 and `elm/json` 1.1.3 cached locally and ran:

```console
ELM_HOME=/tmp/elm-home elm make src/Experiment.elm --output=/dev/null
```

The exact `elm.json` was:

```json
{
  "type": "application",
  "source-directories": [
    "src"
  ],
  "elm-version": "0.19.1",
  "dependencies": {
    "direct": {
      "elm/core": "1.0.5",
      "elm/json": "1.1.3"
    },
    "indirect": {}
  },
  "test-dependencies": {
    "direct": {},
    "indirect": {}
  }
}
```

The package sources were downloaded from the
[`elm/core` 1.0.5 tag](https://github.com/elm/core/archive/refs/tags/1.0.5.tar.gz)
and the
[`elm/json` 1.1.3 tag](https://github.com/elm/json/archive/refs/tags/1.1.3.tar.gz).
Their downloaded archive SHA-256 hashes were respectively
`6e37b11c88c89a68d19d0c7625f1ef39ed70c59e443def95e4de98d6748c80a7`
and
`d0635f33137e4ad3fc323f96ba280e45dc41afa51076c53d9f04fd92c2cf5c4e`.
They were extracted to
`$ELM_HOME/0.19.1/packages/elm/core/1.0.5` and
`$ELM_HOME/0.19.1/packages/elm/json/1.1.3`. This made compiler runs independent
of package-registry availability.

The compiler was run on both the original source and, when elm-format
succeeded, the formatted source. Every table fragment below was placed at the
corresponding position in a complete `module Experiment exposing (..)` module;
the complete modules for the behaviorally distinct cases follow the table.

## Summary

AVH4 elm-format accepts a single `:` in every tested record-field assignment
position. It discards that spelling and renders the canonical `=`. This applies
to:

- the first or a later field;
- record literals and record updates;
- compact syntax without spaces;
- trivia on either side of the separator; and
- nested record literals and updates.

Elm 0.19.1 rejects all those original expression forms. Every output repaired
by elm-format compiled successfully.

| Case | Relevant source | elm-format | Elm on original | Elm on formatted |
| --- | --- | ---: | ---: | ---: |
| Valid literal control | `{ alfa = 13, beta = 17 }` | 0, unchanged | 0 | 0 |
| Valid update control | `{ base \| alfa = 13, beta = 17 }` | 0, unchanged | 0 | 0 |
| Colon in first literal field | `{ alfa : 13, beta = 17 }` | 0, changed | 1 | 0 |
| Colon in later literal field | `{ alfa = 13, beta : 17 }` | 0, changed | 1 | 0 |
| Compact, all fields use colons | `{alfa:13,beta:17}` | 0, changed | 1 | 0 |
| Comments around colon | `{ alfa {- before -} : {- after -} 13 }` | 0, changed | 1 | 0 |
| Colon in first update field | `{ base \| alfa : 13, beta = 17 }` | 0, changed | 1 | 0 |
| Colon in later update field | `{ base \| alfa = 13, beta : 17 }` | 0, changed | 1 | 0 |
| Nested literal in update | `{ base \| inner : { alfa : 13 } }` | 0, changed | 1 | 0 |
| Valid record type plus colon expression | Type fields use `:`, expression fields use `:` | 0, expression changed | 1 | 0 |
| Missing update pipe | `{ base : other }` | 0, changed to a literal | 1 | 0 |
| Double colon | `{ alfa = 13, beta :: 17 }` | 1, unchanged | 1 | 1 |
| Colon plus an invalid assignment expression | `{ base : alfa = 13 }` | 1, unchanged | 1 | 1 |

elm-format prints no correction warning for accepted colon separators. Its
normal output is only `Processing file ...`. For a rejected double colon it
reported:

```console
Unable to parse file Experiment.elm:5:7 To see a detailed explanation, run elm make on the file.
```

The Elm compiler consistently points at the colon and reports:

```text
-- PROBLEM IN RECORD ---------------------------------------- src/Experiment.elm

I am partway through parsing a record, but I got stuck here:

5|     { alfa = 13, beta : 17 }
                         ^
I just saw a field name, so I was expecting to see an equals sign next. So try
putting an = sign here?
```

## Actual module texts

### Record literal: later field

This is the example from the question.

Before:

```elm
module Experiment exposing (..)


value =
    { alfa = 13, beta : 17 }
```

After elm-format:

```elm
module Experiment exposing (..)


value =
    { alfa = 13, beta = 17 }
```

Elm 0.19.1 exited 1 on the original and 0 on the formatted module.

### Record literal: compact fields

Before:

```elm
module Experiment exposing (..)


value =
    {alfa:13,beta:17}
```

After elm-format:

```elm
module Experiment exposing (..)


value =
    { alfa = 13, beta = 17 }
```

This demonstrates that adjacency does not change the recovery rule and that
each field separator is handled independently.

### Trivia around the colon

Before:

```elm
module Experiment exposing (..)


value =
    { alfa {- before -} : {- after -} 13 }
```

After elm-format:

```elm
module Experiment exposing (..)


value =
    { alfa {- before -} = {- after -} 13 }
```

The correction preserves comments on both sides of the separator.

### Record update

Before:

```elm
module Experiment exposing (..)


base =
    { alfa = 0, beta = 0 }


value =
    { base | alfa = 13, beta : 17 }
```

After elm-format:

```elm
module Experiment exposing (..)


base =
    { alfa = 0, beta = 0 }


value =
    { base | alfa = 13, beta = 17 }
```

The same result was observed when the first update field used a colon and the
second used an equals sign.

### Nested record literal inside an update

Before:

```elm
module Experiment exposing (..)


base =
    { inner = { alfa = 0 } }


value =
    { base | inner : { alfa : 13 } }
```

After elm-format:

```elm
module Experiment exposing (..)


base =
    { inner = { alfa = 0 } }


value =
    { base | inner = { alfa = 13 } }
```

Both colons are corrected according to their respective record-expression
contexts.

### Record type and record expression contexts

Before:

```elm
module Experiment exposing (..)


value : { alfa : Int, beta : Int }
value =
    { alfa : 13, beta : 17 }
```

After elm-format:

```elm
module Experiment exposing (..)


value : { alfa : Int, beta : Int }
value =
    { alfa = 13, beta = 17 }
```

The formatter preserves valid colons in the record type while correcting
colons in the record expression. The surrounding grammar, not whitespace,
determines the meaning.

elm-format also implements the inverse leniency for record types. This input:

```elm
module Experiment exposing (..)


value : { alfa = Int }
value =
    { alfa = 13 }
```

is formatted to:

```elm
module Experiment exposing (..)


value : { alfa : Int }
value =
    { alfa = 13 }
```

Elm 0.19.1 reports `UNFINISHED RECORD TYPE` on the original and says it expected
a colon after the field name.

### Important ambiguity: a missing update pipe is not inferred

Before:

```elm
module Experiment exposing (..)


base =
    { alfa = 0 }


other =
    13


value =
    { base : other }
```

After elm-format:

```elm
module Experiment exposing (..)


base =
    { alfa = 0 }


other =
    13


value =
    { base = other }
```

elm-format interprets this as a one-field record literal. It does not infer
`{ base | ... }` update syntax. The formatted module compiles, but it may not
express what a user who forgot the pipe intended.

### Double colon remains invalid

Before and after the rejected formatting attempt:

```elm
module Experiment exposing (..)


value =
    { alfa = 13, beta :: 17 }
```

Both tools reject this module. Leniency is limited to exactly one colon in the
record assignment position.

## Relevant upstream implementations

### AVH4 elm-format

The record-expression parser is
[`recordTerm`](https://github.com/avh4/elm-format/blob/b5cca4c26b473dab06e5d73b98148637e4770d45/elm-format-lib/src/Parse/Expression.hs#L123-L130).
It recognizes an update only when an optional lower-case name is followed by
`|`. Both literal and update fields then use the same `pair` parser with
`lenientEquals`.

[`lenientEquals`](https://github.com/avh4/elm-format/blob/b5cca4c26b473dab06e5d73b98148637e4770d45/elm-format-lib/src/Parse/Helpers.hs#L114-L119)
accepts either `=` or `:` and returns no indication of which token was present.
[`pair`](https://github.com/avh4/elm-format/blob/b5cca4c26b473dab06e5d73b98148637e4770d45/elm-format-lib/src/Parse/Common.hs#L16-L18)
retains only the field name and value. The renderer therefore has no original
separator spelling to preserve and emits canonical record syntax.

The same helper module defines `lenientHasType`, which accepts either `:` or
`=` in type-separator positions. This explains the inverse record-type
experiment.

### Elm compiler 0.19.1

The compiler's
[`record`](https://github.com/elm/compiler/blob/c9aefb6230f5e0bda03205ab0499f6e4af924495/compiler/src/Parse/Expression.hs#L198-L247)
parser chooses an update only for byte `0x7C` (`|`) and chooses a literal only
for byte `0x3D` (`=`). Its `chompField` function also requires `0x3D` for every
later field. There is no colon recovery.

The source AST has separate
[`Update` and `Record` variants](https://github.com/elm/compiler/blob/c9aefb6230f5e0bda03205ab0499f6e4af924495/compiler/src/AST/Source.hs#L43-L62).
The pipe is therefore the structural discriminator, while the field separator
does not survive in the AST. A single colon is also reserved rather than being
a general infix operator in
[`Parse.Symbol`](https://github.com/elm/compiler/blob/c9aefb6230f5e0bda03205ab0499f6e4af924495/compiler/src/Parse/Symbol.hs#L26-L51).

## Pine implementation status

Pine's high-level formatting API calls the C# parser before formatting:
[`ElmFormat.FormatModuleTextReportingSyntaxErrors`](https://github.com/Viir/super-duper-disco/blob/5ae5c8a0fff90098cca73d0eb360d690a0849e64/implement/Pine.Core/Elm/ElmSyntax/ElmFormat.cs#L110-L129).

The C# parser currently:

- tokenizes a lone colon as `TokenType.Colon`, while `::` and other multi-symbol
  operators are separate `TokenType.Operator` tokens
  ([lexer](https://github.com/Viir/super-duper-disco/blob/5ae5c8a0fff90098cca73d0eb360d690a0849e64/implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxParser.cs#L1180-L1198));
- discriminates a record literal only when the first field is followed by
  `TokenType.Equal`; and
- requires `TokenType.Equal` for every literal and update field
  ([record parser](https://github.com/Viir/super-duper-disco/blob/5ae5c8a0fff90098cca73d0eb360d690a0849e64/implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxParser.cs#L4917-L5124)).

The current Pine CLI formatter consequently exits 200 for the question's
example, leaves the file unchanged, and reports:

```text
5:23: Expected token of type Equal but found Colon
```

The Elm-in-Elm concrete parser makes the same strict choice. `parseRecord`
distinguishes updates with `|`, and
[`parseRecordField`](https://github.com/Viir/super-duper-disco/blob/5ae5c8a0fff90098cca73d0eb360d690a0849e64/implement/Pine.Core/Elm/elm-in-elm/pine-elm-syntax/src/ElmSyntax/Concrete/Parser/FromString.elm#L5400-L5614)
accepts only `"="`.

No syntax-model change is needed to emulate elm-format. `RecordExprField`
already stores a separator location named `EqualsLocation`, not the source
token. Pine's
[`Rendering.RenderRecordExprField`](https://github.com/Viir/super-duper-disco/blob/5ae5c8a0fff90098cca73d0eb360d690a0849e64/implement/Pine.Core/Elm/ElmSyntax/Rendering.cs#L1070-L1092)
always emits `=`, and
[`Avh4Format`](https://github.com/Viir/super-duper-disco/blob/5ae5c8a0fff90098cca73d0eb360d690a0849e64/implement/Pine.Core/Elm/ElmSyntax/Avh4Format.cs#L5009-L5051)
already constructs canonical equals locations.

## Recommended adaptation

To match the tested AVH4 behavior:

1. In `ElmSyntaxParser.ParseRecordExpr`, accept `TokenType.Colon` anywhere
   `TokenType.Equal` is accepted as a record field separator. The first-field
   lookahead must also recognize both token types.
2. Consume exactly one separator token and use its start location as the
   existing `EqualsLocation`. Do not preserve whether it was `=` or `:`.
3. In the Elm-in-Elm parser's `parseRecordField`, accept either `"="` or `":"`
   and advance exactly one character before parsing the value.
4. Keep `|` as the only record-update discriminator. Do not reinterpret a colon
   after the first name as a missing update pipe.
5. Keep `::` invalid in this position. The C# lexer already distinguishes it;
   the direct string parser should have an explicit regression test to ensure
   it never accepts only the first character.
6. Cover first and later fields, literals and updates, compact syntax, comments,
   nesting, and the rejected `::` case. Formatter tests should assert that
   accepted colons render as `=` and are stable on a second pass.
7. If broader AVH4 typo recovery is desired, separately consider accepting `=`
   where record type annotations require `:`. That is a distinct,
   context-specific rule and should not be implemented as a global token
   substitution.

## Ambiguity and safety risks

Accepting `:` only after a parsed record-expression field name has relatively
low grammatical ambiguity: record types and record expressions are selected by
their surrounding parser context, and a single colon is not a valid Elm infix
operator. The AVH4 implementation demonstrates that no speculative
whole-expression recovery is required.

There are still important behavioral risks:

- **A rejected typo becomes valid code without warning.** Formatting before
  compilation changes the compiler result from failure to success. This is
  intentional elm-format behavior, but users may not notice the correction.
- **A missing update pipe can silently produce the wrong construct.**
  `{ base : other }` becomes the record literal `{ base = other }`; it never
  means an update of `base`. Expanding leniency to guess a missing `|` would be
  ambiguous and should be avoided.
- **Global `:` to `=` substitution is unsafe.** Colons are valid and required
  in value annotations and record types. Recovery must occur only at a record
  expression field-separator parser state.
- **Prefix acceptance could hide `::`.** A character-oriented parser must not
  accidentally treat the first character of the cons token as the repaired
  field separator. Token-oriented parsing and explicit negative tests avoid
  this.
- **Diagnostics can be lost.** The current concrete syntax model retains only a
  location, so successful parsing cannot later say that a colon was repaired.
  Exact AVH4 compatibility is silent; a language server that wants to inform
  users would need separate recovered-error metadata.
- **Aggressive recovery can mask a larger malformed expression.** The
  `{ base : alfa = 13 }` experiment remains invalid because the value after the
  repaired separator is not an Elm expression. Keeping normal value-expression
  parsing strict prevents the recovery from crossing grammar boundaries.

The safest leniency boundary is therefore the AVH4 boundary: accept exactly one
colon only in the already-established record-field assignment position,
canonicalize it to `=`, and leave update detection and all surrounding syntax
strict.

----

## Direction in Pine

Based on these findings, the decision in Pine:

+ Pine supports the same leniency when parsing, and produces the canonical form when formatting. The canonicalization happens on parsing, so the distinction is not preserved and the syntax model remains the same.
+ Pine keeps the zero-configuration parser and does not offer to preserve the strict behavior.
+ If an application wants to generate errors like the 'was expecting to see an equals sign' we observed in those artifacts reported above, it can use the token location returned by the parser to check the source character.
