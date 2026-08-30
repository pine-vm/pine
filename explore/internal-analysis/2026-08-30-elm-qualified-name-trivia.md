# Elm qualified-name trivia experiments

Date: 2026-08-30

## Question

Determine whether Elm permits whitespace or comments between parts of qualified names, including
expressions, types, patterns, module declarations, and imports.

## Reference tools

| Tool | Source | Version output | SHA-256 |
| --- | --- | --- | --- |
| Elm compiler | [Elm 0.19.1 Linux release](https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz) | `0.19.1` | Executable: `f8f12a61a61f64ac71a85d57284cc4d14fb81f1cbebb8b150839d9731034092e`; downloaded gzip: `e44af52bb27f725a973478e589d990a6428e115fe1bb14f03833134d6c0f155c` |
| elm-test-rs | [v3.0 Linux release](https://github.com/mpizenberg/elm-test-rs/releases/download/v3.0/elm-test-rs_linux.tar.gz) | `elm-test-rs 3.0.1` | Executable: `1562d907037774d8961ef07af860992ee4b257e391737f8c12c1a134124d76e7` |

The experiments used temporary Elm application projects with cached copies of their declared
packages. Each command used the Elm compiler above and `--output=/dev/null`. The elm-test-rs
experiments used `elm-test-rs make --offline` and selected the same Elm compiler with `--compiler`.

## Source inspection

The Elm compiler implements qualified names in
[`Parse.Variable`](https://github.com/elm/compiler/blob/c9aefb6230f5e0bda03205ab0499f6e4af924495/compiler/src/Parse/Variable.hs).
`moduleNameHelp`, `foreignUpperHelp`, and `foreignAlphaHelp` inspect the byte immediately following
each uppercase name part for `.` and then immediately parse the next part. None invokes the trivia
parser from
[`Parse.Space`](https://github.com/elm/compiler/blob/c9aefb6230f5e0bda03205ab0499f6e4af924495/compiler/src/Parse/Space.hs).
Expression parsing later consumes trivia between terms in
[`Parse.Expression`](https://github.com/elm/compiler/blob/c9aefb6230f5e0bda03205ab0499f6e4af924495/compiler/src/Parse/Expression.hs).

elm-test-rs uses
[`src/parser.rs`](https://github.com/mpizenberg/elm-test-rs/blob/d8a08a74bd2db105418e6f90966edac9591de4f4/src/parser.rs)
only to discover potential tests. Its module-name scanner accepts a contiguous dot but does not
consume trivia within the scanned identifier. Test compilation is delegated to the selected Elm
compiler by
[`src/make.rs`](https://github.com/mpizenberg/elm-test-rs/blob/d8a08a74bd2db105418e6f90966edac9591de4f4/src/make.rs).

## Experiments

The following table summarizes direct Elm 0.19.1 compiler runs:

| Form | Source fragment | Exit | Result |
| --- | --- | ---: | --- |
| Expression, block comment before dot | `Wrapper{- comment -}.beta` | 0 | Compiled. With local `type Wrapper a = Wrapper a`, this proves the parse is the application `Wrapper .beta`, where `.beta` is a record-access function. |
| Expression, space before dot | `Wrapper .beta` | 0 | Compiled with the same setup and interpretation. |
| Expression, comment after dot | `Wrapper.{- comment -}beta` | 1 | `MISSING EXPRESSION`; trivia is not accepted after a qualified-name dot. |
| Constructor expression, uppercase part after trivia | `Wrapper{- comment -}.Nested` | 1 | `EXPECTING RECORD ACCESSOR`; the qualified name ended at `Wrapper`, and `.Nested` is not a valid record accessor. |
| Type annotation | `value : Wrapper{- comment -}.Nested` | 1 | `EXPECTING DEFINITION` at the dot. |
| Constructor pattern | `(Wrapper{- comment -}.field)` | 1 | `UNEXPECTED SYMBOL` at the dot. |
| Module declaration | `module Main{- comment -}.Nested exposing (main)` | 1 | `UNFINISHED MODULE DECLARATION` at the dot. |
| Import | `import Platform{- comment -}.Cmd` | 1 | `UNFINISHED IMPORT` at the dot. |

The same expression, module, import, type, and pattern cases were placed in test modules and passed
to elm-test-rs 3.0.1. The expression case compiled successfully. The other four cases exited with
the same Elm compiler diagnostics shown above, confirming that elm-test-rs does not introduce a
different qualified-name rule during test discovery or compilation.

## Conclusion

A dot and both adjacent name parts must be contiguous to form an Elm qualified name. Any trivia
before the dot ends the qualified name. In expression position, a following `.lowerName` can then
be parsed as a separate record-access function and applied to the expression on its left. In module
declarations, imports, type annotations, and patterns there is no equivalent fallback, so the
intervening trivia makes the source invalid.
