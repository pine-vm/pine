# 2026-09-05 Elm Language Server Syntax Errors

So far, we have not prioritized the syntax errors reported by the Elm language server.
Now we want to bring them up to standard (<https://elm-lang.org/news/the-syntax-cliff>)

What users get from the implementation at <https://github.com/elm/compiler/tree/48befde196cbcbdf459114e36c02b52c49b58050> is our model to emulate.

## Outlook - Future Interface Design

### Verbosity Levels

In general, we will provide two verbosity levels for the textual descriptions of syntax errors: `concise` and `verbose`.

Some applications will select the `concise` representation for improved efficiency, and experienced users might prefer `concise` mode as well.

Interfaces such as the language server protocol might force rendering diagnostics to strings. In such cases, the system will determine the verbosity level before publishing diagnostics to the language client.

Long-term, snapshots in tests of syntax error reporting might be easier to maintain using the `concise` representation.

## Elm Syntax Error Survey

As preparation for implementing the automated tests, we enumerate syntax error variants produced by the reference from <https://github.com/elm/compiler/tree/48befde196cbcbdf459114e36c02b52c49b58050>

The survey does not cover errors that can depend on imports (e.g. Naming errors) or the semantic model.

For each syntax error variant, create one subheading under 'Elm 0.19.2 Syntax Error Inventory'.

Under this subheading, link the relevant source code under <https://github.com/elm/compiler/tree/48befde196cbcbdf459114e36c02b52c49b58050> and a list of concrete test cases.

For each concrete test case, create a subdirectory in `survey-elm-0-19-2`. The name of this subdirectory should begin with the slugified version of the subheading caption.

In each test case directory, place a file `Input.elm` that contains the text causing the syntax error.
In each test case directory, place a file `expected-elm-0-19-2.json` that contains the JSON produced by the `make  --report=json` command of the Elm 0.19.2 binary for `Input.elm`. But format these JSON files for better human readability.
The test directory should not contain an `elm.json` file, since these are all the same and are produced on the fly during testing if necessary.

If possible, the input should only produce one problem per fixture. In case we discover that the reference implementation produces multiple problem/error items in a branch, we will also have fixtures with multiple problem items in the JSON.

That means the different concrete test case directories are all placed directly under `survey-elm-0-19-2`, even though they belong to different error variants.

The goal is to have test cases ready to be copied verbatim into a test data directory for automated tests. We will use the JSON snapshots as a more convenient model for tests, instead of the raw data reported by the parsing function, which is more strongly typed. That means the tests will render the raw error value to JSON and then assert equality of the JSON strings after canonicalization.

### Clustering Syntax Error Variants

Any errors with different `title` values in the JSON are distinct error variants. However, there might be different error variants using the same title. Within a `title`, clustering is by the shape of the type that's appropriate to transport all the varying information that's used to compose the output.

### Elm 0.19.2 Syntax Error Inventory

The survey was generated with the official Linux x64 binary from the
[Elm 0.19.2 release](https://github.com/elm/compiler/releases/tag/0.19.2)
(`sha256:66320d27701654fa11bd0e8d84bdf9829694d5770c8dcee2dede6160fad58737`).
The release tag and the pinned reference commit contain the same
`Reporting.Error.Syntax` blob, so the released binary is an exact reference for
the renderer surveyed here.

Each heading below identifies a distinct exercised parser or renderer branch.
Branches remain separate when they produce the same JSON title. A heading can
list multiple fixtures when source inspection shows that they exercise
different nested decisions within the same outer constructor.

<!-- BEGIN GENERATED INVENTORY -->

#### Alias Body

JSON title: `UNFINISHED TYPE ALIAS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/alias-body-01/)

#### Alias Equals

JSON title: `PROBLEM IN TYPE ALIAS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/alias-equals-01/)

#### Alias Indent Body

JSON title: `UNFINISHED TYPE ALIAS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/alias-indent-body-01/)

#### Alias Indent Equals

JSON title: `UNFINISHED TYPE ALIAS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/alias-indent-equals-01/)

#### Alias Name

JSON title: `EXPECTING TYPE ALIAS NAME` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/alias-name-01/)

#### Case Arrow

JSON title: `MISSING ARROW` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/case-arrow-01/)

#### Case Arrow Colon

JSON title: `MISSING ARROW` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/case-arrow-colon-01/)

#### Case Branch

JSON title: `UNFINISHED CASE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/case-branch-01/)

#### Case Of

JSON title: `UNFINISHED CASE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/case-of-01/)

#### Case Operator

JSON title: `MISSING ARROW` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/case-operator-01/)

#### Case Pattern

JSON title: `PROBLEM IN PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4809-L4865) · [test case](survey-elm-0-19-2/case-pattern-01/)

#### Case Reserved Pattern

JSON title: `PROBLEM IN PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4809-L4865) · [test case](survey-elm-0-19-2/case-reserved-pattern-01/)

#### Case Unexpected Operator

JSON title: `PROBLEM IN PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4809-L4865) · [test case](survey-elm-0-19-2/case-unexpected-operator-01/)

#### Char Endless

JSON title: `MISSING SINGLE QUOTE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/char-endless-01/)

#### Char Escape

JSON title: `UNKNOWN ESCAPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/char-escape-01/)

#### Char Not String

JSON title: `NEEDS DOUBLE QUOTES` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/char-not-string-01/)

#### Custom Bar

JSON title: `UNFINISHED CUSTOM TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/custom-bar-01/)

#### Custom Equals

JSON title: `PROBLEM IN CUSTOM TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/custom-equals-01/)

#### Custom Indent After Bar

JSON title: `UNFINISHED CUSTOM TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/custom-indent-after-bar-01/)

#### Custom Indent After Equals

JSON title: `UNFINISHED CUSTOM TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/custom-indent-after-equals-01/)

#### Custom Indent Equals

JSON title: `UNFINISHED CUSTOM TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/custom-indent-equals-01/)

#### Custom Name

JSON title: `EXPECTING TYPE NAME` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/custom-name-01/)

#### Custom Variant

JSON title: `PROBLEM IN CUSTOM TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/custom-variant-01/)

#### Custom Variant Arg

JSON title: `UNFINISHED RECORD TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5457-L5494) · [test case](survey-elm-0-19-2/custom-variant-arg-01/)

#### Decl Expecting

JSON title: `WEIRD DECLARATION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/decl-expecting-01/)

#### Decl Import Indent

JSON title: `SYNTAX PROBLEM` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L788-L816) · [test case](survey-elm-0-19-2/decl-import-indent-01/)

#### Decl Reserved

JSON title: `RESERVED WORD` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/decl-reserved-01/)

#### Decl Symbol

JSON title: `UNEXPECTED SYMBOL` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/decl-symbol-01/)

#### Decl Upper

JSON title: `UNEXPECTED CAPITAL LETTER` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/decl-upper-01/)

#### Def Body

JSON title: `UNFINISHED DEFINITION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/def-body-01/)

#### Def Equals

JSON title: `UNFINISHED DEFINITION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/def-equals-01/)

#### Def Equals Arrow

JSON title: `PROBLEM IN DEFINITION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/def-equals-arrow-01/)

#### Def Equals As

JSON title: `PROBLEM IN DEFINITION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/def-equals-as-01/)

#### Def Indent Body

JSON title: `UNFINISHED DEFINITION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/def-indent-body-01/)

#### Def Indent Equals

JSON title: `UNFINISHED DEFINITION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/def-indent-equals-01/)

#### Def Name Match

JSON title: `NAME MISMATCH` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/def-name-match-01/)

#### Def Name Repeat

JSON title: `PROBLEM IN DEFINITION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/def-name-repeat-01/)

#### Doc Comment Fresh

JSON title: `SYNTAX PROBLEM` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1104-L1114) · [test case](survey-elm-0-19-2/doc-comment-fresh-01/)

#### Endless Comment

JSON title: `ENDLESS COMMENT` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1383-L1430) · [test case](survey-elm-0-19-2/endless-comment-01/)

#### Expecting Definition

JSON title: `EXPECTING DEFINITION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/expecting-definition-01/)

#### Exposing End

JSON title: `UNFINISHED EXPOSING` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/exposing-end-01/)

#### Exposing Operator Close

JSON title: `UNFINISHED EXPOSING` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/exposing-operator-close-01/)

#### Exposing Operator Empty

JSON title: `PROBLEM IN EXPOSING` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/exposing-operator-empty-01/)

#### Exposing Operator Reserved

JSON title: `RESERVED SYMBOL` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/exposing-operator-reserved-01/)

#### Exposing Trailing Comma

JSON title: `PROBLEM IN EXPOSING` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/exposing-trailing-comma-01/)

#### Exposing Type Privacy

JSON title: `PROBLEM EXPOSING CUSTOM TYPE VARIANTS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/exposing-type-privacy-01/)

#### Exposing Value Keyword

JSON title: `PROBLEM IN EXPOSING` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/exposing-value-keyword-01/)

#### Exposing Value Symbol

JSON title: `PROBLEM IN EXPOSING` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/exposing-value-symbol-01/)

#### Expr Access

JSON title: `EXPECTING RECORD ACCESSOR` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/expr-access-01/)

#### Expr Bad Arrow

JSON title: `UNEXPECTED ARROW` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/expr-bad-arrow-01/)

#### Expr Bad Colon

JSON title: `UNEXPECTED SYMBOL` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/expr-bad-colon-01/)

#### Expr Bad Dot

JSON title: `EXPECTING RECORD ACCESSOR` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/expr-bad-dot-01/)

#### Expr Bad Equals

JSON title: `UNEXPECTED EQUALS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/expr-bad-equals-01/)

#### Expr Bad Pipe

JSON title: `UNEXPECTED SYMBOL` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/expr-bad-pipe-01/)

#### Expr Dot

JSON title: `EXPECTING RECORD ACCESSOR` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/expr-dot-01/)

#### Expr Operator Right

JSON title: `MISSING EXPRESSION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/expr-operator-right-01/)

#### Fresh Module

JSON title: `SYNTAX PROBLEM` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L788-L816) · [test case](survey-elm-0-19-2/fresh-module-01/)

#### Fresh Type

JSON title: `SYNTAX PROBLEM` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L788-L816) · [test case](survey-elm-0-19-2/fresh-type-01/)

#### Func Arg

JSON title: `PROBLEM IN PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4809-L4865) · [test case](survey-elm-0-19-2/func-arg-01/)

#### Func Arrow

JSON title: `UNFINISHED ANONYMOUS FUNCTION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/func-arrow-01/)

#### Func Body

JSON title: `UNFINISHED ANONYMOUS FUNCTION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/func-body-01/)

#### Func Missing Argument

JSON title: `MISSING ARGUMENT` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4713-L4734) · [test case](survey-elm-0-19-2/func-missing-argument-01/)

#### If Condition

JSON title: `MISSING EXPRESSION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2574-L2611) · [test case](survey-elm-0-19-2/if-condition-01/)

#### If Else

JSON title: `UNFINISHED IF` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/if-else-01/)

#### If Else Branch

JSON title: `UNFINISHED IF` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/if-else-branch-01/)

#### If Then

JSON title: `UNFINISHED IF` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/if-then-01/)

#### If Then Branch

JSON title: `MISSING EXPRESSION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2574-L2611) · [test case](survey-elm-0-19-2/if-then-branch-01/)

#### Import Alias

JSON title: `EXPECTING IMPORT ALIAS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/import-alias-01/)

#### Import End

JSON title: `UNFINISHED IMPORT` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/import-end-01/)

#### Import Exposing

JSON title: `UNFINISHED IMPORT` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/import-exposing-01/)

#### Import Name

JSON title: `EXPECTING IMPORT NAME` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/import-name-01/)

#### Let Body

JSON title: `UNFINISHED LET` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/let-body-01/)

#### Let Def Equals

JSON title: `UNFINISHED DEFINITION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/let-def-equals-01/)

#### Let Def Name

JSON title: `UNFINISHED LET` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/let-def-name-01/)

#### Let In

JSON title: `UNFINISHED LET` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/let-in-01/)

#### Let Problem

JSON title: `UNFINISHED LET` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3180-L4010) · [test case](survey-elm-0-19-2/let-problem-01/)

#### Let Problem Alignment

JSON title: `LET PROBLEM` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3209-L3230) · [test case](survey-elm-0-19-2/let-problem-alignment-01/)

#### List End

JSON title: `UNFINISHED LIST` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/list-end-01/)

#### List Expr

JSON title: `MISSING EXPRESSION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2574-L2611) · [test case](survey-elm-0-19-2/list-expr-01/)

#### List Open

JSON title: `UNFINISHED LIST` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/list-open-01/)

#### List Trailing Comma

JSON title: `UNFINISHED LIST` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/list-trailing-comma-01/)

#### Missing Argument

JSON title: `PROBLEM IN PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4847-L4865) · [test case](survey-elm-0-19-2/missing-argument-01/)

#### Missing Colon

JSON title: `UNFINISHED DEFINITION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/missing-colon-01/)

#### Module Bad Backtick

JSON title: `SYNTAX PROBLEM` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1104-L1114) · [test case](survey-elm-0-19-2/module-bad-backtick-01/)

#### Module Bad Char

JSON title: `MISSING EXPRESSION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2574-L2611) · [test case](survey-elm-0-19-2/module-bad-char-01/)

#### Module Bad Comma

JSON title: `SYNTAX PROBLEM` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1104-L1114) · [test case](survey-elm-0-19-2/module-bad-comma-01/)

#### Module Bad Dollar

JSON title: `SYNTAX PROBLEM` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1104-L1114) · [test case](survey-elm-0-19-2/module-bad-dollar-01/)

#### Module Bad Semicolon

JSON title: `SYNTAX PROBLEM` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1104-L1114) · [test case](survey-elm-0-19-2/module-bad-semicolon-01/)

#### Module End Close

JSON title: `STRAY PARENTHESIS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1492-L1503) · [test case](survey-elm-0-19-2/module-end-close-01/)

#### Module End Comma

JSON title: `WEIRD DECLARATION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1622-L1646) · [test case](survey-elm-0-19-2/module-end-comma-01/)

#### Module End Semicolon

JSON title: `WEIRD DECLARATION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1622-L1646) · [test case](survey-elm-0-19-2/module-end-semicolon-01/)

#### Module Exposing Start

JSON title: `UNFINISHED MODULE DECLARATION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/module-exposing-start-01/)

#### Module Name

JSON title: `EXPECTING MODULE NAME` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/module-name-01/)

#### Module Name Mismatch

JSON title: `MODULE NAME MISMATCH` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L519-L537) · [test case](survey-elm-0-19-2/module-name-mismatch-01/)

#### Module Name Missing

JSON title: `MODULE NAME MISSING` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L498-L517) · [test case](survey-elm-0-19-2/module-name-missing-01/)

#### Module Problem

JSON title: `UNFINISHED MODULE DECLARATION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L649-L1378) · [test case](survey-elm-0-19-2/module-problem-01/)

#### Multistring Endless

JSON title: `ENDLESS STRING` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/multistring-endless-01/)

#### Need Indent Record

JSON title: `NEED MORE INDENTATION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/need-indent-record-01/)

#### Need Indent Record Type

JSON title: `NEED MORE INDENTATION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/need-indent-record-type-01/)

#### No Ports

JSON title: `NO PORTS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L558-L569) · [test case](survey-elm-0-19-2/no-ports-01/)

#### Number Dot

JSON title: `WEIRD NUMBER` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/number-dot-01/)

#### Number End

JSON title: `WEIRD NUMBER` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/number-end-01/)

#### Number Hex

JSON title: `WEIRD HEXIDECIMAL` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/number-hex-01/)

#### Number Leading Zero

JSON title: `LEADING ZEROS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/number-leading-zero-01/)

#### Pattern Alias

JSON title: `UNFINISHED PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/pattern-alias-01/)

#### Pattern Float

JSON title: `UNEXPECTED PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/pattern-float-01/)

#### Pattern Start

JSON title: `PROBLEM IN DEFINITION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2287-L2310) · [test case](survey-elm-0-19-2/pattern-start-01/)

#### Pattern Wildcard

JSON title: `UNEXPECTED NAME` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/pattern-wildcard-01/)

#### Plist End

JSON title: `UNFINISHED LIST PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/plist-end-01/)

#### Plist Expr

JSON title: `PROBLEM IN PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/plist-expr-01/)

#### Plist Open

JSON title: `UNFINISHED LIST PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/plist-open-01/)

#### Port Colon

JSON title: `PORT PROBLEM` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/port-colon-01/)

#### Port Indent Colon

JSON title: `UNFINISHED PORT` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/port-indent-colon-01/)

#### Port Indent Name

JSON title: `UNFINISHED PORT` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/port-indent-name-01/)

#### Port Indent Type

JSON title: `UNFINISHED PORT` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/port-indent-type-01/)

#### Port Module Exposing

JSON title: `UNFINISHED PORT MODULE DECLARATION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L711-L755) · [test case](survey-elm-0-19-2/port-module-exposing-01/)

#### Port Module Name

JSON title: `EXPECTING MODULE NAME` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L732-L752) · [test case](survey-elm-0-19-2/port-module-name-01/)

#### Port Module Problem

JSON title: `UNFINISHED PORT MODULE DECLARATION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L711-L730) · [test case](survey-elm-0-19-2/port-module-problem-01/)

#### Port Name

JSON title: `PORT PROBLEM` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/port-name-01/)

#### Port Type

JSON title: `UNFINISHED PORT` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1455-L2407) · [test case](survey-elm-0-19-2/port-type-01/)

#### Precord End

JSON title: `UNFINISHED RECORD PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/precord-end-01/)

#### Precord Field

JSON title: `UNFINISHED RECORD PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/precord-field-01/)

#### Precord Open

JSON title: `UNFINISHED RECORD PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/precord-open-01/)

#### Ptuple End

JSON title: `UNFINISHED PARENTHESES` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/ptuple-end-01/)

#### Ptuple Expr

JSON title: `PROBLEM IN PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/ptuple-expr-01/)

#### Ptuple Finished Missing

JSON title: `UNFINISHED PARENTHESES` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/ptuple-finished-missing-01/)

#### Ptuple Open

JSON title: `UNFINISHED PARENTHESES` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4797-L5348) · [test case](survey-elm-0-19-2/ptuple-open-01/)

#### Record End

JSON title: `UNFINISHED RECORD` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/record-end-01/)

#### Record Equals

JSON title: `PROBLEM IN RECORD` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/record-equals-01/)

#### Record Expr

JSON title: `MISSING EXPRESSION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2574-L2611) · [test case](survey-elm-0-19-2/record-expr-01/)

#### Record Extra Comma

JSON title: `EXTRA COMMA` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/record-extra-comma-01/)

#### Record Field Keyword

JSON title: `PROBLEM IN RECORD` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/record-field-keyword-01/)

#### Record Open

JSON title: `PROBLEM IN RECORD` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/record-open-01/)

#### Record Trailing Comma

JSON title: `PROBLEM IN RECORD` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/record-trailing-comma-01/)

#### Shader Endless

JSON title: `ENDLESS SHADER` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/shader-endless-01/)

#### Shader Problem

JSON title: `SHADER PROBLEM` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/shader-problem-01/)

#### String Endless

JSON title: `ENDLESS STRING` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/string-endless-01/)

#### String Unicode Code

JSON title: `BAD UNICODE ESCAPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/string-unicode-code-01/)

#### String Unicode Format

JSON title: `BAD UNICODE ESCAPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/string-unicode-format-01/)

#### String Unicode Long

JSON title: `BAD UNICODE ESCAPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/string-unicode-long-01/)

#### String Unicode Short

JSON title: `BAD UNICODE ESCAPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2476-L3170) · [test case](survey-elm-0-19-2/string-unicode-short-01/)

#### Stray Curly Brace

JSON title: `STRAY CURLY BRACE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1492-L1503) · [test case](survey-elm-0-19-2/stray-curly-brace-01/)

#### Stray Square Bracket

JSON title: `STRAY SQUARE BRACKET` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1492-L1503) · [test case](survey-elm-0-19-2/stray-square-bracket-01/)

#### Tab

JSON title: `NO TABS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1383-L1430) · [test case](survey-elm-0-19-2/tab-01/)

#### Trecord Colon

JSON title: `UNFINISHED RECORD TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5364-L5903) · [test case](survey-elm-0-19-2/trecord-colon-01/)

#### Trecord End

JSON title: `UNFINISHED RECORD TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5364-L5903) · [test case](survey-elm-0-19-2/trecord-end-01/)

#### Trecord Extra Comma

JSON title: `EXTRA COMMA` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5364-L5903) · [test case](survey-elm-0-19-2/trecord-extra-comma-01/)

#### Trecord Field

JSON title: `PROBLEM IN RECORD TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5364-L5903) · [test case](survey-elm-0-19-2/trecord-field-01/)

#### Trecord Open

JSON title: `UNFINISHED RECORD TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5364-L5903) · [test case](survey-elm-0-19-2/trecord-open-01/)

#### Trecord Trailing Comma

JSON title: `PROBLEM IN RECORD TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5364-L5903) · [test case](survey-elm-0-19-2/trecord-trailing-comma-01/)

#### Trecord Type

JSON title: `PROBLEM IN TYPE ALIAS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5364-L5903) · [test case](survey-elm-0-19-2/trecord-type-01/)

#### Ttuple End

JSON title: `UNFINISHED PARENTHESES` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5364-L5903) · [test case](survey-elm-0-19-2/ttuple-end-01/)

#### Ttuple Finished Missing

JSON title: `UNFINISHED PARENTHESES` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5364-L5903) · [test case](survey-elm-0-19-2/ttuple-finished-missing-01/)

#### Ttuple Open

JSON title: `PROBLEM IN TYPE ALIAS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5364-L5903) · [test case](survey-elm-0-19-2/ttuple-open-01/)

#### Ttuple Type

JSON title: `PROBLEM IN TYPE ALIAS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5364-L5903) · [test case](survey-elm-0-19-2/ttuple-type-01/)

#### Tuple End

JSON title: `UNFINISHED PARENTHESES` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/tuple-end-01/)

#### Tuple Expr

JSON title: `MISSING EXPRESSION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2574-L2611) · [test case](survey-elm-0-19-2/tuple-expr-01/)

#### Tuple Finished Missing

JSON title: `UNFINISHED PARENTHESES` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/tuple-finished-missing-01/)

#### Tuple Op Reserved

JSON title: `UNEXPECTED SYMBOL` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/tuple-op-reserved-01/)

#### Tuple Operator Close

JSON title: `UNFINISHED OPERATOR FUNCTION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4019-L4780) · [test case](survey-elm-0-19-2/tuple-operator-close-01/)

#### Type Start

JSON title: `PROBLEM IN TYPE ANNOTATION` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5364-L5903) · [test case](survey-elm-0-19-2/type-start-01/)

#### Unfinished Tuple

JSON title: `UNFINISHED TUPLE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L4444-L4465) · [test case](survey-elm-0-19-2/unfinished-tuple-01/)

#### Unfinished Tuple Pattern

JSON title: `UNFINISHED TUPLE PATTERN` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5216-L5237) · [test case](survey-elm-0-19-2/unfinished-tuple-pattern-01/)

#### Unfinished Tuple Type

JSON title: `UNFINISHED TUPLE TYPE` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L5863-L5884) · [test case](survey-elm-0-19-2/unfinished-tuple-type-01/)

#### Unexpected Comma

Renderer title: `UNEXPECTED COMMA` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1035-L1051) · pinned Linux x64 JSON title: `SYNTAX PROBLEM` · [test case](survey-elm-0-19-2/unexpected-comma-01/)

The fixture preserves the reported `DescribePathEnd` input and the actual output
from the verified Linux x64 release binary. The renderer branch exists, but the
release binary does not select it for this input; the reachability audit below
explains why.

#### Unexpected Port

JSON title: `UNEXPECTED PORTS` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L539-L555) · [test case](survey-elm-0-19-2/unexpected-port-01/)

#### Weird Else

JSON title: `UNFINISHED IF` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3994-L4010) · [test case](survey-elm-0-19-2/weird-else-01/)

#### Weird Else Branch

JSON title: `WEIRD ELSE BRANCH` · [renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3975-L3992) · [test case](survey-elm-0-19-2/weird-else-branch-01/)

<!-- END GENERATED INVENTORY -->

### Scope and Known Unexercised Branches

The fixtures cover application-project syntax that can be reached through
`elm make Input.elm --report=json` without imports or semantic analysis.
Package-only validation branches (`NoPortsInPackage`,
`NoPortModulesInPackage`, and `NoEffectsOutsideKernel`) are outside that
boundary because they require a different generated `elm.json`. Invalid UTF-8
branches are also omitted because `Input.elm` is intended to remain text.
Internal infix declarations and effect-module syntax are not accepted as
source-language constructs by the release compiler, so their report branches
cannot be reached with an ordinary application input.

The audit covered every constructor in `Reporting.Error.Syntax`, every
production parser site that creates those constructors, and every nested
renderer branch. The following application-syntax branches are present in the
reference renderer but cannot be selected by the pinned Linux x64 binary from
an `Input.elm` file:

- `ModuleBadEnd` lookahead for `UNEXPECTED SEMICOLON`,
  `UNEXPECTED COMMA`, `UNEXPECTED CHARACTER`, and the `UNEXPECTED ...` closing
  delimiter reports ([renderer branches](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L937-L1114)).
- `DeclDefEquals` and `DefEquals` lookahead for `MISSING COLON?`
  ([top-level branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L2201-L2261),
  [let branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3366-L3450)).
- `CaseArrow` lookahead for `UNEXPECTED OPERATOR` after `:` or `=`
  ([renderer branches](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L3664-L3713)).
- `FreshLine` keyword lookahead for `TOO MUCH INDENTATION`
  ([renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L772-L816))
  and `DeclFreshLineAfterDocComment` for `EXPECTING DECLARATION`
  ([renderer branch](https://github.com/elm/compiler/blob/48befde196cbcbdf459114e36c02b52c49b58050/compiler/src/Reporting/Error/Syntax.hs#L1473-L1486)).

Most of these dead branches depend on `Reporting.Render.Code.whatIsNext`.
At the pinned commit, that function applies `drop (col - 1)` even though parser
cursor columns are zero-based. It therefore inspects the character before the
error cursor. At column zero, `ModuleBadEnd` instead routes to
`toDeclStartReport`. The `unexpected-comma-01` fixture records this behavior:
the comma reaches the generic `SYNTAX PROBLEM` fallback. The same mechanism is
visible in the existing `module-bad-*`, `def-equals-arrow-01`, and
`case-arrow-colon-01` fixtures.

The inventory remains behavioral: constructors that delegate to an already
covered nested renderer are not duplicated unless they produce a distinct
message or title.

## Comparison with `avh4/elm-format`

This comparison uses
[`avh4/elm-format` at `d07fddc8c0eef412dba07be4ab8768d6abcca796`](https://github.com/avh4/elm-format/tree/d07fddc8c0eef412dba07be4ab8768d6abcca796).

### Syntax Errors

`elm-format` has its own parser and does not reuse the Elm compiler's structured
syntax-error hierarchy. Its active syntax error type has only one case,
`Parse ParsecError`; the compiler-derived module, declaration, expression,
pattern, and type error variants are present only as commented-out code
([source](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/elm-format-lib/src/Reporting/Error/Syntax.hs#L78-L112)).
Internally, a `ParsecError` can combine raw, expected, unexpected, character,
string, and number messages. These details are not rendered to users.

For every Elm parse failure, the CLI reports only the first error location:

```text
Unable to parse file PATH:LINE:COLUMN To see a detailed explanation, run elm make on the file.
```

The formatter explicitly directs the user to `elm make` for the explanation,
and its machine-readable log reduces the result further to
`"Error parsing the file"`
([console and JSON rendering](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/src/ElmFormat/Messages.hs#L57-L97)).
Although the parser returns a list of located errors, the CLI does not expose
their expected/unexpected tokens or literal-error categories
([parse integration](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/src/ElmFormat/Cli.hs#L172-L182)).

Consequently, `elm-format` adds no user-facing syntax-error variants worth
incorporating into the Elm 0.19.2 inventory. It can provide an independently
computed location, but not an additional title, explanation, hint, or source
snippet.

### It Is Not an Elm 0.19.2 Syntax Oracle

The formatter parser is deliberately repair-oriented and accepts some source
that the Elm compiler rejects. For example:

- record expression fields accept either `=` or `:` and are rendered with the
  canonical `=`
  ([record parser](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/elm-format-lib/src/Parse/Expression.hs#L123-L131),
  [lenient separator](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/elm-format-lib/src/Parse/Helpers.hs#L114-L119));
- record type fields likewise accept `=` in place of `:`
  ([type parser](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/elm-format-lib/src/Parse/Type.hs#L34-L43));
- arrows accept `=>` as well as `->`, including in lambdas, case branches, and
  function types
  ([arrow parser](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/elm-format-lib/src/Parse/Helpers.hs#L121-L124));
- a case expression accepts `then` in place of `of`
  ([case parser](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/elm-format-lib/src/Parse/Expression.hs#L208-L216));
- module and import exposing lists may omit `exposing` or parentheses
  ([module parser](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/elm-format-lib/src/Parse/Module.hs#L68-L123),
  [import parser](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/elm-format-lib/src/Parse/Module.hs#L190-L239)).

The reverse difference also exists for malformed encoding. Elm 0.19.2 has
explicit invalid-UTF-8 syntax errors, whereas this formatter revision has no
corresponding error constructors and can terminate with
`error "invalid utf-8"`
([UTF-8 adapter](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/elm-format-lib/src/Parse/ParsecAdapter.hs#L371-L418)).

Therefore a successful `elm-format` parse does not establish that Elm 0.19.2
accepts the source, and a formatter parse failure should not replace the
compiler's diagnostic.

### Other Error Kinds

`elm-format` does report failures outside Elm syntax, but they serve formatter
operations rather than Elm compilation:

- `--validate` reports a parseable file whose text differs from the formatter's
  output as `FileWouldChange`
  ([validation](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/src/ElmFormat/Cli.hs#L159-L169)).
- The JSON-to-Elm mode reports malformed or incompatible JSON as
  `JsonParseError`
  ([JSON decoding](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/src/ElmFormat/Cli.hs#L185-L192)).
- Input resolution reports nonexistent paths and directories containing no
  `.elm` files
  ([input errors](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/elm-format-lib/src/CommandLine/ResolveFiles.hs#L16-L44)).
- CLI validation reports incompatible combinations of standard input, multiple
  files, `--output`, `--validate`, and JSON modes
  ([mode selection](https://github.com/avh4/elm-format/blob/d07fddc8c0eef412dba07be4ab8768d6abcca796/src/ElmFormat/Cli.hs#L56-L109)).

It does not report naming, import resolution, type, exhaustiveness, package, or
other semantic compiler errors. Its useful extra signal is therefore
formatability—especially “parseable but not canonically formatted”—rather than
additional Elm diagnostics.

## Error Type

### Design Goals

The parser should report facts about what happened, not a presentation of those
facts. In particular, its result must be identical regardless of the locale,
verbosity, output format, terminal capabilities, or language-client settings
that a caller later selects.

The design should:

- preserve the identity of the parser or validation branch that failed;
- carry every source-derived value needed by any renderer;
- identify the exact primary region and the larger construct used for context;
- use closed, strongly typed alternatives rather than message strings;
- use the same error type for fatal and recovered parse failures;
- keep source text, paths, localized prose, hints, colors, and JSON formatting
  outside the parser error.

The existing `ElmSyntaxParseError` contains only a `Location` and a `Message`
([current type](../../implement/Pine.Core/Elm/ElmSyntax/ElmSyntaxParseError.cs)).
That is insufficient: `Message` has already committed to language and
verbosity, while one point cannot represent the reference diagnostic's region,
surrounding construct, or related locations.

### Proposed Top-Level Model

The following is a design sketch rather than the final namespace layout:

```csharp
public sealed record ElmSyntaxError(
    Range Region,
    Range? ContextRange,
    ElmSyntaxErrorKind Kind);

public abstract record ElmSyntaxErrorKind
{
    public sealed record Parse(ParseError Error) : ElmSyntaxErrorKind;

    public sealed record ModuleValidation(ModuleValidationError Error)
        : ElmSyntaxErrorKind;
}
```

`Region` is the precise inclusive-start/exclusive-end source range of the
problem. It may be empty at end of input. The parser should use the existing
one-based `SyntaxModel.Location`; an Elm-reference JSON or LSP renderer converts
that range to the target protocol's zero-based coordinates. `ContextRange`
optionally identifies the enclosing declaration or expression from which a
renderer may extract a snippet. It is not a second error location, and is
`null` when there is no meaningful enclosing construct, such as for a missing
module declaration or top-level whitespace error.

The error does not own a copy of the source. Rendering takes both separately:

```csharp
RenderedSyntaxError Render(
    string sourceText,
    ElmSyntaxError error,
    CultureInfo culture,
    SyntaxErrorVerbosity verbosity);
```

This boundary lets a renderer choose concise or verbose prose, localize every
sentence and title, and target Elm JSON, LSP, or plain text without reparsing.
It also avoids retaining a potentially large source string once parsing is
complete.

### Branch Identity

The 159 fixtures currently have 69 distinct JSON titles. A title therefore
cannot identify an error variant: several source branches deliberately share
one title but use different explanations or hints. The error kind must preserve
the branch identity discovered in this survey.

`ParseError` should mirror the grammar domains used by the reference parser:

```csharp
public abstract record ParseError
{
    public sealed record Module(ModuleParseError Error) : ParseError;
    public sealed record Exposing(ExposingParseError Error) : ParseError;
    public sealed record Declaration(DeclarationParseError Error) : ParseError;
    public sealed record Port(PortParseError Error) : ParseError;
    public sealed record TypeAlias(TypeAliasParseError Error) : ParseError;
    public sealed record CustomType(CustomTypeParseError Error) : ParseError;
    public sealed record Definition(DefinitionParseError Error) : ParseError;
    public sealed record Expression(ExpressionParseError Error) : ParseError;
    public sealed record Let(LetParseError Error) : ParseError;
    public sealed record Case(CaseParseError Error) : ParseError;
    public sealed record If(IfParseError Error) : ParseError;
    public sealed record Record(RecordParseError Error) : ParseError;
    public sealed record Tuple(TupleParseError Error) : ParseError;
    public sealed record List(ListParseError Error) : ParseError;
    public sealed record Function(FunctionParseError Error) : ParseError;
    public sealed record Pattern(PatternParseError Error) : ParseError;
    public sealed record Type(TypeParseError Error) : ParseError;
    public sealed record Literal(LiteralParseError Error) : ParseError;
    public sealed record Whitespace(WhitespaceParseError Error) : ParseError;
    public sealed record Shader(ShaderParseError Error) : ParseError;
}
```

Each domain should in turn have one leaf alternative per distinct reference
branch. A leaf may delegate to a nested error while retaining its outer branch,
as the reference does. This is preferable to a generic
`UnexpectedToken(expected, actual)` error: two branches can encounter the same
token and still require different titles, explanations, context, and hints.

For a choice type whose leaves carry no data, use an `enum`. When any leaf needs
data, use the repository's `abstract record` / nested `sealed record` pattern.
Every renderer must switch exhaustively over concrete variants and retain the
required throwing default for future variants.

For example, definition errors need to distinguish parser stages and attach
data only where that branch needs it:

```csharp
public abstract record DefinitionParseError
{
    public sealed record MissingEquals(FoundSyntax Found)
        : DefinitionParseError;

    public sealed record AnnotationNameRepeated(Identifier Name)
        : DefinitionParseError;

    public sealed record AnnotationNameMismatch(
        Located<Identifier> AnnotatedName,
        Identifier DefinedName)
        : DefinitionParseError;

    public sealed record InvalidArgument(PatternParseError Error)
        : DefinitionParseError;

    public sealed record InvalidBody(ExpressionParseError Error)
        : DefinitionParseError;

    public sealed record EqualsNotIndented(
        int RequiredColumn,
        int ActualColumn)
        : DefinitionParseError;
}
```

The concrete names can follow the implementation's terminology, but their
mapping to the inventory headings must be one-to-one and documented in tests.

### Shared Structured Values

Repeated facts should use small shared types rather than pre-rendered text:

```csharp
public abstract record FoundSyntax
{
    public sealed record EndOfFile : FoundSyntax;
    public sealed record Keyword(ElmKeyword Value) : FoundSyntax;
    public sealed record Identifier(IdentifierCase Case, string Text) : FoundSyntax;
    public sealed record Operator(string Text) : FoundSyntax;
    public sealed record Delimiter(DelimiterKind Kind) : FoundSyntax;
    public sealed record Punctuation(PunctuationKind Kind) : FoundSyntax;
    public sealed record Literal(LiteralKind Kind, string SourceText) : FoundSyntax;
    public sealed record Character(Rune Value) : FoundSyntax;
}
```

`ElmKeyword`, `IdentifierCase`, `DelimiterKind`, and `PunctuationKind` are
enums. Operator and identifier spelling remains source data, not prose.
Likewise, use enums for expression, pattern, and type sites such as
`DefinitionBody`, `RecordField`, `CaseSubject`, `CaseBranch`, `FunctionArgument`,
`TypeAlias`, and `Port`. These contexts account for reference branches whose
wording changes according to where the nested failure occurred.

The less common payloads must also remain structured:

| Error family | Data to retain |
| --- | --- |
| Module name | Path-derived expected name and declared name; the declared name is at the primary `Region` |
| Definition name | Annotated name and its related range, plus the defined name at the primary `Region` |
| Unexpected/missing syntax | Classified found token and the grammar branch; not an English “expected” string |
| Operator | Exact operator plus expression context |
| Indentation | Required column, actual column, and the construct start |
| Escape | Escape kind, parsed code point when available, and digit count |
| Number | Number problem kind and parsed integer prefix when used by the reference message |
| Wildcard-like name | Exact identifier and underscore-prefix length |
| Shader | Structured GLSL location/code when available; otherwise an explicitly opaque upstream-parser detail |

Values that can be recovered unambiguously from `sourceText` and a range need
not be duplicated. Values that reflect parser interpretation—such as whether a
name was treated as a keyword, which expression context was active, or which
indentation threshold applied—must be stored.

`Region` is authoritative for the primary location. A payload must not duplicate
that range; it carries ranges only for related source, such as the earlier
annotation in a name mismatch. This prevents the primary region and payload
from disagreeing.

The opaque shader detail is an intentional exception to the closed internal
vocabulary because it originates in another parser. It is source-tool output,
not prose selected by Pine's locale or verbosity. No other syntax-error family
should use an unclassified detail string.

### Parsing Versus Module Validation

Some surveyed Elm compiler errors are not produced by parsing source text.
Module-name mismatch depends on the file path, and package/port/effect
restrictions depend on project context. The text parser should not receive
filesystem or project policy merely to manufacture these errors.

`ModuleValidationError` should therefore be a sibling hierarchy containing
variants such as:

```csharp
public abstract record ModuleValidationError
{
    public sealed record ModuleNameMissing(string ExpectedName)
        : ModuleValidationError;

    public sealed record ModuleNameMismatch(
        string ExpectedName,
        string DeclaredName)
        : ModuleValidationError;

    public sealed record PortInNormalModule : ModuleValidationError;
    public sealed record PortModuleWithoutPorts : ModuleValidationError;
    public sealed record PortInPackage : ModuleValidationError;
    public sealed record PortModuleInPackage : ModuleValidationError;
    public sealed record EffectModuleOutsideKernel : ModuleValidationError;
}
```

The parser emits `ElmSyntaxErrorKind.Parse`; a coordinator that knows the path
and project kind emits `ElmSyntaxErrorKind.ModuleValidation`. Both flow through
the same rendering API, which is how the complete Elm 0.19.2 behavior can be
reproduced without coupling the parser to its environment.

### Fatal and Recovered Errors

Fatal parsing and incomplete-declaration recovery must carry the same
`ElmSyntaxError`. The current syntax model stores an `ElmSyntaxParseError`
inside `IncompleteDeclaration`
([current recovery model](../../implement/Pine.Core/Elm/ElmSyntax/SyntaxModel/AggregateTypes.cs));
that field should use the new structured type. The top-level
`Result<ElmSyntaxError, File>` can remain for fatal failure, while recovered
errors remain attached to successfully parsed incomplete declarations and are
collected by the formatting API.

Recovery metadata is separate from diagnostic meaning. The incomplete node
already owns the original text and declaration range, so these should not be
copied into the error merely because that error happened to be recoverable.

When an output format supports multiple problems, recovered errors are emitted
in source order. A fatal parse error is the final problem because parsing cannot
recover beyond it; if no syntax tree is produced, it is the only problem.
Stable ordering for equal regions follows parser discovery order. This gives a
deterministic mapping to Elm JSON's `problems` array without making ordering
part of the localized renderer.

### Rendering and Compatibility

No parser error field should be named `Message`, `Title`, `Hint`, `Concise`, or
`Verbose`. Those are outputs of rendering. In particular, the 69 current
English JSON titles are snapshot expectations, not parser enum values.

During migration, the existing `Message` property can be retained as a derived,
obsolete compatibility member that invokes the default English concise
renderer. New parser code must construct only structured variants. After all
callers have moved to an explicit renderer, the compatibility property and
`RenderDisplayString` can be removed from the parse-error type.

Tests should assert two independent contracts:

1. every fixture maps to the expected structured leaf and payload, proving
   branch coverage without depending on prose; and
2. rendering that value with English/reference verbosity reproduces the
   canonicalized `expected-elm-0-19-2.json`, proving presentation compatibility.

Additional tests should render the same error with every supported locale and
verbosity and assert that the original `ElmSyntaxError` remains equal. That
guards the central invariant: parsing is complete before any presentation
choice is made.
