# Elm Syntax Model and Parser

There are multiple applications depending on the parsing of Elm syntax, and the syntax model and parser are designed to accommodate their varied needs.

The Elm syntax model and Elm syntax parser in Pine support the following applications:

+ Compilation of Elm programs, translation into a lower-level form.

+ Generation of diagnostics pointing to parts of the source code, including all compilation errors.

+ Production of semantic information for functionality in language servers and IDEs (e.g., ‘Find All References’, ‘Rename Symbol’, reference count CodeLens)

+ Formatting and pretty printing: Need to preserve literal forms. Some numbers and strings can be written in more than one form, and we do not always want the formatter to erase these differences.



To ensure application developers have a good experience, we have additional requirements for usability and robustness:

+ If multiple declarations in a file contain syntax errors, we want to report all of them, not just one.

+ Features such as code formatting or code analysis should not stop working just because a module contains a syntax error.

To make this work, the parser does not stop when it finds a syntax error. Instead, it notes where the error is and which part of the code is affected, then continues through the rest of the module. With this partially finished syntax model, the formatting tool can still format the module's parts without errors, while preserving incomplete code sections, as it does with comments.

## Structured diagnostics and JSON

`ElmSyntaxParseError` carries an end-exclusive `Region`, an optional enclosing
`ContextRange`, and a structured `Kind`. Locations are one-based. Grammar branch
identities, classified tokens, related annotation names, numeric prefixes, escape
details, and indentation requirements are parser facts, independent of presentation.
The obsolete `Message` compatibility property derives concise English prose; the
parser never constructs or inspects that prose.

The diagnostic grammar tracks committed grammar stages before the lossless tree
parser runs. Invalid declaration spans are blanked without moving subsequent source
locations, then retained as `IncompleteDeclaration` nodes containing the original
text and structured error. A diagnostic that does not replace syntax, such as a
missing declaration following a documentation comment, lives in
`File.AdditionalParseErrors`. Header/import failures remain fatal.

Use `ElmSyntaxErrorRenderer.CollectErrors(parseResult)` to collect either a fatal
failure or all recovered errors, stably ordered by primary position. The formatting
API also returns these original values in `ModuleFormatResult.ParseErrors`.

```csharp
var parsed = ElmSyntaxParser.ParseModuleText(sourceText);
var options = new ElmSyntaxErrorJsonOptions("src/Input.elm");
var json = ElmSyntaxErrorRenderer.RenderJson(
    sourceText,
    ElmSyntaxErrorRenderer.CollectErrors(parsed),
    options with { WriteIndented = true });
```

`RenderJson` is a dedicated public renderer, not serialization of the parser's
record layout. It accepts source separately for excerpts and produces Elm 0.19.2
`compile-errors` JSON with zero-based coordinates and styled message segments.
Options compose the path, module name, verbosity, culture, source excerpts, styling,
indentation, and problem limit. English and invariant culture are supported;
unsupported cultures are rejected explicitly. Reference verbosity includes Elm's
explanations and hints; concise verbosity omits them.

The filesystem-derived inventory under `implement/Pine.Core.Tests/TestData/Elm/SyntaxError`
checks structured branches independently of canonical JSON. Snapshot paths use
`src/Input.elm`. Module-name expectations and port/module policy checks are excluded
from text-parser inventory tests because they require coordinator context, and are
represented separately by `ElmSyntaxErrorKind.ModuleValidation`. Pine's existing
record-expression colon-to-equals canonicalization remains supported; missing
separators are still syntax errors.


## Origin and Evolution

Initially, we used parsed Elm syntax only for compilation. We used the parser and syntax model from [`stil4m/elm-syntax`](https://github.com/stil4m/elm-syntax/tree/58671250026416cdae72100bb0c67da17dec92ee/src/Elm/Syntax) version 7 for a long time, and it has provided all the information we needed for the Elm compiler. In 2025, we began rendering generated Elm syntax for snapshot tests and formatting Elm module files.

Starting from `stil4m/elm-syntax` v7, syntax model and parser evolved to meet the needs of the applications detailed above:

+ <https://github.com/pine-vm/pine/blob/06c2abb959d818589f1f2579a1c2a3af7b003842/explore/2025-12-12-elm-formatter-and-syntax-model-design-challenge.md>
+ <https://github.com/pine-vm/pine/blob/06c2abb959d818589f1f2579a1c2a3af7b003842/guide/stil4m-concretized-syntax-model.md>
+ <https://discourse.elm-lang.org/t/elm-syntax-the-rough-edges/10507>


tags:audience-tooling-authors