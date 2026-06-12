# 2026-06-11 - elm-format

The sope of this project is improving usability of the `elm-format` subcommand and making it the default in the Elm language server.

When working in VSCode, we want to enable formatting Elm module files despite incomplete declarations in parts of the file, and also show syntax errors in the right locations as soon as the user attempts to format or save an Elm module file.

### Reporting Syntax Errors on CLI

#### Situation Before

For the following example Elm module source:

```Elm
module Test_A exposing (..)


alfa =
    "a"

beta = ?
    

gamma =
    [1,
    2, 3]

```

The current implementation only offers the formatting, but does not yet show the syntax error:

```
══════════════════════════════════════════════════════════════════
         FILES NEEDING FORMATTING (1)
══════════════════════════════════════════════════════════════════

         K:\Source\Repos\elm-time\explore\2026-06-11-demo-elm-format\Test_A.elm

Are you sure you want to overwrite these files with formatted versions? (y/n)
```

#### Task

Expand the output on the CLI to list all syntax errors before the prompt to format. Show each syntax/parsing error on a dedicated line, and include the location as we already get it from the Elm syntax parser.

### Integrate in Elm Language Server

#### Task

Integrate the Elm format functionality into the language server we run via command `pine  lang-server`

+ When the language client requests 'Format Document', use the same formatting implementation as in the `pine  elm-format` command.
  + If that document contains syntax errors, the language server must also publish diagnostics with severity 'error', reusing the locations and messages reported by the Elm syntax parser.
+ When the language client requests 'textDocument/didSave', the language server must also report the same diagnostics.
+ The server must report diagnostics to the client such that stale diagnostics are removed on formatting or saving. As long as the Elm module contains any syntax errors, it is not necessary to try and merge diagnostics from `elm make`. However, for the whole workspace, syntax errors reported in one file must not disappear on running Elm make on a different file (that is possibly not including the file with syntax errors in its dependency graph)

---

## Implementation Report (2026-06-12)

This section reports on the implementation of the tasks described above.

### Summary

All tasks from this document were implemented:

1. The `pine elm-format` CLI now lists every recovered syntax error, each on a
   dedicated line with its source location, before prompting to overwrite files.
2. The Elm language server (`pine lang-server` / `pine lsp`) now publishes syntax-error
   diagnostics with severity `error` on both *Format Document* and `textDocument/didSave`,
   reusing the locations and messages reported by the Elm syntax parser.

### Core: exposing syntax errors from the formatter

The Elm syntax parser already recovers from declarations it cannot fully parse and stores
them on the parsed `File` as `IncompleteDeclarations`, each carrying an
`ElmSyntaxParseError` (a 1-based `Location` and a `Message`). Previously this information
was discarded by `ElmFormat.FormatModuleText`, which only returned the rendered text.

`Pine.Core/Elm/ElmSyntax/ElmFormat.cs` was extended with:

- `ElmFormat.ModuleSyntaxError` – a record carrying the precise error `Location`, the
  `Range` of the incomplete declaration, and the `Message`.
- `ElmFormat.ModuleFormatResult` – the formatted text together with the list of recovered
  `SyntaxErrors`.
- `ElmFormat.FormatModuleTextReportingSyntaxErrors(...)` – formats and reports recovered
  syntax errors. The existing `FormatModuleText` now delegates to it, so its behavior is
  unchanged for callers that do not need the errors.

A module that fails to parse entirely (for example a malformed module header) is still
reported through the `Result` error channel, as before.

### CLI: listing syntax errors before the format prompt

`pine/Elm/CLI/ElmFormatCommand.cs` now collects recovered syntax errors per file and, when
present, prints a dedicated `SYNTAX ERRORS` section listing each error on its own line in
the form `line:column: message` before the *FILES NEEDING FORMATTING* section and the
overwrite prompt. The overview header's "Syntax errors" count includes both unparseable
files and files with recovered syntax errors. In `--verify-no-changes` (CI) mode, recovered
syntax errors now cause a non-zero exit code (200), so they are not silently accepted.

For the example module in this document the CLI now prints, before the prompt:

```
══════════════════════════════════════════════════════════════════
 ✗ SYNTAX ERRORS (1)
══════════════════════════════════════════════════════════════════

✗ .../Test_A.elm
  7:8: Unsupported token type in expression: Operator at 7:8 - 7:9 - ?
```

### Language server integration

`pine/Elm/LanguageServer.cs`:

- `ComputeSyntaxErrorDiagnostics(string)` converts recovered syntax errors into LSP
  `Diagnostic`s with severity `Error` and source `elm syntax`, translating the parser's
  1-based locations to LSP's 0-based positions and spanning from the error location to the
  end of the incomplete declaration.
- `TextDocument_formatting` now also publishes syntax-error diagnostics for the document.
  Because the client applies the returned edits, diagnostics are computed from the formatted
  content so their locations match what the user sees. Publishing the list (empty when the
  module parses cleanly) ensures stale diagnostics are removed on formatting.
- `TextDocument_didSave` checks for syntax errors first; while a module contains any, it
  publishes those diagnostics and skips `elm make`. When the module parses cleanly, the
  existing `elm make` flow runs unchanged.

Diagnostics are published per document URI, so syntax errors reported for one file are not
cleared by running `elm make` on a different file.

`pine/Elm/LanguageServerRpcTarget.cs` was updated to pass the existing `publishDiagnostics`
delegate to `TextDocument_formatting`.

### Tests

- `Pine.Core.Tests/Elm/ElmSyntax/ElmFormatSyntaxErrorsTests.cs` – verifies clean modules
  report no syntax errors and that an incomplete declaration is reported with the correct
  location while the module still formats.
- `Pine.IntegrationTests/Elm/CLI/ElmFormatCommandRenderingTests.cs` – verifies the rendered
  `SYNTAX ERRORS` section (count, per-file grouping, sort order, `line:column` rendering).
- `Pine.IntegrationTests/ElmLanguageServerTests.cs` – verifies
  `ComputeSyntaxErrorDiagnostics` returns no diagnostics for valid modules and an
  `Error`-severity diagnostic at the right position for an incomplete declaration.

### Follow-up UX expansions and corrections

After the initial implementation, the following user-experience refinements were added:

- **CLI – no misleading success.** When a file is already formatted but still contains
  syntax errors, the CLI no longer reports "all file(s) are already properly formatted".
  Instead it prints a warning referencing the listed syntax errors and exits with a
  non-zero status (`200`). When files are formatted but syntax errors remain, a warning is
  appended after the "Formatted N file(s)" confirmation.
- **Language server – clear stale diagnostics on save.** When a saved module parses cleanly
  but `elm make` cannot run (for example no `elm.json` is found), the server now publishes
  an empty diagnostics list so that previously reported syntax errors for that document are
  removed rather than left stale.
