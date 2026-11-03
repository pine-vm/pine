# Pine extension for Visual Studio Code

Elm developer tools packaged as [Visual Studio Code](https://code.visualstudio.com/) extension.

## Features

+ Syntax highlighting
+ Formatting
+ Error highlighting

+ Completions - shows completion suggestions matching the current context
+ Hover tips - shows type annotations and documentation for a type alias, module, type, or function
+ Go to Definition - jump to the definition of a type alias, module, type, or function
+ Go to symbol - module outline
+ Find All References
+ Rename Symbol - renames a symbol across files
+ Reference count CodeLens - shows reference counts above declarations and opens references peek view
+ Native Test Explorer for Elm tests: source navigation, groups/skips, scoped runs,
  cancellation, continuous runs, failure comparisons and fuzz replay

### Elm testing

VS Code **1.93 or later** and a Pine language server advertising testing protocol v1
are required for Test Explorer integration. Older servers continue to provide their
existing language features without receiving testing requests.

Tests reflect current workspace contents, including unsaved edits. Use the Testing
view or editor gutter to run tests, or enable continuous runs for a selected scope.
`pine.testing.seed` and `pine.testing.fuzzRuns` configure fuzzing. The commands
**Pine: Replay Elm Fuzz Test** and **Pine: Copy Elm Fuzz Reproduction Command** reuse a previous
fuzz result; replay requires unchanged sources and inventory.

The [protocol guide](../../../../guide/elm-testing-language-server-protocol.md)
describes immutable versions, source navigation and the initial UI's scope.

## Install

Visual Studio Code is available for Mac, Windows, and Linux.

1. Install VS Code from <https://code.visualstudio.com/>
2. Follow [this link](https://marketplace.visualstudio.com/items?itemName=Pine.pine) to install the VS Code extension
3. Make sure you have Pine installed: <https://pine.build/download>
