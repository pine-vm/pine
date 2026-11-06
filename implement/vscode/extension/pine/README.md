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

### Build a side-by-side development package

With Node.js 22 or later installed, run from this directory:

```shell
npm ci
npm run test:package
npm run package:dev
```

The VSIX is written to `out/pine-dev-<version>.vsix`. Install it using
**Extensions: Install from VSIX...**, or:

```shell
code --install-extension out/pine-dev-0.3.8.vsix
```

This package is **Pine Dev** (`Pine.pine-dev`); the release manifest is not modified.
Its settings are independent of the installed release:

```json
{
    "pineDevLanguageServer.pathToPineExecutableFile": "C:\\path\\to\\pine.exe",
    "pineDevLanguageServer.trace.server": "verbose",
    "pineDev.testing.fuzzRuns": 100,
    "pineDev.testing.seed": null
}
```

An empty executable path still uses `pine` from `PATH`. The language server is not
bundled; point the setting at your local build to test server changes.
Commands and output channels are labeled **Pine Dev**, and the Test Explorer
controller and versioned test-source documents have separate IDs.

Both extensions can be installed together, but enabling both in the same workspace
runs two language servers and can duplicate language features and tests. Disable
the release extension **for this workspace** while testing Pine Dev if desired.
