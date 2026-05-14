# Pine Changelog - Implement

## 2026-05-09 - Elm Compiler - Migrate Elm record encoding to flat `<Record_Type>` layout

Switched the canonical encoding of Elm records from the legacy nested shape
`[Elm_Record, [[ [name, value], ... ]]]` to a flat shape
`[<Record_Type>, name0, value0, name1, value1, ...]`. Field names are still
sorted alphabetically (ordinal). The new tag uses characters that cannot
appear in an Elm constructor name, eliminating any chance of collision with a
user-defined tag.

Motivation: less list nesting per record means fewer `Pine_kernel.head` /
`skip` operations per access, fewer intermediate `PineValue.List` allocations
per record literal/update, and simpler code paths in
`ExpressionCompiler`/`PatternCompiler`/`RecordRuntime`. Several VM
performance-counter snapshots dropped accordingly (mostly visible as a
reduction in `BuildListCount`).

Back-compat for stored values: the decoder still recognizes the legacy
nested shape via dedicated `_2025` entry points (`ParsePineValueAsRecordTagged_2025`,
`ParsePineValueAsRecord_2025`, `ElmRecordAsPineValue_2025`,
`ElmRecordTypeTagName_2025` / `ElmRecordTypeTagNameAsValue_2025`). The
optimized `PineValueAsElmValue` fast path branches on both tag values, so
existing serialized state remains readable without migration.

Out of scope for this change:

+ The Elm self-hosting compiler (`elm-in-elm`) still emits the legacy layout.
  It will likely be replaced entirely in the future, so investing in its
  encoding now is not justified.
+ Cached/baked artifacts (the `danfishgold` JSON snapshot,
  `ParserFastTests.cs` snapshot) were not regenerated. They keep working
  because the legacy decoder still parses them.

## 2025-09-27 - New Pine to CS Compiler

We introduce a new compilation path to pick low-hanging fruits for optimizing runtime efficiency. The idea behind this approach is not to emit any higher-order function directly but to instantiate it for each set of function arguments in use following the given compilation roots. One limitation of this approach is it works only for a subset of functions that do not depend on any generic parse&eval (metaprogramming). This constraint is satisfied for all programs (entry points) compiled from static source languages like Elm. For details, see [2025-07-27-pine-project-siracusa.md](./../explore/2025-07-27-pine-project-siracusa.md)

How does this differ from the compilation to C# started in 2023?

+ Overall split into separate stages: First, parse from Pine as a static (monomorphic) program. That makes it easier to read and reason about the parts that perform optimizations and emissions.
+ Design to avoid exploding C# compilation times discovered earlier: Emit more statements to have less nesting in expressions.

## 2025-03-08 - Resolved Remaining Dependencies on V8 JavaScript Engine 🚀🔥✅

+ Resolved the last usage of the V8 JavaScript engine by switching applications over to Pine.
+ Pine is now *Free From JavaScript* 🌱

## 2025-03-06 - Freed Web Service Platform from JS

+ Resolved dependency on JavaScript in the web service platform. (https://github.com/pine-vm/pine/issues/20#issuecomment-1816129166)

## 2025-01-03 - Introduced New Execution Engine And IR

+ Changed the internal intermediate representation used to run Pine programs, introducing a new compiler for that IR and a corresponding execution engine.

## 2024-12-22 - Expanded VS Code Extension and Language Server

+ Added feature: 'Rename Symbol'

Headline commit: [`3c66f019`](https://github.com/pine-vm/pine/commit/3c66f01995397466b8e9ae078e0f257fe520197d) - "Expand the Elm language server to support renaming symbols across files". Same-day [`8787aecf`](https://github.com/pine-vm/pine/commit/8787aecfd47934ff6ae9a542ed2c63e9047e9897) begins tracking `elm.json` files in the language server, and [`609d28a2`](https://github.com/pine-vm/pine/commit/609d28a260e4e6fa953fc8c45eadbf33d5e86ddd) loads dependency Elm packages so hover and Go to Definition reach into package sources.

## 2024-12-19 - Expanded VS Code Extension and Language Server

+ Added feature: 'Find All References'

Headline commit: [`e61ea0ee`](https://github.com/pine-vm/pine/commit/e61ea0ee3d322bd4d0389d77145b2baca7e9b115) - "Add 'Find All References' feature in the VSCode extension".

## 2024-12-15 - Expanded VS Code Extension and Language Server

+ Added feature: 'Go to Definition'

Headline commit: [`eae68556`](https://github.com/pine-vm/pine/commit/eae6855677e0c243e3376e0bb8940325b4c75114) - "Add the 'Go to Definition' feature in the Elm language server and VSCode". Two days later, [`526a7b0d`](https://github.com/pine-vm/pine/commit/526a7b0d4c8bf9df1fafd49c7759aa5c5820cf68) adds a document symbol provider, enabling 'Go to Symbol' / module outline.

## 2024-12-08 - Expanded VS Code Extension and Language Server

+ Added feature: Completions: Shows completion suggestions matching the current context
+ Added feature: Hover tips: Shows type annotations and documentation for a type alias, module, custom type or function

Hover tips landed in [`cdb95001`](https://github.com/pine-vm/pine/commit/cdb9500176e6a814be64dc58d53c76d476b6c0db) (2024-12-07), built on the prior-day [`80085903`](https://github.com/pine-vm/pine/commit/80085903e87c77da5f1f89f2a27719751caf6c18) which integrates the language service into the bundled Elm compiler. Completions followed in [`345b6ad8`](https://github.com/pine-vm/pine/commit/345b6ad83b3edd011bd8352692566a3d3c6e9f37) (2024-12-08).

## 2024-10-26 - Added VS Code Extension and Language Server

Published the first version of the Elm developer tools VS Code extension, with a command to format Elm modules.

The LSP-based language server itself is bootstrapped in [`eabc326d`](https://github.com/pine-vm/pine/commit/eabc326d9b250dfc6b361f2cf36cf3aff4e67100) - "Begin implement the language server using LSP" (2024-10-20), with the formatting feature added in [`297bf6ff`](https://github.com/pine-vm/pine/commit/297bf6ff205689eb0a9141b31971e979b00b00b9) - "Expand language server to support formatting Elm module text" (2024-10-23).

## 2024-10-05 - Basic Support for Elm Command-Line Applications

+ Added a framework to integrate the various interfaces we want to use in command-line interface applications (`Platform.CommandLineApp`), including stdIn/stdOut/stdErr streams and environment variables.
+ Introduced the `pine  run` command as a common way to run an app.

## 2024-09-19 - Basic Support for Elm Float Type

Expand the Elm compiler and core libraries to support `Float` literals and `String.toFloat`

## 2024-08-31 - Optimize Pine Expression Encoding

So far, I expected to develop the standard expression encoding into a form completely representable in a file system tree. With such an encoding, we could take a tree from the file system and directly feed it into Parse&Eval. However, I changed my mind about the tradeoffs and now change the encoding in the opposite direction, optimizing for compactness and symmetry with the Elm value encoding.
If we still want to (re)use file systems to model syntax trees, we will add a mapping for such a form later, meaning there would be another hash code for that encoding.

## 2024-07-14 - (R)Evolution of Code Analysis

The previous approach to code analysis aimed at identifying the parts of the environment used to parse expressions. Looking closer at optimizing execution in PineVM this month, it became clear we want to add more parts of the environment constraints collected for specialized compilation. Expanding the environment constraint to cover parts that conditions of conditional expressions depend on allows us to resolve the condition when compiling the specialized representation. This compile-time reduction saves us the check at runtime and shrinks the code size because we eliminate some branches.

But how do we expand the code analysis to cover the condition expressions? Initially, I expected to expand the existing code, adding something similar to what we did so far to identify the subexpressions used in parsing. However, this commit adds an approach to collecting the environment constraints that is entirely different from the one used so far. Instead of looking into the contents of the expression and recursively descending to collect all relevant paths, this new approach does not look into the expression. Instead, it generates classes only by computing intersections of the different concrete environments observed for a given expression. It then computes a score for each class by counting how many of the observed concrete environments match the class. This approach seems preferable for now because it is much simpler. However, the concrete implementation still needs to be refined.

## 2024-05-06 - Elm Compiler - Add runtime type checks and crashes for invalid record access or update

+ Added test cases to check we get specific error messages for four distinct classes of errors: Record access on non-record value, record access with invalid field name, record update on non-record value, and record update with invalid field name.
+ Change the Elm compiler implementation to emit runtime checks to distinguish these four cases and crash the program with the corresponding message. (Checking whether a value represents a record is possible because we use wrap record fields in the "Elm_Record" tag)

## 2024-03-15 Pine to CS Compiler - Introduce specialized parameter lists

Up to March, all the C# methods generated to represent compiled expressions shared the same parameter list. This parameter list contained a single entry for the environment corresponding to the `environment` property of the Pine ParseAndEval expression.
The change 2024-03-15 introduces specialized parameter lists. The compiler now generates parameter lists that result in lower runtime expenses. Since these specialized parameters correspond to parts of the environment, the invoked function needs less processing to retrieve them. At the call sites, the specialized parameters result in fewer heap allocations and processing to package all used parts into a single argument. Besides the better runtime efficiency, another benefit is that the generated C# code is less cluttered and easier to read.

## 2024-03-10 - Pine to CS Compiler - Change return type of compiled Pine expressions

Changed the return type from the compiled representations of expressions from `Result<string, PineValue>` to `PineValue`.
Cases that previously resulted in an error Result are now mapped to runtime exceptions locally.

In which cases did we use the 'Err' case of that 'Result' type?
In the past, kernel functions could return errors, but in 2023, we changed kernel functions to never fail and always return a plain `PineValue`. Since then, only parsing errors from `ParseAndEval` expressions could return an error. The original motivation to use a `Result` was to support finding causes of program errors. However, it might be better to repeat the execution in an interpreter configured for that purpose instead of mixing this support into the compiled code.

## 2023-05-07 - Web Service Platform - Database Functions

Added a feature in the web service platform (then "Elm-Time") that lets operators apply Elm functions to a live application's database from the admin interface, without redeploying. App developers expose functions by placing them in an Elm module named `Backend.ExposeFunctionsToAdmin`. The runtime introspects the function signatures and:

+ Automatically supplies the current application state for any parameter whose type matches the application state type.
+ Treats functions whose return type matches the application state type as updates (commit-able), and the rest as queries/reports.
+ Surfaces the exposed functions both in the GUI admin interface (new "Database Functions" section, with parameter forms, dry-run by default, and a 'Commit resulting state to database' checkbox) and on the CLI via the new `list-functions` and `apply-function` commands (with `--commit-resulting-state` for committing).

This is a less heavyweight alternative to migrations for everyday production data edits: it preserves atomicity and type-checking, but avoids running a deployment.

The compiler/admin-interface plumbing that tracks parameter names and source text of type annotations for exposed functions landed in [`83c2a5b0`](https://github.com/pine-vm/pine/commit/83c2a5b0aa1cfa749377ba3f14c1d74478b9d023) (2023-05-01). The graphical "Apply Function on Database" section landed in [`e7ec3f9d`](https://github.com/pine-vm/pine/commit/e7ec3f9d6870d6d722d49cd0816b09170c1f54b4) (2023-05-05) and was polished for readability (full signatures, return types) in [`c86672fa`](https://github.com/pine-vm/pine/commit/c86672fad86002577ad100f2f2369ebb2a5a20ae) (2023-05-06). The naming cleanup in [`0ae86d63`](https://github.com/pine-vm/pine/commit/0ae86d63e4353c8225794fd3cc214121d6c02847) (2023-05-07) ships in the same `v2023-05-07` release that first contains the feature.

## 2023-04-05 - Elm Silent Teacher

Introduced *Elm Silent Teacher*, an interactive course for learning Elm through small "evaluate this expression" exercises. The implementation lives in `implement/elm-time/ElmTime/learn-elm` and is itself an Elm full-stack app. Compared to the prior implementation:

+ Course authors no longer have to provide a function that computes the correct answer for each exercise. An interpreter running in the browser evaluates the expression instead.
+ Because the interpreter is available client-side, after each answered exercise the learner is offered a REPL-like sandbox seeded with the exercise expression so they can mutate it freely and watch the value change.

The interactive sandbox after each challenge landed in [`cc583063`](https://github.com/pine-vm/pine/commit/cc583063f1b1936a6a6b44575a55cbed017032cb) (2023-04-05) and the exercises were extracted into a dedicated `Exercise` module in [`44d3700a`](https://github.com/pine-vm/pine/commit/44d3700a95643f1b070a09ce9d3db5d392c829ec) (2023-04-05). Post-launch refinements based on community feedback include [`c30e0b79`](https://github.com/pine-vm/pine/commit/c30e0b7963199d3e45b154111a59d2a78e19ec01) (2023-04-23, exercise tweaks), [`20924fe7`](https://github.com/pine-vm/pine/commit/20924fe72d7423fa8b8e502cd074ceebf9e0be6f) (2023-04-24, avoid back-to-back duplicate challenges) and [`6e2cc783`](https://github.com/pine-vm/pine/commit/6e2cc7836c2fd45b8b17f4f8b23eed32018bee7c) (2023-04-24, interleave exercises across topics for retention).

## 2023-02-15 - Project rename: Elm-Fullstack → Elm-Time

Renamed the project, the GitHub organisation/repository (`elm-time/elm-time`) and the executable file from `elm-fullstack` to `elm-time`, and adapted names in source code and guides accordingly. Anchor commit: [`ae3c34a3`](https://github.com/pine-vm/pine/commit/ae3c34a3a883e9f4266d4288c59cd3623f2f1377).

## 2020-09-26 - Elm Editor - First version of the web-based IDE for Elm

Added the *Elm Editor* example app — a browser-based IDE for Elm built on the Monaco editor and itself implemented as an Elm-Fullstack full-stack app. Initial features include:

+ Viewing and editing Elm modules and other text files in a project.
+ Compiling via Elm make and reporting compiler errors with markers in the source and on-hover descriptions.
+ Saving and sharing the current project state, including all files.
+ Importing complete projects from public git repositories.
+ Iframe-based preview for front-end web apps.

The example app was first wired up in [`c8295012`](https://github.com/pine-vm/pine/commit/c8295012d2d522d09d519a3469a9630c939f0fec) (2020-09-26), with [`c3d15b50`](https://github.com/pine-vm/pine/commit/c3d15b5049f1c1feac4106c57f473446882ca3a1) the same day expanding the default `elm.json` (SVG/WebGL) and switching the default app code to a file-based representation. Compilation UX iteration followed in [`3483fba3`](https://github.com/pine-vm/pine/commit/3483fba3e06041f0a15c8277af29a547b08238bb) (in-progress indicator, 2020-10-11) and [`6b0f23f9`](https://github.com/pine-vm/pine/commit/6b0f23f9d751286409c49e89e22098ac67bc8e92) (Elm-make output layout, 2020-10-11). Later in the year, [`056b4ae5`](https://github.com/pine-vm/pine/commit/056b4ae531ca9ca1a1097c3ebe3c1dac18b6a7d9) (2020-12-29) added keyboard shortcuts for format/compile and [`5a823e55`](https://github.com/pine-vm/pine/commit/5a823e5552cc795d4d52bcded4ece3ed5ed90eb3) (2020-12-29) added activity-sidebar affordances for collaboration.

## 2020-04-30 - Web Service Platform - Expand persistence with deployments and state migrations

Persistence of the backend application state had been part of the web service platform since 2018-2019. This change expands that persistence to make program-version changes a first-class, atomic operation:

+ Every deployment can carry a *migration* function that maps the current backend state to the next version's state. The platform type-checks the migration across versions and rejects mismatches at deploy time.
+ Migrations are declared as a normal Elm function in a module with a well-known name (`Backend.MigrateState`), exposing `migrate : OldState -> ( NewState, ElmFullstack.BackendCmds NewState )`. The return type matches the regular update function (minus the event argument), so migrations can also issue commands.
+ Custom-type compatibility for migrations is intentionally relaxed: only constructor-tag names are compared, not module names, so old type declarations can be parked under a common prefix without breaking the migration.
+ Operators who prefer to discard the existing state on deployment can opt out via an option on the `deploy` CLI command, which then takes the new state from the app's `init` function instead of running a migration.

For background and design rationale, see the blog post [Design Report - Migrations in Elm Fullstack Deployments](https://michaelrätzel.com/blog/design-report-migrations-in-elm-fullstack-deployments).

The backbone landed in [`56b4a19b`](https://github.com/pine-vm/pine/commit/56b4a19bae81afdf466024c7f1ebff21eeace5b8) - "Persistent history for deployment and migration 🚀🎉" (2020-04-30), preceded by [`8efd72f8`](https://github.com/pine-vm/pine/commit/8efd72f8750651801877ad9b085aa3fbb84c74e1) (2020-04-27) which promoted the new admin interface (where deployments and migrations live) to the default `run-server` command. Closely related operational features around the same window include [`92265a37`](https://github.com/pine-vm/pine/commit/92265a374eb42019de89a3fe5965bc286878fd30) (2020-05-06, revert process to an earlier state), [`dcf3a63b`](https://github.com/pine-vm/pine/commit/dcf3a63b7847ac072d49fd51c4d21f422f6175d0) (2020-05-11, deployment reports) and [`869fdcbd`](https://github.com/pine-vm/pine/commit/869fdcbdae61ec60be00ecf8f2d6caf2c752b15d) (2020-05-18, atomic synchronous deployment on `run-server`).
