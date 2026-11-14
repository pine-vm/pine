# Elm Make ECMAScript

## Implementation

The maintenance CLI and reference corpus live in
`implement/Pine.IntegrationTests/TestData/ElmMakeEcmascript`.
Scenario directories live under `scenarios`, for example `scenarios/static-html-text-empty`;
the shared `packages` and `elm-reference` directories remain at the corpus root.
From this checkout, use:

```text
pine elm maintain make-ecmascript stats
pine elm maintain make-ecmascript update-packages
pine elm maintain make-ecmascript generate static-html-text-empty/expect/0-19-1/src/Main.elm.js
pine elm maintain make-ecmascript generate static-html-text-empty/expect/0-19-1/src/Main.elm.html
pine elm maintain make-ecmascript generate browser-programs/expect/0-19-1/src/Sandbox.elm.debug.js
pine elm maintain make-ecmascript generate browser-programs/expect/0-19-1/src/Sandbox.elm.optimize.html
pine elm maintain make-ecmascript run static-html-text-empty
pine elm maintain make-ecmascript run
pine elm maintain make-ecmascript run --execution-path SyntaxInterpreter
pine elm maintain make-ecmascript run static-html-text-empty --reference
pine elm maintain make-ecmascript run error-no-manifest
pine elm maintain make-ecmascript generate error-no-manifest/expect/0-19-1/src/Main.elm.html.errors-omitting-details.txt
```

These maintainer-only commands are hidden from default CLI help. Use
`pine elm maintain make-ecmascript --help` to inspect them explicitly.
`--test-data <directory>` selects another corpus root. Generation accepts an
expectation path relative to its `scenarios` directory and refuses to overwrite
an existing file. To start a scenario, add `scenarios/<name>/input-app` and
generate its first expectation. Expectations are stored under
`scenarios/<name>/[skip/]expect/<major-minor-patch>/`, with `0-19-1` identifying
the Elm 0.19.1 target. The qualifier is part of the typed compilation request and
snapshot identity, so different compiler versions can have independent outputs
and diagnostics for the same entry point. Discovery and reporting retain every
version; compilation and generation currently reject targets other than 0.19.1
explicitly rather than using the wrong compiler.
The version qualifier is mandatory and uses three non-negative integers separated
by hyphens, without leading zeroes. Unqualified legacy paths and dotted version
directory names are rejected. Skipped snapshots use the same qualifier, for example
`scenarios/<name>/skip/expect/0-19-1/src/Main.elm.js`.
Package maintenance covers active and skipped expectations, including the
governing application's test dependency pins that the classic compiler verifies.
Existing archives are authoritative: a corrupt archive fails rather than being
silently refreshed, and unused archives and registry entries are retained.

Reference compilation uses the pinned Elm 0.19.1 executable, a fresh temporary
application and `ELM_HOME`, and loopback-only rejecting proxies to prevent
registry updates or package downloads. Only explicit package maintenance and
generation may download missing package archives. The compiler executable itself
is obtained using Pine's existing hash-pinned binary cache.

The snapshot runner and reporting are shared by the CLI and integration tests.
Expectation files define the cases to run; scenarios do not need every combination
of output target and compiler flavor. Coverage audits check every retained output
and require at least one active expectation per entry point, not a complete matrix.
Live-reference variation tests construct their compilation requests independently
of the stored snapshots.
`run` supports `--skip-expect-debug`, `--skip-expect-optimize` and
`--max-diff-hunks <count>`. By default, `run` and
`ElmMakeEcmascript_Run_all_snapshots` execute the actual Elm implementation in
`implement/elm-in-elm/elm-make-ecmascript` using PineVM. The compiler program is
compiled to Pine by the existing .NET compiler, without invoking classic Elm.
`--execution-path SyntaxInterpreter` executes the same source through the syntax
interpreter. `--reference` is an explicit reproducibility check only, never a
fallback for unsupported features or mismatched output.

For PineVM corpus replay, all expectations across the run share an
`ElmMakeCompilationCache`: invocation/eval results, expression compilations,
parsed and encoded expressions, reductions, and expression-template plans.
Each compilation still creates a fresh VM with independent execution state.
`ElmAppCompilation.AsCompletelyLoweredElmApp` creates the same cache scope by
default for the whole lowering operation, retaining it across dependency demands,
entry points, output targets, flavors, and recursive lowering iterations.
Its cache-taking overload can retain that scope across multiple lowering operations.
Syntax-interpreter execution is unchanged.

The compiler parses Elm with `ElmSyntax.Concrete.Parser.FromString`,
resolves names against original package sources, classifies static Html and
unit-flags Program mains from signatures and aliases, traverses reachable declarations
and kernels, and emits development, debug, and optimized JavaScript/HTML. It also supplies
`AsCompletelyLoweredElmApp`'s existing `MissingDependencyError` handover.
Parsing takes only a module's text as its cache input; emission sees declaration
syntax plus module interfaces, not complete unrelated package source bodies.

The retained `static-html-text-empty`, `expressions-and-patterns`, and `browser-programs`
development snapshots match the classic compiler without changing their contents.
The shared conformance runner supports both execution backends; backend cases
enabled in `ElmMakeEcmascript_Run_all_snapshots` select automatic corpus replay.
The emitter has separate ECMAScript syntax/layout, identifier names, literal
encoding, pattern binding and decision stages, tail-call lowering, strongly
connected local initializer ordering, source-literal origins, and recursive
dependency-graph traversal. The literal encoder supplies classic character
expression and pattern forms, including their different carriage-return escapes;
source-sensitive string spelling remains in the literal-origin stage.
It preserves
classic core-call specialization, arity helpers, generated names, shared branch
labels, source literal spelling, and kernel substitutions.
Fresh-name allocation distinguishes top-level/lambda and self-recursive local
pattern parameters from other local-function patterns and unused parameters.
Flattened forward pipelines allocate the input before existing function arguments
while retaining normal JavaScript call and argument order.
Syntax, import/export names, type names, and unused declarations in imported
modules are checked before dead-code removal.
Imports sharing a qualifier are resolved by the requested exported value or type;
only references with multiple matching exports are ambiguous. Imported infix
operators resolve to their declared implementing functions, including private
implementations, and use the same arity helpers as ordinary function calls.
Mixed top-level function/value cycles use named value thunks during initialization
and memoize each initialized value. References within the component call those
thunks; external references use ordinary bindings. Eager initializer dependencies
determine the component's shared fresh-name allocation order, excluding deferred
lambda and local-function bodies. Failed initialization retains the classic
runtime recursion diagnostic.

**Supported scope:** development, debug, and optimized output for statically determined Html mains and
Program mains with unit flags, including Browser sandbox/element/document/
application and Platform workers. Official package effect managers use shared
command/subscription registration nodes with ordered initialization dependencies.
Main initialization is a separate compiler stage and contributes its own decoder
dependencies; only debug Program output retains the debugger kernel and message
type metadata. Production mode shares a field-frequency map across source and
kernel emission, shortens fields, unboxes single-field constructors, enums,
characters and unit, and preserves classic special constructor tags.
Debug references in compiled application modules, including unused declarations, produce a
structured `DebugRemnants` diagnostic under `--optimize`.
Ports, application-defined effect managers, non-unit
flags, GLSL, pure eager value cycles, recursive local value initialization,
incomplete case decisions, and
unresolved higher-order main types return explicit unsupported-feature errors.
Debug metadata requires a known message type: an explicit Program signature
(including aliases), a referenced main, or a supported Program constructor with
a referenced, annotated update function. Other inference remains explicitly
unsupported rather than emitting incomplete debugger metadata.
This is not a claim of full Elm language coverage.
Successful-output expectations fail on compiler errors; compiler-error expectations
fail on successful compilation or differing diagnostics. Infrastructure and
unsupported-feature failures never satisfy compiler-error expectations. Only
explicit `skip/expect/<major-minor-patch>` or CLI filters opt expectations out. The comparison normalizes
line endings, but preserves all other text, spaces, and final newlines.
Neither expectations nor the reference compiler supply actual compilation output.

Compiler-error snapshots use the official `errors-omitting-details` format below.
The runner consumes typed output/error outcomes; reference generation projects
classic reports through the existing `ElmMakeReport` parser. The actual compiler
returns nonempty collections of semantic problems through either execution
backend. Syntax and naming diagnostics retain structured locations; independent
files, declarations, and expression children accumulate problems before a
dependent phase is allowed to proceed. Missing manifests and entry-point files
have live error scenarios. Unsupported capabilities and internal/input-contract
failures are separate variants, never substitutes for source diagnostics.
This does not claim complete classic diagnostic-title or parser-recovery parity.

The compiler API contains no snapshot text renderer. `ElmMake.SourceLocation`
defines application/package file identities and project/file/source locations.
A source location always includes its file and an opaque validated region.
Regions retain one-based numeric start/end line and column fields (exclusive end);
zero-width parser failure positions remain valid. Consumers can sort or compute
range spans without parsing text. Concrete syntax is retained for validation,
then excluded from emission interfaces to keep declaration compilation cacheable.
The shared parser has structured-error entry points; its existing string-error
entry points remain compatibility wrappers, not the compiler's diagnostic path.
The parser optimization audits permit only its open-position `failure` helper's
`row`/`column` reads and the three string-error wrappers' escaped `message`
accessors. These exceptions are bound to original source declarations through
`SourceFunctionIds`, not generated artifact hashes. Other generic reads remain
failures, and successful language-service reference requests still require zero
generic record operations at runtime.

`ElmMakeCompiler` decodes the same structured model into .NET diagnostics,
unsupported capabilities, or internal failures. Only the testing adapters in
`Pine.Elm019.Testing` derive classic-style snapshot titles, logical path strings,
ordering, escaping, and the final text. `ElmMakeScenarioCompiler` adapts compiler
outcomes for the shared runner; production compilation does not call it.

Run small Elm units with:

```text
pine elm test implement/elm-in-elm/elm-make-ecmascript
```

These units cover shared import qualifiers, imported operators, cyclic thunk
references and initialization, eager dependency analysis, character escaping,
fresh-name allocation order, and tail calls with escaped parameter names.
`ElmAppCompilation_Run_all_snapshots` additionally checks complete application
lowering, including transitive package code and embedded frontend output, against
the unchanged classic snapshots.

The installed executable uses its own bundled core implementation. After editing
bundled core sources, build this checkout's Pine executable and use it to test
those changes; an older installed build may still exhibit the bugs being fixed.

The separate [implementation log](elm-make-ecmascript-implementation-log.md)
records discoveries and changes of approach.

### Expression and pattern coverage scenario

`scenarios/expressions-and-patterns` has one `Main.main` consuming every coverage
witness. GLSL is deliberately excluded: it will not be supported for the
foreseeable future. The scenario's development JS and HTML expectations
are produced by classic Elm 0.19.1; all expectations remain active, even when
the implementation under test does not yet support their syntax.

| Area | Live coverage |
| --- | --- |
| Literals | Unit, empty/escaped/multiline/Unicode strings, ordinary/escaped/non-BMP chars, decimal/hex integers, floats/exponents, arithmetic negation |
| Construction | Empty/populated/nested lists, pairs/triples/nested tuples, empty/nested records, record alias constructors, zero/one/two/three-field choice constructors |
| Application | Qualified/unqualified names, curried/partial/nine-argument calls, prefix operators, infix precedence, pipes/composition, lambdas with destructuring |
| Binding and control flow | Nested/forward/recursive let functions, let destructuring, nested conditionals, case expressions, expressions used as case scrutinees |
| Records | Field access/chained access/accessor functions, multiple-field updates, extensible-record update functions, function/lambda/let/case record patterns |
| Patterns | Wildcard/variable/unit/char/string/int, pair/triple/nested tuple, record, empty/singleton/fixed list, cons chains, qualified/unqualified/nested named patterns, nested `as` bindings, booleans, fallback branches |

`ElmMakeEcmascriptCoverageTests` checks all 20 non-GLSL abstract expression
variants, every valid abstract pattern variant, both let variants, and important subcases. It
also requires every app function/constructor to occur in **each** reference
output, preventing coverage from living only in unused source. A separately
named reference-reproducibility test confirms those snapshots still correspond
to the input application; it is not the actual compiler conformance test.

`FloatPattern` exists in the syntax model but is forbidden by Elm 0.19.1.
Negative integer patterns are also forbidden. The fixture therefore uses valid
integer patterns and covers negative integers/floats as expressions instead.
GLSL is an explicit coverage exclusion, not a skipped snapshot. The remaining
expectations stay active and expose implementation gaps normally.

### Browser program variants scenario

`scenarios/browser-programs` contains a separate entry-point module for every
program constructor exported by `elm/browser` 1.0.2:

| Entry point | Constructor | Behavior |
| --- | --- | --- |
| `src/Sandbox.elm` | `Browser.sandbox` | Counter with a pure update function |
| `src/Element.elm` | `Browser.element` | Counter with commands and subscriptions |
| `src/Document.elm` | `Browser.document` | Counter controlling the document title and body |
| `src/Application.elm` | `Browser.application` | URL-aware document handling internal/external link requests and URL changes |

Each entry point has an explicit `Program () model msg` signature and at least one
active successful-output expectation. The scenario includes development, debug,
and optimized output, but does not require every mode/target combination for each
entry point. Unit flags allow the generated HTML to initialize without
custom JavaScript flags. Every snapshot is generated by the isolated, offline,
pinned classic compiler; the required original Browser, Time, and Url package
archives and registry versions are checked in alongside existing dependencies.

The coverage audit checks each constructor, its Program signature, and retained
main/view/update declarations in each reference output.
A separate reference-reproducibility test replays every retained snapshot. Actual
compilation now reproduces them without skips, error-expectation substitutions,
or reference fallback. Unit tests cover command-only, subscription-only and
combined manager layouts, ordered graph dependencies, Program/flags aliases,
unsupported flags, and debugger exclusion. Additional live-reference variations
cover inferred sandbox mains, aliased Program signatures, and Platform workers.
Debug coverage includes the real debugger runtime and transitive message metadata,
including recursive unions, generic record aliases, lists, and tuples. The minimal
static Html scenario retains development HTML and debug/optimized HTML and JavaScript.

### Classic compiler optimization coverage

`scenarios/compiler-optimizations` has one `Main.main` consuming every witness
in `Optimizations.elm`. It contains no Debug calls, so the same source compiles
with classic Elm 0.19.1 in development and `--optimize` modes. Each mode has
JavaScript and HTML snapshots: four active expectations, none skipped. The audit
requires both modes, but does not require both output targets in each mode.
Development compilation itself performs many specializations; it is not an
unoptimized baseline.

The inventory comes from the pinned classic compiler's
[`Optimize.Expression`](https://github.com/elm/compiler/blob/0.19.1/compiler/src/Optimize/Expression.hs),
[`Generate.JavaScript.Expression`](https://github.com/elm/compiler/blob/0.19.1/compiler/src/Generate/JavaScript/Expression.hs),
[`Generate.JavaScript`](https://github.com/elm/compiler/blob/0.19.1/compiler/src/Generate/JavaScript.hs),
and [`Generate.Mode`](https://github.com/elm/compiler/blob/0.19.1/compiler/src/Generate/Mode.hs),
not assumptions about optimizations a compiler might perform.

| Specialization | Reachable witnesses and checks |
| --- | --- |
| Direct self tail calls | `tailIf`, `tailCase`, `tailLet`, `tailDestructure`, `tailBranches`; labelled loops and `continue`, no recursive call left |
| Tail argument reassignment | `tailSwap`; all temporary arguments are evaluated before parameters are overwritten |
| Destructured tail arguments | `tailRecord`, `tailArgumentPattern`; argument bindings occur inside the loop |
| Recursive local functions and tail calls under recursive lets | `localTail`, `tailLocalBinding`; both outer and inner loop labels retained |
| Tail-call eligibility boundaries | `nonTail`, `nonTailBinding`, `nonTailWrapped`, `nonTailLambda`, `mutualEven`, `mutualOdd`, `localMutual`, `tailPipeLeft`, `tailPipeRight` remain calls, not loops |
| Arithmetic | `numericInt`, `numericFloat`, `prefixArithmetic`; `+`, `-`, `*`, `/`, integer division via `\| 0`, and reversed `remainderBy` operands |
| Unary/numeric conversions | `numericFloat`; negation, `truncate` via `\| 0`, identity emission for `toFloat` |
| Boolean core calls | `booleanOps`; `!`, `&&`, `\|\|`, and `!==` for xor |
| Literal equality/inequality | `literalEquality`, `literalStrings`, `literalFloats`; zero and both booleans on either side, strict primitive comparisons, double negation only where classic emits it |
| Nonliteral equality/comparison | `genericEquality`, `genericComparisons`; structural `_Utils_eq`, `_Utils_cmp`, and their four comparison thresholds |
| Literal comparisons | `literalComparisons`, string/float witnesses; direct JS relational operators, including reversed literal positions |
| String/list append | `appendStrings`, `appendLeftNested`, `appendUnknownStrings`, `appendLists`; right-spine string-literal detection versus `_Utils_ap`, without flattening left-nested expressions |
| Core Tuple projections | `tupleAccess`; direct `.a`/`.b` reads |
| Every public Bitwise specialization | `bitwiseOps`; complement, and/or/xor, left/right/unsigned-right shifts, with shift-count argument reversal |
| Pipes | `pipeLeft`, `pipeRight`, `pipeBare`, `accessorPipe`; argument accumulation, direct calls, and accessor application |
| Deliberately unspecialized higher-order forms | `composeLeft`, `composeRight`, `partialCall`, `partialCoreCall`, `partialBasicsCall`; composition and partial application retain their runtime/currying behavior |
| Arity helper boundary | `arity2` through `arity9` use matching `F2`-`F9`/`A2`-`A9`; `arity10` uses nested unary functions/calls |
| Conditional/decision layouts | `conditionalExpression` vs `conditionalBlock`; `caseFanout`, `stringPattern`, `booleanPattern`, `decisionShared` exercise switches, primitive tests and shared labelled exits |
| Production unboxing | `Wrapped`, `unwrap`, `wrappedAsValue`, direct constructor use in `results`; boxed development representation versus identity values, eliminated calls and unboxed pattern paths |
| Production enums/constructor tags | `Color` constructors, `enumPattern`, `Normal` constructors and `normalPattern`; numeric enum values/tests, numeric normal tags, zero-tag simplification |
| Special Dict constructor tags | Public `dictionaryOps` retains core Dict constructors; optimized red-black nodes use negative tags |
| Production field shortening | `recordSpecializations`, `tailRecord`, accessor and record-pattern witnesses; constructions, updates, accessors and destructuring use the renamed fields |
| Production characters/unit | `characterPattern`, `characterEquality`, `literalRepresentations`, `unitPattern`; unboxed chars, no `valueOf`, strict char comparison, and unit `0` |
| Collection representations | `emptyList`, tuple render helpers and `results`; nil/from-array and pair/triple construction remain in the emitted graph |

**Dormant classic branches are not claimed as covered:** the classic
`generateJsArrayCall` checks a core module named `JsArray` (`Name.jsArray`), but
the authoritative core 1.0.5 archive defines the private module `Elm.JsArray`.
Consequently public `Array` operations cannot trigger its `singleton` JS-array
literal or `unsafeGet` index-expression specializations. `arrayOps` reaches both
functions via `Array.push`/`Array.get`; the audit asserts that their calls
remain in both reference modes. Do not alter the package archive or introduce a
fake core module merely to claim those branches fired.

The scope is the classic expression, core-call, tail-call, decision, arity and
production-representation specializations above. GLSL remains excluded; ports,
effect registration and debugger instrumentation are not expression
optimizations and are outside this scenario. The existing Browser corpus covers
main/effect initialization separately.

The audit checks the emitted body of each witness, not incidental kernel code
elsewhere in the output, and requires every app function/constructor in every
reference snapshot. It also checks negative controls to avoid falsely claiming
tail-call/composition optimizations. Reference reproducibility is tested
separately from actual compiler conformance.

The new development witnesses exposed missing Bitwise specializations and a
noncanonical `flag /= False` lowering. These are fixed in the focused `CoreCalls`
stage, shared by named/prefix/infix call lowering. Both development and production
outputs match the unchanged references. No automatic skip or reference-compiler
fallback is permitted.

## Goals

+ Emulate production of JavaScript files and HTML files as with the `make` command in v0.19.1 of the classic Elm compiler.
+ Avoid the failures to connect to <https://package.elm-lang.org/all-packages> observed frequently with said binaries (see [2026-09-13-elm-package-references.md](2026-09-13-elm-package-references.md)) Use the already existing implementation in the Pine repository to load the packages from GitHub directly and cache them locally.
+ Use Elm-based implementations for main parts of that `make` functionality (parse Elm syntax, canonicalize, etc.) to enable automatic caching running on PineVM.
+ Report syntax errors and naming errors.
+ Replace the current approach used in `ElmAppCompilation.AsCompletelyLoweredElmApp` that spawns a new process with the classic binary, with the new implementation. Because of Interpreter/PineVM boundary the handover using `MissingDependencyError` stays the same for now.
+ The functions implemented in Elm must constrain arguments for cache-friendlyness when running on PineVM and/or the Elm syntax interpreter.
+ Implementation of type information to the degree necessary to enable the compatible emission.

## Non Goals

+ Optimizations not in the classic core binaries, such as elm-optimize or save-virtual-dom. We might skip these in the future by switching to WASM directly.
+ No type mismatch errors emitted for now.

## Context

In earlier tests we observed [v0.19.1 of the classic Elm compiler](https://github.com/elm/compiler/releases/tag/0.19.1) produced the same JavaScript or HTML output on repeated invocations.

Testing v0.19.2 showed outputs were not stable, therefore we dont try that here.

## Approach

### Tests

These tests should be run automatically in the integration tests project, or if dependencies allow it, in `Pine.Core.Tests`:

+ Use snapshot testing design analogous to already implemented `ElmAppCompilation_Run_all_snapshots`. Since snapshots of JS and HTML are already precise enough no behavioral testing.
+ Model tests using whole Elm application file trees as input.
+ Store all scenario directories under `scenarios` inside the test data directory, separate from shared package and reference data.
+ Produce snapshots of `make` outputs using the classic binaries offline and store snapshots in Git repository.
+ Use directory name `input-app` for all Elm app files.
+ Use version-qualified directories under `expect` for all expectations, currently `expect/0-19-1`.
+ Use file path `expect/0-19-1/src/Main.elm.html` to model Elm 0.19.1 make HTML output for file `src/Main.elm`.
  + This way we can model expectations for multiple compilation entry points and also HTML and JavaScript separately.
  + File paths like `expect/0-19-1/src/Main.elm.debug.html` and `expect/0-19-1/src/Main.elm.optimize.html` select these flags.
  + Successful-output grammar, relative to `scenarios`: `<scenario>/[skip/]expect/<major-minor-patch>/<source-path>.elm[.debug|.optimize].(html|js)`
  + Compiler-error expectations use the omitting-details format specified below.
+ Use .NET test method as entry point enumerating all scenario directories and compiling report. Searches file path in `input-app` matching encoding in expect file name.
+ .NET Test wrapper parses all expectation files into a structured representation, including compiler version, source path, and typed options. Test fails if there are zero expect files in a scenario. If a test should be skipped, `expect/<major-minor-patch>` is placed inside `skip`, producing `skip/expect/<major-minor-patch>`.
+ .NET Test wrapper parses the relevent `elm.json` file and loads all packages needed. Select the governing manifest for each entry point; unrelated nested projects should not affect compilation.
+ .NET Test wrapper to Elm: The type of the Elm function invoked from the .NET side contains:
  + Representation of File tree. Perhaps use Elm String values for file contents because we dont need to support binary contents in this case.
  + Make File path.
  + Output type HTML/JS.
  + Flavor `development` / `debug` / `optimize`
  + Collection of Elm packages.
+ Reuse `implement\elm-in-elm\pine-elm-syntax\src\ElmSyntax\` for parsing.
+ Contain all ECMAScript specific implementations in a subdirectory in `elm-in-elm`.
+ Use Elm-based tests in `elm-in-elm` runnable with `pine elm test` for covering smaller units.
+ Elm to .NET boundary: Return `Result MakeError String`. `CompilerErrors CompilerProblem (List CompilerProblem)` guarantees at least one problem; each carries a typed semantic kind and a project/file/source location. Source locations pair a structured application/package file identity with a validated region. `UnsupportedFeature UnsupportedProblem` and `InternalFailure InternalProblem` are separate outcomes. No snapshot strings, formatting, ordering, or title derivation belongs in this boundary or the Elm implementation. Extensible for future type mismatch errors.
+ .NET Test wrapper runs all scenarios and reports:
  + Statistics over all tests so that its easy to see when a change in the implementation made multiple tests fail.
  + Statistics grouped by output mode (`development` / `debug` / `optimize` / `HTML` / `JS`)
  + Statistics grouped by target compiler version, independently of output mode and expected outcome.
  + Statistics grouped by expected outcome (`Output` / `ErrorsOmittingDetails`).
  + Which expectation files mismatched in a test scenario.
  + For mismatched snapshots, a rendering of the text-based difference that is easy to parse. It should use a hunk model, and include number of hunks differing. Number of differing hunks rendered should be configurable.
+ Failures from the outer snapshot comparison inform priorities for adding tests in `elm-in-elm`
+ Choice between PineVM and Elm syntax interpreter during execution depends on the learnings during implementation, and might be switched or combined during the project.

### Compiler-error expectations omitting details

Successful-output snapshot contents and comparison semantics remain unchanged;
their paths now include the compiler-version qualifier.

An expectation models both a compilation request and its expected outcome.
The request retains its compiler version, entry point, output target (JavaScript or HTML),
and flavor. The outcome is either successful output or compiler errors. Compiler
errors are not a third output target.

#### Expectation paths

Use the following path grammar, relative to `scenarios`:

```text
<scenario>/[skip/]expect/<major-minor-patch>/<source-path>.elm[.debug|.optimize].(html|js).errors-omitting-details.txt
```

For example:

```text
invalid-name/expect/0-19-1/src/Main.elm.js.errors-omitting-details.txt
invalid-name/expect/0-19-1/src/Main.elm.html.errors-omitting-details.txt
invalid-name/expect/0-19-1/src/Main.elm.debug.js.errors-omitting-details.txt
invalid-name/skip/expect/0-19-1/src/Main.elm.optimize.js.errors-omitting-details.txt
```

The source path names the compilation entry point, not necessarily the file
containing the errors. Errors in imported modules belong to the same expectation.
The output target and flavor select the same compiler options as a successful
expectation. The explicit `errors-omitting-details` suffix identifies the scope:
it does not model the original report's `message` at all. A future expectation
including messages must use a distinct suffix, without changing this format.
For a given request (including compiler version), successful-output and compiler-error expectations are
mutually exclusive; contradictory expectations are a corpus error.

#### Plain-text content

Store UTF-8 text without a BOM. Render one record per reported problem, with
these three labelled lines in this order:

```text
File: src/Helpers.elm
Region: 5:9-5:16
Title: NAMING ERROR

File: src/Main.elm
Region: 12:5-12:18
Title: TYPE MISMATCH
```

Separate records with exactly one blank line. End the file with one newline,
without an extra blank record. Generated files use LF; comparison may normalize
CRLF/CR/LF as for existing snapshots, but must preserve all other content.

+ `File` is the diagnostic's file, relative to the scenario's `input-app` root,
  using `/` separators. For nested applications, retain the nested directory
  prefix rather than making paths relative to their governing manifest.
+ Package files use logical paths such as
  `package:elm/core@1.0.5/src/Basics.elm`. Normalize only known application and
  package roots; do not retain temporary directory names or machine-specific
  cache paths. An unmappable reference path fails generation rather than being
  silently shortened to a basename.
+ `Region` is `start-line:start-column-end-line:end-column`. Preserve the classic
  compiler's one-based coordinates, with inclusive start and exclusive end,
  including multi-line regions. Do not convert them to zero-based coordinates
  or recalculate them from the rendered message.
+ `Title` is the exact problem title, without case folding or trimming.
+ For a project-level compiler error without a region, use `Region: <none>`.
  If it also has no file, use `File: <none>`. Do not invent a source location.
+ To keep fields single-line and unambiguous, escape a literal backslash as `\\`,
  carriage return as `\r`, newline as `\n`, and tab as `\t` in field values.
  The reserved absent-file marker `<none>` is not a literal file path.

Do not include explanatory messages, source excerpts, caret markers, diagrams,
styling, hints, links, or extended guides. Locations are shown by the explicit
file and region fields, not by reproducing the classic source-excerpt rendering.
Module display names are not part of this format.

#### Complete collection and deterministic ordering

Include every problem reported by the compilation request, across all reported
files; do not keep only the first problem or deduplicate identical records.
For a classic `compile-errors` report, flatten every file's `problems` collection.
For a classic project-level `error` report, create one record from its path and
title, with no region. The classic JSON may be used as reference-generation
input, but is neither the persisted snapshot nor the comparison format.

Sort records by the following tuple, using normalized, unescaped field values:

1. File path, using ordinal case-sensitive comparison.
2. Start line, then start column, numerically.
3. End line, then end column, numerically.
4. Title, using ordinal case-sensitive comparison as a tie-breaker.

Absent files sort before present files; within a file, absent regions sort before
present regions. Identical records retain their multiplicity. Apply the same
ordering to reference and actual diagnostics, independently of discovery order,
filesystem timestamps, or compiler scheduling.

"Every problem" means all diagnostics the compiler reports for that invocation,
not hypothetical errors hidden by an earlier compilation-phase failure.
This format does not expand the compiler's supported scope or introduce type
mismatch checking; unsupported active expectations remain failures unless
explicitly skipped.

#### Generation and comparison behavior

An error expectation passes only when compilation returns compiler diagnostics
and their complete canonical text matches the expectation. Successful
compilation fails an error expectation, and compiler diagnostics fail a
successful-output expectation. An empty error snapshot or empty problem
collection is invalid.

Timeouts, cancellation, process crashes, malformed reference reports, missing or
corrupt package snapshots, and interpreter/VM/internal failures are not expected
compiler diagnostics. They remain harness failures. In particular, accepting a
nonzero exit code or any exception is not sufficient to satisfy an error
expectation. An implementation-only unsupported-feature error cannot substitute
for a classic source diagnostic.

Generation uses the pinned classic Elm 0.19.1 compiler and the existing isolated,
offline reference setup. Validate that the failing process produced a recognized
compiler report before projecting it into this format. Generation must still
refuse to overwrite an existing expectation. Actual runs must produce their own
structured diagnostics, never read expectations or use classic compilation as a
fallback.

Preparation must permit compiler-input failures such as a missing entry point
or malformed manifest to reach the compiler when those are the intended
diagnostics. This must not turn missing/corrupt corpus package data into expected
compiler errors. Scenario discovery, generation, skip/flavor filters, shared
reporting, and both execution backends must recognize the new expectation kind;
reports must distinguish expected compiler errors from unexpected test failures.

### Checked-in Package Snapshots

+ Store package snapshots in a shared `packages` directory inside the test data directory, using the existing `ElmPackageSource` ZIP cache layout, for example `packages\github.com\elm\core@1.0.5.zip`.
+ Check in the complete package set required by the scenarios, including transitive dependencies and dependencies verified by the classic compiler. Archives contain the original package manifests and sources, including kernel JavaScript.
+ Both the .NET test wrapper and the reference snapshot generator load these same archives using the existing ZIP decoding implementation.
+ Test runs use only this package directory, with no fallback to user caches, GitHub, or the package registry. Missing or invalid snapshots fail explicitly with the package name, version, and expected archive path.
+ The tool for adding scenario snapshots may acquire missing packages before reference compilation, but must save them in this directory for inclusion in Git. Existing snapshots are authoritative and must not be refreshed implicitly.

### Reference Snapshot Generation

+ Invoke the classic Elm 0.19.1 binary with an explicit, temporary, writable `ELM_HOME`, independent of the user's cache. Extract the checked-in packages there using Elm's layout, for example `<ELM_HOME>\0.19.1\packages\elm\core\1.0.5\elm.json` and `src`.
+ Seed `<ELM_HOME>\0.19.1\packages\registry.dat` from a checked-in Elm 0.19.1-compatible registry snapshot containing the required package versions. Store that seed at `elm-reference\registry.dat` inside the test data directory; adding packages must ensure the seed covers their versions.
+ Run reference compilation without network access. `ELM_HOME` alone does not disable network requests: Elm attempts a registry update and falls back to the cached registry when it fails.
+ Start with a fresh copy of the scenario application without `elm-stuff`, and package sources without compiled `artifacts.dat`, so stale artifacts cannot substitute for the checked-in sources. Generated caches remain temporary and are not committed.
+ Compile the requested entry points, output types, and flavors for the qualified compiler version and write their outputs to the corresponding `expect/0-19-1` paths (or `skip/expect/0-19-1` for explicitly skipped cases). Other compiler versions currently fail before dependency acquisition or compilation; they must not be generated with the 0.19.1 binary. The invocation must preserve the configured `ELM_HOME`, rather than use the existing wrapper's hard-coded user cache. Clean up the temporary application and Elm home afterward.

### Maintenance CLI

The maintainer-only subcommand `elm  maintain  make-ecmascript` in the Pine CLI implements maintenance functions around these test scenarios and snapshots and is hidden from default CLI help:

+ Updating the package snapshots and registry file tracked in the repo to cover dependencies of all test scenarions. Reports unused packages remaining, but does not remove them. Loads only packages from GitHub currently missing.
+ Running selected scenarios and print their result to the console.
+ Displaying statistics of persisted registry and package snapshots, test scenarios and expectations, including expectation counts by compiler version.
+ Invoking the snapshot generation via classic Elm 0.19.1 binary for a particular expectation, using the full expectation path including scenario name and compiler-version qualifier as argument. Fails if the snapshot file already exists, must be removed manually. Implicitly acquires and saves package snapshots and updates package registry if required.
+ May offer options to filter expectations for output types (e.g. `--skip-expect-debug`, `--skip-expect-optimize`)
+ May offer option to explicitly select execution path, syntax interpreter or compiler and PineVM.
+ Code for running test scenarios and printing the results must be shared between CLI and the test runner.
+ Prints specific and helpful error messages. For example:
  + Attempting to create snapshot: on non existing scenario name, list existing scenarios and closest name.
