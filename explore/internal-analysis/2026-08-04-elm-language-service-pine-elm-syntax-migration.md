# Plan for migrating the Elm language service to `pine-elm-syntax`

## Goal

Migrate `implement/Pine.Core/Elm/elm-in-elm/src/LanguageService.elm` from the
legacy modules under `elm-syntax/src` to the concrete and abstract models under
`pine-elm-syntax/src`.

The final data flow should be:

1. Parse source text once into `ElmSyntax.Concrete.File.File`.
2. Convert that file once with `ElmSyntax.Abstract.ConvertFromConcrete.fromFile`.
3. Perform declaration, reference, import, exposure, scope, and name-resolution
   analysis only on `ElmSyntax.Abstract.*` values.
4. Represent source-facing results from that analysis with structural syntax
   paths, not ranges.
5. Resolve a path against the retained concrete file only when a request needs
   to inspect a cursor position, render source documentation, or return a source
   range.

The migration is complete when the transitive compilation closure rooted at
`LanguageService.elm` compiles without `elm-syntax/src`, even if unrelated
elm-in-elm compiler modules continue to use that legacy package.

## Current state

### Legacy syntax dependency

`LanguageService.elm` directly imports `Elm.Parser` and thirteen
`Elm.Syntax.*` modules. The legacy model appears in the service state and in
most analysis helpers:

- `ParsedModuleCache` and `ParsedCookedModuleCache` retain
  `Elm.Syntax.File.File`.
- Core and package module caches retain the same syntax type.
- `ModuleCompletionItems` eagerly retains declaration and scope ranges.
- `ImportedModule` retains parsed legacy files and import-reference ranges.
- `SyntaxNode` combines a semantic occurrence with an eager range.
- A definition is identified internally by file location plus declaration
  range.

The three parsing entry points are:

- `addFile`
- `handleRequestAddPackage`
- `updateLanguageServiceState`

All three call `Elm.Parser.parseToFile`, then eagerly derive completion items
containing ranges and source-rendered documentation.

### Analysis currently coupled to locations

The following work is semantic analysis but currently traverses range-bearing
legacy nodes:

- module, import, alias, and exposing resolution;
- top-level and local declaration discovery;
- reference discovery in exposing lists, type annotations, declarations,
  expressions, let blocks, and patterns;
- local-scope filtering;
- implicit-import and imported-item resolution;
- completion candidate construction;
- definition identity and cross-file reference matching.

Ranges also serve as semantic identities and scope boundaries. That prevents
the analysis result from being reused when source positions or formatting
change.

### Source-facing operations

The concrete tree or source text is genuinely needed for:

- mapping an incoming cursor location to a semantic occurrence;
- suppressing completion inside comments;
- returning definition, document-symbol, reference, and rename ranges;
- returning both signature and implementation name ranges for rename;
- distinguishing the module qualifier and unqualified name portions of a
  reference;
- extracting declaration source and documentation for completion and hover;
- retaining the current last-successful-parse behavior while the edited text is
  temporarily invalid.

These operations should remain source-aware, but they should not cause ranges,
comments, or source text to enter the abstract analysis model.

### `pine-elm-syntax` capabilities and gaps

The replacement package already provides:

- `ElmSyntax.Concrete.Parser.FromString.parseFile`;
- a concrete file model containing nodes, ranges, comments, documentation,
  token locations, and separated-list delimiters;
- a range-free abstract model for files, imports, exposing lists,
  declarations, expressions, patterns, and type annotations;
- `ElmSyntax.Abstract.ConvertFromConcrete.fromFile` and subtree conversion
  functions.

It does not yet provide a structural path vocabulary or a path-to-concrete-node
lookup API. That is the principal prerequisite for keeping analysis abstract
while still producing exact client ranges.

The converter also has correspondences that a source mapper must handle
explicitly:

- concrete expression and pattern parentheses disappear;
- decimal and hexadecimal literals are normalized;
- regular and multiline strings share one abstract variant;
- record-expression fields are sorted by name in the abstract model;
- separated concrete lists become ordinary abstract lists;
- documentation, comments, locations, and incomplete declarations disappear.

An abstract-to-concrete reconstruction with synthetic ranges would not recover
the original source and is not part of this plan.

## Target architecture

### Parsed module state

Each successfully parsed module should retain three separate concerns:

- source state: file URI, source text, and concrete syntax;
- semantic state: abstract syntax and analysis derived only from it;
- presentation state: no persistent range index; source ranges and rendered
  documentation are produced on demand.

The current `parsedFileLastSuccess` behavior should remain. A failed parse of
the latest edit may continue to use the previous successful concrete/abstract
pair, while completion-prefix handling can still inspect the latest text.

Core modules, workspace modules, and package modules should use the same parsed
module representation so that resolution and source mapping do not have
parallel implementations.

### Range-free semantic analysis

Replace the range-bearing contents of `ModuleCompletionItems` with a
range-free module analysis containing:

- module identity and imports;
- declared symbols and symbol kinds;
- exposure status;
- declaration and declaration-name paths;
- local lexical-scope paths;
- reference occurrences with qualified names and occurrence paths;
- semantic target identities used by definition, references, and rename;
- completion metadata that does not contain source excerpts or documentation
  comments.

A semantic target should be identified by module identity plus declaration
identity/path, not by a source range. The name already present in that target
can replace the current range-to-name lookup optimization in `findReferences`.

Local visibility should be determined from structural scope ancestry. If an
incoming cursor must be checked against a stale last-successful parse, the
scope path can be resolved to a concrete range for that request only.

### Structural path model

Add the reusable path model beside the syntax models in `pine-elm-syntax`.
Paths should cover:

- module name and module exposing entries;
- imports, imported module names, aliases, and imported exposing entries;
- top-level declarations and declaration-specific parts;
- function signature name, implementation name, arguments, type annotation,
  and body;
- choice-type names, constructors, constructor arguments, aliases, ports, and
  infix declarations;
- expression children, including application function/arguments, operators,
  condition branches, lambda arguments/body, case subject/branches, let
  declarations/body, tuples, lists, records, record updates, and accesses;
- pattern children and binding/name targets;
- type-annotation children and named type targets;
- whole-node, declaration-name, reference-name, and module-qualifier source
  selections where one semantic occurrence can require different ranges.

List positions may use indices where order is preserved. Paths for normalized
or reordered children, especially record setters, should use a stable key plus
an occurrence index so they remain unambiguous without assuming concrete and
abstract list indices are identical.

The concrete lookup layer should:

- traverse the original concrete file without converting the abstract tree
  back to concrete;
- skip concrete-only parentheses according to documented rules;
- return `Nothing` for a path/model mismatch rather than inventing a range;
- expose typed lookup functions for concrete nodes and a common source-range
  projection;
- derive token subranges only where the concrete model has no separately
  wrapped node;
- keep path values free of ranges and source text.

Cursor lookup should use the same vocabulary in the opposite direction:
select the most specific relevant concrete occurrence at a position and return
its structural path. This prevents the request layer from independently
reimplementing syntax traversal.

### Presentation boundary

Only request/presentation functions should combine analysis paths with concrete
syntax:

- completion resolves local scope paths and obtains documentation only for
  candidates being returned;
- hover maps the cursor to a reference/declaration path, resolves the semantic
  target, then renders documentation from the target module's concrete tree;
- definition maps only the resolved target path to a range;
- document symbols map only returned declaration paths;
- references map matching occurrence paths after semantic resolution;
- rename maps declaration-name paths and reference-name paths, not whole
  declarations or qualified references.

`DeclarationRange`, the service's internal `Range`, and Monaco-range conversion
can remain as short-lived response/presentation types. They must not be stored
in the semantic analysis cache.

## Existing automated tests

### Tests that directly exercise the bundled `LanguageService.elm`

`implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/ElmLanguageServiceTests.cs`
contains five active end-to-end tests:

- `References_request_finds_usage_across_modules`
- `References_request_finds_usage_across_modules_via_interpreter`
- `References_request_finds_usage_across_modules_via_interpreter_challenging`
- `References_request_finds_usage_across_modules_challenging`
- `Rename_request_renames_usage_across_modules_challenging`

They cover workspace updates, cross-module references, rename edits, exact
ranges, compiled execution, interpreter execution, and performance snapshots.

`implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/LanguageServiceReferencesEmptyResponseRegressionTests.cs`
contains nineteen active tests. The three feature-level checks are:

- `References_request_for_recursive_call_site_returns_non_empty`
- `ProvideDefinition_response_is_well_formed`
- `Probe_hoverItemsFromParsedModule_fromDeclarations`

The remaining checks preserve compiler/runtime behavior used by the language
service:

- `Probe_synthetic_triple_identity`
- `Probe_synthetic_triple_via_maybe_destructure`
- `Probe_synthetic_triple_via_listMapFind`
- `Probe_synthetic_via_assocListGet_with_completionItem_destructure`
- `Probe_no_assocListGet_no_completionItem`
- `Probe_assocListGet_no_completionItem_destructure`
- `Probe_no_assocListGet_with_completionItem_destructure`
- `Probe_letFn_direct_call_no_concatMap`
- `Probe_letFn_concatMap_no_closure`
- `Probe_topLevelFn_concatMap_with_args`
- `Probe_letFn_concatMap_with_closure_minimal`
- `Probe_single_letFn_with_internal_maybe`
- `Probe_two_letFns_inner_returns_plain`
- `Probe_two_letFns_inner_returns_just_pair`
- `Probe_two_letFns_inner_returns_just_pair_int`
- `Probe_two_letFns_inner_no_args`

`implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/PrecompiledLeaves/LanguageServicePrecompiledLeavesEffectivenessTests.cs`
contains three active tests. In particular,
`LanguageService_removeWrappingFromMultilineComment_leaf_short_circuits_work`
guards a precompiled leaf whose key is rebuilt by compiling the language
service source closure.

`implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/LanguageServiceCompileToPineVMRegressionTests.cs`
contains
`Dict_insert_with_record_update_on_LanguageServiceState_does_not_overflow`,
currently skipped with `Skip = "Temp speedup"`.

### Existing replacement-parser and model tests

`implement/Pine.Core/Elm/elm-in-elm/pine-elm-syntax/tests/ParseFromStringTests.elm`
tests:

- successful and failing expression parsing;
- every expression variant;
- nested patterns and expression boundaries;
- declaration-or-expression parsing and classification;
- mixed line endings and token positions;
- literal and comment tokenization;
- concrete file parsing with ranges;
- documentation attachment;
- complete-file conversion to the abstract model.

`implement/Pine.Core/Elm/elm-in-elm/pine-elm-syntax/tests/ConvertConcreteToAbstractTests.elm`
has retained tests for:

- literal normalization, parenthesis removal, multiline strings, sorted record
  setters, and all expression variants;
- all pattern and type-annotation variants;
- module kinds, explicit exposing entries, and import aliases;
- all declaration variants;
- file conversion dropping source-only comments and incomplete declarations.

`implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/ElmParserNewFileTests.cs`
contains seven active compiled-parser tests:

- `File_with_zero_imports_and_two_simple_declarations`
- `File_with_imports`
- `File_matches_language_service_scenario_ModuleA`
- `File_matches_language_service_scenario_ModuleB`
- `File_with_type_annotations`
- `File_with_function_with_parameter`
- `File_with_explicit_exposing_list`

The Module A and B cases already use the basic cross-module language-service
reference fixtures.

`implement/Pine.Core.Tests/Elm/ElmCompilerInDotnet/ApplicationTests/TokensFromStringConcreteTests.cs`
tests full-file tokenization, mixed line endings, operators, escaped and
multiline strings, nested comments, and literal classification with exact
locations.

### Tests that are useful as a fixture source but are not current migration guards

`implement/example-apps/elm-editor/tests/LanguageServiceTests.elm` contains
twenty Elm tests covering hover, completion, references, and one module
definition scenario. They import the separate copy at
`implement/example-apps/elm-editor/src/LanguageService.elm`, so passing them
does not prove that the bundled target file migrated correctly. Their scenarios
should be ported into the direct target suite rather than treated as sufficient
coverage.

`implement/Pine.IntegrationTests/ElmLanguageServiceTests.cs` contains
`Language_service_provides_hover` and
`Language_service_provides_completion_items`, but both are skipped pending the
new compiler path. They document expected behavior but are not an active gate.

### Important current coverage gaps

There is no active direct-target request suite covering all of:

- hover;
- completion;
- document symbols;
- local and imported definition ranges;
- comments and documentation;
- local lexical scopes;
- package-module definitions;
- all structural paths that lose wrappers or reorder children during abstract
  conversion.

These gaps must be closed before switching production request paths.

## Tests to add and keep

All tests introduced for this migration should be committed before the
production path they protect and retained after the legacy dependency is
removed. Temporary runtime comparisons against `Elm.Parser` should not become
permanent tests; expected semantic and protocol results should instead be
captured as stable fixtures.

### Direct request contract tests

Extend the direct bundled-language-service C# suite with active, exact-response
tests for every request:

- hover on a local declaration, function argument, local type, imported item,
  imported alias, module name, and documented declaration;
- completion at top level, inside a let scope, after a module qualifier, in an
  import statement, and inside each comment form;
- definition for local declarations, signature references, imports and aliases,
  exposed imports, constructors, and nested local bindings;
- document symbols for functions, aliases, choice types, and constructors;
- references and rename for signatures plus implementation names, exposing
  entries, qualified and exposed imports, type references, constructors,
  patterns, nested let declarations, and references inside parentheses;
- workspace add/update/delete and last-successful-parse behavior;
- package modules and implicit core imports where the existing interface
  already supports them.

Each range-bearing assertion should use deliberately shifted and reformatted
versions of a fixture so a correct semantic result with a stale or hard-coded
range cannot pass.

### Structural path and source lookup tests

Add retained `pine-elm-syntax` tests for:

- every file, declaration, expression, pattern, and type-annotation path edge;
- whole-node, name-token, and qualifier-token selections;
- nested redundant parentheses;
- applications with multiple arguments;
- let-declaration and case-branch indices;
- separated tuples, lists, records, constructors, and exposing lists;
- record setters whose abstract order differs from source order;
- duplicate keyed children and invalid/out-of-bounds paths;
- exact ranges with LF, CRLF, comments, and multiline constructs;
- cursor-to-path and path-to-range agreement.

These tests should verify the concrete node selected by a path, not only the
final numeric range.

### Abstract analysis tests

Add pure tests whose inputs and expected outputs contain no ranges:

- module/import/exposure summaries;
- top-level symbols and constructor symbols;
- references from type annotations, expressions, patterns, exposing lists, and
  record updates;
- declaration-name paths for signatures and implementations;
- lexical scope ancestry for function arguments, lambdas, cases, and nested
  let declarations;
- resolution through canonical imports, aliases, explicit exposing lists, and
  implicit imports;
- shadowed names and same-name declarations in disjoint scopes.

Add an invariant test showing that whitespace, comments, line shifts,
parentheses, and equivalent literal spellings produce the same abstract
analysis whenever they produce the same abstract file.

### Presentation and cache-boundary tests

Add tests showing that:

- equal abstract analyses can map to different correct source ranges in
  differently formatted concrete files;
- documentation changes do not change semantic analysis;
- no range or source-text field is present in the cached analysis type;
- a failed current parse retains the previous successful abstract/concrete pair
  without associating new source ranges with the old abstract tree;
- only final response assembly performs path-to-range projection.

### Dependency-closure test

Add an automated compilation test that builds the `LanguageService.elm`
transitive closure with:

- `src`;
- `pine-elm-syntax/src`;
- kernel and other required library modules;
- no files from `elm-syntax/src`.

The test should compile and initialize the service, then run at least one
range-bearing request. Keep this as the final regression guard against
reintroducing the old package.

## Incremental migration

### Increment 0: Lock down behavior

1. Add the direct request contract tests described above, starting with hover,
   completion, definition, and document symbols, which have the weakest active
   coverage.
2. Reuse scenarios from the example editor tests, but run them against the
   bundled target `LanguageService.elm`.
3. Add formatting-shift variants for every range-bearing request.
4. Record current successful response values as the oracle; do not make the
   retained test suite call the legacy parser directly.

Verification:

- all new contract tests pass on the current implementation;
- the five existing `ElmLanguageServiceTests` and nineteen reference-regression
  tests remain unchanged and pass;
- skipped tests remain explicitly reported rather than counted as coverage.

### Increment 1: Complete parser prerequisites

1. Make `pine-elm-syntax/src` available to the elm-in-elm project and language
   service test builders while retaining `elm-syntax/src` temporarily.
2. Run every direct contract fixture through
   `ElmSyntax.Concrete.Parser.FromString.parseFile`.
3. Add permanent concrete-parser snapshots for any fixture shape not already
   covered.
4. Fix only parser/model gaps demonstrated by those fixtures before changing
   the service.

Verification:

- the existing `ParseFromStringTests`,
  `ConvertConcreteToAbstractTests`, `ElmParserNewFileTests`, and
  `TokensFromStringConcreteTests` pass;
- every language-service fixture parses into both a concrete file and an
  abstract file.

### Increment 2: Add the structural path API

1. Define the range-free path and source-selection vocabulary in
   `pine-elm-syntax`.
2. Implement concrete-node lookup and path-to-range projection.
3. Implement cursor-to-path selection for semantic declarations and
   references.
4. Document normalization correspondence for parentheses, separated lists,
   normalized literals, and sorted record setters.

Verification:

- all new structural path tests pass;
- every semantic occurrence in the parser fixture corpus resolves to the
  expected concrete node;
- malformed paths return `Nothing`.

This increment changes no language-service request behavior.

### Increment 3: Introduce a dual parsed-module cache

1. Centralize the three parse call sites behind one module-parsing function.
2. Add the concrete file and its one-time abstract conversion to workspace,
   core, and package cache entries.
3. Retain the legacy file and legacy completion items temporarily so all
   production requests still use the old path.
4. Preserve current last-successful-parse semantics.

Verification:

- all request contract tests remain byte-for-byte equivalent;
- cache tests prove the concrete and abstract files come from the same
  successful parse;
- formatting-equivalent fixtures produce equal abstract files.

### Increment 4: Build abstract top-level analysis

1. Introduce a range-free analysis module for module identity, imports,
   exposing lists, top-level declarations, choice constructors, and completion
   metadata.
2. Store declaration and name paths instead of `DeclarationRange`.
3. Keep documentation and source excerpts out of this analysis.
4. Compute the new analysis beside the still-active legacy completion cache.

Verification:

- pure abstract-analysis tests cover every module, exposing, import, and
  declaration variant;
- expected symbols, kinds, names, and exposure flags match the request
  fixtures;
- changing source positions or comments leaves the analysis equal.

### Increment 5: Build abstract nested analysis and resolution

Split this increment into independently passing changes:

1. Type annotations and patterns: produce path-bearing type and constructor
   references plus binding declarations.
2. Expressions and let blocks: produce path-bearing value references,
   declaration-name paths, and structural scope ancestry.
3. Workspace resolution: resolve local, imported, aliased, explicitly exposed,
   implicit-core, workspace, and package symbols by semantic identity.
4. Reference search: compare semantic targets, retaining name-based filtering
   without recovering names from ranges.

Verification after each change:

- the corresponding pure analysis tests pass;
- no new analysis type contains a concrete node, range, comment, or source
  string;
- existing end-to-end request tests still pass through the legacy response
  path.

### Increment 6: Establish the presentation layer and migrate document symbols

1. Add presentation helpers that resolve declaration paths, name paths,
   qualifier paths, scopes, documentation, and source excerpts against a
   concrete file.
2. Make failure to resolve a path explicit and omit only that result rather
   than manufacturing a zero range.
3. Switch `TextDocumentSymbolRequest` to abstract symbols plus on-demand
   concrete lookup.
4. Remove document-symbol dependence on range-bearing cached completion items.

Verification:

- source-lookup and formatting-shift tests pass;
- document-symbol contract tests return the expected range and selection range;
- all other requests remain on their previous path and continue to pass.

### Increment 7: Migrate completion

1. Select candidates, imports, exposure, and local visibility from abstract
   analysis.
2. Resolve a local scope path against concrete syntax only for the current
   cursor request.
3. Continue checking comments in the current concrete parse.
4. Render declaration snippets and documentation from concrete syntax only for
   returned items.
5. Remove completion ranges and cooked documentation from the persistent
   semantic cache.

Verification:

- top-level, qualified, import, local-scope, comment, documentation, and stale
  parse completion contracts pass;
- formatting and comment-only edits do not rebuild or alter abstract analysis.

### Increment 8: Migrate hover and definition

1. Map the incoming cursor to a structural occurrence path.
2. Resolve that occurrence to a semantic declaration target using abstract
   analysis.
3. Render hover text from the target concrete file on demand.
4. Resolve only the final definition target to a source range.
5. Remove hover/definition use of legacy `SyntaxNode`, `ImportedModule`
   reference ranges, and range-based declaration identity.

Verification:

- local, imported, aliased, exposed, type, constructor, pattern, and module
  hover/definition contracts pass;
- redundant-parenthesis and formatting-shift cases return the correct ranges.

### Increment 9: Migrate references and rename

1. Search path-bearing abstract reference occurrences across cached modules.
2. Compare resolved semantic targets rather than file/range pairs.
3. Map only matching occurrence paths to concrete reference-name ranges.
4. Map signature and implementation declaration-name paths separately for
   rename.
5. Preserve grouping by workspace/package file location and the rule that
   package files are not edited.

Verification:

- all existing basic, challenging, interpreter, recursive-reference, and rename
  tests pass;
- added tests cover exposing entries, signatures, local scopes, types,
  constructors, patterns, nested lets, qualifiers, and parentheses;
- rename edits target only identifier names and never module qualifiers or
  whole declarations.

### Increment 10: Remove the legacy language-service path

1. Delete the temporary legacy parse and cache fields.
2. Remove all `Elm.Parser` and `Elm.Syntax.*` imports and all legacy traversal
   helpers from `LanguageService.elm` and any new language-service modules.
3. Remove range-bearing semantic cache types and conversions that no longer
   have a presentation-layer caller.
4. Change language-service compilation and test source builders from
   `elm-syntax/src` to `pine-elm-syntax/src`, including:
   - `implement/pine/Elm/LanguageServiceCompilation.cs`;
   - `LanguageServicePrecompiledLeaves.cs`;
   - the language-service builders in `ElmLanguageServiceTests.cs`;
   - `LanguageServiceReferencesEmptyResponseRegressionTests.cs`;
   - `LanguageServiceCompileToPineVMRegressionTests.cs`;
   - `LanguageServicePrecompiledLeavesEffectivenessTests.cs`.
5. Keep `elm-syntax/src` in broader elm-in-elm/compiler configurations where
   unrelated compiler roots still require it; do not expand this migration into
   an unrelated compiler rewrite.
6. Update comments and documentation that describe the old dependency.

Verification:

- the dependency-closure test compiles and runs the language service without
  `elm-syntax/src`;
- a repository search finds no legacy syntax import in the language-service
  implementation and no old source-tree path in language-service-specific
  builders;
- all retained parser, path, analysis, request, interpreter, compiled-runtime,
  and precompiled-leaf tests pass;
- the full relevant .NET test projects pass.

## Validation commands

The repository uses Microsoft Testing Platform. Run .NET tests with
`dotnet run` from the project directory, not with a filtered `dotnet test`.

During the increments, use method filters for the affected classes, for
example:

- `*ElmParserNewFileTests*`
- `*TokensFromStringConcreteTests*`
- `*ElmLanguageServiceTests*`
- `*LanguageServiceReferencesEmptyResponseRegressionTests*`
- `*LanguageServicePrecompiledLeavesEffectivenessTests*`

Run the complete `Pine.Core.Tests` project before each production request-path
switch and at final cleanup. Run `Pine.IntegrationTests` at the final boundary;
report its two currently skipped language-service tests separately.

Run the `pine-elm-syntax` Elm test suites with the repository's existing
Elm test runner whenever parser, conversion, or path modules change. Format all
changed C# with `dotnet format`.

## Risks and controls

- **Parser/model parity:** Gate every service fixture on the new parser before
  switching caches.
- **Incorrect path correspondence:** Exhaustively test concrete-only
  parentheses, sorted record setters, separated lists, and token subranges.
- **Range leakage into caching:** Keep source presentation types in a separate
  module and assert formatting-independent analysis equality.
- **Scope regressions:** Test nested and shadowed declarations structurally
  before migrating completion or resolution.
- **Documentation loss:** Treat comments and source excerpts as on-demand
  concrete presentation data, since the abstract converter intentionally drops
  them.
- **Stale parse mismatch:** Retain concrete and abstract files as one atomic
  last-successful pair.
- **Compiled/interpreter divergence:** Keep both existing execution paths and
  the compiler probe tests throughout the migration.
- **Performance snapshot churn:** Measure each switched request path; update
  snapshots only for understood changes, never to conceal a semantic
  regression.

## Final acceptance criteria

- `LanguageService.elm` and its transitive language-service-only modules import
  only `ElmSyntax.Abstract.*`, `ElmSyntax.Concrete.*`, and other non-legacy
  dependencies.
- Source parsing uses `ElmSyntax.Concrete.Parser.FromString.parseFile` and
  performs one concrete-to-abstract conversion per successful module parse.
- All declaration/reference/scope/import/exposure analysis consumes only the
  abstract model.
- Semantic caches contain paths and semantic identities, not source ranges,
  comments, or source text.
- Cursor interpretation and response range generation use the retained
  concrete model only on demand.
- The dedicated language-service build and all language-service-specific test
  builders omit `elm-syntax/src`.
- The language-service request/response interface remains compatible.
- All existing active tests and all migration tests added above pass, and every
  newly added test remains in the repository.

## Implementation progress log

### 2026-08-04: Existing checkpoint assessed

- Found that a prior checkpoint had already implemented the structural path
  vocabulary, concrete source lookup, range-free `LanguageServiceAnalysis`,
  migrated request handlers, and language-service-specific source builders.
- Verified by inspection that `LanguageService.elm` no longer imports
  `Elm.Parser` or `Elm.Syntax.*`, and that its persistent semantic cache stores
  abstract analysis and paths rather than source ranges.
- Surprise: The implementation had advanced directly through most production
  migration increments without first adding the direct hover, completion,
  definition, and document-symbol contracts required by Increment 0. The
  implementation plan is therefore adjusted to validate and repair the
  checkpoint before filling those retained coverage gaps, rather than repeating
  the already-completed dual-cache transition.
- Surprise: Running `elm-test-rs` for `pine-elm-syntax` is currently blocked
  because the sandbox cannot fetch `https://package.elm-lang.org/all-packages`.
  The existing .NET compiled-parser tests remain available for local validation;
  the Elm suite must be reported separately unless its package cache becomes
  available.
- Validation of the five existing direct language-service tests found no
  response regressions: both interpreter tests passed, and the three compiled
  tests reached their exact response assertions. The compiled tests then failed
  only because their performance snapshots still described the legacy syntax
  implementation.
- Updated those three snapshots to record the measured migrated implementation.
  This is an understood cost change rather than a semantic acceptance change:
  the new parser and path-based analysis increase invocation, loop, and
  instruction counts while reducing compiled-expression count and, in the
  challenging scenarios, list construction.

### 2026-08-04: Presentation and package-resolution defects repaired

- Added direct exact-response contracts for hover, completion, definition,
  document symbols, and references. The fixture deliberately crosses the
  workspace/package boundary and covers documentation, exposed imports, package
  file locations, declaration ranges, name-only selection ranges, and choice
  constructor children.
- Surprise: The checkpoint's completion path could list an item exposed from a
  package, but semantic resolution did not add explicitly exposed imports to the
  unqualified name-resolution context. Consequently hover and definition
  returned empty responses for the same item. Centralized filtering of
  declarations exposed by an import and reused it for semantic resolution.
- Surprise: Package files were retained in the cache and could be targets of
  workspace references, but package locations could not themselves be queried,
  and package modules were omitted when searching for references. Added a
  file-location lookup shared by workspace and package requests, propagated the
  real file location through reference resolution, and included package modules
  in reference searches without making package files editable by rename.
- Completed document-symbol presentation for choice types: constructors are now
  children instead of duplicate top-level symbols, and each symbol's
  `selectionRange` resolves its name path rather than repeating the whole
  declaration range.
- Plan adjustment: The package-boundary contract was promoted into the current
  validation increment because it exposed coupled resolution defects that the
  workspace-only reference fixtures could not detect. This remains within the
  original goal of preserving all language-service request behavior while
  moving semantic identity from ranges to paths.

### 2026-08-04: Final validation

- The direct migrated language-service suite passed all 6 tests, including the
  new multi-request package-boundary contract and both compiled/interpreter
  reference paths.
- The 19 reference regressions, 7 compiled concrete-file parser tests, 23
  concrete tokenizer/parser tests, and 3 precompiled-leaf tests all passed.
- The complete `Pine.Core.Tests` project passed: 4,543 total, 4,479 succeeded,
  64 skipped, and 0 failed.
- `Pine.IntegrationTests` completed 138 tests successfully and reported its 66
  existing skips, including the two explicitly skipped legacy language-service
  tests. Its single failure was unrelated to this migration:
  `demo-backend-state` could not fetch
  `https://package.elm-lang.org/all-packages` in the sandbox. This is the same
  network restriction that prevented `elm-test-rs` from resolving its test
  dependencies.
- Final repository searches found no `Elm.Parser` or `Elm.Syntax.*` imports in
  the migrated language-service modules and no `elm-syntax/src` merge in any
  language-service-specific source builder.
