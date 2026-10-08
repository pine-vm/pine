# 2026-10-07 Frontend Compiler Design

## Background

+ First Elm compiler was designed very much for incremental compilation, and it powered the Elm Interactive.
+ Second Elm compiler was added in 2026, optimizing more for batch use, and not supporting incremental compilation.
+ First Elm compiler was lacking some optimizations enabled by whole-program analysis, therefore its output was not as efficient at runtime.
+ First Elm compiler was deactivated in 2026 when we changed encodings of Pine language and Elm values (Records, Choices).

## Goals

Support extending and revising programs, answering source-level queries, and compiling demanded entry points, while reusing unchanged work and retaining optimizations whose analysis crosses emission-unit boundaries.

Any implementation of the Elm compiler must be compatible with the constraints of the Elm programming language, so that porting it over is simple.

In the future, to enable complete type inference, we might insert a representation after the abstract Elm syntax model (typed AST). The new design must not make that harder.

## Agreed Implementation Contract

These decisions specify the frontend contract. Implementation status and remaining application integration are distinguished below.

### Scope and Compiler Inputs

Modules are not compilation units. They supply import directives, namespaces, exposing rules, and source provenance for name resolution. Canonicalization resolves references to declaration identities; module boundaries are then dissolved for dependency analysis, optimization, and emission. Retaining source metadata for diagnostics or grouping output for inspection does not make a module a compilation unit.

The compiler must not introduce a "frontend snapshot" or "program revision" concept, corresponding API fields, or result identities. Operations use explicitly supplied source and resolution inputs. Editor document tracking and Interactive submission history, redeclarations, rewinding, and branching belong to callers.

The initial implementation retains type checking only as far as it works today and must preserve existing regression tests. Complete Elm-compatible type checking is a future project, not a prerequisite for this change. Application analysis must not advertise complete type checking or interpret absence of reported type errors as proof of type correctness.

No new compiler abstraction is prescribed by this contract. The behavior can be expressed using declarations, source scopes, resolved references, diagnostics, and existing syntax models. The concrete-to-abstract boundary remains; a future typed representation can follow canonical abstract syntax before transformations such as lambda lifting and specialization.

### Inputs and Results

Compilation, application analysis, and source queries share name-resolution and semantic-analysis logic, but demand different work.

| Operation | Inputs and demanded work | Result |
|---|---|---|
| Compile declarations | Explicit declaration roots and their semantic dependencies. | Executable exports for the requested roots only. Dependencies and generated helpers remain internal. |
| Analyze application | Application source declarations, without selecting entry points. | Source-located diagnostics, including errors in unused declarations and disconnected source modules. |
| Query source | A source position, declaration, or search scope and the information needed to answer the query. No compilation roots. | Reliable requested information, such as definitions or inferred types, with relevant diagnostics. Explain when an answer cannot be established. |

Callers supply all compilation roots explicitly, including host-facing bridge functions. The compiler discovers dependencies and generates implementation helpers, but neither is an implicit addition to the root set. There are no module/file roots in the compiler; callers may enumerate declarations from a file if useful.

A missing requested root fails the entire compilation request rather than producing a partial executable result. An explicitly empty root set succeeds with no compiled declarations and no declaration analysis; it does not mean application-wide analysis.

Compilation does not evaluate roots to materialize their values. Callers execute compiled code separately, including constructing test values. This does not prohibit ordinary compile-time optimization.

Application analysis returns diagnostics; callers obtain definitions and inferred types through source queries. Source errors must not stop analysis of independent declarations. Unrelated package declarations need not be diagnosed, but package information required to analyze application code cannot be treated as valid when unavailable.

Diagnostics explain declaration demand, not module reachability: identify the failing declaration and each root-to-failure declaration reference, including same-source-file hops, exact reference ranges, and referenced constructors or types whose owning declaration has another name. Imports explain visibility but are not demand edges. Reused canonicalization results retain this source-reference information. Lookup failures distinguish an unavailable module, a missing API, a private member, and an incorrect qualifier or namespace. Editor displays retain related source locations without introducing compilation roots into rootless application analysis.

A source query may return a reliable definition location despite a type error in the declaration. Failure to establish an answer must not be presented as a completed lookup that found nothing. Likewise, successful root compilation does not certify the whole application, and application-wide errors must not block an unrelated root compilation.

### Declaration Demand and Name Resolution

The demand boundary is a top-level declaration. Canonicalization processes the complete body of a demanded declaration, including unused local `let` bindings. For example, a demanded `root = let unused = misspelledName in 42` reports the naming error. Pruning local bindings or control-flow branches before canonicalization is not part of this change.

The semantic closure includes references in bodies, signatures, type aliases, constructor definitions and patterns, record-alias constructors, infix declarations, ports where relevant, and implicit compiler facilities. Recursive declarations must be handled together where their analysis requires it. Function-call reachability alone is insufficient.

Consulting names, exports, or scope information does not demand every body in that scope. An unrelated top-level naming error or unused misspelled import must not block compilation.

For example, with `import Good exposing (answer)` and `import Missing exposing (..)`, a demanded `root = answer` can resolve to the available `Good.answer`. Hypothetical exports of the missing module are not ambiguity candidates. If two available, visible imports expose the demanded name, ordinary Elm ambiguity rules still apply. If a demanded reference requires an unavailable declaration, report the resolution error.

Qualified references must respect imports and the declaring module's exposing list. The implementation review explicitly chose enforcing those restrictions even when existing sources need correction. Type and value namespaces remain distinct: a type and a constructor belonging to another type can legitimately share a spelling.

Application-wide analysis reports the missing import even when root compilation ignores it. It also checks application imports and exposing rules that no compilation root happens to demand.

Demand-driven preparation must preserve package ownership, direct-dependency visibility, private exports, and authoritative substitutions. Do not search upstream behind a substitution or invent executable implementations. If platform declarations are absent from the supplied sources, analyzing code that references them reports the absence; this change does not require adding separate type-only platform sources.

These naming-error guarantees do not add a requirement for new syntax-error recovery. Source parsing must still establish the declarations and scopes needed for the requested operation.

### Reusing Work and Retaining Optimizations

Reuse depends on the actual inputs consulted, not compiler-level history or revision identities. Track dependencies on scope and exports as well as bodies, including failed name lookups and ambiguity candidates. Adding a previously missing name or changing an exposing list must not leave stale results.

Keep source locations accurate when reusing semantic work after formatting or location changes. Reuse across compilation, application analysis, and queries must not broaden a request's error relevance.

Declaration-level reuse does not require independent optimization or emission of each declaration. Retain cross-declaration specialization, inlining, representation analysis, and recursive-function handling. Optimization reuse must account for relevant callers, roots, and settings, not just the source text of a callee.

Callers manage Interactive history and supply the bindings appropriate to each submission. Indexing a seed application must not eagerly compile every declaration, and referencing another seed declaration must allow unchanged dependency work to be reused.

### Acceptance Examples and Implementation Sequence

| Example | Required outcome |
|---|---|
| Run the compiler project's unchanged `elm test` command below. | Pure-function tests do not demand the unrelated worker wrappers or their platform implementations. |
| Put an erroneous unused declaration beside a demanded pure function. | Compilation succeeds; application analysis reports the unused declaration's error. |
| Move the same erroneous reference into a demanded declaration. | Compilation fails with the relevant source-located error. |
| Include an unused missing wildcard import alongside a resolvable demanded name. | Compilation uses available declarations; application analysis reports the missing import. |
| Introduce two visible imports exposing the demanded name. | Report ambiguity; do not choose by traversal order. |
| Demand a function whose signature or constructor pattern needs another declaration. | Include the semantic dependency even if there is no function call to it. |
| Demand a function containing an erroneous unused local binding. | Report the error in the complete demanded body. |
| Request a missing root or an empty root set. | Respect the failure and empty-success contracts above. |
| Select tests and host bridge functions. | Export only those explicit roots; callers evaluate test values separately. |
| Analyze disconnected application files containing independent errors. | Report errors across the application with today's type-checking coverage, without emitting or executing code. |
| Ask for a definition in code with an unrelated type error. | Return the reliable location and relevant diagnostics. |
| Add a root, correct a missing name, change exports, or branch Interactive history. | Reuse unchanged work without stale bindings or diagnostics; results agree with fresh processing of the same inputs. |

Implementation must connect dependency preparation, declaration-demanded canonicalization, existing semantic analysis, and emission, then migrate compilation callers and language-service consumers. Do not implement the contract by canonicalizing all bodies and discarding unrelated errors afterward.

Use representative seed applications and small edits to measure work reuse and response times. Keep compiler models and transformations compatible with Elm; host I/O, document coordination, and concurrency must not become requirements of the compiler core.

### Implementation Status

The .NET frontend now has declaration-demanded canonicalization, explicit-root lowering and emission, caller-managed root evaluation, and caller-owned parsing/canonicalization memoization. `PrepareForDeclarationDemandAsync` preserves available sources and import diagnostics instead of requiring the module-import closure; supplied rewritten syntax retains original source ranges.

`AnalyzeApplication` returns rootless diagnostics with the existing partial inference machinery. `QueryDeclaration` returns source declaration information and available type information using the same demanded resolver. The host language server uses compiler-backed project diagnostics with unsaved sources, application-wide replacement of results, and refreshes on document and workspace changes. Existing language-service queries remain available; the new declaration query API does not replace every legacy query implementation.

These API names describe operations and result data, not new program identities. A cache entry retains resolved dependency names so reusing a body can still demand its dependencies. The language-server application-diagnostics interface identifies a project for scheduling and replacing diagnostics; without it, results triggered by different files could retain obsolete errors. Neither adds a compiler-level snapshot or revision.

Lazy seed compilation in the legacy Elm Interactive remains a separate application-integration boundary: its submission evaluator consumes runtime bindings rather than source scopes. The new compiler supports additional explicit roots and reusable declaration work, but the legacy caller still enumerates seed declarations when no roots are selected. This limitation does not relax the Interactive goals or authorize a new compiler history abstraction.

## Applications

This section lists applications with different demands and different preferences with regards to optimization, to inform the design of frontend compilers.

### Elm Interactive

In the interactive exploration, users repeatedly submit new program snippets which can be expressions or new declarations to integrate.

The interactive needs to:

+ Support loading a whole Elm application project as seed for a new session, potentially with many Elm modules.
+ Show types for each submission, both on expressions and new declarations: Run type inference.
+ When the submission is a declaration, integrate it so that future submissions can use it.
+ Show the values resulting from submitted expressions.
+ The readonly aspects listed above should be shown and updated not only on explicit submission, but whenever the user edits the text. (For a live example see Chrome DevTools Console view)
+ The Elm Interactive must support time travel, rewinding and branching. Redoing the work of all submissions from scratch when the user branches off an earlier instant would not scale acceptably.
+ With each new submission, we may reference any declaration from any of the source modules given with the seed. However we dont want to delay the first response with a compilation for thousands of compilation roots and hundreds of modules. At the same time, we dont want to redo all work compiling common modules when the latest submission references a new declaration from the application seed.

Another challenge in the Elm Interactive is the support for redeclarations. In contrast to a standard application build, we need to consider submission order when resolving a name that was redeclared.

### Core Development Loop

In the core development loop, we frequently edit a part of a complete application and then want some feedback on that new revisions.

In contrast to the Elm Interactive use case, such edits can change existing declarations, affecting dependent code across many other declarations and modules.

The feedback we want after editing can come in different forms. Sometimes via running tests or even the whole application.

In large projects, compiling everything from scratch would be too slow in this case.

But the challenge for response times does not stop there. We also want fast feedback when typing in an editor. A type mismatch should be shown soon after typing, without having to explicitly start compilation or saving a file.

Anders Hejlsberg explained it well in this [Video on Modern Compiler Construction](https://www.youtube.com/watch?v=wSdV1M7n4gQ)

Anders Hejlsberg Explains Modern Compiler Construction: <https://www.infoq.com/news/2016/05/anders-hejlsberg-compiler/>

### Running Tests Against Application Modules

Tests often import a module containing both pure functions and an application entry point. Running those tests must not require a runtime implementation of the entry point when no test depends on it.

The concrete regression to cover is running this command against the compiler's own Elm project:

```powershell
Set-Location K:\Source\Repos\elm-time\implement\Pine.Core\Elm\elm-in-elm
& "K:\Source\Repos\elm-time\implement\pine\bin\Release\net10.0\pine.exe" elm test
```

On 2026-10-07 this failed during dependency preparation with:

```text
Import 'Platform' in 'src/Main.elm' has no implementation in the resolved environment.
Import path: tests/CompileElmAppTests.elm -> src/Main.elm -> Platform.
```

[CompileElmAppTests.elm](../../implement/Pine.Core/Elm/elm-in-elm/tests/CompileElmAppTests.elm) calls the pure functions [Main.jsonEncodeDependencyKey and Main.jsonDecodeDependencyKey](../../implement/Pine.Core/Elm/elm-in-elm/src/Main.elm). The imported module also declares an unrelated `main : Program Int () String` using `Platform.worker`, `Cmd.none`, and `Sub.none`. That entry point exists to communicate roots to the classic Elm compiler, not to start a worker when Pine runs these tests. [ElmInteractiveMain.elm](../../implement/Pine.Core/Elm/elm-in-elm/src/ElmInteractiveMain.elm) has a similar wrapper.

The project already declares `elm/core@1.0.5` as a direct dependency in [elm.json](../../implement/Pine.Core/Elm/elm-in-elm/elm.json). The bundled core replacement in [ElmPackageSubstitutions.cs](../../implement/Pine.Core/Elm/ElmPackageSubstitutions.cs) does not include `Platform`, `Platform.Cmd`, or `Platform.Sub`. [ElmResolvedBuildPreparation.ValidateImports](../../implement/Pine.Core/Elm/ElmResolvedBuildPreparation.cs) requires source implementations for the transitive module-import closure and therefore rejects the project before declaration-level reachability can help.

There is a second boundary to address: [ElmCompiler.cs](../../implement/Pine.Core/Elm/ElmCompilerInDotnet/ElmCompiler.cs) canonicalizes module bodies before filtering declarations. Merely postponing the resolver error would still leave unresolved platform references in the unused entry point. Also, its `rootDeclarationsAsPlainValues` argument controls value materialization; it does not select compilation roots. A complete fix must connect dependency preparation, canonicalization, declaration demand, and emission.

This is not a request to add another direct dependency, upgrade a package, delete the worker wrapper, or search upstream behind a substitution. The manifest and worker wrappers must remain unchanged. Enforcing exposing restrictions revealed existing qualified accesses to private `ParserFast` constructors; the subsequent implementation decision permits correcting those source exposing lists rather than weakening name resolution.

### PR Gating Checks and Build Pipeline

In many classic projects, we did not notice a minute spent on compilation in the build pipeline, since automated tests after that took more time anyway. However, the general speedup we get from automated distribution and fine-grained caching shifts that balance. If the design of the frontend compiler causes poor cache hit rates even on small changes, we risk the compilation becoming the new bottleneck.
