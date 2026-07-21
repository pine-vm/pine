# Migrating the .NET Elm compiler to the abstract syntax model

## Purpose

The .NET Elm compiler should use only the two syntax models whose semantics match its compiler-stage boundaries:

- `Pine.Core.Elm.ElmSyntax.SyntaxModel` (the concrete model) for parsing and canonicalization.
- `Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract` (the abstract model) for lowering, optimization, type inference, and Pine emission.

`Stil4mElmSyntax7` should cease to be an internal compiler representation. It remains useful for applications and interfaces that explicitly implement the stil4m/elm-syntax 7 format, but its historical use in the compiler is no longer an architectural reason to retain it there.

This document describes the current state, the target boundary, the expected costs and benefits, and a phased implementation plan.

## The three models today

### Concrete syntax model

`ElmSyntax.SyntaxModel` is the parser's native output. It retains ranges, token locations, separators, comments, incomplete declarations, redundant parentheses, and the source spelling of literals. These properties make it appropriate for parsing, formatting, source editing, and diagnostics.

### Abstract syntax model

`ElmSyntax.ElmSyntaxAbstract` deliberately omits source-only details. It removes ranges and trivia, unwraps redundant parentheses, normalizes literals, uses structural value equality, and precomputes selected `PineValue` encodings. These properties make equivalent source fragments equal regardless of formatting or source position, which is the desired identity for compilation and future caches.

The direct boundary already exists in `ElmSyntaxAbstract/ConvertFromConcrete.cs`. `ElmSyntaxAbstract/ConvertToConcrete.cs` supports rendering and tests, but necessarily creates placeholder ranges and cannot reconstruct the original source.

### Stil4mElmSyntax7

`ElmSyntax.Stil4mElmSyntax7` is another source-shaped model. It preserves more syntax detail than the abstract model but less than `SyntaxModel`. Converters in `Stil4mElmSyntax7/FromFullSyntaxModel.cs` and `ToFullSyntaxModel.cs` translate between it and `SyntaxModel`, inventing locations when converting back.

This model currently occupies most compiler stages because it predated the newer concrete and abstract models, not because its properties are a good fit for those stages.

## Current compiler flow

The principal path in `ElmCompiler.cs` is currently:

1. `ElmSyntaxParser.ParseModuleText` produces `SyntaxModel.File`.
2. `Stil4mElmSyntax7.FromFullSyntaxModel.Convert` converts each parsed file to `Stil4mElmSyntax7.File` (`ParseAndCanonicalizeForLowering`).
3. `Canonicalization.CanonicalizeAllowingErrors` accepts and returns `Stil4mElmSyntax7.File`.
4. Flattening, lambda lifting, specialization, inlining, cleanup passes, reachability analysis, module reconstruction, `CompilationPipelineStageResults`, and `DefaultLoweredResults` continue to carry `Stil4mElmSyntax7`.
5. Consumers that already require `ElmSyntaxAbstract` convert selected files, declarations, expressions, patterns, function structures, or type annotations on demand.
6. The Pine emission backend itself consumes abstract expressions through `CompilationContext`, `ExpressionCompiler`, `PatternCompiler`, `OperatorCompiler`, and `TypeInference`.

The on-demand bridge is `ElmCompilerInDotnet/ElmSyntaxAbstractConversion.cs`. Every bridge operation performs:

1. `Stil4mElmSyntax7` to `SyntaxModel` through `ToFullSyntaxModel`, creating synthetic locations.
2. `SyntaxModel` to `ElmSyntaxAbstract` through `ConvertFromConcrete`.

The compiler therefore crosses the desired concrete-to-abstract boundary late, piecemeal, and repeatedly.

## Current resource waste

### Whole-file conversion in the wrong direction

Immediately after parsing, `ElmCompiler.cs` converts the parser's full concrete file to `Stil4mElmSyntax7`. Later consumers convert portions of that tree back to `SyntaxModel` before converting them to abstract syntax. The route is therefore:

`SyntaxModel -> Stil4mElmSyntax7 -> SyntaxModel -> ElmSyntaxAbstract`

The middle two traversals do not add compiler-relevant information. The return to `SyntaxModel` cannot restore source details discarded by the first conversion; it merely allocates nodes containing synthetic ranges so that the existing direct abstract converter can consume them.

### Repeated subtree conversions

The same post-canonicalization syntax can be converted more than once:

- `ElmSyntaxOptimization.BuildFunctionSignatures` converts every declaration whenever it rebuilds the signature map. Optimization runs in bounded rounds, so unchanged declarations can be converted again in later rounds.
- `BuiltinOperatorLowering` converts declarations, function structures, expressions, patterns, and annotations at numerous type-inference decision points while its surrounding rewrite remains on `Stil4mElmSyntax7`.
- `ElmCompiler.EmitCompiledEnvironmentFromPipelineResults` converts every function declaration to build function type metadata, then separately converts every custom-type constructor argument annotation.
- `CompilationContext` retains a compatibility constructor that converts a complete function dictionary.
- `AddInferredTypeAnnotations` parses to `SyntaxModel`, converts files to `Stil4mElmSyntax7` for canonicalization, and then converts files and individual expression/pattern subtrees through `SyntaxModel` to abstract syntax for inference.

These conversions recursively allocate intermediate trees, repeatedly normalize the same literals, repeatedly construct precomputed values, and repeatedly calculate structures that would already be present if the later stages carried abstract nodes.

### Cache-hostile stage results

`CompilationPipelineStageResults`, `DefaultLoweredResults`, `OptimizationIterationStageResults`, and `OptimizedElmSyntaxDeclarations` expose or contain `Stil4mElmSyntax7` values. Even before a compiler cache is implemented, this makes the public stage boundary express the wrong identity. A future cache built around these values would either include source positions and syntax trivia indirectly or require another normalization layer.

With abstract declarations as keys, moving a declaration, changing comments, changing whitespace, adding redundant parentheses, or changing an equivalent literal spelling does not invalidate work. This is particularly valuable for the planned compiler implementation in Pine, where memoization over structurally equal immutable values can reuse work without a separate cache-specific syntax projection.

## Target compiler flow

The target path should be:

1. Parse source once to `SyntaxModel.File`.
2. Compute module dependencies and canonicalize using `SyntaxModel`.
3. Preserve the canonicalized concrete result for diagnostics and source mapping.
4. Convert each successfully canonicalized module once with `ElmSyntaxAbstract.ConvertFromConcrete.FromFile`.
5. Run every later stage on `ElmSyntaxAbstract`: flattening, lambda lifting, specialization, inlining, normalization, operator lowering, reachability analysis, module reconstruction, type inference, and Pine emission.

`CompilationPipelineStageResults` should make this boundary explicit:

- `Canonicalized` remains concrete because it is the result of the source-aware stage.
- `Lowered`, `ModulesForCompilation`, optimization snapshots, and the standard lowering payload use abstract declarations and files.
- Generic lowering delegates and declaration extraction functions use abstract declarations.

The transition should occur once after canonicalization rather than inside downstream operations.

## Conversions that remain after the migration

### Required production conversion

There should be one production direction in the compiler:

`canonicalized SyntaxModel -> ElmSyntaxAbstract`

It should occur at the canonicalization/lowering boundary. The concrete tree can remain alive alongside the abstract tree when diagnostics or source lookup are required, but downstream transformations should not convert it again.

### Source-facing tooling

Features that modify source, such as adding inferred type annotations, must still retain the original concrete file as their output template. They may:

1. Parse and canonicalize concrete syntax.
2. Convert the canonicalized file once to abstract syntax.
3. Infer information on the abstract tree.
4. Apply the result to the original concrete tree using structural paths.

This is not a compiler-stage round trip. The source tree and compiler tree serve distinct outputs and are correlated by paths.

### Rendering and tests

`ElmSyntaxAbstract.ConvertToConcrete` should remain available for snapshots, debugging, and formatting generated abstract syntax. Its zero ranges make it unsuitable for recovering source locations or reconstructing original formatting. Likewise, the `Stil4mElmSyntax7` converters remain for consumers of that external model, but not on the compiler's production path.

### Source locations from abstract paths

An abstract node can be identified by a graph path, for example:

- module declaration index;
- function body;
- application argument index;
- let declaration index or declared name;
- case branch index;
- tuple, list, or record item index.

Combining such a path with the retained concrete tree provides the corresponding `Node<T>.Range`. This preserves the cache-friendly abstract representation without attaching source positions to every compiler node.

The path scheme must account for concrete-only nodes that disappear during conversion, especially parentheses, trivia, and separated lists. The converter is the natural place to define path correspondence. Generated declarations and rewritten expressions have no unique original range; if diagnostics need one, transformations should carry optional provenance such as the path of the source node that caused generation. Provenance should remain side metadata rather than part of abstract node equality.

## Canonicalization on the concrete model

### Advantages

- Canonicalization is the first semantic stage and already produces diagnostics for unresolved references, ambiguous imports, naming clashes, and shadowing.
- Concrete ranges allow those diagnostics to point directly at both the new and shadowed bindings.
- Imports, exposing lists, aliases, operators, and binding sites can be resolved while their precise source representation is still available.
- Keeping the concrete tree through canonicalization gives the abstract converter a canonicalized input, so later stages receive resolved names without needing source metadata.
- Source tools can share the same canonicalization implementation and correlate results with editable source.
- It creates one clear and auditable loss-of-source-information boundary.

### Disadvantages

- Canonicalization must traverse the more verbose model, including wrappers and variants that have no semantic effect.
- Its rewrite must preserve ranges and concrete structure while replacing names, which is more implementation work than rewriting a minimal tree.
- Structural equality and caching of canonicalization itself remain sensitive to locations and formatting unless a separate cache key is used.
- Canonicalization and later abstract traversals cannot directly share all helper functions because their node types differ.
- Concrete syntax includes distinctions such as redundant parentheses that canonicalization must either preserve or deliberately look through.

### Assessment

The advantages outweigh the disadvantages for this boundary. Canonicalization is precisely where source-aware diagnostics and name resolution intersect. Moving it to abstract syntax would simplify its traversals, but would either weaken diagnostics or require source-path metadata before canonicalization has completed.

The proposed concrete canonicalization does not prevent caching. Parsing and canonicalization can use source-oriented cache keys, while the much larger set of repeated later transformations uses position-independent abstract values.

## A possible future typed model

### Why another model may be useful

The existing `TypeInference` already has a substantial `InferredType` hierarchy, including functions, constrained variables, tuples, choice types, lists, and open and closed records. `StructuralType` provides another downstream-oriented type representation. Today type information is mostly stored in dictionaries or recomputed at individual use sites rather than being a persistent compiler-stage result.

A typed representation becomes increasingly attractive if the compiler expands from the current inference needed for emission and selected optimizations to Elm-compatible complete type checking:

- Every expression, pattern, binding, and declaration needs a type, not only top-level function signatures.
- Type errors need relationships between several nodes and must be mapped back to precise source ranges.
- Exhaustiveness, constructor arity, record-row constraints, constrained type variables, ports, and annotation conformance need a stable checked-program input.
- Optimizations can consume proven types instead of invoking inference on selected subtrees.
- Inference should run once and its substitutions should not be recomputed after each consumer asks a similar question.

For limited additions to the existing inference helpers, a separate model is not necessary. For complete type checking and broadly type-directed lowering, the likelihood that a distinct typed intermediate representation becomes worthwhile is high.

### Placement

The typed stage belongs after:

1. concrete canonicalization;
2. the one-time conversion to canonical abstract syntax.

It should normally precede transformations that change binding structure, especially lambda lifting and specialization. This lets checking describe the user's canonical program and gives later passes reusable type facts.

It should live with compiler intermediate models, for example under `ElmCompilerInDotnet`, rather than in `ElmSyntax.SyntaxModel`. Types are semantic compiler results, not source syntax. It also should not alter the untyped `ElmSyntaxAbstract` records, because embedding inferred types there would:

- make general parsing and interpretation depend on a particular inference implementation;
- change structural equality and cache keys;
- complicate recursive type construction and substitutions;
- force every syntax transformation to manufacture or update annotations.

### Shape and implications

There are two principal designs:

- A typed tree whose node variants contain both syntax and resolved type information.
- An abstract tree plus immutable maps keyed by stable graph paths or compiler-assigned node identities.

A typed tree gives direct and statically visible access to types, but every rewrite must preserve or recompute them. Side maps keep the syntax model reusable and equality simple, but require disciplined identity/path handling. A practical design may use typed declarations and expressions for the checked input, plus explicit provenance paths back to canonical concrete syntax.

Introducing the model would create another intentional one-way boundary:

`canonical abstract syntax -> checked/typed compiler representation`

It would also require decisions about:

- whether transformed nodes retain proven types or trigger local/global rechecking;
- how generalized type schemes differ from instantiated expression types;
- whether `InferredType` and `StructuralType` converge or remain inference and emission representations;
- how graph paths survive generated declarations, inlining, specialization, and lambda lifting;
- serialization and equality if typed values become cache keys;
- exposing typed stage results without coupling source tools to lowering internals.

This model should be introduced only with a concrete consumer, such as complete type checking or removal of repeated inference from optimizations. The current migration should prepare for it by establishing a clean canonical-concrete to canonical-abstract boundary, but should not add a speculative typed tree.

## Implementation plan

### Phase 1: Establish behavior and model parity

1. Inventory every `Stil4mElmSyntax7` type in `ElmCompilerInDotnet` and classify it as canonicalization, lowering, emission, source tooling, or compatibility API.
2. Add parity tests that run representative modules through the current path and through direct `SyntaxModel -> ElmSyntaxAbstract` conversion, comparing canonical names, normalized literals, declaration order, and emitted Pine behavior.
3. Add focused tests for concrete constructs that disappear or change shape in abstract syntax: parentheses, operator applications, tuple and record syntax, let destructuring, patterns, comments, hexadecimal literals, and escaped literals.
4. Define the graph-path vocabulary used to correlate abstract nodes with canonical concrete nodes, including rules for skipped concrete-only nodes.

### Phase 2: Move canonicalization to `SyntaxModel`

1. Change `Canonicalization`, `CanonicalizationResult`, `NamingErrorDetection`, and related helpers to accept and return `SyntaxModel` nodes.
2. Preserve all existing range-bearing diagnostics and shadowing data.
3. Remove the parser-to-`Stil4mElmSyntax7` conversion from `ParseAndCanonicalizeForLowering`.
4. Update canonicalization tests first, then compiler integration tests, to use parser-native concrete files.
5. Keep compatibility adapters only at non-compiler interfaces that explicitly require `Stil4mElmSyntax7`.

### Phase 3: Introduce the single abstract boundary

1. Convert each canonicalized concrete file once with `ElmSyntaxAbstract.ConvertFromConcrete.FromFile`.
2. Change the generic lowering delegate inputs and declaration extractors to abstract declarations.
3. Change `CompilationPipelineStageResults` so only `Canonicalized` is concrete and all later syntax-bearing fields are abstract.
4. Change `DefaultLoweredResults`, `OptimizationIterationStageResults`, and `OptimizedElmSyntaxDeclarations` to abstract files and declarations.
5. Add tests proving formatting-only and location-only source changes produce equal abstract post-canonicalization values.

### Phase 4: Migrate lowering and optimization in dependency order

1. Migrate shared records and small traversal utilities, including qualified-name, declaration, pattern, and free-variable helpers.
2. Migrate flattening and module reconstruction.
3. Migrate lambda lifting and its validator.
4. Migrate specialization data structures and analyses.
5. Migrate `ElmSyntaxOptimization` and its partial files.
6. Migrate post-passes such as application normalization, case consolidation, wrapper cancellation, declaration deduplication, reachability analysis, and built-in operator lowering.
7. Replace synthetic `Node<T>` construction and zero-range constants with direct abstract nodes.
8. At each step, compare stage snapshots and runtime behavior rather than textual formatting, since abstract syntax intentionally loses formatting.

### Phase 5: Remove downstream bridges

1. Delete compiler calls to `ElmSyntaxAbstractConversion`.
2. Pass abstract declarations directly into `TypeInference`, `CompilationContext`, and the Pine emission backend.
3. Remove compatibility constructors that accept `Stil4mElmSyntax7` from compiler contexts once callers have migrated.
4. Update `AddInferredTypeAnnotations` to canonicalize concrete syntax, convert once per module, infer on abstract syntax, and apply annotations to the original concrete file by declaration path.
5. Verify that no production file under `ElmCompilerInDotnet` imports `Stil4mElmSyntax7`.

### Phase 6: Source mapping and diagnostics

1. Implement path-to-range lookup by combining abstract graph paths with canonical concrete files.
2. Add tests for paths through lists, tuples, records, let declarations, case branches, patterns, and nodes wrapped in redundant parentheses.
3. Define optional provenance for generated or rewritten nodes without adding it to abstract structural equality.
4. Route any later-stage diagnostics through this lookup instead of converting abstract syntax back to concrete syntax.

### Phase 7: Validation and cleanup

1. Run the full `Pine.Core.Tests` test project with `dotnet run`, following the repository's Microsoft Testing Platform convention.
2. Run relevant integration tests and compilation snapshot suites.
3. Format all changed C# with `dotnet format`.
4. Compare compiled output and runtime behavior for optimization enabled and disabled, multiple optimization rounds, source files with formatting-only differences, and source tools that add inferred annotations.
5. Remove compiler-only compatibility code after repository-wide searches confirm no remaining compiler references.
6. Update the compiler implementation guide and syntax-model guide to document the final boundary.

## Main risks and mitigations

- **Variant mismatch:** The models do not have identical wrappers and literal variants. Migrate exhaustive switches with focused parity tests and preserve the repository rule requiring explicit handling of every variant.
- **Loss of diagnostic locations:** Keep canonicalized concrete files and implement path-based lookup before moving any diagnostic-producing later stage.
- **Snapshot churn:** Compare abstract structures or behavior, not placeholder-range concrete rendering.
- **Transformation semantics:** Migrate passes in dependency order and keep stage-by-stage differential tests until the old path is removed.
- **Public API breakage:** Treat `CompilationPipelineStageResults` and generic lowering delegates as deliberate migration boundaries; add temporary adapters only where external callers require them.
- **Type-inference regressions:** Make type inference consume the already-abstract tree directly before removing old bridges, then verify record, constrained-variable, constructor, and annotation cases.
- **Generated-node provenance:** Keep provenance optional and external to abstract value equality so caching benefits are not lost.

## Expected end state

The compiler has one source-aware phase and one source-independent phase:

`source -> concrete parse -> concrete canonicalization -> abstract lowering/optimization/type inference/emission`

No production compiler path depends on `Stil4mElmSyntax7`. Source locations remain available through retained concrete syntax and graph paths. Later compiler values become stable under source movement and formatting changes, providing the correct foundation for structural caching and for a future compiler implementation in Pine.
