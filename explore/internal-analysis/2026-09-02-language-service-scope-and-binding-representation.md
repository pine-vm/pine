# Language-service scope and binding representation

Date: 2026-09-02

## Question

Pine previously represented declaration visibility as:

```elm
type DeclarationScope
    = TopLevelScope
    | LocalScope Path
```

Pattern traversal initially assigns `TopLevelScope` even to local binders and
some callers replace that value later. Besides the misleading name, this permits
incorrect intermediate states.

Elm 0.19 rejects shadowing of non-imported value names. Does that make the
following smaller representation sufficient?

```elm
type DeclarationScope
    = ModuleLevelScope
    | LocalScope String
```

Here the `String` is the name of the module-level declaration containing the
local binding.

## Conclusion

**No, not as the source of truth for lexical resolution.** The enclosing
module-level declaration name is a useful coarse partition, but it does not
identify the lexical region where a binding is visible.

Elm's no-shadowing rule says that two non-imported bindings with the same name
cannot be simultaneously visible. It does not say that a name can be bound only
once within a module-level declaration:

- separate case branches can use the same pattern name;
- separate lambdas or disjoint `let` expressions can use the same local name;
- a local can shadow an imported name, and the imported name remains the target
  outside that local region.

Those cases produce different targets with the same declaration name and the
same proposed `LocalScope "enclosingDeclaration"`. The current deferred
resolver could not distinguish them.

The proposed type is sufficient only for a narrower question such as “is this
binding module-level?” or “which module-level declaration contains it?” In that
role it should be named `DeclarationPlacement`, not `DeclarationScope`.

The selected implementation is Design B below: a raw `scopePath : Path`, with
the empty path as the unique module-level value. This is sufficient for the
current resolution architecture and smaller at runtime than a wrapper, though
it does not prevent path-role mistakes through typing. A more substantial
alternative is to resolve local references to typed binding IDs during
analysis; then exact lexical scope does not need to survive in each declaration
occurrence, and a coarse module-declaration owner can be stored separately if
useful.

## Implemented representation

`DeclarationOccurrence` now stores:

```elm
scopePath : Path
```

All module-level declarations and imported unqualified bindings use `[]`.
Localizing a declaration directly replaces `scopePath` with its structural
visibility root. Module-level filtering tests for `[]`, and reference
resolution uniformly calls `ElmSyntax.Path.isPrefixOf scopePath reference.path`
without a custom-type branch.

This implements only the representation change. It intentionally retains
existing collection and resolution behavior, including post-hoc localization,
legacy `let` widening, missing lambda/case/destructuring binders, and
first-match resolution. Those are separate semantic improvements described
below.

## What Elm's no-shadowing rule actually guarantees

The official Elm compiler's
[`Canonicalize.Environment.addLocals`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Canonicalize/Environment.hs#L146-L177)
handles four
[`Var` states](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Canonicalize/Environment.hs#L96-L100):

```haskell
data Var
  = Local A.Region
  | TopLevel A.Region
  | Foreign ModuleName.Canonical Can.Annotation
  | Foreigns ModuleName.Canonical (OneOrMore.OneOrMore ModuleName.Canonical)
```

Adding a local over `Local` or `TopLevel` reports `Shadowing`; adding one over
`Foreign` or `Foreigns` succeeds and replaces the imported binding with the
local one. Therefore:

- a function argument cannot reuse the name of another argument, an outer
  local, or a declaration in its module;
- a nested `let`, lambda, or case pattern cannot reuse an outer local name;
- a local **can** shadow an explicitly or implicitly imported unqualified name.

The compiler's official
[`hints/shadowing.md`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/hints/shadowing.md)
also says that the shown same-name outer/pattern example “does not even
compile,” and explains that Elm deliberately makes the non-shadowing practice
mandatory.

### Non-overlapping reuse is different from shadowing

The prohibition applies when the old and new bindings coexist in the
environment. It does not prohibit equal names in independent environments.

For a case expression, the official compiler traverses every branch with the
same parent environment, and
[`canonicalizeCaseBranch`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Canonicalize/Expression.hs#L193-L207)
adds that branch's pattern bindings only to the environment used for that
branch expression. Thus this shape is valid:

```elm
fromChoice choice =
    case choice of
        First value ->
            value

        Second value ->
            value
```

The two `value` binders do not shadow one another. They are never visible in the
same branch. Both would nevertheless be represented as:

```text
name  = "value"
scope = LocalScope "fromChoice"
```

The same ambiguity occurs for independent lambdas:

```elm
both lists =
    ( List.map (\item -> item + 1) lists
    , List.map (\item -> item - 1) lists
    )
```

and for separate `let` expressions in different expression branches. A
module-level owner partitions locals across top-level declarations, but not
within one declaration.

### Imported names make exact extent necessary even without duplicates

Because `addLocals` permits a local to replace `Foreign` or `Foreigns`, the same
spelling can refer to a local in one subexpression and an import elsewhere in
the same top-level declaration:

```elm
import List exposing (map)

example values =
    ( List.map (\map -> map + 1) values
    , map identity values
    )
```

Inside the first lambda, `map` is the parameter. In the second tuple element,
the unqualified `map` is the imported function. `LocalScope "example"` would
make the parameter appear visible across both elements unless some other data
retained the lambda boundary.

Ambiguous imports are also separate from lexical scope. The official compiler
uses `Specific` and `Ambiguous` import information in
[`Canonicalize.Environment`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Canonicalize/Environment.hs#L54-L75).
A local may replace either, but leaving its lexical region must reveal the
original imported candidate or ambiguity again.

## Why the language server cannot assume only valid modules

Even if `LocalScope String` were sufficient for compiler-accepted programs, a
language server operates while users are typing. Pine builds
`LanguageServiceAnalysis.ModuleAnalysis` from successfully parsed abstract
syntax; it does not first run Elm's canonicalizer or reject every semantically
invalid duplicate/shadowing state
(`LanguageServiceAnalysis.elm:90-109`).

Hover, references, rename, and CodeLens should remain deterministic in a module
that parses but temporarily contains:

- a local shadowing another local or module declaration;
- duplicate module-level declaration names;
- an incomplete rename where old and new spellings coexist;
- unresolved or ambiguous imports.

A scope model should therefore encode the syntax's actual lexical regions and
let diagnostics decide whether the program is legal. Treating no-shadowing as
an invariant of all analyzed editor states would couple navigation correctness
to validation that the language service does not currently perform.

## Information that should remain separate

Four concepts are related but should not be collapsed:

1. **Syntax identity:** `declarationPath` and `namePaths` locate syntax nodes in
   one file.
2. **Lexical visibility:** the structural region in which an unqualified local
   binding is available.
3. **Module-level owner:** the module declaration containing a local occurrence.
   This can be useful for grouping or early rejection, but is coarser than
   lexical visibility.
4. **Import provenance and source origin:** how a foreign binding became visible
   and where its declaration is stored.

Pine already combines `FileLocation` and a declaration path for resolved target
identity (`LanguageService.elm:112-131`).
`FileLocation = WorkspaceFileLocation ... | ElmPackageFileLocation ...`
preserves workspace versus package name/version/path
(`LanguageServiceInterface.elm:145-155`). Package origin should not be encoded
in `DeclarationScope`: a package module has the same internal lexical rules as
a workspace module.

Explicit import aliases and exposing clauses are represented by
`ImportOccurrence`; resolved imports retain `canonicalName`, `importedName`,
`exposingList`, `moduleNamePaths`, `fileLocation`, and the parsed source module
(`LanguageServiceAnalysis.elm:82-87`;
`LanguageService.elm:134-141,2477-2597`). Implicit `elm/core` imports have no
source import path. This provenance belongs to the importing module's resolution
context, not to the foreign declaration's intrinsic scope.

## Can a structural path store the needed scope?

Yes, within one module. `ElmSyntax.Path.Path` is `List Step` rooted at the file,
and `isPrefixOf` already expresses structural containment
(`ElmSyntax/Path.elm:39-42,116-122`):

```text
scope path []      = module-level visibility
scope path [ ... ] = visibility below that syntax node
```

The empty path is a prefix of every reference path, so one prefix operation can
handle module and local visibility uniformly. Testing whether a scope is
module-level is an O(1) empty-list match.

The **declaration path itself** is not the scope path:

- a function argument is declared under `StepArgument` but is visible in the
  function implementation body;
- mutually recursive declarations have separate declaration paths but share
  their enclosing `let` scope;
- a case pattern is declared in the branch pattern and is visible only in that
  branch body.

Deriving scope later from declaration paths would duplicate AST binding rules,
and generic `StepChild` values do not by themselves identify a lexical
construct. The analyzer should supply the visibility root while traversing the
construct that introduces it.

Paths are intentionally file-relative and should not encode file URI, package
identity, import alias, or exposing mode.

This describes what the path model can represent, not what today's analysis
already represents precisely. Current `let` processing deliberately widens
nested declaration scopes to the enclosing `let` path, and lambda and
case-pattern binders are not yet collected.

## Detailed evaluation of `ModuleLevelScope | LocalScope String`

### What it represents well

- The constructors clearly distinguish module-level from local declarations.
- For a valid module, a module-level value name identifies the top-level
  declaration containing each expression and local binder.
- Two functions can both call an argument `value` without their locals becoming
  confused across functions.
- CodeLens eligibility becomes a direct constructor match.
- A string comparison may reject references in unrelated top-level declarations
  before any structural containment work.

### Information it loses

- Which case branch, lambda, or `let` region introduces the binding.
- Whether a same-spelled occurrence elsewhere in the top-level declaration is
  outside the binding and should resolve to an import or remain unresolved.
- Which of multiple same-named binders in disjoint scopes is the target.
- Scope relationships in semantically invalid but parseable editor states.
- An intrinsic connection to the containing declaration's structural identity.

### Impossible and ambiguous states admitted by the type

- `LocalScope ""`.
- `LocalScope "missing"` when no such module declaration exists.
- A stale or mistyped owner name.
- Two module declarations with the same name in an invalid editor state.
- A `LocalScope` owner that names a type/port rather than an expression-bearing
  value declaration.

Wrapping `String` in an opaque `ModuleDeclarationName` removes empty or
accidentally interchanged strings, but cannot prove that the name exists or is
the actual structural parent. Using a module-declaration index or path gives
stable structural identity, but still does not provide the nested visibility
extent.

### Runtime characteristics

The representation is compact only if the owner string is already shared or
interned. Logically it repeats a potentially long name on every local
declaration. Equality costs scale with name length, although most comparisons
will be short.

A structural scope path repeats a list prefix and prefix checks scale with
lexical depth. Elm modules normally have shallow lexical nesting, but Pine's
current `ElmSyntax.Path.isPrefixOf` implementation performs `List.length`,
`List.take`, and equality rather than a single simultaneous traversal
(`ElmSyntax/Path.elm:116-122`).

An integer `ModuleDeclarationId` is cheaper than either for coarse ownership.
An integer `ScopeId` with a scope table can also make exact ancestry cheap.
These gains require additional analysis structures and should be driven by
profiles rather than assumed.

Most importantly, a fast representation of insufficient information is not a
valid optimization. With the current deferred resolver, `LocalScope String`
would require an additional lexical path/ID or repeated AST inspection, losing
its apparent simplicity.

## Design alternatives

### Design A: Rename the existing union

```elm
type DeclarationScope
    = ModuleLevelScope
    | LocalScope Path
```

**Pros**

- Smallest migration and better terminology.
- Retains exact structural visibility and current prefix checks.

**Cons**

- `LocalScope []` duplicates module scope.
- Pattern helpers can still create module placeholders and rewrite them later.
- A local path can be unrelated to its declaration path.
- Imported bindings are still given fabricated declaration scopes in the
  current resolution context (`LanguageService.elm:1185-1215`).

This improves naming but not the core type design.

### Design B: Store a raw scope path

Replace the union with `scopePath : Path`, where `[]` is module-level.

**Status: implemented.**

**Pros**

- One representation for module scope.
- One containment operation for module and local scopes.
- Minimal runtime data; CodeLens uses an empty-list check.

**Cons**

- Because `Path` is a type alias, declaration, name, reference, and scope paths
  are interchangeable to Elm's type checker.
- The meaning of `[]` is only a convention.
- Any nonempty path can be passed, including one that is not a lexical root.

The information is sufficient and minimizes runtime structure, but the API does
not use types as strongly as Design C.

### Design C: Opaque structural `ScopePath`

Use a custom type in a narrowly exported module. Internally it wraps a `Path`.
Expose operations such as:

- `moduleLevel`;
- `local : Step -> List Step -> ScopePath`;
- `contains : ScopePath -> Path -> Bool`;
- `isModuleLevel : ScopePath -> Bool`;
- optionally `depth`.

The local constructor requires at least one step, so module scope has exactly
one representation.

**Pros**

- Compact and compatible with the current flattened occurrence model.
- Prevents accidental exchange of syntax paths and visibility roots.
- Makes `LocalScope []` impossible.
- Centralizes representation and containment behavior.

**Cons**

- A wrapper adds one custom-type node around the path in the Pine value.
- It cannot prove that a supplied path points to an AST node that introduces a
  scope; traversal logic and tests enforce that.
- It requires narrowing exports; `LanguageServiceAnalysis` currently exposes
  everything.

This remains a possible type-safety follow-up, but was not selected for the
current migration.

### Design D: Coarse owner plus exact local scope

Keep the proposed optimization without discarding exact scope:

```elm
type DeclarationScope
    = ModuleLevelScope
    | LocalScope
        { moduleDeclaration : ModuleDeclarationId
        , visibilityRoot : ScopePath
        }
```

`ModuleDeclarationId` should be structural, such as the top-level declaration
index, rather than its renameable `String` name.

**Pros**

- Quickly rejects references in other top-level declarations.
- Retains exact semantics for branches, lambdas, `let`, and imported-name
  fallback.
- Avoids repeated owner strings.

**Cons**

- The owner is derivable from the first declaration step of `visibilityRoot`,
  so it duplicates information.
- The extra comparison helps only if profiles show path containment across many
  unrelated declarations is significant.
- Two fields can become inconsistent unless an opaque constructor derives both
  from one path.

This is useful only as a measured performance optimization.

### Design E: Split module and local declaration occurrence types

Represent module declarations and local declarations as separate variants or
records. Put `isExposed` only on module declarations and `ScopePath` only on
locals.

**Pros**

- A local declaration cannot be exposed.
- CodeLens and import/export APIs can accept only module declarations.
- Module/local impossible states are removed by construction.

**Cons**

- Common occurrence fields need nesting, accessors, or duplication.
- Existing consumers require more variant handling.
- More Pine value structure and a larger migration.

This offers strong API types but may be disproportionate for current needs.

### Design F: Scope IDs and a module-local scope tree

Build a scope table during analysis. Declarations and references carry opaque
`ScopeId` values; each scope records a parent or preorder interval. Syntax paths
remain separately for source lookup.

**Pros**

- Small IDs and O(1) interval ancestry tests are possible.
- Same-scope grouping and most-specific resolution are explicit.
- Repeated full scope paths need not be stored on each declaration.

**Cons**

- Largest redesign: references, declarations, module analysis, and resolution
  all change.
- IDs require their owning module and cannot replace `FileLocation`.
- Imports still need separate treatment at the importing module's root.
- Paths remain necessary for ranges, navigation, rename, and CodeLens.

This may be best at very large scale, but should follow profiling.

### Design G: Resolve local binding identity during analysis

Elm's non-shadowing semantics can simplify the model more effectively if used
during traversal rather than compressed into `LocalScope String`.

Maintain a lexical environment while walking one module. Assign each declared
local an opaque `BindingId`. Emit references as:

```text
ReferenceTarget
    = KnownLocal BindingId
    | UnresolvedName
        { qualifier : Maybe ModuleName
        , name : String
        }
```

Local references are associated with their declaration immediately. Imported,
module-level, missing, and ambiguous names remain unresolved for the
cross-module resolution phase.

**Pros**

- Later reference searches compare binding identities instead of repeatedly
  resolving path containment.
- Disjoint same-named locals are unambiguous.
- The analyzer's environment naturally restores imported names on exit from a
  lambda, branch, or `let`.
- `DeclarationPlacement = ModuleLevel | WithinModuleDeclaration
  ModuleDeclarationId` may then be sufficient for grouping and CodeLens
  filtering because it is no longer pretending to encode exact scope.

**Cons**

- Analysis becomes stateful and must mirror Elm's binding semantics precisely.
- Local recursive and mutually recursive `let` groups require predeclaration.
- Invalid shadowing needs deterministic recovery and diagnostics rather than
  relying on valid-program assumptions.
- Module-level and imported targets still require a second resolution phase.
- Binding IDs must be stable enough for the lifetime of a cached module
  analysis.

This is the strongest design if local-resolution performance justifies a larger
change. It does not make the proposed `DeclarationScope` sufficient; it makes
exact declaration scope unnecessary for already resolved local references.

### Design H: Relative declaration paths

Store an opaque pair of a scope path and a nonempty declaration path relative to
that scope.

**Pros**

- Makes “scope is an ancestor of declaration” true by construction.
- Retains unique empty-path module scope.

**Cons**

- Name paths and source lookup still need absolute paths.
- Constructing absolute paths adds work or duplicated cached data.
- Declaration location and visibility remain distinct for arguments and
  mutually visible `let` declarations.

This improves one invariant but fits the existing source-lookup API poorly.

## Imports and packages: visible bindings are not declaration scopes

`DeclarationOccurrence` should describe a declaration in its source module.
Resolution in another module should use a separate type:

```text
UnqualifiedBinding
    = OwnBinding ScopePath ResolvedDeclaration
    | ExplicitImportBinding ImportOccurrence ResolvedDeclaration
    | ImplicitImportBinding ResolvedDeclaration
```

Imported bindings are module-visible in the importing module, but that does not
change the source declaration's intrinsic scope. Qualified modules remain in a
separate namespace keyed by their imported name/alias, as in the current
`importedModules` context (`LanguageService.elm:1221-1229,1305-1314`).

The explicit-import variant retains syntax paths, alias, and exposing data. The
implicit variant correctly has no import syntax path. Both targets retain
`FileLocation`, which distinguishes workspace files and exact package
name/version/module paths.

CodeLens candidates come only from the current parsed module's own module-level
declarations. They never come from visible imported bindings, so importing a
workspace or package declaration cannot manufacture a lens in the importing
file.

## Resolution ordering and indexing

The current `ModuleResolutionContext` orders own module declarations, exposed
imports, implicit imports, and then locals. `Common.assocListGet` picks the first
same-named item, after which `resolveReferenceInContext` tests that one item's
scope (`LanguageService.elm:1105-1128,1283-1303`; `Common.elm:68-79`).

This is incorrect independently of which scope type is selected: a local cannot
win over a same-named import, and an out-of-scope local candidate can hide a
later valid candidate.

With deferred resolution:

1. Gather every same-named own binding whose `ScopePath` contains the reference.
2. Select the deepest scope.
3. If no own binding applies, resolve exposed and implicit imports according to
   Elm's ambiguity rules.
4. Resolve qualified references in the imported-module namespace.

A `Dict String (List UnqualifiedBinding)` avoids scanning unrelated names.
Choosing the most specific binding still examines same-named candidates, unlike
today's incorrect first-match behavior. Typical candidate counts and lexical
depth are small; measure before adding scope trees or intervals.

## Recommendation and migration

Design B is now implemented. Follow-up semantic corrections can build on the raw
path without changing its representation:

1. Pass the exact scope path into every declaration-producing traversal. Do not
   create an empty-path placeholder and localize it later.
2. Give `declarationOccurrencesForFunction` two explicit concepts: the enclosing
   scope of the function name and the implementation scope of its arguments.
   Top-level function names use `[]`; `let` function names use the enclosing
   `let` path.
3. Collect currently missing lambda, case-pattern, and `let`-destructuring
   bindings. A case binding uses its individual `StepCaseBranch` root so it
   cannot leak into sibling branches.
4. Replace fabricated imported scope paths with typed own, explicit import, and
   implicit import bindings.
5. Resolve the most specific applicable lexical binding before imports.
6. Use `scopePath == []` plus declaration kind for CodeLens filtering.
7. Add tests for disjoint same-named case/lambda bindings, local-over-import
   behavior inside and outside the local region, invalid shadowing recovery,
   explicit and implicit imports, and workspace/package targets.

If profiling later shows that repeated local resolution dominates, consider
Design G and resolve locals to binding IDs during module analysis. Use
`DeclarationPlacement` with a structural `ModuleDeclarationId` for grouping;
do not call `LocalScope String` a scope once exact resolution has moved
elsewhere.

`LocalScope String` by itself should not replace `LocalScope Path`.
