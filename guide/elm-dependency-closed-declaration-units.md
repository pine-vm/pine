# Elm Dependency-Closed Declaration Units

## Purpose and terminology

This specification defines package-scoped Elm name resolution and reusable
declaration processing. It separates the meaning of a declaration in a
particular source context from the reusable result of processing its syntax.

A **source scope** belongs to an application or one selected package. It
contains that owner's modules and the modules visible through its direct
package dependencies. Each selected package has its own scope.

A **source binding** identifies a declaration in one such scope. The text of a
qualified name, such as `Namespace.Module.decl`, is not sufficient to identify
that binding: the same text can resolve to different declarations in different
scopes. A constructor reference identifies both its owning type declaration and
the particular constructor.

An **entry point** is a source declaration explicitly requested by the caller.
An **invocation** is one request to process a chosen set of entry points.
A declaration's **direct dependencies** are the source declarations referenced
anywhere in its syntax, after resolving those references in their respective
scopes. This includes references in signatures, type aliases, constructor
arguments and patterns, operator definitions, function bodies, and unused local
declarations. Locally bound names do not create dependencies on other source
declarations.

The **required declaration set** is the smallest set containing every requested
entry point and every direct dependency of every declaration already in the
set. It is determined by syntax, not by which expressions execute. Processing
repeatedly follows direct dependencies until no new declaration is found.

**Canonicalization** transforms declaration syntax so references have explicit,
unambiguous meanings rather than depending on unresolved module names.
A **semantic artifact** is an immutable result of such processing that can be
shared between source bindings. **Provenance** is the information describing
where a particular use came from, including files, source positions, package
origins, and the path from the caller's entry point.

A **nominal type** is a type whose identity depends on its source binding, not
merely its shape. Two choice types with identical definitions in different
source bindings remain distinct types.

## Required behavior

1. References must be resolved in the scope of the module containing them.
   Different packages may contain different definitions under the same module
   name without colliding when those definitions are used in separate scopes.
2. Modules that a dependency does not expose must not collide with modules in
   another scope or become visible merely because their code is required
   elsewhere. Genuine ambiguity within one visible scope must remain an error.
3. Renaming a package, publishing an unchanged fork, or changing a version label
   must not change the meaning of otherwise unchanged scope relationships or
   prevent equivalent artifacts from converging to one retained representation.
   Changed sources or visibility must still affect the result.
4. Unknown references outside the required declaration set must not fail
   compilation, whether they occur in application modules or package modules.
   Unknown references inside that set must be reported even when execution
   would not reach them.
5. Compilation with initially empty caches must traverse declarations and
   dependencies efficiently, without repeatedly expanding dependency bodies.
6. Every potentially expensive transformation that commonly recurs after small
   edits must have an independently reusable input and result. Incidental
   package names, versions, and provenance must not enter semantic cache inputs.
7. Package references must admit module-name mappings, such as a prefix applied
   to imported module names, so otherwise ambiguous dependencies can be used
   together. Elm import aliases must remain usable with these mappings.
8. The design must be expressible using immutable values and pure functions,
    without requiring side effects or hidden global state.
9. Artifact sharing must preserve visibility rules and must not automatically
    unify distinct source bindings or nominal types.

## Required runtime efficiency

+ A retained declaration artifact must be shared when referenced through several packages. A dependent declaration must not retain another copy of its dependencies' bodies. Where the implementation language is Elm, the Pine runtime takes care of reusing values and deduplication. In an implementation without such a runtime system, this sharing must be implemented explicitly with a cache. In a language with side-effects, that cache can be a mutable interface offering methods for cache entry lookup and insertion.
+ Canonicalization work must not be repeated for the same declaration input during one compilation, including when several dependency paths reach it.

## Scope resolution and module mappings

Package selection, source lookup, imports, exposure rules, and mappings are
resolved before invoking reusable semantic transformations. Every lookup of a
syntax-level reference must retain access to the scope containing that
reference. Dependencies reached in another package use that package's scope,
not the scope of the application requesting the entry point.

Only an owner's own modules and exposed modules of its direct dependencies
enter its visible module scope. Declaration-level exposure and import aliases
further determine which names a module can use. Indirect dependencies do not
become directly importable. Candidate selection must not use package spelling
or enumeration order to choose arbitrarily between ambiguous visible names.

A **package-reference edge** connects an importing application or package to
one of its direct dependencies. A module-name mapping belongs to that edge.
A prefix mapping changes the module names visible through that edge; it does
not rename the dependency's own modules within its scope or alter other edges.
The prefix must be a valid Elm module-name prefix, and the mapped edge must
exist. Conflicting mappings for one edge must be rejected.

For example, two directly imported packages exposing `Utilities` can be made
visible as `First.Utilities` and `Second.Utilities`. Import aliases can then
give either module a local spelling independent of those prefixes. Unchanged
declaration syntax with equivalent resolved bindings must remain reusable when
a prefix or package label changes.

## Dependency-closed units

The required declaration set forms a directed graph: declarations are vertices,
and direct references are edges. A **strongly connected component (SCC)** is a
maximal nonempty group of vertices in which every vertex can reach every other
by following edges. A single nonrecursive declaration also forms an SCC.
Grouping the graph into SCCs leaves an acyclic graph between components.

### References Within SCC

The Elm language allows recursion only between declarations in the same module.
Therefore references to members of the same SCC use unqualified names as identifiers.
Moving an SCC as a whole between modules or packages does not change these references in the contained syntax.

### SCC Data Model

On the semantic level, SCCs do not have separate identifiers, but embed their dependencies as values:

Type declarations reference only other types. They therefore cannot form an
SCC with value declarations. The model distinguishes value SCCs from type
SCCs and restricts their member kinds accordingly. Type SCCs have no value
dependency collection.

```Elm
import Dict
import ElmSyntax.Abstract.Declaration
import ElmSyntax.Abstract.Expression
import ElmSyntax.Abstract.Infix


type SccModel
    = ValueScc ValueSccModel
    | TypeScc TypeSccModel


type alias ValueSccModel =
    { members : Dict.Dict String ValueDeclaration
    , dependencies : Dict.Dict (List String, String) SccModel
    , typeDependencies : Dict.Dict (List String, String) TypeSccModel
    }


type TypeSccModel
    = TypeSccModel
        { members : Dict.Dict String TypeDeclaration
        , typeDependencies : Dict.Dict (List String, String) TypeSccModel
        }


type ValueDeclaration
    = FunctionDeclaration ElmSyntax.Abstract.Expression.FunctionStruct
    | PortDeclaration ElmSyntax.Abstract.Expression.Signature
    | InfixDeclaration ElmSyntax.Abstract.Infix.Infix


type TypeDeclaration
    = ChoiceTypeDeclaration ElmSyntax.Abstract.Declaration.ChoiceTypeStruct
    | AliasDeclaration ElmSyntax.Abstract.Declaration.TypeAlias
```

+ Entries in `dependencies` and `typeDependencies` can originate from different packages. Package provenance is not modelled in the SCC. Each key is `(moduleName, symbolName)`, identifying the referenced symbol rather than necessarily naming a member of the associated SCC. Keys are local to the containing SCC and their collection.
+ A value SCC's `dependencies` can target another value SCC through `ValueScc`, or a type SCC through `TypeScc` when referencing a constructor. Its `typeDependencies` and a type SCC's `typeDependencies` target only `TypeSccModel` values.
+ Collections can reference the same SCC for different symbols. References to a type and its constructors share the same `TypeSccModel` value, with only the constructor references wrapped in `TypeScc`. These references do not copy declaration bodies.
+ These types represent nodes in the dependency graph produced by canonicalization.
+ Optimizations based on Elm syntax transformations consume and produce graphs of these SCC values, not necessarily one output SCC per input SCC. Examples are inlining, elimination of higher-order functions by instantiation and monomorphizing in general.
+ Graphs of these SCC values are inputs to transformations to lower level representations in applications such as Elm compiler and Elm syntax interpreter.

### Value and Type Dependency Lookup

The dependency collections are symbol-keyed. A value SCC's `dependencies`
contains value symbols, including functions and constructors used in
expressions or patterns. Both SCC kinds have `typeDependencies` containing
type symbols, including choice types and type aliases. The syntax context
determines which collection to use, and its resolved module and symbol name
determine the key. Source imports and exposure rules are resolved before
constructing these entries.

For example, references to the type `Maybe.Maybe`, constructor `Maybe.Just`,
and constructor pattern `Maybe.Nothing` use these entries:

```text
typeDependencies[ (["Maybe"], "Maybe") ] = maybeTypeScc
dependencies[ (["Maybe"], "Just") ]     = TypeScc maybeTypeScc
dependencies[ (["Maybe"], "Nothing") ]  = TypeScc maybeTypeScc
```

The target `maybeTypeScc` contains member `"Maybe"`, whose declaration defines
constructors `"Just"` and `"Nothing"`. Constructor symbols remain in the syntax
and dependency keys; they are not replaced with the owning type's name, nor
stored as separate declaration members.

The collections distinguish identical qualified names in the type and value
namespaces. For example, module `Types` can contain:

```Elm
type Payload = Thunk Int
type Tree = Payload Payload
```

A type reference to `Types.Payload` and a constructor expression or pattern
using `Types.Payload` have the same key but select different SCCs:

```text
typeDependencies[ (["Types"], "Payload") ] = payloadTypeScc
dependencies[ (["Types"], "Payload") ]     = TypeScc treeTypeScc
```

For a `ValueScc` value dependency, the symbol selects a value declaration
member. For a `TypeScc` value dependency, it selects a constructor belonging to
a type member, not the type name itself. A closed record type alias also
supplies its record constructor in the value namespace. For a
`typeDependencies` entry, the symbol selects a type declaration member
directly. Each selection must identify exactly one target in the requested
namespace; missing or ambiguous selections must be rejected.
Consumers can reuse derived indexes over target SCC members and constructor
names; lookup must not repeatedly scan declaration bodies or search transitive
dependencies for each reference. Unqualified references within the current SCC
use `members` with the same type/value namespace distinction.

Required declaration discovery, SCC construction, and dependency-closure
traversals follow both collections of a value SCC and the `typeDependencies`
collection of a type SCC. Type dependency paths therefore remain entirely
within type SCCs. An SCC referenced through several symbols or through both
collections must remain shared and must not cause repeated canonicalization
work.

### SCC Partition Changes During Optimization

Optimizations may change dependency edges and therefore the SCC partition.
For example:

```Elm
f n =
    if True then
        n
    else
        g n

g n =
    f n
```

Initially, `f` and `g` form one SCC. Simplifying the conditional to `f n = n`
removes the edge from `f` to `g`. If both declarations remain required, the
result has two SCCs, with `g` depending on `f`.

A transformation of one input SCC may therefore return several output SCCs.
Results must reflect the transformed graph's SCC partition, not preserve the
input grouping when its members are no longer strongly connected. The result
maps each caller-requested declaration to its resulting SCC and member or
constructor symbol. Resulting SCCs are represented as a graph with shared
dependency values where edges remain; they must not be packed into one
artificial SCC to fit a single-result interface.

References crossing a newly created SCC boundary must become dependency
entries with corresponding qualified syntax references, using fresh module
names where necessary as described below. Dependency collections must reflect
the transformed syntax. Removing references during optimization does not relax
the requirement to diagnose unknown references in the original required
declaration set.

### Dependency Scope Preservation During Inlining

Package A's `Alpha.run` and package B's `Beta.run` may each depend on a
different private `Internal.helper`. Their separate dependency collections
are unambiguous. Inlining both bodies into the same destination reference
scope while retaining the helpers as dependencies would make the original
keys collide.

Where necessary, inlining must generate fresh module names and rewrite
affected syntax references and corresponding keys in `dependencies` and
`typeDependencies`, for example:

```text
Inline0.Internal.helper -> first helper's SCC
Inline1.Internal.helper -> second helper's SCC
```

Generated names must be valid Elm module names, collision-free in the
destination, and deterministic from semantic inputs rather than package
provenance. They are local qualifiers, not new source modules. Renaming
qualifiers alone must preserve resolved binding relationships and leave
referenced SCC values unchanged and shared. Previously unqualified references
to original SCC members that remain outside the moved code must also be
qualified.

## Incremental processing and storage

Every reusable semantic transformation accepts immutable, provenance-free
values and returns immutable, provenance-free values. This applies to both
successes and failures, and to any intermediate models included in a cache
input. A cached failure records the failed operation, not a formatted message
with a previous caller's location.

Besides template and SCC construction, reusable stages can include conversion
to a consumer's semantic representation, **lowering** (rewriting that
representation into simpler operations), code generation from an SCC and its
already generated dependencies, and preparation for interpreted execution.
An expensive stage must not run again merely because a preceding stage was
retrieved from a cache, when that stage's own input and cached result are
unchanged.

Inputs include only the semantic environment facts that the transformation
consults, such as type constraints, constructor information, and relationships
between nominal bindings. Unused local signatures still contribute type facts
when the transformation consults them. Unrelated entry points and provenance
must not invalidate that input. Binding identifiers are normalized to relative
roles before caching; correspondence to the caller's bindings is restored
outside the reusable transformation.

Language-defined runtime representations are semantic facts, not provenance.
Sharing an ordinary choice type with the same shape must not grant it a
language-defined role. Optimizations must preserve language-level behavior:
for example, collections representing equal values with different internal
trees cannot be compared solely by their tree structure.

## Provenance from entry points and structural paths

Provenance belongs to the caller's source context, not reusable artifacts.
A particular source use is identified by an entry point, the sequence of
resolved reference edges leading from it, and a **structural path** within the
declaration being processed.

A structural path describes syntax children, including signatures, function
arguments and bodies, type annotations, constructors, patterns, case branches,
and local declarations. Expression and pattern parentheses are transparent to
semantic paths. Record fields are selected by name and occurrence so paths do
not depend on differing field orders in semantic and source representations.

When a transformation changes tree structure, its result includes
output-to-input path correspondences. A generated expression for a record
update, for example, points back to the original update and its fields. These
correspondences contain structural steps, never source coordinates.

Diagnostic presentation follows the current caller's reference path and
structural path to retrieve files, imports, package origins, and source ranges.
It must not select an arbitrary source owner by reversing a shared artifact
identity. The same rule applies to failures during later transformations and
code generation. Invalid paths must produce explicit errors, not fabricated
locations.

Source-oriented presentation obtains documentation, token and separator
positions, literal spellings, and parentheses from the current source syntax.
These details are attached outside semantic caching. Moving code or changing
source formatting must not cause an old location or spelling to be presented
after a cache hit.

Clients observing prepared expressions, such as test discovery and source
navigation, associate shared preparation results with fresh caller-specific
source uses. They must not repeat semantic preparation merely to obtain distinct
observation identities. Execution state, captured runtime values, observers,
cancellation, and execution limits remain outside stored semantic templates.

An incremental environment retaining earlier resolved bindings preserves those
bindings when later submissions change the visible scope. It still follows
their existing dependency edges when constructing required SCC values, rather
than silently resolving earlier references against the new scope.

## Work bounds and acceptance properties

Each required source binding is traversed once per invocation, and each distinct
canonicalization input is transformed at most once. Dependency discovery and
SCC construction must take work proportional to the number of required
declarations and direct reference edges. Body processing is proportional to
the syntax processed, not the number of paths leading to it.

SCC construction processes its own `members` and direct entries in its
dependency collections without expanding transitive dependency bodies.
Runtime work for comparing or storing shared SCC values must not repeatedly
process the same dependency bodies through different paths. Derived flat views
follow value and type dependency edges and visit each distinct SCC value once.
Any runtime fingerprints or internal handles used to provide this efficiency
are implementation details, not separate semantic SCC identifiers.

Automated acceptance checks must cover at least:

- Different public and private module definitions in separate package scopes,
  and ambiguity within one scope.
- An application and several packages sharing a dependency through multiple
  paths, with one retained artifact and no repeated canonicalization.
- Unchanged forks and version-label changes reusing artifacts, changed bodies
  changing dependent SCC values, and changed visibility invalidating answers.
- Unknown references outside the required set being ignored, and references
  inside signatures, aliases, patterns, operators, or unused locals being
  required and diagnosed.
- Constructor expressions and patterns selecting the owning type member while
  preserving symbol-keyed lookup, several symbols sharing one retained SCC,
  and identical qualified names selecting different SCCs through `dependencies`
  and `typeDependencies`.
- Dependency discovery and closure following value and type edges, including
  dependencies referenced only in type annotations.
- Value and type SCCs containing only their permitted declaration kinds,
  type dependency paths containing only type SCCs, and references to a type
  and its constructors sharing one retained `TypeSccModel` value.
- Equivalent recursive groups converging despite source-owner changes and
  different entry-point enumeration orders.
- Edge-local module prefixes resolving collisions while aliases preserve
  equivalent declaration inputs.
- Inlining across dependency scopes rewriting colliding qualifiers and newly
  external member references without changing bindings or copying dependency
  SCCs.
- Optimization removing a cycle and returning several valid SCCs, with updated
  dependency references and caller-requested declaration mappings, rather than
  retaining the original member grouping.
- Identical choice type artifacts remaining distinct nominal source types.
- Cached successes and failures reporting current source locations through
  multiple reference paths, including transformed expressions and parentheses.
- Cached inputs, intermediate models, and results containing no provenance.
- Empty SCCs, member groups that are not strongly connected, and dependency
  keys that do not select a valid symbol in the required namespace being
  rejected.
- Concurrent requests retaining one representation of equal SCC values.
- Initially empty caches and small-edit reuse meeting the stated work bounds
  without relying on preexisting compilation history.

These properties can be exercised with an in-memory application, package
sources, dependency relationships, exposure rules, and requested entry points;
they do not depend on a particular storage system or test framework.
