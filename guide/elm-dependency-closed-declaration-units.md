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

Artifacts are **equivalent** when their semantic transformation inputs are
equal after source-specific identities have been replaced with relative
references. Equal outputs for a few executions are not sufficient.
**Convergence** means sharing one retained representation for equivalent
artifacts, independently of their source owners.

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
3. A retained declaration artifact must be shared when referenced through
   several packages. A dependent declaration must not retain another copy of
   its dependencies' bodies.
4. Renaming a package, publishing an unchanged fork, or changing a version label
   must not change the meaning of otherwise unchanged scope relationships or
   prevent equivalent artifacts from converging to one retained representation.
   Changed sources or visibility must still affect the result.
5. Unknown references outside the required declaration set must not fail
   compilation, whether they occur in application modules or package modules.
   Unknown references inside that set must be reported even when execution
   would not reach them.
6. Canonicalization work must not be repeated for the same declaration input
   during one compilation, including when several dependency paths reach it.
7. Compilation with initially empty caches must traverse declarations and
   dependencies efficiently, without repeatedly expanding dependency bodies.
8. Every potentially expensive transformation that commonly recurs after small
   edits must have an independently reusable input and result. Incidental
   package names, versions, and provenance must not enter semantic cache inputs.
9. Package references must admit module-name mappings, such as a prefix applied
   to imported module names, so otherwise ambiguous dependencies can be used
   together. Elm import aliases must remain usable with these mappings.
10. The design must be expressible using immutable values and pure functions,
    without requiring side effects or hidden global state.
11. Artifact sharing must preserve visibility rules and must not automatically
    unify distinct source bindings or nominal types.

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

## Declaration-local transformations

A **reference slot** is a declaration-local identifier for a resolved binding.
It has no package name or source location. Multiple uses of the same binding
reuse one slot. Distinct bindings retain distinct slots even if their semantic
artifacts are equal. This preserves which references denote the same binding
without retaining its source identity.

A **declaration template** is canonical syntax plus its reference slots and
structural observations. Its transformation input consists of:

- Syntax without source positions, documentation, or other presentation details.
- Answers to the name-resolution questions made by that syntax, expressed using
  reference slots, referenced symbols, and relevant semantic facts.
- Options that affect the transformation's meaning.

References to the declaration's own module are represented relatively rather
than using a source-owner namespace. Lexical names that affect meaning remain
syntax inputs; the design does not require all renamings of local variables or
declarations to produce equal artifacts.

Resolution answers must distinguish available declarations, language-defined
facilities, and explicitly available type information without executable code.
They must also distinguish missing names or modules, private declarations,
names used as a value instead of a type or conversely, unimported qualifiers,
and ambiguity. Checks for existing bindings must include observations relevant
to shadowing and to avoiding collisions with generated local names.

Before reusing a template, the current scope must supply its current answers.
Removing an exposure, adding a competing import, or changing whether a name
exists must invalidate any affected success or failure. The semantic worker
receives these answers as data, not as a resolver or callback carrying a scope.

A template does not include dependency bodies or their content identities.
Changing a referenced function's body can therefore reuse an unchanged
caller's canonicalization. Dependency changes are incorporated at the unit
boundary defined below.

## Dependency-closed units

The required declaration set forms a directed graph: declarations are vertices,
and direct references are edges. A **strongly connected component (SCC)** is a
maximal nonempty group of vertices in which every vertex can reach every other
by following edges. A single nonrecursive declaration also forms an SCC.
Grouping the graph into SCCs leaves an acyclic graph between components.

A **declaration unit** represents one SCC. Units are constructed bottom-up:
every external dependency unit is available before its dependent unit is
constructed. Recursive references select members of the current unit.
An **export selection** identifies one member of a unit and, where necessary,
a symbol belonging to that member, such as a constructor.

A unit contains only:

- Its own canonical declaration bodies.
- Direct export selections from already constructed dependency units.
- Any explicit type-only interfaces and language-defined semantic roles needed
  to interpret those bodies.
- Transformation options that affect their meaning.

A **type-only interface** supplies a declaration's type information without
executable code. Any nominal identities in that information are represented as
local parameters whose distinctness is preserved. These interfaces can support
analysis, but requesting executable code that they do not provide must fail.
An unresolved reference must not be disguised as an interface or an invented
dependency. Analysis that explicitly permits unresolved references produces an
incomplete result, not a dependency-closed unit.

All references inside a closed unit must select a valid member, dependency
export, interface, local binding, or language-defined facility. Each selected
symbol must belong to its target. Empty components, invalid selections, and
groups whose members are not strongly connected must be rejected.

Member and dependency slots must be labeled deterministically from semantic
structure, not package labels, source paths, entry-point enumeration, or
incidental traversal order. Structurally tied recursive members must be handled
without a source-based tie-breaker. Each caller retains the correspondence
between its source bindings and the resulting member selections.

A **content identity** identifies a unit from its own syntax, interfaces,
semantic roles, options, and direct dependency export identities. It must not
be supplied independently of that validated content. Its computation must not
serialize transitive dependency bodies again. Comparing unit identities and
export selections must likewise avoid recursively comparing those bodies.
Dependency edits propagate through dependent unit identities without requiring
the canonicalization of unchanged caller templates.

Equal units share one retained representation, including when several callers
request them concurrently. Their direct references keep dependency artifacts
shared rather than copying the dependencies into each caller. Distinct source
binding roles remain distinct selections even when their target artifact is
shared. Type analysis uses source binding relationships, not artifact equality,
to decide nominal type equality.

A consumer requiring a flat declaration collection may derive one by visiting
each shared unit once. This derived view is not a replacement for the shared
dependency graph. Public output names and selections come from the caller's
requested entry points; required dependencies are not automatically published
as additional entry points.

## Incremental processing and storage

Every reusable semantic transformation accepts immutable, provenance-free
values and returns immutable, provenance-free values. This applies to both
successes and failures, and to any intermediate models included in a cache
input. A cached failure records the failed operation, not a formatted message
with a previous caller's location.

Besides template and unit construction, reusable stages can include conversion
to a consumer's semantic representation, **lowering** (rewriting that
representation into simpler operations), code generation from a unit and its
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

An **immutable value** cannot be changed after it is created. A **pure function**
determines its result only from its input values and does not change external
state. A **cache** retains a transformation's input and result for later reuse.
Storage ownership and retention policy are explicit. Pure functions may receive
an immutable cache and return an updated cache alongside their result. Neither
hidden compilation history nor a global mutable identity table is required.
Discarding cached entries may cause recomputation but must not change results.
An invocation must still avoid duplicate canonicalization work even if storage
retained between invocations is empty or limited. Stored formats must distinguish
incompatible semantic or model revisions.

Switching an import to an equivalent declaration can reuse both the template
and unit. Switching it to a declaration with a changed body may reuse the
template but changes the dependent unit. These cases must not be conflated.

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
their existing dependency edges when constructing required units, rather than
silently resolving earlier references against the new scope.

## Work bounds and acceptance properties

Each required source binding is traversed once per invocation, and each distinct
canonicalization input is transformed at most once. Dependency discovery and
SCC construction must take work proportional to the number of required
declarations and direct reference edges. Body processing is proportional to
the syntax processed, not the number of paths leading to it.

Computing unit content identities examines own bodies and direct selections,
not expanded transitive bodies. Derived flat views visit each unique unit once.
Deterministic labeling may require extra comparisons for structurally symmetric
recursive groups; that work must remain local to the group and must not repeat
dependency-body transformations.

Automated acceptance checks must cover at least:

- Different public and private module definitions in separate package scopes,
  and ambiguity within one scope.
- An application and several packages sharing a dependency through multiple
  paths, with one retained artifact and no repeated canonicalization.
- Unchanged forks and version-label changes reusing artifacts, changed bodies
  invalidating dependent units, and changed visibility invalidating answers.
- Unknown references outside the required set being ignored, and references
  inside signatures, aliases, patterns, operators, or unused locals being
  required and diagnosed.
- Equivalent recursive groups converging despite source-owner changes and
  different entry-point enumeration orders.
- Edge-local module prefixes resolving collisions while aliases preserve
  equivalent declaration inputs.
- Identical choice type artifacts remaining distinct nominal source types.
- Cached successes and failures reporting current source locations through
  multiple reference paths, including transformed expressions and parentheses.
- Cached inputs, intermediate models, and results containing no provenance.
- Invalid units and export selections being rejected, and concurrent requests
  retaining one representation of equal units.
- Initially empty caches and small-edit reuse meeting the stated work bounds
  without relying on preexisting compilation history.

These properties can be exercised with an in-memory application, package
sources, dependency relationships, exposure rules, and requested entry points;
they do not depend on a particular storage system or test framework.
