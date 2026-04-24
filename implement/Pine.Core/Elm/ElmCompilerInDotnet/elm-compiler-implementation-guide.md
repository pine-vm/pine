# Elm Compiler Implementation Guide

For semantics of the Elm programming language, consult the file 'elm-programming-language-semantics.md'

## Entry Point

The entry point for compilation is the declaration in one of the Elm modules in the source code that supports integration with and connection to the outside world. Compiler applications often expose an API that offers selecting a module as an entry point. In these cases, the compiler application applies a default selection to pick a declaration from that module. Traditionally, that was simply the one declaration named main.

### Encoding of Elm Values at Entry Points

To serve its role in connecting to other systems, an entry point must support incoming values and encode outgoing values using predictable, stable, legible encodings.

For parameters and return values of entry points, the encoding of Elm values follows the model established in the `ElmValueEncoding.cs` file. The definitions in `ElmValueEncoding.cs` cover the encoding of all Elm values except functions. Functions are encoded according to the model in FunctionValueBuilder.cs.

### Relation to Platforms

When looking at the compilation of Elm programs to Pine, a platform is just a constraint on the type of the entry point. Since the platform is the system that translates between values and effects, it is outside the scope of the Elm app and this specification.

### Simplified Internal Function Interfaces

For functions not called directly from outside, the compiler may emit a simplified interface to optimize for smaller program size or runtime efficiency.

The following are some examples of such optimized internal representations:

+ For a parameter that has a choice type, we might omit the tag if the source type declaration only contains a single tag anyway.
+ For a parameter that has a record type, we might omit field name labels if the record type is closed, or we can otherwise prove that each field name used by the function always has the same offset.

We use these optimizations for both function parameters and return values.

The compiler tracks the concrete representations of function parameters and return values internally to ensure correct argument packaging and return value consumption.

## Function Applications

To compile a function application from Elm syntax, we use the 'ParseAndEval' expression variant of the Pine language. This expression creates a new environment for evaluating an expression, similar to the 'eval' functions found in other languages, such as Python. Since there are no symbolic references in Pine, we transport any other functions that the called function depends on into this new environment.

Function applications are grouped into full applications and partial applications. Partial applications are all usages of functions in which fewer arguments are supplied than the number of parameters. These partial applications produce function values.

> Note: The approach using function values and a generic application works in any case. The reason we use a different implementation for the full application case is to emit more efficient code.

### Full Function Applications

For function applications where the number of arguments equals the number of parameters of the function (non-partial application), we use the following pattern to compile Elm function applications, as a convention:
The environment is a flat list. The first item in this list contains all the encoded function bodies needed for further applications. This set contains all the transitively referenced functions that have not been inlined. The remaining items in the list are the arguments from the source Elm code, each at its own position in the root list.

The important point here is what **flat** means: The arguments are flat at the
**root env level**. The env-functions payload itself still remains grouped as
the first root item. So for a full application with two Elm arguments, the
shape is:

```txt
[ [envFunctions]
, arg0
, arg1
]
```

and **not**:

```txt
[ [envFunctions]
, [arg0, arg1]
]
```

This distinction matters when reading instruction traces. A trace that shows
one `Build_List` to materialize the env-functions list and a second
`Build_List (arity + 1)` to materialize the root env is still following the
flat-root convention. The former builds the item at root index `0`; the latter
assembles the complete root env list containing the env-functions at index `0`
followed by the flattened arguments.

For example, a recursive function, when calling itself via 'ParseAndEval', composes the new environment so that it also contains a representation of the function itself in encoded form, to enable continuing recursion in the non-terminating branch.

The following example illustrates the pattern using a concrete recursive function:

```Elm
factorial : Int -> Int
factorial n =
    if Pine_builtin.int_is_sorted_asc [ n, 1 ] then
        1

    else
        Pine_builtin.int_mul
            [ factorial (Pine_builtin.int_add [ n, -1 ])
            , n
            ]
```

Here, we have one entry for the encoded function and one entry for the parameter defined in the Elm source code.
Following the pattern established earlier, our expression to create the new environment value looks as follows:

```txt
[ [ value_encoding_factorial ]
, Pine_builtin.int_add [ n, -1 ]
]
```

Because of the direct recursion, the calling function happens to be the called function, so placement of the components in the environment is symmetrical. The parameter named "n" is at index 1 in the flat environment (right after the env functions list):

```txt
[ [ current_env[0][0] ]
, Pine_builtin.int_add [ current_env[1], -1 ]
]
```

Following this pattern ensures that common inspection and profiling tooling can parse and analyze the invocations. We also use these tools to derive symbols for pseudo-functions rendered as part of snapshot tests.

When implementing or reviewing this compiler path, the key property to preserve
is: **Every Elm argument of a full application must become a sibling entry in
the root env list, immediately after the env-functions list.** If a trace shows
the compiler first assembling a separate root-level args list and then pairing
that list with the env-functions list, that would be the older nested shape and
would not satisfy this convention.

### Composition of the Environment Functions List

The following are general rules we use to compose the list of functions in the eval environment introduced above.

> **Post-§7.7 status (analysis-doc 2026-04-22).** The runtime
> env-functions list is now restricted to the **SCC members only**.
> Cross-SCC dependencies are no longer threaded through
> `current_env[0]`; instead they are inlined at the call site as a
> `Literal(callee.EncodedExpression)` (Form A) or
> `Literal(callee.WrapperValue)` (Form B) sourced from the
> caller-side `CompiledFunctionInfo` cache. See the subsection
> *"Storing Compiled Functions and Two Call-Site Forms (Form A and
> Form B)"* below for the details.

#### What to Include in the Environment Functions List

+ All functions in a group of mutually recursive functions (the SCC members) must be included.
+ For non-recursive single-member SCCs the env-functions list is **empty** and the wrapper is emitted in the `WithoutEnvFunctions` shape — there is no `env[0]` env-functions slot at runtime, parameters live at `env[i]` directly.

#### How to Order Entries in the Environment Functions List

+ All functions in a group of mutually recursive functions ([strongly connected component](https://en.wikipedia.org/wiki/Strongly_connected_component)) use the same order. This makes it easier to model invocations in the recursive set, since we can forward the entire list rather than individual items.
+ The order is the SCC members in stable sorted order (typically alphabetical by qualified name) so that emission is deterministic across compilations.

### Function Values And Generic Function Application

The Elm programming language supports first-class functions and partial application. When a function whose declaration we know (it is not a parameter in the current context) escapes the current context, the Elm compiler emits a representation of this function as a value that can then be freely passed around to other parts of the program and applied sometime later.

A function value encodes a function in a way that enables the incremental addition of further arguments. For each argument, the applying side uses a `ParseAndEval` expression — this is exactly the **Form B** mechanism described in the *"Storing Compiled Functions and Two Call-Site Forms"* subsection below; the same mechanism is used both for a function value of unknown origin and for a known callee that is not saturated at its call site.

This function value contains not only an encoding of the function body, but also a list of the encoded function bodies the wrapped function depends on. For recursive SCC members the list contains the SCC members; for non-recursive functions the list is empty (the wrapper is emitted in the `WithoutEnvFunctions` shape) and any cross-SCC callee referenced from the body is inlined at its own call site as a literal — so the dependency does not need to be carried around as an env-functions entry.

For function applications where the function is a value of unknown origin, the compiler emits an expression that adds the given arguments using a form that allows for generic partial application. It emits this partial application as `ParseAndEvalExpression`, where the `Environment` contains the argument value. (If the Elm application expression contains multiple arguments, the compiler nests this pattern recursively)

When emitting a function value, the compiler creates a corresponding wrapper matching the number of parameters. On application of the last argument, the wrapper uses an environment structure as described in the 'Full Function Applications' section.

### Storing Compiled Functions and Two Call-Site Forms (Form A and Form B)

This subsection describes the design that the emitter and all consumers
of compiled Pine values are migrating toward. The full rationale, the
incremental migration plan, and the per-step status notes live in
[`explore/internal-analysis/2026-04-22-analysis-inline-non-recursive-callees.md`](../../../../explore/internal-analysis/2026-04-22-analysis-inline-non-recursive-callees.md).

**Four properties stored per emitted function.** For every top-level
function the compiler records:

1. **`WrapperValue`** — the partial-application wrapper as a `PineValue`,
   produced by either `FunctionValueBuilder.EmitFunctionValueWithEnvFunctions`
   (recursive SCC members) or `FunctionValueBuilder.EmitFunctionValueWithoutEnvFunctions`
   (non-recursive members). This is the value that escapes when the
   function is used as a first-class value.
2. **`ParameterCount`** — the function's declared arity.
3. **`EnvFunctions`** — the list of captured env-function values used
   by the closure. Empty for non-recursive functions emitted with the
   `WithoutEnvFunctions` shape; otherwise the SCC member list in stable
   sorted order.
4. **`EncodedExpression`** — the function body encoded as a `PineValue`
   in the env-functions-at-index-0 layout (i.e. `env[0]` holds the
   captured env-functions list and `env[1..N]` hold arguments
   `arg0..arg{n-1}`). This is the artifact callers parse-and-eval at
   saturated call sites (Form A).

**Two functionally equivalent call-site forms.** At every call site the
emitter chooses one of:

- **Form A — saturated, compact.** Used when the number of supplied
  arguments equals `callee.ParameterCount`. A single `ParseAndEval`
  whose `encoded` is `Literal(callee.EncodedExpression)` and whose
  `environment` is `List [ Literal(List(callee.EnvFunctions)), arg0, …, arg{n-1} ]`.
  For non-recursive callees the literal env-functions list is empty.
- **Form B — generic, per-argument chain.** Used otherwise (partial
  application, or where the arity is not statically known). A nested
  `ParseAndEval` chain whose innermost `encoded` is
  `Literal(callee.WrapperValue)` and where each level supplies one
  argument as the `environment`.

**Rationale.**

- Form A keeps the saturated common case down to a single
  `ParseAndEval`, reducing both expression-node count and runtime work.
- Form B preserves a uniform partial-application pathway and lets call
  sites pass a callee whose arity is not statically pinned.
- Storing all four properties — instead of just the wrapper value —
  removes the per-call-site cost of re-deriving the encoded body from
  the wrapper.

**Canonicalization requirement.** All consumers of compiled Pine —
most importantly the static-program parser used by snapshot tests —
must recognize **both forms** and canonicalize them to the **same
name** for any saturated application of the same callee. Without this,
a downstream comparison would diff Form A and Form B occurrences of
the same logical call. The canonicalization currently lives in
`StaticProgramParser.ParseCurriedFunctionApplication`; Form A is
detected via the optional `IdentifyEncodedBodyOptional` callback on
`StaticProgramParserConfig<T>`, which maps an encoded-body value back
to the originating callee identifier.

**Wrapper layout discriminant.** Because the two emitter shapes use
different runtime environment layouts — `[envFunctions, arg0, …]` for
`WithEnvFunctions` and `[arg0, …]` for `WithoutEnvFunctions` —
`FunctionRecord` carries a `bool UsesEnvFunctionsLayout` discriminant.
Downstream consumers that build env-value classes
(`NamesFromCompiledEnv.BuildApplicationFromFunctionRecord`), parse
parameter references (`StaticProgramParser.ParseExpression` and
helpers via `envParametersOffset`), or construct runtime environments
(`ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr`) all
branch on this flag.

## Closures

The Elm programming language supports closures that capture the values of bindings in scope. Following is an example:

```Elm
map : (a -> b) -> List a -> List b
map f xs =
    foldr (\x acc -> f x :: acc) [] xs
```

The compiler resolves closures via lambda lifting in an early step in the compilation pipeline. The code below shows the result of the lambda lifting stage for the earlier example of a closure:

```Elm
map : (a -> b) -> List a -> List b
map f xs =
    foldr (map__lifted__lambda1 f) [] xs


map__lifted__lambda1 f x acc =
    f x :: acc
```

## Type Inference

This Elm compiler implements only partial type inference. Since it is not sufficient to detect all type mismatches, users also want to use an additional type checker to verify that a given Elm program is valid.

Another implication of this partial type inference is that adding type annotations can yield more efficient code.

The limited type inference here supports:

+ Type of binding from type annotation, if it does not contain any type variable or open record.
  + This also covers infix operators that constrain to a concrete type, like `//` and `/`.
+ Type of function argument from function type.
  + This function type does not have to come from an annotation, but could also have been inferred.
  + Includes function applications using choice type tags (e.g., `Maybe.Just`, `Result.Ok`)
  + Includes applications of functions implied by record type alias declarations.
  + When a variable is used as an argument, its type is inferred from the function's parameter type at that position.
+ Picking up primitive types from int pattern, hex pattern, string pattern, etc.
+ Type of tuple from tuple expression. (item -> tuple)
+ Type of tuple item from tuple expression. (tuple -> item)
+ Type of tuple item from tuple pattern.
+ Type of list item from list pattern.
+ Type of list item from uncons pattern.
+ Type of list from list expression.
+ Type of argument from 'named' pattern (deconstruction of choice type tag).
  + This also works in expression contexts: when a variable is used as an argument to a choice type constructor, its type is inferred from the constructor's argument type.
+ Type of record from record expression.
+ Type of record field from record access.
+ Type of record field from record update.
+ Distinction between `String` and `List a` to emit specialized code where the `++` operator (`appendable` type class) is applied.
+ Propagation via arithmetic operators like `+` `-` (`number` type class) to distinguish between `Int` and `Float` and emit specialized code for `Int` arithmetic. When one operand has a known type, the type propagates to the other operand in the expression.
+ Type inference through let expressions: Types are propagated from parameters through let bindings. When a let binding's expression has an inferred type, that type is associated with the binding name for use in subsequent expressions.
+ Type unification between branches: In if/case expressions, the types of all branches are unified to determine the most specific common type. For example, if one branch returns `Int` and another returns a `number` literal, the result type is `Int`.

## Arithmetic Operations

The Elm core library offers arithmetic operations that are polymorphic and work on the `number` type class. The core library exposes these only as operator symbols.
For example, it [exposes `Basics.add` as an infix operator `+`](https://github.com/elm/core/blob/84f38891468e8e153fc85a9b63bdafd81b24664e/src/Basics.elm#L82), which also implies the availability as a function named `(+)`.

For operations where we prove the type must be `Int`, we emit specialized code accordingly. In cases where the type is `Float` or an unconstrained `number`, we emit a function that works with any `number` values.

Where the type is constrained to `Int`, we lower `Basics.add` and `Basics.mul` from Elm directly to the `int_add` and `int_mul` Pine builtin functions. We also lower `Basics.sub` to a combination of `int_add` and `int_mul` by multiplying the right operand by `-1` and then adding it.

## Records

For Elm code producing record values, we always produce records with fields sorted alphabetically by field name. This makes subsequent operations on these values, like equality checks, simpler.

### Record Access and Record Update

How we handle record access and record updates depends on how much we know about the record type at compile time: when we have inferred a closed set of field names, we use the field-name index to perform an index-based lookup or insertion.

However, due to row polymorphism, that index can vary across call sites if the function we emit has not been narrowed to a closed set of field names. In these cases, we emit a function that recursively scans the record to find the field with the matching name.

### Record Constructors

In Elm, a `type alias` declaration of a record type also implies the creation of a function of the same name. These record constructors can be used like other functions, including partial application.

The order of arguments is the same as the order in which the fields appear in the type alias declaration code. This means that multiple record type alias declarations can produce the same type, but different constructors.

## Composition of Module Values

The compiler interface offers functions that return module values, or a composition comprising multiple named modules, such as `CompileInteractiveEnvironment`

Each module value in turn encodes a list of named values. The declarations included here fall into two categories:

1. Declarations the caller selected as entry points to be compiled.
2. Additional entries to support inspection and debugging use cases.

Tooling for inspection and parsing uses these additional names when transforming from Pine program code into a human-readable representation. Some of these names correspond directly to declarations in the source program code. Other names are derived from source declaration names by adding a suffix to uniquely distinguish specialized variants of functions from the source code. That suffix consists of a plus sign, followed by a hexadecimal representation of a hash code. That hash code is derived from a description of all the specializations applied to the source function to arrive at the named variant.

Examples for specializations that can result in varied forms of a function:

+ Fixing a function parameter to a concrete value.
+ Constraining the type of a parameter from an open record to a closed record.
+ Constraining a parameter from a type class like `number` to a concrete type like `Int`.

## Compilation Stages

### Overall Pipeline

At a high level, the compiler processes Elm modules in the following order:

1. Parse Elm source into Elm syntax trees.
2. Canonicalize names across the application and its dependencies.
3. Lambda-lift closures into top-level helper functions.
4. Run the specialization stage, unless the caller disables the syntax-optimization pipeline.
5. Run the inlining stage on the already-specialized modules.
6. Run lambda lifting again on code introduced or exposed by specialization and inlining.
7. Run builtin-operator lowering on the optimized Elm syntax.
8. Compile the resulting Elm expressions and declarations to Pine values.

The specialization and inlining stages therefore work on already-canonicalized, already-lambda-lifted Elm syntax. They are still operating on Elm syntax trees at this point; they do not work on Pine expressions yet.

### Canonicalization

#### Input to Canonicalization

The input to canonicalization is a tree of ‘package units’ where the root unit is the application, and the next level of units are the packages directly imported by the application. Each of these units contains a set of Elm modules.

We could not input all modules in a flat set, because that could lead to name clashes when module namespaces are used by multiple different modules in the dependency tree.

The module contents are modeled as the parsed Elm syntax.

#### Output from Canonicalization

The module contents returned from canonicalization use the same Elm module syntax model as the input. Also, despite exchanging names within the syntax nodes, canonicalization returns the same locations and ranges, so that the following compilation stages can produce error messages aligned with the source layout.

The module syntax models returned by canonicalization do not contain any import statements or exposing lists, since these are no longer necessary after this stage.

Where necessary to avoid name clashes between transitively imported packages, canonicalization adds prefixes to module namespaces. To optimize readability during inspection, prefixes are added only where necessary. Different versions of packages in a dependency diamond do not force prefixing, since the contents of the actually used modules can still be the same.

Besides canonicalizing references in module contents, the canonicalization stage also produces errors like ‘Name Error’ and ‘Name Clash’, including the source ranges to render error messages in the right places.

### Specialization

The dedicated specialization stage runs after the first lambda-lifting pass and before the dedicated inlining stage. The compiler currently enters this stage from `ElmCompiler.cs` by calling `ElmSyntaxSpecialization.Apply(...)` on the lambda-lifted modules.

**Specialization** means creating an additional declaration whose parameter list or return type is specialized for a particular call pattern.

Typical examples are:

+ A higher-order function where one parameter is fixed to a concrete top-level function, inline lambda, or record-access function such as `.fieldName`.
+ A function that destructures a single-constructor choice type, where the specialized variant can receive inner fields directly instead of receiving the wrapped outer value.
+ A recursive or mutually recursive function where the compiler needs a named specialized variant to preserve recursion while removing or reshaping specialized parameters.

The defining properties are:

+ Specialization **adds declarations**.
+ The specialized declaration may have a **different function signature** from the source declaration.
+ Call sites may be rewritten to target the new specialized declaration.

Generated specialized functions are named deterministically. Tooling and inspection code can therefore show both original declarations and their specialized variants in a stable way.

#### Shape of the Specialization Transformation

Specialization uses two main passes:

1. A collection pass walks expressions and records which function specializations are needed.
2. A rewrite pass deduplicates those requests, assigns stable generated names, adds the specialized declarations to modules, and updates call sites to use them.

This split is important because recursive and mutually recursive functions often need a named specialized variant instead of direct substitution at the original call site.

#### What the Specialization Stage Produces

The input to the stage is a set of Elm module syntax trees after lambda lifting. The output is another set of Elm module syntax trees:

+ Existing declarations may have rewritten bodies.
+ Modules may gain additional generated function declarations representing specialized variants.
+ Generated declarations may have parameter lists that differ from the original source declaration because specialized-away parameters are removed or wrapper parameters are flattened.
+ The transformed syntax remains in Elm form so that later pipeline stages can continue to use the same syntax model and compilation logic.

#### Boundaries and Non-Goals

The current stage also has deliberate limits:

+ It is optional and can be disabled by the caller.
+ It does not try to specialize every call site; the current compiler configuration restricts it to opportunities that align with the current function-oriented heuristics.
+ It avoids relying on local let-bound helper references as specialization triggers, because those can interact poorly with lifted dependencies across module boundaries.
+ It preserves recursion by generating specialized recursive variants instead of blindly expanding recursive calls inline.
+ It skips specialization for very large function bodies to avoid excessive work and code growth.

### Inlining

The dedicated inlining stage runs after specialization and before the second lambda-lifting pass.

#### Definition of Inlining

**Inlining** means rewriting expressions inside existing declarations without introducing additional module-level declarations and without changing the signature of the declaration being rewritten.

This includes the following categories:

+ Inlining direct calls where the supplied arguments make substitution profitable.
+ Beta-reducing lambda applications.
+ Reducing record-access-function applications when the record value is known.
+ Simplifying generated let expressions and related wrapper/unwrapper patterns after substitution.

The defining properties are:

+ Inlining **does not add declarations**.
+ Inlining **does not change function signatures**.
+ Inlining rewrites bodies of existing declarations so later stages receive simpler Elm syntax.

The stage operates on whole modules and can therefore rewrite both same-module and cross-module call sites, as long as the referenced function is available in the compiler's module set.

#### What the Inlining Stage Produces

The input to the stage is a set of Elm module syntax trees after specialization. The output is another set of Elm module syntax trees:

+ Existing declarations may have rewritten bodies.
+ No additional module-level declarations are introduced by the inlining stage itself.
+ Function names and function signatures of those existing declarations remain unchanged.
+ The transformed syntax remains in Elm form so that later pipeline stages can continue to use the same syntax model and compilation logic.

Because of these limits, there are optimizations that remain outside this stage. For example, simpler let-block substitution is described separately below and is not the same as adding specialized top-level declarations.

### Builtin Operator Lowering

After specialization, inlining, and the second lambda-lifting pass, the compiler runs a builtin-operator-lowering stage.

This stage stays in Elm syntax, but it rewrites certain canonicalized Elm operator applications into explicit builtin-oriented Elm expressions that later compilation stages can map more directly into Pine. In particular, when type inference proves an arithmetic operation is on `Int`, the stage lowers:

+ `Basics.add` / `(+)` to `Pine_builtin.int_add`
+ `Basics.mul` / `(*)` to `Pine_builtin.int_mul`
+ `Basics.sub` / `(-)` to `Pine_builtin.int_add` combined with `Pine_builtin.int_mul [ -1, right ]`

When type inference proves a comparison is on `Int`, the stage also lowers comparison operators to `Pine_builtin.int_is_sorted_asc`:

+ `Basics.le` / `(<=)` to `Pine_builtin.int_is_sorted_asc [ left, right ]`
+ `Basics.ge` / `(>=)` to `Pine_builtin.int_is_sorted_asc [ right, left ]` (operands swapped)
+ `Basics.lt` / `(<)` to `Pine_builtin.int_is_sorted_asc [ Pine_builtin.int_add [ left, 1 ], right ]` — since `int_is_sorted_asc` checks `<=`, adding 1 to the left operand converts the check to strict `<`. When either operand is a literal, the compiler folds the +1 offset into the literal instead of emitting `int_add`.
+ `Basics.gt` / `(>)` to `Pine_builtin.int_is_sorted_asc [ Pine_builtin.int_add [ right, 1 ], left ]` — operands swapped with the same offset strategy as `<`.

Additionally, when the stage encounters `Basics.and` / `(&&)` combining two `int_is_sorted_asc` applications that share a common middle operand, it merges them into a single call. For example, `a <= b && b <= c` becomes `Pine_builtin.int_is_sorted_asc [ a, b, c ]`. For strict comparisons (`<`), the merged form preserves all operands including the offset elements, resulting in `Pine_builtin.int_is_sorted_asc [ int_add [ a, 1 ], b, int_add [ b, 1 ], c ]`.

#### Equality Lowering

The stage also lowers `Basics.eq` / `(==)` to `Pine_builtin.equal` when type inference proves that the operand type supports *primitive equality* — meaning Pine structural equality is equivalent to Elm equality.

In Elm, only `Dict` and `Set` values can have different Pine representations that must be treated as equal (because the concrete red-black tree structure depends on insertion order). Similarly, `Float` (and the polymorphic `number` constraint) can have different Pine representations for the same value (The current implementation will sometimes reduce to a plain integer if the denominator is `1`). Any type that is guaranteed to never contain a `Dict`, `Set`, or `Float` value can therefore use `Pine_builtin.equal` directly, avoiding the overhead of the generic `Basics.eq` function.

The check is recursive over composite types via a single `TypeSupportsPrimitiveEquality` method:

+ **Primitive types safe for `Pine_builtin.equal`:** `Int`, `String`, `Char`, `Bool`
+ **Primitive types NOT safe:** `Float`, `number` (different Pine representations for the same value)
+ **Tuple types:** safe if every element type is safe (recursive)
+ **List types:** safe if the element type is safe (recursive)
+ **Choice types (Sum types):** safe if every argument type of every constructor tag is safe (recursive), with cycle detection for recursive types
+ **Type aliases:** expanded before checking
+ **Built-in collection types:** `List.List a` is treated as a list; `Dict.Dict` and `Set.Set` are always unsafe
+ **Type variables and unknown types:** not safe (could contain anything)

### Current Implementation Deviations From This Conceptual Split

The current implementation already uses separate pipeline stages in `ElmCompiler.cs`, but it still deviates from the conceptual split above in a few ways:

+ The main implementation still lives in `Inlining.cs`, which contains both specialization and inlining logic behind the internal `PipelineStage` enum (`Combined`, `SpecializationOnly`, `InliningOnly`). The public wrapper classes `ElmSyntaxSpecialization` and `ElmSyntaxInlining` are currently thin entry points over that shared implementation, rather than fully separate implementations.
+ The shared implementation still exposes a combined entry point, `Inlining.Inline(...)`, which conceptually mixes specialization and inlining even though the production compiler pipeline now calls the two dedicated stages separately.
+ Configuration and heuristics are still named from the inlining perspective: the shared stage uses `Inlining.Config` and the helper `ShouldInline(...)` to decide not only classic inlining but also several specialization opportunities. So the implementation boundary is cleaner in the pipeline than it is in the naming and control flow inside the optimization engine.
+ `Inlining.cs` still contains logic and comments that treat specialization-generated declarations as part of the broader “inlining” machinery, for example in `InlineModule(...)`, which collects generated declarations and immediately reprocesses them. That behavior is correct for the shared engine, but it means the code structure still blurs the distinction between “specialization adds declarations” and “inlining rewrites existing declarations only”.

## Let Blocks

The most generic way to compile a let block is to treat it analogously to a module: local declarations can be compiled like module-level declarations. However, apply extensive inlining of let blocks (declarations are substituted at their usage sites) to simplify the emitted code.

A local declaration is not suitable for inlining if it contains a function application and is referenced more than once.

There is no need to check for recursive references in the block declarations separately, because a self-referencing declaration has at least one reference from within its own body (the recursive call), plus any references from the let block's body expression or other declarations. Since a recursive function must also be used somewhere (otherwise it is dead code), the total reference count is at least 2, and the declaration is automatically preserved in the block.

## Structural Type Model for Later Compilation Stages

### Scope and Purpose

The structural type model (`StructuralType`) is a type representation for use in the later compilation stages — after canonicalization and type checking have completed. It complements the existing `TypeInference.InferredType` model, which uses nominal typing (types identified by module-qualified names) as appropriate for the earlier stages.

In the structural type model, types are identified entirely by their structure (shape) rather than by their declared name. For example, the Elm types `Maybe Int` and a user-defined `type Option a = Some a | None` with `Option Int` would be represented identically if they have the same tag structure — both resolve to a choice type with tags `Just`/`Some` carrying an `Int` and `Nothing`/`None` carrying nothing.

The structural type model is used in compilation stages where:
- Type names have already been validated by the Elm type checker.
- User-facing error messages about type mismatches have already been produced.
- What remains is structural information needed for optimization and code generation: "this value is a choice type with these tags" or "this is a record with these fields."

### What the Model Represents

The model supports **non-specific types as they appear in source declarations**. It is not limited to fully-monomorphized concrete types. The full set of type variants:

**Primitive types:** `Int`, `Float`, `String`, `Char`, `Bool` — leaf nodes with no children.

**Type variables** (`a`, `b`, etc.) — for polymorphic functions that have not yet been specialized. For example, `List.map : (a -> b) -> List a -> List b` retains its type variables in the structural model.

**Constrained type variables** — Elm's built-in type classes, preserved as constrained variable variants:
- `number` — can be `Int` or `Float`
- `comparable` — can be `Int`, `Float`, `Char`, `String`, `List comparable`, or tuples of comparables
- `appendable` — can be `String` or `List a`

**Composite types:**
- `FunctionType(argumentType, returnType)` — function types, curried as in Elm.
- `ListType(elementType)` — list type with element type.
- `TupleType(elementTypes)` — tuple type with a list of element types.

**Record types:**
- `ClosedRecord(fields)` — a record with a fixed, known set of fields. Fields are stored in a dictionary keyed by field name.
- `OpenRecord(fields, rowVariable)` — an extensible record (`{ a | name : String }`) with known fields plus a row variable representing additional unknown fields.

**Choice (union/sum) types:**
- `ChoiceType(tags)` — identified structurally by its set of tags and their field types. Tags are stored in a dictionary keyed by tag name.

**Self-reference:**
- `Self` — represents "the enclosing type being defined," used to model recursive types without circular references. See *Recursive Types* below.

### Use Cases

**Equality checking:** Two types are equal if and only if their structures are equal. No need to compare module-qualified type names, handle re-exports, or trace type aliases. This is particularly valuable for specialization, where checking "is this the same type?" is a core operation.

**Assignability / subsumption:** The model provides a "fits into" check (`IsAssignableTo`) that determines whether a specific type satisfies the constraints of a more general type. This is used to:
- Select the right specialization when multiple specialized forms of a function exist.
- Determine if a value can be used as a function argument at a given call site.

Examples of assignability:
- `Int` fits into `number` (a number-constrained variable).
- A closed record `{ name : String, age : Int }` fits into an open record `{ a | name : String }`.
- `List Int` fits into `List number`.
- A choice type with a subset of tags fits into one with more tags.

**Type variable substitution:** The model supports substituting type variables with concrete types. This is used when instantiating a polymorphic function for a specific call site — for example, replacing `number` with `Int` to create a specialized version of `add : number -> number -> number` as `add : Int -> Int -> Int`. The `Substitute` operation traverses the type tree, replacing variables according to a substitution dictionary and leaving everything else (including `Self` references) unchanged.

### Limits

The structural type model is designed for later compilation stages only. It does **not** replace the nominal `InferredType` model used in earlier stages (canonicalization, type checking). Specifically:

- **No user-facing error messages.** The structural model does not carry enough provenance information to produce messages like "Expected `Maybe Int` but got `Result String Int`." Diagnostic output would need optional annotations for developer ergonomics.
- **Tag names are significant.** Structural equality compares tag names, field names, and the full structure of field types. Two nominally distinct types with different tag names (e.g., `type Meters = Meters Float` and `type Seconds = Seconds Float`) are structurally **different** — the tag names `Meters` and `Seconds` do not match. Only types with identical tag names, field names, and field type structures are considered equal.
- **Type variables are name-significant.** Two `TypeVariable("a")` nodes are equal, but `TypeVariable("a")` and `TypeVariable("b")` are not. Determining whether they represent "the same type" requires alpha-equivalence reasoning in the calling code.

### Recursive Types

Recursive types pose a fundamental challenge for a structural type model. A naive structural expansion of a recursive type leads to an infinite tree:

```elm
type Tree a
    = Leaf
    | Branch (Tree a) a (Tree a)
```

Expanding `Tree Int` structurally would try to inline the recursive reference infinitely. We cannot construct a finite value to represent this.

**Why circular references are not an option:** In .NET, one could in principle use circular object references and implement equality checks that detect cycles. However, this approach fails in languages without mutation, where reference cycles are impossible to construct. We want a type model with straightforward value-equality semantics that could be directly represented in pure/immutable languages.

**Solution: the `Self` type reference.** The model introduces a `Self` variant that means "the enclosing type being defined." This is analogous to the μ (mu) binder in type theory (recursive types via fixpoints), but simpler — only one level of self-reference is needed because mutual recursion can be reduced to direct recursion.

The structural representation of `Tree Int` becomes:

```
ChoiceType({
    "Leaf"   → [],
    "Branch" → [Self, Int, Self]
})
```

This is a finite tree. `Self` is a leaf node — no cycles, no mutation, no names.

**Mutual recursion** is handled by inlining one type into the other, the same technique used for mutually recursive functions. For example:

```elm
type Forest a = Forest (List (Tree a))
type Tree a   = Node a (Forest a)
```

Inlining `Forest` into `Tree` yields `Tree a = Node a (List (Tree a))`, which is directly recursive and representable with a single `Self` reference. This works for any finite set of mutually recursive types.

**Nested recursive types** (a recursive type containing another recursive type as a field) work naturally because built-in types like `List` are primitives in the model — their own recursion is handled by `ListType` being a dedicated variant. The `Self` always refers to the user-defined type being constructed. User-defined recursive types that contain other user-defined recursive types are handled by inlining one into the other, reducing to a single `Self` reference.

**Equality with `Self`** works naturally: two structural types are equal if their trees are equal, where `Self` at corresponding positions matches `Self`. This is standard structural tree comparison with no cycle detection needed.

**Substitution preserves `Self`:** When substituting type variables in a recursive type (e.g., replacing `a` with `Int` in `List a = Cons a Self | Nil`), the `Self` node is left unchanged — it continues to refer to the enclosing type after substitution.

## Infrastructure for Testing and Verification

### Elm Syntax Interpreter for Verification of Lowering Transformations

Some optimizations in the Elm compiler are implemented as lowering transformations. That means these stages produce new Elm syntax.

Work on lowering stages in the Elm compiler turned out not to be trivial; we repeatedly observed defects introduced by these transformations. While such defects can also be detected by reviewing the generated Elm syntax, these bugs often only surfaced during execution after further compilation to Pine. To better support dynamic verification, we introduced an interpreter that runs on the Elm syntax model.

The Elm syntax interpreter offers:

+ Computing the values resulting from running a program, consistent with the semantics of the execution path for productive use.
+ Reporting applications of named functions or constructors to support inspection of runtime traces.
+ Detecting and reporting cases of infinite recursion with stack traces showing declaration names.

## Future Exploration

### Future Exploration - Monomorphizing Extensible Records

The following is an idea to further optimize programs with row polymorphism for throughput:

```
The Elm language supports records that can be extended with additional fields, often called “extensible records” or “row polymorphism”.
Instead of only using fixed shapes like `{ name : String, age : Int }`, Elm also allows types such as `{ r | name : String }`, which means “any record that has at least a name : String field, plus possibly more fields collected in r.” This feature lets a function say, in its type, “I need these fields, but I do not care what else is in the record.”

The compiler monomorphizes functions that accept extensible records. This monomorphization has the following implications:

+ When emitting a record access or record update, the concrete record fields are known from prior type inference, allowing the compiler to use an index to access the field value.
+ The compiler does not support entry points accepting extensible records. If the application author selects such a function as a compilation entry point, the compiler returns an error message.

```
