# Elm syntax interpreter application caching

The syntax interpreter can reuse successful function applications without
compiling their bodies to Pine. The same runtime model can inspect whether
functions, dependencies, captures and higher-order arguments have reusable
identities.

## Default lazy declaration values

With the default configuration, the interpreter memoizes successful
**parameterless module declarations** on first invocation. Preparation does not
evaluate unused declarations. Subsequent references reuse the completed
`PineValueInProcess`, including lists, records, tagged values and closures, without
materializing it to a Pine encoding. Values are shared across evaluations/roots
using the same immutable prepared declaration snapshot. Parameterized functions
still execute for each application.

A declaration such as `convert = \n -> ...` caches the function value, not the
result of applying it to `n`: extra arguments are applied after looking up or
completing the declaration's value. Local let bindings retain their lexical
evaluation/binding behavior and are not shared across separate enclosing calls.

This in-memory cache is on by default for ordinary and counter-enabled entry
points. Set `EvaluationConfig.CacheNullaryValues = false` to bypass it:

```csharp
var uncached = ElmSyntaxInterpreter.EvaluationConfig.Default with
{
    CacheNullaryValues = false
};
```

The cache is weakly owned by the immutable program snapshot. Releasing the program
and its resolver allows the associated values to be collected; it is not a global
content-addressed or persisted cache. There are no per-value size or entry-count
limits: completed invoked declarations remain reusable for that snapshot's
lifetime. Source/dependency edits must produce a new snapshot, not mutate an
existing declaration dictionary. Internal resolver/operator indexes follow the
same immutability contract.

Only completed values are published into concurrent storage. Concurrent cold
misses may evaluate independently; pending entries are private to each evaluation,
so recursive declarations still reach cancellation/quota errors instead of
waiting on themselves or other evaluations. Runtime failures and unfinished
cancelled/quota-exhausted computations are not cached.

Source observers are replayed in their original order, including observations of
compound/function-bearing intermediate values. Cached replays share observation
segments rather than copying each event into every enclosing cached value.
Observer dictionaries and caller-supplied operator maps must be treated as
immutable and are reference-keyed cache scopes; different observer sets,
operator maps and builtin modes cannot reuse each other's values/callbacks.
Observer captures can retain intermediate values while their scope lives.
Replay still honors cancellation and propagates observer exceptions.

Custom/arbitrary/multicast resolvers are not assumed pure and retain uncached
behavior. Successful values using the default `Debug` facilities can be retained
(`Debug.log` is a pass-through in this interpreter); `Debug.todo` still fails and
its failure is not cached. An explicit application-cache policy disables
the implicit declaration cache, including when observers prevent application
caching, to avoid bypassing that policy.

## Storage and policy belong to the caller

Set `ElmSyntaxInterpreter.EvaluationConfig.ApplicationCache` to an
`ApplicationCaching` configuration. Storage implements
`ICache<ApplicationCacheInput, PineValue>`; interpretation only calls
`TryGetValue` and `Set`. It adds no lock around either operation.

An implementation backed by a plain dictionary is suitable for one execution
thread. `MemoryCache` supports shared concurrent use. `LayeredCache` and
`FileStoreCache` provide read-through and persistent tiers. A caller can also
implement a cascade that forwards only selected entries, independently of the
interpreter's eligibility policy. Retention, eviction, storage synchronization,
policy synchronization and notification synchronization remain caller concerns.
Concurrent misses may execute the same function more than once: the interface
does not require single-flight execution.

`ShouldCache` receives the function's caller-side qualified identity **before**
graph construction or storage lookup. Named and evaluated top-level closures use
the same resolved declaration identity. Local functions and lambdas use the
interpreter's diagnostic identities. Policy names never enter stored inputs.
Underapplication creates a function value, rather than executing a saturated
body, and does not attempt result caching.

For example, after preparing an environment containing `transform`:

```csharp
using Pine.Core.Caching;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.PineLanguage;

var storage =
    new MemoryCache<ElmSyntaxInterpreter.ApplicationCacheInput, PineValue, int>(0);

var config = ElmSyntaxInterpreter.EvaluationConfig.Default with
{
    ApplicationCache = new(
        storage,
        name => name == environment.ValueBindings["transform"])
};

var result = environment.Evaluate("transform 42", config);
```

The setting reaches ordinary expression evaluation, direct declared invocation,
evaluated closures, local functions, recursion and counter-enabled entry points.
Builtin higher-order operations implemented in Elm use these same dispatch
paths. A hit skips the body, so work counters describe the work actually executed,
not the work that an uncached invocation would have performed.

For fully saturated declarations, lookup also precedes trusted native
optimizations. A builtin alias whose syntactic arity differs from its target's
native arity is not cached as one combined overapplication: doing so could store
results under a key missing some arguments. Primitive facilities without an Elm
declaration are represented in dependency graphs, but are not separate
interpreter application-cache entry points.

With an explicit configuration, the interpreter's implicit nullary-result
memoization is disabled: it cannot bypass the caller's function policy. A null
`ApplicationCache` configuration uses the default lazy declaration cache described
above, unless `CacheNullaryValues` is false.
Syntax preparation and source lookup caches are separate from application results.

## Closed runtime identity

`ApplicationCacheModel` is a source-bound adapter over an immutable prepared
program snapshot. It resolves references without evaluating function bodies.
The produced `ApplicationCacheInput` contains an immutable Pine graph and a Debug
eligibility fact, **not** the program, package manifests, package names, package
versions, module ownership, paths, source positions, runtime closures or delegates.

The rooted graph represents:

+ Executable parameter patterns and prepared expressions.
+ Ordered dependency edges in place of source-qualified references.
+ Concrete Pine data, including nested data.
+ Functions embedded in lists, records or other composite arguments.
+ Captured environments, including cyclic local-function captures.
+ Previously collected arguments in partial applications.
+ Choice constructors, record constructors and record access chains.
+ Stable native facility roles and the interpreter semantic-format domain.

Lexical binding names, runtime constructor tags, field names, literal encodings,
argument order and executable precomputed field values remain semantic data.
Function declaration names and dependency module names are used only by the
adapter to find code and are replaced by graph slots. Unrelated program
declarations do not enter the graph. A changed dependency body changes identity;
renaming or reversioning its package does not.

Function and nominal binding slots preserve **relationships** within the rooted
input: two references to one binding must not become indistinguishable from
references to two separate bindings with identical bodies. The slots are
allocated by rooted traversal, not by source names or CLR addresses. Ordered
partition refinement then merges graph nodes with equal labels and equivalent
ordered children. Thus irrelevant allocation sharing does not change the final
graph, and cycles do not require infinite expansion.

The current interpreter's function equality observes every captured binding,
even one not read by the function body. Consequently all captured values
participate. Removing apparently unused captures would allow false hits for
functions comparing higher-order arguments. This does not change Elm/runtime
equality to use cache identity.

Application construction folds a closure's collected arguments into the supplied
arguments. The public model and the interpreter therefore produce the same input
for partial and fully supplied calls at a saturated-body boundary.

## Cryptographic addresses and persistent data

`DescribeFunction`, `DescribeValue` and both `DescribeApplication` overloads
produce inspectable closed inputs. `ComputeFunctionHash`, `ComputeValueHash`,
`ComputeApplicationHash` and `ApplicationCacheInput.ComputeHash` derive SHA-256
addresses from deterministic binary Pine encoding. Do not use CLR `GetHashCode`
or hash a rendered source name for file addresses.

```csharp
var model = new ElmSyntaxInterpreter.ApplicationCacheModel(environment.Program);
var hash = model.ComputeFunctionHash(environment.ValueBindings["transform"]);
var fileName = Convert.ToHexStringLower(hash.Span);
```

The model's `enableDefaultBuiltins` setting must match the resolver mode used for
execution. Interpreter-owned resolver compositions select this automatically.
Default-builtin and source-builtin modes have separate domains. The
`pine-elm-interpreter-application-v1` domain identifies this semantic/key format,
not a dependency package version; incompatible interpreter semantics require a
new domain.

The supplied `ApplicationResultCodec` losslessly encodes materializable Pine
results and explicitly rejects malformed, noncanonical or trailing encodings.
It composes with existing
file-store payload integrity checks:

```csharp
using Pine.Core.IO;
using System.Collections.Immutable;

var disk = new FileStoreCache<ElmSyntaxInterpreter.ApplicationCacheInput, PineValue>(
    new FileStoreFromSystemIOFile(cacheDirectory),
    ImmutableList.Create("elm-interpreter", "applications-v1"),
    input => input.ComputeHash(),
    ElmSyntaxInterpreter.ApplicationResultCodec);

var cascade =
    new LayeredCache<ElmSyntaxInterpreter.ApplicationCacheInput, PineValue>(storage, disk);
```

Use `cascade` as the configuration's cache. A new process can prepare an
equivalent program, produce the same address and read the result without
reconstructing the original program's source-bound objects. Tests exercise
actual separate processes and files, including renamed and reversioned packages.
The process-reuse scenario includes a function argument from the renamed package,
not only scalar arguments.

## Publication and explicit boundaries

Only completed successful **data** results are published. Errors, unfinished
recursive applications, cancellation and quota-exhausted applications are not
published. Completed child applications may remain cached if a later parent
fails. Storage failures, malformed stored data and identity-construction failures
surface explicitly; they are not converted into cache misses.

Function-bearing **arguments** are supported. Function-bearing **results** are
not stored: retaining runtime closures would persist source-bound state or bind a
hit to the wrong program. `FunctionResultExcluded` reports this boundary. Such
results are evaluated normally and may subsequently be applied.

For overapplication, the saturated body's result is processed before applying
leftover arguments. A cached data result does not bypass the error from applying
extra arguments to a nonfunction. The model can describe an entire application,
but interpreter entries identify each saturated-body stage, not one combined
overapplication.

Dependency graphs containing a `Debug` facility are excluded before storage
lookup, including Debug functions passed as higher-order arguments. This is
conservative: even an unreachable Debug branch excludes that input. Evaluations
with nonempty expression observers are excluded because a hit would otherwise
silently suppress their callbacks. Arbitrary resolver delegates and custom
overrides are rejected when shared application caching is configured: their
hidden semantics cannot be inferred from the program snapshot.

`OnEvent` reports `Hit`, `Miss`, `Stored`, `DebugExcluded`,
`ObserversExcluded` and `FunctionResultExcluded`. Notifications may include a
source function identity for diagnostics and a closed input for inspecting its
address; this observation is outside the stored model.

Selected tail calls coalesce pending publications into one continuation so
application caching does not add one retained continuation frame per tail call.
Pending keys still consume memory until a result completes, bounded by execution
quotas and the caller's selection policy. Cancellation is checked around graph
construction, lookup and publication; storage operations themselves follow the
storage implementation's cancellation behavior.
