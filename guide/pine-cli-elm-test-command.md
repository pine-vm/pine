# Pine CLI Elm Test Command

Run Elm tests using the [`elm-explorations/test` package](https://github.com/elm-explorations/test)

## Dependency resolution

`pine elm test <project-directory>` resolves only that project's `elm.json`. Manifests in benchmarks,
examples, review projects and nested fixtures are independent projects, not additional constraints.
Only the declared source directories and `tests` enter the build. Declared parent directories such
as `../src` are supported without scanning unrelated parent projects.

Package manifests use Elm ranges; application manifests contain exact direct/indirect pins. Resolution
intersects constraints numerically, checks compiler compatibility and searches transitive dependencies,
backtracking when a newer candidate cannot work. Application pins are never silently upgraded, and missing
transitive pins are errors. Test builds include the root's test dependencies, but not dependencies' own
test dependencies. Ordinary builds exclude test-only dependencies. Exactly one version of each package
is selected in a compilation environment; separate projects may resolve independently.

Imports must come from the selected project's own sources or an exposed module in a direct dependency.
Packages can use their own private modules and their declared dependencies. Private modules with the same
name in different packages are isolated; unrelated package modules cannot leak into application imports.

Use `--offline` to prohibit registry/source HTTP requests. This uses Pine's metadata and source caches and
the Elm package cache under `ELM_HOME` (or the platform's default Elm home). Missing offline metadata is
reported as unavailable information, not proof that the dependency constraints conflict.

Use `--dependency-report <file>` to save the full resolution report on success or failure:

```console
pine elm test . --offline --dependency-report dependencies.json
```

Actual conflicts report the original declarations, declaring manifests, dependency chains and scopes,
and give actionable guidance. Unsupported replacement versions, compiler mismatches, missing application
pins, package cycles, registry failures and invalid module imports have distinct failure categories.
The JSON contains the exact selected graph, configuration, compiler identity, original root/metadata
text, candidate history, rejected branches, provider origins and underlying exception details.
Build preparation also records source fingerprints. Reports can contain local paths and project data;
inspect them before sharing.

## Terminal package substitutions

Builds accept an `ElmDependencyResolutionConfiguration` with explicit `ElmPackageSubstitution` entries.
A substitution replaces the *whole* upstream package and terminates version/metadata/source discovery
for it. Only the replacement's declared `ImplementationDependencies` are traversed, never the upstream
implementation's dependencies. If no configured version satisfies the constraints, resolution fails
with the supported versions and implementation identity; it never falls back to JavaScript kernels.

Each entry has a stable implementation identity, sources under package-relative `src` paths, exposed
modules, and a nonempty explicit set of compatible versions. For example, semantically equivalent
versions differing only in JavaScript performance optimizations can share one implementation:

```csharp
var replacement = ElmPackageSubstitution.Create(
    "author/kernel", "my-kernel-v1",
    versions: ["1.0.0", "1.0.1"],
    sources: replacementSources,
    exposedModules: ["Kernel"])
    with
    {
        ImplementationDependencies =
            ImmutableDictionary<string, string>.Empty.Add("elm/core", "1.0.0 <= v < 2.0.0"),
    };

var configuration = ElmTestRunner.DefaultResolutionConfiguration.Value with
{
    Substitutions = ElmTestRunner.DefaultResolutionConfiguration.Value.Substitutions.Add(replacement),
};
```

Different implementations of one package may cover disjoint version sets. Overlapping sets are rejected
as invalid configuration. Do not add a supported version solely because it has the same major version:
the replacement's semantics and supported API must actually match.

The bundled build defaults currently support `elm/core` 1.0.5, `elm/bytes` 1.0.8,
`elm/json` 1.1.3 and 1.1.4, `elm/parser` 1.1.0, `elm/time` 1.0.0 and `elm/url` 1.0.0.
Test builds additionally substitute `elm-explorations/test` 2.2.0 and 2.2.1 using the pinned 2.2.1
non-HTML Elm implementation, and `elm/random` 1.0.0 using its pure generator/seed APIs.
The test replacement exposes `Test`, `Expect`, `Fuzz`, `Test.Runner`, `Test.Runner.Failure` and
`Test.Distribution`. HTML testing, `Test.RunnerV2` and browser effects such as `Random.generate`
are not implemented. Unsupported modules/APIs are errors, not an invitation to load upstream
JavaScript. `Basics` and `Debug` are provided
by Pine's native compiler implementations, not by replacement Elm source files.

## Profiling and instrumentation

Use `elm test profile` to investigate hangs, unexpected work, or expensive computations.
Profiling executes directly on that command;

Profiling requires **exactly one runnable Elm test** after the optional filter. A project
with one test needs no filter.

```console
pine  elm  test  profile  .  --filter "tests/ExampleTests/group/one test"  --budget 1000000  --loop-budget 10000  --include-inputs  --include-locals  --sort Loops  --output profile.json
```

All ordinary test options are accepted, including source, filtering/listing, seed, fuzz count,
offline resolution, dependency reports, color and duration reporting. A selected test executes
on one worker. Diagnostic options include:

| Option | Meaning |
| --- | --- |
| `--budget` | Shortcut for both invocation and loop budgets; specific options override it. Shared with ordinary execution. |
| `--invocation-budget`, `--loop-budget` | Command-wide VM invocation and backward-jump budgets. |
| `--timeout` | Cooperative wall-clock deadline in seconds. Resolution observes cancellation; synchronous compilation/native leaves stop at their next cancellation boundary. |
| `--interval` | Status and stack sampling interval in seconds; `0` disables periodic sampling. |
| `--max-stack-depth`, `--stack-depth` | VM safety limit and maximum recorded stack frames, respectively. |
| `--top`, `--sort` | Terminal ranking size and sort metric: `Invocations`, `Instructions`, `Loops`. JSON always contains all recorded expressions. |
| `--include-inputs`, `--include-locals` | Query and record input paths/values and VM locals in sampled or stopped frames. Lazy values are described but never forced. |
| `--expressions`, `--no-stacks` | Show expression descriptions or hide terminal stack traces; neither discards JSON details. |
| `--no-precompiled-leaves`, `--no-invocation-cache` | Inspect pure Pine work or uncached work. |
| `--no-tail-recursion`, `--no-reduction` | Disable tail-call frame replacement or VM expression reduction. Compiler-generated backward jumps can still occur. |
| `--output` | JSON report path. Default: a unique file under the project's `elm-stuff/pine/test-profiles`. |

## Fuzz testing

`Test.fuzz`, `fuzz2`, `fuzz3` and `fuzzWith` run using the bundled Elm generation and shrinking
engine, including dependent fuzzers such as `Fuzz.andThen`. A failing generated example is simplified
by replaying and reducing its random-choice tape, which preserves the generator's constraints.
The output includes the final counterexample in Elm notation.

The controls match `elm-test-rs`:

```console
pine elm test . --seed 597517184 --fuzz 100
```

`--fuzz` defaults to 100 and accepts a positive unsigned 32-bit integer. `--seed` accepts an
unsigned 32-bit integer, including zero; omitting it selects a new seed once per invocation.
`Test.fuzzWith` can override the run count for one property. Distribution expectations may need
additional examples beyond the configured count.

Seed distribution happens before filtering or worker scheduling, so changing `--workers` or
isolating a property with `--filter` does not change its generated examples. Fuzz runs print a
reproduction command with the effective seed and count. Reproduction assumes unchanged source
ordering, package implementations and compiler; identical CLI flags do not promise identical
inputs between Pine and JavaScript runners.

`--list-tests` includes fuzz properties without running them. `Test.only` and `Test.skip` follow
upstream selection semantics and make a run incomplete, even when the selected properties pass.
Invalid generators and exhausted rejection filters produce explicit failures. VM evaluation errors,
including execution-budget exhaustion, fail the individual test; an interrupted engine is not
reported as having successfully completed shrinking.

The API accepts `ElmFuzzOptions` separately from dependency configuration. Completed runs retain
the effective settings and resolution report; each fuzz result includes the assigned seed state,
actual/requested counts, failing iteration, original and simplified inputs, random-choice tapes,
distribution metadata and any evaluation error. `completed.ToDebugJson()` exports these diagnostics.
Choice tapes are retained for failed examples; the full unsuccessful shrink-attempt history is not collected.

Test-root values are materialized with the stack-safe Pine VM. The float generator's fixed exponent
permutation uses an equivalent closed-form mapping rather than allocating and sorting a lookup table.

Bundled sources, licenses, upstream commit identities and documented adaptations are under
`implement/Pine.Core/Elm/Testing/elm-test` and `elm-random`. No network access or Node process is
needed to load or execute the test/fuzz replacements.

## Resolution and build APIs

`ElmDependencyResolver.ResolveAsync(sourceTree, manifestPath, configuration, provider)` returns
an `ElmDependencyResolutionReport` for successful resolutions and ordinary resolution failures.
The manifest path is explicit. `IElmPackageProvider` separates version listings, metadata and source
fetching, so rejected candidates need not download their source archives. An incomplete offline version
listing remains distinguishable from a complete registry listing.

`ElmResolvedBuildPreparation.PrepareAsync(...)` fetches and verifies selected sources, validates import
ownership and returns an `ElmResolvedBuild`. Pass `projectDirectory` when the supplied tree is rooted
at that filesystem project and declared parent source directories should be loaded. Without it, the
supplied tree must already contain those directories. Compile with
`ElmCompiler.CompileResolvedEnvironment(build)` so no implicit bundled sources can override the
configured implementations. Raw compiler APIs retain their historical bundled-source defaults.

Preparation failures throw `ElmDependencyResolutionException`; its `Report` retains the diagnostics
and resolution history. `report.ToJson()` exports solver information, and `build.ToDebugJson()` adds
roots, source fingerprints and compiler module identities. Builds retain unmodified `ProjectSources`
and `PackageSources` alongside the rewritten compiler inputs. The default compiler identity includes
the assembly version and module build identity, so native compiler changes also affect fingerprints.
`ElmTestRunner.CompileAndRunTests` accepts a
configuration/provider and an `onDependenciesResolved` callback to capture the report.

To reproduce a package selection after new releases, supply an explicit lock:

```csharp
var lockedConfiguration = configuration with
{
    LockedVersions = report.Packages.ToImmutableDictionary(
        item => item.Key, item => item.Value.Identity.Version),
};
```

Locks add exact constraints with their own provenance; they do not override incompatible declarations
or invent dependencies. Source hashes and compiler/replacement identities remain necessary when
comparing builds: version pins alone do not identify a custom implementation.

## Planned

From Simons PR at <https://github.com/elm-explorations/test/pull/260>

> This deprecates the `Test.Runner` module and adds `Test.RunnerV2`. This is a backwards-compatible change.
> 
> node-test-runner PR: https://github.com/rtfeldman/node-test-runner/pull/686. The idea is to _require_ a recent enough version of elm-explorations/test in node-test-runner (updating the package in projects is a no-brainer), but keep compatibility in this package, so we don’t leave elm-test-rs behind.
> 
> Changes in `Test.RunnerV2`:
> 
> - Removed seed distribution. All fuzz tests now run with the same seed. This means that moving a test, adding another fuzz test, or commenting out a fuzz test no longer can cause fuzz tests to behave differently, even though you passed a fixed seed. This also got rid of a lot of complexity.
> - Unit tests and fuzz tests are now returned separately. This allows a runner to more cleverly distribute tests across threads. For example, spread the fuzz tests on multiple threads, while running all unit tests single threaded. This also allowed for more precise types. Previously it looked like you could get a `DistributionReport` for unit tests, while in practice you can’t, for example.
> - Make it possible for runners to collect `Debug.log` from the run that caused a fuzz test to fail (and ignore `Debug.log` from exploratory runs).
> - Make it possible for runners to run fuzz tests with the “fuzzer ints” (`RandomRun`) from a previous failure, allowing for an instant reproduction of a fuzz test failure.
