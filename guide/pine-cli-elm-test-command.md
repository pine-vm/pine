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
Test builds additionally substitute `elm-explorations/test` 2.2.0 and 2.2.1. Its Pine implementation
exposes `Test` and `Expect`, with `Test`, `describe`, `test` and `todo` in the `Test` module.
Fuzzing and `Test.Runner`/`Test.RunnerV2` are not implemented. Unsupported modules/APIs are errors,
not an invitation to load the upstream JavaScript implementation. `Basics` and `Debug` are provided
by Pine's native compiler implementations, not by replacement Elm source files.

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
