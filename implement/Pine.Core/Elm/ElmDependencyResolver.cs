using Pine.Core.Elm.Elm019;
using Pine.Core.Files;
using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Elm;

/// <summary>Project-scoped, deterministic constraint solving with an inspectable history of every branch.</summary>
public static class ElmDependencyResolver
{
    /// <summary>
    /// Resolves only the selected manifest, returning complete diagnostics for ordinary failures.
    /// Invalid replacement configuration throws an argument exception; requested cancellation propagates.
    /// No source archives are downloaded through the provider's source API during constraint solving.
    /// </summary>
    public static async Task<ElmDependencyResolutionReport> ResolveAsync(
        FileTree sourceTree,
        IReadOnlyList<string> manifestPath,
        ElmDependencyResolutionConfiguration configuration,
        IElmPackageProvider provider,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(configuration);
        ArgumentNullException.ThrowIfNull(provider);
        ValidateConfiguration(configuration);

        var path = string.Join("/", manifestPath);
        var summary = Summarize(configuration);
        ElmJsonStructure? manifest = null;
        ImmutableArray<ElmDependencyRequirement> roots = [];
        var trace = new List<ElmResolutionTraceEntry>();
        var metadataCache = new Dictionary<ElmPackageIdentity, ElmPackageMetadata>();
        var versionCache = new Dictionary<string, ElmPackageVersionListing>(StringComparer.Ordinal);

        var substitutions =
            configuration.Substitutions.GroupBy(item => item.PackageName, StringComparer.Ordinal)
            .ToDictionary(group => group.Key, group => group.ToImmutableArray(), StringComparer.Ordinal);

        ElmDependencyResolutionReport Report(
            ImmutableArray<ElmDependencyRequirement> requirements,
            ImmutableDictionary<string, ElmResolvedPackage> packages,
            ImmutableArray<ElmResolutionFailure> failures) =>
            new(
                path,
                manifest,
                summary,
                requirements,
                packages,
                [.. trace],
                failures,
                failures.IsEmpty
                ?
                JsonFingerprint(
                    JsonSerializer.Serialize(
                        new
                        {
                            Configuration = summary,
                            Manifest = manifest,
                            Packages =
                            packages.OrderBy(kv => kv.Key, StringComparer.Ordinal).Select(
                                item => new
                                {
                                    item.Value.Identity,
                                    item.Value.Manifest,
                                    item.Value.SubstitutionImplementationId,
                                    item.Value.ExposedModules,
                                    Dependencies =
                                    item.Value.Dependencies.Select(
                                        dependency => new
                                        {
                                            dependency.PackageName,
                                            dependency.DeclaredVersion,
                                            dependency.Constraint,
                                            dependency.Scope,
                                        }),
                                }),
                        }))
                :
                null)
            {
                ManifestText =
                sourceTree.GetNodeAtPath(manifestPath) is FileTree.FileNode file
                ?
                Encoding.UTF8.GetString(file.Bytes.Span)
                :
                null,
            };

        void Record(
            string action,
            string name,
            ElmPackageVersion? candidate,
            string detail,
            string? origin,
            ImmutableArray<ElmDependencyRequirement> requirements,
            ImmutableDictionary<string, ElmResolvedPackage> selection,
            ElmResolutionFailure? failure = null) =>
            trace.Add(
                new(
                    trace.Count,
                    action,
                    name,
                    candidate,
                    detail,
                    origin,
                    requirements,
                    [
                    .. selection.Values.OrderBy(package => package.Identity.Name, StringComparer.Ordinal).Select(
                        package => package.Identity)
                    ],
                    failure));

        ElmResolutionFailure Failure(
            ElmResolutionFailureKind kind,
            string name,
            string message,
            ImmutableArray<ElmDependencyRequirement> requirements,
            ImmutableDictionary<string, ElmResolvedPackage> selection) =>
            new(kind, name, message, requirements, [.. selection.Values.Select(package => package.Identity)]);

        try
        {
            cancellationToken.ThrowIfCancellationRequested();
            manifest = ReadManifest(sourceTree, manifestPath);
            ValidateManifestShape(manifest, path);
            ValidateRootCompiler(manifest, configuration, path);
            roots = ReadRequirements(manifest, path, null, [], configuration.IncludeTests);
            ValidateRootDuplicates(roots, manifest.Type, path);
        }
        catch (Exception exception) when (exception is JsonException or FormatException or ArgumentException)
        {
            var kind =
                exception is ElmCompilerRequirementException
                ?
                ElmResolutionFailureKind.CompilerIncompatible
                :
                ElmResolutionFailureKind.InvalidManifest;

            var failure =
                Failure(kind, "", exception.Message, roots, [])
                with
                { ExceptionDetail = exception.ToString() };

            return Report(roots, [], [failure]);
        }

        var pinnedNames = roots.Select(requirement => requirement.PackageName).ToHashSet(StringComparer.Ordinal);

        async Task<SearchResult> Search(
            ImmutableArray<ElmDependencyRequirement> requirements,
            ImmutableDictionary<string, ElmResolvedPackage> selection)
        {
            cancellationToken.ThrowIfCancellationRequested();

            foreach (var name in requirements.Select(item => item.PackageName).Distinct(StringComparer.Ordinal).ToArray())
                if (configuration.LockedVersions.TryGetValue(name, out var lockedVersion) &&
                    !requirements.Any(item => item.PackageName == name && item.Scope == ElmDependencyScope.ResolutionLock))
                {
                    requirements =
                        requirements.Add(
                            new(
                                name,
                                lockedVersion.ToString(),
                                ElmPackageVersionConstraint.Exact(lockedVersion),
                                "resolution-lock",
                                null,
                                ElmDependencyScope.ResolutionLock,
                                []));
                }

            var constraints = new Dictionary<string, ElmPackageVersionConstraint>(StringComparer.Ordinal);

            foreach (var requirement in requirements)
            {
                if (constraints.TryGetValue(requirement.PackageName, out var existing))
                {
                    if (existing.Intersect(requirement.Constraint) is not { } intersection)
                    {
                        return
                            Reject(
                                ElmResolutionFailureKind.ConstraintConflict,
                                requirement.PackageName,
                                $"No version of '{requirement.PackageName}' satisfies all requirements: " +
                                $"'{existing}' and '{requirement.Constraint}' have an empty intersection.",
                                [.. requirements.Where(item => item.PackageName == requirement.PackageName)],
                                selection);
                    }

                    constraints[requirement.PackageName] = intersection;
                }
                else
                {
                    constraints.Add(requirement.PackageName, requirement.Constraint);
                }
            }

            foreach (var (name, selected) in selection)
            {
                if (!constraints[name].Contains(selected.Identity.Version))
                {
                    return
                        Reject(
                            ElmResolutionFailureKind.ConstraintConflict,
                            name,
                            $"Selected '{selected.Identity}' does not satisfy the accumulated requirement '{constraints[name]}'.",
                            [.. requirements.Where(item => item.PackageName == name)],
                            selection);
                }
            }

            var nextName =
                constraints.Keys.Where(name => !selection.ContainsKey(name))
                .OrderBy(name => constraints[name].IsExact ? 0 : 1)
                .ThenBy(name => name, StringComparer.Ordinal).FirstOrDefault();

            if (nextName is null)
            {
                if (FindCycle(selection) is { } cycle)
                {
                    return
                        Reject(
                            ElmResolutionFailureKind.DependencyCycle,
                            cycle[0],
                            "Cyclic package dependencies: " + string.Join(" -> ", cycle),
                            requirements,
                            selection);
                }

                Record(
                    "Solved",
                    "",
                    null,
                    "All selected versions satisfy the complete dependency graph.",
                    null,
                    requirements,
                    selection);

                return new(selection, requirements, []);
            }

            var relevant = requirements.Where(item => item.PackageName == nextName).ToImmutableArray();

            if (manifest!.Type == "application" && !pinnedNames.Contains(nextName))
            {
                return
                    Reject(
                        ElmResolutionFailureKind.UndeclaredDependency,
                        nextName,
                        $"Application '{path}' does not pin required dependency '{nextName}'. Add it to the application's dependency table.",
                        relevant,
                        selection);
            }

            var constraint = constraints[nextName];

            Record(
                "Discovering",
                nextName,
                null,
                $"Discovering candidates satisfying '{constraint}'.",
                null,
                requirements,
                selection);

            ElmPackageVersionListing listing;

            var isSubstituted = substitutions.TryGetValue(nextName, out var substitutionChoices);

            if (isSubstituted)
            {
                listing =
                    new(
                        [.. substitutionChoices.SelectMany(item => item.Versions)],
                        "substitution:" + string.Join(",", substitutionChoices.Select(item => item.ImplementationId)),
                        true);
            }
            else if (constraint.IsExact)
            {
                // Exact pins need no registry enumeration; their metadata proves that the version exists.
                listing = new([constraint.Lower], "exact requirement", true);
            }
            else
            {
                if (!versionCache.TryGetValue(nextName, out listing!))
                {
                    listing = await provider.GetVersionsAsync(nextName, cancellationToken);
                    versionCache.Add(nextName, listing);
                }
            }

            var candidates =
                listing.Versions.Distinct().Where(constraint.Contains)
                .OrderBy(version => version).ToImmutableArray();

            if (!configuration.PreferOldest)
                candidates = [.. candidates.Reverse()];

            Record(
                "Candidates",
                nextName,
                null,
                $"Requirement '{constraint}'; available: [{string.Join(", ", listing.Versions.OrderBy(version => version))}]; " +
                $"eligible: [{string.Join(", ", candidates)}]; listing complete: {listing.IsComplete}.",
                listing.Origin,
                requirements,
                selection);

            if (candidates.IsEmpty)
            {
                var kind =
                    isSubstituted
                    ?
                    ElmResolutionFailureKind.UnsupportedSubstitutionVersion
                    :
                    listing.IsComplete
                    ?
                    ElmResolutionFailureKind.NoMatchingVersion
                    :
                    ElmResolutionFailureKind.OfflineUnavailable;

                return
                    Reject(
                        kind,
                        nextName,
                        isSubstituted
                        ?
                        $"Substitution '{listing.Origin}' for '{nextName}' supports only " +
                        $"[{string.Join(", ", listing.Versions)}], none of which satisfies '{constraint}'. " +
                        "Upstream lookup is intentionally disabled for substituted packages."
                        :
                        $"No {(listing.IsComplete ? "published" : "locally known")} version of '{nextName}' satisfies '{constraint}'. " +
                        $"Available versions: [{string.Join(", ", listing.Versions)}]." +
                        (listing.IsComplete ? "" : " Retry online or populate the local package metadata cache."),
                        relevant,
                        selection);
            }

            var rejected = ImmutableArray.CreateBuilder<ElmResolutionFailure>();

            foreach (var version in candidates)
            {
                cancellationToken.ThrowIfCancellationRequested();
                var identity = new ElmPackageIdentity(nextName, version);

                var substitution =
                    isSubstituted ? substitutionChoices.Single(item => item.Versions.Contains(version)) : null;

                Record("Trying", nextName, version, $"Trying '{identity}'.", listing.Origin, requirements, selection);
                ElmResolvedPackage selected;

                if (substitution is not null)
                {
                    var dependencies =
                        substitution.ImplementationDependencies.OrderBy(item => item.Key, StringComparer.Ordinal)
                        .Select(
                            item => new ElmDependencyRequirement(
                                item.Key,
                                item.Value,
                                ParseRequirement(item.Value, "substitution:" + substitution.ImplementationId, item.Key, false),
                                "substitution:" + substitution.ImplementationId,
                                identity,
                                ElmDependencyScope.SubstitutionDependency,
                                relevant[0].DependencyPath.Add(identity))).ToImmutableArray();

                    selected =
                        new(
                            identity,
                            null,
                            "substitution:" + substitution.ImplementationId,
                            substitution.ImplementationId,
                            substitution.ExposedModules,
                            dependencies);

                    Record(
                        "Substituted",
                        nextName,
                        version,
                        $"Using '{substitution.ImplementationId}'; upstream metadata, sources and dependencies are not searched.",
                        listing.Origin,
                        requirements,
                        selection);
                }
                else
                {
                    if (!metadataCache.TryGetValue(identity, out var metadata))
                    {
                        metadata = await provider.GetMetadataAsync(identity, cancellationToken);
                        ValidatePackageMetadata(metadata, identity);
                        metadataCache.Add(identity, metadata);
                    }

                    Record(
                        "Metadata",
                        nextName,
                        version,
                        metadata.ManifestText ?? JsonSerializer.Serialize(metadata.Manifest),
                        metadata.Origin,
                        requirements,
                        selection);

                    var compilerConstraint =
                        ParseRequirement(metadata.Manifest.ElmVersion, metadata.Origin, "elm-version", false);

                    if (!compilerConstraint.Contains(configuration.CompilerVersion))
                    {
                        var compilerFailure =
                            Reject(
                                ElmResolutionFailureKind.CompilerIncompatible,
                                nextName,
                                $"'{identity}' requires Elm '{metadata.Manifest.ElmVersion}', but this build targets Elm '{configuration.CompilerVersion}'.",
                                relevant,
                                selection);

                        rejected.AddRange(compilerFailure.Failures);
                        continue;
                    }

                    var dependencies =
                        ReadRequirements(
                            metadata.Manifest,
                            metadata.Origin,
                            identity,
                            relevant[0].DependencyPath.Add(identity),
                            includeTests: false);

                    if (manifest!.Type == "application" &&
                        dependencies.FirstOrDefault(item => !pinnedNames.Contains(item.PackageName)) is { } missingPin)
                    {
                        var missing =
                            Reject(
                                ElmResolutionFailureKind.UndeclaredDependency,
                                missingPin.PackageName,
                                $"'{identity}' requires '{missingPin.PackageName}', but '{path}' does not pin it in dependencies" +
                                (configuration.IncludeTests ? " or test-dependencies" : "") +
                                ". The application dependency table is incomplete; regenerate it with an Elm dependency-management tool.",
                                [missingPin],
                                selection);

                        rejected.AddRange(missing.Failures);
                        continue;
                    }

                    selected =
                        new(
                            identity,
                            metadata.Manifest,
                            metadata.Origin,
                            null,
                            [.. metadata.Manifest.ExposedModules ?? []],
                            dependencies);
                }

                var branch =
                    await Search(requirements.AddRange(selected.Dependencies), selection.Add(nextName, selected));

                if (branch.Failures.IsEmpty)
                    return branch;

                rejected.AddRange(branch.Failures);

                Record(
                    "Backtracked",
                    nextName,
                    version,
                    $"'{identity}' cannot complete the graph; trying the next eligible version.",
                    selected.Origin,
                    branch.Requirements,
                    selection);
            }

            return new(selection, requirements, rejected.ToImmutable());
        }

        SearchResult Reject(
            ElmResolutionFailureKind kind,
            string name,
            string message,
            ImmutableArray<ElmDependencyRequirement> requirements,
            ImmutableDictionary<string, ElmResolvedPackage> selection)
        {
            var failure = Failure(kind, name, message, requirements, selection);

            Record(
                "Rejected",
                name,
                selection.GetValueOrDefault(name)?.Identity.Version,
                message,
                null,
                requirements,
                selection,
                failure);

            return new(selection, requirements, [failure]);
        }

        try
        {
            var result = await Search(roots, []);
            return Report(result.Requirements, result.Packages, result.Failures);
        }
        catch (ElmPackageProviderException exception)
        {
            var last = trace.LastOrDefault();

            var failure =
                new ElmResolutionFailure(
                    exception.Kind,
                    last?.PackageName ?? "",
                    $"{exception.Message}\nPackage data source: {exception.Origin}",
                    last?.Requirements ?? roots,
                    last?.Selection ?? [])
                {
                    ExceptionDetail = exception.ToString()
                };

            return Report(roots, [], [failure]);
        }
        catch (Exception exception) when (exception is JsonException or FormatException or ArgumentException)
        {
            var last = trace.LastOrDefault();

            var failure =
                new ElmResolutionFailure(
                    ElmResolutionFailureKind.InvalidManifest,
                    last?.PackageName ?? "",
                    exception.Message,
                    last?.Requirements ?? roots,
                    last?.Selection ?? [])
                {
                    ExceptionDetail = exception.ToString()
                };

            return Report(roots, [], [failure]);
        }
    }

    /// <summary>Reads the selected manifest, rejecting duplicate JSON properties without inspecting unrelated projects.</summary>
    public static ElmJsonStructure ReadManifest(FileTree tree, IReadOnlyList<string> path)
    {
        var displayPath = string.Join("/", path);

        if (tree.GetNodeAtPath(path) is not FileTree.FileNode node)
        {
            throw new JsonException(
                $"Elm manifest '{displayPath}' was not found or is not a file. Select a project containing elm.json.");
        }

        try
        {
            using var document = JsonDocument.Parse(node.Bytes);
            ValidateJsonProperties(document.RootElement, displayPath);

            return
                JsonSerializer.Deserialize<ElmJsonStructure>(node.Bytes.Span)
                ?? throw new JsonException("The manifest is JSON null.");
        }
        catch (JsonException exception)
        {
            throw new JsonException($"Cannot parse Elm manifest '{displayPath}': {exception.Message}", exception);
        }
    }

    private static void ValidateJsonProperties(JsonElement element, string path)
    {
        if (element.ValueKind == JsonValueKind.Object)
        {
            var names = new HashSet<string>(StringComparer.Ordinal);

            foreach (var property in element.EnumerateObject())
            {
                if (!names.Add(property.Name))
                {
                    throw new JsonException(
                        $"Duplicate JSON property '{property.Name}' at '{path}'. Remove the contradictory duplicate declaration.");
                }

                ValidateJsonProperties(property.Value, path + "/" + property.Name);
            }
        }
        else if (element.ValueKind == JsonValueKind.Array)
        {
            foreach (var item in element.EnumerateArray())
                ValidateJsonProperties(item, path);
        }
    }

    internal static ImmutableArray<ElmDependencyRequirement> ReadRequirements(
        ElmJsonStructure manifest,
        string manifestPath,
        ElmPackageIdentity? declaringPackage,
        ImmutableArray<ElmPackageIdentity> dependencyPath,
        bool includeTests)
    {
        var requirements = ImmutableArray.CreateBuilder<ElmDependencyRequirement>();

        void Add(IReadOnlyDictionary<string, string>? dependencies, ElmDependencyScope scope)
        {
            foreach (var (name, text) in (dependencies ?? ImmutableDictionary<string, string>.Empty)
                .OrderBy(kv => kv.Key, StringComparer.Ordinal))
            {
                ValidatePackageName(name);
                var constraint = ParseRequirement(text, manifestPath, name, manifest.Type == "application");

                if (manifest.Type == "package" && constraint.IsExact)
                {
                    throw new FormatException(
                        $"Package dependency '{name}' in '{manifestPath}' must be an Elm range, not exact version '{text}'.");
                }

                requirements.Add(
                    new(
                        name,
                        text,
                        constraint,
                        manifestPath,
                        declaringPackage,
                        scope,
                        dependencyPath));
            }
        }

        if (manifest.Type == "application")
        {
            Add(manifest.Dependencies.Direct, ElmDependencyScope.Direct);
            Add(manifest.Dependencies.Indirect, ElmDependencyScope.Indirect);

            if (includeTests)
            {
                Add(manifest.TestDependencies?.Direct, ElmDependencyScope.TestDirect);
                Add(manifest.TestDependencies?.Indirect, ElmDependencyScope.TestIndirect);
            }
        }
        else
        {
            Add(
                manifest.Dependencies.Flat,
                declaringPackage is null ? ElmDependencyScope.Direct : ElmDependencyScope.PackageDependency);

            if (includeTests)
                Add(manifest.TestDependencies?.Flat, ElmDependencyScope.TestDirect);
        }

        return requirements.ToImmutable();
    }

    private static ElmPackageVersionConstraint ParseRequirement(
        string text, string manifestPath, string name, bool exact)
    {
        if (string.IsNullOrEmpty(text))
        {
            throw new FormatException(
                $"Empty requirement for '{name}' in '{manifestPath}'. Expected an Elm version or range.");
        }

        try
        {
            var constraint = ElmPackageVersionConstraint.Parse(text);

            if (exact && !constraint.IsExact)
                throw new FormatException("Application dependencies must be exact versions, not ranges.");

            return constraint;
        }
        catch (FormatException exception)
        {
            throw new FormatException(
                $"Invalid requirement for '{name}' in '{manifestPath}': '{text}'. {exception.Message}",
                exception);
        }
    }

    private static void ValidateRootCompiler(
        ElmJsonStructure manifest, ElmDependencyResolutionConfiguration configuration, string path)
    {
        if (string.IsNullOrEmpty(manifest.ElmVersion))
            throw new FormatException($"Manifest '{path}' is missing 'elm-version'.");

        var constraint = ParseRequirement(manifest.ElmVersion, path, "elm-version", manifest.Type == "application");

        if (!constraint.Contains(configuration.CompilerVersion))
        {
            throw new ElmCompilerRequirementException(
                $"Project '{path}' requires Elm '{manifest.ElmVersion}', but this build targets Elm '{configuration.CompilerVersion}'. " +
                "Choose a compatible compiler target; package-version changes cannot fix this mismatch.");
        }
    }

    internal static void ValidateManifestShape(ElmJsonStructure manifest, string path)
    {
        if (manifest.Type is not ("application" or "package") || manifest.Dependencies is null)
        {
            throw new JsonException(
                $"Manifest '{path}' must declare type 'application' or 'package' and a dependencies object.");
        }

        if (manifest.Type == "application" &&
            (manifest.SourceDirectories is null || manifest.SourceDirectories.Count == 0))
            throw new JsonException($"Application manifest '{path}' requires a nonempty 'source-directories' array.");

        if (manifest.Type == "application" &&
            manifest.SourceDirectories!.Any(
                directory =>
                string.IsNullOrWhiteSpace(directory) || Path.IsPathRooted(directory) || directory.Contains('\0')))
        {
            throw new JsonException(
                $"Application manifest '{path}' requires nonempty relative paths in 'source-directories', not null or absolute paths.");
        }

        if (manifest.Type == "package")
        {
            if (string.IsNullOrEmpty(manifest.Name) || string.IsNullOrEmpty(manifest.Version))
                throw new JsonException($"Package manifest '{path}' must declare its 'name' and 'version'.");

            ValidatePackageName(manifest.Name);
            _ = ElmPackageVersion.Parse(manifest.Version);

            if (manifest.ExposedModules is null || manifest.ExposedModules.Any(string.IsNullOrWhiteSpace))
            {
                throw new JsonException(
                    $"Package manifest '{path}' requires 'exposed-modules' containing nonempty module-name strings.");
            }
        }

        foreach (var dependencies in new[] { manifest.Dependencies, manifest.TestDependencies }.Where(item => item is not null))
        {
            if (manifest.Type == "application" &&
                (dependencies.Direct is null || dependencies.Indirect is null || dependencies.Flat?.Count > 0))
            {
                throw new JsonException(
                    $"Application manifest '{path}' requires direct/indirect dependency objects, not a flat package table.");
            }

            if (manifest.Type == "package" && (dependencies.Direct is not null || dependencies.Indirect is not null))
            {
                throw new JsonException(
                    $"Package manifest '{path}' requires flat dependency ranges, not application direct/indirect objects.");
            }
        }
    }

    private static void ValidateRootDuplicates(
        ImmutableArray<ElmDependencyRequirement> roots, string type, string path)
    {
        foreach (var group in roots.GroupBy(requirement => requirement.PackageName))
        {
            var duplicates = group.ToArray();

            if (duplicates.Length <= 1)
                continue;

            if (type == "application" && duplicates.Length == 2 &&
                duplicates.Any(item => item.Scope == ElmDependencyScope.Indirect) &&
                duplicates.Any(item => item.Scope == ElmDependencyScope.TestDirect))
                continue;

            throw new JsonException(
                $"'{group.Key}' is declared in multiple dependency sections in '{path}'. " +
                "Only an application indirect dependency also used as a test-direct dependency may be repeated.");
        }
    }

    private static void ValidatePackageMetadata(ElmPackageMetadata metadata, ElmPackageIdentity expected)
    {
        ValidateManifestShape(metadata.Manifest, metadata.Origin);

        if (metadata.Identity != expected || metadata.Manifest.Type != "package" ||
            metadata.Manifest.Name != expected.Name ||
            ElmPackageVersion.Parse(metadata.Manifest.Version) != expected.Version)
        {
            throw new JsonException(
                $"Metadata from '{metadata.Origin}' does not describe requested '{expected}': " +
                $"declares '{metadata.Manifest.Name}@{metadata.Manifest.Version}' ({metadata.Manifest.Type}).");
        }
    }

    internal static void ValidatePackageName(string name)
    {
        var segments = name.Split('/');

        if (segments.Length != 2 ||
            segments.Any(
                segment => segment.Length == 0 ||
                    segment is "." or ".." ||
                    segment.Any(character => !char.IsAsciiLetterOrDigit(character) && character != '-')))
            throw new FormatException($"Invalid Elm package name '{name}'. Expected 'author/package' without path traversal.");
    }

    private static void ValidateConfiguration(ElmDependencyResolutionConfiguration configuration)
    {
        if (string.IsNullOrWhiteSpace(configuration.CompilerImplementationId) ||
            configuration.CompilerVersion.Major < 0 ||
            configuration.CompilerVersion.Minor < 0 ||
            configuration.CompilerVersion.Patch < 0)
        {
            throw new ArgumentException(
                "A resolution configuration requires a compiler implementation identity and a non-negative compiler version.");
        }

        foreach (var (name, version) in configuration.LockedVersions)
        {
            ValidatePackageName(name);

            if (version.Major < 0 || version.Minor < 0 || version.Patch < 0)
                throw new ArgumentException($"Resolution lock for '{name}' contains an invalid negative version.");
        }

        if (configuration.Substitutions.IsDefault)
            throw new ArgumentException("Substitutions must be an initialized array (use [] for no substitutions).");

        foreach (var group in configuration.Substitutions.GroupBy(item => item.PackageName))
        {
            ValidatePackageName(group.Key);
            var versions = new HashSet<ElmPackageVersion>();

            foreach (var item in group)
            {
                if (string.IsNullOrWhiteSpace(item.ImplementationId) || item.Versions.IsDefaultOrEmpty ||
                    item.ExposedModules.IsDefault || item.Sources is null ||
                    item.Versions.Any(version => version.Major < 0 || version.Minor < 0 || version.Patch < 0))
                {
                    throw new ArgumentException(
                        $"Substitution '{group.Key}' needs an implementation identity, an explicit nonempty version set, sources and exposed modules.");
                }

                foreach (var version in item.Versions)
                    if (!versions.Add(version))
                    {
                        throw new ArgumentException(
                            $"Version '{group.Key}@{version}' is covered by multiple substitution entries. Version sets must be disjoint.");
                    }

                foreach (var dependency in item.ImplementationDependencies)
                {
                    ValidatePackageName(dependency.Key);
                    _ = ElmPackageVersionConstraint.Parse(dependency.Value);
                }
            }
        }
    }

    private static string[]? FindCycle(ImmutableDictionary<string, ElmResolvedPackage> packages)
    {
        var visited = new HashSet<string>(StringComparer.Ordinal);
        var stack = new List<string>();

        string[]? Visit(string name)
        {
            if (stack.Contains(name, StringComparer.Ordinal))
                return [.. stack.SkipWhile(item => item != name), name];

            if (!visited.Add(name))
                return null;

            stack.Add(name);

            foreach (var dependency in packages[name].Dependencies)
                if (Visit(dependency.PackageName) is { } cycle)
                    return cycle;

            stack.RemoveAt(stack.Count - 1);
            return null;
        }

        foreach (var name in packages.Keys.Order(StringComparer.Ordinal))
            if (Visit(name) is { } cycle)
                return cycle;

        return null;
    }

    internal static string Fingerprint(string content) =>
        Convert.ToHexStringLower(SHA256.HashData(Encoding.UTF8.GetBytes(content)));

    internal static string JsonFingerprint(string json)
    {
        using var document = JsonDocument.Parse(json);
        using var stream = new MemoryStream();

        using (var writer = new Utf8JsonWriter(stream))
        {
            void Write(JsonElement element)
            {
                if (element.ValueKind == JsonValueKind.Object)
                {
                    writer.WriteStartObject();

                    foreach (var property in element.EnumerateObject().OrderBy(property => property.Name, StringComparer.Ordinal))
                    {
                        writer.WritePropertyName(property.Name);
                        Write(property.Value);
                    }

                    writer.WriteEndObject();
                }
                else if (element.ValueKind == JsonValueKind.Array)
                {
                    writer.WriteStartArray();

                    foreach (var item in element.EnumerateArray())
                        Write(item);

                    writer.WriteEndArray();
                }
                else
                {
                    element.WriteTo(writer);
                }
            }

            Write(document.RootElement);
        }

        return Convert.ToHexStringLower(SHA256.HashData(stream.ToArray()));
    }

    internal static string SourceFingerprint(FileTree tree)
    {
        using var hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256);
        Span<byte> length = stackalloc byte[4];

        foreach (var file in tree.EnumerateFilesTransitive().OrderBy(file => string.Join("/", file.path), StringComparer.Ordinal))
        {
            var path = Encoding.UTF8.GetBytes(string.Join("/", file.path));
            BinaryPrimitives.WriteInt32LittleEndian(length, path.Length);
            hash.AppendData(length);
            hash.AppendData(path);
            BinaryPrimitives.WriteInt32LittleEndian(length, file.fileContent.Length);
            hash.AppendData(length);
            hash.AppendData(file.fileContent.Span);
        }

        return Convert.ToHexStringLower(hash.GetHashAndReset());
    }

    private static ElmDependencyResolutionConfigurationSummary Summarize(
        ElmDependencyResolutionConfiguration configuration) =>
        new(
            configuration.CompilerVersion,
            configuration.CompilerImplementationId,
            configuration.IncludeTests,
            configuration.Offline,
            configuration.PreferOldest,
            [
            .. configuration.Substitutions.OrderBy(item => item.PackageName, StringComparer.Ordinal)
            .ThenBy(item => item.ImplementationId, StringComparer.Ordinal).Select(
                item => new ElmSubstitutionSummary(
                    item.PackageName,
                    item.ImplementationId,
                    [.. item.Versions.OrderBy(version => version)],
                    item.ExposedModules,
                    SourceFingerprint(item.Sources),
                    item.ImplementationDependencies))
            ],
            configuration.LockedVersions);

    private sealed record SearchResult(
        ImmutableDictionary<string, ElmResolvedPackage> Packages,
        ImmutableArray<ElmDependencyRequirement> Requirements,
        ImmutableArray<ElmResolutionFailure> Failures);

    private sealed class ElmCompilerRequirementException(string message) : ArgumentException(message);
}
