using Pine.Core.Elm;
using Pine.Core.Elm.Elm019;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Elm019;

public sealed record ElmMakeSnapshotInputs(
    IReadOnlyList<string> ManifestPath,
    IReadOnlyDictionary<ElmPackageIdentity, FileTree> Packages);

/// <summary>Checked-in archives are authoritative. Only explicit maintenance may acquire missing archives.</summary>
public sealed class ElmMakePackageSnapshots(string testDataDirectory)
{
    public string PackagesDirectory { get; } = Path.Combine(testDataDirectory, "packages");

    public string RegistryPath { get; } = Path.Combine(testDataDirectory, "elm-reference", "registry.dat");

    public string ArchivePath(ElmPackageIdentity identity) =>
        Path.Combine(
            PackagesDirectory,
            "github.com",
            identity.Name.Split('/')[0],
            identity.Name.Split('/')[1] + "@" + identity.Version + ".zip");

    public async Task<ElmMakeSnapshotInputs> PrepareAsync(
        ElmMakeScenario scenario,
        ElmMakeExpectation expectation,
        bool acquireMissing = false,
        CancellationToken cancellationToken = default)
    {
        var sourcePath = expectation.SourcePath.Split('/');

        if (expectation.Kind is ElmMakeExpectationKind.Output &&
            scenario.AppFiles.GetNodeAtPath(sourcePath) is not FileTree.FileNode)
        {
            throw new InvalidDataException($"Scenario '{scenario.Name}' is missing entry point '{expectation.SourcePath}'.");
        }

        var manifestPath = FindManifestPath(scenario.AppFiles, sourcePath);

        if (manifestPath is null)
        {
            if (expectation.Kind is ElmMakeExpectationKind.ErrorsOmittingDetails)
                return new(["elm.json"], new Dictionary<ElmPackageIdentity, FileTree>());

            throw new InvalidDataException($"No governing elm.json for '{scenario.Name}/{expectation.SourcePath}'.");
        }

        ElmJsonStructure manifest;

        try
        {
            manifest = ElmDependencyResolver.ReadManifest(scenario.AppFiles, manifestPath);
        }
        catch (JsonException) when (expectation.Kind is ElmMakeExpectationKind.ErrorsOmittingDetails)
        {
            // Invalid compiler input must reach the compiler, unlike a broken package fixture.
            return new(manifestPath, new Dictionary<ElmPackageIdentity, FileTree>());
        }

        if (manifest.Type is not "application" && expectation.Kind is ElmMakeExpectationKind.Output)
        {
            throw new InvalidDataException(
                "ECMAScript snapshot scenarios currently require an application elm.json with exact dependency pins.");
        }

        var loaded = new Dictionary<ElmPackageIdentity, FileTree>();

        var provider =
            new ElmDelegatePackageProvider(
                (name, version) =>
                {
                    var identity = new ElmPackageIdentity(name, ElmPackageVersion.Parse(version));
                    var files = LoadAsync(identity, acquireMissing, cancellationToken).GetAwaiter().GetResult();
                    loaded[identity] = FileTree.FromSetOfFilesWithStringPath(files);
                    return files;
                });

        var resolution =
            await ElmDependencyResolver.ResolveAsync(
                scenario.AppFiles,
                manifestPath,
                new() { IncludeTests = true, Offline = !acquireMissing, CompilerImplementationId = "elm-0.19.1-reference" },
                provider,
                cancellationToken);

        if (!resolution.Succeeded)
        {
            if (expectation.Kind is ElmMakeExpectationKind.ErrorsOmittingDetails && loaded.Count is 0 &&
                resolution.Failures.All(
                    failure =>
                    failure.Kind is ElmResolutionFailureKind.InvalidManifest or ElmResolutionFailureKind.CompilerIncompatible &&
                    failure.PackageName.Length is 0))
            {
                return new(manifestPath, loaded);
            }

            if (expectation.Kind is ElmMakeExpectationKind.ErrorsOmittingDetails && manifest.Type is "application" &&
                resolution.Failures.All(
                    failure => failure.Kind is
                    ElmResolutionFailureKind.ConstraintConflict or ElmResolutionFailureKind.UndeclaredDependency or
                    ElmResolutionFailureKind.DependencyCycle or ElmResolutionFailureKind.NoMatchingVersion))
            {
                // Let the compiler diagnose an invalid dependency graph, but still verify every explicit pin.
                var tables =
                    new[]
                    {
                        manifest.Dependencies.Direct, manifest.Dependencies.Indirect,
                        manifest.TestDependencies?.Direct, manifest.TestDependencies?.Indirect,
                    };

                foreach (var (name, version) in tables.OfType<IReadOnlyDictionary<string, string>>().SelectMany(table => table))
                {
                    var identity = new ElmPackageIdentity(name, ElmPackageVersion.Parse(version));

                    if (!loaded.ContainsKey(identity))
                    {
                        var files = await LoadAsync(identity, acquireMissing, cancellationToken);
                        loaded.Add(identity, FileTree.FromSetOfFilesWithStringPath(files));
                    }
                }

                return new(manifestPath, loaded);
            }

            throw new InvalidDataException(
                $"Dependencies for '{scenario.Name}/{string.Join("/", manifestPath)}' could not be loaded:\n" +
                string.Join("\n", resolution.Failures.Select(failure => failure.Message)) +
                "\nPackage snapshot paths: " + string.Join(", ", loaded.Keys.Select(ArchivePath)));
        }

        return new(manifestPath, loaded);
    }

    private static IReadOnlyList<string>? FindManifestPath(FileTree files, IReadOnlyList<string> sourcePath)
    {
        for (var count = sourcePath.Count - 1; count >= 0; --count)
        {
            string[] candidate = [.. sourcePath.Take(count), "elm.json"];

            if (files.GetNodeAtPath(candidate) is not null)
                return candidate;
        }

        return null;
    }

    public async Task<IReadOnlyList<ElmPackageIdentity>> RequiredPackagesAsync(
        IReadOnlyList<ElmMakeScenario> scenarios,
        bool acquireMissing = false,
        CancellationToken cancellationToken = default)
    {
        var required = new HashSet<ElmPackageIdentity>();

        foreach (var scenario in scenarios)
        {
            if (scenario.Expectations.Count is 0)
                throw new InvalidDataException("Scenario '" + scenario.Name + "' has zero expectation files.");

            foreach (var expectation in scenario.Expectations.DistinctBy(item => (item.SourcePath, item.Kind)))
            {
                var inputs = await PrepareAsync(scenario, expectation, acquireMissing, cancellationToken);
                required.UnionWith(inputs.Packages.Keys);
            }
        }

        return [.. required.OrderBy(item => item.Name, StringComparer.Ordinal).ThenBy(item => item.Version)];
    }

    public async Task UpdateAsync(
        IReadOnlyList<ElmMakeScenario> scenarios,
        TextWriter output,
        CancellationToken cancellationToken = default)
    {
        var before = Inventory();
        var registry = File.Exists(RegistryPath) ? ReadRegistry() : [];
        var required = await RequiredPackagesAsync(scenarios, acquireMissing: true, cancellationToken);
        var updated = registry.Union(required).ToArray();
        var bytes = Elm019Registry.Write(updated);

        if (!File.Exists(RegistryPath) || !File.ReadAllBytes(RegistryPath).SequenceEqual(bytes))
        {
            Directory.CreateDirectory(Path.GetDirectoryName(RegistryPath)!);
            var temporary = RegistryPath + "." + Guid.NewGuid().ToString("N") + ".tmp";

            try
            {
                await File.WriteAllBytesAsync(temporary, bytes, cancellationToken);
                File.Move(temporary, RegistryPath, overwrite: true);
            }
            finally
            {
                if (File.Exists(temporary))
                    File.Delete(temporary);
            }
        }

        await output.WriteLineAsync(
            $"Required package snapshots: {required.Count}; acquired: {Inventory().Count - before.Count}.");

        await output.WriteLineAsync(
            $"Registry: {updated.Length} versions across {updated.Select(item => item.Name).Distinct().Count()} packages.");

        foreach (var unused in Inventory().Except(required))
            await output.WriteLineAsync("Unused package snapshot (retained): " + unused);
    }

    public IReadOnlyList<ElmPackageIdentity> ReadRegistry()
    {
        if (!File.Exists(RegistryPath))
        {
            throw new FileNotFoundException(
                "Missing Elm 0.19.1 registry snapshot. Run update-packages: " + RegistryPath,
                RegistryPath);
        }

        try
        {
            return Elm019Registry.Read(File.ReadAllBytes(RegistryPath));
        }
        catch (Exception exception) when (exception is IOException or InvalidDataException or ArgumentException or FormatException)
        {
            throw new InvalidDataException(
                "Invalid Elm 0.19.1 registry snapshot '" + RegistryPath + "': " + exception.Message,
                exception);
        }
    }

    public IReadOnlyList<ElmPackageIdentity> Inventory()
    {
        if (!Directory.Exists(PackagesDirectory))
            return [];

        return
            [
            .. Directory.EnumerateFiles(PackagesDirectory, "*.zip", SearchOption.AllDirectories)
            .Order(StringComparer.Ordinal)
            .Select(
                path =>
                {
                    var relative = Path.GetRelativePath(PackagesDirectory, path).Replace('\\', '/');
                    var separator = relative.LastIndexOf('@');

                    if (!relative.StartsWith("github.com/", StringComparison.Ordinal) || separator < 0)
                        throw new InvalidDataException("Invalid package snapshot archive path: " + path);

                    var name = relative["github.com/".Length..separator];
                    ElmDependencyResolver.ValidatePackageName(name);
                    return new ElmPackageIdentity(name, ElmPackageVersion.Parse(relative[(separator + 1)..^4]));
                })
            ];
    }

    private async Task<IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadAsync(
        ElmPackageIdentity identity,
        bool acquireMissing,
        CancellationToken cancellationToken)
    {
        var path = ArchivePath(identity);

        try
        {
            if (!File.Exists(path))
            {
                if (!acquireMissing)
                    throw new FileNotFoundException("Missing package snapshot.", path);

                await ElmPackageSource.LoadElmPackageAsync(
                    identity.Name,
                    identity.Version.ToString(),
                    [
                    PackagesDirectory
                    ],
                    cancellationToken: cancellationToken);
            }

            var files =
                ElmPackageSource.LoadElmPackageFromZipBytes(await File.ReadAllBytesAsync(path, cancellationToken));

            foreach (var file in files)
                ElmMakeEcmascriptScenarios.ValidatePath(file.Key);

            var tree = FileTree.FromSetOfFilesWithStringPath(files);
            var manifest = ElmDependencyResolver.ReadManifest(tree, ["elm.json"]);

            if (manifest.Type != "package" || manifest.Name != identity.Name ||
                manifest.Version != identity.Version.ToString())
            {
                throw new InvalidDataException("Archive manifest does not match package identity " + identity);
            }

            if (!files.Keys.Any(file => file.Count > 1 && file[0] == "src" && file[^1].EndsWith(".elm", StringComparison.Ordinal)))
                throw new InvalidDataException("Archive contains no Elm sources under src.");

            return files;
        }
        catch (Exception exception) when (exception is IOException or InvalidDataException or ArgumentException or FormatException or JsonException or System.Net.Http.HttpRequestException)
        {
            throw new InvalidDataException(
                $"Cannot load package snapshot '{identity}' at '{path}': {exception.Message}",
                exception);
        }
    }
}
