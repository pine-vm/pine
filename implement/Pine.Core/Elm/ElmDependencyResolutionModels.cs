using Pine.Core.Elm.Elm019;
using Pine.Core.Files;
using System;
using System.Collections.Immutable;
using System.Linq;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Elm;

/// <summary>The manifest section or build policy responsible for a requirement.</summary>
public enum ElmDependencyScope
{
    /// <summary>A root production dependency available to application imports.</summary>
    Direct,
    /// <summary>An application pin for a transitive production dependency.</summary>
    Indirect,
    /// <summary>A root dependency available only in test builds.</summary>
    TestDirect,
    /// <summary>An application pin for a transitive test dependency.</summary>
    TestIndirect,
    /// <summary>A selected package's production dependency.</summary>
    PackageDependency,
    /// <summary>A replacement implementation's own dependency, not an upstream declaration.</summary>
    SubstitutionDependency,
    /// <summary>An explicit pin supplied to reproduce a previous resolution.</summary>
    ResolutionLock,
}

/// <summary>Distinguishes unsatisfiable constraints from missing information, invalid inputs and unsupported implementations.</summary>
public enum ElmResolutionFailureKind
{
    /// <summary>A manifest or selected archive does not describe a valid Elm package/project.</summary>
    InvalidManifest,
    /// <summary>The intersection of declarations for a package is empty.</summary>
    ConstraintConflict,
    /// <summary>No published candidate satisfies the requirements.</summary>
    NoMatchingVersion,
    /// <summary>The authoritative replacement table has no supported candidate.</summary>
    UnsupportedSubstitutionVersion,
    /// <summary>The selected compiler version does not satisfy an elm-version requirement.</summary>
    CompilerIncompatible,
    /// <summary>The local caches cannot prove or reproduce a resolution without network access.</summary>
    OfflineUnavailable,
    /// <summary>Registry, source acquisition or cache access failed.</summary>
    ProviderFailure,
    /// <summary>Selected packages form a dependency cycle.</summary>
    DependencyCycle,
    /// <summary>A transitive dependency is missing from an application's exact pin tables.</summary>
    UndeclaredDependency,
    /// <summary>An import is missing, private, ambiguous or outside the declared dependency scope.</summary>
    InvalidModuleImport,
}

/// <summary>A package name and the exact upstream version selected for it.</summary>
public sealed record ElmPackageIdentity(string Name, ElmPackageVersion Version)
{
    /// <inheritdoc/>
    public override string ToString() => $"{Name}@{Version}";
}

/// <summary>The unmodified declaration, its parsed meaning, and its path through the dependency graph.</summary>
public sealed record ElmDependencyRequirement(
    string PackageName,
    string DeclaredVersion,
    ElmPackageVersionConstraint Constraint,
    string ManifestPath,
    ElmPackageIdentity? DeclaringPackage,
    ElmDependencyScope Scope,
    ImmutableArray<ElmPackageIdentity> DependencyPath);

/// <summary>
/// Replaces an entire upstream package. Versions are aliases for this implementation, not additional
/// upstream packages to search. Sources use package-relative paths beginning with "src".
/// </summary>
public sealed record ElmPackageSubstitution(
    string PackageName,
    string ImplementationId,
    ImmutableArray<ElmPackageVersion> Versions,
    FileTree Sources,
    ImmutableArray<string> ExposedModules)
{
    /// <summary>Dependencies of the replacement itself, never those of the upstream package.</summary>
    public ImmutableDictionary<string, string> ImplementationDependencies { get; init; } =
        [];

    /// <summary>Creates an authoritative replacement from explicit upstream version strings.</summary>
    public static ElmPackageSubstitution Create(
        string packageName,
        string implementationId,
        string[] versions,
        FileTree sources,
        string[] exposedModules) =>
        new(packageName, implementationId, [.. versions.Select(ElmPackageVersion.Parse)], sources, [.. exposedModules]);
}

/// <summary>
/// Build-specific resolution policy. The substitution table is authoritative: an unsupported version
/// fails instead of falling through to JavaScript/kernel sources that this build cannot implement.
/// </summary>
public sealed record ElmDependencyResolutionConfiguration
{
    /// <summary>The Elm language/compiler version against which all manifests are checked.</summary>
    public ElmPackageVersion CompilerVersion { get; init; } = ElmPackageVersion.Parse("0.19.1");

    /// <summary>Identifies the compiler implementation for reproducibility and fingerprints.</summary>
    public string CompilerImplementationId { get; init; } =
        "pine-elm-" + typeof(ElmDependencyResolutionConfiguration).Assembly.GetName().Version + "-" +
        typeof(ElmDependencyResolutionConfiguration).Assembly.ManifestModule.ModuleVersionId.ToString("N");

    /// <summary>Includes only the root project's test requirements and test sources.</summary>
    public bool IncludeTests { get; init; }

    /// <summary>Disables network access in the default registry provider; custom providers must honor this policy themselves.</summary>
    public bool Offline { get; init; }

    /// <summary>Searches compatible versions oldest-first instead of newest-first.</summary>
    public bool PreferOldest { get; init; }

    /// <summary>Whole-package replacements; entries for the same package must have disjoint version sets.</summary>
    public ImmutableArray<ElmPackageSubstitution> Substitutions { get; init; } = [];

    /// <summary>Previously selected exact versions, supplied explicitly to reproduce a package build.</summary>
    public ImmutableDictionary<string, ElmPackageVersion> LockedVersions { get; init; } =
        [];
}

/// <summary>Known candidates and their origin. Incomplete listings cannot prove that no compatible release exists.</summary>
public sealed record ElmPackageVersionListing(
    ImmutableArray<ElmPackageVersion> Versions,
    string Origin,
    bool IsComplete);

/// <summary>The requested exact identity, parsed upstream manifest and metadata origin.</summary>
public sealed record ElmPackageMetadata(
    ElmPackageIdentity Identity,
    ElmJsonStructure Manifest,
    string Origin)
{
    /// <summary>Original metadata, including fields unknown to this version of Pine.</summary>
    public string? ManifestText { get; init; }
}

/// <summary>Package discovery is separate from fetching the selected source archives.</summary>
public interface IElmPackageProvider
{
    /// <summary>Lists known published candidates, declaring whether the list is complete.</summary>
    Task<ElmPackageVersionListing> GetVersionsAsync(string packageName, CancellationToken cancellationToken);

    /// <summary>Loads metadata for one candidate without requiring a source archive.</summary>
    Task<ElmPackageMetadata> GetMetadataAsync(ElmPackageIdentity identity, CancellationToken cancellationToken);

    /// <summary>Loads a selected package tree rooted at its elm.json, with source paths under src.</summary>
    Task<FileTree> GetSourcesAsync(ElmPackageIdentity identity, CancellationToken cancellationToken);
}

/// <summary>A classified package-data failure with the failing resource/cache origin and underlying exception.</summary>
public sealed class ElmPackageProviderException(
    ElmResolutionFailureKind kind,
    string origin,
    string message,
    Exception? innerException = null) : Exception(message, innerException)
{
    /// <summary>The cause category, not necessarily a dependency conflict.</summary>
    public ElmResolutionFailureKind Kind { get; } = kind;

    /// <summary>The URL, cache path or custom provider origin responsible for the error.</summary>
    public string Origin { get; } = origin;
}

/// <summary>
/// One search event, including the branch's complete requirements and selection. Rejected branches
/// remain available on successful resolutions as well as on failures.
/// </summary>
public sealed record ElmResolutionTraceEntry(
    int Sequence,
    string Action,
    string PackageName,
    ElmPackageVersion? Candidate,
    string Detail,
    string? Origin,
    ImmutableArray<ElmDependencyRequirement> Requirements,
    ImmutableArray<ElmPackageIdentity> Selection,
    ElmResolutionFailure? Failure = null);

/// <summary>An actionable failure with the exact branch requirements and selected identities that produced it.</summary>
public sealed record ElmResolutionFailure(
    ElmResolutionFailureKind Kind,
    string PackageName,
    string Message,
    ImmutableArray<ElmDependencyRequirement> Requirements,
    ImmutableArray<ElmPackageIdentity> Selection)
{
    /// <summary>Underlying provider/parser exception details, without classifying them as constraint conflicts.</summary>
    public string? ExceptionDetail { get; init; }
}

/// <summary>An exact graph node. Substituted nodes have no upstream manifest and retain their implementation identity.</summary>
public sealed record ElmResolvedPackage(
    ElmPackageIdentity Identity,
    ElmJsonStructure? Manifest,
    string Origin,
    string? SubstitutionImplementationId,
    ImmutableArray<string> ExposedModules,
    ImmutableArray<ElmDependencyRequirement> Dependencies);

/// <summary>A serializable resolution report. File trees and providers are deliberately kept outside it.</summary>
public sealed record ElmDependencyResolutionReport(
    string ManifestPath,
    ElmJsonStructure? Manifest,
    ElmDependencyResolutionConfigurationSummary Configuration,
    ImmutableArray<ElmDependencyRequirement> Requirements,
    ImmutableDictionary<string, ElmResolvedPackage> Packages,
    ImmutableArray<ElmResolutionTraceEntry> Trace,
    ImmutableArray<ElmResolutionFailure> Failures,
    string? Fingerprint)
{
    /// <summary>Unmodified root manifest, also retained when parsing fails.</summary>
    public string? ManifestText { get; init; }

    /// <summary>Whether resolution or build preparation completed without failures.</summary>
    public bool Succeeded => Failures.IsEmpty;

    /// <summary>Exports all graph, configuration, provenance and search-history information as indented JSON.</summary>
    public string ToJson() =>
        JsonSerializer.Serialize(
            this,
            new JsonSerializerOptions
            {
                WriteIndented = true,
                Converters = { new JsonStringEnumConverter() },
            });
}

/// <summary>Serializable replacement configuration, including implementation dependencies and a source SHA-256.</summary>
public sealed record ElmSubstitutionSummary(
    string PackageName,
    string ImplementationId,
    ImmutableArray<ElmPackageVersion> Versions,
    ImmutableArray<string> ExposedModules,
    string SourceFingerprint,
    ImmutableDictionary<string, string> ImplementationDependencies);

/// <summary>Serializable build policy, replacement identities and explicit resolution locks.</summary>
public sealed record ElmDependencyResolutionConfigurationSummary(
    ElmPackageVersion CompilerVersion,
    string CompilerImplementationId,
    bool IncludeTests,
    bool Offline,
    bool PreferOldest,
    ImmutableArray<ElmSubstitutionSummary> Substitutions,
    ImmutableDictionary<string, ElmPackageVersion> LockedVersions);

/// <summary>A failed preparation with a user-facing explanation and the complete retained resolution report.</summary>
/// <remarks>Renders actionable errors while preserving the report and underlying exception.</remarks>
public sealed class ElmDependencyResolutionException(ElmDependencyResolutionReport report, Exception? innerException = null) : Exception(RenderMessage(report), innerException)
{
    /// <summary>The graph and diagnostics captured before the build failed.</summary>
    public ElmDependencyResolutionReport Report { get; } = report;

    private static string RenderMessage(ElmDependencyResolutionReport report) =>
        $"Failed resolving Elm dependencies for '{report.ManifestPath}' ({(report.Configuration.IncludeTests ? "tests" : "build")}).\n" +
        string.Join(
            "\n\n",
            report.Failures.DistinctBy(failure => (failure.Kind, failure.PackageName, failure.Message)).Select(
                failure =>
                failure.Message + "\n" +
                string.Join(
                    "\n",
                    failure.Requirements.Select(
                        requirement =>
                        $"  {requirement.ManifestPath} [{requirement.Scope}]" +
                        (requirement.DependencyPath.IsEmpty
                        ?
                        ""
                        :
                        " -> " + string.Join(" -> ", requirement.DependencyPath)) +
                        $" -> {requirement.PackageName}: '{requirement.DeclaredVersion}'")))) +
        "\n" + string.Join("\n", report.Failures.Select(failure => Remedy(failure.Kind)).Distinct()) + " " +
        "The exception's Report contains the full resolution trace.";

    private static string Remedy(ElmResolutionFailureKind kind) =>
        kind switch
        {
            ElmResolutionFailureKind.ConstraintConflict or ElmResolutionFailureKind.NoMatchingVersion =>
            "Application versions are pins; builds never silently upgrade them. Update the incompatible dependency declarations explicitly.",

            ElmResolutionFailureKind.UnsupportedSubstitutionVersion =>
            "Configure a compatible replacement implementation/version set, or select a supported package version; upstream fallback is disabled.",

            ElmResolutionFailureKind.CompilerIncompatible =>
            "Select a compiler target compatible with the project's and packages' elm-version requirements.",

            ElmResolutionFailureKind.OfflineUnavailable =>
            "Retry online to populate the metadata/source caches. Missing local data does not prove a version conflict.",

            ElmResolutionFailureKind.ProviderFailure =>
            "Check the reported registry URL, network connection and cache permissions. This is not a dependency-constraint conflict.",

            ElmResolutionFailureKind.InvalidManifest =>
            "Correct the indicated elm.json fields or duplicate declarations. Invalid manifests are never silently ignored.",

            ElmResolutionFailureKind.DependencyCycle =>
            "Remove the indicated package dependency cycle or select package versions without that cycle.",

            ElmResolutionFailureKind.UndeclaredDependency =>
            "Regenerate the application's direct/indirect dependency tables; builds never silently add or upgrade application pins.",

            ElmResolutionFailureKind.InvalidModuleImport =>
            "Check the module's exposure, direct-dependency declarations and replacement implementation's supported API.",

            _ =>
            throw new NotImplementedException($"{nameof(Remedy)} does not handle failure kind: {kind}"),
        };
}

/// <summary>Prepared sources, ownership metadata and the exact graph used for a build.</summary>
public sealed record ElmResolvedBuild(
    FileTree Sources,
    ImmutableArray<ImmutableArray<string>> RootFilePaths,
    ElmDependencyResolutionReport Resolution,
    ImmutableDictionary<string, string> PackageSourceFingerprints,
    ImmutableDictionary<string, FileTree> PackageSources)
{
    /// <summary>Selected project sources before compiler namespace rewriting, with their original contents.</summary>
    public FileTree ProjectSources { get; init; } = FileTree.EmptyTree;

    /// <summary>SHA-256 of the selected, unmodified project sources.</summary>
    public string? ProjectSourceFingerprint { get; init; }

    /// <summary>Original source paths mapped to compiler identities, including disambiguated private modules.</summary>
    public ImmutableDictionary<string, string> CompilerModuleNames { get; init; } =
        [];

    /// <summary>Serializes the resolution, source hashes, roots and compiler identity mapping without file-tree internals.</summary>
    public string ToDebugJson() =>
        JsonSerializer.Serialize(
            new
            {
                Resolution = JsonSerializer.Deserialize<JsonElement>(Resolution.ToJson()),
                RootFilePaths,
                ProjectSourceFingerprint,
                PackageSourceFingerprints,
                CompilerModuleNames,
            },
            new JsonSerializerOptions { WriteIndented = true });
}
