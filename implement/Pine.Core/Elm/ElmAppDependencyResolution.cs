using Pine.Core.Elm.Elm019;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace Pine.Core.Elm;

/// <summary>
/// Describes the source and package inputs available to an Elm application.
/// Declaration roots are selected separately by compilation callers.
/// </summary>
/// <param name="AppFiles">The project's source tree.</param>
/// <param name="Packages">The ordered list of packages (files and parsed elm.json) needed for the compilation.</param>
public record AppCompilationUnits(
    FileTree AppFiles,
    IReadOnlyList<(FileTree files, ElmJsonStructure elmJson)> Packages)
{
    /// <summary>The exact dependency graph and search history used to prepare these compilation units.</summary>
    public ElmDependencyResolutionReport? Resolution { get; init; }

    /// <summary>Prepared sources with package-qualified identities for private module collisions.</summary>
    public ElmResolvedBuild? ResolvedBuild { get; init; }

    /// <summary>
    /// Creates an <see cref="AppCompilationUnits"/> instance that contains only the given app code
    /// and no packages. Useful for tests or scenarios where no external packages are required.
    /// </summary>
    /// <param name="appCode">The app code files.</param>
    /// <returns>An <see cref="AppCompilationUnits"/> with empty packages.</returns>
    public static AppCompilationUnits WithoutPackages(
        FileTree appCode)
    {
        return
            new AppCompilationUnits(
                appCode,
                Packages: []);
    }
}

/// <summary>
/// Resolves Elm application dependencies and filters source trees for compilation.
/// Provides helpers to locate the right elm.json, compute source directories, and
/// determine the exact subset of files and packages required for a given entry point.
/// </summary>
public class ElmAppDependencyResolution
{
    /// <summary>
    /// Prepares the project's available declarations and resolves the selected entry module name.
    /// Unused unavailable imports are retained as diagnostics rather than blocking an unrelated compilation root.
    /// </summary>
    /// <param name="sourceFiles">The complete source file tree of the app.</param>
    /// <param name="entryPointFilePath">The path to the entry point .elm file (segments, not OS path).</param>
    /// <param name="configuration">Optional build policy, defaulting to Pine's bundled kernel substitutions.</param>
    /// <param name="provider">Optional package registry/source provider.</param>
    /// <param name="additionalRootFilePaths">Other entry modules that must remain in the same prepared environment.</param>
    /// <returns>
    /// A tuple containing:
    /// - files: The prepared <see cref="AppCompilationUnits"/>
    /// - entryModuleName: The parsed module name of the entry point file
    /// </returns>
    /// <exception cref="Exception">Thrown when the entry file is missing, not a blob, or the module name cannot be parsed.</exception>
    public static (AppCompilationUnits files, IReadOnlyList<string> entryModuleName)
        AppCompilationUnitsForEntryPoint(
        FileTree sourceFiles,
        IReadOnlyList<string> entryPointFilePath,
        ElmDependencyResolutionConfiguration? configuration = null,
        IElmPackageProvider? provider = null,
        IReadOnlyList<IReadOnlyList<string>>? additionalRootFilePaths = null)
    {
        if (sourceFiles.GetNodeAtPath(entryPointFilePath) is not { } entryFileNode)
        {
            throw new Exception("Entry file not found: " + string.Join("/", entryPointFilePath));
        }

        if (entryFileNode is not FileTree.FileNode entryFileBlob)
        {
            throw new Exception(
                "Entry file is not a blob: " + string.Join("/", entryPointFilePath));
        }

        var entryFileText =
            Encoding.UTF8.GetString(entryFileBlob.Bytes.Span);

        if (ElmModule.ParseModuleName(entryFileText).IsOkOrNull() is not { } moduleName)
        {
            throw new Exception(
                "Failed to parse module name from entry file: " + string.Join("/", entryPointFilePath));
        }

        var manifest =
            FindElmJsonForEntryPoint(sourceFiles, entryPointFilePath)
            ?? throw new ArgumentException(
                "No governing elm.json was found for entry point: " + string.Join("/", entryPointFilePath));

        configuration ??= ElmPackageSubstitutions.DefaultBuild.Value;

        var build =
            ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                sourceFiles,
                manifest.filePath,
                [entryPointFilePath, .. additionalRootFilePaths ?? []],
                configuration,
                provider).GetAwaiter().GetResult();

        var appFiles =
            ElmResolvedBuildPreparation.SelectProjectSources(
                sourceFiles,
                manifest.filePath,
                manifest.elmJsonParsed,
                configuration.IncludeTests);

        var packages =
            build.Resolution.Packages.Values.OrderBy(package => package.Identity.Name, StringComparer.Ordinal)
            .Select(
                package =>
                {
                    var files =
                        ElmResolvedBuildPreparation.AddPackageSources(
                            FileTree.EmptyTree,
                            package.Identity.Name,
                            build.PackageSources[package.Identity.Name]);

                    return (files, ManifestForResolvedPackage(package, configuration));
                }).ToImmutableArray();

        return (new AppCompilationUnits(appFiles, packages) { Resolution = build.Resolution, ResolvedBuild = build }, moduleName);
    }

    /// <summary>
    /// Resolves packages for one selected elm.json. Nested projects do not contribute requirements.
    /// </summary>
    /// <param name="appSourceFiles">Flat dictionary representation of the app source tree.</param>
    /// <param name="includePackage">Legacy filters are rejected; use whole-package substitutions instead.</param>
    /// <param name="loadPackage">Optional exact-version source loader; ranges require a provider with version listings.</param>
    /// <param name="manifestPath">Selected manifest, defaulting to the tree's root elm.json.</param>
    /// <param name="configuration">Optional build policy, defaulting to bundled substitutions.</param>
    /// <param name="provider">Optional version/metadata/source provider, mutually exclusive with loadPackage.</param>
    /// <returns>A map of package name to its files and parsed elm.json.</returns>
    public static IReadOnlyDictionary<string, (FileTree files, ElmJsonStructure elmJson)>
        LoadPackagesForElmApp(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> appSourceFiles,
        Func<string, bool>? includePackage = null,
        Func<string, string, IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>? loadPackage = null,
        IReadOnlyList<string>? manifestPath = null,
        ElmDependencyResolutionConfiguration? configuration = null,
        IElmPackageProvider? provider = null)
    {
        if (includePackage is not null)
        {
            throw new ArgumentException(
                "Package filters cannot safely resolve dependencies. Supply explicit package substitutions instead.",
                nameof(includePackage));
        }

        configuration ??= ElmPackageSubstitutions.DefaultBuild.Value;

        if (loadPackage is not null)
        {
            if (provider is not null)
                throw new ArgumentException("Supply either loadPackage or provider, not both.");

            provider = new ElmDelegatePackageProvider(loadPackage);
        }

        var tree = FileTree.FromSetOfFilesWithStringPath(appSourceFiles);

        var build =
            ElmResolvedBuildPreparation.PrepareAsync(
                tree,
                manifestPath ?? ["elm.json"],
                [],
                configuration,
                provider).GetAwaiter().GetResult();

        return
            build.Resolution.Packages.ToImmutableDictionary(
                item => item.Key,
                item => (build.PackageSources[item.Key], ManifestForResolvedPackage(item.Value, configuration)));
    }

    internal static ElmJsonStructure ManifestForResolvedPackage(
        ElmResolvedPackage package, ElmDependencyResolutionConfiguration configuration) =>
        package.Manifest ??
        new ElmJsonStructure(
            "package",
            package.Identity.Name,
            "Pine package substitution",
            "",
            package.Identity.Version.ToString(),
            package.ExposedModules,
            ["src"],
            configuration.CompilerVersion.ToString(),
            new(
                null,
                null,
                package.Dependencies.ToImmutableDictionary(item => item.PackageName, item => item.DeclaredVersion)),
            new(null, null, ImmutableDictionary<string, string>.Empty));

    /// <summary>
    /// Filters a tree of files to include only the Elm files needed to compile from the given root files,
    /// based on module dependency analysis. This overload accepts a list of root file paths.
    /// </summary>
    /// <param name="tree">The complete file tree.</param>
    /// <param name="rootFilePaths">The set of root .elm file paths that should be included.</param>
    /// <returns>A filtered tree that contains only files needed for compilation from the given roots.</returns>
    public static FileTree FilterTreeForCompilationRoots(
        FileTree tree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths)
    {
        var allAvailableElmFiles =
            tree
            .EnumerateFilesTransitive()
            .Where(blobAtPath => blobAtPath.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(blobAtPath => (blobAtPath, moduleText: Encoding.UTF8.GetString(blobAtPath.fileContent.Span)))
            .ToImmutableArray();

        var rootElmFiles =
            allAvailableElmFiles
            .Where(c => rootFilePaths.Any(root => c.blobAtPath.path.SequenceEqual(root)))
            .ToImmutableArray();

        var elmModulesIncluded =
            ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: [.. rootElmFiles.Select(file => file.moduleText)],
                availableModulesTexts: [.. allAvailableElmFiles.Select(file => file.moduleText)]);

        var filePathsExcluded =
            allAvailableElmFiles
            .Where(elmFile => !elmModulesIncluded.Any(included => elmFile.moduleText == included))
            .Select(elmFile => elmFile.blobAtPath.path)
            .ToImmutableHashSet(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        return
            FileTree.FilterNodesByPath(
                tree,
                nodePath =>
                !filePathsExcluded.Contains(nodePath));
    }

    /// <summary>
    /// Filters a tree of files to include only the Elm files needed to compile from the given root files,
    /// with optional restriction to the source-directories of the matching elm.json. When skipFilteringForSourceDirs
    /// is true, all .elm files are considered available no matter their directory; otherwise only files under
    /// the relevant source-directories are considered.
    /// </summary>
    /// <param name="tree">The complete file tree.</param>
    /// <param name="rootFilePaths">The set of root .elm file paths that should be included.</param>
    /// <param name="skipFilteringForSourceDirs">Whether to skip filtering by source-directories from elm.json.</param>
    /// <returns>A filtered tree that contains only files needed for compilation from the given roots.</returns>
    public static FileTree FilterTreeForCompilationRoots(
        FileTree tree,
        IReadOnlySet<IReadOnlyList<string>> rootFilePaths,
        bool skipFilteringForSourceDirs)
    {
        var trees =
            rootFilePaths
            .Select(
                rootFilePath =>
                FilterTreeForCompilationRoot(
                    tree,
                    rootFilePath,
                    skipFilteringForSourceDirs: skipFilteringForSourceDirs))
            .ToImmutableArray();

        return
            trees
            .Aggregate(
                seed: FileTree.EmptyTree,
                FileTree.MergeFiles);
    }

    /// <summary>
    /// Filters a tree of files to include only the Elm files needed to compile the specified root file.
    /// When <paramref name="skipFilteringForSourceDirs"/> is false, only files under the source-directories
    /// of the elm.json governing the root are considered.
    /// </summary>
    /// <param name="tree">The complete file tree.</param>
    /// <param name="rootFilePath">The root .elm file path that should be included.</param>
    /// <param name="skipFilteringForSourceDirs">Whether to skip filtering by source-directories from elm.json.</param>
    /// <returns>A filtered tree that contains only files needed for compilation of the root.</returns>
    public static FileTree FilterTreeForCompilationRoot(
        FileTree tree,
        IReadOnlyList<string> rootFilePath,
        bool skipFilteringForSourceDirs)
    {
        var keepElmModuleAtFilePath =
            skipFilteringForSourceDirs
            ?
            _ => true
            :
            BuildPredicateFilePathIsInSourceDirectory(tree, rootFilePath);

        var allAvailableElmFiles =
            tree
            .EnumerateFilesTransitive()
            .Where(blobAtPath => blobAtPath.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .ToImmutableArray();

        var availableElmFiles =
            allAvailableElmFiles
            .Where(blobAtPath => keepElmModuleAtFilePath(blobAtPath.path))
            .ToImmutableArray();

        var rootElmFiles =
            availableElmFiles
            .Where(c => c.path.SequenceEqual(rootFilePath))
            .ToImmutableArray();

        var elmModulesIncluded =
            ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts:
                [.. rootElmFiles.Select(file => Encoding.UTF8.GetString(file.fileContent.Span))],
                availableModulesTexts:
                [.. availableElmFiles.Select(file => Encoding.UTF8.GetString(file.fileContent.Span))]);

        var filePathsExcluded =
            allAvailableElmFiles
            .Where(
                elmFile =>
                !elmModulesIncluded.Any(included => Encoding.UTF8.GetString(elmFile.fileContent.Span) == included))
            .Select(elmFile => elmFile.path)
            .ToImmutableHashSet(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        return
            FileTree.FilterNodesByPath(
                tree,
                nodePath =>
                !filePathsExcluded.Contains(nodePath));
    }

    /// <summary>
    /// Builds a predicate that determines whether a given file path is located under any source-directory
    /// declared in the elm.json governing the specified root file. Throws if the source-directories cannot be mapped.
    /// </summary>
    /// <param name="tree">The full file tree.</param>
    /// <param name="rootFilePath">The root .elm file path whose elm.json defines the source-directories.</param>
    /// <returns>A predicate returning true when the path is inside a source-directory.</returns>
    private static Func<IReadOnlyList<string>, bool> BuildPredicateFilePathIsInSourceDirectory(
        FileTree tree,
        IReadOnlyList<string> rootFilePath)
    {
        if (FindElmJsonForEntryPoint(tree, rootFilePath) is not { } elmJsonForEntryPoint)
        {
            throw new Exception(
                "Failed to find elm.json for entry point: " + string.Join("/", rootFilePath));
        }

        IReadOnlyList<ElmJsonStructure.RelativeDirectory> sourceDirectories =
            elmJsonForEntryPoint.elmJsonParsed.Type is "package"
            ?
            [new(0, ["src"])]
            :
            [.. elmJsonForEntryPoint.elmJsonParsed.ParsedSourceDirectories];

        IReadOnlyList<string> elmJsonDirectoryPath =
            [.. elmJsonForEntryPoint.filePath.SkipLast(1)];

        var sourceDirectoriesMapped = new List<IReadOnlyList<string>>();

        foreach (var sourceDirectory in sourceDirectories)
        {
            if (sourceDirectory.ParentLevel > elmJsonDirectoryPath.Count)
            {
                throw new InvalidOperationException(
                    "Path is not contained in source: Source directory parent level is " +
                    sourceDirectory.ParentLevel +
                    " but elm.json is at path " +
                    string.Join("/", elmJsonDirectoryPath));
            }

            IReadOnlyList<string> mappedPrefix =
                [.. elmJsonDirectoryPath.SkipLast(sourceDirectory.ParentLevel)];

            IReadOnlyList<string> mappedSourceDirectory =
                [
                ..mappedPrefix,
                ..sourceDirectory.Subdirectories
                ];

            sourceDirectoriesMapped.Add(mappedSourceDirectory);
        }

        bool FilePathIsInSourceDirectory(IReadOnlyList<string> filePath)
        {
            foreach (var mappedSourceDirectory in sourceDirectoriesMapped)
            {
                if (filePath.Count < mappedSourceDirectory.Count)
                {
                    continue;
                }

                if (filePath.Take(mappedSourceDirectory.Count).SequenceEqual(mappedSourceDirectory))
                {
                    return true;
                }
            }

            return false;
        }

        return FilePathIsInSourceDirectory;
    }

    /// <summary>
    /// Finds the closest elm.json (walking upwards from the directory of the entry point) that includes
    /// the given entry point within one of its source-directories.
    /// </summary>
    /// <param name="sourceFiles">The full source file tree.</param>
    /// <param name="entryPointFilePath">The path to the entry point .elm file (segments, not OS path).</param>
    /// <returns>
    /// A tuple with the elm.json file path and its parsed content, or null if no suitable elm.json is found.
    /// </returns>
    public static (IReadOnlyList<string> filePath, ElmJsonStructure elmJsonParsed)?
        FindElmJsonForEntryPoint(
        FileTree sourceFiles,
        IReadOnlyList<string> entryPointFilePath)
    {
        var currentDirectory = DirectoryOf(entryPointFilePath);

        while (true)
        {
            IReadOnlyList<string> elmJsonFilePath = [.. currentDirectory, "elm.json"];

            if (sourceFiles.GetNodeAtPath(elmJsonFilePath) is not null)
            {
                var elmJsonParsed = ElmDependencyResolver.ReadManifest(sourceFiles, elmJsonFilePath);

                if (ElmJsonIncludesEntryPoint(
                    currentDirectory,
                    elmJsonParsed,
                    entryPointFilePath))
                {
                    return (elmJsonFilePath, elmJsonParsed);
                }
            }

            if (currentDirectory.Count is 0)
            {
                return null;
            }

            currentDirectory = [.. currentDirectory.Take(currentDirectory.Count - 1)];
        }
    }

    /// <summary>
    /// Returns all but the last segment of filePath (i.e. the directory path).
    /// </summary>
    private static IReadOnlyList<string> DirectoryOf(IReadOnlyList<string> filePath)
    {
        if (filePath.Count is 0)
            return filePath;

        return [.. filePath.Take(filePath.Count - 1)];
    }

    /// <summary>
    /// Checks whether the given elm.json file includes entryPointFilePath in one of its source-directories.
    /// Since source-directories in elm.json are relative to the directory containing elm.json,
    /// we build absolute paths and compare.
    /// </summary>
    private static bool ElmJsonIncludesEntryPoint(
        IReadOnlyList<string> elmJsonDirectory,
        ElmJsonStructure elmJson,
        IReadOnlyList<string> entryPointFilePath)
    {
        // For each source directory in elm.json, build its absolute path (relative to elm.jsonDirectory),
        // and check whether entryPointFilePath starts with that path.

        var sourceDirectories =
            elmJson.Type is "package"
            ?
            [new ElmJsonStructure.RelativeDirectory(0, ["src"])]
            :
            elmJson.ParsedSourceDirectories;

        foreach (var sourceDir in sourceDirectories)
        {
            // Combine the elmJsonDirectory with the subdirectories from sourceDir
            // to get the absolute path to the "source directory":
            IReadOnlyList<string> absSourceDir =
                ElmResolvedBuildPreparation.MapSourceDirectory(elmJsonDirectory, sourceDir);

            // Check if entryPointFilePath is "under" absSourceDir:
            if (entryPointFilePath.Count >= absSourceDir.Count &&
                entryPointFilePath
                .Take(absSourceDir.Count)
                .SequenceEqual(absSourceDir))
            {
                // The entry point sits in one of the source-directories recognized by this elm.json
                return true;
            }
        }

        return false;
    }
}
