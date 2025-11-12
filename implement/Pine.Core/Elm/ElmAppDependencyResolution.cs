using Pine.Core.Elm.Elm019;
using Pine.Core.Elm.ElmSyntax;
using Pine.Elm;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace Pine.Core.Elm;

/// <summary>
/// Describes the compilation units that make up an Elm app: the app files and the selected package files
/// needed for a specific compilation. Use this to pass a self-contained set of sources to the compiler.
/// </summary>
/// <param name="AppFiles">The filtered tree of app files relevant to the compilation.</param>
/// <param name="Packages">The ordered list of packages (files and parsed elm.json) needed for the compilation.</param>
public record AppCompilationUnits(
    BlobTreeWithStringPath AppFiles,
    IReadOnlyList<(BlobTreeWithStringPath files, ElmJsonStructure elmJson)> Packages)
{
    /// <summary>
    /// Creates an <see cref="AppCompilationUnits"/> instance that contains only the given app code
    /// and no packages. Useful for tests or scenarios where no external packages are required.
    /// </summary>
    /// <param name="appCode">The app code files.</param>
    /// <returns>An <see cref="AppCompilationUnits"/> with empty packages.</returns>
    public static AppCompilationUnits WithoutPackages(
        BlobTreeWithStringPath appCode)
    {
        return new AppCompilationUnits(
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
    /// Builds the <see cref="AppCompilationUnits"/> and resolves the entry module name for the given entry point file.
    /// This filters the app files down to the minimal set needed for compilation and selects the required packages
    /// based on imports and package dependencies.
    /// </summary>
    /// <param name="sourceFiles">The complete source file tree of the app.</param>
    /// <param name="entryPointFilePath">The path to the entry point .elm file (segments, not OS path).</param>
    /// <returns>
    /// A tuple containing:
    /// - files: The filtered <see cref="AppCompilationUnits"/>
    /// - entryModuleName: The parsed module name of the entry point file
    /// </returns>
    /// <exception cref="Exception">Thrown when the entry file is missing, not a blob, or the module name cannot be parsed.</exception>
    public static (AppCompilationUnits files, IReadOnlyList<string> entryModuleName)
        AppCompilationUnitsForEntryPoint(
        BlobTreeWithStringPath sourceFiles,
        IReadOnlyList<string> entryPointFilePath)
    {
        if (sourceFiles.GetNodeAtPath(entryPointFilePath) is not { } entryFileNode)
        {
            throw new Exception("Entry file not found: " + string.Join("/", entryPointFilePath));
        }

        if (entryFileNode is not BlobTreeWithStringPath.BlobNode entryFileBlob)
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

        var sourceFilesDict =
            PineValueComposition.TreeToFlatDictionaryWithPathComparer(sourceFiles);

        var sourceFilesFiltered =
            FilterTreeForCompilationRoots(
                sourceFiles,
                ImmutableHashSet.Create(
                    EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>(),
                    entryPointFilePath),
                skipFilteringForSourceDirs: false);

        IReadOnlyList<KeyValuePair<IReadOnlyList<string>, IReadOnlyList<IReadOnlyList<string>>>>
            remainingElmModulesNameAndImports =
            [.. sourceFilesFiltered
            .EnumerateBlobsTransitive()
            .Where(blob => blob.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(blob =>
            {
                var moduleText = Encoding.UTF8.GetString(blob.blobContent.Span);

                if (ElmModule.ParseModuleName(moduleText).IsOkOrNull() is not { } moduleName)
                {
                    throw new Exception("Failed to parse module name from file: " + string.Join("/", blob.path));
                }

                return new KeyValuePair<IReadOnlyList<string>, IReadOnlyList<IReadOnlyList<string>>>(
                    moduleName,
                    [.. ElmModule.ParseModuleImportedModulesNames(moduleText)]);
            })];

        var remainingElmModulesImports =
            remainingElmModulesNameAndImports
            .SelectMany(kv => kv.Value)
            .Where(importedModuleName => !remainingElmModulesNameAndImports.Any(kv => kv.Key.SequenceEqual(importedModuleName)))
            .ToImmutableHashSet(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        var packages = LoadPackagesForElmApp(sourceFilesDict);

        /*
         * We filter packages to include only those needed for the current compilation entry point.
         * 
         * Referencing two packages that expose modules with the same name is
         * no problem as long as the app does not reference that module name.
         * */

        IReadOnlySet<string> aggregateExposedModuleNames =
            packages
            .SelectMany(package => package.Value.elmJson.ExposedModules)
            .ToImmutableHashSet();

        var packagesFromExposedModuleName =
            aggregateExposedModuleNames
            .Select(moduleName =>
            {
                return
                    new KeyValuePair<string, IReadOnlySet<string>>(
                        moduleName,
                        packages
                        .Where(package => package.Value.elmJson.ExposedModules.Contains(moduleName))
                        .Select(package => package.Key)
                        .ToImmutableHashSet());
            })
            .ToImmutableDictionary();

        var packagesToIncludeRootsNames =
            remainingElmModulesImports
            .Select(importedModuleName =>
            {
                var importedModuleNameFlat = string.Join(".", importedModuleName);

                if (!packagesFromExposedModuleName.TryGetValue(importedModuleNameFlat, out var packages))
                {
                    throw new Exception("Failed to find package for imported module: " + importedModuleNameFlat);
                }

                if (packages.Count is not 1)
                {
                    throw new Exception(
                        "Imported module " + importedModuleNameFlat +
                        " is exposed by multiple packages: " + string.Join(", ", packages));
                }

                return packages.First();
            })
            .ToImmutableHashSet();

        IEnumerable<string> EnumeratePackageDependenciesTransitive(string packageName)
        {
            if (!packages.TryGetValue(packageName, out var package))
            {
                yield break;
            }

            var dependencies =
                package.elmJson.Dependencies.Direct.EmptyIfNull()
                .Concat(package.elmJson.Dependencies.Indirect.EmptyIfNull())
                .Concat(package.elmJson.Dependencies.Flat.EmptyIfNull());

            foreach (var dependency in dependencies)
            {
                yield return dependency.Key;

                foreach (var transitiveDependency in EnumeratePackageDependenciesTransitive(dependency.Key))
                {
                    yield return transitiveDependency;
                }
            }
        }

        var packagesToIncludeNames =
            packagesToIncludeRootsNames
            .Concat(packagesToIncludeRootsNames.SelectMany(EnumeratePackageDependenciesTransitive))
            .ToImmutableHashSet();

        var packagesToInclude =
            packages
            .Where(kv => packagesToIncludeNames.Contains(kv.Key))
            .ToImmutableDictionary();

        var packagesOrdered =
            packagesToInclude
            .OrderBy(kv => EnumeratePackageDependenciesTransitive(kv.Key).Count())
            .ThenBy(kv => kv.Key)
            .ToImmutableList();

        return
            (new AppCompilationUnits(
                sourceFilesFiltered,
                Packages: [.. packagesOrdered.Select(pkg => pkg.Value)]),
                moduleName);
    }

    /// <summary>
    /// Loads all packages referenced by any elm.json files found in the given app source tree.
    /// Returns a dictionary from package name to its file tree and parsed elm.json. If multiple elm.json files
    /// exist, their dependencies are merged. Package content is fetched based on the versions declared in elm.json.
    /// </summary>
    /// <param name="appSourceFiles">Flat dictionary representation of the app source tree.</param>
    /// <returns>A map of package name to its files and parsed elm.json.</returns>
    public static IReadOnlyDictionary<string, (BlobTreeWithStringPath files, ElmJsonStructure elmJson)>
        LoadPackagesForElmApp(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> appSourceFiles)
    {
        /*
        * TODO: select elm.json for the given entry point
        * */

        var elmJsonFiles =
            appSourceFiles
            .Where(entry => entry.Key.Last() is "elm.json")
            .ToImmutableDictionary();

        var elmJsonAggregateDependenciesVersions =
            elmJsonFiles
            .SelectMany(elmJsonFile =>
            {
                try
                {
                    var elmJsonParsed =
                    System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile.Value.Span);

                    return
                    new[]
                    {
                        elmJsonParsed?.Dependencies.Direct,
                        elmJsonParsed?.Dependencies.Indirect,
                        elmJsonParsed?.Dependencies.Flat
                    }
                    .WhereNotNull();
                }
                catch (Exception e)
                {
                    Console.WriteLine("Failed to parse elm.json file: " + e);

                    return [];
                }
            })
            .SelectMany(dependency => dependency)
            .ToImmutableDictionary();

        var elmJsonAggregateDependencies =
            elmJsonAggregateDependenciesVersions
            .ToImmutableDictionary(
                keySelector: dependency => dependency.Key,
                elementSelector:
                dependency =>
                {
                    var packageFiles =
                        ElmPackageSource.LoadElmPackageAsync(dependency.Key, dependency.Value).Result;

                    return packageFiles;
                });

        return
            elmJsonAggregateDependencies
            .ToImmutableDictionary(
                keySelector:
                kv => kv.Key,
                elementSelector:
                kv =>
                {
                    try
                    {
                        if (!kv.Value.TryGetValue(["elm.json"], out var elmJsonFile))
                        {
                            throw new Exception("Did not find elm.json file");
                        }

                        var elmJsonParsed =
                        System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile.Span)
                        ??
                        throw new Exception("Parsing elm.json returned null");

                        return (PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(kv.Value), elmJsonParsed);
                    }
                    catch (Exception e)
                    {
                        throw new Exception(
                            "Failed to load package: " + kv.Key + ": " + e.Message, e);
                    }
                });
    }

    /// <summary>
    /// Filters a tree of files to include only the Elm files needed to compile from the given root files,
    /// based on module dependency analysis. This overload accepts a list of root file paths.
    /// </summary>
    /// <param name="tree">The complete file tree.</param>
    /// <param name="rootFilePaths">The set of root .elm file paths that should be included.</param>
    /// <returns>A filtered tree that contains only files needed for compilation from the given roots.</returns>
    public static BlobTreeWithStringPath FilterTreeForCompilationRoots(
        BlobTreeWithStringPath tree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths)
    {
        var allAvailableElmFiles =
            tree
            .EnumerateBlobsTransitive()
            .Where(blobAtPath => blobAtPath.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(blobAtPath => (blobAtPath, moduleText: Encoding.UTF8.GetString(blobAtPath.blobContent.Span)))
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
            BlobTreeWithStringPath.FilterNodesByPath(
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
    public static BlobTreeWithStringPath FilterTreeForCompilationRoots(
        BlobTreeWithStringPath tree,
        IReadOnlySet<IReadOnlyList<string>> rootFilePaths,
        bool skipFilteringForSourceDirs)
    {
        var trees =
            rootFilePaths
            .Select(rootFilePath =>
            FilterTreeForCompilationRoot(
                tree,
                rootFilePath,
                skipFilteringForSourceDirs: skipFilteringForSourceDirs))
            .ToImmutableArray();

        return
            trees
            .Aggregate(
                seed: BlobTreeWithStringPath.EmptyTree,
                BlobTreeWithStringPath.MergeBlobs);
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
    public static BlobTreeWithStringPath FilterTreeForCompilationRoot(
        BlobTreeWithStringPath tree,
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
            .EnumerateBlobsTransitive()
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
            Core.Elm.ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts:
                [.. rootElmFiles.Select(file => Encoding.UTF8.GetString(file.blobContent.Span))
                ],
                availableModulesTexts:
                [.. availableElmFiles.Select(file => Encoding.UTF8.GetString(file.blobContent.Span))
                ]);

        var filePathsExcluded =
            allAvailableElmFiles
            .Where(elmFile => !elmModulesIncluded.Any(included => Encoding.UTF8.GetString(elmFile.blobContent.Span) == included))
            .Select(elmFile => elmFile.path)
            .ToImmutableHashSet(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        return
            BlobTreeWithStringPath.FilterNodesByPath(
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
        BlobTreeWithStringPath tree,
        IReadOnlyList<string> rootFilePath)
    {
        if (FindElmJsonForEntryPoint(tree, rootFilePath) is not { } elmJsonForEntryPoint)
        {
            throw new Exception(
                "Failed to find elm.json for entry point: " + string.Join("/", rootFilePath));
        }

        IReadOnlyList<ElmJsonStructure.RelativeDirectory> sourceDirectories =
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
                [..mappedPrefix,
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
        BlobTreeWithStringPath sourceFiles,
        IReadOnlyList<string> entryPointFilePath)
    {
        // Collect all elm.json files from the tree, storing each parsed ElmJsonStructure along with its path:
        var elmJsonFiles =
            sourceFiles
            .EnumerateBlobsTransitive()
            .SelectMany(pathAndContent =>
            {
                if (!pathAndContent.path.Last().EndsWith("elm.json", StringComparison.OrdinalIgnoreCase))
                {
                    return [];
                }

                var elmJsonContent = pathAndContent.blobContent;

                try
                {
                    var elmJsonParsed =
                        System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonContent.Span);

                    return
                        new[]
                        {
                            (filePath: (IReadOnlyList<string>)pathAndContent.path, elmJsonParsed)
                        };
                }
                catch (Exception)
                {
                    return [];
                }
            })
            .ToImmutableDictionary(
                keySelector: entry => entry.filePath,
                elementSelector: entry => entry.elmJsonParsed,
                keyComparer: EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        // Walk upwards from the directory of entryPointFilePath to find the "closest" elm.json
        // that includes the entryPointFilePath in one of its source-directories:
        var currentDirectory = DirectoryOf(entryPointFilePath);

        while (true)
        {
            // See if there is an elm.json directly in this directory:

            IReadOnlyList<string> elmJsonFilePath = [.. currentDirectory, "elm.json"];

            if (elmJsonFiles.TryGetValue(elmJsonFilePath, out var elmJsonParsed) && elmJsonParsed is not null)
            {
                // We found an elm.json in the current directory; now check if it includes the entry point
                // by verifying that entryPointFilePath is under one of its source-directories:
                if (ElmJsonIncludesEntryPoint(
                    currentDirectory, elmJsonParsed, entryPointFilePath))
                {
                    return (elmJsonFilePath, elmJsonParsed);
                }
            }

            // If we are at the root (no parent to move up to), stop:
            if (currentDirectory.Count is 0)
            {
                return null;
            }

            // Move up one level:
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

        foreach (var sourceDir in elmJson.ParsedSourceDirectories)
        {
            // Combine the elmJsonDirectory with the subdirectories from sourceDir
            // to get the absolute path to the "source directory":
            IReadOnlyList<string> absSourceDir =
                [.. elmJsonDirectory, .. sourceDir.Subdirectories];

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
