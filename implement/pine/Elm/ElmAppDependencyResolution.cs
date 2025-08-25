using ElmTime.ElmSyntax;
using Pine.Core;
using Pine.Elm019;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Elm;

public record AppCompilationUnits(
    BlobTreeWithStringPath AppFiles,
    IReadOnlyList<(BlobTreeWithStringPath files, ElmJsonStructure elmJson)> Packages)
{
    public static AppCompilationUnits WithoutPackages(
        BlobTreeWithStringPath appCode)
    {
        return new AppCompilationUnits(
            appCode,
            Packages: []);
    }
}

public class ElmAppDependencyResolution
{
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
            System.Text.Encoding.UTF8.GetString(entryFileBlob.Bytes.Span);

        if (ElmModule.ParseModuleName(entryFileText).IsOkOrNull() is not { } moduleName)
        {
            throw new Exception(
                "Failed to parse module name from entry file: " + string.Join("/", entryPointFilePath));
        }

        var sourceFilesDict =
            PineValueComposition.TreeToFlatDictionaryWithPathComparer(sourceFiles);

        var sourceFilesFiltered =
            ElmCompiler.FilterTreeForCompilationRoots(
                sourceFiles,
                ImmutableHashSet.Create(
                    EnumerableExtension.EqualityComparer<IReadOnlyList<string>>(),
                    entryPointFilePath),
                skipFilteringForSourceDirs: false);

        IReadOnlyList<KeyValuePair<IReadOnlyList<string>, IReadOnlyList<IReadOnlyList<string>>>>
            remainingElmModulesNameAndImports =
            [.. sourceFilesFiltered
            .EnumerateBlobsTransitive()
            .Where(blob => blob.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(blob =>
            {
                var moduleText = System.Text.Encoding.UTF8.GetString(blob.blobContent.Span);

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
            .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

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

        IReadOnlyDictionary<string, IReadOnlySet<string>>
            packagesFromExposedModuleName =
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

        IEnumerable<string> enumeratePackageDependenciesTransitive(string packageName)
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

                foreach (var transitiveDependency in enumeratePackageDependenciesTransitive(dependency.Key))
                {
                    yield return transitiveDependency;
                }
            }
        }

        var packagesToIncludeNames =
            packagesToIncludeRootsNames
            .Concat(packagesToIncludeRootsNames.SelectMany(enumeratePackageDependenciesTransitive))
            .ToImmutableHashSet();

        var packagesToInclude =
            packages
            .Where(kv => packagesToIncludeNames.Contains(kv.Key))
            .ToImmutableDictionary();

        var packagesOrdered =
            packagesToInclude
            .OrderBy(kv => enumeratePackageDependenciesTransitive(kv.Key).Count())
            .ThenBy(kv => kv.Key)
            .ToImmutableList();

        return
            (new AppCompilationUnits(
                sourceFilesFiltered,
                Packages: [.. packagesOrdered.Select(pkg => pkg.Value)]),
                moduleName);
    }

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
}
