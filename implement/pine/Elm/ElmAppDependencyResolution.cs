using Pine.Core;
using Pine.Elm019;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Elm;

public class ElmAppDependencyResolution
{
    public static IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> MergePackagesElmModules(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> appSourceFiles)
    {
        /*
        * TODO: select elm.json for the given entry point
        * */

        var elmJsonFiles =
            appSourceFiles
            .Where(entry => entry.Key.Last() is "elm.json")
            .ToImmutableDictionary();

        var elmJsonAggregateDependencies =
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
            .ToImmutableDictionary(
                keySelector: dependency => dependency.Key,
                elementSelector:
                dependency =>
                {
                    var packageFiles =
                        ElmPackageSource.LoadElmPackageAsync(dependency.Key, dependency.Value).Result;

                    return packageFiles;
                });

        var sourceElmModulesNames =
            appSourceFiles
            .Where(entry => entry.Key.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(entry => ElmTime.ElmSyntax.ElmModule.ParseModuleName(entry.Value).WithDefault(null))
            .WhereNotNull()
            .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        var packagesModulesNames =
            new HashSet<IReadOnlyList<string>>(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        var sourceFilesWithMergedPackages =
            elmJsonAggregateDependencies
            .Aggregate(
                seed: appSourceFiles,
                func: (aggregate, package) =>
                {
                    if (package.Key is "elm/core" ||
                    package.Key is "elm/json" ||
                    package.Key is "elm/bytes" ||
                    package.Key is "elm/parser")
                    {
                        return aggregate;
                    }

                    var packageExposedModuleFiles =
                    ElmPackage.ExposedModules(package.Value);

                    return
                    packageExposedModuleFiles
                    .Aggregate(
                        seed: aggregate.ToImmutableDictionary(),
                        func: (innerAggregate, packageElmModuleFile) =>
                        {
                            var relativePath = packageElmModuleFile.Key;

                            var moduleName = packageElmModuleFile.Value.moduleName;

                            if (sourceElmModulesNames.Contains(moduleName))
                            {
                                Console.WriteLine(
                                    "Skipping Elm module file " +
                                    string.Join("/", relativePath) +
                                    " from package " + package.Key + " because it is already present in the source files.");

                                return innerAggregate;
                            }

                            if (packagesModulesNames.Contains(moduleName))
                            {
                                Console.WriteLine(
                                    "Skipping Elm module file " +
                                    string.Join("/", relativePath) +
                                    " from package " + package.Key + " because it is already present in the packages.");

                                return innerAggregate;
                            }

                            packagesModulesNames.Add(moduleName);

                            return
                                innerAggregate.SetItem(
                                    ["dependencies", .. package.Key.Split('/'), .. packageElmModuleFile.Key],
                                    packageElmModuleFile.Value.fileContent);
                        });
                });

        return sourceFilesWithMergedPackages;
    }
}
