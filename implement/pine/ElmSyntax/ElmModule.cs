using Pine.Core;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.RegularExpressions;

namespace ElmTime.ElmSyntax;

public static class ElmModule
{
    static readonly IReadOnlyList<IReadOnlyList<string>> ElmCoreAutoImportedModulesNames =
        [
            ["Basics"],
            ["Tuple"],
            ["Maybe"],
            ["List"],
            ["Char"],
            ["String"],
            ["Result"],
        ];

    record ParsedModule(
        IReadOnlyList<string> ModuleName,
        IImmutableSet<IReadOnlyList<string>> ImportedModulesNames);

    public static IReadOnlyList<string> ModulesTextOrderedForCompilationByDependencies(
        IReadOnlyList<string> modulesTexts) =>
        ModulesTextOrderedForCompilationByDependencies(
            rootModulesTexts: modulesTexts,
            availableModulesTexts: []);

    public static IReadOnlyList<string> ModulesTextOrderedForCompilationByDependencies(
        IReadOnlyList<string> rootModulesTexts,
        IReadOnlyList<string> availableModulesTexts)
    {
        var allModulesTexts =
            rootModulesTexts
            .Concat(availableModulesTexts)
            .Distinct()
            .ToImmutableList();

        bool IsRootModule(string moduleText) =>
            rootModulesTexts.Contains(moduleText);

        var parsedModules =
            allModulesTexts
            .Select(
                moduleText =>
                (moduleText,
                parsedModule: new ParsedModule
                (ModuleName: ParseModuleName(moduleText).Extract(err => throw new Exception("Failed parsing module name: " + err)),
                ImportedModulesNames:
                    ParseModuleImportedModulesNames(moduleText)
                    .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>())
                )))
            .ToImmutableList();

        var parsedModulesByName =
            parsedModules
            .ToImmutableDictionary(
                keySelector: moduleTextAndParsed => moduleTextAndParsed.parsedModule.ModuleName,
                elementSelector: parsedModule => parsedModule,
                keyComparer: EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        IReadOnlySet<IReadOnlyList<string>> ListImportsOfModuleTransitive(IReadOnlyList<string> moduleName)
        {
            var queue =
                new Queue<IReadOnlyList<string>>([moduleName]);

            var set =
                new HashSet<IReadOnlyList<string>>(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

            while (queue.TryDequeue(out var currentModuleName))
            {
                if (set.Add(currentModuleName) &&
                    parsedModulesByName.TryGetValue(currentModuleName, out var currentModule))
                {
                    foreach (var importedModuleName in currentModule.parsedModule.ImportedModulesNames)
                    {
                        queue.Enqueue(importedModuleName);
                    }
                }
            }

            return set;
        }

        var parsedRootModules =
            parsedModules
            .Where(parsedModule => IsRootModule(parsedModule.moduleText))
            .ToImmutableList();

        var includedModulesNames =
            parsedRootModules
            .OrderByDescending(parsedModule => parsedModule.moduleText.Length)
            .SelectMany(rootModule =>
            ListImportsOfModuleTransitive(rootModule.parsedModule.ModuleName)
            .Prepend(rootModule.parsedModule.ModuleName))
            .Intersect(
                parsedModules.Select(pm => pm.parsedModule.ModuleName),
                EnumerableExtension.EqualityComparer<IReadOnlyList<string>>())
            .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        IReadOnlyList<IReadOnlyList<string>> includedModulesNamesWithDeps =
            [
                .. ElmCoreAutoImportedModulesNames,
                .. includedModulesNames
                .SelectMany(moduleName => ListImportsOfModuleTransitive(moduleName).Prepend(moduleName))
                .OrderBy(moduleName => ListImportsOfModuleTransitive(moduleName).Count),
            ];

        var includedModulesNamesOrdered =
            includedModulesNamesWithDeps
            .Distinct(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>())
            .Intersect(
                includedModulesNames,
                EnumerableExtension.EqualityComparer<IReadOnlyList<string>>())
            .ToImmutableArray();

        return
            [.. includedModulesNamesOrdered
            .Select(moduleName => parsedModulesByName[moduleName])
            .Select(parsedModule => parsedModule.moduleText)];
    }

    public class DelegateComparer<T>(Func<T?, T?, int> func) : IComparer<T>
    {
        private readonly Func<T?, T?, int> func = func;

        public int Compare(T? x, T? y) => func(x, y);
    }

    public static Result<string, IReadOnlyList<string>> ParseModuleName(string moduleText)
    {
        foreach (var moduleTextLine in moduleText.Trim().ModuleLines())
        {
            var match = Regex.Match(moduleTextLine, @"^(port\s+)?module\s+([\w.]+)\s+exposing");

            if (match.Success)
            {
                return Result<string, IReadOnlyList<string>>.ok(match.Groups[2].Value.Split('.'));
            }
        }

        return Result<string, IReadOnlyList<string>>.err("No module name found");
    }

    public static IEnumerable<IReadOnlyList<string>> ParseModuleImportedModulesNames(string moduleText)
    {
        foreach (var moduleTextLine in moduleText.Trim().ModuleLines())
        {
            var match = Regex.Match(moduleTextLine, @"^import\s+([\w.]+)(\s|$)");

            if (match.Success)
            {
                yield return match.Groups[1].Value.Split('.');
            }
        }
    }

    public static IEnumerable<string> ModuleLines(this string moduleText) =>
        moduleText.Split('\n', '\r');


    public static TreeNodeWithStringPath FilterAppCodeTreeForRootModulesAndDependencies(
        TreeNodeWithStringPath appCodeTree,
        Func<IReadOnlyList<string>, bool> moduleNameIsRootModule)
    {
        var originalBlobs =
            appCodeTree.EnumerateBlobsTransitive()
            .ToImmutableArray();

        var allElmModules =
            originalBlobs
            .SelectWhere(
                blobPathAndContent =>
                {
                    try
                    {
                        var blobContentAsString = System.Text.Encoding.UTF8.GetString(blobPathAndContent.blobContent.Span);

                        return
                            ParseModuleName(blobContentAsString)
                            .Unpack(
                                fromErr: _ =>
                                Maybe<(IReadOnlyList<string> path, string content)>.nothing(),

                                fromOk: moduleName =>
                                (blobPathAndContent.path, blobContentAsString));
                    }
                    catch
                    {
                        return Maybe<(IReadOnlyList<string> path, string content)>.nothing();
                    }
                })
            .ToImmutableArray();

        var rootModulesTexts =
            allElmModules
            .Where(moduleNameAndText => moduleNameIsRootModule(ParseModuleName(moduleNameAndText.content).WithDefault([])))
            .Select(moduleNameAndText => moduleNameAndText.content)
            .ToImmutableArray();

        var availableModulesTexts =
            allElmModules
            .Select(moduleNameAndText => moduleNameAndText.content)
            .Except(rootModulesTexts)
            .ToImmutableArray();

        var filteredModules =
            ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: rootModulesTexts,
                availableModulesTexts: availableModulesTexts);

        var filteredModulesPaths =
            filteredModules
            .Select(moduleText => allElmModules.First(moduleNameAndText => moduleNameAndText.content == moduleText).path)
            .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        return
            PineValueComposition.SortedTreeFromSetOfBlobs(
                [.. originalBlobs
                .Where(pathAndContent =>
                filteredModulesPaths.Contains(pathAndContent.path) ||
                pathAndContent.path.LastOrDefault() is "elm.json")]);
    }
}
