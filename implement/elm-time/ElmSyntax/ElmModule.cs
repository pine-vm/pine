using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.RegularExpressions;

namespace ElmTime.ElmSyntax;

public static class ElmModule
{
    record ParsedModule(
        IReadOnlyList<string> ModuleName,
        IImmutableSet<IReadOnlyList<string>> ImportedModulesNames);

    public static IReadOnlyList<string> ModulesTextOrderedForCompilationByDependencies(IReadOnlyList<string> modulesTexts)
    {
        var parsedModules =
            modulesTexts
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

        IEnumerable<IReadOnlyList<string>> EnumerateImportsOfModuleTransitive(IReadOnlyList<string> moduleName) =>
            !parsedModulesByName.TryGetValue(moduleName, out (string moduleText, ParsedModule parsedModule) value) ?
            []
            : value.parsedModule.ImportedModulesNames
            .SelectMany(
                importedModuleName =>
                EnumerateImportsOfModuleTransitive(importedModuleName)
                .Prepend(importedModuleName));

        bool FirstModuleImportsSecondModuleTransitive(
            IReadOnlyList<string> moduleA,
            IReadOnlyList<string> moduleB) =>
            EnumerateImportsOfModuleTransitive(moduleA)
            .Contains(moduleB, EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        int ModuleSortOrder(IReadOnlyList<string> moduleA, IReadOnlyList<string> moduleB) =>
            FirstModuleImportsSecondModuleTransitive(moduleB, moduleA)
            ?
            -1 :
            FirstModuleImportsSecondModuleTransitive(moduleA, moduleB)
            ?
            1 :
            0;

        return
            parsedModules
            .OrderBy(
                parsedModule => parsedModule.parsedModule.ModuleName,
                new DelegateComparer<IReadOnlyList<string>>(ModuleSortOrder!))
            .Select(parsedModule => parsedModule.moduleText)
            .ToImmutableList();
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
}
