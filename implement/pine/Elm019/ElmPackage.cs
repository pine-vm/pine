using Pine.Core;
using Pine.Core.Elm.Elm019;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;

namespace Pine.Elm019;

public class ElmPackage
{
    record ParsedModule(
        IReadOnlyList<string> ModuleName,
        IImmutableSet<IReadOnlyList<string>> ImportedModulesNames);

    public static IReadOnlyDictionary<IReadOnlyList<string>, (ReadOnlyMemory<byte> fileContent, string moduleText, IReadOnlyList<string> moduleName)>
        ExposedModules(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> packageSourceFiles)
    {
        Dictionary<IReadOnlyList<string>, (ReadOnlyMemory<byte> fileContent, string moduleText, IReadOnlyList<string> moduleName)> exposedModules =
            new(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        var elmJsonFile =
            /*
             * If package files come from GitHub zip archive, then elm.json file might not be at the root but in
             * a directory named after the repository, e.g. "elm-flate-2.0.5".
             * */
            packageSourceFiles
            .OrderBy(kv => kv.Key.Count)
            .FirstOrDefault(kv => kv.Key.Last() is "elm.json");

        if (elmJsonFile.Key is null)
        {
            return exposedModules;
        }

        var elmJson = JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile.Value.Span);

        Dictionary<IReadOnlyList<string>, (IReadOnlyList<string> filePath, ReadOnlyMemory<byte> fileContent, string moduleText, ParsedModule parsedModule)> parsedModulesByName =
            new(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        foreach (var (filePath, fileContent) in packageSourceFiles)
        {
            try
            {
                var moduleText = Encoding.UTF8.GetString(fileContent.Span);

                if (Core.Elm.ElmSyntax.ElmModule.ParseModuleName(moduleText).IsOkOrNull() is not { } moduleName)
                {
                    continue;
                }

                parsedModulesByName[moduleName] =
                    (filePath,
                    fileContent,
                    moduleText,
                    new ParsedModule(
                        ModuleName: moduleName,
                        ImportedModulesNames: [.. Core.Elm.ElmSyntax.ElmModule.ParseModuleImportedModulesNames(moduleText)]));
            }
            catch (Exception)
            {
            }
        }

        IReadOnlySet<IReadOnlyList<string>> ListImportsOfModuleTransitive(IReadOnlyList<string> moduleName)
        {
            var queue =
                new Queue<IReadOnlyList<string>>([moduleName]);

            var set =
                new HashSet<IReadOnlyList<string>>(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

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

        foreach (var exposedModuleNameFlat in elmJson.ExposedModules)
        {
            var exposedModuleName = exposedModuleNameFlat.Split('.').ToImmutableList();

            var exposedNameIncludingDependencies =
                ListImportsOfModuleTransitive(exposedModuleName)
                .Prepend(exposedModuleName);

            foreach (var moduleName in exposedNameIncludingDependencies)
            {
                if (parsedModulesByName.TryGetValue(moduleName, out var module))
                {
                    exposedModules[module.filePath] =
                        (module.fileContent, module.moduleText, module.parsedModule.ModuleName);
                }
            }
        }

        return exposedModules;
    }
}
