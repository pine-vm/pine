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
                (ModuleName: ParseModuleName(moduleText)
                .Extract(err =>
                throw new Exception(
                    string.Concat(
                        "Failed parsing module name: ", err,
                        "\nmodule text (" + moduleText.Length.ToString() + "):\n:",
                        moduleText.AsSpan(0, Math.Min(1000, moduleText.Length))))),
                ImportedModulesNames:
                    ParseModuleImportedModulesNames(moduleText)
                    .ToImmutableHashSet(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
                )))
            .ToImmutableList();

        foreach (var module in parsedModules)
        {
            var modulesWithSameName =
                parsedModules
                .Where(parsedModule => parsedModule.parsedModule.ModuleName.SequenceEqual(module.parsedModule.ModuleName))
                .ToImmutableList();

            if (1 < modulesWithSameName.Count)
            {
                throw new Exception(
                    "Multiple modules with the same name: " + string.Join(".", module.parsedModule.ModuleName));
            }
        }

        var parsedModulesByName =
            parsedModules
            .ToImmutableDictionary(
                keySelector: moduleTextAndParsed => moduleTextAndParsed.parsedModule.ModuleName,
                elementSelector: parsedModule => parsedModule,
                keyComparer: EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

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
            .Concat(ElmCoreAutoImportedModulesNames)
            .Intersect(
                parsedModules.Select(pm => pm.parsedModule.ModuleName),
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .ToImmutableHashSet(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        IReadOnlyList<IReadOnlyList<string>> includedModulesNamesWithDeps =
            [
                .. ElmCoreAutoImportedModulesNames,
                .. includedModulesNames
                .OrderBy(moduleName => string.Join(".", moduleName))
                .SelectMany(moduleName => ListImportsOfModuleTransitive(moduleName).Prepend(moduleName))
                .OrderBy(moduleName => ListImportsOfModuleTransitive(moduleName).Count),
            ];

        var includedModulesNamesOrdered =
            includedModulesNamesWithDeps
            .Distinct(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Where(includedModulesNames.Contains)
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

    public static Result<string, IReadOnlyList<string>> ParseModuleName(ReadOnlyMemory<byte> moduleContent)
    {
        try
        {
            var moduleText =
                System.Text.Encoding.UTF8.GetString(moduleContent.Span);

            return ParseModuleName(moduleText);
        }
        catch (Exception exception)
        {
            return "Failed decoding text: " + exception.Message;
        }
    }

    public static Result<string, IReadOnlyList<string>> ParseModuleName(string moduleText)
    {
        var textWithoutLeadingComments = RemoveLeadingTrivia(moduleText);

        {
            // Match: optional `port `, then `module`, then a dotted identifier, then `exposing`.
            // Example:  port module MyModule exposing

            var match =
                Regex.Match(
                    textWithoutLeadingComments,
                    @"^(port\s+)?module\s+([\w.]+)\s+exposing",
                    RegexOptions.Singleline);

            if (match.Success)
            {
                return Result<string, IReadOnlyList<string>>.ok(
                    match.Groups[2].Value.Split('.'));
            }
        }

        {
            var match =
                Regex.Match(
                    textWithoutLeadingComments,
                    @"^effect\s+module\s+([\w.]+)\s+(where|exposing)",
                    RegexOptions.Singleline);

            if (match.Success)
            {
                return Result<string, IReadOnlyList<string>>.ok(
                    match.Groups[1].Value.Split('.'));
            }
        }

        return Result<string, IReadOnlyList<string>>.err("No module name found");
    }

    public static IEnumerable<IReadOnlyList<string>> ParseModuleImportedModulesNames(string moduleText)
    {
        var textWithoutLeadingComments = RemoveLeadingTrivia(moduleText);

        var inTripleQuotedString = false;

        foreach (var line in ModuleLines(textWithoutLeadingComments))
        {
            // We'll do a simple toggle for every """ we see on the line.
            // Each occurrence flips us from outside->inside or inside->outside.
            var searchStart = 0;
            while (true)
            {
                var index = line.IndexOf("\"\"\"", searchStart, StringComparison.Ordinal);
                if (index < 0)
                    break;

                inTripleQuotedString = !inTripleQuotedString;
                searchStart = index + 3;
            }

            // Only parse imports if we are outside any triple-quoted string
            if (!inTripleQuotedString)
            {
                var match = Regex.Match(line, @"^import\s+([\w.]+)(\s|$)");
                if (match.Success)
                {
                    yield return match.Groups[1].Value.Split('.');
                }
            }
        }
    }

    public static string RemoveLeadingTrivia(string moduleText)
    {
        // This pattern removes all leading:
        //   - whitespace (\s)
        //   - single-line comments (--... up to end of line)
        //   - multi-line comments ({- ... -}), which can span multiple lines
        // The * at the end repeats that pattern until it no longer matches.
        // Using RegexOptions.Singleline so '.' can match across newlines within {- -}.
        var textWithoutLeadingComments = Regex.Replace(
            moduleText,
            pattern: @"\A(?:
                      \s+                                 # skip any whitespace
                    | --[^\r\n]*(?:\r\n|\r|\n|$)         # skip single-line comment + EOL
                    | \{\-[\s\S]*?\-\}                   # skip multi-line comment
                  )*",
            replacement: "",
            options: RegexOptions.IgnorePatternWhitespace | RegexOptions.Singleline);

        return textWithoutLeadingComments;
    }

    /// <summary>
    /// Enumerates strings of individual lines, the subsequences of characters between line breaks.
    /// <para />
    /// Any of the following four character sequences is considered a line break:
    /// <list type="bullet">
    /// <item>LF (line feed)</item>
    /// <item>CR (carriage return)</item>
    /// <item>CR LF (carriage return + line feed</item>
    /// <item>LF CR (line feed + carriage return)</item>
    /// </list>
    /// </summary>
    public static IEnumerable<string> ModuleLines(this string moduleText)
    {
        var lastLineStartIndex = 0;

        foreach (var (lineEnd, lineStart) in ModuleLinesBounds(moduleText))
        {
            var lineLength = lineEnd - lastLineStartIndex;

            var lineText = moduleText.Substring(lastLineStartIndex, lineLength);

            yield return lineText;

            lastLineStartIndex = lineStart;
        }

        var remainingText = moduleText[lastLineStartIndex..];

        yield return remainingText;
    }

    /// <summary>
    /// Enumerates line breaks, returning the indices of characters at the ending of one line and the start of the following line.
    /// <para />
    /// Any of the following four character sequences is considered a line break:
    /// <list type="bullet">
    /// <item>LF (line feed)</item>
    /// <item>CR (carriage return)</item>
    /// <item>CR LF (carriage return + line feed</item>
    /// <item>LF CR (line feed + carriage return)</item>
    /// </list>
    /// </summary>
    public static IEnumerable<(int lineEnd, int lineStart)> ModuleLinesBounds(this string moduleText)
    {
        for (var charIndex = 0; charIndex < moduleText.Length; charIndex++)
        {
            var currentChar = moduleText[charIndex];

            if (currentChar is '\n')
            {
                if (moduleText.Length > charIndex + 1 && moduleText[charIndex + 1] is '\r')
                {
                    yield return (charIndex, charIndex + 2);

                    ++charIndex;
                }
                else
                {
                    yield return (charIndex, charIndex + 1);
                }
            }

            if (currentChar is '\r')
            {
                if (moduleText.Length > charIndex + 1 && moduleText[charIndex + 1] is '\n')
                {
                    yield return (charIndex, charIndex + 2);

                    ++charIndex;
                }
                else
                {
                    yield return (charIndex, charIndex + 1);
                }
            }
        }
    }

    public static BlobTreeWithStringPath FilterAppCodeTreeForRootModulesAndDependencies(
        BlobTreeWithStringPath appCodeTree,
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
            .ToImmutableHashSet(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        return
            PineValueComposition.SortedTreeFromSetOfBlobs(
                [.. originalBlobs
                .Where(pathAndContent =>
                filteredModulesPaths.Contains(pathAndContent.path) ||
                pathAndContent.path.LastOrDefault() is "elm.json")]);
    }
}
