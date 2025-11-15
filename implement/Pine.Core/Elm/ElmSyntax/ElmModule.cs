using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.RegularExpressions;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Provides helpers for parsing Elm module texts and determining compilation order based on
/// inter-module dependencies. This includes:
/// <list type="bullet">
/// <item>Parsing module names and imported module names.</item>
/// <item>Removing leading Elm trivia (comments/whitespace).</item>
/// <item>Enumerating lines and line bounds from a module text.</item>
/// <item>Computing a filtered app code tree containing only required modules for given roots.</item>
/// </list>
/// </summary>
public static partial class ElmModule
{
    static readonly IReadOnlyList<IReadOnlyList<string>> s_elmCoreAutoImportedModulesNames =
        [
            ["Basics"],
            ["Tuple"],
            ["Maybe"],
            ["List"],
            ["Char"],
            ["String"],
            ["Result"],
        ];

    /// <summary>
    /// Internal representation containing the parsed module name and its directly imported modules.
    /// </summary>
    /// <param name="ModuleName">Segments composing the module name, e.g. ["Http", "Request"].</param>
    /// <param name="ImportedModulesNames">Set of module names (as segments) directly imported.</param>
    record ParsedModule(
        IReadOnlyList<string> ModuleName,
        IImmutableSet<IReadOnlyList<string>> ImportedModulesNames);

    /// <summary>
    /// Returns the ordered list of module texts (exact strings passed in) required for compilation,
    /// considering each input as a root. This overload assumes there are no additional available modules
    /// beyond the provided list.
    /// </summary>
    /// <param name="modulesTexts">The module texts to treat as roots.</param>
    /// <returns>Ordered list of module texts including their transitive dependencies.</returns>
    public static IReadOnlyList<string> ModulesTextOrderedForCompilationByDependencies(
        IReadOnlyList<string> modulesTexts) =>
        ModulesTextOrderedForCompilationByDependencies(
            rootModulesTexts: modulesTexts,
            availableModulesTexts: []);

    /// <summary>
    /// Computes the ordered list of module texts needed to compile the given root modules. All dependencies
    /// (direct and transitive) that are found among the <paramref name="rootModulesTexts"/> and
    /// <paramref name="availableModulesTexts"/> are included. Modules are ordered roughly by dependency depth so that
    /// dependencies appear before the modules that depend on them.
    /// </summary>
    /// <param name="rootModulesTexts">Module texts representing roots of compilation.</param>
    /// <param name="availableModulesTexts">Additional module texts that may be depended upon by roots.</param>
    /// <returns>Ordered list of module texts ready for compilation.</returns>
    /// <exception cref="Exception">Thrown if a module name cannot be parsed or duplicate module names are found.</exception>
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
            .Concat(s_elmCoreAutoImportedModulesNames)
            .Intersect(
                parsedModules.Select(pm => pm.parsedModule.ModuleName),
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .ToImmutableHashSet(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        IReadOnlyList<IReadOnlyList<string>> includedModulesNamesWithDeps =
            [
                .. s_elmCoreAutoImportedModulesNames,
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

    /// <summary>
    /// Parses the module name from Elm source text. Handles standard <c>module</c>, <c>port module</c>, and
    /// <c>effect module</c> declarations. Leading comments/whitespace are skipped.
    /// </summary>
    /// <param name="moduleText">Raw Elm module source text.</param>
    /// <returns>A result containing the module name segments or an error string.</returns>
    public static Result<string, IReadOnlyList<string>> ParseModuleName(string moduleText)
    {
        var textWithoutLeadingComments = RemoveLeadingTrivia(moduleText);

        {
            // Match: optional `port `, then `module`, then a dotted identifier, then `exposing`.
            // Example:  port module MyModule exposing

            var match =
                ModuleDeclarationSyntaxRegex().Match(textWithoutLeadingComments);

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

    /// <summary>
    /// Enumerates imported module names found in an Elm module text. Leading trivia is removed before parsing.
    /// Triple-quoted string regions are ignored to avoid false positives on <c>import</c> keywords appearing inside them.
    /// </summary>
    /// <param name="moduleText">Elm module source text.</param>
    /// <returns>Sequence of module names (each as list of segments) imported by the module.</returns>
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
                var match = ImportDeclarationSyntaxRegex().Match(line);

                if (match.Success)
                {
                    yield return match.Groups[1].Value.Split('.');
                }
            }
        }
    }

    /// <summary>
    /// Removes leading Elm trivia (whitespace, single-line comments, multi-line comments) from a module text.
    /// Useful before attempting to parse declarations at the top of the file.
    /// </summary>
    /// <param name="moduleText">Raw Elm module source.</param>
    /// <returns>Text with leading trivia removed.</returns>
    public static string RemoveLeadingTrivia(string moduleText)
    {
        // This pattern removes all leading:
        //   - whitespace (\s)
        //   - single-line comments (--... up to end of line)
        //   - multi-line comments ({- ... -}), which can span multiple lines
        // The * at the end repeats that pattern until it no longer matches.
        // Using RegexOptions.Singleline so '.' can match across newlines within {- -}.

        var textWithoutLeadingComments =
            LeadingTriviaSyntaxRegex().Replace(moduleText, replacement: "");

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

    /// <summary>
    /// Filters an application code tree to retain only the modules required for compilation given a predicate
    /// identifying root modules. All transitive dependencies of those roots are included. Non-Elm files are
    /// preserved only if they are <c>elm.json</c> files.
    /// </summary>
    /// <param name="appCodeTree">Tree containing application code blobs.</param>
    /// <param name="moduleNameIsRootModule">Predicate returning true for module names that should act as compilation roots.</param>
    /// <returns>A new tree containing only required Elm modules and any <c>elm.json</c> files.</returns>
    public static FileTree FilterAppCodeTreeForRootModulesAndDependencies(
        FileTree appCodeTree,
        Func<IReadOnlyList<string>, bool> moduleNameIsRootModule)
    {
        var originalBlobs =
            appCodeTree.EnumerateFilesTransitive()
            .ToImmutableArray();

        var allElmModules =
            originalBlobs
            .SelectWhere(
                blobPathAndContent =>
                {
                    try
                    {
                        var blobContentAsString = System.Text.Encoding.UTF8.GetString(blobPathAndContent.fileContent.Span);

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

    [GeneratedRegex(@"^import\s+([\w.]+)(\s|$)")]
    private static partial Regex ImportDeclarationSyntaxRegex();

    [GeneratedRegex(@"^(port\s+)?module\s+([\w.]+)\s+exposing", RegexOptions.Singleline)]
    private static partial Regex ModuleDeclarationSyntaxRegex();

    [GeneratedRegex(@"\A(?:
                      \s+                                 # skip any whitespace
                    | --[^\r\n]*(?:\r\n|\r|\n|$)         # skip single-line comment + EOL
                    | \{\-[\s\S]*?\-\}                   # skip multi-line comment
                  )*", RegexOptions.Singleline | RegexOptions.IgnorePatternWhitespace)]
    private static partial Regex LeadingTriviaSyntaxRegex();
}
