using Pine.Core;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace Pine.Elm.CLI;

public class ElmFormatCommand
{
    public static Command CreateElmFormatCommand()
    {
        var command = new Command("elm-format", "Format Elm module files.");

        var pathsArgument = new Argument<string[]>("paths")
        {
            Description = "Paths to Elm files or directories to format",
            Arity = ArgumentArity.OneOrMore
        };

        var yesOption = new Option<bool>("--yes")
        {
            Description = "Overwrite files without prompting for confirmation"
        };

        var verifyNoChangesOption = new Option<bool>("--verify-no-changes")
        {
            Description = "Check if all Elm modules are already formatted (for CI/automated reviews)"
        };

        command.Add(pathsArgument);
        command.Add(yesOption);
        command.Add(verifyNoChangesOption);

        command.SetAction((parseResult) =>
        {
            var paths = parseResult.GetValue(pathsArgument);
            var yes = parseResult.GetValue(yesOption);
            var verifyNoChanges = parseResult.GetValue(verifyNoChangesOption);

            return ElmFormatCommandExecute(
                paths: paths!,
                skipPrompt: yes,
                verifyNoChanges: verifyNoChanges);
        });

        return command;
    }

    private static int ElmFormatCommandExecute(
        string[] paths,
        bool skipPrompt,
        bool verifyNoChanges)
    {
        // Collect all .elm files from the given paths
        var elmFiles = new List<string>();

        var shouldShowCount = paths.Length > 1;

        foreach (var path in paths)
        {
            try
            {
                var fullPath = Path.GetFullPath(path);

                if (File.Exists(fullPath))
                {
                    if (fullPath.EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
                    {
                        elmFiles.Add(fullPath);
                    }
                    else
                    {
                        Console.WriteLine($"Warning: Skipping non-.elm file: {fullPath}");
                    }
                }
                else
                {
                    shouldShowCount = true;

                    if (Directory.Exists(fullPath))
                    {
                        var filesInDir = Directory.GetFiles(fullPath, "*.elm", SearchOption.AllDirectories);
                        elmFiles.AddRange(filesInDir);
                    }
                    else
                    {
                        Console.Error.WriteLine($"Error: Path not found: {fullPath}");
                        return 1;
                    }
                }
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"Error processing path '{path}': {ex.Message}");
                return 1;
            }
        }

        if (elmFiles.Count is 0)
        {
            Console.WriteLine("No .elm files found.");
            return 0;
        }

        // Sort paths alphabetically
        elmFiles.Sort(StringComparer.Ordinal);

        // Parse, format, and check each file in parallel.
        // Parallel.ForEach automatically adapts max concurrency based on the available
        // processors, using the default ThreadPool scheduler.
        var alreadyFormatted = new ConcurrentBag<string>();
        var needsFormatting = new ConcurrentBag<(string path, string formattedContent)>();
        var parseErrors = new ConcurrentBag<(string path, string error)>();

        Parallel.ForEach(elmFiles, (filePath) =>
        {
            try
            {
                var originalContent = File.ReadAllText(filePath);

                var formatResult = ElmFormat.FormatModuleText(originalContent);

                if (formatResult.IsErrOrNull() is { } formatErr)
                {
                    parseErrors.Add((filePath, formatErr));
                    return;
                }

                if (formatResult.IsOkOrNull() is not { } rendered)
                {
                    parseErrors.Add((filePath, "Unexpected format result type"));
                    return;
                }

                // Check if content changed
                if (originalContent == rendered)
                {
                    alreadyFormatted.Add(filePath);
                }
                else
                {
                    needsFormatting.Add((filePath, rendered));
                }
            }
            catch (Exception ex)
            {
                parseErrors.Add((filePath, ex.Message));
            }
        });

        // Output already formatted files (sorted for consistent output)
        var sortedAlreadyFormatted =
            alreadyFormatted
            .OrderBy(p => p, StringComparer.Ordinal)
            .ToImmutableList();

        if (sortedAlreadyFormatted.Count is not 0)
        {
            foreach (var path in sortedAlreadyFormatted)
            {
                Console.WriteLine(path);
            }
        }

        // Sort collections for consistent output
        var sortedParseErrors =
            parseErrors
            .OrderBy(e => e.path, StringComparer.Ordinal)
            .ToImmutableList();

        var sortedNeedsFormatting =
            needsFormatting
            .OrderBy(f => f.path, StringComparer.Ordinal)
            .ToImmutableList();

        // Handle verify-no-changes mode
        if (verifyNoChanges)
        {
            if (sortedParseErrors.Count is not 0)
            {
                Console.WriteLine("\nFiles with syntax errors:");

                foreach (var (path, error) in sortedParseErrors)
                {
                    Console.WriteLine($"{path}: {error}");
                }

                return 200;
            }

            if (sortedNeedsFormatting.Count is not 0)
            {
                Console.WriteLine("\nFiles not formatted:");

                foreach (var (path, _) in sortedNeedsFormatting)
                {
                    Console.WriteLine(path);
                }

                return 100;
            }

            return 0;
        }

        // Report parse errors
        if (sortedParseErrors.Count is not 0)
        {
            Console.WriteLine("\nFiles with syntax errors:");

            foreach (var (path, error) in sortedParseErrors)
            {
                Console.WriteLine($"{path}: {error}");
            }

            return 200;
        }

        // Report files that need formatting
        if (sortedNeedsFormatting.Count is 0)
        {
            // All files already formatted
            if (shouldShowCount)
            {
                Console.WriteLine("\n0 file(s) need formatting");
            }

            if (elmFiles.Count is 1)
            {
                Console.WriteLine($"File {elmFiles[0]} is already formatted.");
            }
            else
            {
                Console.WriteLine($"All {elmFiles.Count} files are already formatted.");
            }

            return 0;
        }
        else if (sortedNeedsFormatting.Count is not 0)
        {
            // Report count if processing multiple files or a directory
            if (shouldShowCount)
            {
                Console.WriteLine($"\n{sortedNeedsFormatting.Count} file(s) need formatting");
            }

            // List files to be formatted
            foreach (var (path, _) in sortedNeedsFormatting)
            {
                Console.WriteLine(path);
            }

            // Prompt for confirmation unless --yes was specified
            if (!skipPrompt)
            {
                Console.WriteLine("\nAre you sure you want to overwrite these files with formatted versions? (y/n)");

                var response =
                    Console.ReadLine()
                    ?.Trim()
                    .ToLowerInvariant();

                if (response is not "y" && response is not "yes")
                {
                    Console.WriteLine("Formatting cancelled.");
                    return 0;
                }
            }

            // Write formatted content to files
            foreach (var (path, formattedContent) in sortedNeedsFormatting)
            {
                File.WriteAllText(path, formattedContent);
            }

            Console.WriteLine($"\nFormatted {sortedNeedsFormatting.Count} file(s).");
        }
        else
        {
            // All files already formatted
            if (shouldShowCount)
            {
                Console.WriteLine("\n0 file(s) need formatting");
            }
        }

        return 0;
    }
}
