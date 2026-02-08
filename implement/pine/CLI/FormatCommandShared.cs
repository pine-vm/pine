using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace Pine.CLI;

/// <summary>
/// Represents the result of formatting a single source file.
/// </summary>
public abstract record FormatFileResult
{
    private FormatFileResult() { }


    /// <summary>File had an error and could not be processed.</summary>
    public sealed record Error(string ErrorText) : FormatFileResult;


    /// <summary>File was already properly formatted (no changes needed).</summary>
    public sealed record Stable : FormatFileResult;


    /// <summary>File needs formatting (content differs from formatted version).</summary>
    public sealed record Changed(string FormattedText) : FormatFileResult;
}


/// <summary>
/// Shared logic for format commands (e.g., elm-format, csharp-format).
/// </summary>
public static class FormatCommandShared
{
    /// <summary>
    /// Minimum number of files before showing detailed overview with grouping.
    /// </summary>
    public const int MinFilesForDetailedOverview = 5;


    /// <summary>
    /// Executes a format command for the given file extension and formatter.
    /// </summary>
    /// <param name="paths">Paths to files or directories to format.</param>
    /// <param name="fileExtension">File extension to filter (e.g., ".elm", ".cs").</param>
    /// <param name="formatFile">Function that takes file content and returns a FormatFileResult.</param>
    /// <param name="skipPrompt">Whether to skip confirmation prompt.</param>
    /// <param name="verifyNoChanges">Whether to only verify files are formatted.</param>
    /// <param name="commandLabel">Label for the command (e.g., "elm-format", "csharp-format").</param>
    /// <returns>Exit code: 0 for success, 100 for files needing formatting, 200 for errors.</returns>
    public static int Execute(
        string[] paths,
        string fileExtension,
        Func<string, FormatFileResult> formatFile,
        bool skipPrompt,
        bool verifyNoChanges,
        string commandLabel)
    {
        var files = new List<string>();
        var shouldShowCount = paths.Length > 1;

        foreach (var path in paths)
        {
            try
            {
                var fullPath = Path.GetFullPath(path);

                if (File.Exists(fullPath))
                {
                    if (fullPath.EndsWith(fileExtension, StringComparison.OrdinalIgnoreCase))
                    {
                        files.Add(fullPath);
                    }
                    else
                    {
                        Console.WriteLine($"Warning: Skipping non-{fileExtension} file: {fullPath}");
                    }
                }
                else
                {
                    shouldShowCount = true;

                    if (Directory.Exists(fullPath))
                    {
                        var filesInDir =
                            Directory.GetFiles(
                                fullPath,
                                "*" + fileExtension,
                                SearchOption.AllDirectories);

                        files.AddRange(filesInDir);
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

        if (files.Count is 0)
        {
            Console.WriteLine($"No {fileExtension} files found.");
            return 0;
        }

        files.Sort(StringComparer.Ordinal);

        var alreadyFormatted = new ConcurrentBag<string>();
        var needsFormatting = new ConcurrentBag<(string path, string formattedContent)>();
        var parseErrors = new ConcurrentBag<(string path, string error)>();

        Parallel.ForEach(
            files,
            (filePath) =>
            {
                try
                {
                    var originalContent = File.ReadAllText(filePath);
                    var result = formatFile(originalContent);

                    switch (result)
                    {
                        case FormatFileResult.Error errorResult:
                            parseErrors.Add((filePath, errorResult.ErrorText));
                            break;

                        case FormatFileResult.Stable:
                            alreadyFormatted.Add(filePath);
                            break;

                        case FormatFileResult.Changed changedResult:
                            needsFormatting.Add((filePath, changedResult.FormattedText));
                            break;
                    }
                }
                catch (Exception ex)
                {
                    parseErrors.Add((filePath, ex.Message));
                }
            });

        var sortedAlreadyFormatted =
            alreadyFormatted
            .OrderBy(p => p, StringComparer.Ordinal)
            .ToImmutableList();

        var sortedParseErrors =
            parseErrors
            .OrderBy(e => e.path, StringComparer.Ordinal)
            .ToImmutableList();

        var sortedNeedsFormatting =
            needsFormatting
            .OrderBy(f => f.path, StringComparer.Ordinal)
            .ToImmutableList();

        var showDetailedOverview = files.Count >= MinFilesForDetailedOverview;

        if (showDetailedOverview)
        {
            PrintOverviewHeader(
                commandLabel,
                files.Count,
                sortedAlreadyFormatted.Count,
                sortedNeedsFormatting.Count,
                sortedParseErrors.Count,
                verifyNoChanges);
        }

        if (verifyNoChanges)
        {
            if (sortedParseErrors.Count is not 0)
            {
                PrintFilesWithErrors(sortedParseErrors, showDetailedOverview);
                return 200;
            }

            if (sortedNeedsFormatting.Count is not 0)
            {
                PrintFilesNeedingFormatting(
                    [.. sortedNeedsFormatting.Select(f => f.path)],
                    showDetailedOverview);

                return 100;
            }

            PrintSuccessMessage(sortedAlreadyFormatted.Count, verifyNoChanges);
            return 0;
        }

        if (sortedParseErrors.Count is not 0)
        {
            PrintFilesWithErrors(sortedParseErrors, showDetailedOverview);
            return 200;
        }

        if (!showDetailedOverview && sortedAlreadyFormatted.Count is not 0)
        {
            foreach (var path in sortedAlreadyFormatted)
            {
                Console.WriteLine(path);
            }
        }

        if (sortedNeedsFormatting.Count is 0)
        {
            PrintSuccessMessage(files.Count, verifyNoChanges);
            return 0;
        }
        else
        {
            PrintFilesNeedingFormatting(
                [.. sortedNeedsFormatting.Select(f => f.path)],
                showDetailedOverview);

            if (!skipPrompt)
            {
                Console.WriteLine(
                    "\nAre you sure you want to overwrite these files with formatted versions? (y/n)");

                var response =
                    Console.ReadLine()
                    ?.Trim()
                    .ToLowerInvariant();

                if (response is not "y" and not "yes")
                {
                    Console.WriteLine("Formatting cancelled.");
                    return 0;
                }
            }

            foreach (var (path, formattedContent) in sortedNeedsFormatting)
            {
                File.WriteAllText(path, formattedContent);
            }

            Console.WriteLine(
                $"\n✓ Formatted {sortedNeedsFormatting.Count} file{(sortedNeedsFormatting.Count is 1 ? "" : "s")}.");
        }

        return 0;
    }


    private static void PrintOverviewHeader(
        string commandLabel,
        int totalFiles,
        int alreadyFormattedCount,
        int needsFormattingCount,
        int parseErrorCount,
        bool verifyMode)
    {
        var width = GetConsoleWidth() ?? 80;
        var innerWidth = width - 2;

        const int NumberEndColumn = 33;

        static string FormatLine(string label, int number, string? suffix, int numberEndCol)
        {
            var numStr = number.ToString();
            var spacesNeeded = numberEndCol - label.Length - numStr.Length;

            if (spacesNeeded < 1)
                spacesNeeded = 1;

            return label + new string(' ', spacesNeeded) + numStr + (suffix ?? "");
        }

        Console.WriteLine("╔" + new string('═', innerWidth) + "╗");
        Console.WriteLine("║" + PadCenter(commandLabel + " Summary", innerWidth) + "║");
        Console.WriteLine("╠" + new string('═', innerWidth) + "╣");

        Console.WriteLine(
            "║" + PadRight(FormatLine("  Total files scanned:", totalFiles, null, NumberEndColumn), innerWidth) + "║");

        Console.WriteLine(
            "║" + PadRight(FormatLine("  Already formatted:", alreadyFormattedCount, "  ✓", NumberEndColumn), innerWidth) + "║");

        Console.WriteLine(
            "║" +
            PadRight(
                FormatLine(
                    "  Need formatting:",
                    needsFormattingCount,
                    "  " + (needsFormattingCount > 0 ? "○" : "✓"),
                    NumberEndColumn),
                innerWidth) + "║");

        if (parseErrorCount > 0)
        {
            Console.WriteLine(
                "║" + PadRight(FormatLine("  Syntax errors:", parseErrorCount, "  ✗", NumberEndColumn), innerWidth) + "║");
        }

        Console.WriteLine("╚" + new string('═', innerWidth) + "╝");
        Console.WriteLine("");
    }


    private static void PrintFilesWithErrors(
        IReadOnlyList<(string path, string error)> errors,
        bool showGrouped)
    {
        var width = GetConsoleWidth() ?? 80;

        Console.WriteLine(new string('═', width));
        Console.WriteLine(" ✗ FILES WITH ERRORS");
        Console.WriteLine(new string('═', width));
        Console.WriteLine("");

        if (showGrouped && errors.Count >= MinFilesForDetailedOverview)
        {
            var groupedByDir =
                errors
                .GroupBy(e => Path.GetDirectoryName(e.path) ?? "")
                .OrderBy(g => g.Key, StringComparer.Ordinal);

            foreach (var group in groupedByDir)
            {
                var displayDir =
                    string.IsNullOrEmpty(group.Key)
                    ?
                    "."
                    :
                    group.Key.Replace('\\', '/');

                Console.WriteLine($"┌─ {displayDir}/");

                foreach (var (filePath, error) in group.OrderBy(e => Path.GetFileName(e.path), StringComparer.Ordinal))
                {
                    Console.WriteLine($"│  ✗ {Path.GetFileName(filePath)}");
                    Console.WriteLine($"│      Error: {error}");
                }

                Console.WriteLine("└─");
                Console.WriteLine("");
            }
        }
        else
        {
            foreach (var (filePath, error) in errors.OrderBy(e => e.path, StringComparer.Ordinal))
            {
                Console.WriteLine($"✗ {filePath}");
                Console.WriteLine($"  Error: {error}");
            }

            Console.WriteLine("");
        }
    }


    private static void PrintFilesNeedingFormatting(
        IReadOnlyList<string> files,
        bool showGrouped)
    {
        var width = GetConsoleWidth() ?? 80;

        Console.WriteLine(new string('═', width));
        Console.WriteLine($" ○ FILES NEEDING FORMATTING ({files.Count})");
        Console.WriteLine(new string('═', width));
        Console.WriteLine("");

        if (showGrouped && files.Count >= MinFilesForDetailedOverview)
        {
            var groupedByDir =
                files
                .GroupBy(f => Path.GetDirectoryName(f) ?? "")
                .OrderBy(g => g.Key, StringComparer.Ordinal);

            foreach (var group in groupedByDir)
            {
                var displayDir =
                    string.IsNullOrEmpty(group.Key)
                    ?
                    "."
                    :
                    group.Key.Replace('\\', '/');

                var fileCount = group.Count();

                Console.WriteLine($"┌─ {displayDir}/ ({fileCount} file{(fileCount is 1 ? "" : "s")})");

                foreach (var filePath in group.OrderBy(f => Path.GetFileName(f), StringComparer.Ordinal))
                {
                    Console.WriteLine($"│  ○ {Path.GetFileName(filePath)}");
                }

                Console.WriteLine("└─");
                Console.WriteLine("");
            }
        }
        else
        {
            foreach (var filePath in files.OrderBy(f => f, StringComparer.Ordinal))
            {
                Console.WriteLine($"○ {filePath}");
            }
        }
    }


    private static void PrintSuccessMessage(int fileCount, bool verifyMode)
    {
        Console.WriteLine("");

        if (fileCount is 1)
        {
            Console.WriteLine("✓ File is already properly formatted.");
        }
        else
        {
            Console.WriteLine($"✓ All {fileCount} file(s) are already properly formatted.");
        }

        if (verifyMode)
        {
            Console.WriteLine("  Verification passed.");
        }
    }


    private static int? GetConsoleWidth()
    {
        try
        {
            return Console.WindowWidth;
        }
        catch
        {
            return null;
        }
    }


    private static string PadCenter(string text, int width)
    {
        if (text.Length >= width)
            return text[..width];

        var totalPadding = width - text.Length;
        var leftPadding = totalPadding / 2;
        var rightPadding = totalPadding - leftPadding;

        return new string(' ', leftPadding) + text + new string(' ', rightPadding);
    }


    private static string PadRight(string text, int width)
    {
        if (text.Length >= width)
            return text[..width];

        return text + new string(' ', width - text.Length);
    }
}
