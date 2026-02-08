using Pine.Core;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Pine.Elm.CLI;

/// <summary>
/// Represents the result of formatting a single Elm file.
/// This is a choice type where each variant carries its specific data.
/// </summary>
public abstract record ElmFormatFileResult
{
    private ElmFormatFileResult() { }

    /// <summary>File had a syntax error and could not be parsed.</summary>
    public sealed record ParseError(string ErrorText) : ElmFormatFileResult;

    /// <summary>File was already properly formatted (no changes needed).</summary>
    public sealed record FormatStable : ElmFormatFileResult;

    /// <summary>File needs formatting (content differs from formatted version).</summary>
    public sealed record FormatChanged(string FormattedModuleText) : ElmFormatFileResult;
}

public class ElmFormatCommand
{
    /// <summary>
    /// Minimum number of files before showing detailed overview with grouping.
    /// </summary>
    public const int MinFilesForDetailedOverview = 5;

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

        // Sort collections for consistent output
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

        // For larger file counts, show a nice overview
        var showDetailedOverview = elmFiles.Count >= MinFilesForDetailedOverview;

        // Print header summary for larger operations
        if (showDetailedOverview)
        {
            PrintOverviewHeader(
                elmFiles.Count,
                sortedAlreadyFormatted.Count,
                sortedNeedsFormatting.Count,
                sortedParseErrors.Count,
                verifyNoChanges);
        }

        // Handle verify-no-changes mode
        if (verifyNoChanges)
        {
            if (sortedParseErrors.Count is not 0)
            {
                PrintFilesWithErrors(sortedParseErrors, showDetailedOverview);
                return 200;
            }

            if (sortedNeedsFormatting.Count is not 0)
            {
                PrintFilesNeedingFormatting([.. sortedNeedsFormatting.Select(f => f.path)], showDetailedOverview);
                return 100;
            }

            PrintSuccessMessage(sortedAlreadyFormatted.Count, verifyNoChanges);
            return 0;
        }

        // Report parse errors
        if (sortedParseErrors.Count is not 0)
        {
            PrintFilesWithErrors(sortedParseErrors, showDetailedOverview);
            return 200;
        }

        // Report already formatted files (for single file mode)
        if (!showDetailedOverview && sortedAlreadyFormatted.Count is not 0)
        {
            foreach (var path in sortedAlreadyFormatted)
            {
                Console.WriteLine(path);
            }
        }

        // Report files that need formatting
        if (sortedNeedsFormatting.Count is 0)
        {
            PrintSuccessMessage(elmFiles.Count, verifyNoChanges);
            return 0;
        }
        else
        {
            PrintFilesNeedingFormatting([.. sortedNeedsFormatting.Select(f => f.path)], showDetailedOverview);

            // Prompt for confirmation unless --yes was specified
            if (!skipPrompt)
            {
                Console.WriteLine("\nAre you sure you want to overwrite these files with formatted versions? (y/n)");

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

            // Write formatted content to files
            foreach (var (path, formattedContent) in sortedNeedsFormatting)
            {
                File.WriteAllText(path, formattedContent);
            }

            Console.WriteLine($"\n✓ Formatted {sortedNeedsFormatting.Count} file(s).");
        }

        return 0;
    }

    /// <summary>
    /// Default width for rendered output. This matches common terminal defaults.
    /// </summary>
    public const int DefaultRenderWidth = 66;

    /// <summary>
    /// Renders the overview header with statistics about the formatting operation.
    /// </summary>
    /// <param name="writeLine">Delegate to write a line of output.</param>
    /// <param name="totalFiles">Total number of files scanned.</param>
    /// <param name="alreadyFormattedCount">Number of files already formatted.</param>
    /// <param name="needsFormattingCount">Number of files needing formatting.</param>
    /// <param name="parseErrorCount">Number of files with parse errors.</param>
    /// <param name="verifyMode">Whether in verify mode.</param>
    /// <param name="width">The total width of the rendered box (including borders).</param>
    public static void RenderOverviewHeader(
        Action<string> writeLine,
        int totalFiles,
        int alreadyFormattedCount,
        int needsFormattingCount,
        int parseErrorCount,
        bool verifyMode,
        int width)
    {
        var innerWidth = width - 2; // Subtract 2 for the border characters

        // All numbers right-align so their last digit ends at column 32 (0-indexed from start of inner content)
        // Example: "10" ends at position 32 (positions 31-32), "8" at position 32
        const int NumberEndColumn = 33; // Position after last digit

        static string FormatLine(string label, int number, string? suffix, int numberEndCol)
        {
            var numStr = number.ToString();
            var spacesNeeded = numberEndCol - label.Length - numStr.Length;
            return label + new string(' ', spacesNeeded) + numStr + (suffix ?? "");
        }

        writeLine("╔" + new string('═', innerWidth) + "╗");
        writeLine("║" + PadCenter("elm-format Summary", innerWidth) + "║");
        writeLine("╠" + new string('═', innerWidth) + "╣");

        writeLine("║" + PadRight(FormatLine("  Total files scanned:", totalFiles, null, NumberEndColumn), innerWidth) + "║");
        writeLine("║" + PadRight(FormatLine("  Already formatted:", alreadyFormattedCount, "  ✓", NumberEndColumn), innerWidth) + "║");
        writeLine("║" + PadRight(FormatLine("  Need formatting:", needsFormattingCount, "  " + (needsFormattingCount > 0 ? "○" : "✓"), NumberEndColumn), innerWidth) + "║");

        if (parseErrorCount > 0)
        {
            writeLine("║" + PadRight(FormatLine("  Syntax errors:", parseErrorCount, "  ✗", NumberEndColumn), innerWidth) + "║");
        }

        writeLine("╚" + new string('═', innerWidth) + "╝");
        writeLine("");
    }

    /// <summary>
    /// Renders the overview header to a string with configurable width.
    /// </summary>
    public static string RenderOverviewHeaderToString(
        int totalFiles,
        int alreadyFormattedCount,
        int needsFormattingCount,
        int parseErrorCount,
        bool verifyMode,
        int width)
    {
        var sb = new StringBuilder();
        RenderOverviewHeader(
            line => sb.Append(line + "\n"),
            totalFiles,
            alreadyFormattedCount,
            needsFormattingCount,
            parseErrorCount,
            verifyMode,
            width);
        return sb.ToString();
    }

    /// <summary>
    /// Renders the overview header to a string with default width.
    /// </summary>
    public static string RenderOverviewHeaderToString(
        int totalFiles,
        int alreadyFormattedCount,
        int needsFormattingCount,
        int parseErrorCount,
        bool verifyMode) =>
        RenderOverviewHeaderToString(
            totalFiles,
            alreadyFormattedCount,
            needsFormattingCount,
            parseErrorCount,
            verifyMode,
            DefaultRenderWidth);

    /// <summary>
    /// Prints the overview header to the console.
    /// </summary>
    private static void PrintOverviewHeader(
        int totalFiles,
        int alreadyFormattedCount,
        int needsFormattingCount,
        int parseErrorCount,
        bool verifyMode) =>
        RenderOverviewHeader(
            Console.WriteLine,
            totalFiles,
            alreadyFormattedCount,
            needsFormattingCount,
            parseErrorCount,
            verifyMode,
            width: GetConsoleWidth() ?? 80);

    /// <summary>
    /// Gets the current console width, or returns the default width if the console is not available.
    /// </summary>
    /// <returns>The console width in characters, or DefaultRenderWidth if unavailable.</returns>
    public static int? GetConsoleWidth()
    {
        try
        {
            // Console.WindowWidth throws if stdout is redirected or no console is attached
            return Console.WindowWidth;
        }
        catch
        {
            return null;
        }
    }

    /// <summary>
    /// Renders the success message directly to the console.
    /// </summary>
    public static void RenderSuccessMessageToConsole(int fileCount, bool verifyMode) =>
        RenderSuccessMessage(Console.WriteLine, fileCount, verifyMode);

    /// <summary>
    /// Pads text with spaces on both sides to center it within a given width.
    /// Uses slightly more padding on the right to match common formatting conventions.
    /// </summary>
    private static string PadCenter(string text, int width)
    {
        if (text.Length >= width)
            return text[..width];
        var totalPadding = width - text.Length;
        // Use floor division for left padding (slightly less), ceiling for right (slightly more)
        var leftPadding = totalPadding / 2;
        var rightPadding = totalPadding - leftPadding;
        return new string(' ', leftPadding) + text + new string(' ', rightPadding);
    }

    /// <summary>
    /// Pads text to the right to fill a given width.
    /// </summary>
    private static string PadRight(string text, int width)
    {
        if (text.Length >= width)
            return text[..width];
        return text + new string(' ', width - text.Length);
    }

    /// <summary>
    /// Renders files with syntax errors, grouped by directory for better navigation.
    /// </summary>
    /// <param name="writeLine">Delegate to write a line of output.</param>
    /// <param name="errors">Dictionary mapping file paths to parse errors.</param>
    /// <param name="showGrouped">Whether to group files by directory.</param>
    /// <param name="width">The total width of the separator lines.</param>
    public static void RenderFilesWithErrors(
        Action<string> writeLine,
        IReadOnlyDictionary<string, ElmFormatFileResult.ParseError> errors,
        bool showGrouped,
        int width)
    {
        writeLine(new string('═', width));
        writeLine(" ✗ FILES WITH SYNTAX ERRORS");
        writeLine(new string('═', width));
        writeLine("");

        var errorsList = errors.ToList();

        if (showGrouped && errorsList.Count >= MinFilesForDetailedOverview)
        {
            // Group by directory for better navigation
            var groupedByDir =
                errorsList
                .GroupBy(e => Path.GetDirectoryName(e.Key) ?? "")
                .OrderBy(g => g.Key, StringComparer.Ordinal);

            foreach (var group in groupedByDir)
            {
                var displayDir =
                    string.IsNullOrEmpty(group.Key)
                    ?
                    "."
                    :
                    group.Key.Replace('\\', '/');

                writeLine($"┌─ {displayDir}/");

                foreach (var (filePath, parseError) in group.OrderBy(e => Path.GetFileName(e.Key), StringComparer.Ordinal))
                {
                    var fileName = Path.GetFileName(filePath);
                    writeLine($"│  ✗ {fileName}");
                    writeLine($"│      Error: {parseError.ErrorText}");
                }

                writeLine("└─");
                writeLine("");
            }
        }
        else
        {
            // Simple list for small number of files
            foreach (var (filePath, parseError) in errorsList.OrderBy(e => e.Key, StringComparer.Ordinal))
            {
                writeLine($"✗ {filePath}");
                writeLine($"  Error: {parseError.ErrorText}");
            }
            writeLine("");
        }
    }

    /// <summary>
    /// Renders files with syntax errors with default width.
    /// </summary>
    public static void RenderFilesWithErrors(
        Action<string> writeLine,
        IReadOnlyDictionary<string, ElmFormatFileResult.ParseError> errors,
        bool showGrouped) =>
        RenderFilesWithErrors(writeLine, errors, showGrouped, DefaultRenderWidth);

    /// <summary>
    /// Renders files with syntax errors to a string with configurable width.
    /// </summary>
    public static string RenderFilesWithErrorsToString(
        IReadOnlyDictionary<string, ElmFormatFileResult.ParseError> errors,
        bool showGrouped,
        int width)
    {
        var sb = new StringBuilder();
        RenderFilesWithErrors(line => sb.Append(line + "\n"), errors, showGrouped, width);
        return sb.ToString();
    }

    /// <summary>
    /// Renders files with syntax errors to a string with default width.
    /// </summary>
    public static string RenderFilesWithErrorsToString(
        IReadOnlyDictionary<string, ElmFormatFileResult.ParseError> errors,
        bool showGrouped) =>
        RenderFilesWithErrorsToString(errors, showGrouped, DefaultRenderWidth);

    /// <summary>
    /// Prints files with syntax errors to the console.
    /// </summary>
    private static void PrintFilesWithErrors(
        IReadOnlyList<(string path, string error)> errors,
        bool showGrouped) =>
        RenderFilesWithErrors(
            Console.WriteLine,
            errors.ToImmutableDictionary(e => e.path, e => new ElmFormatFileResult.ParseError(e.error)),
            showGrouped);

    /// <summary>
    /// Renders files needing formatting, grouped by directory for better navigation.
    /// </summary>
    /// <param name="writeLine">Delegate to write a line of output.</param>
    /// <param name="files">Dictionary mapping file paths to format results.</param>
    /// <param name="showGrouped">Whether to group files by directory.</param>
    /// <param name="width">The total width of the separator lines.</param>
    public static void RenderFilesNeedingFormatting(
        Action<string> writeLine,
        IReadOnlyDictionary<string, ElmFormatFileResult.FormatChanged> files,
        bool showGrouped,
        int width)
    {
        writeLine(new string('═', width));
        writeLine($" ○ FILES NEEDING FORMATTING ({files.Count})");
        writeLine(new string('═', width));
        writeLine("");

        var filesList = files.ToList();

        if (showGrouped && filesList.Count >= MinFilesForDetailedOverview)
        {
            // Group by directory for better navigation
            var groupedByDir = filesList
                .GroupBy(f => Path.GetDirectoryName(f.Key) ?? "")
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
                writeLine($"┌─ {displayDir}/ ({fileCount} file{(fileCount == 1 ? "" : "s")})");

                foreach (var (filePath, _) in group.OrderBy(f => Path.GetFileName(f.Key), StringComparer.Ordinal))
                {
                    var fileName = Path.GetFileName(filePath);
                    writeLine($"│  ○ {fileName}");
                }

                writeLine("└─");
                writeLine("");
            }
        }
        else
        {
            // Simple list for small number of files
            foreach (var (filePath, _) in filesList.OrderBy(f => f.Key, StringComparer.Ordinal))
            {
                writeLine($"○ {filePath}");
            }
        }
    }

    /// <summary>
    /// Renders files needing formatting with default width.
    /// </summary>
    public static void RenderFilesNeedingFormatting(
        Action<string> writeLine,
        IReadOnlyDictionary<string, ElmFormatFileResult.FormatChanged> files,
        bool showGrouped) =>
        RenderFilesNeedingFormatting(writeLine, files, showGrouped, DefaultRenderWidth);

    /// <summary>
    /// Renders files needing formatting to a string with configurable width.
    /// </summary>
    public static string RenderFilesNeedingFormattingToString(
        IReadOnlyDictionary<string, ElmFormatFileResult.FormatChanged> files,
        bool showGrouped,
        int width)
    {
        var sb = new StringBuilder();
        RenderFilesNeedingFormatting(line => sb.Append(line + "\n"), files, showGrouped, width);
        return sb.ToString();
    }

    /// <summary>
    /// Renders files needing formatting to a string with default width.
    /// </summary>
    public static string RenderFilesNeedingFormattingToString(
        IReadOnlyDictionary<string, ElmFormatFileResult.FormatChanged> files,
        bool showGrouped) =>
        RenderFilesNeedingFormattingToString(files, showGrouped, DefaultRenderWidth);

    /// <summary>
    /// Prints files needing formatting to the console.
    /// </summary>
    private static void PrintFilesNeedingFormatting(
        IReadOnlyList<string> files,
        bool showGrouped) =>
        RenderFilesNeedingFormatting(
            Console.WriteLine,
            files.ToImmutableDictionary(f => f, _ => new ElmFormatFileResult.FormatChanged("")),
            showGrouped);

    /// <summary>
    /// Renders a success message when all files are already formatted.
    /// </summary>
    public static void RenderSuccessMessage(
        Action<string> writeLine,
        int fileCount,
        bool verifyMode)
    {
        writeLine("");

        if (fileCount is 1)
        {
            writeLine("✓ File is already properly formatted.");
        }
        else
        {
            writeLine($"✓ All {fileCount} file(s) are already properly formatted.");
        }

        if (verifyMode)
        {
            writeLine("  Verification passed.");
        }
    }

    /// <summary>
    /// Renders a success message to a string.
    /// </summary>
    public static string RenderSuccessMessageToString(int fileCount, bool verifyMode)
    {
        var sb = new StringBuilder();
        RenderSuccessMessage(line => sb.Append(line + "\n"), fileCount, verifyMode);
        return sb.ToString();
    }

    /// <summary>
    /// Prints a success message to the console.
    /// </summary>
    private static void PrintSuccessMessage(int fileCount, bool verifyMode) =>
        RenderSuccessMessage(Console.WriteLine, fileCount, verifyMode);
}
