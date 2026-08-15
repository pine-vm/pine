using Spectre.Console;
using Spectre.Console.Rendering;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.CommandLine;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace Pine.CLI;

public sealed record FormatFileDiagnostic(
    string Message,
    int? Line = null,
    int? Column = null)
{
    public string RenderText() =>
        Line is { } line && Column is { } column
        ?
        $"{line}:{column}: {Message}"
        :
        Message;
}


public abstract record FormatFileResult
{
    private FormatFileResult() { }


    public sealed record Error(string ErrorText) : FormatFileResult;


    public sealed record Stable(
        IReadOnlyList<FormatFileDiagnostic> Diagnostics) : FormatFileResult
    {
        public Stable()
            : this([])
        {
        }
    }


    public sealed record Changed(
        string FormattedText,
        IReadOnlyList<FormatFileDiagnostic> Diagnostics) : FormatFileResult
    {
        public Changed(string formattedText)
            : this(formattedText, [])
        {
        }
    }
}


public static class FormatCommandTheme
{
    public static Style Default { get; } =
        new(foreground: Color.Default);

    public static Style Heading { get; } =
        new(foreground: Color.Default, decoration: Decoration.Bold);

    // ANSI base colors are resolved through the user's terminal palette,
    // keeping status output legible with both light and dark terminal themes.
    public static Style Success { get; } =
        new(foreground: Color.Green);

    public static Style Warning { get; } =
        new(foreground: Color.Yellow);

    public static Style Error { get; } =
        new(foreground: Color.Red);
}


public enum FormatCommandColorMode
{
    Auto,
    Always,
    Never,
}


public static class FormatCommandShared
{
    public const int MinFilesForDetailedOverview = 5;

    public const string ColorEnvironmentVariable = "PINE_TERM_COLOR";


    public static Option<FormatCommandColorMode?> CreateColorOption() =>
        new("--color")
        {
            Description =
            "Color output: auto, always, or never. Overrides PINE_TERM_COLOR.",
            Arity = ArgumentArity.ExactlyOne,
        };


    public static int Execute(
        string[] paths,
        string fileExtension,
        Func<string, FormatFileResult> formatFile,
        bool skipPrompt,
        bool verifyNoChanges,
        string commandLabel,
        FormatCommandColorMode? colorMode = null,
        IAnsiConsole? console = null,
        IAnsiConsole? errorConsole = null,
        Func<string?>? readLine = null)
    {
        FormatCommandColorMode resolvedColorMode;

        try
        {
            resolvedColorMode =
                ResolveColorMode(
                    colorMode,
                    Environment.GetEnvironmentVariable(ColorEnvironmentVariable));
        }
        catch (ArgumentException exception)
        {
            errorConsole ??=
                CreateSystemConsole(
                    Console.Error,
                    FormatCommandColorMode.Auto);

            WriteLabelledLine(
                errorConsole,
                "Error:",
                FormatCommandTheme.Error,
                exception.Message);

            return 1;
        }

        console ??= CreateSystemConsole(Console.Out, resolvedColorMode);
        errorConsole ??= CreateSystemConsole(Console.Error, resolvedColorMode);
        readLine ??= Console.ReadLine;

        var files = new List<string>();

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
                        WriteLabelledLine(
                            console,
                            "Warning:",
                            FormatCommandTheme.Warning,
                            $"Skipping non-{fileExtension} file: {fullPath}");
                    }
                }
                else if (Directory.Exists(fullPath))
                {
                    files.AddRange(
                        Directory.GetFiles(
                            fullPath,
                            "*" + fileExtension,
                            SearchOption.AllDirectories));
                }
                else
                {
                    WriteLabelledLine(
                        errorConsole,
                        "Error:",
                        FormatCommandTheme.Error,
                        $"Path not found: {fullPath}");

                    return 1;
                }
            }
            catch (Exception ex)
            {
                WriteLabelledLine(
                    errorConsole,
                    "Error:",
                    FormatCommandTheme.Error,
                    $"Error processing path '{path}': {ex.Message}");

                return 1;
            }
        }

        if (files.Count is 0)
        {
            console.WriteLine($"No {fileExtension} files found.");
            return 0;
        }

        files.Sort(StringComparer.Ordinal);

        var alreadyFormatted = new ConcurrentBag<string>();
        var needsFormatting = new ConcurrentBag<(string path, string formattedContent)>();
        var formatErrors = new ConcurrentBag<(string path, string error)>();
        var diagnostics = new ConcurrentBag<(string path, IReadOnlyList<FormatFileDiagnostic> diagnostics)>();

        Parallel.ForEach(
            files,
            filePath =>
            {
                try
                {
                    var originalContent = File.ReadAllText(filePath);
                    var result = formatFile(originalContent);

                    switch (result)
                    {
                        case FormatFileResult.Error errorResult:
                            formatErrors.Add((filePath, errorResult.ErrorText));
                            break;

                        case FormatFileResult.Stable stableResult:
                            alreadyFormatted.Add(filePath);
                            AddDiagnostics(filePath, stableResult.Diagnostics);
                            break;

                        case FormatFileResult.Changed changedResult:
                            needsFormatting.Add((filePath, changedResult.FormattedText));
                            AddDiagnostics(filePath, changedResult.Diagnostics);
                            break;
                    }
                }
                catch (Exception ex)
                {
                    formatErrors.Add((filePath, ex.Message));
                }
            });

        var sortedAlreadyFormatted =
            alreadyFormatted
            .OrderBy(path => path, StringComparer.Ordinal)
            .ToList();

        var sortedFormatErrors =
            formatErrors
            .OrderBy(error => error.path, StringComparer.Ordinal)
            .ToList();

        var sortedNeedsFormatting =
            needsFormatting
            .OrderBy(file => file.path, StringComparer.Ordinal)
            .ToList();

        var sortedDiagnostics =
            diagnostics
            .OrderBy(file => file.path, StringComparer.Ordinal)
            .ToList();

        var showDetailedOverview = files.Count >= MinFilesForDetailedOverview;

        if (showDetailedOverview)
        {
            WriteOverview(
                console,
                commandLabel,
                files.Count,
                sortedAlreadyFormatted.Count,
                sortedNeedsFormatting.Count,
                sortedFormatErrors.Count + sortedDiagnostics.Count);
        }

        if (verifyNoChanges)
        {
            if (sortedFormatErrors.Count is not 0)
            {
                WriteFilesWithErrors(console, sortedFormatErrors, showDetailedOverview);
                return 200;
            }

            if (sortedDiagnostics.Count is not 0)
            {
                WriteFileDiagnostics(console, sortedDiagnostics);
                return 200;
            }

            if (sortedNeedsFormatting.Count is not 0)
            {
                WriteFilesNeedingFormatting(
                    console,
                    [.. sortedNeedsFormatting.Select(file => file.path)],
                    showDetailedOverview);

                return 100;
            }

            WriteSuccessMessage(console, sortedAlreadyFormatted.Count, verifyNoChanges);
            return 0;
        }

        if (sortedFormatErrors.Count is not 0)
        {
            WriteFilesWithErrors(console, sortedFormatErrors, showDetailedOverview);
            return 200;
        }

        if (sortedDiagnostics.Count is not 0)
        {
            WriteFileDiagnostics(console, sortedDiagnostics);
        }

        if (!showDetailedOverview)
        {
            foreach (var path in sortedAlreadyFormatted)
            {
                console.Profile.Out.Writer.WriteLine(path);
            }
        }

        if (sortedNeedsFormatting.Count is 0)
        {
            if (sortedDiagnostics.Count is not 0)
            {
                console.WriteLine();

                WriteStatusLine(
                    console,
                    "⚠",
                    FormatCommandTheme.Warning,
                    $"{sortedDiagnostics.Count} file(s) contain syntax errors (see above).");

                return 200;
            }

            WriteSuccessMessage(console, files.Count, verifyNoChanges);
            return 0;
        }

        WriteFilesNeedingFormatting(
            console,
            [.. sortedNeedsFormatting.Select(file => file.path)],
            showDetailedOverview);

        if (!skipPrompt)
        {
            console.WriteLine();

            var overwrite = ConfirmOverwrite(console, readLine);

            if (!overwrite)
            {
                console.WriteLine("Formatting cancelled.");
                return 0;
            }
        }

        foreach (var (path, formattedContent) in sortedNeedsFormatting)
        {
            File.WriteAllText(path, formattedContent);
        }

        console.WriteLine();

        WriteStatusLine(
            console,
            "✓",
            FormatCommandTheme.Success,
            $"Formatted {sortedNeedsFormatting.Count} file{(sortedNeedsFormatting.Count is 1 ? "" : "s")}.");

        if (sortedDiagnostics.Count is not 0)
        {
            WriteStatusLine(
                console,
                "⚠",
                FormatCommandTheme.Warning,
                $"{sortedDiagnostics.Count} file(s) still contain syntax errors (see above).");
        }

        return 0;

        void AddDiagnostics(
            string filePath,
            IReadOnlyList<FormatFileDiagnostic> fileDiagnostics)
        {
            if (fileDiagnostics.Count is not 0)
            {
                diagnostics.Add((filePath, fileDiagnostics));
            }
        }
    }


    public static FormatCommandColorMode ResolveColorMode(
        FormatCommandColorMode? commandLineValue,
        string? environmentValue)
    {
        if (commandLineValue is { } colorMode)
        {
            return colorMode;
        }

        if (string.IsNullOrWhiteSpace(environmentValue))
        {
            return FormatCommandColorMode.Auto;
        }

        if (Enum.TryParse<FormatCommandColorMode>(
            environmentValue,
            ignoreCase: true,
            out var environmentColorMode) &&
            Enum.IsDefined(environmentColorMode))
        {
            return environmentColorMode;
        }

        throw new ArgumentException(
            $"Unsupported value '{environmentValue}' for {ColorEnvironmentVariable}. " +
            "Expected auto, always, or never.");
    }


    public static AnsiSupport AnsiSupportForColorMode(
        FormatCommandColorMode colorMode) =>
        colorMode switch
        {
            FormatCommandColorMode.Auto => AnsiSupport.Detect,
            FormatCommandColorMode.Always => AnsiSupport.Yes,
            FormatCommandColorMode.Never => AnsiSupport.No,

            _ =>
            throw new ArgumentOutOfRangeException(nameof(colorMode)),
        };


    public static ColorSystemSupport ColorSystemSupportForColorMode(
        FormatCommandColorMode colorMode) =>
        colorMode switch
        {
            FormatCommandColorMode.Auto => ColorSystemSupport.Detect,
            FormatCommandColorMode.Always => ColorSystemSupport.Standard,
            FormatCommandColorMode.Never => ColorSystemSupport.NoColors,

            _ =>
            throw new ArgumentOutOfRangeException(nameof(colorMode)),
        };


    public static ConfirmationPrompt CreateOverwritePrompt() =>
        new(
            "Are you sure you want to overwrite these files with formatted versions?")
        {
            ChoicesStyle = FormatCommandTheme.Heading,
            DefaultValue = false,
            DefaultValueStyle = FormatCommandTheme.Heading,
        };


    public static void WriteOverview(
        IAnsiConsole console,
        string commandLabel,
        int totalFiles,
        int alreadyFormattedCount,
        int needsFormattingCount,
        int errorCount)
    {
        var table =
            new Table
            {
                Border = TableBorder.Rounded,
                BorderStyle = FormatCommandTheme.Default,
            }
            .Title(
                new TableTitle(
                    Markup.Escape(commandLabel + " Summary"),
                    FormatCommandTheme.Heading))
            .HideHeaders()
            .AddColumn(new TableColumn(new Text("Status")))
            .AddColumn(new TableColumn(new Text("Count")).RightAligned())
            .AddColumn(new TableColumn(new Text("Result")));

        table.AddRow(
            new Text("Total files scanned:"),
            CountText(totalFiles),
            new Text(""));

        table.AddRow(
            new Text("Already formatted:", FormatCommandTheme.Success),
            CountText(alreadyFormattedCount, FormatCommandTheme.Success),
            new Text("✓", FormatCommandTheme.Success));

        var needsFormattingStyle =
            needsFormattingCount is 0
            ?
            FormatCommandTheme.Success
            :
            FormatCommandTheme.Warning;

        table.AddRow(
            new Text("Need formatting:", needsFormattingStyle),
            CountText(needsFormattingCount, needsFormattingStyle),
            new Text(
                needsFormattingCount is 0 ? "✓" : "○",
                needsFormattingStyle));

        if (errorCount is not 0)
        {
            table.AddRow(
                new Text("Syntax errors:", FormatCommandTheme.Error),
                CountText(errorCount, FormatCommandTheme.Error),
                new Text("✗", FormatCommandTheme.Error));
        }

        console.Write(table);
        console.WriteLine();
    }


    public static void WriteFilesWithErrors(
        IAnsiConsole console,
        IReadOnlyList<(string path, string error)> errors,
        bool showGrouped)
    {
        WriteRule(console, "FILES WITH ERRORS", FormatCommandTheme.Error);
        console.WriteLine();

        if (showGrouped &&
            errors.Count >= MinFilesForDetailedOverview &&
            console.Profile.Out.IsTerminal)
        {
            var groupedByDirectory =
                errors
                .GroupBy(error => Path.GetDirectoryName(error.path) ?? "")
                .OrderBy(group => group.Key, StringComparer.Ordinal);

            foreach (var group in groupedByDirectory)
            {
                var tree =
                    new Tree(
                        new Text(
                            DisplayDirectory(group.Key) + "/",
                            FormatCommandTheme.Heading))
                    {
                        Guide = TreeGuide.Line,
                        Style = FormatCommandTheme.Default,
                    };

                foreach (var (filePath, error) in
                    group.OrderBy(
                        item => Path.GetFileName(item.path),
                        StringComparer.Ordinal))
                {
                    var fileNode =
                        tree.AddNode(
                            StatusText(
                                "✗",
                                FormatCommandTheme.Error,
                                Path.GetFileName(filePath)));

                    fileNode.AddNode(new Text("Error: " + error));
                }

                console.Write(tree);
                console.WriteLine();
            }
        }
        else if (showGrouped && errors.Count >= MinFilesForDetailedOverview)
        {
            var groupedByDirectory =
                errors
                .GroupBy(error => Path.GetDirectoryName(error.path) ?? "")
                .OrderBy(group => group.Key, StringComparer.Ordinal);

            foreach (var group in groupedByDirectory)
            {
                console.Profile.Out.Writer.WriteLine(
                    DisplayDirectory(group.Key) + "/");

                foreach (var (filePath, error) in
                    group.OrderBy(
                        item => Path.GetFileName(item.path),
                        StringComparer.Ordinal))
                {
                    WriteStatusLine(
                        console,
                        "✗",
                        FormatCommandTheme.Error,
                        Path.GetFileName(filePath));

                    console.Profile.Out.Writer.WriteLine("  Error: " + error);
                }

                console.WriteLine();
            }
        }
        else
        {
            foreach (var (filePath, error) in
                errors.OrderBy(item => item.path, StringComparer.Ordinal))
            {
                WriteStatusLine(
                    console,
                    "✗",
                    FormatCommandTheme.Error,
                    filePath);

                console.Profile.Out.Writer.WriteLine("  Error: " + error);
            }

            console.WriteLine();
        }
    }


    public static void WriteFileDiagnostics(
        IAnsiConsole console,
        IReadOnlyList<(string path, IReadOnlyList<FormatFileDiagnostic> diagnostics)> filesWithDiagnostics)
    {
        var totalDiagnostics =
            filesWithDiagnostics.Sum(file => file.diagnostics.Count);

        WriteRule(
            console,
            $"SYNTAX ERRORS ({totalDiagnostics.ToString(CultureInfo.InvariantCulture)})",
            FormatCommandTheme.Error);

        console.WriteLine();

        foreach (var (filePath, fileDiagnostics) in
            filesWithDiagnostics.OrderBy(file => file.path, StringComparer.Ordinal))
        {
            if (!console.Profile.Out.IsTerminal)
            {
                WriteStatusLine(
                    console,
                    "✗",
                    FormatCommandTheme.Error,
                    filePath);

                foreach (var diagnostic in
                    fileDiagnostics
                    .OrderBy(item => item.Line)
                    .ThenBy(item => item.Column))
                {
                    console.Profile.Out.Writer.WriteLine(
                        "  " + diagnostic.RenderText());
                }

                console.WriteLine();
                continue;
            }

            var tree =
                new Tree(
                    StatusText(
                        "✗",
                        FormatCommandTheme.Error,
                        filePath))
                {
                    Guide = TreeGuide.Line,
                    Style = FormatCommandTheme.Default,
                };

            foreach (var diagnostic in
                fileDiagnostics
                .OrderBy(item => item.Line)
                .ThenBy(item => item.Column))
            {
                tree.AddNode(new Text(diagnostic.RenderText()));
            }

            console.Write(tree);
            console.WriteLine();
        }
    }


    public static void WriteFilesNeedingFormatting(
        IAnsiConsole console,
        IReadOnlyList<string> files,
        bool showGrouped)
    {
        WriteRule(
            console,
            $"FILES NEEDING FORMATTING ({files.Count.ToString(CultureInfo.InvariantCulture)})",
            FormatCommandTheme.Warning);

        console.WriteLine();

        if (showGrouped &&
            files.Count >= MinFilesForDetailedOverview &&
            console.Profile.Out.IsTerminal)
        {
            var groupedByDirectory =
                files
                .GroupBy(path => Path.GetDirectoryName(path) ?? "")
                .OrderBy(group => group.Key, StringComparer.Ordinal);

            foreach (var group in groupedByDirectory)
            {
                var fileCount = group.Count();
                var fileLabel = fileCount is 1 ? "file" : "files";

                var tree =
                    new Tree(
                        new Text(
                            $"{DisplayDirectory(group.Key)}/ ({fileCount.ToString(CultureInfo.InvariantCulture)} {fileLabel})",
                            FormatCommandTheme.Heading))
                    {
                        Guide = TreeGuide.Line,
                        Style = FormatCommandTheme.Default,
                    };

                foreach (var filePath in
                    group.OrderBy(Path.GetFileName, StringComparer.Ordinal))
                {
                    tree.AddNode(
                        StatusText(
                            "○",
                            FormatCommandTheme.Warning,
                            Path.GetFileName(filePath)));
                }

                console.Write(tree);
                console.WriteLine();
            }
        }
        else if (showGrouped && files.Count >= MinFilesForDetailedOverview)
        {
            var groupedByDirectory =
                files
                .GroupBy(path => Path.GetDirectoryName(path) ?? "")
                .OrderBy(group => group.Key, StringComparer.Ordinal);

            foreach (var group in groupedByDirectory)
            {
                var fileCount = group.Count();
                var fileLabel = fileCount is 1 ? "file" : "files";

                console.Profile.Out.Writer.WriteLine(
                    $"{DisplayDirectory(group.Key)}/ " +
                    $"({fileCount.ToString(CultureInfo.InvariantCulture)} {fileLabel})");

                foreach (var filePath in
                    group.OrderBy(Path.GetFileName, StringComparer.Ordinal))
                {
                    WriteStatusLine(
                        console,
                        "○",
                        FormatCommandTheme.Warning,
                        Path.GetFileName(filePath));
                }

                console.WriteLine();
            }
        }
        else
        {
            foreach (var filePath in files.OrderBy(path => path, StringComparer.Ordinal))
            {
                WriteStatusLine(
                    console,
                    "○",
                    FormatCommandTheme.Warning,
                    filePath);
            }
        }
    }


    public static void WriteSuccessMessage(
        IAnsiConsole console,
        int fileCount,
        bool verifyMode)
    {
        console.WriteLine();

        WriteStatusLine(
            console,
            "✓",
            FormatCommandTheme.Success,
            fileCount is 1
            ?
            "File is already properly formatted."
            :
            $"All {fileCount.ToString(CultureInfo.InvariantCulture)} file(s) are already properly formatted.");

        if (verifyMode)
        {
            console.WriteLine("  Verification passed.");
        }
    }


    private static IAnsiConsole CreateSystemConsole(
        TextWriter writer,
        FormatCommandColorMode colorMode) =>
        AnsiConsole.Create(
            new AnsiConsoleSettings
            {
                Ansi = AnsiSupportForColorMode(colorMode),
                ColorSystem = ColorSystemSupportForColorMode(colorMode),
                Out = new AnsiConsoleOutput(writer),
            });


    private static Text CountText(int count) =>
        new(count.ToString(CultureInfo.InvariantCulture));


    private static Text CountText(int count, Style style) =>
        new(count.ToString(CultureInfo.InvariantCulture), style);


    private static string DisplayDirectory(string directory) =>
        string.IsNullOrEmpty(directory)
        ?
        "."
        :
        directory.Replace('\\', '/');


    private static IRenderable StatusText(
        string symbol,
        Style symbolStyle,
        string message) =>
        new Markup(
            $"[{symbolStyle.ToMarkup()}]{Markup.Escape(symbol + " " + message)}[/]");


    private static void WriteStatusLine(
        IAnsiConsole console,
        string symbol,
        Style symbolStyle,
        string message)
    {
        if (console.Profile.Out.IsTerminal)
        {
            console.Write(new Text(symbol + " " + message, symbolStyle));
            console.WriteLine();
            return;
        }

        console.Write(new Text(symbol, symbolStyle));
        console.Profile.Out.Writer.WriteLine(" " + message);
    }


    private static void WriteLabelledLine(
        IAnsiConsole console,
        string label,
        Style labelStyle,
        string message)
    {
        console.Write(new Text(label, labelStyle));
        console.Profile.Out.Writer.WriteLine(" " + message);
    }


    private static bool ConfirmOverwrite(
        IAnsiConsole console,
        Func<string?> readLine)
    {
        if (console.Profile.Capabilities.Interactive)
        {
            return console.Prompt(CreateOverwritePrompt());
        }

        console.Profile.Out.Writer.WriteLine(
            "Are you sure you want to overwrite these files with formatted versions? (y/n)");

        var response =
            readLine()
            ?.Trim();

        return
            string.Equals(response, "y", StringComparison.OrdinalIgnoreCase) ||
            string.Equals(response, "yes", StringComparison.OrdinalIgnoreCase);
    }


    private static void WriteRule(
        IAnsiConsole console,
        string title,
        Style style) =>
        console.Write(
            new Rule(Markup.Escape(title))
            {
                Border = BoxBorder.Double,
                Justification = Justify.Left,
                Style = style,
            });
}
