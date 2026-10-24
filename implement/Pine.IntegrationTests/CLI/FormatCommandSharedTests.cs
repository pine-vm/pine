using AwesomeAssertions;
using Pine.CLI;
using Pine.Core.CLI;
using Pine.Core.IO;
using Spectre.Console;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.IntegrationTests.CLI;

public class FormatCommandSharedTests
{
    [Fact]
    public void WriteOverview_renders_summary_via_ansi_console()
    {
        var (console, output) = CreateConsole();

        FormatCommandShared.WriteOverview(
            console,
            commandLabel: "elm-[format]",
            totalFiles: 10,
            alreadyFormattedCount: 7,
            needsFormattingCount: 2,
            errorCount: 1);

        var rendered = output.ToString();

        rendered.Should().Contain("elm-[format] Summary");
        rendered.Should().Contain("Total files scanned:");
        rendered.Should().Contain("Already formatted:");
        rendered.Should().Contain("Need formatting:");
        rendered.Should().Contain("Syntax errors:");
        rendered.Should().Contain("✓");
        rendered.Should().Contain("○");
        rendered.Should().Contain("✗");
        rendered.Should().Contain("10");
        rendered.Should().Contain("7");
        rendered.Should().Contain("2");
        rendered.Should().Contain("1");
    }


    [Fact]
    public void WriteOverview_colors_status_rows()
    {
        var (console, output) = CreateConsole(AnsiSupport.Yes);

        FormatCommandShared.WriteOverview(
            console,
            commandLabel: "elm-format",
            totalFiles: 10,
            alreadyFormattedCount: 7,
            needsFormattingCount: 2,
            errorCount: 1);

        var rendered = output.ToString();

        rendered.Should().Contain("\u001b[32mAlready formatted:");
        rendered.Should().Contain("\u001b[93mNeed formatting:");
        rendered.Should().Contain("\u001b[91mSyntax errors:");
    }


    [Theory]
    [InlineData(437, 0)]
    [InlineData(437, 2)]
    [InlineData(850, 0)]
    [InlineData(850, 2)]
    public void WriteOverview_legacy_encoding_keeps_table_borders_aligned(
        int codePage,
        int needsFormattingCount)
    {
        var rendered =
            RenderLegacyOutput(
                codePage,
                console =>
                FormatCommandShared.WriteOverview(
                    console,
                    commandLabel: "dotnet-format",
                    totalFiles: 932,
                    alreadyFormattedCount: 931 - needsFormattingCount,
                    needsFormattingCount: needsFormattingCount,
                    errorCount: 1));

        rendered.Should().NotContain("\t");
        rendered.Should().NotContain("?");
        rendered.Should().Contain("+");
        rendered.Should().Contain("x");

        if (needsFormattingCount is not 0)
        {
            rendered.Should().Contain("*");
        }

        var tableLines =
            rendered
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
            .Skip(1)
            .ToArray();

        tableLines.Should().HaveCount(6);
        tableLines.Select(line => line.Length).Distinct().Should().ContainSingle();
    }


    [Theory]
    [InlineData(437, false, false)]
    [InlineData(437, false, true)]
    [InlineData(437, true, false)]
    [InlineData(437, true, true)]
    [InlineData(850, false, false)]
    [InlineData(850, false, true)]
    [InlineData(850, true, false)]
    [InlineData(850, true, true)]
    public void WriteFilesNeedingFormatting_legacy_encoding_preserves_status_prefix(
        int codePage,
        bool showGrouped,
        bool isTerminal)
    {
        var rendered =
            RenderLegacyOutput(
                codePage,
                console =>
                FormatCommandShared.WriteFilesNeedingFormatting(
                    console,
                    [
                        "/project/src/Module1.cs",
                        "/project/src/Module2.cs",
                        "/project/src/Module3.cs",
                        "/project/src/Module4.cs",
                        "/project/src/Module5.cs",
                    ],
                    showGrouped),
                isTerminal);

        rendered.Should().NotContain("\t");
        rendered.Should().NotContain("?");

        foreach (var index in Enumerable.Range(1, 5))
        {
            rendered.Should().Contain(
                showGrouped
                ?
                $"* Module{index}.cs"
                :
                $"* /project/src/Module{index}.cs");
        }
    }


    [Theory]
    [InlineData(437, false)]
    [InlineData(437, true)]
    [InlineData(850, false)]
    [InlineData(850, true)]
    public void Status_messages_legacy_encoding_preserves_error_and_success_markers(
        int codePage,
        bool isTerminal)
    {
        var rendered =
            RenderLegacyOutput(
                codePage,
                console =>
                {
                    (string path, string error)[] errors =
                        [
                            ("/project/Module1.cs", "First error"),
                            ("/project/Module2.cs", "Second error"),
                            ("/project/Module3.cs", "Third error"),
                            ("/project/Module4.cs", "Fourth error"),
                            ("/project/Module5.cs", "Fifth error"),
                        ];

                    FormatCommandShared.WriteFilesWithErrors(console, errors, showGrouped: false);
                    FormatCommandShared.WriteFilesWithErrors(console, errors, showGrouped: true);

                    FormatCommandShared.WriteFileDiagnostics(
                        console,
                        [
                            ("/project/Module.cs",
                            (IReadOnlyList<FormatFileDiagnostic>)[new("Diagnostic")]),
                        ]);

                    FormatCommandShared.WriteSuccessMessage(console, fileCount: 1, verifyMode: false);
                },
                isTerminal);

        rendered.Should().NotContain("\t");
        rendered.Should().NotContain("?");
        rendered.Should().Contain("x /project/Module1.cs");
        rendered.Should().Contain("x Module1.cs");
        rendered.Should().Contain("x /project/Module.cs");
        rendered.Should().Contain("+ File is already properly formatted.");
    }


    [Fact]
    public void WriteFilesWithErrors_preserves_markup_characters_in_values()
    {
        var (console, output) = CreateConsole();

        FormatCommandShared.WriteFilesWithErrors(
            console,
            [
                ("/project/[module].elm",
                "Expected [value] before the end of input"),
            ],
            showGrouped: false);

        var rendered = output.ToString();

        rendered.Should().Contain("/project/[module].elm");
        rendered.Should().Contain("Expected [value] before the end of input");
    }


    [Fact]
    public void WriteFileDiagnostics_counts_and_sorts_diagnostics()
    {
        var (console, output) = CreateConsole();

        FormatCommandShared.WriteFileDiagnostics(
            console,
            [
                ("/project/Beta.elm",
                (IReadOnlyList<FormatFileDiagnostic>)
                [
                    new("second [error]", Line: 20, Column: 3),
                    new("first error", Line: 5, Column: 1),
                ]),
                ("/project/Alfa.elm",
                (IReadOnlyList<FormatFileDiagnostic>)
                [
                    new("alfa error", Line: 1, Column: 1),
                ]),
            ]);

        var rendered = output.ToString();

        rendered.Should().Contain("SYNTAX ERRORS (3)");
        rendered.Should().Contain("20:3: second [error]");

        rendered.IndexOf("Alfa.elm", StringComparison.Ordinal)
            .Should().BeLessThan(
            rendered.IndexOf("Beta.elm", StringComparison.Ordinal));

        rendered.IndexOf("5:1: first error", StringComparison.Ordinal)
            .Should().BeLessThan(
            rendered.IndexOf("20:3: second [error]", StringComparison.Ordinal));
    }


    [Fact]
    public void WriteFilesNeedingFormatting_groups_files_by_directory()
    {
        var (console, output) = CreateConsole();

        FormatCommandShared.WriteFilesNeedingFormatting(
            console,
            [
                "/project/src/Module1.elm",
                "/project/src/Module2.elm",
                "/project/tests/Test1.elm",
                "/project/tests/Test2.elm",
                "/project/tests/Test3.elm",
            ],
            showGrouped: true);

        var rendered = output.ToString();

        rendered.Should().Contain("FILES NEEDING FORMATTING (5)");
        rendered.Should().Contain("/project/src/ (2 files)");
        rendered.Should().Contain("/project/tests/ (3 files)");
        rendered.Should().Contain("Module1.elm");
        rendered.Should().Contain("Test1.elm");
    }


    [Fact]
    public void WriteFilesNeedingFormatting_colors_flat_file_entries()
    {
        var (console, output) =
            CreateConsole(AnsiSupport.Yes, isTerminal: true);

        FormatCommandShared.WriteFilesNeedingFormatting(
            console,
            ["/project/Module.elm"],
            showGrouped: false);

        output.ToString()
            .Should().Contain("\u001b[93m○ /project/Module.elm");
    }


    [Fact]
    public void WriteFilesNeedingFormatting_colors_grouped_file_entries()
    {
        var (console, output) =
            CreateConsole(AnsiSupport.Yes, isTerminal: true);

        FormatCommandShared.WriteFilesNeedingFormatting(
            console,
            [
                "/project/src/Module1.elm",
                "/project/src/Module2.elm",
                "/project/tests/Test1.elm",
                "/project/tests/Test2.elm",
                "/project/tests/Test3.elm",
            ],
            showGrouped: true);

        output.ToString()
            .Should().Contain("\u001b[93m○ Module1.elm");
    }


    [Fact]
    public void Status_colors_use_theme_adaptive_ansi_palette()
    {
        FormatCommandTheme.Default.Foreground.Should().Be(Color.Default);
        FormatCommandTheme.Heading.Foreground.Should().Be(Color.Default);

        FormatCommandTheme.Success.Foreground.Should().Be(Color.Green);
        FormatCommandTheme.Warning.Foreground.Should().Be(Color.Yellow);
        FormatCommandTheme.Error.Foreground.Should().Be(Color.Red);
    }


    [Fact]
    public void ResolveColorMode_follows_cli_design_precedence()
    {
        FormatCommandShared.ResolveColorMode(
            commandLineValue: null,
            environmentValue: null)
            .Should().Be(CommandLineColorMode.Auto);

        FormatCommandShared.ResolveColorMode(
            commandLineValue: null,
            environmentValue: "never")
            .Should().Be(CommandLineColorMode.Never);

        FormatCommandShared.ResolveColorMode(
            commandLineValue: null,
            environmentValue: "ALWAYS")
            .Should().Be(CommandLineColorMode.Always);

        FormatCommandShared.ResolveColorMode(
            commandLineValue: CommandLineColorMode.Auto,
            environmentValue: "never")
            .Should().Be(CommandLineColorMode.Auto);
    }


    [Fact]
    public void ResolveColorMode_rejects_invalid_environment_value()
    {
        var resolve =
            () =>
            FormatCommandShared.ResolveColorMode(
                commandLineValue: null,
                environmentValue: "sometimes");

        resolve.Should().Throw<ArgumentException>()
            .WithMessage("*PINE_TERM_COLOR*auto, always, or never*");
    }


    [Fact]
    public void ColorMode_maps_to_spectre_ansi_support()
    {
        FormatCommandShared.AnsiSupportForColorMode(
            CommandLineColorMode.Auto)
            .Should().Be(AnsiSupport.Detect);

        FormatCommandShared.AnsiSupportForColorMode(
            CommandLineColorMode.Always)
            .Should().Be(AnsiSupport.Yes);

        FormatCommandShared.AnsiSupportForColorMode(
            CommandLineColorMode.Never)
            .Should().Be(AnsiSupport.No);

        FormatCommandShared.ColorSystemSupportForColorMode(
            CommandLineColorMode.Auto)
            .Should().Be(ColorSystemSupport.Detect);

        FormatCommandShared.ColorSystemSupportForColorMode(
            CommandLineColorMode.Always)
            .Should().Be(ColorSystemSupport.Standard);

        FormatCommandShared.ColorSystemSupportForColorMode(
            CommandLineColorMode.Never)
            .Should().Be(ColorSystemSupport.NoColors);
    }


    [Fact]
    public void Overwrite_prompt_defaults_to_no()
    {
        FormatCommandShared.CreateOverwritePrompt()
            .DefaultValue.Should().BeFalse();
    }


    [Theory]
    [InlineData(null, false)]
    [InlineData("", false)]
    [InlineData("n", false)]
    [InlineData("yes", true)]
    public void Execute_handles_noninteractive_confirmation(
        string? response,
        bool shouldOverwrite)
    {
        var tempDirectory =
            Path.Combine(
                Path.GetTempPath(),
                "pine-format-command-tests",
                Guid.NewGuid().ToString("N"));

        var filePath = Path.Combine(tempDirectory, "Module.elm");

        try
        {
            Directory.CreateDirectory(tempDirectory);
            File.WriteAllText(filePath, "before");

            var (console, _) = CreateConsole();
            var (errorConsole, _) = CreateConsole();

            var exitCode =
                FormatCommandShared.Execute(
                    paths: [filePath],
                    fileExtension: ".elm",
                    formatFile:
                    _ =>
                    new FormatFileResult.Changed("after"),
                    skipPrompt: false,
                    verifyNoChanges: false,
                    commandLabel: "elm-format",
                    colorMode: CommandLineColorMode.Never,
                    console: console,
                    errorConsole: errorConsole,
                    readLine: () => response);

            exitCode.Should().Be(0);

            File.ReadAllText(filePath)
                .Should().Be(shouldOverwrite ? "after" : "before");
        }
        finally
        {
            Filesystem.DeleteLocalDirectoryRecursive(tempDirectory);
        }
    }


    [Fact]
    public void Redirected_output_does_not_wrap_file_paths()
    {
        var (console, output) = CreateConsole();

        var longPath =
            "/project/" +
            new string('a', 120) +
            "/Module.elm";

        FormatCommandShared.WriteFilesNeedingFormatting(
            console,
            [longPath],
            showGrouped: false);

        output.ToString()
            .Split(Environment.NewLine)
            .Should().Contain(line => line.Contains(longPath, StringComparison.Ordinal));
    }


    [Fact]
    public void Execute_writes_to_injected_ansi_consoles()
    {
        var (console, output) = CreateConsole();
        var (errorConsole, errorOutput) = CreateConsole();

        var missingPath =
            Path.Combine(
                Path.GetTempPath(),
                "pine-format-command-tests",
                Guid.NewGuid().ToString("N"),
                "Missing.elm");

        var exitCode =
            FormatCommandShared.Execute(
                paths: [missingPath],
                fileExtension: ".elm",
                formatFile: _ => throw new InvalidOperationException("Formatter should not run"),
                skipPrompt: true,
                verifyNoChanges: false,
                commandLabel: "elm-format",
                colorMode: CommandLineColorMode.Never,
                console: console,
                errorConsole: errorConsole);

        exitCode.Should().Be(1);
        output.ToString().Should().BeEmpty();
        errorOutput.ToString().Should().Contain("Error:");
        errorOutput.ToString().Should().Contain("Path not found:");
        errorOutput.ToString().Should().Contain(missingPath);
    }


    [Theory]
    [InlineData(".cs", false)]
    [InlineData(".cs", true)]
    [InlineData(".elm", false)]
    [InlineData(".elm", true)]
    public void Execute_filters_workspace_files_from_directories(
        string extension,
        bool scanSubdirectory)
    {
        var directory = CreateTempDirectory();
        var sourceDirectory = Path.Combine(directory, "src");
        var objectDirectory = Path.Combine(sourceDirectory, "obj");
        var nestedRepository = Path.Combine(sourceDirectory, "nested");

        var sourcePath = Path.Combine(sourceDirectory, "Source" + extension);
        var trackedPath = Path.Combine(sourceDirectory, "Tracked" + extension);
        var ignoredPath = Path.Combine(sourceDirectory, "Ignored" + extension);
        var generatedPath = Path.Combine(objectDirectory, "Generated" + extension);
        var excludedPath = Path.Combine(sourceDirectory, "Excluded" + extension);
        var nestedPath = Path.Combine(nestedRepository, "Nested" + extension);

        try
        {
            Directory.CreateDirectory(objectDirectory);
            Directory.CreateDirectory(Path.Combine(nestedRepository, ".git"));
            InitializeGit(directory);

            File.WriteAllText(Path.Combine(directory, ".gitignore"), "obj/\nIgnored*\nTracked*\n");
            File.WriteAllText(Path.Combine(directory, ".git", "info", "exclude"), "Excluded*\n");
            File.WriteAllText(Path.Combine(directory, ".git", "Metadata" + extension), "metadata");
            File.WriteAllText(sourcePath, "source");
            File.WriteAllText(trackedPath, "tracked");
            File.WriteAllText(ignoredPath, "ignored");
            File.WriteAllText(generatedPath, "generated");
            File.WriteAllText(excludedPath, "excluded");
            File.WriteAllText(nestedPath, "nested");

            RunGit(directory, "add", "--force", trackedPath);

            var (console, output) = CreateConsole();
            var (errorConsole, errorOutput) = CreateConsole();

            var exitCode =
                FormatCommandShared.Execute(
                    paths: [scanSubdirectory ? sourceDirectory : directory],
                    fileExtension: extension,
                    formatFile: content => new FormatFileResult.Changed("formatted " + content),
                    skipPrompt: true,
                    verifyNoChanges: false,
                    commandLabel: "format",
                    colorMode: CommandLineColorMode.Never,
                    console: console,
                    errorConsole: errorConsole);

            exitCode.Should().Be(0);
            errorOutput.ToString().Should().BeEmpty();
            File.ReadAllText(sourcePath).Should().Be("formatted source");
            File.ReadAllText(trackedPath).Should().Be("formatted tracked");
            File.ReadAllText(ignoredPath).Should().Be("ignored");
            File.ReadAllText(generatedPath).Should().Be("generated");
            File.ReadAllText(excludedPath).Should().Be("excluded");
            File.ReadAllText(nestedPath).Should().Be("nested");
            output.ToString().Should().NotContain("Ignored" + extension);
            output.ToString().Should().NotContain("Generated" + extension);
            output.ToString().Should().NotContain("Metadata" + extension);
        }
        finally
        {
            Filesystem.DeleteLocalDirectoryRecursive(directory);
        }
    }


    [Theory]
    [InlineData(".cs", false)]
    [InlineData(".cs", true)]
    [InlineData(".elm", false)]
    [InlineData(".elm", true)]
    public void Execute_filters_explicit_ignored_workspace_files(
        string extension,
        bool verifyNoChanges)
    {
        var directory = CreateTempDirectory();
        var filePath = Path.Combine(directory, "Ignored" + extension);

        try
        {
            InitializeGit(directory);
            File.WriteAllText(Path.Combine(directory, ".gitignore"), "Ignored*\n");
            File.WriteAllText(filePath, "ignored");

            var (console, output) = CreateConsole();
            var (errorConsole, _) = CreateConsole();

            var exitCode =
                FormatCommandShared.Execute(
                    paths: [filePath],
                    fileExtension: extension,
                    formatFile: _ => throw new InvalidOperationException("Ignored files should not be formatted"),
                    skipPrompt: true,
                    verifyNoChanges: verifyNoChanges,
                    commandLabel: "format",
                    colorMode: CommandLineColorMode.Never,
                    console: console,
                    errorConsole: errorConsole);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain($"No {extension} files found.");
            File.ReadAllText(filePath).Should().Be("ignored");
        }
        finally
        {
            Filesystem.DeleteLocalDirectoryRecursive(directory);
        }
    }


    [Theory]
    [InlineData(".cs")]
    [InlineData(".elm")]
    public void Execute_preserves_directory_scanning_outside_git(string extension)
    {
        var directory = CreateTempDirectory();
        var objectDirectory = Path.Combine(directory, "obj");
        var filePath = Path.Combine(objectDirectory, "Source" + extension);

        try
        {
            Directory.CreateDirectory(objectDirectory);

            File.WriteAllText(Path.Combine(directory, ".gitignore"), "obj/\n");
            File.WriteAllText(filePath, "before");

            var (console, _) = CreateConsole();
            var (errorConsole, _) = CreateConsole();

            var exitCode =
                FormatCommandShared.Execute(
                    paths: [directory],
                    fileExtension: extension,
                    formatFile: _ => new FormatFileResult.Changed("after"),
                    skipPrompt: true,
                    verifyNoChanges: false,
                    commandLabel: "format",
                    colorMode: CommandLineColorMode.Never,
                    console: console,
                    errorConsole: errorConsole);

            exitCode.Should().Be(0);
            File.ReadAllText(filePath).Should().Be("after");
        }
        finally
        {
            Filesystem.DeleteLocalDirectoryRecursive(directory);
        }
    }


    [Fact]
    public void Temp_directory_cleanup_removes_readonly_git_files()
    {
        var directory = CreateTempDirectory();
        var objectDirectory = Path.Combine(directory, ".git", "objects", "80");
        var objectPath = Path.Combine(objectDirectory, "test-object");

        try
        {
            Directory.CreateDirectory(objectDirectory);
            File.WriteAllText(objectPath, "object");
            File.SetAttributes(objectPath, File.GetAttributes(objectPath) | FileAttributes.ReadOnly);

            Filesystem.DeleteLocalDirectoryRecursive(directory);

            Directory.Exists(directory).Should().BeFalse();
        }
        finally
        {
            Filesystem.DeleteLocalDirectoryRecursive(directory);
        }
    }


    [Theory]
    [InlineData(1, false, "File is already properly formatted.")]
    [InlineData(3, false, "All 3 file(s) are already properly formatted.")]
    [InlineData(3, true, "Verification passed.")]
    public void WriteSuccessMessage_renders_expected_text(
        int fileCount,
        bool verifyMode,
        string expectedText)
    {
        var (console, output) = CreateConsole();

        FormatCommandShared.WriteSuccessMessage(
            console,
            fileCount,
            verifyMode);

        output.ToString().Should().Contain(expectedText);
    }


    private static string CreateTempDirectory()
    {
        var directory =
            Path.Combine(
                Path.GetTempPath(),
                "pine-format-command-tests",
                Guid.NewGuid().ToString("N"));

        Directory.CreateDirectory(directory);
        return directory;
    }


    private static void InitializeGit(string directory)
    {
        RunGit(directory, "init", "--quiet", "--template=", "--object-format=sha1");
        RunGit(directory, "config", "core.excludesFile", "");
        Directory.CreateDirectory(Path.Combine(directory, ".git", "info"));
    }


    private static void RunGit(string directory, params string[] arguments)
    {
        var startInfo =
            new ProcessStartInfo("git")
            {
                WorkingDirectory = directory,
                UseShellExecute = false,
                RedirectStandardError = true,
            };

        // Keep Git configuration and repository overrides local to this child process.
        foreach (var name in new List<string>(startInfo.Environment.Keys))
        {
            if (name.StartsWith("GIT_", StringComparison.OrdinalIgnoreCase))
            {
                startInfo.Environment.Remove(name);
            }
        }

        startInfo.Environment["GIT_CONFIG_NOSYSTEM"] = "1";
        startInfo.Environment["GIT_CONFIG_GLOBAL"] = Path.Combine(directory, ".gitconfig-empty");

        foreach (var argument in arguments)
        {
            startInfo.ArgumentList.Add(argument);
        }

        using var process =
            Process.Start(startInfo)
            ?? throw new InvalidOperationException("Failed to start Git");

        var errorTask = process.StandardError.ReadToEndAsync();

        if (!process.WaitForExit(30_000))
        {
            process.Kill(entireProcessTree: true);
            process.WaitForExit();
            throw new TimeoutException("Git did not exit within 30 seconds.");
        }

        var error = errorTask.GetAwaiter().GetResult();
        process.ExitCode.Should().Be(0, error);
    }


    private static (IAnsiConsole console, StringWriter output) CreateConsole(
        AnsiSupport ansi = AnsiSupport.No,
        bool isTerminal = false)
    {
        var output = new StringWriter();

        var console =
            AnsiConsole.Create(
                new AnsiConsoleSettings
                {
                    Ansi = ansi,
                    ColorSystem = ColorSystemSupport.Standard,
                    Interactive = InteractionSupport.No,
                    Out =
                    isTerminal
                    ?
                    new TestConsoleOutput(output)
                    :
                    new AnsiConsoleOutput(output),
                });

        return (console, output);
    }


    private static string RenderLegacyOutput(
        int codePage,
        Action<IAnsiConsole> render,
        bool isTerminal = true)
    {
        var encoding =
            CodePagesEncodingProvider.Instance.GetEncoding(codePage)
            ?? throw new InvalidOperationException($"Encoding {codePage} is unavailable");

        using var bytes = new MemoryStream();
        using var writer = new StreamWriter(bytes, encoding, leaveOpen: true);

        var console =
            AnsiConsole.Create(
                new AnsiConsoleSettings
                {
                    Ansi = AnsiSupport.No,
                    Interactive = InteractionSupport.No,
                    Enrichment = new ProfileEnrichment { UseDefaultEnrichers = false },
                    Out =
                    isTerminal
                    ?
                    new TestConsoleOutput(writer)
                    :
                    new AnsiConsoleOutput(writer),
                });

        console.Profile.Capabilities.Unicode.Should().BeFalse();

        render(console);
        writer.Flush();

        return encoding.GetString(bytes.ToArray());
    }


    private sealed class TestConsoleOutput(TextWriter writer) : IAnsiConsoleOutput
    {
        public TextWriter Writer { get; } = writer;

        public bool IsTerminal => true;

        public int Width => 120;

        public int Height => 40;

        public void SetEncoding(Encoding encoding)
        {
        }
    }
}
