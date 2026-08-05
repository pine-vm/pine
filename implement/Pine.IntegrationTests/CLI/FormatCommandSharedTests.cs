using AwesomeAssertions;
using Pine.CLI;
using Spectre.Console;
using System;
using System.Collections.Generic;
using System.IO;
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
            .Should().Be(FormatCommandColorMode.Auto);

        FormatCommandShared.ResolveColorMode(
            commandLineValue: null,
            environmentValue: "never")
            .Should().Be(FormatCommandColorMode.Never);

        FormatCommandShared.ResolveColorMode(
            commandLineValue: null,
            environmentValue: "ALWAYS")
            .Should().Be(FormatCommandColorMode.Always);

        FormatCommandShared.ResolveColorMode(
            commandLineValue: FormatCommandColorMode.Auto,
            environmentValue: "never")
            .Should().Be(FormatCommandColorMode.Auto);
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
            FormatCommandColorMode.Auto)
            .Should().Be(AnsiSupport.Detect);

        FormatCommandShared.AnsiSupportForColorMode(
            FormatCommandColorMode.Always)
            .Should().Be(AnsiSupport.Yes);

        FormatCommandShared.AnsiSupportForColorMode(
            FormatCommandColorMode.Never)
            .Should().Be(AnsiSupport.No);

        FormatCommandShared.ColorSystemSupportForColorMode(
            FormatCommandColorMode.Auto)
            .Should().Be(ColorSystemSupport.Detect);

        FormatCommandShared.ColorSystemSupportForColorMode(
            FormatCommandColorMode.Always)
            .Should().Be(ColorSystemSupport.Standard);

        FormatCommandShared.ColorSystemSupportForColorMode(
            FormatCommandColorMode.Never)
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

        Directory.CreateDirectory(tempDirectory);
        File.WriteAllText(filePath, "before");

        try
        {
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
                    console: console,
                    errorConsole: errorConsole,
                    readLine: () => response);

            exitCode.Should().Be(0);

            File.ReadAllText(filePath)
                .Should().Be(shouldOverwrite ? "after" : "before");
        }
        finally
        {
            Directory.Delete(tempDirectory, recursive: true);
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
                console: console,
                errorConsole: errorConsole);

        exitCode.Should().Be(1);
        output.ToString().Should().BeEmpty();
        errorOutput.ToString().Should().Contain("Error:");
        errorOutput.ToString().Should().Contain("Path not found:");
        errorOutput.ToString().Should().Contain(missingPath);
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


    private sealed class TestConsoleOutput(StringWriter writer) : IAnsiConsoleOutput
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
