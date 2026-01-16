using AwesomeAssertions;
using Pine.Elm.CLI;
using System.Collections.Immutable;
using Xunit;

namespace Pine.IntegrationTests.Elm.CLI;

public class ElmFormatCommandRenderingTests
{
    [Fact]
    public void RenderOverviewHeader_with_no_errors()
    {
        var rendered = ElmFormatCommand.RenderOverviewHeaderToString(
            totalFiles: 10,
            alreadyFormattedCount: 8,
            needsFormattingCount: 2,
            parseErrorCount: 0,
            verifyMode: false,
            width: 66);

        rendered.Trim().Should().Be(
            """"
            ╔════════════════════════════════════════════════════════════════╗
            ║                       elm-format Summary                       ║
            ╠════════════════════════════════════════════════════════════════╣
            ║  Total files scanned:         10                               ║
            ║  Already formatted:            8  ✓                            ║
            ║  Need formatting:              2  ○                            ║
            ╚════════════════════════════════════════════════════════════════╝
            """".Trim());
    }

    [Fact]
    public void RenderOverviewHeader_with_errors()
    {
        var rendered = ElmFormatCommand.RenderOverviewHeaderToString(
            totalFiles: 10,
            alreadyFormattedCount: 7,
            needsFormattingCount: 2,
            parseErrorCount: 1,
            verifyMode: false,
            width: 66);

        rendered.Trim().Should().Be(
            """"
            ╔════════════════════════════════════════════════════════════════╗
            ║                       elm-format Summary                       ║
            ╠════════════════════════════════════════════════════════════════╣
            ║  Total files scanned:         10                               ║
            ║  Already formatted:            7  ✓                            ║
            ║  Need formatting:              2  ○                            ║
            ║  Syntax errors:                1  ✗                            ║
            ╚════════════════════════════════════════════════════════════════╝
            """".Trim());
    }

    [Fact]
    public void RenderOverviewHeader_with_many_errors()
    {
        var rendered = ElmFormatCommand.RenderOverviewHeaderToString(
            totalFiles: 10,
            alreadyFormattedCount: 7,
            needsFormattingCount: 2,
            parseErrorCount: 123,
            verifyMode: false,
            width: 66);

        rendered.Trim().Should().Be(
            """"
            ╔════════════════════════════════════════════════════════════════╗
            ║                       elm-format Summary                       ║
            ╠════════════════════════════════════════════════════════════════╣
            ║  Total files scanned:         10                               ║
            ║  Already formatted:            7  ✓                            ║
            ║  Need formatting:              2  ○                            ║
            ║  Syntax errors:              123  ✗                            ║
            ╚════════════════════════════════════════════════════════════════╝
            """".Trim());
    }

    [Fact]
    public void RenderFilesWithErrors_simple_list()
    {
        var errors = ImmutableDictionary<string, ElmFormatFileResult.ParseError>.Empty
            .Add("/path/to/File1.elm", new ElmFormatFileResult.ParseError("Unexpected token"))
            .Add("/path/to/File2.elm", new ElmFormatFileResult.ParseError("Missing module declaration"));

        var rendered = ElmFormatCommand.RenderFilesWithErrorsToString(errors, showGrouped: false);

        rendered.Trim().Should().Be(
            """"
            ══════════════════════════════════════════════════════════════════
             ✗ FILES WITH SYNTAX ERRORS
            ══════════════════════════════════════════════════════════════════

            ✗ /path/to/File1.elm
              Error: Unexpected token
            ✗ /path/to/File2.elm
              Error: Missing module declaration

            """".Trim());
    }

    [Fact]
    public void RenderFilesWithErrors_grouped_by_directory()
    {
        var errors =
            ImmutableDictionary<string, ElmFormatFileResult.ParseError>.Empty
            .Add("/project/src/Module1.elm", new ElmFormatFileResult.ParseError("Error 1"))
            .Add("/project/src/Module2.elm", new ElmFormatFileResult.ParseError("Error 2"))
            .Add("/project/tests/Test1.elm", new ElmFormatFileResult.ParseError("Error 3"))
            .Add("/project/tests/Test2.elm", new ElmFormatFileResult.ParseError("Error 4"))
            .Add("/project/tests/Test3.elm", new ElmFormatFileResult.ParseError("Error 5"));

        var rendered = ElmFormatCommand.RenderFilesWithErrorsToString(errors, showGrouped: true);

        rendered.Should().Contain("FILES WITH SYNTAX ERRORS");
        rendered.Should().Contain("/project/src/");
        rendered.Should().Contain("/project/tests/");
        rendered.Should().Contain("Module1.elm");
        rendered.Should().Contain("Test1.elm");
    }

    [Fact]
    public void RenderFilesNeedingFormatting_simple_list()
    {
        var files = ImmutableDictionary<string, ElmFormatFileResult.FormatChanged>.Empty
            .Add("/path/to/File1.elm", new ElmFormatFileResult.FormatChanged("formatted content 1"))
            .Add("/path/to/File2.elm", new ElmFormatFileResult.FormatChanged("formatted content 2"));

        var rendered = ElmFormatCommand.RenderFilesNeedingFormattingToString(files, showGrouped: false);

        rendered.Should().Contain("FILES NEEDING FORMATTING");
        rendered.Should().Contain("(2)");
        rendered.Should().Contain("/path/to/File1.elm");
        rendered.Should().Contain("/path/to/File2.elm");
    }

    [Fact]
    public void RenderFilesNeedingFormatting_grouped_by_directory()
    {
        var files = ImmutableDictionary<string, ElmFormatFileResult.FormatChanged>.Empty
            .Add("/project/src/Module1.elm", new ElmFormatFileResult.FormatChanged("formatted 1"))
            .Add("/project/src/Module2.elm", new ElmFormatFileResult.FormatChanged("formatted 2"))
            .Add("/project/tests/Test1.elm", new ElmFormatFileResult.FormatChanged("formatted 3"))
            .Add("/project/tests/Test2.elm", new ElmFormatFileResult.FormatChanged("formatted 4"))
            .Add("/project/tests/Test3.elm", new ElmFormatFileResult.FormatChanged("formatted 5"));

        var rendered = ElmFormatCommand.RenderFilesNeedingFormattingToString(files, showGrouped: true);

        rendered.Should().Contain("FILES NEEDING FORMATTING");
        rendered.Should().Contain("(5)");
        rendered.Should().Contain("/project/src/");
        rendered.Should().Contain("(2 files)");
        rendered.Should().Contain("/project/tests/");
        rendered.Should().Contain("(3 files)");
        rendered.Should().Contain("Module1.elm");
        rendered.Should().Contain("Test1.elm");
    }

    [Fact]
    public void RenderSuccessMessage_single_file()
    {
        var rendered = ElmFormatCommand.RenderSuccessMessageToString(fileCount: 1, verifyMode: false);

        rendered.Should().Contain("File is already properly formatted");
        rendered.Should().NotContain("Verification passed");
    }

    [Fact]
    public void RenderSuccessMessage_multiple_files()
    {
        var rendered = ElmFormatCommand.RenderSuccessMessageToString(fileCount: 5, verifyMode: false);

        rendered.Should().Contain("All 5 file(s) are already properly formatted");
        rendered.Should().NotContain("Verification passed");
    }

    [Fact]
    public void RenderSuccessMessage_verify_mode()
    {
        var rendered = ElmFormatCommand.RenderSuccessMessageToString(fileCount: 3, verifyMode: true);

        rendered.Should().Contain("All 3 file(s) are already properly formatted");
        rendered.Should().Contain("Verification passed");
    }

    [Fact]
    public void RenderOverviewHeader_uses_delegate()
    {
        var lines = new System.Collections.Generic.List<string>();

        ElmFormatCommand.RenderOverviewHeader(
            writeLine: lines.Add,
            totalFiles: 5,
            alreadyFormattedCount: 3,
            needsFormattingCount: 2,
            parseErrorCount: 0,
            verifyMode: false,
            width: 60);

        lines.Should().HaveCountGreaterThan(5);
        lines.Should().Contain(line => line.Contains("elm-format Summary"));
    }

    [Fact]
    public void RenderFilesWithErrors_uses_delegate()
    {
        var lines = new System.Collections.Generic.List<string>();

        var errors =
            ImmutableDictionary<string, ElmFormatFileResult.ParseError>.Empty
            .Add("/path/File.elm", new ElmFormatFileResult.ParseError("Test error"));

        ElmFormatCommand.RenderFilesWithErrors(
            writeLine: lines.Add,
            errors: errors,
            showGrouped: false);

        lines.Should().HaveCountGreaterThan(3);
        lines.Should().Contain(line => line.Contains("FILES WITH SYNTAX ERRORS"));
    }

    [Fact]
    public void RenderFilesNeedingFormatting_uses_delegate()
    {
        var lines = new System.Collections.Generic.List<string>();

        var files =
            ImmutableDictionary<string, ElmFormatFileResult.FormatChanged>.Empty
            .Add("/path/File.elm", new ElmFormatFileResult.FormatChanged("formatted content"));

        ElmFormatCommand.RenderFilesNeedingFormatting(
            writeLine: lines.Add,
            files: files,
            showGrouped: false);

        lines.Should().HaveCountGreaterThan(3);
        lines.Should().Contain(line => line.Contains("FILES NEEDING FORMATTING"));
    }

    [Fact]
    public void RenderSuccessMessage_uses_delegate()
    {
        var lines = new System.Collections.Generic.List<string>();

        ElmFormatCommand.RenderSuccessMessage(
            writeLine: lines.Add,
            fileCount: 3,
            verifyMode: true);

        lines.Should().HaveCountGreaterThanOrEqualTo(2);
        lines.Should().Contain(line => line.Contains("properly formatted"));
        lines.Should().Contain(line => line.Contains("Verification passed"));
    }
}
