using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.LanguageServerProtocol;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using ElmLanguageServer = Pine.Core.Elm.LanguageServer.LanguageServer;
using Location = Pine.Core.Elm.ElmSyntax.SyntaxModel.Location;
using LspRange = Pine.Core.LanguageServerProtocol.Range;
using SyntaxRange = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Tests.Elm.LanguageServer;

public class ElmSyntaxDiagnosticsProviderTests
{
    private const string DocumentUri = "file:///project/Main.elm";

    private sealed class DocumentTextSource(string text) : IDocumentTextSource
    {
        public string? TryGetDocumentText(string documentUri) =>
            documentUri == DocumentUri ? text : null;
    }

    [Theory]
    [InlineData("\n")]
    [InlineData("\r\n")]
    public async Task Syntax_diagnostics_report_fatal_primary_region_and_explanation(string newline)
    {
        var source = newline + "{- unclosed" + newline;
        var provider = new ElmSyntaxDiagnosticsProvider(new DocumentTextSource(source));

        ElmSyntaxParser.ParseModuleText(source).IsErrOrNullable().Should().NotBeNull();

        var result = await provider.GetDiagnosticsAsync(DocumentUri, CancellationToken.None);
        var document = result.IsOkOrNull().Should().ContainSingle().Subject;
        document.DocumentUri.Should().Be(DocumentUri);

        var diagnostic = document.Diagnostics.Should().ContainSingle().Subject;
        diagnostic.Range.Should().Be(new LspRange(new Position(1, 0), new Position(1, 2)));
        diagnostic.Severity.Should().Be(DiagnosticSeverity.Error);
        diagnostic.Source.Should().Be("elm syntax");

        diagnostic.Message.Should().Be(
            "I cannot find the end of this multi-line comment:\n\n\n" +
            "Add a -} somewhere after this to end the comment.\n\n" +
            "Hint: Multi-line comments can be nested in Elm, so {- {- -} -} is a comment that\n" +
            "happens to contain another comment. Like parentheses and curly braces, the start\n" +
            "and end markers must always be balanced. Maybe that is the problem?");
    }

    [Fact]
    public async Task Syntax_diagnostics_include_recovered_and_additional_errors_in_source_order()
    {
        const string Source =
            "module Main exposing (..)\n\nfirst = 903.\n\nsize : Int\ncount = 1\n\nvalid = 1\n\n{-| documentation -}\n";

        var parsed = ElmSyntaxParser.ParseModuleText(Source).IsOkOrNull();
        parsed.Should().NotBeNull();
        parsed!.IncompleteDeclarations.Should().HaveCount(2);
        parsed.AdditionalParseErrors.Should().ContainSingle();

        var provider = new ElmSyntaxDiagnosticsProvider(new DocumentTextSource(Source));
        var result = await provider.GetDiagnosticsAsync(DocumentUri, CancellationToken.None);
        var diagnostics = result.IsOkOrNull().Should().ContainSingle().Subject.Diagnostics;

        diagnostics.Should().HaveCount(3);

        diagnostics.Select(d => d.Range).Should().Equal(
            new LspRange(new Position(2, 11), new Position(2, 11)),
            new LspRange(new Position(5, 0), new Position(5, 0)),
            new LspRange(new Position(10, 0), new Position(10, 0)));

        diagnostics[0].Message.Should().Be(
            "Numbers cannot end with a dot like this:\n\n\nSwitching to 903 or 903.0 will work though!");

        diagnostics[1].Message.Should().Be(
            "I just saw a type annotation for `size`, but it is followed by a definition for\n" +
            "`count`:\n\n\nThese names do not match! Is there a typo?\n\n    count -> size");

        diagnostics[2].Message.Should().StartWith(
            "I am trying to parse a declaration, but I am getting stuck here:");
    }

    [Theory]
    [InlineData("module Main exposing (..)\n\nmain=0\n")]
    [InlineData("module Other exposing (..)\n\nport send : String -> Cmd msg\n")]
    [InlineData("port module Main exposing (..)\n\nmain=0\n")]
    public async Task Syntax_diagnostics_do_not_perform_formatting_or_module_validation(string source)
    {
        var provider = new ElmSyntaxDiagnosticsProvider(new DocumentTextSource(source));
        var result = await provider.GetDiagnosticsAsync(DocumentUri, CancellationToken.None);

        result.IsOkOrNull().Should().BeEmpty();
    }

    [Fact]
    public void Syntax_diagnostics_compose_only_parse_errors_in_stable_source_order()
    {
        var earlier =
            new ElmSyntaxParseError(
                new SyntaxRange(new Location(2, 5), new Location(2, 7)),
                new SyntaxRange(new Location(1, 1), new Location(10, 20)),
                new ElmSyntaxErrorKind.Parse(
                    new ElmSyntaxProblem.Expected(
                        new ExpectedSyntax.Keyword("exposing"),
                        new FoundSyntax(FoundSyntaxKind.EndOfFile, ""))));

        var later = earlier with { Region = new SyntaxRange(new Location(4, 2), new Location(5, 3)) };

        var tied =
            earlier with
            {
                Kind =
                new ElmSyntaxErrorKind.Parse(
                    new ElmSyntaxProblem.Expected(
                        new ExpectedSyntax.Token(SyntaxTokenKind.CloseParen),
                        new FoundSyntax(FoundSyntaxKind.EndOfFile, "")))
            };

        var validation =
            earlier with
            {
                Kind = new ElmSyntaxErrorKind.ModuleValidation(ModuleValidationProblem.PortInPackage)
            };

        var diagnostics =
            ElmLanguageServer.ComputeSyntaxErrorDiagnostics("", [later, validation, earlier, tied]);

        diagnostics.Select(d => d.Message).Should().Equal(
            "Expected `exposing`.",
            "Expected CloseParen.",
            "Expected `exposing`.");

        diagnostics.Select(d => d.Range).Should().Equal(
            new LspRange(new Position(1, 4), new Position(1, 6)),
            new LspRange(new Position(1, 4), new Position(1, 6)),
            new LspRange(new Position(3, 1), new Position(4, 2)));
    }

    [Theory]
    [InlineData(0, 0, 0, 0, 0, 0, 0, 0)]
    [InlineData(int.MinValue, -1, -2, int.MinValue, 0, 0, 0, 0)]
    [InlineData(3, 5, 2, 9, 2, 4, 2, 4)]
    [InlineData(3, 5, 3, 2, 2, 4, 2, 4)]
    [InlineData(3, 5, 3, 5, 2, 4, 2, 4)]
    [InlineData(3, 5, 4, 2, 2, 4, 3, 1)]
    [InlineData(int.MaxValue, int.MaxValue, int.MaxValue, int.MaxValue,
        int.MaxValue - 1, int.MaxValue - 1, int.MaxValue - 1, int.MaxValue - 1)]
    public void Syntax_diagnostics_clamp_invalid_coordinates_and_keep_ranges_ordered(
        int startRow,
        int startColumn,
        int endRow,
        int endColumn,
        int expectedStartLine,
        int expectedStartCharacter,
        int expectedEndLine,
        int expectedEndCharacter)
    {
        var error =
            new ElmSyntaxParseError(
                new SyntaxRange(new Location(startRow, startColumn), new Location(endRow, endColumn)),
                null,
                new ElmSyntaxErrorKind.Parse(
                    new ElmSyntaxProblem.Expected(
                        new ExpectedSyntax.Keyword("module"),
                        new FoundSyntax(FoundSyntaxKind.EndOfFile, ""))));

        var diagnostic =
            ElmLanguageServer.ComputeSyntaxErrorDiagnostics("", [error]).Should().ContainSingle().Subject;

        diagnostic.Range.Should().Be(
            new LspRange(
                new Position((uint)expectedStartLine, (uint)expectedStartCharacter),
                new Position((uint)expectedEndLine, (uint)expectedEndCharacter)));
    }
}
