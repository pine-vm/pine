using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax;

public class ParseIncompleteTests
{
    /*
     * The parser should be able to parse Elm modules containing incomplete declarations,
     * by detecting the start of individual module-level declarations even if
     * preceding code is incomplete and contains syntax errors.
     * */

    static File ParseFile(string elmModuleText)
    {
        var parseResult =
            ElmSyntaxParser.ParseModuleText(elmModuleText);

        if (parseResult.IsErrOrNull() is { } err)
        {
            throw new System.Exception($"Failed to parse Elm module text: {err}");
        }

        if (parseResult.IsOkOrNull() is not { } ok)
        {
            throw new System.Exception(
                $"Unexpected type of parse result: {parseResult.GetType().FullName}");
        }

        return ok;
    }

    static string RenderParseError(ElmSyntaxParseError elmSyntaxParseError) =>
        $"Error at {elmSyntaxParseError.Location.Row}:{elmSyntaxParseError.Location.Column}: {elmSyntaxParseError.Message}";

    [Fact]
    public void First_declaration_complete_second_incomplete()
    {
        var elmModuleText =
            """"       
            module App exposing (..)


            decl_a =
                42


            decl_b =

            """";

        var parsedModule =
            ParseFile(elmModuleText);

        // decl_a should be parsed correctly

        parsedModule.Declarations.Should().HaveCount(1);

        var functionDeclarations =
            parsedModule.Declarations
            .Select(d => d.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .ToList();

        functionDeclarations.Should().HaveCount(1);

        var declA = functionDeclarations[0];

        declA.Function.Declaration.Value.Name.Value.Should().Be("decl_a");

        // Should have one parsing error for the incomplete declaration
        parsedModule.IncompleteDeclarations.Should().HaveCount(1);

        var incompleteDecl = parsedModule.IncompleteDeclarations[0];

        // The range should correctly point to the start of the incomplete declaration
        incompleteDecl.Range.Start.Row.Should().Be(8);
        incompleteDecl.Range.Start.Column.Should().Be(1);

        // Incomplete declarations should not be in comments anymore
        parsedModule.Comments.Should().HaveCount(0);

        // The incomplete declaration text should contain "decl_b ="
        incompleteDecl.Value.OriginalText.Should().Be("decl_b =");

        // The ErrorLocation should point to where the actual parsing error occurred
        // For "decl_b =", the error occurs after the equals sign when expecting an expression
        RenderParseError(incompleteDecl.Value.ParseError).Should().Be(
            "Error at 8:1: Unfinished definition");
    }

    [Fact]
    public void Produces_syntax_error_unfinished_definition()
    {
        var input =
            """"
            module Test exposing (..)


            decl =

            """";

        var parseFileResult =
            ElmSyntaxParser.ParseModuleText(input);

        var parseFileOk =
            parseFileResult
            .Extract(err => throw new System.Exception(err));

        var incompleteDeclaration =
            parseFileOk.IncompleteDeclarations.Single();

        RenderParseError(incompleteDeclaration.Value.ParseError).Should().Be(
            "Error at 4:7: Unfinished definition");
    }

    [Fact]
    public void Produces_syntax_error_unfinished_list_after_last_item()
    {
        var input =
            """"
            module Test exposing (..)


            decl = [ 17, 19

            """";

        var parseFileResult =
            ElmSyntaxParser.ParseModuleText(input);

        var parseFileOk =
            parseFileResult
            .Extract(err => throw new System.Exception(err));

        var incompleteDeclaration =
            parseFileOk.IncompleteDeclarations.Single();

        RenderParseError(incompleteDeclaration.Value.ParseError).Should().Be(
            "Error at 4:16: Unfinished list");
    }
}
