using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mConcretized;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mConcretized;

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
        incompleteDecl.Value.ErrorLocation.Row.Should().BeGreaterThanOrEqualTo(8);

        // The ErrorMessage should be non-empty
        incompleteDecl.Value.ErrorMessage.Should().NotBeEmpty();
    }
}
