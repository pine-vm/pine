using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using System.Linq;
using Xunit;

using AbstractSyntax = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using SyntaxModel = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class ConcreteCanonicalizationTests
{
    [Fact]
    public void Preserves_concrete_source_structure_and_ranges()
    {
        var parsed =
            Parse(
                """
                module Test exposing (..)

                import Maybe exposing (Maybe(..))

                -- retained comment
                local x =
                    x


                value =
                    [ local 1, "line\n", 0x2A ]
                """);

        var canonicalized = Canonicalize(parsed);

        canonicalized.ModuleDefinition.Should().Be(parsed.ModuleDefinition);
        canonicalized.Imports.Should().Equal(parsed.Imports);
        canonicalized.Comments.Should().Equal(parsed.Comments);
        canonicalized.IncompleteDeclarations.Should().Equal(parsed.IncompleteDeclarations);
        canonicalized.Declarations.Select(declaration => declaration.Range)
            .Should().Equal(parsed.Declarations.Select(declaration => declaration.Range));

        var parsedValue = FunctionNamed(parsed, "value");
        var canonicalizedValue = FunctionNamed(canonicalized, "value");

        canonicalizedValue.Declaration.Value.Expression.Range
            .Should().Be(parsedValue.Declaration.Value.Expression.Range);

        var parsedList =
            parsedValue.Declaration.Value.Expression.Value
            .Should().BeOfType<SyntaxModel.Expression.ListExpr>().Which;

        var canonicalizedList =
            canonicalizedValue.Declaration.Value.Expression.Value
            .Should().BeOfType<SyntaxModel.Expression.ListExpr>().Which;

        canonicalizedList.Elements.Should().BeOfType<SyntaxModel.SeparatedSyntaxList<SyntaxModel.Node<SyntaxModel.Expression>>.NonEmpty>()
            .Which.Rest.Select(item => item.SeparatorLocation)
            .Should().Equal(
                parsedList.Elements
                .Should().BeOfType<SyntaxModel.SeparatedSyntaxList<SyntaxModel.Node<SyntaxModel.Expression>>.NonEmpty>()
                .Which.Rest.Select(item => item.SeparatorLocation));

        canonicalizedList.Elements.Last().Value
            .Should().Be(new SyntaxModel.Expression.Integer("0x2A"));

        canonicalizedList.Elements.ElementAt(1).Value
            .Should().Be(new SyntaxModel.Expression.Literal("line\n", """line\n"""));
    }

    [Fact]
    public void Concrete_to_abstract_matches_expected_canonical_names_and_literals()
    {
        var canonicalized =
            Canonicalize(
                Parse(
                    """
                    module Test exposing (..)

                    helper x =
                        x


                    value x =
                        ( helper x, 0x2A )
                    """));

        var expected =
            Parse(
                """
                module Test exposing (..)

                helper x =
                    x


                value x =
                    ( Test.helper x, 0x2A )
                """);

        AbstractSyntax.ConvertFromConcrete.FromFile(canonicalized)
            .Should().Be(AbstractSyntax.ConvertFromConcrete.FromFile(expected));
    }

    [Fact]
    public void Formatting_only_changes_produce_equal_abstract_canonical_files()
    {
        var compact =
            Canonicalize(
                Parse(
                    """
                    module Test exposing (..)
                    helper x = x
                    value = helper 0x2A
                    """));

        var expanded =
            Canonicalize(
                Parse(
                    """
                    module Test exposing (..)

                    helper x =
                        x


                    value =
                        helper
                            0x2A
                    """));

        AbstractSyntax.ConvertFromConcrete.FromFile(compact)
            .Should().Be(AbstractSyntax.ConvertFromConcrete.FromFile(expanded));
    }

    private static SyntaxModel.File Parse(string source) =>
        ElmSyntaxParser.ParseModuleText(source)
        .Extract(error => throw new System.Exception("Failed parsing: " + error));

    private static SyntaxModel.File Canonicalize(SyntaxModel.File file) =>
        Canonicalization.Canonicalize([file])
        .Extract(error => throw new System.Exception("Failed canonicalization: " + error))
        [["Test"]]
        .Extract(error => throw new System.Exception("Module canonicalization failed: " + error));

    private static SyntaxModel.FunctionStruct FunctionNamed(
        SyntaxModel.File file,
        string name) =>
        file.Declarations
        .Select(declaration => declaration.Value)
        .OfType<SyntaxModel.Declaration.FunctionDeclaration>()
        .Single(declaration => declaration.Function.Declaration.Value.Name.Value == name)
        .Function;
}
