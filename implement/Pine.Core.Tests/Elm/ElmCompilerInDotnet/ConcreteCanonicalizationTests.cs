using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using System.Linq;
using Xunit;

using AbstractSyntax = Pine.Core.Elm.Syntax.Abstract;
using ConcreteSyntax = Pine.Core.Elm.Syntax.Concrete;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class ConcreteCanonicalizationTests
{
    [Theory]
    [InlineData("Status(..)", "Status", "type alias Status = First.Status")]
    [InlineData("Status", "Status", "type alias Status = First.Status")]
    [InlineData("Status(..)", "Status(..)", "type Status = OtherStatus")]
    [InlineData("Status", "Status", "type alias Status = { value : Int }")]
    [InlineData("..", "Status", "type alias Status = First.Status")]
    [InlineData("..", "Status(..)", "type Status = OtherStatus")]
    public void Later_explicit_type_import_replaces_earlier_type_import(
        string firstExposing,
        string secondExposing,
        string secondDeclaration)
    {
        var files =
            new[]
            {
                Parse("module First exposing (Status(..))\ntype Status = Status"),
                Parse($"module Second exposing (..)\nimport First\n{secondDeclaration}"),
                Parse(
                    $$"""
                    module Main exposing (root)
                    import First exposing ({{firstExposing}})
                    import Second exposing ({{secondExposing}})
                    root : Status -> Status
                    root value = value
                    """)
            };

        var canonicalized =
            Canonicalization.Canonicalize(files)
            .Extract(error => throw new System.Exception(error))
            [["Main"]];

        canonicalized.Errors.Should().BeEmpty();

        var function = FunctionNamed(canonicalized.File, "root");

        var signature =
            function.Signature!.Value.TypeAnnotation.Value
            .Should().BeOfType<ConcreteSyntax.TypeAnnotation.FunctionTypeAnnotation>().Which;

        signature.ArgumentType.Value.Should().BeOfType<ConcreteSyntax.TypeAnnotation.Typed>()
            .Which.TypeName.Value.ModuleName.Should().Equal("Second");

        signature.ReturnType.Value.Should().BeOfType<ConcreteSyntax.TypeAnnotation.Typed>()
            .Which.TypeName.Value.ModuleName.Should().Equal("Second");

        var root = DeclQualifiedName.Create(["Main"], "root");

        var demanded =
            Canonicalization.CanonicalizeDeclarations(files, [root])
            .Extract(error => throw new System.Exception(error));

        demanded.Declarations.Values.SelectMany(declaration => declaration.Errors).Should().BeEmpty();
        demanded.Declarations[root].Value.Should().Be(canonicalized.File.Declarations.Single());
        var expectedType = DeclQualifiedName.Create(["Second"], "Status");
        demanded.References[root].Should().HaveCount(2);

        demanded.References[root].Should().OnlyContain(
            reference => reference.ReferencedName == expectedType && reference.IsTypeReference);
    }

    [Theory]
    [InlineData("..", "..")]
    [InlineData("Status", "..")]
    [InlineData("Status(..)", "..")]
    public void Later_wildcard_type_import_preserves_ambiguity(string firstExposing, string secondExposing)
    {
        var files =
            new[]
            {
                Parse("module First exposing (Status(..))\ntype Status = Status"),
                Parse("module Second exposing (Status(..))\ntype Status = OtherStatus"),
                Parse(
                    $$"""
                    module Main exposing (root)
                    import First exposing ({{firstExposing}})
                    import Second exposing ({{secondExposing}})
                    root : Status
                    root = First.Status
                    """)
            };

        var canonicalized =
            Canonicalization.Canonicalize(files)
            .Extract(error => throw new System.Exception(error))
            [["Main"]];

        var error =
            canonicalized.Errors.Should().ContainSingle()
            .Which.Should().BeOfType<CanonicalizationError.AmbiguousImport>().Which;

        error.Name.Should().Be("Status");
        error.ImportingModules.Should().Equal("First", "Second");

        var root = DeclQualifiedName.Create(["Main"], "root");

        var demanded =
            Canonicalization.CanonicalizeDeclarations(files, [root])
            .Extract(failure => throw new System.Exception(failure));

        demanded.Declarations[root].Errors.Should().BeEquivalentTo(canonicalized.Errors);
    }

    [Theory]
    [InlineData("Status(..)", "type Status = Status", "root _ = Status")]
    [InlineData("Status(..)", "type Status = Status", "root Status = First.Status")]
    [InlineData("Status", "type alias Status = { value : Int }", "root _ = Status 1")]
    public void Explicit_type_imports_do_not_hide_ambiguous_value_constructors(
        string exposing,
        string declaration,
        string definition)
    {
        var files =
            new[]
            {
                Parse($"module First exposing (..)\n{declaration}"),
                Parse($"module Second exposing (..)\n{declaration}"),
                Parse(
                    $$"""
                    module Main exposing (root)
                    import First exposing ({{exposing}})
                    import Second exposing ({{exposing}})
                    root : Status -> Status
                    {{definition}}
                    """)
            };

        var canonicalized =
            Canonicalization.Canonicalize(files)
            .Extract(error => throw new System.Exception(error))
            [["Main"]];

        var error =
            canonicalized.Errors.Should().ContainSingle()
            .Which.Should().BeOfType<CanonicalizationError.AmbiguousImport>().Which;

        error.Name.Should().Be("Status");
        error.ImportingModules.Should().Equal("First", "Second");

        var root = DeclQualifiedName.Create(["Main"], "root");

        var demanded =
            Canonicalization.CanonicalizeDeclarations(files, [root])
            .Extract(failure => throw new System.Exception(failure));

        demanded.Declarations[root].Errors.Should().BeEquivalentTo(canonicalized.Errors);
    }

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
            .Should().BeOfType<ConcreteSyntax.Expression.ListExpr>().Which;

        var canonicalizedList =
            canonicalizedValue.Declaration.Value.Expression.Value
            .Should().BeOfType<ConcreteSyntax.Expression.ListExpr>().Which;

        canonicalizedList.Elements
            .Should().BeOfType<ConcreteSyntax.SeparatedSyntaxList<ConcreteSyntax.Node<ConcreteSyntax.Expression>>.NonEmpty>()
            .Which.Rest.Select(item => item.SeparatorLocation)
            .Should().Equal(
            parsedList.Elements
            .Should().BeOfType<ConcreteSyntax.SeparatedSyntaxList<ConcreteSyntax.Node<ConcreteSyntax.Expression>>.NonEmpty>()
            .Which.Rest.Select(item => item.SeparatorLocation));

        canonicalizedList.Elements.Last().Value
            .Should().Be(new ConcreteSyntax.Expression.IntegerLiteral("0x2A"));

        canonicalizedList.Elements.ElementAt(1).Value
            .Should().Be(new ConcreteSyntax.Expression.StringLiteral("line\n", """line\n"""));
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

    private static ConcreteSyntax.File Parse(string source) =>
        ElmSyntaxParser.ParseModuleText(source)
        .Extract(error => throw new System.Exception("Failed parsing: " + error));

    private static ConcreteSyntax.File Canonicalize(ConcreteSyntax.File file) =>
        Canonicalization.CanonicalizeOrThrow([file])
        .Extract(error => throw new System.Exception("Failed canonicalization: " + error))
        [["Test"]]
        .Extract(error => throw new System.Exception("Module canonicalization failed: " + error));

    private static ConcreteSyntax.FunctionStruct FunctionNamed(
        ConcreteSyntax.File file,
        string name) =>
        file.Declarations
        .Select(declaration => declaration.Value)
        .OfType<ConcreteSyntax.Declaration.FunctionDeclaration>()
        .Single(declaration => declaration.Function.Declaration.Value.Name.Value == name)
        .Function;
}
