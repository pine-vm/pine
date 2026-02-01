using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using Xunit;

using ExpressionSyntax = Pine.Core.Elm.ElmSyntax.SyntaxModel.Expression;

namespace Pine.Core.Tests.Elm.ElmSyntax;

public class ElmSyntaxParserTests
{
    [Theory]
    [InlineData("0xe5")]
    [InlineData("0xABCDEF")]
    [InlineData("0xface")]
    [InlineData("0xCAFE")]
    public void Hex_literals_with_e_are_parsed_as_integers(string hexLiteral)
    {
        // Hex literals can contain 'e' as a digit and should not be misidentified as floats
        var input =
            $""""
            module Test exposing (..)


            value =
                {hexLiteral}
            """";

        var parsedFile =
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new System.Exception(err));

        var funcDecl =
            parsedFile.Declarations[0].Value as Declaration.FunctionDeclaration;

        funcDecl.Should().NotBeNull();

        var expr = funcDecl!.Function.Declaration.Value.Expression.Value as ExpressionSyntax.Integer;

        expr.Should().NotBeNull("the expression should be parsed as an integer, not a float");

        expr!.LiteralText.Should().Be(hexLiteral);
    }

    [Fact]
    public void Record_field_range_ends_at_field_type_end()
    {
        // Test that record field ranges end exactly where the field type ends,
        // not including any trailing whitespace or comments
        var input =
            """"
            module Test exposing (..)


            type alias Record =
                { a : Int
                , b : String
                }
            """";

        var parsedFile =
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new System.Exception(err));

        var aliasDecl =
            parsedFile.Declarations[0].Value as Declaration.AliasDeclaration;

        aliasDecl.Should().NotBeNull();

        var recordType = aliasDecl!.TypeAlias.TypeAnnotation.Value as TypeAnnotation.Record;
        recordType.Should().NotBeNull();

        var fields = recordType!.RecordDefinition.Fields as SeparatedSyntaxList<Node<RecordField>>.NonEmpty;
        fields.Should().NotBeNull();

        // First field: a : Int
        var firstField = fields!.First;
        // The field should end where "Int" ends
        firstField.Value.FieldType.Range.End.Should().Be(firstField.Range.End,
            "first field range should end at the field type end");

        // Second field: b : String
        var secondField = fields.Rest[0].Node;
        // The field should end where "String" ends
        secondField.Value.FieldType.Range.End.Should().Be(secondField.Range.End,
            "second field range should end at the field type end");
    }

    [Fact]
    public void Record_field_range_ends_at_field_type_end_with_trailing_comment()
    {
        // Test that record field ranges end at the field type, not at the trailing comment
        var input =
            """"
            module Test exposing (..)


            type alias Record =
                { a : Int -- trailing comment
                , b : String
                }
            """";

        var parsedFile =
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new System.Exception(err));

        var aliasDecl =
            parsedFile.Declarations[0].Value as Declaration.AliasDeclaration;

        aliasDecl.Should().NotBeNull();

        var recordType = aliasDecl!.TypeAlias.TypeAnnotation.Value as TypeAnnotation.Record;
        recordType.Should().NotBeNull();

        var fields = recordType!.RecordDefinition.Fields as SeparatedSyntaxList<Node<RecordField>>.NonEmpty;
        fields.Should().NotBeNull();

        // First field: a : Int -- trailing comment
        var firstField = fields!.First;
        // The field range should end where "Int" ends, NOT at the end of the comment
        firstField.Value.FieldType.Range.End.Should().Be(firstField.Range.End,
            "first field range should end at the field type end, not including trailing comment");

        // Verify the type annotation ends at "Int", not at the comment
        // Row 5: "    { a : Int -- trailing comment"
        // "Int" ends at column 14 (1-indexed)
        firstField.Value.FieldType.Range.End.Column.Should().Be(14,
            "Int should end at column 14");
    }

    [Fact]
    public void Record_field_range_ends_at_field_type_end_multiline_with_comments_between()
    {
        // Test with comments between fields
        var input =
            """"
            module Test exposing (..)


            type alias Record =
                { a : Int

                -- comment between fields
                , b : String -- trailing
                }
            """";

        var parsedFile =
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new System.Exception(err));

        var aliasDecl =
            parsedFile.Declarations[0].Value as Declaration.AliasDeclaration;

        var recordType = aliasDecl!.TypeAlias.TypeAnnotation.Value as TypeAnnotation.Record;
        var fields = recordType!.RecordDefinition.Fields as SeparatedSyntaxList<Node<RecordField>>.NonEmpty;

        // First field: a : Int
        var firstField = fields!.First;
        firstField.Value.FieldType.Range.End.Should().Be(firstField.Range.End,
            "first field range should end at the field type end");

        // Second field: b : String -- trailing
        var secondField = fields.Rest[0].Node;
        secondField.Value.FieldType.Range.End.Should().Be(secondField.Range.End,
            "second field range should end at the field type end, not including trailing comment");
    }
}
