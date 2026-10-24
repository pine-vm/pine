using AwesomeAssertions;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Concrete;
using Pine.Core.Functional;
using System.Linq;
using Xunit;

using ExpressionSyntax = Pine.Core.Elm.Syntax.Concrete.Expression;

namespace Pine.Core.Tests.Elm.Syntax;

public class ElmSyntaxParserTests
{
    [Theory]
    [InlineData("{-| selected -}\nvalue = 1", "{-| selected -}")]
    [InlineData("{-| selected -}\n\nvalue = 1", null)]
    [InlineData("{-| ignored -}\nimport Alfa\n\nvalue = 1", null)]
    [InlineData("import Alfa\n{-| selected -}\n\nvalue = 1", "{-| selected -}")]
    [InlineData("{-| ignored -}\n\nimport Alfa\n{-| selected -}\n\nvalue = 1", "{-| selected -}")]
    [InlineData("{-| ignored -}\n{-| selected -}\nvalue = 1", "{-| selected -}")]
    [InlineData("{-| selected -} {- ordinary -}\nvalue = 1", "{-| selected -}")]
    [InlineData("{-| ignored -}\n-- intervening\nvalue = 1", null)]
    [InlineData("-- earlier\n{-| selected -}\nvalue = 1", "{-| selected -}")]
    [InlineData("{-| ignored -}\n{- ordinary -}\n{-| selected -}\nvalue = 1", "{-| selected -}")]
    [InlineData("{-| ignored -}\n{- ordinary\nmultiline -}\nvalue = 1", null)]
    public void Documentation_attachment_preserves_first_declaration_and_import_boundaries(
        string body, string? expectedDocumentation)
    {
        var source = "module Test exposing (..)\n\n" + body;

        var file =
            ElmSyntaxParser.ParseModuleText(source)
            .Extract(error => throw new System.Exception(error.ToString()));

        file.IncompleteDeclarations.Should().BeEmpty();
        var declaration = file.Declarations.Should().ContainSingle().Which;
        var function = declaration.Value.Should().BeOfType<Declaration.FunctionDeclaration>().Subject.Function;
        function.Documentation?.Value.Should().Be(expectedDocumentation);

        var commentCount = body.Split('\n').Count(line => line.StartsWith("{-") || line.StartsWith("--"));

        if (body.Contains("{- ordinary -}", System.StringComparison.Ordinal) && body.StartsWith("{-| selected -}"))
            commentCount++;

        file.Comments.Should().HaveCount(commentCount - (expectedDocumentation is null ? 0 : 1));

        if (function.Documentation is { } documentation)
        {
            documentation.Range.End.Row.Should().BeLessThan(function.Declaration.Range.Start.Row);
            declaration.Range.Start.Should().Be(documentation.Range.Start);
            file.Comments.Should().NotContain(comment => comment.Range == documentation.Range);
        }
    }

    [Theory]
    [InlineData("\n")]
    [InlineData("\r\n")]
    public void Documentation_attachment_ignores_future_comments_and_preserves_global_order(string newline)
    {
        var source =
            """
            {-| before module -}
            module Test exposing (..)

            {-| first -}
            first = 1

            {-| blocked -}
            -- intervening
            second = 2

            -- before later documentation
            {-| superseded -}
            {-| third -}

            third = 3

            {-| trailing -}
            """.Replace("\n", newline);

        var file =
            ElmSyntaxParser.ParseModuleText(source)
            .Extract(error => throw new System.Exception(error.ToString()));

        file.IncompleteDeclarations.Should().BeEmpty();
        file.Declarations.Should().HaveCount(3);

        file.Declarations.Select(node => ((Declaration.FunctionDeclaration)node.Value).Function.Documentation?.Value)
            .Should().Equal("{-| first -}", null, "{-| third -}");

        file.Comments.Select(comment => comment.Value).Should().Equal(
            "{-| before module -}",
            "{-| blocked -}",
            "-- intervening",
            "-- before later documentation",
            "{-| superseded -}",
            "{-| trailing -}");

        file.Comments.Select(comment => comment.Range.Start.Row).Should().Equal(1, 7, 8, 11, 12, 17);
    }

    [Fact]
    public void Documentation_attachment_excludes_only_supported_declaration_kinds()
    {
        const string Source =
            """
            port module Test exposing (..)

            {-| alias -}
            type alias Alias = Int

            {-| choice -}
            type Choice = Choice

            {-| port -}
            port send : Int -> Cmd msg

            {-| infix -}
            infix left 6 (+) = add

            {-| function -}
            add a b = a
            """;

        var file =
            ElmSyntaxParser.ParseModuleText(Source)
            .Extract(error => throw new System.Exception(error.ToString()));

        file.IncompleteDeclarations.Should().BeEmpty();
        file.Declarations.Should().HaveCount(5);

        ((Declaration.AliasDeclaration)file.Declarations[0].Value).TypeAlias.Documentation!.Value.Should().Be(
            "{-| alias -}");

        ((Declaration.ChoiceTypeDeclaration)file.Declarations[1].Value).TypeDeclaration.Documentation!.Value
            .Should().Be("{-| choice -}");

        ((Declaration.FunctionDeclaration)file.Declarations[4].Value).Function.Documentation!.Value.Should().Be(
            "{-| function -}");

        file.Comments.Select(comment => comment.Value).Should().Equal("{-| port -}", "{-| infix -}");
    }

    [Theory]
    [InlineData("", "{-| reused -}")]
    [InlineData("-- blocker", null)]
    public void Tree_recovery_preserves_documentation_eligibility_after_an_incomplete_declaration(
        string interveningComment, string? expectedDocumentation)
    {
        var source =
            string.Join(
                "\n",
                "effect module Test where { command = Cmd } exposing (..)",
                "",
                "first = 1",
                "",
                "{-| reused -}",
                "broken = )",
                interveningComment,
                "second = 2");

        ElmSyntaxDiagnostics.Parse(source).Should().BeEmpty();

        var file =
            ElmSyntaxParser.ParseModuleText(source)
            .Extract(error => throw new System.Exception(error.ToString()));

        file.Declarations.Should().HaveCount(2);
        var second = ((Declaration.FunctionDeclaration)file.Declarations[1].Value).Function;
        second.Documentation?.Value.Should().Be(expectedDocumentation);
        second.Declaration.Range.Start.Should().Be(new Location(8, 1));
        file.IncompleteDeclarations.Should().ContainSingle().Which.Value.OriginalText.Should().Be("broken = )");

        file.IncompleteDeclarations[0].Range.Should().Be(
            new Range(new(6, 1), new(6, 11)));

        file.Comments.Select(comment => comment.Value).Should().Equal(
            expectedDocumentation is null ? ["{-| reused -}", "-- blocker"] : []);
    }

    [Fact]
    public void Tree_recovery_before_first_complete_declaration_keeps_unattached_documentation_global()
    {
        const string Source =
            """
            effect module Test where { command = Cmd } exposing (..)

            {-| unattached -}
            broken = )

            value = 1
            """;

        var file =
            ElmSyntaxParser.ParseModuleText(Source)
            .Extract(error => throw new System.Exception(error.ToString()));

        file.Declarations.Should().ContainSingle();
        ((Declaration.FunctionDeclaration)file.Declarations[0].Value).Function.Documentation.Should().BeNull();
        file.IncompleteDeclarations.Should().ContainSingle().Which.Range.Start.Should().Be(new Location(4, 1));
        file.Comments.Should().ContainSingle().Which.Value.Should().Be("{-| unattached -}");
    }

    [Theory]
    [InlineData("\n")]
    [InlineData("\r\n")]
    public void Tree_recovery_reconstructs_literal_delimiters_escapes_and_indentation(string newline)
    {
        var source =
            string.Join(
                newline,
                "effect module Test where { command = Cmd } exposing (..)",
                "",
                "broken =",
                "    [ \"a\\n\", \"\"\"line",
                "next\"\"\", '\\u{1F41F}',",
                "",
                "after = 1");

        ElmSyntaxDiagnostics.Parse(source).Should().BeEmpty();

        var file =
            ElmSyntaxParser.ParseModuleText(source)
            .Extract(error => throw new System.Exception(error.ToString()));

        file.Declarations.Should().ContainSingle().Which.Range.Start.Should().Be(new Location(7, 1));

        file.IncompleteDeclarations.Should().ContainSingle().Which.Value.OriginalText.Should().Be(
            "broken =\n    [ \"a\\n\", \"\"\"line\nnext\"\"\", '\\u{1F41F}',");

        file.IncompleteDeclarations[0].Range.Should().Be(
            new Range(new(3, 1), new(5, 22)));
    }

    [Theory]
    [InlineData("\n")]
    [InlineData("\r\n")]
    [InlineData("\r")]
    public void Diagnostic_masking_preserves_recovery_ranges_error_order_and_newlines(string newline)
    {
        var source =
            string.Join(
                newline,
                "module Test exposing (..)",
                "",
                "first = 1",
                "",
                "broken = \"\\q\"",
                "",
                "wrong : Int",
                "other = 2",
                "",
                "last = 3",
                "",
                "{-| trailing one -}",
                "{-| trailing two -}",
                "");

        var diagnostics = ElmSyntaxDiagnostics.Parse(source);

        var file =
            ElmSyntaxParser.ParseModuleText(source)
            .Extract(error => throw new System.Exception(error.ToString()));

        file.Declarations
            .Select(
            node => ((Declaration.FunctionDeclaration)node.Value).Function.Declaration.Value.Name.Value)
            .Should().Equal("first", "last");

        file.Declarations.Select(node => node.Range.Start.Row).Should().Equal(3, 10);
        file.IncompleteDeclarations.Select(node => node.Range.Start.Row).Should().Equal(5, 7);

        file.IncompleteDeclarations.Select(node => node.Value.OriginalText)
            .Should().Equal("broken = \"\\q\"", "wrong : Int\nother = 2");

        file.IncompleteDeclarations.Select(node => node.Value.ParseError)
            .Should().Equal(
            diagnostics.Where(diagnostic => !diagnostic.PreserveTree).Select(diagnostic => diagnostic.Error));

        file.IncompleteDeclarations.Select(node => node.Range)
            .Should().Equal(
            diagnostics.Where(diagnostic => !diagnostic.PreserveTree).Select(diagnostic => diagnostic.Declaration!));

        file.AdditionalParseErrors.Should().Equal(
            diagnostics.Where(diagnostic => diagnostic.PreserveTree).Select(diagnostic => diagnostic.Error));

        file.Comments.Select(comment => comment.Value).Should().Equal("{-| trailing one -}", "{-| trailing two -}");
    }

    [Theory]
    [InlineData("\"plain 🐟 text\"", "plain 🐟 text", "plain 🐟 text")]
    [InlineData("\"\"", "", "")]
    [InlineData("\"prefix\\n\\u{1F41F}suffix\"", "prefix\n🐟suffix", "prefix\\n\\u{1F41F}suffix")]
    [InlineData("\"\\t\\r\\b\\\\\\\"\"", "\t\r\b\\\"", "\\t\\r\\b\\\\\\\"")]
    public void String_tokenization_preserves_decoded_values_and_original_escapes(
        string source, string expectedValue, string expectedSourceText)
    {
        var expression =
            ElmSyntaxParser.ParseExpression(source)
            .Extract(error => throw new System.Exception(error));

        var literal = expression.Should().BeOfType<ExpressionSyntax.StringLiteral>().Subject;
        literal.Value.Should().Be(expectedValue);
        literal.SourceText.Should().Be(expectedSourceText);
    }

    [Theory]
    [InlineData("\"\\r\"", "\r")]
    [InlineData("\"\\u{000D}\"", "\r")]
    [InlineData("\"\\u{000d}\"", "\r")]
    [InlineData("\"\\r\\u{000D}\\n\"", "\r\r\n")]
    [InlineData("\"\\\\r\"", "\\r")]
    [InlineData("\"\"\"first\\r\r\nsecond\\u{000D}\"\"\"", "first\r\nsecond\r")]
    public void String_patterns_preserve_decoded_values_and_original_literal_text(
        string source, string expectedValue)
    {
        var expression =
            ElmSyntaxParser.ParseExpression("case text of\n    " + source + " ->\n        1")
            .Extract(error => throw new System.Exception(error));

        var caseExpression = expression.Should().BeOfType<ExpressionSyntax.CaseExpression>().Subject;

        var pattern =
            caseExpression.CaseBlock.Cases[0].Pattern.Value
            .Should().BeOfType<Pattern.StringPattern>().Subject;

        pattern.Value.Should().Be(expectedValue);
        pattern.LiteralText.Should().Be(source.Replace("\r\n", "\n"));
    }

    [Theory]
    [InlineData("'🐟'", 0x1F41F)]
    [InlineData("'\\u{1F41F}'", 0x1F41F)]
    [InlineData("'\\n'", 10)]
    [InlineData("'x'", 120)]
    public void Character_tokenization_preserves_supplementary_and_escaped_code_points(string source, int expected)
    {
        var expression =
            ElmSyntaxParser.ParseExpression(source)
            .Extract(error => throw new System.Exception(error));

        expression.Should().BeOfType<ExpressionSyntax.CharLiteral>().Which.Value.Should().Be(expected);
    }

    [Theory]
    [InlineData("\n")]
    [InlineData("\r\n")]
    [InlineData("\r")]
    public void Multiline_string_tokenization_normalizes_newlines_without_decoding_source_escapes(string newline)
    {
        var source = "\"\"\"prefix" + newline + "🐟\\nend\"\"\"";

        var expression =
            ElmSyntaxParser.ParseExpression(source)
            .Extract(error => throw new System.Exception(error));

        var literal = expression.Should().BeOfType<ExpressionSyntax.MultilineStringLiteral>().Subject;
        literal.Value.Should().Be("prefix\n🐟\nend");
        literal.SourceLines.Should().Equal("prefix", "🐟\\nend");
    }

    [Theory]
    [InlineData("0xCAFE", false)]
    [InlineData("012345678901234567890123456789", false)]
    [InlineData("12.3400", true)]
    [InlineData("1e+09", true)]
    [InlineData("42E-3", true)]
    public void Number_tokenization_preserves_literal_spelling(string source, bool isFloat)
    {
        var expression =
            ElmSyntaxParser.ParseExpression(source)
            .Extract(error => throw new System.Exception(error));

        if (isFloat)
            expression.Should().BeOfType<ExpressionSyntax.FloatLiteral>().Which.LiteralText.Should().Be(source);

        else
            expression.Should().BeOfType<ExpressionSyntax.IntegerLiteral>().Which.LiteralText.Should().Be(source);
    }

    [Theory]
    [InlineData("\n")]
    [InlineData("\r\n")]
    public void Comment_tokenization_preserves_nested_text_and_code_point_locations(string newline)
    {
        var source =
            "module Test exposing (..)" + newline +
            "-- 🐟 single" + newline +
            "{- outer" + newline +
            "   {- 🐟 nested -}" + newline +
            "-}" + newline +
            "value = 1";

        var file =
            ElmSyntaxParser.ParseModuleText(source)
            .Extract(error => throw new System.Exception(error.ToString()));

        file.IncompleteDeclarations.Should().BeEmpty();

        file.Comments.Select(comment => comment.Value)
            .Should().Equal("-- 🐟 single", "{- outer" + newline + "   {- 🐟 nested -}" + newline + "-}");

        file.Comments[0].Range.Should().Be(new Range(new(2, 1), new(2, 12)));
        file.Comments[1].Range.Should().Be(new Range(new(3, 1), new(5, 3)));
        file.Declarations[0].Range.Start.Should().Be(new Location(6, 1));
    }

    [Fact]
    public void GLSL_tokenization_preserves_source_newlines_and_delimiter_like_characters()
    {
        const string Shader = "\r\nvoid main() { /* 🐟 */ }\r\n| x ]\n";

        var expression =
            ElmSyntaxParser.ParseExpression("[glsl|" + Shader + "|]")
            .Extract(error => throw new System.Exception(error));

        expression.Should().BeOfType<ExpressionSyntax.GLSLExpression>().Which.ShaderCode.Should().Be(Shader);
    }

    [Fact]
    public void Parses_outdented_record_type_argument_in_record_field()
    {
        var input =
            """
            module Test exposing (..)

            type alias Token =
                { tokenType : TokenType
              {}
                , lexeme : String
                }
            """;

        var parsedFile =
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new System.Exception(err.ToString()));

        var aliasDeclaration =
            parsedFile.Declarations[0].Value.Should().BeOfType<Declaration.AliasDeclaration>().Subject;

        var recordType =
            aliasDeclaration.TypeAlias.TypeAnnotation.Value.Should().BeOfType<TypeAnnotation.Record>().Subject;

        var fields =
            recordType.RecordDefinition.Fields.Should()
            .BeOfType<SeparatedSyntaxList<Node<RecordField>>.NonEmpty>().Subject;

        var tokenType =
            fields.First.Value.FieldType.Value.Should().BeOfType<TypeAnnotation.Typed>().Subject;

        tokenType.TypeArguments.Should().ContainSingle()
            .Which.Value.Should().BeOfType<TypeAnnotation.Record>();
    }

    [Theory]
    [InlineData("module Main{- comment -}.Nested exposing (..)")]
    [InlineData("module Main exposing (..)\n\nimport Alfa{- comment -}.Beta")]
    public void Comment_between_module_name_parts_is_rejected(string elmModuleText)
    {
        // Reference-tool experiments:
        // https://github.com/Viir/super-duper-disco/blob/main/explore/internal-analysis/2026-08-30-elm-qualified-name-trivia.md
        ElmSyntaxParser.ParseModuleText(elmModuleText)
            .IsErrOrNullable()
            .Should().NotBeNull();
    }

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
            .Extract(err => throw new System.Exception(err.ToString()));

        var funcDecl =
            parsedFile.Declarations[0].Value as Declaration.FunctionDeclaration;

        funcDecl.Should().NotBeNull();

        var expr = funcDecl!.Function.Declaration.Value.Expression.Value as ExpressionSyntax.IntegerLiteral;

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
            .Extract(err => throw new System.Exception(err.ToString()));

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
        firstField.Value.FieldType.Range.End.Should().Be(
            firstField.Range.End,
            "first field range should end at the field type end");

        // Second field: b : String
        var secondField = fields.Rest[0].Node;
        // The field should end where "String" ends
        secondField.Value.FieldType.Range.End.Should().Be(
            secondField.Range.End,
            "second field range should end at the field type end");
    }

    [Fact]
    public void Application_expression_uses_separate_function_property()
    {
        var input =
            """"
            module Test exposing (..)


            value =
                foo bar baz
            """";

        var parsedFile =
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new System.Exception(err.ToString()));

        var funcDecl =
            parsedFile.Declarations[0].Value as Declaration.FunctionDeclaration;

        funcDecl.Should().NotBeNull();

        var expr = funcDecl!.Function.Declaration.Value.Expression.Value as ExpressionSyntax.Application;

        expr.Should().NotBeNull();
        expr!.Arguments.Should().HaveCount(2);
        expr.Function.Value.Should().Be(new ExpressionSyntax.Identifier([], "foo"));
        expr.Arguments[0].Value.Should().Be(new ExpressionSyntax.Identifier([], "bar"));
        expr.Arguments[1].Value.Should().Be(new ExpressionSyntax.Identifier([], "baz"));
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
            .Extract(err => throw new System.Exception(err.ToString()));

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
        firstField.Value.FieldType.Range.End.Should().Be(
            firstField.Range.End,
            "first field range should end at the field type end, not including trailing comment");

        // Verify the type annotation ends at "Int", not at the comment
        // Row 5: "    { a : Int -- trailing comment"
        // "Int" ends at column 14 (1-indexed)
        firstField.Value.FieldType.Range.End.Column.Should().Be(
            14,
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
            .Extract(err => throw new System.Exception(err.ToString()));

        var aliasDecl =
            parsedFile.Declarations[0].Value as Declaration.AliasDeclaration;

        var recordType = aliasDecl!.TypeAlias.TypeAnnotation.Value as TypeAnnotation.Record;
        var fields = recordType!.RecordDefinition.Fields as SeparatedSyntaxList<Node<RecordField>>.NonEmpty;

        // First field: a : Int
        var firstField = fields!.First;

        firstField.Value.FieldType.Range.End.Should().Be(
            firstField.Range.End,
            "first field range should end at the field type end");

        // Second field: b : String -- trailing
        var secondField = fields.Rest[0].Node;

        secondField.Value.FieldType.Range.End.Should().Be(
            secondField.Range.End,
            "second field range should end at the field type end, not including trailing comment");
    }

    [Fact]
    public void Parses_imports_after_top_level_declarations()
    {
        var input =
            """"
            module Test exposing (..)

            import String


            x =
                1

            import Dict exposing (empty)

            y =
                2
            """";

        var parsedFile =
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new System.Exception(err.ToString()));

        parsedFile.Imports.Select(import => string.Join(".", import.Value.ModuleName.Value))
            .Should()
            .Equal("String", "Dict");

        parsedFile.IncompleteDeclarations.Should().BeEmpty();

        parsedFile.Declarations.Should().HaveCount(2);

        parsedFile.Declarations
            .Select(
            declaration =>
            ((Declaration.FunctionDeclaration)declaration.Value).Function.Declaration.Value.Name.Value)
            .Should()
            .Equal("x", "y");
    }

    [Fact]
    public void Nested_case_expression_does_not_absorb_dedented_outer_branch()
    {
        var input =
            """"
            module Test exposing (value)


            value parse step s0 =
                case parse s0 of
                    Good p1 step1 s1 ->
                        case step of
                            Loop newState ->
                                p1

                            Done result ->
                                result

                    Bad p1 x ->
                        x
            """";

        var parsedFile =
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new System.Exception(err.ToString()));

        var functionDeclaration =
            parsedFile.Declarations
            .Select(declaration => declaration.Value)
            .OfType<Declaration.FunctionDeclaration>()
            .Single();

        var outerCase =
            functionDeclaration.Function.Declaration.Value.Expression.Value
            .Should()
            .BeOfType<ExpressionSyntax.CaseExpression>()
            .Subject;

        outerCase.CaseBlock.Cases.Should().HaveCount(2);

        var nestedCase =
            outerCase.CaseBlock.Cases[0].Expression.Value
            .Should()
            .BeOfType<ExpressionSyntax.CaseExpression>()
            .Subject;

        nestedCase.CaseBlock.Cases.Should().HaveCount(2);
        outerCase.CaseBlock.Cases[1].Expression.Value.Should().BeOfType<ExpressionSyntax.Identifier>();
    }

    [Fact]
    public void Cons_pattern_with_multi_argument_constructor_head_parses()
    {
        // Regression test reproducing a ParserException seen when parsing
        // elm/parser-1.1.0 src/Parser/Advanced.elm:
        //
        //     case parsers of
        //         [] -> Bad False bag
        //         Parser parse :: remainingParsers -> ...
        //
        // The parser used to consume `Parser parse` as a NamedPattern with
        // an argument and return immediately, never looking for `::` on the
        // outer cons pattern, so the `::` token was left at the case-branch
        // pattern position and the `->` that followed was reported as an
        // unexpected token.
        var input =
            """"
            module Test exposing (..)


            run parsers =
                case parsers of
                    [] ->
                        0

                    Parser parse :: remainingParsers ->
                        parse remainingParsers
            """";

        var parsedFile =
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new System.Exception(err.ToString()));

        parsedFile.IncompleteDeclarations.Should().BeEmpty();

        var funcDecl =
            parsedFile.Declarations[0].Value as Declaration.FunctionDeclaration;

        funcDecl.Should().NotBeNull();

        var caseExpr =
            funcDecl!.Function.Declaration.Value.Expression.Value
            .Should()
            .BeOfType<ExpressionSyntax.CaseExpression>()
            .Subject;

        caseExpr.CaseBlock.Cases.Should().HaveCount(2);

        var consBranch = caseExpr.CaseBlock.Cases[1];

        var consPattern =
            consBranch.Pattern.Value
            .Should()
            .BeOfType<Pattern.UnConsPattern>()
            .Subject;

        var headPattern =
            consPattern.Head.Value
            .Should()
            .BeOfType<Pattern.NamedPattern>()
            .Subject;

        headPattern.Name.Name.Should().Be("Parser");
        headPattern.Arguments.Should().HaveCount(1);

        headPattern.Arguments[0].Value.Should().BeOfType<Pattern.VarPattern>()
            .Which.Name.Should().Be("parse");

        consPattern.Tail.Value.Should().BeOfType<Pattern.VarPattern>()
            .Which.Name.Should().Be("remainingParsers");
    }

    [Fact]
    public void As_pattern_after_multi_argument_constructor_parses()
    {
        // Regression test reproducing the companion shape from
        // elm/parser-1.1.0 src/Parser/Advanced.elm `oneOfHelp`:
        //
        //     case parse s0 of
        //         Good _ _ _ as step -> step
        //         Bad p x as step -> ...
        //
        // Same root cause as the cons-pattern regression: a NamedPattern with
        // arguments must still allow a trailing `as <name>` alias.
        var input =
            """"
            module Test exposing (..)


            classify step =
                case step of
                    Good a b c as ok ->
                        ok

                    Bad p x as bad ->
                        bad
            """";

        var parsedFile =
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new System.Exception(err.ToString()));

        parsedFile.IncompleteDeclarations.Should().BeEmpty();

        var funcDecl =
            parsedFile.Declarations[0].Value as Declaration.FunctionDeclaration;

        funcDecl.Should().NotBeNull();

        var caseExpr =
            funcDecl!.Function.Declaration.Value.Expression.Value
            .Should()
            .BeOfType<ExpressionSyntax.CaseExpression>()
            .Subject;

        caseExpr.CaseBlock.Cases.Should().HaveCount(2);

        var firstAs =
            caseExpr.CaseBlock.Cases[0].Pattern.Value
            .Should()
            .BeOfType<Pattern.AsPattern>()
            .Subject;

        firstAs.Name.Value.Should().Be("ok");

        var firstInner =
            firstAs.Pattern.Value
            .Should()
            .BeOfType<Pattern.NamedPattern>()
            .Subject;

        firstInner.Name.Name.Should().Be("Good");
        firstInner.Arguments.Should().HaveCount(3);
    }
}
