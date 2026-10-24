using AwesomeAssertions;
using Pine.Core.Elm.Syntax;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using System;
using System.Linq;
using System.Numerics;
using Xunit;

using AbstractSyntax = Pine.Core.Elm.Syntax.Abstract;
using ConcreteSyntax = Pine.Core.Elm.Syntax.Concrete;

namespace Pine.Core.Tests.Elm.Syntax;

/// <summary>
/// Tests for <see cref="ElmSyntaxParser.ParseExpression(string)"/>, which parses a standalone
/// Elm expression directly (without wrapping it in a synthetic module declaration).
/// <para>
/// Each test parses the expression, converts the resulting concrete syntax node into the abstract
/// syntax model via <see cref="ConvertFromConcrete.FromExpression"/>, and asserts
/// equivalency on the complete abstract syntax node.
/// </para>
/// </summary>
public class ElmSyntaxParserExpressionTests
{
    /// <summary>
    /// Parses <paramref name="expressionText"/> with <see cref="ElmSyntaxParser.ParseExpression(string)"/>
    /// and converts the resulting concrete expression into the abstract syntax model.
    /// </summary>
    private static AbstractSyntax.Expression ParseAndConvert(string expressionText)
    {
        var concrete =
            ElmSyntaxParser.ParseExpression(expressionText)
            .Extract(err => throw new Exception("Parse failed: " + err));

        return AbstractSyntax.ConvertFromConcrete.FromExpression(concrete);
    }

    /// <summary>
    /// Parses <paramref name="expressionText"/> through the established module-wrapping path
    /// (<see cref="ElmSyntaxParser.ParseModuleText(string)"/> applied to a synthetic
    /// <c>value =</c> declaration) and converts the body expression into the abstract syntax model.
    /// Used as a reference oracle to confirm the direct expression parser yields the same abstract tree.
    /// </summary>
    private static AbstractSyntax.Expression ParseAndConvertViaModule(string expressionText)
    {
        var indented =
            string.Join(
                "\n",
                expressionText
                .Replace("\r\n", "\n")
                .Split('\n')
                .Select(line => "    " + line));

        var moduleText =
            "module Test exposing (..)\n\n\nvalue =\n" + indented + "\n";

        var file =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Module parse failed: " + err));

        var functionDeclaration =
            file.Declarations[0].Value as ConcreteSyntax.Declaration.FunctionDeclaration;

        functionDeclaration.Should().NotBeNull();

        return
            AbstractSyntax.ConvertFromConcrete.FromExpression(
                functionDeclaration!.Function.Declaration.Value.Expression.Value);
    }

    private static AbstractSyntax.Expression IntegerExpr(long value) =>
        new AbstractSyntax.Expression.IntegerLiteral(
            new BigInteger(value),
            IntegerEncoding.EncodeSignedInteger(value));

    [Fact]
    public void Parses_decimal_integer_literal()
    {
        ParseAndConvert("42")
            .Should().Be(IntegerExpr(42));
    }

    [Fact]
    public void Parses_hexadecimal_integer_literal()
    {
        ParseAndConvert("0xFF")
            .Should().Be(IntegerExpr(255));
    }

    [Fact]
    public void Parses_string_literal()
    {
        ParseAndConvert("\"hello\"")
            .Should().Be(AbstractSyntax.Expression.StringLiteral.Create("hello"));
    }

    [Fact]
    public void Parses_char_literal()
    {
        ParseAndConvert("'a'")
            .Should().Be(AbstractSyntax.Expression.CharLiteral.Create('a'));
    }

    [Fact]
    public void Parses_unit_expression()
    {
        ParseAndConvert("()")
            .Should().Be(AbstractSyntax.Expression.UnitExpr.Instance);
    }

    [Fact]
    public void Parses_function_or_value_reference()
    {
        ParseAndConvert("foo")
            .Should().Be(AbstractSyntax.Expression.Identifier.Create([], "foo"));
    }

    [Fact]
    public void Parses_qualified_function_or_value_reference()
    {
        ParseAndConvert("String.fromInt")
            .Should().Be(AbstractSyntax.Expression.Identifier.Create(["String"], "fromInt"));
    }

    [Fact]
    public void Parses_list_literal()
    {
        ParseAndConvert("[1, 2, 3]")
            .Should().Be(
            new AbstractSyntax.Expression.ListExpr(
                [IntegerExpr(1), IntegerExpr(2), IntegerExpr(3)]));
    }

    [Fact]
    public void Parses_empty_list_literal()
    {
        ParseAndConvert("[]")
            .Should().Be(new AbstractSyntax.Expression.ListExpr([]));
    }

    [Fact]
    public void Parses_tuple_expression()
    {
        ParseAndConvert("( 1, 2 )")
            .Should().Be(
            new AbstractSyntax.Expression.TupledExpression(
                [IntegerExpr(1), IntegerExpr(2)]));
    }

    [Fact]
    public void Parses_operator_application()
    {
        ParseAndConvert("1 + 2")
            .Should().Be(
            new AbstractSyntax.Expression.OperatorApplication(
                "+",
                ConcreteSyntax.InfixDirection.Left,
                IntegerExpr(1),
                IntegerExpr(2)));
    }

    [Fact]
    public void Parses_function_application()
    {
        ParseAndConvert("String.fromInt 42")
            .Should().Be(
            new AbstractSyntax.Expression.Application(
                AbstractSyntax.Expression.Identifier.Create(["String"], "fromInt"),
                [IntegerExpr(42)]));
    }

    [Fact]
    public void Parses_if_expression()
    {
        ParseAndConvert("if cond then 1 else 2")
            .Should().Be(
            new AbstractSyntax.Expression.IfBlock(
                AbstractSyntax.Expression.Identifier.Create([], "cond"),
                IntegerExpr(1),
                IntegerExpr(2)));
    }

    [Fact]
    public void Parses_negation_expression()
    {
        ParseAndConvert("-x")
            .Should().Be(
            new AbstractSyntax.Expression.Negation(
                AbstractSyntax.Expression.Identifier.Create([], "x")));
    }

    [Fact]
    public void Parses_record_access_function()
    {
        ParseAndConvert(".name")
            .Should().Be(
            new AbstractSyntax.Expression.RecordAccessFunction(
                "name",
                StringEncoding.ValueFromString("name")));
    }

    [Fact]
    public void Parses_record_access_function_as_application_argument()
    {
        ParseAndConvert("Ok .extensionRight")
            .Should().Be(
            new AbstractSyntax.Expression.Application(
                AbstractSyntax.Expression.Identifier.Create([], "Ok"),
                [
                new AbstractSyntax.Expression.RecordAccessFunction(
                    "extensionRight",
                    StringEncoding.ValueFromString("extensionRight"))
                ]));
    }

    [Theory]
    [InlineData("{ alfa : 13, beta = 17 }", "{ alfa = 13, beta = 17 }")]
    [InlineData("{ alfa = 13, beta : 17 }", "{ alfa = 13, beta = 17 }")]
    [InlineData("{alfa:13,beta:17}", "{alfa=13,beta=17}")]
    [InlineData(
        "{ alfa {- before -} : {- after -} 13 }",
        "{ alfa {- before -} = {- after -} 13 }")]
    [InlineData("{ base | alfa : 13, beta = 17 }", "{ base | alfa = 13, beta = 17 }")]
    [InlineData("{ base | alfa = 13, beta : 17 }", "{ base | alfa = 13, beta = 17 }")]
    [InlineData("{ base | inner : { alfa : 13 } }", "{ base | inner = { alfa = 13 } }")]
    [InlineData("{ base : other }", "{ base = other }")]
    public void Record_field_colon_parses_as_equals(
        string sourceExpression,
        string canonicalExpression)
    {
        var parsedSource =
            ElmSyntaxParser.ParseExpression(sourceExpression)
            .Extract(err => throw new Exception("Source parse failed: " + err));

        var parsedCanonical =
            ElmSyntaxParser.ParseExpression(canonicalExpression)
            .Extract(err => throw new Exception("Canonical parse failed: " + err));

        parsedSource.Should().Be(parsedCanonical);
    }

    [Theory]
    [InlineData("{ alfa = 13, beta :: 17 }")]
    [InlineData("{ base : alfa = 13 }")]
    public void Invalid_record_field_separator_recovery_is_rejected(string expressionText)
    {
        ElmSyntaxParser.ParseExpression(expressionText)
            .IsErrOrNull()
            .Should().NotBeNull();
    }

    [Fact]
    public void Comment_before_dot_ends_qualified_name_and_starts_record_accessor_argument()
    {
        // Reference-tool experiments:
        // https://github.com/Viir/super-duper-disco/blob/main/explore/internal-analysis/2026-08-30-elm-qualified-name-trivia.md
        ParseAndConvert("Alfa{- comment -}.beta")
            .Should().Be(
            new AbstractSyntax.Expression.Application(
                AbstractSyntax.Expression.Identifier.Create([], "Alfa"),
                [
                new AbstractSyntax.Expression.RecordAccessFunction(
                    "beta",
                    StringEncoding.ValueFromString("beta"))
                ]));
    }

    [Theory]
    [InlineData("Alfa.{- comment -}beta")]
    [InlineData("Alfa{- comment -}.Beta")]
    public void Invalid_record_accessor_forms_are_rejected(string expressionText)
    {
        // Reference-tool experiments:
        // https://github.com/Viir/super-duper-disco/blob/main/explore/internal-analysis/2026-08-30-elm-qualified-name-trivia.md
        ElmSyntaxParser.ParseExpression(expressionText)
            .IsErrOrNull()
            .Should().NotBeNull();
    }

    [Theory]
    // Literals
    [InlineData("0")]
    [InlineData("123")]
    [InlineData("-7")]
    [InlineData("0x1F")]
    [InlineData("3.14")]
    [InlineData("\"a string with \\\"quotes\\\"\"")]
    [InlineData("'z'")]
    [InlineData("()")]
    // References and access
    [InlineData("identifier")]
    [InlineData("Module.Sub.value")]
    [InlineData("record.field")]
    [InlineData(".field")]
    [InlineData("(+)")]
    // Collections
    [InlineData("[]")]
    [InlineData("[1, 2, 3]")]
    [InlineData("[ \"a\", \"b\" ]")]
    [InlineData("( 1, 2 )")]
    [InlineData("( 1, 2, 3 )")]
    // Operators and application
    [InlineData("1 + 2 * 3")]
    [InlineData("a |> b |> c")]
    [InlineData("f x y z")]
    [InlineData("a == b && c /= d")]
    [InlineData("\"a\" ++ \"b\" ++ \"c\"")]
    // Records
    [InlineData("{ a = 1, b = 2 }")]
    [InlineData("{ record | a = 1 }")]
    // Control flow
    [InlineData("if a then b else c")]
    [InlineData("if a then b else if c then d else e")]
    [InlineData("\\x -> x + 1")]
    [InlineData("\\x y -> x + y")]
    // Multi-line constructs
    [InlineData("let\n    x = 1\nin\nx")]
    [InlineData("let\n    x = 1\n    y = 2\nin\nx + y")]
    [InlineData("case x of\n    A -> 1\n    B -> 2")]
    [InlineData("foo\n    bar\n    baz")]
    [InlineData("Just\n    (1 + 2)")]
    public void Direct_expression_parse_matches_module_wrapping_reference(string expressionText)
    {
        var direct = ParseAndConvert(expressionText);

        var viaModule = ParseAndConvertViaModule(expressionText);

        direct.Should().Be(viaModule);
    }

    [Fact]
    public void Parse_of_empty_input_returns_error()
    {
        var result = ElmSyntaxParser.ParseExpression("   ");

        result.IsErrOrNull().Should().NotBeNull();
    }

    [Fact]
    public void Parse_with_trailing_unexpected_token_returns_error()
    {
        // A leftover closing parenthesis after a complete expression must be reported as an error
        // rather than silently ignored.
        var result = ElmSyntaxParser.ParseExpression("1 )");

        result.IsErrOrNull().Should().NotBeNull();
    }
}
