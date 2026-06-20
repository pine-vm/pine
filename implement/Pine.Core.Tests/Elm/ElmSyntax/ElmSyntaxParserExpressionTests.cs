using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Linq;
using System.Numerics;
using Xunit;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using Concrete = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmSyntax;

/// <summary>
/// Tests for <see cref="ElmSyntaxParser.ParseExpression(string)"/>, which parses a standalone
/// Elm expression directly (without wrapping it in a synthetic module declaration).
/// <para>
/// Each test parses the expression, converts the resulting concrete syntax node into the abstract
/// syntax model via <see cref="Abstract.ConvertFromConcrete.FromExpression"/>, and asserts
/// equivalency on the complete abstract syntax node.
/// </para>
/// </summary>
public class ElmSyntaxParserExpressionTests
{
    /// <summary>
    /// Parses <paramref name="expressionText"/> with <see cref="ElmSyntaxParser.ParseExpression(string)"/>
    /// and converts the resulting concrete expression into the abstract syntax model.
    /// </summary>
    private static Abstract.Expression ParseAndConvert(string expressionText)
    {
        var concrete =
            ElmSyntaxParser.ParseExpression(expressionText)
            .Extract(err => throw new Exception("Parse failed: " + err));

        return Abstract.ConvertFromConcrete.FromExpression(concrete);
    }

    /// <summary>
    /// Parses <paramref name="expressionText"/> through the established module-wrapping path
    /// (<see cref="ElmSyntaxParser.ParseModuleText(string)"/> applied to a synthetic
    /// <c>value =</c> declaration) and converts the body expression into the abstract syntax model.
    /// Used as a reference oracle to confirm the direct expression parser yields the same abstract tree.
    /// </summary>
    private static Abstract.Expression ParseAndConvertViaModule(string expressionText)
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
            file.Declarations[0].Value as Concrete.Declaration.FunctionDeclaration;

        functionDeclaration.Should().NotBeNull();

        return
            Abstract.ConvertFromConcrete.FromExpression(
                functionDeclaration!.Function.Declaration.Value.Expression.Value);
    }

    private static Abstract.Expression IntegerExpr(long value) =>
        new Abstract.Expression.Integer(
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
            .Should().Be(Abstract.Expression.StringLiteral.Create("hello"));
    }

    [Fact]
    public void Parses_char_literal()
    {
        ParseAndConvert("'a'")
            .Should().Be(Abstract.Expression.CharLiteral.Create('a'));
    }

    [Fact]
    public void Parses_unit_expression()
    {
        ParseAndConvert("()")
            .Should().Be(Abstract.Expression.UnitExpr.Instance);
    }

    [Fact]
    public void Parses_function_or_value_reference()
    {
        ParseAndConvert("foo")
            .Should().Be(Abstract.Expression.FunctionOrValue.Create([], "foo"));
    }

    [Fact]
    public void Parses_qualified_function_or_value_reference()
    {
        ParseAndConvert("String.fromInt")
            .Should().Be(Abstract.Expression.FunctionOrValue.Create(["String"], "fromInt"));
    }

    [Fact]
    public void Parses_list_literal()
    {
        ParseAndConvert("[1, 2, 3]")
            .Should().Be(
            new Abstract.Expression.ListExpr(
                [IntegerExpr(1), IntegerExpr(2), IntegerExpr(3)]));
    }

    [Fact]
    public void Parses_empty_list_literal()
    {
        ParseAndConvert("[]")
            .Should().Be(new Abstract.Expression.ListExpr([]));
    }

    [Fact]
    public void Parses_tuple_expression()
    {
        ParseAndConvert("( 1, 2 )")
            .Should().Be(
            new Abstract.Expression.TupledExpression(
                [IntegerExpr(1), IntegerExpr(2)]));
    }

    [Fact]
    public void Parses_operator_application()
    {
        ParseAndConvert("1 + 2")
            .Should().Be(
            new Abstract.Expression.OperatorApplication(
                "+",
                Concrete.InfixDirection.Left,
                IntegerExpr(1),
                IntegerExpr(2)));
    }

    [Fact]
    public void Parses_function_application()
    {
        ParseAndConvert("String.fromInt 42")
            .Should().Be(
            new Abstract.Expression.Application(
                Abstract.Expression.FunctionOrValue.Create(["String"], "fromInt"),
                [IntegerExpr(42)]));
    }

    [Fact]
    public void Parses_if_expression()
    {
        ParseAndConvert("if cond then 1 else 2")
            .Should().Be(
            new Abstract.Expression.IfBlock(
                Abstract.Expression.FunctionOrValue.Create([], "cond"),
                IntegerExpr(1),
                IntegerExpr(2)));
    }

    [Fact]
    public void Parses_negation_expression()
    {
        ParseAndConvert("-x")
            .Should().Be(
            new Abstract.Expression.Negation(
                Abstract.Expression.FunctionOrValue.Create([], "x")));
    }

    [Fact]
    public void Parses_record_access_function()
    {
        ParseAndConvert(".name")
            .Should().Be(
            new Abstract.Expression.RecordAccessFunction(
                "name",
                StringEncoding.ValueFromString("name")));
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
