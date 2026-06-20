using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Numerics;
using Xunit;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using Concrete = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmSyntax;

/// <summary>
/// Tests for <see cref="ElmSyntaxParser.ParseDeclarationOrExpression(string)"/>, which parses input
/// as either a top-level declaration or a standalone expression.
/// <para>
/// Each test parses the input, converts the resulting concrete syntax node (declaration or
/// expression) into the abstract syntax model, and asserts equivalency on the complete abstract
/// syntax node.
/// </para>
/// </summary>
public class ElmSyntaxParserDeclarationOrExpressionTests
{
    /// <summary>
    /// Parses <paramref name="text"/> with
    /// <see cref="ElmSyntaxParser.ParseDeclarationOrExpression(string)"/>, asserts the OK branch is a
    /// declaration, and converts it into the abstract syntax model.
    /// </summary>
    private static Abstract.Declaration ParseDeclarationAndConvert(string text)
    {
        var parsed =
            ElmSyntaxParser.ParseDeclarationOrExpression(text)
            .Extract(err => throw new Exception("Parse failed: " + err));

        var declaration =
            parsed.Should().BeOfType<ElmSyntaxParser.DeclarationOrExpression.DeclarationSyntax>().Subject;

        return Abstract.ConvertFromConcrete.FromDeclaration(declaration.Declaration);
    }

    /// <summary>
    /// Parses <paramref name="text"/> with
    /// <see cref="ElmSyntaxParser.ParseDeclarationOrExpression(string)"/>, asserts the OK branch is an
    /// expression, and converts it into the abstract syntax model.
    /// </summary>
    private static Abstract.Expression ParseExpressionAndConvert(string text)
    {
        var parsed =
            ElmSyntaxParser.ParseDeclarationOrExpression(text)
            .Extract(err => throw new Exception("Parse failed: " + err));

        var expression =
            parsed.Should().BeOfType<ElmSyntaxParser.DeclarationOrExpression.ExpressionSyntax>().Subject;

        return Abstract.ConvertFromConcrete.FromExpression(expression.Expression);
    }

    /// <summary>
    /// Parses <paramref name="declarationText"/> as the sole declaration of a synthetic module and
    /// converts it into the abstract syntax model. Used as a reference oracle to confirm the
    /// declaration-or-expression parser yields the same abstract tree as the full module parser.
    /// </summary>
    private static Abstract.Declaration ReferenceDeclaration(string declarationText)
    {
        var moduleText =
            "module Test exposing (..)\n\n\n" + declarationText + "\n";

        var file =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Module parse failed: " + err));

        return Abstract.ConvertFromConcrete.FromDeclaration(file.Declarations[0].Value);
    }

    private static Abstract.Expression IntegerExpr(long value) =>
        new Abstract.Expression.Integer(
            new BigInteger(value),
            IntegerEncoding.EncodeSignedInteger(value));

    [Fact]
    public void Parses_simple_value_declaration()
    {
        ParseDeclarationAndConvert("x = 1")
            .Should().Be(
            new Abstract.Declaration.FunctionDeclaration(
                new Abstract.FunctionStruct(
                    Signature: null,
                    Declaration: new Abstract.FunctionImplementation(
                        "x",
                        [],
                        IntegerExpr(1)))));
    }

    [Fact]
    public void Parses_function_declaration_with_arguments()
    {
        ParseDeclarationAndConvert("add a b = a + b")
            .Should().Be(
            new Abstract.Declaration.FunctionDeclaration(
                new Abstract.FunctionStruct(
                    Signature: null,
                    Declaration: new Abstract.FunctionImplementation(
                        "add",
                        [
                            new Abstract.Pattern.VarPattern("a"),
                            new Abstract.Pattern.VarPattern("b"),
                        ],
                        new Abstract.Expression.OperatorApplication(
                            "+",
                            Concrete.InfixDirection.Left,
                            Abstract.Expression.FunctionOrValue.Create([], "a"),
                            Abstract.Expression.FunctionOrValue.Create([], "b"))))));
    }

    [Fact]
    public void Parses_infix_declaration()
    {
        ParseDeclarationAndConvert("infix right 5 (++) = append")
            .Should().Be(
            new Abstract.Declaration.InfixDeclaration(
                new Abstract.Infix(
                    Concrete.InfixDirection.Right,
                    5,
                    "++",
                    "append")));
    }

    [Theory]
    // Value and function declarations
    [InlineData("x = 1")]
    [InlineData("greeting = \"hello\"")]
    [InlineData("add a b = a + b")]
    [InlineData("identity x = x")]
    // Declarations with a preceding type signature
    [InlineData("answer : Int\nanswer = 42")]
    [InlineData("toLower : String -> String\ntoLower s = s")]
    // Type declarations
    [InlineData("type Bool = True | False")]
    [InlineData("type Maybe a = Just a | Nothing")]
    // Type alias declarations
    [InlineData("type alias Name = String")]
    [InlineData("type alias Point = { x : Int, y : Int }")]
    // Port declarations
    [InlineData("port send : String -> Cmd msg")]
    [InlineData("port receive : (String -> msg) -> Sub msg")]
    // Infix declarations
    [InlineData("infix right 5 (++) = append")]
    [InlineData("infix left 6 (+) = add")]
    [InlineData("infix non 4 (==) = eq")]
    public void Declaration_parse_matches_module_reference(string declarationText)
    {
        var parsed = ParseDeclarationAndConvert(declarationText);

        var reference = ReferenceDeclaration(declarationText);

        parsed.Should().Be(reference);
    }

    [Fact]
    public void Parses_integer_expression()
    {
        ParseExpressionAndConvert("42")
            .Should().Be(IntegerExpr(42));
    }

    [Fact]
    public void Parses_operator_application_expression()
    {
        ParseExpressionAndConvert("1 + 2")
            .Should().Be(
            new Abstract.Expression.OperatorApplication(
                "+",
                Concrete.InfixDirection.Left,
                IntegerExpr(1),
                IntegerExpr(2)));
    }

    [Theory]
    [InlineData("42")]
    [InlineData("\"a string\"")]
    [InlineData("'c'")]
    [InlineData("()")]
    [InlineData("foo")]
    [InlineData("String.fromInt")]
    [InlineData("[1, 2, 3]")]
    [InlineData("( 1, 2 )")]
    [InlineData("1 + 2 * 3")]
    [InlineData("f x y")]
    [InlineData("a |> b")]
    [InlineData("{ a = 1 }")]
    [InlineData("if a then b else c")]
    [InlineData("\\x -> x")]
    [InlineData("case x of\n    A -> 1\n    B -> 2")]
    [InlineData("let\n    y = 1\nin\ny")]
    public void Expression_parse_matches_direct_expression_parser(string expressionText)
    {
        var viaDeclarationOrExpression = ParseExpressionAndConvert(expressionText);

        var viaParseExpression =
            Abstract.ConvertFromConcrete.FromExpression(
                ElmSyntaxParser.ParseExpression(expressionText)
                .Extract(err => throw new Exception("ParseExpression failed: " + err)));

        viaDeclarationOrExpression.Should().Be(viaParseExpression);
    }

    [Fact]
    public void Empty_input_returns_error()
    {
        var result = ElmSyntaxParser.ParseDeclarationOrExpression("   ");

        result.IsErrOrNull().Should().NotBeNull();
    }

    [Fact]
    public void Bare_identifier_is_parsed_as_expression_not_declaration()
    {
        var parsed =
            ElmSyntaxParser.ParseDeclarationOrExpression("foo")
            .Extract(err => throw new Exception("Parse failed: " + err));

        parsed.Should().BeOfType<ElmSyntaxParser.DeclarationOrExpression.ExpressionSyntax>();
    }

    [Fact]
    public void Function_application_is_parsed_as_expression_not_declaration()
    {
        var parsed =
            ElmSyntaxParser.ParseDeclarationOrExpression("foo bar baz")
            .Extract(err => throw new Exception("Parse failed: " + err));

        parsed.Should().BeOfType<ElmSyntaxParser.DeclarationOrExpression.ExpressionSyntax>();
    }
}
