using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using Pine.Core.Internal;
using System.Collections.Generic;
using System.Numerics;
using Xunit;

using AbstractExpr = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Expression;
using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;
using InfixDirection = Pine.Core.Elm.ElmSyntax.SyntaxModel.InfixDirection;
using PreparedExpr = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter.PreparedExpression;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

public class PreparedExpressionRenderingTests
{
    private static AbstractExpr Identifier(string name) => AbstractExpr.Identifier.Create([], name);

    private static AbstractExpr Integer(int value) =>
        new AbstractExpr.IntegerLiteral(value, IntegerEncoding.EncodeSignedInteger(value));

    private static RecordSetter Field(string name, AbstractExpr value) =>
        new(name, StringEncoding.ValueFromString(name), value);

    private static string Render(AbstractExpr expression) =>
        PreparedExpressionRendering.Render(ElmInterpreter.PrepareExpression(expression));

    public static IEnumerable<object[]> LiteralCases()
    {
        yield return [AbstractExpr.UnitExpr.Instance, "[]"];
        yield return [AbstractExpr.StringLiteral.Create(""), "\"\""];
        yield return [AbstractExpr.StringLiteral.Create("hello"), "\"hello\""];
        yield return [AbstractExpr.StringLiteral.Create("quote \" slash \\ newline\n"), "\"quote \\\" slash \\\\ newline\\n\""];
        yield return [AbstractExpr.StringLiteral.Create("carriage\r tab\t control\u0001"), "\"carriage\\u{000D} tab\\t control\\u{0001}\""];
        yield return [AbstractExpr.StringLiteral.Create("😀"), "\"😀\""];
        yield return [AbstractExpr.CharLiteral.Create('a'), "'a'"];
        yield return [AbstractExpr.CharLiteral.Create('\''), "'\\''"];
        yield return [AbstractExpr.CharLiteral.Create('\\'), "'\\\\'"];
        yield return [AbstractExpr.CharLiteral.Create('\n'), "'\\n'"];
        yield return [AbstractExpr.CharLiteral.Create('\r'), "'\\u{000D}'"];
        yield return [AbstractExpr.CharLiteral.Create('\t'), "'\\t'"];
        yield return [AbstractExpr.CharLiteral.Create('\u0001'), "'\\u{0001}'"];
        yield return [AbstractExpr.CharLiteral.Create(0x1F600), "'😀'"];
        yield return [Integer(0), "0"];
        yield return [Integer(42), "42"];
        yield return [Integer(-42), "-42"];
        var large = BigInteger.Parse("123456789012345678901234567890");
        yield return [new AbstractExpr.IntegerLiteral(large, IntegerEncoding.EncodeSignedInteger(large)), large.ToString()];
        yield return [new AbstractExpr.FloatLiteral(314, 100), "3.14"];
        yield return [new AbstractExpr.FloatLiteral(-3, 2), "-1.5"];
        yield return [new AbstractExpr.FloatLiteral(1, 1), "1.0"];

        yield return [
            new AbstractExpr.FloatLiteral(
                BigInteger.Parse("123456789012345678901234567890123456789"), BigInteger.Pow(10, 35)),
            "1234.56789012345678901234567890123456789"];
    }

    [Theory]
    [MemberData(nameof(LiteralCases))]
    public void Prepared_literal_values_render_as_Elm_expressions(AbstractExpr literal, string expected) =>
        Render(literal).Should().Be(expected);

    public static IEnumerable<object[]> ExpressionCases()
    {
        yield return [Identifier("x"), "x"];
        yield return [AbstractExpr.Identifier.Create(["List", "Extra"], "map"), "List.Extra.map"];
        yield return [new AbstractExpr.Negation(Integer(7)), "-7"];
        yield return [new AbstractExpr.ListExpr([]), "[]"];
        yield return [new AbstractExpr.ListExpr([Integer(1), AbstractExpr.StringLiteral.Create("a")]), "[1, \"a\"]"];
        yield return [new AbstractExpr.TupledExpression([Integer(1), Identifier("x")]), "(1, x)"];
        yield return [new AbstractExpr.IfBlock(Identifier("flag"), Integer(1), Integer(2)), "if flag then 1 else 2"];
        yield return [new AbstractExpr.PrefixOperator("+"), "(+)"];
        yield return [new AbstractExpr.Application(Identifier("mapItem"), []), "mapItem"];
        yield return [new AbstractExpr.Application(Identifier("f"), [Integer(1), Identifier("x")]), "f 1 x"];
        yield return [new AbstractExpr.Application(Identifier("f"), [Integer(-7)]), "f (-7)"];

        yield return [
            new AbstractExpr.Application(Identifier("f"),
                [new AbstractExpr.Application(Identifier("g"), [Integer(1)]),
                 new AbstractExpr.IfBlock(Identifier("c"), Integer(2), Integer(3))]),
            "f (g 1) (if c then 2 else 3)"];

        yield return [
            new AbstractExpr.OperatorApplication("+", InfixDirection.Left,
                new AbstractExpr.Application(Identifier("f"), [Integer(1)]), Integer(2)),
            "(f 1) + 2"];

        yield return [
            new AbstractExpr.LambdaExpression([new Pattern.VarPattern("x")], Identifier("x")),
            "\\x -> x"];

        yield return [
            new AbstractExpr.RecordExpr([Field("a", Integer(1)), Field("b", Identifier("x"))]),
            "{a = 1, b = x}"];

        yield return [
            new AbstractExpr.RecordAccess(Identifier("record"), "field", StringEncoding.ValueFromString("field")),
            "record.field"];

        yield return [
            new AbstractExpr.RecordAccessFunction("field", StringEncoding.ValueFromString("field")),
            ".field"];

        yield return [
            new AbstractExpr.RecordUpdateExpression("record", [Field("a", Integer(2))]),
            "{record | a = 2}"];

        yield return [new AbstractExpr.GLSLExpression("void main() {}"), "[glsl|void main() {}|]"];
    }

    [Theory]
    [MemberData(nameof(ExpressionCases))]
    public void Prepared_expression_variants_render_with_structure(AbstractExpr expression, string expected) =>
        Render(expression).Should().Be(expected);

    [Fact]
    public void Case_dispatch_preserves_all_arms_and_patterns()
    {
        var expression =
            new AbstractExpr.CaseExpression(
                Identifier("item"),
                [
                    new Case(new Pattern.IntPattern(1), AbstractExpr.StringLiteral.Create("one")),
                    new Case(new Pattern.IntPattern(2), AbstractExpr.StringLiteral.Create("two")),
                    new Case(
                        new Pattern.NamedPattern(new QualifiedNameRef([], "Just"), [new Pattern.VarPattern("x")]),
                        Identifier("x")),
                    new Case(new Pattern.AllPattern(), AbstractExpr.StringLiteral.Create("other"))
                ]);

        Render(expression).Should().Be(
            "case item of\n    1 -> \"one\"\n    2 -> \"two\"\n    Just x -> x\n    _ -> \"other\"");
    }

    [Fact]
    public void Nested_case_arms_are_indented_under_their_parent_arm()
    {
        var expression =
            new AbstractExpr.CaseExpression(
                Identifier("maybe"),
                [
                    new Case(
                        new Pattern.NamedPattern(new QualifiedNameRef([], "Just"), [new Pattern.VarPattern("x")]),
                        new AbstractExpr.CaseExpression(
                            Identifier("x"),
                            [
                                new Case(new Pattern.IntPattern(0), AbstractExpr.StringLiteral.Create("zero")),
                                new Case(new Pattern.AllPattern(), AbstractExpr.StringLiteral.Create("other"))
                            ])),
                    new Case(
                        new Pattern.NamedPattern(new QualifiedNameRef([], "Nothing"), []),
                        AbstractExpr.StringLiteral.Create("none"))
                ]);

        Render(expression).Should().Be(
            "case maybe of\n    Just x -> case x of\n        0 -> \"zero\"\n        _ -> \"other\"\n    Nothing -> \"none\"");
    }

    [Fact]
    public void If_with_multiline_branch_keeps_else_separate_from_case_arms()
    {
        var expression =
            new AbstractExpr.IfBlock(
                Identifier("flag"),
                new AbstractExpr.CaseExpression(
                    Identifier("x"),
                    [
                        new Case(new Pattern.IntPattern(0), Integer(1)),
                        new Case(new Pattern.AllPattern(), Integer(2))
                    ]),
                Integer(3));

        Render(expression).Should().Be(
            "if flag then case x of\n        0 -> 1\n        _ -> 2\nelse 3");
    }

    [Fact]
    public void Let_and_lambda_render_nested_declarations_and_destructuring()
    {
        var expression =
            new AbstractExpr.LetExpression(
                [
                    new LetDeclaration.LetFunction(
                        new FunctionStruct(
                            null,
                            new FunctionImplementation(
                                "increment",
                                [new Pattern.VarPattern("x")],
                                new AbstractExpr.OperatorApplication(
                                    "+",
                                    InfixDirection.Left,
                                    Identifier("x"),
                                    Integer(1))))),
                    new LetDeclaration.LetDestructuring(
                        new Pattern.TuplePattern([new Pattern.VarPattern("a"), new Pattern.VarPattern("b")]),
                        new AbstractExpr.TupledExpression([Integer(1), Integer(2)]))
                ],
                new AbstractExpr.Application(Identifier("increment"), [Identifier("a")]));

        Render(expression).Should().Be("let\n    increment x = x + 1\n    (a, b) = (1, 2)\nin\n    increment a");
    }

    [Fact]
    public void Literal_override_is_applied_recursively_without_affecting_other_nodes()
    {
        var expression =
            ElmInterpreter.PrepareExpression(
                new AbstractExpr.Application(
                    Identifier("f"),
                    [new AbstractExpr.ListExpr([Integer(1), Integer(2)])]));

        var values = new List<BigInteger>();

        PreparedExpressionRendering.Render(
            expression,
            value =>
            {
                values.Add(value.IntegerOrNull!.Value);
                return "literal-" + value.IntegerOrNull.Value;
            }).Should().Be("f [literal-1, literal-2]");

        values.Should().Equal(1, 2);
        PreparedExpressionRendering.Render(expression).Should().Be("f [1, 2]");
    }

    [Fact]
    public void Unit_and_empty_list_values_render_as_empty_list()
    {
        PreparedExpressionRendering.Render(new PreparedExpr.ValueLiteral(PineValueInProcess.EmptyList))
            .Should().Be("[]");

        Render(AbstractExpr.UnitExpr.Instance).Should().Be("[]");
    }

    [Fact]
    public void Folded_empty_list_and_unit_render_as_empty_list_after_serialization()
    {
        const string moduleText =
            """
            module Test exposing (..)


            empty =
                []


            unit =
                ()
            """;

        var prepared =
            ElmInterpreter.PrepareModules([moduleText])
            .Extract(err => throw new System.Exception(err));

        static PreparedExpr ExpressionFor(
            ElmInterpreter.Prepared prepared, string name) =>
            ((ElmInterpreter.PreparedDeclaration.FunctionDeclaration)
            prepared.Declarations[Pine.Core.CodeAnalysis.DeclQualifiedName.Create(["Test"], name)])
            .Function.Declaration.Expression;

        PreparedExpressionRendering.Render(ExpressionFor(prepared, "empty")).Should().Be("[]");
        PreparedExpressionRendering.Render(ExpressionFor(prepared, "unit")).Should().Be("[]");

        var back =
            ElmSyntaxInterpreterPreparedJson.FromJsonString(
                ElmSyntaxInterpreterPreparedJson.ToJsonString(prepared));

        PreparedExpressionRendering.Render(ExpressionFor(back, "empty")).Should().Be("[]");
        PreparedExpressionRendering.Render(ExpressionFor(back, "unit")).Should().Be("[]");
    }

    [Fact]
    public void Folded_constructor_literal_keeps_parentheses_as_application_argument()
    {
        var constructor =
            new PreparedExpr.ValueLiteral(
                PineValueInProcess.CreateFullyRepresented(
                    ElmValueEncoding.TagAsPineValue("Just", [IntegerEncoding.EncodeSignedInteger(1)])));

        var expression =
            new PreparedExpr.Application(
                new PreparedExpr.Identifier(Pine.Core.CodeAnalysis.DeclQualifiedName.Create([], "f")),
                [constructor]);

        PreparedExpressionRendering.Render(expression).Should().Be("f (Just 1)");
    }

    [Fact]
    public void Prepared_patterns_render_all_variants()
    {
        var patterns =
            new Pattern[]
            {
                new Pattern.UnitPattern(),
                new Pattern.CharPattern('c'),
                new Pattern.StringPattern("s"),
                new Pattern.FloatPattern(1.5),
                new Pattern.FloatPattern(1),
                new Pattern.RecordPattern(
                    System.Collections.Immutable.ImmutableArray.Create(
                        (FieldName: "a", FieldNameValue: StringEncoding.ValueFromString("a")))),
                new Pattern.UnConsPattern(new Pattern.VarPattern("head"), new Pattern.VarPattern("tail")),
                new Pattern.ListPattern([new Pattern.VarPattern("x")]),
                new Pattern.AsPattern(new Pattern.VarPattern("x"), "whole")
            };

        var expression = new AbstractExpr.LambdaExpression(patterns, Identifier("whole"));

        Render(expression).Should().Be(
            "\\() 'c' \"s\" 1.5 1.0 {a} (head :: tail) [x] (x as whole) -> whole");
    }
}
