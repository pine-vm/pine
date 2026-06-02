using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Numerics;
using Xunit;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using Concrete = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxAbstractTests;

/// <summary>
/// Tests verifying conversion from the abstract model back into the concrete syntax model (namespace
/// <c>SyntaxModel</c>) via <see cref="Abstract.ConvertToConcrete"/>.
/// <para>
/// The primary correctness property exercised here is a roundtrip: converting an abstract value to the
/// concrete model and back (<see cref="Abstract.ConvertFromConcrete"/>) must reproduce the original abstract
/// value. Because the abstract model is the normalized, location-free form, this roundtrip is an equality
/// (unlike a concrete-to-abstract-to-concrete roundtrip, which would lose locations and trivia).
/// </para>
/// </summary>
public class ConvertToConcreteTests
{
    /// <summary>
    /// Parse a single Elm expression by wrapping it in a trivial module declaration, then convert
    /// the resulting concrete expression into the abstract model.
    /// </summary>
    private static Abstract.Expression ParseExpression(string expressionText)
    {
        var moduleText =
            "module Test exposing (..)\n\n\nvalue =\n    " + expressionText + "\n";

        var file =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Parse failed: " + err));

        var functionDeclaration =
            file.Declarations[0].Value as Concrete.Declaration.FunctionDeclaration;

        functionDeclaration.Should().NotBeNull();

        var concreteExpression =
            functionDeclaration!.Function.Declaration.Value.Expression.Value;

        return Abstract.ConvertFromConcrete.FromExpression(concreteExpression);
    }

    /// <summary>
    /// Assert that an abstract expression survives a roundtrip through the concrete model: converting it to
    /// concrete and back must reproduce a value-equal abstract expression.
    /// </summary>
    private static void AssertExpressionRoundTrips(Abstract.Expression abstractExpression)
    {
        var concrete = Abstract.ConvertToConcrete.ToExpression(abstractExpression);

        var roundTripped = Abstract.ConvertFromConcrete.FromExpression(concrete);

        roundTripped.Should().Be(abstractExpression);
        roundTripped.GetHashCode().Should().Be(abstractExpression.GetHashCode());
    }

    [Theory]
    [InlineData("()")]
    [InlineData("42")]
    [InlineData("-7")]
    [InlineData("0xFF")]
    [InlineData("1.5")]
    [InlineData("2.0e1")]
    [InlineData("-3.25")]
    [InlineData("'a'")]
    [InlineData("'\\n'")]
    [InlineData("\"hello\"")]
    [InlineData("\"line1\\nline2\\tend\\\\done\"")]
    [InlineData("\"\\u{0041}BC\"")]
    [InlineData("x")]
    [InlineData("Basics.identity")]
    [InlineData("Just")]
    [InlineData("[]")]
    [InlineData("[ 1, 2, 3 ]")]
    [InlineData("( 1, 2 )")]
    [InlineData("( 1, 2, 3 )")]
    [InlineData("f 1 2")]
    [InlineData("f x (g y)")]
    [InlineData("a + b")]
    [InlineData("a + b * c")]
    [InlineData("a |> f |> g")]
    [InlineData("-x")]
    [InlineData("if x then 1 else 2")]
    [InlineData("\\x y -> x")]
    [InlineData(".field")]
    [InlineData("rec.field")]
    [InlineData("(+)")]
    [InlineData("{ alfa = 1, beta = 2 }")]
    [InlineData("{ rec | alfa = 1, beta = 2 }")]
    public void Expression_round_trips_through_concrete_model(string expressionText)
    {
        var abstractExpression = ParseExpression(expressionText);

        AssertExpressionRoundTrips(abstractExpression);
    }

    [Fact]
    public void Case_expression_round_trips_through_concrete_model()
    {
        var abstractExpression =
            ParseExpression(
                "case x of\n" +
                "        0 ->\n" +
                "            1\n\n" +
                "        'a' ->\n" +
                "            2\n\n" +
                "        \"s\" ->\n" +
                "            3\n\n" +
                "        Just y ->\n" +
                "            y\n\n" +
                "        _ ->\n" +
                "            4\n    ");

        AssertExpressionRoundTrips(abstractExpression);
    }

    [Fact]
    public void Let_expression_round_trips_through_concrete_model()
    {
        var abstractExpression =
            ParseExpression(
                "let\n" +
                "        a =\n" +
                "            1\n\n" +
                "        ( b, c ) =\n" +
                "            ( 2, 3 )\n" +
                "    in\n" +
                "    a\n    ");

        AssertExpressionRoundTrips(abstractExpression);
    }

    [Fact]
    public void Nested_pattern_in_case_round_trips_through_concrete_model()
    {
        var abstractExpression =
            ParseExpression(
                "case x of\n" +
                "        ( Just a, [ b, c ] ) ->\n" +
                "            a\n\n" +
                "        (first :: rest) as whole ->\n" +
                "            first\n\n" +
                "        { name } ->\n" +
                "            name\n\n" +
                "        _ ->\n" +
                "            0\n    ");

        AssertExpressionRoundTrips(abstractExpression);
    }

    [Fact]
    public void Decimal_integer_round_trips_with_literal_text()
    {
        var concrete =
            Abstract.ConvertToConcrete.ToExpression(
                Abstract.ConvertFromConcrete.FromExpression(new Concrete.Expression.Integer("42")));

        var integer = concrete.Should().BeOfType<Concrete.Expression.Integer>().Subject;

        integer.LiteralText.Should().Be("42");
    }

    [Fact]
    public void Hexadecimal_integer_is_serialized_as_decimal_text()
    {
        // The abstract model normalizes away the hex/decimal distinction, so the concrete value is decimal.
        var concrete =
            Abstract.ConvertToConcrete.ToExpression(
                Abstract.ConvertFromConcrete.FromExpression(new Concrete.Expression.Integer("0xFF")));

        var integer = concrete.Should().BeOfType<Concrete.Expression.Integer>().Subject;

        integer.LiteralText.Should().Be("255");
    }

    [Fact]
    public void Negative_integer_round_trips_with_literal_text()
    {
        var concrete =
            Abstract.ConvertToConcrete.ToExpression(
                Abstract.ConvertFromConcrete.FromExpression(new Concrete.Expression.Integer("-123")));

        var integer = concrete.Should().BeOfType<Concrete.Expression.Integer>().Subject;

        integer.LiteralText.Should().Be("-123");
    }

    [Theory]
    [InlineData("1.5")]
    [InlineData("0.25")]
    [InlineData("-3.5")]
    [InlineData("2.0e1")]
    [InlineData("1.5e-2")]
    public void Float_literal_text_round_trips_to_same_rational(string literalText)
    {
        var expected = FloatLiteralConversion.ToElmFloat(literalText);

        var serialized = Abstract.ConvertToConcrete.FloatLiteralText(expected.Numerator, expected.Denominator);

        var reparsed = FloatLiteralConversion.ToElmFloat(serialized);

        reparsed.Numerator.Should().Be(expected.Numerator);
        reparsed.Denominator.Should().Be(expected.Denominator);
    }

    [Fact]
    public void Float_literal_text_of_whole_number_includes_decimal_point()
    {
        var text = Abstract.ConvertToConcrete.FloatLiteralText(new BigInteger(20), BigInteger.One);

        text.Should().Be("20.0");
    }

    [Fact]
    public void Record_access_function_re_adds_leading_dot()
    {
        var abstractExpression =
            new Abstract.Expression.RecordAccessFunction(
                "field",
                PopularEncodings.StringEncoding.ValueFromString("field"));

        var concrete = Abstract.ConvertToConcrete.ToExpression(abstractExpression);

        var accessFunction = concrete.Should().BeOfType<Concrete.Expression.RecordAccessFunction>().Subject;

        accessFunction.FunctionName.Should().Be(".field");
    }

    [Fact]
    public void Named_pattern_drops_pine_value_and_keeps_structure()
    {
        var abstractPattern =
            new Abstract.Pattern.NamedPattern(
                new Abstract.QualifiedNameRef(["Maybe"], "Just"),
                [new Abstract.Pattern.VarPattern("x")]);

        var concrete = Abstract.ConvertToConcrete.ToPattern(abstractPattern);

        var namedPattern = concrete.Should().BeOfType<Concrete.Pattern.NamedPattern>().Subject;

        namedPattern.Name.ModuleName.Should().Equal("Maybe");
        namedPattern.Name.Name.Should().Be("Just");
        namedPattern.Arguments.Should().HaveCount(1);
        namedPattern.Arguments[0].Value.Should().BeOfType<Concrete.Pattern.VarPattern>();
    }

    [Fact]
    public void Pattern_round_trips_through_concrete_model()
    {
        var abstractPattern =
            new Abstract.Pattern.TuplePattern(
                [
                    new Abstract.Pattern.IntPattern(3),
                    new Abstract.Pattern.CharPattern('z'),
                    new Abstract.Pattern.StringPattern("s"),
                    new Abstract.Pattern.NamedPattern(
                        new Abstract.QualifiedNameRef([], "Leaf"),
                        []),
                ]);

        var concrete = Abstract.ConvertToConcrete.ToPattern(abstractPattern);

        var roundTripped = Abstract.ConvertFromConcrete.FromPattern(concrete);

        roundTripped.Should().Be(abstractPattern);
    }

    [Fact]
    public void Full_module_round_trips_through_concrete_model()
    {
        var moduleText =
            string.Join(
                "\n",
                "module Main exposing (main)",
                "",
                "import Html exposing (text)",
                "",
                "",
                "main =",
                "    text \"hello\"",
                "");

        var file =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Parse failed: " + err));

        var abstractFile = Abstract.ConvertFromConcrete.FromFile(file);

        var concreteFile = Abstract.ConvertToConcrete.FromFile(abstractFile);

        var roundTripped = Abstract.ConvertFromConcrete.FromFile(concreteFile);

        roundTripped.Should().Be(abstractFile);
    }

    [Fact]
    public void Module_with_choice_type_and_alias_round_trips()
    {
        var moduleText =
            string.Join(
                "\n",
                "module Data exposing (..)",
                "",
                "",
                "type Tree a",
                "    = Leaf",
                "    | Node a (Tree a) (Tree a)",
                "",
                "",
                "type alias Point =",
                "    { x : Int, y : Int }",
                "",
                "",
                "origin : Point",
                "origin =",
                "    { x = 0, y = 0 }",
                "");

        var file =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Parse failed: " + err));

        var abstractFile = Abstract.ConvertFromConcrete.FromFile(file);

        var concreteFile = Abstract.ConvertToConcrete.FromFile(abstractFile);

        var roundTripped = Abstract.ConvertFromConcrete.FromFile(concreteFile);

        roundTripped.Should().Be(abstractFile);
    }

    [Fact]
    public void Converted_concrete_file_renders_to_valid_reparsable_elm()
    {
        var moduleText =
            string.Join(
                "\n",
                "module Main exposing (main)",
                "",
                "",
                "main =",
                "    case x of",
                "        Just y ->",
                "            y + 1",
                "",
                "        Nothing ->",
                "            0",
                "");

        var file =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Parse failed: " + err));

        var abstractFile = Abstract.ConvertFromConcrete.FromFile(file);

        var concreteFile = Abstract.ConvertToConcrete.FromFile(abstractFile);

        // The concrete file produced from the abstract model carries placeholder ranges, but must still
        // format to valid Elm that re-parses to a value-equal abstract model.
        var formatted = SnapshotTestFormat.Format(concreteFile);

        var rendered = Rendering.ToString(formatted);

        var reparsed =
            ElmSyntaxParser.ParseModuleText(rendered)
            .Extract(err => throw new Exception("Reparse failed: " + err + "\n\n" + rendered));

        var reparsedAbstract = Abstract.ConvertFromConcrete.FromFile(reparsed);

        reparsedAbstract.Should().Be(abstractFile);
    }

    [Fact]
    public void Conversion_is_deterministic_and_value_equal()
    {
        var abstractExpression = ParseExpression("{ rec | field = f 1 }");

        var first = Abstract.ConvertToConcrete.ToExpression(abstractExpression);
        var second = Abstract.ConvertToConcrete.ToExpression(abstractExpression);

        first.Should().Be(second);
        first.GetHashCode().Should().Be(second.GetHashCode());
    }
}
