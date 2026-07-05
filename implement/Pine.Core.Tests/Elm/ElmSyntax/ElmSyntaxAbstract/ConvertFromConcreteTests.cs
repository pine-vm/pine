using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Numerics;
using Xunit;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using Concrete = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxAbstract;

/// <summary>
/// Tests verifying conversion from the concrete syntax model (namespace <c>SyntaxModel</c>) into the
/// abstract model, including normalization of literals such as integers, floats and string escapes.
/// </summary>
public class ConvertFromConcreteTests
{
    /// <summary>
    /// Parse a single Elm expression by wrapping it in a trivial module declaration, then convert
    /// the resulting concrete expression into the abstract model.
    /// </summary>
    private static Abstract.Expression ParseAndConvertExpression(string expressionText)
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

    [Fact]
    public void Decimal_integer_literal_is_normalized_with_pine_value()
    {
        var converted = Abstract.ConvertFromConcrete.FromExpression(new Concrete.Expression.Integer("42"));

        var integer = converted.Should().BeOfType<Abstract.Expression.Integer>().Subject;

        integer.Value.Should().Be(new BigInteger(42));
        integer.ValueAsPineValue.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Hexadecimal_integer_literal_is_normalized()
    {
        var converted = Abstract.ConvertFromConcrete.FromExpression(new Concrete.Expression.Integer("0xFF"));

        var integer = converted.Should().BeOfType<Abstract.Expression.Integer>().Subject;

        integer.Value.Should().Be(new BigInteger(255));
        integer.ValueAsPineValue.Should().Be(IntegerEncoding.EncodeSignedInteger(255));
    }

    [Fact]
    public void Negative_integer_literal_is_normalized()
    {
        var converted = Abstract.ConvertFromConcrete.FromExpression(new Concrete.Expression.Integer("-123"));

        var integer = converted.Should().BeOfType<Abstract.Expression.Integer>().Subject;

        integer.Value.Should().Be(new BigInteger(-123));
        integer.ValueAsPineValue.Should().Be(IntegerEncoding.EncodeSignedInteger(-123));
    }

    [Fact]
    public void Float_literal_is_normalized_to_rational()
    {
        var converted = Abstract.ConvertFromConcrete.FromExpression(new Concrete.Expression.FloatLiteral("1.5"));

        var floatable = converted.Should().BeOfType<Abstract.Expression.FloatLiteral>().Subject;

        floatable.Numerator.Should().Be(new BigInteger(3));
        floatable.Denominator.Should().Be(new BigInteger(2));
    }

    [Fact]
    public void Float_literal_in_scientific_notation_is_normalized()
    {
        var converted = Abstract.ConvertFromConcrete.FromExpression(new Concrete.Expression.FloatLiteral("2.0e1"));

        var floatable = converted.Should().BeOfType<Abstract.Expression.FloatLiteral>().Subject;

        // 2.0e1 == 20
        ((double)floatable.Numerator / (double)floatable.Denominator).Should().Be(20.0);
    }

    [Fact]
    public void String_literal_escape_sequences_are_normalized()
    {
        // The Elm source contains the escape sequences \n, \t and \\; after conversion the abstract
        // model must hold the decoded characters.
        var converted = ParseAndConvertExpression("\"line1\\nline2\\tend\\\\done\"");

        var stringLiteral = converted.Should().BeOfType<Abstract.Expression.StringLiteral>().Subject;

        stringLiteral.Value.Should().Be("line1\nline2\tend\\done");
    }

    [Fact]
    public void String_literal_unicode_escape_is_normalized()
    {
        var converted = ParseAndConvertExpression("\"\\u{0041}BC\"");

        var stringLiteral = converted.Should().BeOfType<Abstract.Expression.StringLiteral>().Subject;

        stringLiteral.Value.Should().Be("ABC");
    }

    [Fact]
    public void String_literal_value_is_normalized_and_source_representation_is_dropped()
    {
        // The concrete model keeps a (decoded) value alongside the original source representation
        // (SourceText). ConvertFromConcrete normalizes the literal to its decoded value and drops the
        // varied source representation, precomputing the encoded Pine value.
        var concrete =
            new Concrete.Expression.Literal(
                Value: "line1\nline2",
                SourceText: "line1\\nline2");

        var converted = Abstract.ConvertFromConcrete.FromExpression(concrete);

        var stringLiteral = converted.Should().BeOfType<Abstract.Expression.StringLiteral>().Subject;

        stringLiteral.Value.Should().Be("line1\nline2");
        stringLiteral.ValueAsPineValue.Should().Be(ElmValueEncoding.StringAsPineValue("line1\nline2"));

        // Normalization collapses to the canonical Create result, regardless of source representation.
        converted.Should().Be(Abstract.Expression.StringLiteral.Create("line1\nline2"));
    }

    [Fact]
    public void String_literals_with_different_source_representations_normalize_equally()
    {
        // Two concrete literals that decode to the same value but carry different source
        // representations must normalize to the same abstract string literal.
        var fromEscape =
            Abstract.ConvertFromConcrete.FromExpression(
                new Concrete.Expression.Literal("A", SourceText: "\\u{0041}"));

        var fromLiteral =
            Abstract.ConvertFromConcrete.FromExpression(
                new Concrete.Expression.Literal("A", SourceText: "A"));

        fromEscape.Should().Be(fromLiteral);
        fromEscape.Should().Be(Abstract.Expression.StringLiteral.Create("A"));
    }

    [Fact]
    public void Triple_quoted_string_literal_value_is_normalized()
    {
        var concrete =
            new Concrete.Expression.MultilineStringLiteral(
                Value: "first\nsecond",
                SourceLines: ["first", "second"]);

        var converted = Abstract.ConvertFromConcrete.FromExpression(concrete);

        var stringLiteral = converted.Should().BeOfType<Abstract.Expression.StringLiteral>().Subject;

        stringLiteral.Value.Should().Be("first\nsecond");
        stringLiteral.ValueAsPineValue.Should().Be(ElmValueEncoding.StringAsPineValue("first\nsecond"));
    }

    [Fact]
    public void String_literal_without_source_representation_is_normalized()
    {
        // Literals produced without source text (for example synthesized during lowering) are
        // normalized from their value alone.
        var converted =
            Abstract.ConvertFromConcrete.FromExpression(new Concrete.Expression.Literal("plain"));

        converted.Should().Be(Abstract.Expression.StringLiteral.Create("plain"));
    }

    [Fact]
    public void Parenthesized_expression_is_unwrapped()
    {
        var converted = ParseAndConvertExpression("(42)");

        converted.Should().BeOfType<Abstract.Expression.Integer>();
    }

    [Fact]
    public void Record_access_holds_field_name_pine_value()
    {
        var converted = ParseAndConvertExpression("rec.field");

        var recordAccess = converted.Should().BeOfType<Abstract.Expression.RecordAccess>().Subject;

        recordAccess.FieldName.Should().Be("field");
        recordAccess.FieldNameValue.Should().Be(StringEncoding.ValueFromString("field"));
        recordAccess.Record.Should().BeOfType<Abstract.Expression.FunctionOrValue>();
    }

    [Fact]
    public void Record_access_function_strips_leading_dot_and_holds_pine_value()
    {
        var converted = ParseAndConvertExpression(".field");

        var accessFunction = converted.Should().BeOfType<Abstract.Expression.RecordAccessFunction>().Subject;

        accessFunction.FieldName.Should().Be("field");
        accessFunction.FieldNameValue.Should().Be(StringEncoding.ValueFromString("field"));
    }

    [Fact]
    public void Record_update_holds_field_name_pine_values()
    {
        var converted = ParseAndConvertExpression("{ rec | alfa = 1, beta = 2 }");

        var recordUpdate = converted.Should().BeOfType<Abstract.Expression.RecordUpdateExpression>().Subject;

        recordUpdate.RecordName.Should().Be("rec");
        recordUpdate.Fields.Should().HaveCount(2);

        recordUpdate.Fields[0].FieldName.Should().Be("alfa");
        recordUpdate.Fields[0].FieldNameValue.Should().Be(StringEncoding.ValueFromString("alfa"));
        recordUpdate.Fields[1].FieldName.Should().Be("beta");
        recordUpdate.Fields[1].FieldNameValue.Should().Be(StringEncoding.ValueFromString("beta"));
    }

    [Fact]
    public void Record_literal_holds_field_name_pine_values()
    {
        var converted = ParseAndConvertExpression("{ alfa = 1, beta = 2 }");

        var record = converted.Should().BeOfType<Abstract.Expression.RecordExpr>().Subject;

        record.Fields.Should().HaveCount(2);
        record.Fields[0].FieldName.Should().Be("alfa");
        record.Fields[0].FieldNameValue.Should().Be(StringEncoding.ValueFromString("alfa"));
    }

    [Fact]
    public void Application_is_converted()
    {
        var converted = ParseAndConvertExpression("f 1 2");

        var application = converted.Should().BeOfType<Abstract.Expression.Application>().Subject;

        application.Function.Should().BeOfType<Abstract.Expression.FunctionOrValue>();
        application.Arguments.Should().HaveCount(2);
    }

    [Fact]
    public void If_block_is_converted()
    {
        var converted = ParseAndConvertExpression("if x then 1 else 2");

        converted.Should().BeOfType<Abstract.Expression.IfBlock>();
    }

    [Fact]
    public void Lambda_is_converted_and_flattened()
    {
        var converted = ParseAndConvertExpression("\\x y -> x");

        var lambda = converted.Should().BeOfType<Abstract.Expression.LambdaExpression>().Subject;

        lambda.Arguments.Should().HaveCount(2);
        lambda.Expression.Should().BeOfType<Abstract.Expression.FunctionOrValue>();
    }

    [Fact]
    public void Case_expression_is_converted_and_normalizes_int_patterns()
    {
        var converted =
            ParseAndConvertExpression(
                "case x of\n        0 ->\n            1\n\n        _ ->\n            2\n    ");

        var caseExpression = converted.Should().BeOfType<Abstract.Expression.CaseExpression>().Subject;

        caseExpression.Cases.Should().HaveCount(2);
        caseExpression.Cases[0].Pattern.Should().BeOfType<Abstract.Pattern.IntPattern>();
        caseExpression.Cases[1].Pattern.Should().BeOfType<Abstract.Pattern.AllPattern>();
    }

    [Fact]
    public void Hex_pattern_is_normalized_to_int_pattern()
    {
        var converted =
            Abstract.ConvertFromConcrete.FromPattern(new Concrete.Pattern.HexPattern(0xFF));

        var intPattern = converted.Should().BeOfType<Abstract.Pattern.IntPattern>().Subject;

        intPattern.Value.Should().Be(new BigInteger(255));
        intPattern.ValueAsPineValue.Should().Be(IntegerEncoding.EncodeSignedInteger(255));
    }

    [Fact]
    public void Int_pattern_holds_precomputed_pine_value()
    {
        var converted =
            Abstract.ConvertFromConcrete.FromPattern(new Concrete.Pattern.IntPattern(42));

        var intPattern = converted.Should().BeOfType<Abstract.Pattern.IntPattern>().Subject;

        intPattern.Value.Should().Be(new BigInteger(42));
        intPattern.ValueAsPineValue.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Char_pattern_holds_precomputed_pine_value()
    {
        var converted =
            Abstract.ConvertFromConcrete.FromPattern(new Concrete.Pattern.CharPattern('a'));

        var charPattern = converted.Should().BeOfType<Abstract.Pattern.CharPattern>().Subject;

        charPattern.Value.Should().Be('a');
        charPattern.ValueAsPineValue.Should().Be(ElmValueEncoding.ElmCharAsPineValue('a'));
    }

    [Fact]
    public void String_pattern_holds_precomputed_pine_value()
    {
        var converted =
            Abstract.ConvertFromConcrete.FromPattern(new Concrete.Pattern.StringPattern("hello"));

        var stringPattern = converted.Should().BeOfType<Abstract.Pattern.StringPattern>().Subject;

        stringPattern.Value.Should().Be("hello");
        stringPattern.ValueAsPineValue.Should().Be(ElmValueEncoding.StringAsPineValue("hello"));
    }

    [Fact]
    public void Named_pattern_holds_precomputed_tag_name_pine_value()
    {
        var converted =
            Abstract.ConvertFromConcrete.FromPattern(
                new Concrete.Pattern.NamedPattern(
                    new Concrete.QualifiedNameRef(["Maybe"], "Just"),
                    [
                        new Concrete.Node<Concrete.Pattern>(
                            new Concrete.Range(new Concrete.Location(0, 0), new Concrete.Location(0, 0)),
                            new Concrete.Pattern.VarPattern("x")),
                    ]));

        var namedPattern = converted.Should().BeOfType<Abstract.Pattern.NamedPattern>().Subject;

        namedPattern.Name.Name.Should().Be("Just");
        namedPattern.TagNameAsPineValue.Should().Be(StringEncoding.ValueFromString("Just"));
    }

    [Fact]
    public void Parenthesized_pattern_is_unwrapped()
    {
        var converted =
            Abstract.ConvertFromConcrete.FromPattern(
                new Concrete.Pattern.ParenthesizedPattern(
                    new Concrete.Node<Concrete.Pattern>(
                        new Concrete.Range(new Concrete.Location(0, 0), new Concrete.Location(0, 0)),
                        new Concrete.Pattern.VarPattern("x"))));

        converted.Should().BeOfType<Abstract.Pattern.VarPattern>();
    }

    [Fact]
    public void Let_expression_is_converted()
    {
        var converted =
            ParseAndConvertExpression(
                "let\n        a =\n            1\n    in\n    a\n    ");

        var letExpression = converted.Should().BeOfType<Abstract.Expression.LetExpression>().Subject;

        letExpression.Declarations.Should().HaveCount(1);
        letExpression.Expression.Should().BeOfType<Abstract.Expression.FunctionOrValue>();
    }

    [Fact]
    public void Full_module_is_converted()
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

        var normalModule =
            abstractFile.ModuleDefinition.Should().BeOfType<Abstract.Module.NormalModule>().Subject;

        normalModule.ModuleData.ModuleName.Should().Equal("Main");

        abstractFile.Imports.Should().HaveCount(1);
        abstractFile.Imports[0].ModuleName.Should().Equal("Html");

        abstractFile.Declarations.Should().HaveCount(1);

        var functionDeclaration =
            abstractFile.Declarations[0].Should().BeOfType<Abstract.Declaration.FunctionDeclaration>().Subject;

        functionDeclaration.Function.Declaration.Name.Should().Be("main");
    }

    [Fact]
    public void Conversion_is_deterministic_and_value_equal()
    {
        var first = ParseAndConvertExpression("{ rec | field = f 1 }");
        var second = ParseAndConvertExpression("{ rec | field = f 1 }");

        first.Should().Be(second);
        first.GetHashCode().Should().Be(second.GetHashCode());
    }
}
