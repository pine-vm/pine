using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.CommonEncodings;

public class ExpressionEncoding2026Tests
{
    private static readonly Expression s_literal =
        Expression.LitralInst(StringEncoding.ValueFromString("literal"));

    [Fact]
    public void EncodeExpressionAsValue_uses_the_2026_shapes_and_labels()
    {
        Expression input = Expression.LitralInst(StringEncoding.ValueFromString("input"));
        Expression condition = Expression.LitralInst(StringEncoding.ValueFromString("condition"));
        Expression falseBranch = Expression.LitralInst(StringEncoding.ValueFromString("false"));
        Expression trueBranch = Expression.LitralInst(StringEncoding.ValueFromString("true"));
        Expression encoded = Expression.LitralInst(StringEncoding.ValueFromString("encoded"));
        Expression environment = Expression.EnvironmentInstance;
        Expression labeled = Expression.LitralInst(StringEncoding.ValueFromString("labeled"));

        var testCases =
            new (Expression expression, PineValue.ListValue expected)[]
            {
                (
                    s_literal,
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("Litral"),
                        StringEncoding.ValueFromString("literal")
                        ])),
                (
                    Expression.ListInst([s_literal, input]),
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("List"),
                        ExpressionEncoding2026.EncodeExpressionAsValue(s_literal),
                        ExpressionEncoding2026.EncodeExpressionAsValue(input)
                        ])),
                (
                    Expression.BuiltinInst("length", input),
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("Builtin"),
                        StringEncoding.ValueFromString("length"),
                        ExpressionEncoding2026.EncodeExpressionAsValue(input)
                        ])),
                (
                    Expression.ConditionalInst(condition, falseBranch, trueBranch),
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("Conditional"),
                        ExpressionEncoding2026.EncodeExpressionAsValue(condition),
                        ExpressionEncoding2026.EncodeExpressionAsValue(falseBranch),
                        ExpressionEncoding2026.EncodeExpressionAsValue(trueBranch)
                        ])),
                (
                    Expression.EnvironmentInstance,
                    PineValue.List([StringEncoding.ValueFromString("Environment")])),
                (
                    new Expression.Eval(encoded, environment),
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("Eval"),
                        ExpressionEncoding2026.EncodeExpressionAsValue(encoded),
                        ExpressionEncoding2026.EncodeExpressionAsValue(environment)
                        ])),
                (
                    new Expression.Label("label", labeled),
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("Label"),
                        StringEncoding.ValueFromString("label"),
                        ExpressionEncoding2026.EncodeExpressionAsValue(labeled)
                        ]))
            };

        foreach (var (expression, expected) in testCases)
            ExpressionEncoding2026.EncodeExpressionAsValue(expression).Should().Be(expected);
    }

    [Fact]
    public void EncodeExpressionAsValue_encodes_an_empty_list_as_only_the_List_label()
    {
        ExpressionEncoding2026.EncodeExpressionAsValue(Expression.EmptyList)
            .Should().Be(PineValue.List([StringEncoding.ValueFromString("List")]));
    }

    [Fact]
    public void Encoding_and_decoding_are_symmetric_for_all_expression_variants()
    {
        var expressions =
            new Expression[]
            {
                s_literal,
                Expression.EmptyList,
                Expression.ListInst([s_literal, Expression.EnvironmentInstance]),
                Expression.BuiltinInst("length", s_literal),
                Expression.ConditionalInst(
                    s_literal,
                    Expression.EmptyList,
                    Expression.EnvironmentInstance),
                Expression.EnvironmentInstance,
                new Expression.Eval(s_literal, Expression.EnvironmentInstance),
                new Expression.Label("label", s_literal)
            };

        foreach (var expression in expressions)
        {
            var encoded = ExpressionEncoding2026.EncodeExpressionAsValue(expression);
            var decoded =
                ExpressionEncoding2026.ParseExpressionFromValue(encoded)
                .Extract(error => throw new Exception(error));

            decoded.Should().Be(expression);
        }
    }

    [Fact]
    public void Internal_parse_results_are_value_types()
    {
        typeof(ExpressionEncoding2026.ParseExpressionResult).IsValueType.Should().BeTrue();
        typeof(StringEncoding.StringParseResult).IsValueType.Should().BeTrue();
    }

    [Fact]
    public void Internal_string_parser_does_not_allocate_for_reused_values()
    {
        var encodedTag = StringEncoding.ValueFromString("Conditional");

        for (var i = 0; i < 10; ++i)
            _ = StringEncoding.StringFromValueWithoutResultAllocation(encodedTag);

        var allocatedBefore = GC.GetAllocatedBytesForCurrentThread();

        StringEncoding.StringParseResult parsed = default;

        for (var i = 0; i < 100; ++i)
            parsed = StringEncoding.StringFromValueWithoutResultAllocation(encodedTag);

        var allocatedBytes =
            GC.GetAllocatedBytesForCurrentThread() - allocatedBefore;

        GC.KeepAlive(parsed.Value);

        parsed.Value.Should().Be("Conditional");
        allocatedBytes.Should().Be(0);
    }

    [Fact]
    public void Public_node_parser_uses_supplied_child_parser()
    {
        var input = Expression.LitralInst(StringEncoding.ValueFromString("input"));
        var expression = Expression.BuiltinInst("length", input);
        var encoded = ExpressionEncoding2026.EncodeExpressionAsValue(expression);
        var parsedChildren = new List<PineValue>();

        Result<string, Expression> ParseChild(PineValue child)
        {
            parsedChildren.Add(child);
            return ExpressionEncoding2026.ParseExpressionFromValue(child);
        }

        var parsed =
            ExpressionEncoding2026.ParseExpressionFromValue(encoded, ParseChild)
            .Extract(error => throw new Exception(error));

        parsed.Should().Be(expression);
        parsedChildren.Should().Equal(encoded.Items.Span[2]);
    }

    [Fact]
    public void Label_accepts_any_PineValue()
    {
        var labelValue =
            PineValue.List(
                [
                PineValue.Blob([1, 2, 3]),
                PineValue.EmptyList
                ]);

        var expression = new Expression.Label(labelValue, s_literal);

        var encoded = ExpressionEncoding2026.EncodeExpressionAsValue(expression);
        encoded.Items.Span[1].Should().Be(labelValue);

        var decoded =
            ExpressionEncoding2026.ParseExpressionFromValue(encoded)
            .Extract(error => throw new Exception(error))
            .Should().BeOfType<Expression.Label>().Subject;

        decoded.LabelValue.Should().Be(labelValue);
        decoded.Should().Be(expression);
    }

    [Fact]
    public void Shared_parser_recognizes_Litral_as_the_2026_literal_encoding()
    {
        var literal2026 = ExpressionEncoding2026.EncodeExpressionAsValue(s_literal);

        StringEncoding.StringFromValue(literal2026.Items.Span[0])
            .Extract(error => throw new Exception(error))
            .Should().Be("Litral");

        ExpressionEncoding2024.ParseExpressionFromValue(literal2026)
            .IsErrOrNull().Should().NotBeNull();

        ExpressionEncoding.ParseExpressionFromValue(literal2026)
            .Extract(error => throw new Exception(error))
            .Should().Be(s_literal);
    }

    [Fact]
    public void Parser_reports_malformed_values()
    {
        var testCases =
            new PineValue[]
            {
                PineValue.EmptyBlob,
                PineValue.EmptyList,
                PineValue.List([StringEncoding.ValueFromString("Unknown")]),
                PineValue.List([StringEncoding.ValueFromString("Litral")]),
                PineValue.List(
                    [
                    StringEncoding.ValueFromString("Builtin"),
                    StringEncoding.ValueFromString("length")
                    ]),
                PineValue.List(
                    [
                    StringEncoding.ValueFromString("Conditional"),
                    ExpressionEncoding2026.EncodeExpressionAsValue(s_literal)
                    ]),
                PineValue.List(
                    [
                    StringEncoding.ValueFromString("Environment"),
                    PineValue.EmptyList
                    ]),
                PineValue.List(
                    [
                    StringEncoding.ValueFromString("Eval"),
                    ExpressionEncoding2026.EncodeExpressionAsValue(s_literal)
                    ]),
                PineValue.List(
                    [
                    StringEncoding.ValueFromString("Label"),
                    StringEncoding.ValueFromString("label")
                    ])
            };

        foreach (var testCase in testCases)
            ExpressionEncoding2026.ParseExpressionFromValue(testCase).IsErrOrNull().Should().NotBeNull();
    }

    [Fact]
    public void Encoder_delegate_is_consulted_once_for_each_direct_subexpression()
    {
        Expression condition = Expression.LitralInst(StringEncoding.ValueFromString("condition"));
        Expression falseBranch = Expression.LitralInst(StringEncoding.ValueFromString("false"));
        Expression trueBranch = Expression.LitralInst(StringEncoding.ValueFromString("true"));

        var expression =
            Expression.ConditionalInst(condition, falseBranch, trueBranch);

        var calls = new List<Expression>();

        PineValue.ListValue Encode(Expression child)
        {
            calls.Add(child);
            return ExpressionEncoding2026.EncodeExpressionAsValue(child);
        }

        ExpressionEncoding2026.EncodeExpressionAsValueWithoutTopLevelCacheLookup(expression, Encode);

        calls.Should().Equal(condition, falseBranch, trueBranch);
    }

    [Fact]
    public void ParseExpressionFromValue_does_not_overflow_on_deeply_nested_expression()
    {
        const int depth = 100_000;

        Expression nested = s_literal;

        for (var i = 0; i < depth; ++i)
            nested = Expression.BuiltinInst(nameof(BuiltinFunction.length), nested);

        var encoded = ExpressionEncoding2026.EncodeExpressionAsValue(nested);
        var decoded =
            ExpressionEncoding2026.ParseExpressionFromValue(encoded)
            .Extract(error => throw new Exception(error));

        var current = decoded;

        for (var i = 0; i < depth; ++i)
        {
            var builtin = current.Should().BeOfType<Expression.Builtin>().Subject;

            builtin.Function.Should().Be(nameof(BuiltinFunction.length));
            current = builtin.Input;
        }

        current.Should().Be(s_literal);
    }
}
