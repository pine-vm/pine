using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.CommonEncodings;

public class ExpressionEncoding2026Tests
{
    private static readonly Expression s_literal =
        Expression.LiteralInstance(StringEncoding.ValueFromString("literal"));

    [Fact]
    public void EncodeExpressionAsValue_uses_the_2026_shapes_and_labels()
    {
        Expression input = Expression.LiteralInstance(StringEncoding.ValueFromString("input"));
        Expression condition = Expression.LiteralInstance(StringEncoding.ValueFromString("condition"));
        Expression falseBranch = Expression.LiteralInstance(StringEncoding.ValueFromString("false"));
        Expression trueBranch = Expression.LiteralInstance(StringEncoding.ValueFromString("true"));
        Expression encoded = Expression.LiteralInstance(StringEncoding.ValueFromString("encoded"));
        Expression environment = Expression.EnvironmentInstance;
        Expression labeled = Expression.LiteralInstance(StringEncoding.ValueFromString("labeled"));

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
                    Expression.ListInstance([s_literal, input]),
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("List"),
                        ExpressionEncoding2026.EncodeExpressionAsValue(s_literal),
                        ExpressionEncoding2026.EncodeExpressionAsValue(input)
                        ])),
                (
                    Expression.KernelApplicationInstance("length", input),
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("Builtin"),
                        StringEncoding.ValueFromString("length"),
                        ExpressionEncoding2026.EncodeExpressionAsValue(input)
                        ])),
                (
                    Expression.ConditionalInstance(condition, falseBranch, trueBranch),
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("Condition"),
                        ExpressionEncoding2026.EncodeExpressionAsValue(condition),
                        ExpressionEncoding2026.EncodeExpressionAsValue(falseBranch),
                        ExpressionEncoding2026.EncodeExpressionAsValue(trueBranch)
                        ])),
                (
                    Expression.EnvironmentInstance,
                    PineValue.List([StringEncoding.ValueFromString("Environment")])),
                (
                    new Expression.ParseAndEval(encoded, environment),
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("Eval"),
                        ExpressionEncoding2026.EncodeExpressionAsValue(encoded),
                        ExpressionEncoding2026.EncodeExpressionAsValue(environment)
                        ])),
                (
                    new Expression.StringTag("label", labeled),
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
                Expression.ListInstance([s_literal, Expression.EnvironmentInstance]),
                Expression.KernelApplicationInstance("length", s_literal),
                Expression.ConditionalInstance(
                    s_literal,
                    Expression.EmptyList,
                    Expression.EnvironmentInstance),
                Expression.EnvironmentInstance,
                new Expression.ParseAndEval(s_literal, Expression.EnvironmentInstance),
                new Expression.StringTag("label", s_literal)
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
    public void Label_accepts_any_PineValue()
    {
        var labelValue =
            PineValue.List(
                [
                PineValue.Blob([1, 2, 3]),
                PineValue.EmptyList
                ]);

        var expression = new Expression.StringTag(labelValue, s_literal);

        var encoded = ExpressionEncoding2026.EncodeExpressionAsValue(expression);
        encoded.Items.Span[1].Should().Be(labelValue);

        var decoded =
            ExpressionEncoding2026.ParseExpressionFromValue(encoded)
            .Extract(error => throw new Exception(error))
            .Should().BeOfType<Expression.StringTag>().Subject;

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
                    StringEncoding.ValueFromString("Condition"),
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
        Expression condition = Expression.LiteralInstance(StringEncoding.ValueFromString("condition"));
        Expression falseBranch = Expression.LiteralInstance(StringEncoding.ValueFromString("false"));
        Expression trueBranch = Expression.LiteralInstance(StringEncoding.ValueFromString("true"));

        var expression =
            Expression.ConditionalInstance(condition, falseBranch, trueBranch);

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
            nested = Expression.KernelApplicationInstance(nameof(KernelFunction.length), nested);

        var encoded = ExpressionEncoding2026.EncodeExpressionAsValue(nested);
        var decoded =
            ExpressionEncoding2026.ParseExpressionFromValue(encoded)
            .Extract(error => throw new Exception(error));

        var current = decoded;

        for (var i = 0; i < depth; ++i)
        {
            var builtin = current.Should().BeOfType<Expression.KernelApplication>().Subject;

            builtin.Function.Should().Be(nameof(KernelFunction.length));
            current = builtin.Input;
        }

        current.Should().Be(s_literal);
    }
}
