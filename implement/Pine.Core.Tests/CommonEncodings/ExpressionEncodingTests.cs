using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Xunit;

namespace Pine.Core.Tests.CommonEncodings;

public class ExpressionEncodingTests
{
    [Fact]
    public void ExpressionEncoding_emits_2026_and_parses_2024()
    {
        var expression =
            Expression.KernelApplicationInstance(
                nameof(BuiltinFunction.length),
                Expression.LiteralInstance(StringEncoding.ValueFromString("input")));

        ExpressionEncoding.EncodeExpressionAsValue(expression)
            .Should().Be(ExpressionEncoding2026.EncodeExpressionAsValue(expression));

        var encoded2024 = ExpressionEncoding2024.EncodeExpressionAsValue(expression);

        ExpressionEncoding.ParseExpressionFromValue(encoded2024)
            .Extract(error => throw new System.Exception(error))
            .Should().Be(expression);

        new Core.CodeAnalysis.PineVMParseCache()
            .ParseExpression(encoded2024)
            .Extract(error => throw new System.Exception(error))
            .Should().Be(expression);

        new PineExpressionEncodingCache()
            .EncodeExpressionAsValue(expression)
            .Should().Be(ExpressionEncoding2026.EncodeExpressionAsValue(expression));
    }

    [Fact]
    public void ExpressionEncoding_Decoding_Symmetry()
    {
        var testCases =
            new Expression[]
            {
                Expression.LiteralInstance(StringEncoding.ValueFromString("literal content")),

                new Expression.Environment(),

                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(StringEncoding.ValueFromString("list element alfa")),
                    Expression.LiteralInstance(StringEncoding.ValueFromString("list element beta")),
                    Expression.LiteralInstance(StringEncoding.ValueFromString("list element gamma")),
                    ]),

                Expression.ConditionalInstance(
                    condition: Expression.LiteralInstance(StringEncoding.ValueFromString("condition")),
                    falseBranch: Expression.LiteralInstance(StringEncoding.ValueFromString("if false")),
                    trueBranch: Expression.LiteralInstance(StringEncoding.ValueFromString("if true"))),

                new Expression.ParseAndEval(
                    encoded: Expression.LiteralInstance(StringEncoding.ValueFromString("encoded")),
                    environment: Expression.LiteralInstance(StringEncoding.ValueFromString("environment"))),

                Expression.KernelApplicationInstance(
                    function: nameof(BuiltinFunction.length),
                    input: Expression.LiteralInstance(StringEncoding.ValueFromString("kernel app arg"))),

                new Expression.StringTag(
                    tag: "tag text",
                    tagged: Expression.LiteralInstance(StringEncoding.ValueFromString("tagged expr")))
            };

        foreach (var testCase in testCases)
        {
            var encoded =
                ExpressionEncoding.EncodeExpressionAsValue(testCase);

            var decoded =
                ExpressionEncoding.ParseExpressionFromValue(encoded)
                .Extract(err => throw new System.Exception("Failed to decode expression: " + err));

            decoded.Should().Be(testCase);
        }
    }

    [Fact]
    public void ParseExpressionFromValue_does_not_stack_overflow_on_deeply_nested_expression()
    {
        // A deeply nested chain of expressions previously overflowed the call stack when decoding,
        // because parsing recursed once per nesting level. Decoding now uses an explicit work stack.

        const int depth = 100_000;

        Expression nested =
            Expression.LiteralInstance(StringEncoding.ValueFromString("innermost"));

        for (var i = 0; i < depth; ++i)
        {
            nested =
                Expression.KernelApplicationInstance(
                    function: nameof(BuiltinFunction.length),
                    input: nested);
        }

        var encoded =
            ExpressionEncoding.EncodeExpressionAsValue(nested);

        var decoded =
            ExpressionEncoding.ParseExpressionFromValue(encoded)
            .Extract(err => throw new System.Exception("Failed to decode expression: " + err));

        // Verify the decoded structure by unwrapping iteratively; a recursive equality check
        // (e.g. 'Should().Be') would itself overflow the stack at this depth.
        var current = decoded;

        for (var i = 0; i < depth; ++i)
        {
            var kernelApplication = current.Should().BeOfType<Expression.KernelApplication>().Subject;

            kernelApplication.Function.Should().Be(nameof(BuiltinFunction.length));

            current = kernelApplication.Input;
        }

        current.Should().BeOfType<Expression.Literal>()
            .Subject.Value.Should().Be(StringEncoding.ValueFromString("innermost"));
    }

    [Fact]
    public void PineVMParseCache_does_not_stack_overflow_on_deeply_nested_expression()
    {
        // Parsing through PineVMParseCache previously recursed once per nesting level via the
        // 'generalParser' delegate. It now uses an explicit work stack backed by the cache.

        const int depth = 100_000;

        Expression nested =
            Expression.LiteralInstance(StringEncoding.ValueFromString("innermost"));

        for (var i = 0; i < depth; ++i)
        {
            nested =
                Expression.KernelApplicationInstance(
                    function: nameof(BuiltinFunction.length),
                    input: nested);
        }

        var encoded =
            ExpressionEncoding.EncodeExpressionAsValue(nested);

        var parseCache = new Core.CodeAnalysis.PineVMParseCache();

        var decoded =
            parseCache.ParseExpression(encoded)
            .Extract(err => throw new System.Exception("Failed to parse expression: " + err));

        // Verify the parsed structure by unwrapping iteratively; a recursive equality check
        // (e.g. 'Should().Be') would itself overflow the stack at this depth.
        var current = decoded;

        for (var i = 0; i < depth; ++i)
        {
            var kernelApplication = current.Should().BeOfType<Expression.KernelApplication>().Subject;

            kernelApplication.Function.Should().Be(nameof(BuiltinFunction.length));

            current = kernelApplication.Input;
        }

        current.Should().BeOfType<Expression.Literal>()
            .Subject.Value.Should().Be(StringEncoding.ValueFromString("innermost"));
    }
}
