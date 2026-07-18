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
            Expression.BuiltinInst(
                nameof(BuiltinFunction.length),
                Expression.LitralInst(StringEncoding.ValueFromString("input")));

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
                Expression.LitralInst(StringEncoding.ValueFromString("literal content")),

                new Expression.Environment(),

                Expression.ListInst(
                    [
                    Expression.LitralInst(StringEncoding.ValueFromString("list element alfa")),
                    Expression.LitralInst(StringEncoding.ValueFromString("list element beta")),
                    Expression.LitralInst(StringEncoding.ValueFromString("list element gamma")),
                    ]),

                Expression.ConditionalInst(
                    condition: Expression.LitralInst(StringEncoding.ValueFromString("condition")),
                    falseBranch: Expression.LitralInst(StringEncoding.ValueFromString("if false")),
                    trueBranch: Expression.LitralInst(StringEncoding.ValueFromString("if true"))),

                new Expression.Eval(
                    encoded: Expression.LitralInst(StringEncoding.ValueFromString("encoded")),
                    environment: Expression.LitralInst(StringEncoding.ValueFromString("environment"))),

                Expression.BuiltinInst(
                    function: nameof(BuiltinFunction.length),
                    input: Expression.LitralInst(StringEncoding.ValueFromString("kernel app arg"))),

                new Expression.Label(
                    tag: "tag text",
                    tagged: Expression.LitralInst(StringEncoding.ValueFromString("tagged expr")))
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
            Expression.LitralInst(StringEncoding.ValueFromString("innermost"));

        for (var i = 0; i < depth; ++i)
        {
            nested =
                Expression.BuiltinInst(
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
            var kernelApplication = current.Should().BeOfType<Expression.Builtin>().Subject;

            kernelApplication.Function.Should().Be(nameof(BuiltinFunction.length));

            current = kernelApplication.Input;
        }

        current.Should().BeOfType<Expression.Litral>()
            .Subject.Value.Should().Be(StringEncoding.ValueFromString("innermost"));
    }

    [Fact]
    public void PineVMParseCache_does_not_stack_overflow_on_deeply_nested_expression()
    {
        // Parsing through PineVMParseCache previously recursed once per nesting level via the
        // 'generalParser' delegate. It now uses an explicit work stack backed by the cache.

        const int depth = 100_000;

        Expression nested =
            Expression.LitralInst(StringEncoding.ValueFromString("innermost"));

        for (var i = 0; i < depth; ++i)
        {
            nested =
                Expression.BuiltinInst(
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
            var kernelApplication = current.Should().BeOfType<Expression.Builtin>().Subject;

            kernelApplication.Function.Should().Be(nameof(BuiltinFunction.length));

            current = kernelApplication.Input;
        }

        current.Should().BeOfType<Expression.Litral>()
            .Subject.Value.Should().Be(StringEncoding.ValueFromString("innermost"));
    }
}
