using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Xunit;

namespace Pine.Core.Tests.CommonEncodings;

public class ExpressionEncodingTests
{
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
                    function: nameof(KernelFunction.length),
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
}
