using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.Core.PopularEncodings;

namespace TestElmTime;

[TestClass]
public class PineVMEncodeExpressionTests
{
    [TestMethod]
    public void TestExpressionEncodeDecodeSymmetry()
    {
        var testCases = new Expression[]
        {
            new Expression.Literal(StringEncoding.ValueFromString("literal content")),

            new Expression.Environment(),

            Expression.ListInstance(
                [
                    new Expression.Literal(StringEncoding.ValueFromString("list element alfa")),
                    new Expression.Literal(StringEncoding.ValueFromString("list element beta")),
                    new Expression.Literal(StringEncoding.ValueFromString("list element gamma")),
                ]),

            Expression.ConditionalInstance(
                condition: new Expression.Literal(StringEncoding.ValueFromString("condition")),
                falseBranch: new Expression.Literal(StringEncoding.ValueFromString("if false")),
                trueBranch: new Expression.Literal(StringEncoding.ValueFromString("if true"))),

            new Expression.ParseAndEval(
                encoded: new Expression.Literal(StringEncoding.ValueFromString("encoded")),
                environment: new Expression.Literal(StringEncoding.ValueFromString("environment"))),

            new Expression.KernelApplication(
                function: nameof(KernelFunction.length),
                input: new Expression.Literal(StringEncoding.ValueFromString("kernel app arg"))),

            new Expression.StringTag(
                tag: "tag text",
                tagged: new Expression.Literal(StringEncoding.ValueFromString("tagged expr")))
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
