using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.PineVM;

namespace TestElmTime;

[TestClass]
public class PineVMEncodeExpressionTests
{
    [TestMethod]
    public void TestExpressionEncodeDecodeSymmetry()
    {
        var testCases = new Expression[]
        {
            new Expression.LiteralExpression(PineValueAsString.ValueFromString("literal content")),

            new Expression.EnvironmentExpression(),

            new Expression.ListExpression(
                [
                    new Expression.LiteralExpression(PineValueAsString.ValueFromString("list element alfa")),
                    new Expression.LiteralExpression(PineValueAsString.ValueFromString("list element beta")),
                    new Expression.LiteralExpression(PineValueAsString.ValueFromString("list element gamma")),
                ]),

            new Expression.ConditionalExpression(
                condition: new Expression.LiteralExpression(PineValueAsString.ValueFromString("condition")),
                trueBranch: new Expression.LiteralExpression(PineValueAsString.ValueFromString("if true")),
                falseBranch: new Expression.LiteralExpression(PineValueAsString.ValueFromString("if false"))),

            new Expression.ParseAndEvalExpression(
                expression: new Expression.LiteralExpression(PineValueAsString.ValueFromString("expression")),
                environment: new Expression.LiteralExpression(PineValueAsString.ValueFromString("environment"))),

            new Expression.KernelApplicationExpression(
                functionName: nameof(KernelFunction.length),
                argument: new Expression.LiteralExpression(PineValueAsString.ValueFromString("kernel app arg")),
                function: _ => throw new System.NotImplementedException()),

            new Expression.StringTagExpression(
                tag: "tag text",
                tagged: new Expression.LiteralExpression(PineValueAsString.ValueFromString("tagged expr")))
        };

        foreach (var testCase in testCases)
        {
            var encoded =
                ExpressionEncoding.EncodeExpressionAsValue(testCase)
                .Extract(err => throw new System.Exception("Failed to encode expression: " + err));

            var decoded =
                ExpressionEncoding.ParseExpressionFromValueDefault(encoded)
                .Extract(err => throw new System.Exception("Failed to decode expression: " + err));

            Assert.AreEqual(testCase, decoded);
        }
    }
}
