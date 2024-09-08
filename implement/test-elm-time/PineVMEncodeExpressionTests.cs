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
            new Expression.Literal(PineValueAsString.ValueFromString("literal content")),

            new Expression.Environment(),

            Expression.ListInstance(
                [
                    new Expression.Literal(PineValueAsString.ValueFromString("list element alfa")),
                    new Expression.Literal(PineValueAsString.ValueFromString("list element beta")),
                    new Expression.Literal(PineValueAsString.ValueFromString("list element gamma")),
                ]),

            Expression.ConditionalInstance(
                condition: new Expression.Literal(PineValueAsString.ValueFromString("condition")),
                falseBranch: new Expression.Literal(PineValueAsString.ValueFromString("if false")),
                trueBranch: new Expression.Literal(PineValueAsString.ValueFromString("if true"))),

            new Expression.ParseAndEval(
                encoded: new Expression.Literal(PineValueAsString.ValueFromString("encoded")),
                environment: new Expression.Literal(PineValueAsString.ValueFromString("environment"))),

            new Expression.KernelApplication(
                function: nameof(KernelFunction.length),
                input: new Expression.Literal(PineValueAsString.ValueFromString("kernel app arg"))),

            new Expression.StringTag(
                tag: "tag text",
                tagged: new Expression.Literal(PineValueAsString.ValueFromString("tagged expr")))
        };

        foreach (var testCase in testCases)
        {
            var encoded =
                ExpressionEncoding.EncodeExpressionAsValue(testCase);

            var decoded =
                ExpressionEncoding.ParseExpressionFromValueDefault(encoded)
                .Extract(err => throw new System.Exception("Failed to decode expression: " + err));

            Assert.AreEqual(testCase, decoded);
        }
    }
}
