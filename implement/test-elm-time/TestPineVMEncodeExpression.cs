using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System.Collections.Immutable;
using System.Text.Json;

namespace TestElmTime;

[TestClass]
public class TestPineVMEncodeExpression
{
    [TestMethod]
    public void TestExpressionEncodeDecodeSymmetry()
    {
        var testCases = new PineVM.Expression[]
        {
            new PineVM.Expression.LiteralExpression(Composition.ComponentFromString("literal content")),

            new PineVM.Expression.EnvironmentExpression(),

            new PineVM.Expression.ListExpression(
                ImmutableArray.Create<PineVM.Expression>(
                    new PineVM.Expression.LiteralExpression(Composition.ComponentFromString("list element alfa")),
                    new PineVM.Expression.LiteralExpression(Composition.ComponentFromString("list element beta")),
                    new PineVM.Expression.LiteralExpression(Composition.ComponentFromString("list element gamma"))
            )),

            new PineVM.Expression.ConditionalExpression(
                condition: new PineVM.Expression.LiteralExpression(Composition.ComponentFromString("condition")),
                ifTrue: new PineVM.Expression.LiteralExpression(Composition.ComponentFromString("if true")),
                ifFalse: new PineVM.Expression.LiteralExpression(Composition.ComponentFromString("if false"))),

            new PineVM.Expression.DecodeAndEvaluateExpression(
                expression: new PineVM.Expression.LiteralExpression(Composition.ComponentFromString("expression")),
                environment: new PineVM.Expression.LiteralExpression(Composition.ComponentFromString("environment"))),

            new PineVM.Expression.KernelApplicationExpression(
                functionName: nameof(PineVM.KernelFunction.length),
                argument: new PineVM.Expression.LiteralExpression(Composition.ComponentFromString("kernel app arg")),
                function: _ => Result<string, PineValue>.err("Not implemented")),

            new PineVM.Expression.StringTagExpression(
                tag: "tag text",
                tagged: new PineVM.Expression.LiteralExpression(Composition.ComponentFromString("tagged expr")))
        };

        foreach (var testCase in testCases)
        {
            var encoded =
                PineVM.EncodeExpressionAsValue(testCase)
                .Extract(err => throw new System.Exception("Failed to encode expression: " + err));

            var decoded =
                PineVM.DecodeExpressionFromValueDefault(encoded)
                .Extract(err => throw new System.Exception("Failed to decode expression: " + err));

            var testCaseJson = JsonSerializer.Serialize(testCase);
            var decodedJson = JsonSerializer.Serialize(decoded);

            Assert.AreEqual(testCase, decoded);
        }
    }
}
