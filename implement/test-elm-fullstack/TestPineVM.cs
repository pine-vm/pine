using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System.Collections.Immutable;

namespace TestElmFullstack;

[TestClass]
public class TestPineVM
{
    [TestMethod]
    public void Evaluate_expression()
    {
        var testCases = new[]
        {
            new
            {
                expression = (PineVM.Expression)new PineVM.Expression.LiteralExpression(PineValue.Blob(new byte[]{1,4,7})),
                expected = Result<string, PineValue>.ok(
                    PineValue.Blob(new byte[]{1,4,7}))
            },
            new
            {
                expression = (PineVM.Expression)new PineVM.Expression.ListExpression(ImmutableArray<PineVM.Expression>.Empty),
                expected = Result<string, PineValue>.ok(
                    PineValue.List(ImmutableList<PineValue>.Empty))
            },
            new
            {
                expression = (PineVM.Expression)new PineVM.Expression.KernelApplicationExpression
                (
                    functionName : "concat",
                    argument : new PineVM.Expression.ListExpression(ImmutableArray<PineVM.Expression>.Empty)
                ),
                expected = Result<string, PineValue>.ok(
                    PineValue.List(ImmutableList<PineValue>.Empty))
            }
        };

        foreach (var testCase in testCases)
        {
            var evaluated = PineVM.EvaluateExpression(
                PineValue.List(ImmutableList<PineValue>.Empty),
                testCase.expression);

            Assert.AreEqual(testCase.expected, evaluated);
        }
    }
}
