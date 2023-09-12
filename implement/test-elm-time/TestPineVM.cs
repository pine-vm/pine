using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.PineVM;
using System;

namespace TestElmTime;

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
                expression = (Expression)new Expression.LiteralExpression(PineValue.Blob(new byte[] { 1, 4, 7 })),
                expected = Result<string, PineValue>.ok(
                    PineValue.Blob(new byte[] { 1, 4, 7 }))
            },
            new
            {
                expression = (Expression)new Expression.ListExpression([]),
                expected = Result<string, PineValue>.ok(PineValue.EmptyList)
            },
            new
            {
                expression = (Expression)PineVM.DecodeKernelApplicationExpression
                (
                    functionName: "concat",
                    argument: new Expression.ListExpression([])
                ).Extract(fromErr: err => throw new Exception(err)),
                expected = Result<string, PineValue>.ok(PineValue.EmptyList)
            }
        };

        foreach (var testCase in testCases)
        {
            var pineVM = new PineVM();

            var evaluated = pineVM.EvaluateExpression(
                testCase.expression,
                PineValue.EmptyList);

            Assert.AreEqual(testCase.expected, evaluated);
        }
    }
}