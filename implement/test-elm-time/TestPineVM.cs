using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.PineVM;
using System;
using System.Collections.Immutable;

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
                expression = (Expression)new Expression.ListExpression(ImmutableArray<Expression>.Empty),
                expected = Result<string, PineValue>.ok(
                    PineValue.List(ImmutableList<PineValue>.Empty))
            },
            new
            {
                expression = (Expression)PineVM.DecodeKernelApplicationExpression
                (
                    functionName: "concat",
                    argument: new Expression.ListExpression(ImmutableArray<Expression>.Empty)
                ).Extract(fromErr: err => throw new Exception(err)),
                expected = Result<string, PineValue>.ok(
                    PineValue.List(ImmutableList<PineValue>.Empty))
            }
        };

        foreach (var testCase in testCases)
        {
            var pineVM = new PineVM();

            var evaluated = pineVM.EvaluateExpression(
                testCase.expression,
                PineValue.List(ImmutableList<PineValue>.Empty));

            Assert.AreEqual(testCase.expected, evaluated);
        }
    }
}