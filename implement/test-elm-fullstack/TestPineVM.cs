using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Immutable;

namespace test_elm_fullstack;

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
                expression = new Pine.Expression
                {
                    LiteralExpression= Pine.Composition.Component.Blob(new byte[]{1,4,7})
                },
                expected = Pine.Result<string, Pine.Composition.Component>.ok(
                    Pine.Composition.Component.Blob(new byte[]{1,4,7}))
            },
            new
            {
                expression = new Pine.Expression
                {
                    ListExpression = ImmutableArray<Pine.Expression>.Empty
                },
                expected = Pine.Result<string, Pine.Composition.Component>.ok(
                    Pine.Composition.Component.List(ImmutableList<Pine.Composition.Component>.Empty))
            },
            new
            {
                expression = new Pine.Expression
                {
                    KernelApplicationExpression = new Pine.KernelApplicationExpressionStructure
                    (
                        functionName : "concat",
                        argument : new Pine.Expression
                        {
                            ListExpression = ImmutableArray<Pine.Expression>.Empty
                        }
                    )
                },
                expected = Pine.Result<string, Pine.Composition.Component>.ok(
                    Pine.Composition.Component.List(ImmutableList<Pine.Composition.Component>.Empty))
            }
        };

        foreach (var testCase in testCases)
        {
            var evaluated = Pine.PineVM.EvaluateExpression(
                Pine.Composition.Component.List(ImmutableList<Pine.Composition.Component>.Empty),
                testCase.expression);

            Assert.AreEqual(testCase.expected, evaluated);
        }
    }
}
