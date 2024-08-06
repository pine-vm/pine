using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.PineVM;
using System.Collections.Generic;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class EncodePineExpressionAsJsonTests
{
    [TestMethod]
    public void EncodePineExpressionAsJsonRoundtrips()
    {
        IReadOnlyList<Expression> testCases =
            [
            Expression.Environment,
            new Expression.ListExpression([]),

            new Expression.LiteralExpression(
                PineValueAsInteger.ValueFromSignedInteger(1)),

            new Expression.LiteralExpression(
                PineValueAsString.ValueFromString("Hello world!")),

            new Expression.LiteralExpression(
                PineValue.List(
                    [
                    PineValueAsString.ValueFromString("String"),
                    PineValue.List([PineValueAsString.ValueFromString("Hello world!")])
                    ]
                    )),

            // Function 'isPineList' from the core libraries.
            new Expression.KernelApplicationExpression(
            argument: new Expression.ListExpression(
                [new Expression.KernelApplicationExpression(
                    argument: new Expression.ListExpression(
                        [new Expression.LiteralExpression(
                            PineValueAsInteger.ValueFromSignedInteger(
                                0)), new Expression.KernelApplicationExpression(
                            argument: new Expression.KernelApplicationExpression(
                                argument: new Expression.KernelApplicationExpression(
                                    argument: new Expression.ListExpression(
                                        [new Expression.LiteralExpression(
                                            PineValueAsInteger.ValueFromSignedInteger(
                                                1)), new Expression.EnvironmentExpression()]),
                                    functionName: "skip"),
                                functionName: "list_head"),
                            functionName: "list_head")]),
                    functionName: "take"), new Expression.LiteralExpression(
                    PineValue.EmptyList)]),
            functionName: "equal")

            ];

        for (int i = 0; i < testCases.Count; i++)
        {
            var testCase = testCases[i];

            var json = EncodePineExpressionAsJson.ToJsonString(testCase);

            var deserialized = EncodePineExpressionAsJson.SingleFromJsonString(json);

            Assert.AreEqual(testCase, deserialized, "test case " + i);
        }
    }

    [TestMethod]
    public void EncodePineExpressionListAsJsonRoundtrips()
    {
        IReadOnlyList<IReadOnlyList<Expression>> testCases =
            [
            [Expression.Environment],
            [new Expression.ListExpression([])],

            [new Expression.LiteralExpression(
                PineValueAsInteger.ValueFromSignedInteger(1))],

            [new Expression.LiteralExpression(
                PineValueAsString.ValueFromString("Hello world!"))],

            ];

        for (int i = 0; i < testCases.Count; i++)
        {
            var testCase = testCases[i];

            var json = EncodePineExpressionAsJson.ToJsonString(testCase);

            var deserialized = EncodePineExpressionAsJson.ListFromJsonString(json);

            Assert.IsTrue(testCase.SequenceEqual(deserialized), "test case " + i);
        }
    }
}