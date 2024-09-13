using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
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
            Expression.EnvironmentInstance,
            Expression.ListInstance([]),

            new Expression.Literal(
                PineValueAsInteger.ValueFromSignedInteger(1)),

            new Expression.Literal(
                PineValueAsString.ValueFromString("Hello world!")),

            new Expression.Literal(
                PineValue.List(
                    [
                    PineValueAsString.ValueFromString("String"),
                    PineValue.List([PineValueAsString.ValueFromString("Hello world!")])
                    ]
                    )),

            // Function 'isPineList' from the core libraries.
            new Expression.KernelApplication(
            input: Expression.ListInstance(
                [new Expression.KernelApplication(
                    input: Expression.ListInstance(
                        [new Expression.Literal(
                            PineValueAsInteger.ValueFromSignedInteger(
                                0)), new Expression.KernelApplication(
                            input: new Expression.KernelApplication(
                                input: new Expression.KernelApplication(
                                    input: Expression.ListInstance(
                                        [new Expression.Literal(
                                            PineValueAsInteger.ValueFromSignedInteger(
                                                1)), new Expression.Environment()]),
                                    function: "skip"),
                                function: "head"),
                            function: "head")]),
                    function: "take"), new Expression.Literal(
                    PineValue.EmptyList)]),
            function: "equal")

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
            [Expression.EnvironmentInstance],
            [Expression.ListInstance([])],

            [new Expression.Literal(
                PineValueAsInteger.ValueFromSignedInteger(1))],

            [new Expression.Literal(
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