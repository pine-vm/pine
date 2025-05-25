using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.Core.PopularEncodings;
using Pine.Core.Json;
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
                IntegerEncoding.EncodeSignedInteger(1)),

            new Expression.Literal(
                StringEncoding.ValueFromString("Hello world!")),

            new Expression.Literal(
                PineValue.List(
                    [
                    StringEncoding.ValueFromString("String"),
                    PineValue.List([StringEncoding.ValueFromString("Hello world!")])
                    ]
                    )),

            // Function 'isPineList' from the core libraries.
            new Expression.KernelApplication(
            input: Expression.ListInstance(
                [new Expression.KernelApplication(
                    input: Expression.ListInstance(
                        [new Expression.Literal(
                            IntegerEncoding.EncodeSignedInteger(
                                0)), new Expression.KernelApplication(
                            input: new Expression.KernelApplication(
                                input: new Expression.KernelApplication(
                                    input: Expression.ListInstance(
                                        [new Expression.Literal(
                                            IntegerEncoding.EncodeSignedInteger(
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

            deserialized.Should().Be(testCase, $"test case {i} should roundtrip properly");
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
                IntegerEncoding.EncodeSignedInteger(1))],

            [new Expression.Literal(
                StringEncoding.ValueFromString("Hello world!"))],

            ];

        for (int i = 0; i < testCases.Count; i++)
        {
            var testCase = testCases[i];

            var json = EncodePineExpressionAsJson.ToJsonString(testCase);

            var deserialized = EncodePineExpressionAsJson.ListFromJsonString(json);

            deserialized.Should().Equal(testCase, $"test case {i} should roundtrip properly");
        }
    }
}