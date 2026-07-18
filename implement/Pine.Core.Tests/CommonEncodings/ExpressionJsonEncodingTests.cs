using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Json;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.CommonEncodings;

public class ExpressionJsonEncodingTests
{
    [Fact]
    public void Decode_expression_from_JSON_string()
    {
        var testCases =
            new (string json, Expression expected)[]
            {
                // Environment expression.
                ("""{"Environment":[]}"""
                , Expression.EnvironmentInstance),

                // Empty list expression.
                ("""{"List":[[]]}"""
                , Expression.ListInst([])),

                // Literal expression with an integer value.
                ("""{"Literal":[42]}"""
                , Expression.LitralInst(
                    IntegerEncoding.EncodeSignedInteger(42))),

                // Literal expression with a string value.
                ("""{"Literal":[{"BlobAsString":"Hello"}]}"""
                , Expression.LitralInst(
                    StringEncoding.ValueFromString("Hello"))),

                // Literal expression with an empty list value.
                ("""{"Literal":[[]]}"""
                , Expression.LitralInst(PineValue.EmptyList)),

                // List expression with multiple items.
                ("""{"List":[[{"Environment":[]},{"Literal":[1]}]]}"""
                , Expression.ListInst(
                    [Expression.EnvironmentInstance,
                     Expression.LitralInst(
                        IntegerEncoding.EncodeSignedInteger(1))])),

                // Parse-and-eval expression.
                ("""{"ParseAndEval":[{"Literal":[7]},{"Environment":[]}]}"""
                , new Expression.Eval(
                    encoded: Expression.LitralInst(
                        IntegerEncoding.EncodeSignedInteger(7)),
                    environment: Expression.EnvironmentInstance)),

                // Kernel application expression.
                ("""{"KernelApplication":["head",{"Environment":[]}]}"""
                , Expression.BuiltinInst(
                    function: "head",
                    input: Expression.EnvironmentInstance)),

                // Conditional expression.
                ("""{"Conditional":[{"Environment":[]},{"Literal":[0]},{"Literal":[1]}]}"""
                , Expression.ConditionalInst(
                    condition: Expression.EnvironmentInstance,
                    falseBranch: Expression.LitralInst(
                        IntegerEncoding.EncodeSignedInteger(0)),
                    trueBranch: Expression.LitralInst(
                        IntegerEncoding.EncodeSignedInteger(1)))),

                // String tag expression.
                ("""{"StringTag":["my-tag",{"Environment":[]}]}"""
                , new Expression.Label(
                    tag: "my-tag",
                    tagged: Expression.EnvironmentInstance)),

                // Nested: kernel application with a list of subexpressions.
                ("""{"KernelApplication":["equal",{"List":[[{"KernelApplication":["head",{"Environment":[]}]},{"Literal":[4]}]]}]}"""
                , Expression.BuiltinInst(
                    function: "equal",
                    input: Expression.ListInst(
                        [Expression.BuiltinInst(
                            function: "head",
                            input: Expression.EnvironmentInstance),
                         Expression.LitralInst(
                            IntegerEncoding.EncodeSignedInteger(4))]))),

                // Deeply nested: conditional combining kernel application, parse-and-eval and string tag.
                ("""{"Conditional":[{"KernelApplication":["equal",{"Environment":[]}]},{"ParseAndEval":[{"Literal":[{"BlobAsString":"F"}]},{"List":[[{"Environment":[]}]]}]},{"StringTag":["branch",{"Literal":[9]}]}]}"""
                , Expression.ConditionalInst(
                    condition: Expression.BuiltinInst(
                        function: "equal",
                        input: Expression.EnvironmentInstance),
                    falseBranch: new Expression.Eval(
                        encoded: Expression.LitralInst(
                            StringEncoding.ValueFromString("F")),
                        environment: Expression.ListInst(
                            [Expression.EnvironmentInstance])),
                    trueBranch: new Expression.Label(
                        tag: "branch",
                        tagged: Expression.LitralInst(
                            IntegerEncoding.EncodeSignedInteger(9))))),
            };

        foreach (var testCase in testCases)
        {
            try
            {
                var decoded =
                    EncodePineExpressionAsJson.SingleFromJsonString(testCase.json);

                decoded.Should().Be(testCase.expected);
            }
            catch (System.Exception e)
            {
                throw new System.Exception(
                    "Failed for test case: " + testCase.expected,
                    e);
            }
        }
    }

    [Fact]
    public void Decoding_from_JSON_reuses_common_instances()
    {
        IReadOnlyList<string> testCases =
            [
            """{"Environment":[]}""",

            """{"List":[[]]}""",

            """{"Literal":[[]]}""",

            """{"Literal":[{"BlobAsString":"List"}]}""",

            """{"Literal":[{"BlobAsString":"Literal"}]}""",

            """{"Literal":[{"BlobAsString":"Litral"}]}""",

            """{"Literal":[{"BlobAsString":"Builtin"}]}""",

            """{"KernelApplication":["head",{"Environment":[]}]}""",
            ];

        foreach (var testCase in testCases)
        {
            try
            {
                {
                    var parsedA =
                        EncodePineExpressionAsJson.SingleFromJsonString(testCase);

                    var parsedB =
                        EncodePineExpressionAsJson.SingleFromJsonString(testCase);

                    ReferenceEquals(parsedA, parsedB).Should().BeTrue();
                }

                {
                    var parsedA =
                        EncodePineExpressionAsJson.ListFromJsonString("[" + testCase + "]").Single();

                    var parsedB =
                        EncodePineExpressionAsJson.ListFromJsonString("[" + testCase + "]").Single();

                    ReferenceEquals(parsedA, parsedB).Should().BeTrue();
                }
            }
            catch (System.Exception e)
            {
                throw new System.Exception(
                    "Failed for test case: " + testCase,
                    e);
            }
        }
    }
}
