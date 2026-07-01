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
                , Expression.ListInstance([])),

                // Literal expression with an integer value.
                ("""{"Literal":[42]}"""
                , Expression.LiteralInstance(
                    IntegerEncoding.EncodeSignedInteger(42))),

                // Literal expression with a string value.
                ("""{"Literal":[{"BlobAsString":"Hello"}]}"""
                , Expression.LiteralInstance(
                    StringEncoding.ValueFromString("Hello"))),

                // Literal expression with an empty list value.
                ("""{"Literal":[[]]}"""
                , Expression.LiteralInstance(PineValue.EmptyList)),

                // List expression with multiple items.
                ("""{"List":[[{"Environment":[]},{"Literal":[1]}]]}"""
                , Expression.ListInstance(
                    [Expression.EnvironmentInstance,
                     Expression.LiteralInstance(
                        IntegerEncoding.EncodeSignedInteger(1))])),

                // Parse-and-eval expression.
                ("""{"ParseAndEval":[{"Literal":[7]},{"Environment":[]}]}"""
                , new Expression.ParseAndEval(
                    encoded: Expression.LiteralInstance(
                        IntegerEncoding.EncodeSignedInteger(7)),
                    environment: Expression.EnvironmentInstance)),

                // Kernel application expression.
                ("""{"KernelApplication":["head",{"Environment":[]}]}"""
                , Expression.KernelApplicationInstance(
                    function: "head",
                    input: Expression.EnvironmentInstance)),

                // Conditional expression.
                ("""{"Conditional":[{"Environment":[]},{"Literal":[0]},{"Literal":[1]}]}"""
                , Expression.ConditionalInstance(
                    condition: Expression.EnvironmentInstance,
                    falseBranch: Expression.LiteralInstance(
                        IntegerEncoding.EncodeSignedInteger(0)),
                    trueBranch: Expression.LiteralInstance(
                        IntegerEncoding.EncodeSignedInteger(1)))),

                // String tag expression.
                ("""{"StringTag":["my-tag",{"Environment":[]}]}"""
                , new Expression.StringTag(
                    tag: "my-tag",
                    tagged: Expression.EnvironmentInstance)),

                // Nested: kernel application with a list of subexpressions.
                ("""{"KernelApplication":["equal",{"List":[[{"KernelApplication":["head",{"Environment":[]}]},{"Literal":[4]}]]}]}"""
                , Expression.KernelApplicationInstance(
                    function: "equal",
                    input: Expression.ListInstance(
                        [Expression.KernelApplicationInstance(
                            function: "head",
                            input: Expression.EnvironmentInstance),
                         Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(4))]))),

                // Deeply nested: conditional combining kernel application, parse-and-eval and string tag.
                ("""{"Conditional":[{"KernelApplication":["equal",{"Environment":[]}]},{"ParseAndEval":[{"Literal":[{"BlobAsString":"F"}]},{"List":[[{"Environment":[]}]]}]},{"StringTag":["branch",{"Literal":[9]}]}]}"""
                , Expression.ConditionalInstance(
                    condition: Expression.KernelApplicationInstance(
                        function: "equal",
                        input: Expression.EnvironmentInstance),
                    falseBranch: new Expression.ParseAndEval(
                        encoded: Expression.LiteralInstance(
                            StringEncoding.ValueFromString("F")),
                        environment: Expression.ListInstance(
                            [Expression.EnvironmentInstance])),
                    trueBranch: new Expression.StringTag(
                        tag: "branch",
                        tagged: Expression.LiteralInstance(
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
