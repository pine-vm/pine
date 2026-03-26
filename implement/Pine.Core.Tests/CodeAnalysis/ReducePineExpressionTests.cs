using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Json;
using System.IO;
using System.Linq;
using System.Text.Json;
using Xunit;

namespace Pine.Core.Tests.CodeAnalysis;

public class ReducePineExpressionTests
{
    private static readonly PineVMParseCache s_parseCache = new();

    [Fact]
    public void TryEvaluateExpressionIndependent_EvaluatesLiteral_ListAndLengthKernel()
    {
        // literal
        var lit = Expression.LiteralInstance(PineValue.Blob([1, 2]));

        ReducePineExpression.TryEvaluateExpressionIndependent(lit, s_parseCache)
            .IsOkOrNull().Should().Be(PineValue.Blob([1, 2]));

        // list of literals
        var list =
            Expression.ListInstance(
                [
                Expression.LiteralInstance(PineValue.Blob([3])),
                Expression.LiteralInstance(PineValue.EmptyList)
                ]);

        var evalList = ReducePineExpression.TryEvaluateExpressionIndependent(list, s_parseCache).IsOkOrNull();

        (evalList is PineValue.ListValue).Should().BeTrue();

        var evalListItems = ((PineValue.ListValue)evalList!).Items;

        evalListItems.Length.Should().Be(2);

        // kernel length([x,y]) -> 2
        var lengthOfList =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.length),
                list);

        var lengthValue =
            ReducePineExpression.TryEvaluateExpressionIndependent(lengthOfList, s_parseCache).IsOkOrNull();

        lengthValue.Should().Be(IntegerEncoding.EncodeSignedInteger(2));
    }

    [Fact]
    public void Reduce_expression_bottom_up_from_files()
    {
        var results =
            TestResultSummary.RunFileBasedTestCases(
                "ReduceExpressionBottomUp",
                caseDir =>
                {
                    var expressionJson = File.ReadAllText(Path.Combine(caseDir, "expression.json"));
                    var expression = EncodePineExpressionAsJson.SingleFromJsonString(expressionJson);

                    var expectedReducedJson =
                        File.ReadAllText(Path.Combine(caseDir, "expression-reduced.json")).TrimEnd();

                    var reduced =
                        ReducePineExpression.ReduceExpressionBottomUp(expression, s_parseCache);

                    var reducedJson = SerializeExpressionPretty(reduced).TrimEnd();

                    return (expected: expectedReducedJson, actual: reducedJson);
                },
                trimWhitespace: true);

        var summary = TestResultSummary.RenderSummary(results);

        results.Where(r => !r.Passed).Should().BeEmpty(summary);
    }

    [Fact]
    public void TryEvaluateExpressionIndependent_ParseAndEval_WithLiteralArgs_EvaluatesInnerExpression()
    {
        // Build encoded expression value representing a simple literal
        var innerLiteral = Expression.LiteralInstance(PineValue.Blob([42]));
        var encodedValue = ExpressionEncoding.EncodeExpressionAsValue(innerLiteral);

        var parseAndEval =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(encodedValue),
                environment: Expression.LiteralInstance(PineValue.EmptyList));

        var result = ReducePineExpression.TryEvaluateExpressionIndependent(parseAndEval, s_parseCache);

        result.IsOkOrNull().Should().Be(PineValue.Blob([42]));
    }

    [Fact]
    public void Transform_ReplacingEnvironmentWithLiteral_RemovesEnvAndReportsNoReference()
    {
        var expr =
            Expression.ListInstance(
                [
                Expression.EnvironmentInstance,

                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.head),
                    Expression.LiteralInstance(
                        PineValue.List(
                            [
                            PineValue.Blob([1])
                            ])))
                ]);

        var (mapped, referencesOriginalEnv) =
            ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                e => e is Expression.Environment ? Expression.LiteralInstance(PineValue.EmptyList) : null,
                expr);

        referencesOriginalEnv.Should().BeFalse();

        // Ensure no Environment nodes remain
        foreach (var node in Expression.EnumerateSelfAndDescendants(mapped))
        {
            (node is Expression.Environment).Should().BeFalse();
        }
    }

    [Fact]
    public void TryInferListLengthLowerBounds_SkipConst_OnLiteralList_ReturnsLowerBound()
    {
        var list =
            Expression.LiteralInstance(
                PineValue.List(
                    [
                    PineValue.Blob([1]),
                    PineValue.Blob([2]),
                    PineValue.Blob([3]),
                    PineValue.Blob([4]),
                    PineValue.Blob([5])
                    ]));

        var skip =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.skip),
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(2)),
                    list
                    ]));

        var dummyClass = PineValueClass.CreateEquals(PineValue.EmptyList);

        var bounds = ReducePineExpression.EnumerateInferListLengthBounds(skip, dummyClass, s_parseCache).ToArray();

        bounds.Select(b => b.lower).Should().Contain(3);
    }

    private static string SerializeExpressionPretty(Expression expression)
    {
        var options = EncodePineExpressionAsJson.BuildJsonSerializerOptions();
        options.WriteIndented = true;
        return JsonSerializer.Serialize(expression, options);
    }
}
