using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Json;
using Pine.Core.PineVM;
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

    [Fact]
    public void IsKnownBooleanExpression_returns_true_for_Boolean_literals_and_predicate_kernels()
    {
        // Literals: TrueValue / FalseValue
        ReducePineExpression
            .IsKnownBooleanExpression(Expression.LiteralInstance(PineKernelValues.TrueValue))
            .Should().BeTrue();

        ReducePineExpression
            .IsKnownBooleanExpression(Expression.LiteralInstance(PineKernelValues.FalseValue))
            .Should().BeTrue();

        // Predicate kernels
        ReducePineExpression
            .IsKnownBooleanExpression(
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.equal),
                Expression.ListInstance(
                    [
                    Expression.EnvironmentInstance,
                    Expression.LiteralInstance(PineValue.Blob([7])),
                    ])))
            .Should().BeTrue();

        ReducePineExpression
            .IsKnownBooleanExpression(
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.int_is_sorted_asc),
                Expression.LiteralInstance(PineValue.EmptyList)))
            .Should().BeTrue();
    }

    [Fact]
    public void IsKnownBooleanExpression_returns_false_for_environment_and_non_predicate_kernels()
    {
        // Environment access: not provably Boolean.
        ReducePineExpression
            .IsKnownBooleanExpression(Expression.EnvironmentInstance)
            .Should().BeFalse();

        // skip[1, 2] reduces to a literal byte sequence [0x02] whose bytes match
        // PineKernelValues.FalseValue, but the value semantically represents the
        // integer 2, not Boolean false. The literal must therefore not be treated
        // as Boolean here. (Note: a Literal whose Value compares-equal to
        // FalseValue is, however, accepted - that is the only case where the
        // ambiguous bit pattern can safely be treated as Boolean, since the
        // calling reduction is the one that produced the ambiguity.)
        ReducePineExpression
            .IsKnownBooleanExpression(
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.skip),
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(2)),
                    ])))
            .Should().BeFalse();

        // Arithmetic: not Boolean.
        ReducePineExpression
            .IsKnownBooleanExpression(
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.int_add),
                Expression.LiteralInstance(PineValue.EmptyList)))
            .Should().BeFalse();
    }

    [Fact]
    public void Reduce_does_not_flip_branches_when_equal_compares_against_byte_2_with_non_Boolean_operand()
    {
        // Conditional( equal[ skip[1, n], skip[1, 2] ], trueBranch, falseBranch )
        //
        // skip[1, 2] reduces to a literal [0x02] which is byte-identical to
        // PineKernelValues.FalseValue. A naive reducer would treat this as
        // "equal to False" and flip the branches, even though the other operand
        // (skip[1, n]) is a byte sequence that may or may not be Boolean.
        //
        // After the fix, this reduction is gated on the other operand being a
        // known-Boolean expression, so the conditional is preserved.

        var nibble =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.skip),
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                    Expression.EnvironmentInstance,
                    ]));

        var byteLiteralTwo = Expression.LiteralInstance(PineValue.Blob([0x02]));

        var conditional =
            Expression.ConditionalInstance(
                condition:
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.equal),
                    Expression.ListInstance([nibble, byteLiteralTwo])),
                falseBranch: Expression.LiteralInstance(PineValue.Blob([0x07])),
                trueBranch: Expression.LiteralInstance(PineValue.Blob([0x09])));

        var reduced =
            ReducePineExpression.ReduceExpressionBottomUp(conditional, s_parseCache);

        // The conditional must remain a Conditional whose condition is still the
        // equal-application; the branches must be unchanged.
        var reducedConditional = reduced.Should().BeOfType<Expression.Conditional>().Subject;

        reducedConditional.Condition.Should().BeOfType<Expression.KernelApplication>()
            .Which.Function.Should().Be(nameof(KernelFunction.equal));

        reducedConditional.TrueBranch.Should().BeOfType<Expression.Literal>()
            .Which.Value.Should().Be(PineValue.Blob([0x09]));

        reducedConditional.FalseBranch.Should().BeOfType<Expression.Literal>()
            .Which.Value.Should().Be(PineValue.Blob([0x07]));
    }

    private static string SerializeExpressionPretty(Expression expression)
    {
        var options = EncodePineExpressionAsJson.BuildJsonSerializerOptions();
        options.WriteIndented = true;
        return JsonSerializer.Serialize(expression, options);
    }
}
