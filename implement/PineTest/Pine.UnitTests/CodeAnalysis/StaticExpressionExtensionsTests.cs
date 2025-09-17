using Pine.Core;
using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using System.Linq;
using AwesomeAssertions;
using Xunit;

namespace Pine.UnitTests.CodeAnalysis;

public class StaticExpressionExtensionsTests
{
    [Fact]
    public void TransformStaticExpression_identity_no_replacement_keeps_tree_and_reports_env_presence()
    {
        // Build a tree that contains an Environment node in the true branch.
        var expr =
            StaticExpression<string>.ConditionalInstance(
                condition: StaticExpression<string>.KernelApplicationInstance(
                    nameof(KernelFunction.length),
                    StaticExpression<string>.ListInstance([])),
                falseBranch: StaticExpression<string>.FunctionApplicationInstance(
                    "F",
                    StaticExpression<string>.ListInstance([])),
                trueBranch: StaticExpression<string>.ListInstance([
                    StaticExpression<string>.EnvironmentInstance,
                    StaticExpression<string>.LiteralInstance(PineValue.EmptyBlob)
                ]));

        var (mapped, referencesOriginalEnv) =
            StaticExpressionExtension.TransformStaticExpressionWithOptionalReplacement(
                findReplacement: _ => null,
                expression: expr);

        expr.Equals(mapped).Should().BeTrue();

        // Compute expected env presence in the original tree (there was one Environment node in true branch)
        var expectedEnvPresence =
            expr.EnumerateAllDescendants(skipDescendants: null)
                .OfType<StaticExpression<string>.Environment>()
                .Any();

        referencesOriginalEnv.Should().Be(expectedEnvPresence);
        expectedEnvPresence.Should().BeTrue(); // sanity: this tree should reference env
    }

    [Fact]
    public void TransformStaticExpression_replace_environment_with_literal_removes_original_env_reference()
    {
        var expr =
            StaticExpression<string>.ListInstance([
                StaticExpression<string>.EnvironmentInstance,
                StaticExpression<string>.KernelApplicationInstance(
                    nameof(KernelFunction.head),
                    StaticExpression<string>.ListInstance([]))
            ]);

        var (mapped, referencesOriginalEnv) =
            StaticExpressionExtension.TransformStaticExpressionWithOptionalReplacement(
                findReplacement: e => e is StaticExpression<string>.Environment
                    ? StaticExpression<string>.LiteralInstance(PineValue.EmptyList)
                    : null,
                expression: expr);

        referencesOriginalEnv.Should().BeFalse();

        // Ensure no original Environment nodes remain in the transformed tree
        mapped.EnumerateAllDescendants(skipDescendants: null)
            .OfType<StaticExpression<string>.Environment>()
            .Any()
            .Should().BeFalse();
    }

    [Fact]
    public void TransformStaticExpression_replace_literal_with_environment_does_not_count_as_original()
    {
        // Original tree contains no environment
        var expr =
            StaticExpression<string>.ListInstance([
                StaticExpression<string>.LiteralInstance(PineValue.EmptyList),
                StaticExpression<string>.KernelApplicationInstance(
                    nameof(KernelFunction.length),
                    StaticExpression<string>.LiteralInstance(PineValue.EmptyList))
            ]);

        var (mapped, referencesOriginalEnv) =
            StaticExpressionExtension.TransformStaticExpressionWithOptionalReplacement(
                findReplacement: e => e is StaticExpression<string>.Literal
                    ? StaticExpression<string>.EnvironmentInstance
                    : null,
                expression: expr);

        // Although replacements introduced Environment nodes, they do not reference the original env per contract
        referencesOriginalEnv.Should().BeFalse();
        mapped.EnumerateAllDescendants(skipDescendants: null)
            .OfType<StaticExpression<string>.Environment>()
            .Any()
            .Should().BeTrue();
    }

    [Fact]
    public void TransformStaticExpression_replacement_short_circuits_descendants()
    {
        var env = StaticExpression<string>.EnvironmentInstance;
        var innerLit = StaticExpression<string>.LiteralInstance(PineValue.EmptyList);
        var innerList = StaticExpression<string>.ListInstance([env, innerLit]);

        var root = StaticExpression<string>.ListInstance([
            innerList,
            StaticExpression<string>.LiteralInstance(PineValue.EmptyBlob)
        ]);

        var visited = new HashSet<StaticExpression<string>>();

        var replacementTarget = innerList;

        var (mapped, _) =
            StaticExpressionExtension.TransformStaticExpressionWithOptionalReplacement(
                findReplacement: e =>
                {
                    visited.Add(e);

                    if (e.Equals(replacementTarget))
                    {
                        return StaticExpression<string>.LiteralInstance(PineValue.Blob([1, 2]));
                    }

                    return null;
                },
                expression: root);

        // Ensure we visited the root and the target, but not the target's children
        visited.Should().Contain(root);
        visited.Should().Contain(innerList);
        visited.Should().NotContain(env);
        visited.Should().NotContain(innerLit);

        // Ensure mapping actually replaced the inner list with a literal at position 0
        (mapped is StaticExpression<string>.List).Should().BeTrue();

        var mappedList = (StaticExpression<string>.List)mapped;

        (mappedList.Items[0] is StaticExpression<string>.Literal).Should().BeTrue();
    }

    [Fact]
    public void TransformStaticExpression_crashing_parse_and_eval_children_are_transformed_and_flags_aggregated()
    {
        var expr = new StaticExpression<string>.CrashingParseAndEval(
            Encoded: StaticExpression<string>.ListInstance([
                StaticExpression<string>.EnvironmentInstance,
                StaticExpression<string>.LiteralInstance(PineValue.EmptyBlob)
            ]),
            EnvironmentExpr: StaticExpression<string>.ListInstance([
                StaticExpression<string>.LiteralInstance(PineValue.EmptyList)
            ]));

        // First, no replacement: should report env presence from Encoded branch
        var (_, referencesOriginalEnv1) =
            StaticExpressionExtension.TransformStaticExpressionWithOptionalReplacement(
                _ => null,
                expr);

        referencesOriginalEnv1.Should().BeTrue();

        // Replace environment nodes with literal: should remove original env reference
        var (_, referencesOriginalEnv2) =
            StaticExpressionExtension.TransformStaticExpressionWithOptionalReplacement(
                e => e is StaticExpression<string>.Environment
                    ? StaticExpression<string>.LiteralInstance(PineValue.EmptyList)
                    : null,
                expr);

        referencesOriginalEnv2.Should().BeFalse();
    }
}
