using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Xunit;

namespace Pine.UnitTests.DotNet;

public class StaticProgramCSharpMethodCSETests
{
    // Helper method to create a simple ignore function that only ignores literals and environment
    private static bool IgnoreSimple<TFuncId>(StaticExpression<TFuncId> expr) =>
        expr is StaticExpression<TFuncId>.Literal or StaticExpression<TFuncId>.Environment;

    #region Trivial Cases

    [Fact]
    public void CollectSubexpressionsToSeparate_returns_empty_for_literal()
    {
        var literal = StaticExpression<string>.LiteralInstance(PineValue.EmptyList);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            literal,
            IgnoreSimple);

        result.Should().BeEmpty();
    }

    [Fact]
    public void CollectSubexpressionsToSeparate_returns_empty_for_environment()
    {
        var env = StaticExpression<string>.EnvironmentInstance;

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            env,
            IgnoreSimple);

        result.Should().BeEmpty();
    }

    [Fact]
    public void CollectSubexpressionsToSeparate_returns_empty_for_single_kernel_application()
    {
        var kernelApp = StaticExpression<string>.KernelApplicationInstance(
            "head",
            StaticExpression<string>.EnvironmentInstance);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            kernelApp,
            IgnoreSimple);

        result.Should().BeEmpty();
    }

    #endregion

    #region Simple Cases

    [Fact]
    public void CollectSubexpressionsToSeparate_returns_empty_for_list_with_distinct_elements()
    {
        // List with two different literals - no common subexpressions
        var list = StaticExpression<string>.ListInstance(
        [
            StaticExpression<string>.LiteralInstance(PineValue.EmptyList),
            StaticExpression<string>.LiteralInstance(PineValue.EmptyBlob)
        ]);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            list,
            IgnoreSimple);

        result.Should().BeEmpty();
    }

    [Fact]
    public void CollectSubexpressionsToSeparate_identifies_repeated_subexpression_in_list()
    {
        // A kernel application that appears twice in the list
        var kernelApp = StaticExpression<string>.KernelApplicationInstance(
            "head",
            StaticExpression<string>.EnvironmentInstance);

        var list = StaticExpression<string>.ListInstance([kernelApp, kernelApp]);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            list,
            IgnoreSimple);

        result.Should().HaveCount(1);
        result.Should().Contain(kernelApp);
    }

    [Fact]
    public void CollectSubexpressionsToSeparate_identifies_nested_repeated_subexpression()
    {
        // Inner expression that will be repeated
        var innerKernelApp = StaticExpression<string>.KernelApplicationInstance(
            "head",
            StaticExpression<string>.EnvironmentInstance);

        // Two outer kernel applications that both use the same inner expression
        var outerKernelApp1 = StaticExpression<string>.KernelApplicationInstance(
            "length",
            innerKernelApp);

        var outerKernelApp2 = StaticExpression<string>.KernelApplicationInstance(
            "head",
            innerKernelApp);

        var list = StaticExpression<string>.ListInstance([outerKernelApp1, outerKernelApp2]);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            list,
            IgnoreSimple);

        // Should identify the inner kernel app as needing CSE
        result.Should().Contain(innerKernelApp);
    }

    #endregion

    #region Simple Conditional Cases

    [Fact]
    public void CollectSubexpressionsToSeparate_returns_empty_for_simple_conditional_no_repeats()
    {
        var condition = StaticExpression<string>.KernelApplicationInstance(
            "head",
            StaticExpression<string>.EnvironmentInstance);

        var trueBranch = StaticExpression<string>.LiteralInstance(
            PineValue.EmptyList);

        var falseBranch = StaticExpression<string>.LiteralInstance(
            PineValue.EmptyBlob);

        var conditional = StaticExpression<string>.ConditionalInstance(
            condition,
            falseBranch,
            trueBranch);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            conditional,
            IgnoreSimple);

        result.Should().BeEmpty();
    }

    [Fact]
    public void CollectSubexpressionsToSeparate_identifies_expression_in_condition_and_branch()
    {
        var sharedExpr =
            StaticExpression<string>.KernelApplicationInstance(
                "head",
                StaticExpression<string>.EnvironmentInstance);

        var trueBranch =
            StaticExpression<string>.KernelApplicationInstance(
                "length",
                sharedExpr);

        var conditional =
            StaticExpression<string>.ConditionalInstance(
                condition: sharedExpr,
                falseBranch: StaticExpression<string>.LiteralInstance(PineValue.EmptyList),
                trueBranch: trueBranch);

        var result =
            StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
                conditional,
                IgnoreSimple);

        // sharedExpr appears once unconditionally (in condition) and once in true branch
        result.Should().Contain(sharedExpr);
    }

    [Fact]
    public void CollectSubexpressionsToSeparate_identifies_expression_in_both_branches()
    {
        var sharedExpr = StaticExpression<string>.KernelApplicationInstance(
            "head",
            StaticExpression<string>.EnvironmentInstance);

        var condition = StaticExpression<string>.KernelApplicationInstance(
            "length",
            StaticExpression<string>.EnvironmentInstance);

        var trueBranch = StaticExpression<string>.KernelApplicationInstance(
            "length",
            sharedExpr);

        var falseBranch = StaticExpression<string>.KernelApplicationInstance(
            "head",
            sharedExpr);

        var conditional = StaticExpression<string>.ConditionalInstance(
            condition,
            falseBranch,
            trueBranch);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            conditional,
            IgnoreSimple);

        // sharedExpr appears in both branches, so it should be treated as unconditional
        // and extracted
        result.Should().Contain(sharedExpr);
    }

    #endregion

    #region Nested Conditional Cases

    [Fact]
    public void CollectSubexpressionsToSeparate_handles_nested_conditional_with_shared_expression()
    {
        // This is the key test case mentioned in the problem statement:
        // An expression used in both branches of a nested conditional
        // should be recognized as common to all branches and not returned multiple times

        var sharedExpr = StaticExpression<string>.KernelApplicationInstance(
            "head",
            StaticExpression<string>.EnvironmentInstance);

        var innerCondition = StaticExpression<string>.KernelApplicationInstance(
            "length",
            StaticExpression<string>.EnvironmentInstance);

        // Inner conditional uses sharedExpr in both branches
        var innerConditional = StaticExpression<string>.ConditionalInstance(
            innerCondition,
            falseBranch: sharedExpr,  // sharedExpr in false branch
            trueBranch: sharedExpr);  // sharedExpr in true branch

        // Outer conditional
        var outerCondition = StaticExpression<string>.KernelApplicationInstance(
            "head",
            StaticExpression<string>.EnvironmentInstance);

        var outerConditional = StaticExpression<string>.ConditionalInstance(
            outerCondition,
            falseBranch: innerConditional,
            trueBranch: StaticExpression<string>.LiteralInstance(PineValue.EmptyList));

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            outerConditional,
            IgnoreSimple);

        // Since sharedExpr appears in both branches of the inner conditional,
        // it should be treated as effectively unconditional relative to that conditional
        // and should be extracted
        result.Should().Contain(sharedExpr);
    }

    [Fact]
    public void CollectSubexpressionsToSeparate_minimal_set_nested_conditional_both_branches()
    {
        // Expression that appears in both branches of the nested conditional
        var commonExpr = StaticExpression<string>.KernelApplicationInstance(
            "head",
            StaticExpression<string>.EnvironmentInstance);

        // Different expressions for different branches
        var trueBranchExpr = StaticExpression<string>.KernelApplicationInstance(
            "length",
            commonExpr);

        var falseBranchExpr = StaticExpression<string>.KernelApplicationInstance(
            "skip",
            commonExpr);

        // Inner conditional with common expression in both branches
        var innerCondition = StaticExpression<string>.LiteralInstance(PineValue.EmptyList);
        var innerConditional = StaticExpression<string>.ConditionalInstance(
            innerCondition,
            falseBranch: falseBranchExpr,
            trueBranch: trueBranchExpr);

        // Outer conditional
        var outerCondition = StaticExpression<string>.LiteralInstance(PineValue.EmptyBlob);
        var outerConditional = StaticExpression<string>.ConditionalInstance(
            outerCondition,
            falseBranch: innerConditional,
            trueBranch: innerConditional);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            outerConditional,
            IgnoreSimple);

        // commonExpr appears in both branches of inner conditional (via trueBranchExpr and falseBranchExpr)
        // and the inner conditional appears in both branches of outer conditional
        // So commonExpr should be extracted
        result.Should().Contain(commonExpr);

        // The inner conditional should also be extracted since it appears twice
        result.Should().Contain(innerConditional);
    }

    [Fact]
    public void CollectSubexpressionsToSeparate_deeply_nested_conditional_returns_minimal_set()
    {
        // Base expression used everywhere
        var baseExpr = StaticExpression<string>.KernelApplicationInstance(
            "head",
            StaticExpression<string>.EnvironmentInstance);

        // Build a deeply nested structure where baseExpr appears in all leaf branches
        var level1True = StaticExpression<string>.KernelApplicationInstance("length", baseExpr);
        var level1False = StaticExpression<string>.KernelApplicationInstance("skip", baseExpr);

        var level1Cond = StaticExpression<string>.ConditionalInstance(
            StaticExpression<string>.LiteralInstance(PineValue.EmptyList),
            falseBranch: level1False,
            trueBranch: level1True);

        var level2True = StaticExpression<string>.KernelApplicationInstance("reverse", baseExpr);
        var level2Cond = StaticExpression<string>.ConditionalInstance(
            StaticExpression<string>.LiteralInstance(PineValue.EmptyBlob),
            falseBranch: level1Cond,
            trueBranch: level2True);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            level2Cond,
            IgnoreSimple);

        // baseExpr should be identified as it appears in all branches
        result.Should().Contain(baseExpr);
    }

    [Fact]
    public void CollectSubexpressionsToSeparate_conditional_with_expression_only_in_one_branch()
    {
        var exprOnlyInTrue = StaticExpression<string>.KernelApplicationInstance(
            "head",
            StaticExpression<string>.EnvironmentInstance);

        var trueBranch = StaticExpression<string>.ListInstance(
            [exprOnlyInTrue, exprOnlyInTrue]); // Used twice in true branch

        var falseBranch = StaticExpression<string>.LiteralInstance(PineValue.EmptyList);

        var conditional = StaticExpression<string>.ConditionalInstance(
            StaticExpression<string>.EnvironmentInstance,
            falseBranch,
            trueBranch);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            conditional,
            IgnoreSimple);

        // Expression appears twice but only in one branch (conditional context)
        // Current implementation does NOT extract it because it never appears unconditionally
        // This is correct behavior - we only extract when seen unconditionally at least once
        result.Should().BeEmpty();
    }

    #endregion

    #region Edge Cases and Complex Scenarios

    [Fact]
    public void CollectSubexpressionsToSeparate_handles_function_application()
    {
        var args = StaticExpression<string>.ListInstance(
        [
            StaticExpression<string>.EnvironmentInstance,
            StaticExpression<string>.EnvironmentInstance
        ]);

        var funcApp = StaticExpression<string>.FunctionApplicationInstance(
            "myFunction",
            args);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            funcApp,
            IgnoreSimple);

        // No repeated subexpressions (environment is ignored)
        result.Should().BeEmpty();
    }

    [Fact]
    public void CollectSubexpressionsToSeparate_complex_nested_lists_with_shared_expressions()
    {
        var sharedExpr =
            StaticExpression<string>.KernelApplicationInstance(
                "head",
                StaticExpression<string>.EnvironmentInstance);

        // Reuse the same innerList object to test CSE extraction
        var innerList = StaticExpression<string>.ListInstance([sharedExpr]);

        var outerList = StaticExpression<string>.ListInstance([innerList, innerList]);

        var result =
            StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
                outerList,
                IgnoreSimple);

        // innerList appears twice unconditionally, so it should be extracted
        result.Should().Contain(innerList);
    }

    [Fact]
    public void CollectSubexpressionsToSeparate_multiple_levels_of_repeated_expressions()
    {
        // Build a hierarchy: expr1 -> expr2 -> expr3
        // Where expr3 is used multiple times
        var expr3 = StaticExpression<string>.KernelApplicationInstance(
            "head",
            StaticExpression<string>.EnvironmentInstance);

        var expr2a = StaticExpression<string>.KernelApplicationInstance("length", expr3);
        var expr2b = StaticExpression<string>.KernelApplicationInstance("skip", expr3);

        var expr1 = StaticExpression<string>.ListInstance([expr2a, expr2b]);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            expr1,
            IgnoreSimple);

        // expr3 appears twice unconditionally (through expr2a and expr2b)
        result.Should().Contain(expr3);
    }

    [Fact]
    public void CollectSubexpressionsToSeparate_returns_minimal_set_no_unnecessary_extractions()
    {
        // This test ensures we don't extract expressions that don't need CSE
        var expr1 = StaticExpression<string>.KernelApplicationInstance(
            "head",
            StaticExpression<string>.EnvironmentInstance);

        var expr2 = StaticExpression<string>.KernelApplicationInstance(
            "length",
            StaticExpression<string>.EnvironmentInstance);

        var list = StaticExpression<string>.ListInstance([expr1, expr2]);

        var result = StaticProgramCSharpMethodCSE.CollectSubexpressionsToSeparate(
            list,
            IgnoreSimple);

        // No repeated expressions, so nothing should be extracted
        result.Should().BeEmpty();
    }

    #endregion
}
