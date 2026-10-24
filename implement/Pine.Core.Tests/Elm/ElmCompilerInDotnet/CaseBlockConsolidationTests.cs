using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections.Immutable;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Targeted tests for <see cref="CaseBlockConsolidation"/>: the two
/// local case-block consolidation rewrites described in
/// <c>explore/internal-analysis/2026-05-20-elm-syntax-case-block-consolidation.md</c>.
///
/// <para>
/// Each test parses a small Elm function body, applies
/// <see cref="CaseBlockConsolidation.RewriteExpression"/> to
/// the outermost <see cref="Expression.CaseExpression"/>,
/// and asserts a structural property of the result (e.g. the outer
/// case is gone; the if-chain has absorbed the arms).
/// </para>
/// </summary>
public class CaseBlockConsolidationTests
{
    // =========================================================================
    // Rewrite 1 — Conditional duplication of case arm by leaf shape.
    // =========================================================================

    [Fact]
    public void ConditionalDuplication_two_branch_if_collapses_outer_case()
    {
        // Outer case scrutinee is `if b then Just x else Nothing`; both
        // leaves are statically tagged, the case has one arm per tag,
        // and the arms are tiny. The rewrite should fold the case into
        // the if-chain.
        const string ModuleText =
            """
            module Test exposing (..)


            foo b x s =
                case (if b then Just x else Nothing) of
                    Just y ->
                        plus y s

                    Nothing ->
                        s
            """;

        var (rewritten, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeTrue("the if-chain scrutinee + non-overlapping tag arms should trigger consolidation");

        // After consolidation the top-level expression should be an
        // IfBlock — the outer case is gone.
        rewritten.Should().BeOfType<Expression.IfBlock>();

        var original = (Expression.CaseExpression)ParseFunctionByName(ModuleText, "foo").Expression;
        var originalIf = (Expression.IfBlock)original.Expression;

        rewritten.Should().Be(
            new Expression.IfBlock(
                originalIf.Condition,
                new Expression.LetExpression(
                    [new LetDeclaration.LetDestructuring(original.Cases[0].Pattern, originalIf.ThenBlock)],
                    original.Cases[0].Expression),
                original.Cases[1].Expression));
    }

    [Fact]
    public void ConditionalDuplication_else_if_chain_duplicates_arm()
    {
        // The `Just` arm is referenced from two branches (then and the
        // first else-if then), exercising the duplication path.
        const string ModuleText =
            """
            module Test exposing (..)


            foo b1 b2 x s =
                case (if b1 then Just x else if b2 then Just s else Nothing) of
                    Just y ->
                        plus y 1

                    Nothing ->
                        0
            """;

        var (rewritten, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeTrue();
        rewritten.Should().BeOfType<Expression.IfBlock>();
    }

    [Fact]
    public void ConditionalDuplication_wildcard_before_match_blocks_rewrite()
    {
        // A wildcard arm BEFORE the predicted-tag arm short-circuits
        // the match in Elm's first-match semantics — consolidation
        // must not fire.
        const string ModuleText =
            """
            module Test exposing (..)


            foo b x =
                case (if b then Just x else Nothing) of
                    _ ->
                        0

                    Just _ ->
                        1

                    Nothing ->
                        2
            """;

        var (_, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeFalse("a wildcard arm before the predicted tag arms blocks the rewrite");
    }

    [Fact]
    public void ConditionalDuplication_arm_too_large_blocks_rewrite()
    {
        // The `Just` arm is huge — well beyond the conservative
        // MaxArmBodyNodeCount budget — so consolidation must not fire.
        const string ModuleText =
            """
            module Test exposing (..)


            foo b x =
                case (if b then Just x else Nothing) of
                    Just y ->
                        plus (plus (plus (plus (plus (plus (plus (plus y y) y) y) y) y) y) y) y

                    Nothing ->
                        0
            """;

        var (_, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeFalse();
    }

    [Fact]
    public void ConditionalDuplication_mismatched_leaf_blocks_rewrite()
    {
        // One leaf is a call to a lowercase name — tag prediction fails
        // on that leaf and the rewrite must not fire.
        const string ModuleText =
            """
            module Test exposing (..)


            foo b x =
                case (if b then Just x else identity x) of
                    Just _ ->
                        0

                    Nothing ->
                        1
            """;

        var (_, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeFalse();
    }

    [Fact]
    public void ConditionalDuplication_zero_arity_arm_body_inlined_without_destructure()
    {
        // The `Nothing` arm has a zero-arity constructor pattern, so
        // no destructure should be emitted at the leaf — only the
        // arm body.
        const string ModuleText =
            """
            module Test exposing (..)


            foo b s =
                case (if b then Just s else Nothing) of
                    Just _ ->
                        1

                    Nothing ->
                        0
            """;

        var (rewritten, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeTrue();
        rewritten.Should().BeOfType<Expression.IfBlock>();

        // Walk into the else branch — should be the integer literal `0`
        // unchanged (no let-destructure wrapper, since the Nothing
        // pattern binds no variables).
        var ifBlock = (Expression.IfBlock)rewritten;
        var elseValue = ifBlock.ElseBlock;

        // After peeling, must be a non-let expression — no destructure was emitted.
        elseValue.Should().NotBeOfType<Expression.LetExpression>();
    }

    [Fact]
    public void ConditionalDuplication_zero_arity_aliased_arm_preserves_alias_binding()
    {
        const string ModuleText =
            """
            module Test exposing (..)

            foo b x =
                case (if b then Just x else Nothing) of
                    Just _ ->
                        Nothing

                    Nothing as alias ->
                        alias
            """;

        var (rewritten, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeTrue();

        var ifBlock = (Expression.IfBlock)rewritten;
        var elseBody = (Expression.LetExpression)ifBlock.ElseBlock;
        var original = (Expression.CaseExpression)ParseFunctionByName(ModuleText, "foo").Expression;
        var originalIf = (Expression.IfBlock)original.Expression;

        elseBody.Should().Be(
            new Expression.LetExpression(
                [new LetDeclaration.LetDestructuring(original.Cases[1].Pattern, originalIf.ElseBlock)],
                original.Cases[1].Expression));

        SyntaxAnalysis.CollectRemainingFreeVariables(elseBody).Should().NotContain("alias");
    }

    // =========================================================================
    // Rewrite 2 — Deduct tag constraint via alias pattern (case-of-case).
    // =========================================================================

    [Fact]
    public void NestedCaseOfCase_alias_pattern_consolidates_into_inner_case()
    {
        // Outer case has Good / Bad arms. Inner case has
        // `(Good _ _) as good -> good` (alias-pinned tag) and a direct
        // `Bad _ x -> Bad True x` arm. The two cases should collapse
        // into the inner case with each arm absorbing its outer body.
        const string ModuleText =
            """
            module Test exposing (..)


            foo simplified =
                case
                    case simplified of
                        (Good a b) as good ->
                            good

                        Bad e x ->
                            Bad e x
                of
                    (Good p q) as g ->
                        wrap g

                    Bad r s ->
                        Bad r s
            """;

        var (rewritten, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeTrue();

        // After consolidation the top-level expression is a single case.
        rewritten.Should().BeOfType<Expression.CaseExpression>();

        var consolidated = (Expression.CaseExpression)rewritten;
        consolidated.Cases.Count.Should().Be(2);

        var original = (Expression.CaseExpression)ParseFunctionByName(ModuleText, "foo").Expression;
        var inner = (Expression.CaseExpression)original.Expression;

        consolidated.Expression.Should().Be(inner.Expression);

        for (var i = 0; i < inner.Cases.Count; i++)
        {
            consolidated.Cases[i].Pattern.Should().Be(inner.Cases[i].Pattern);

            consolidated.Cases[i].Expression.Should().Be(
                new Expression.LetExpression(
                    [
                    new LetDeclaration.LetDestructuring(
                        original.Cases[i].Pattern,
                        inner.Cases[i].Expression)
                    ],
                    original.Cases[i].Expression));
        }
    }

    [Fact]
    public void NestedCaseOfCase_outer_let_preserved_as_wrapper()
    {
        // The outer scrutinee is `let s1_0 = ... in case ...`. After
        // consolidation, the `let s1_0 = ... in` wrapper must remain
        // around the consolidated case expression.
        const string ModuleText =
            """
            module Test exposing (..)


            foo simplified offset =
                case
                    let
                        s1_0 = offset
                    in
                    case simplified of
                        (Good a b) as good ->
                            good

                        Bad e x ->
                            Bad e x
                of
                    Good p q ->
                        s1_0

                    Bad r s ->
                        Bad r s
            """;

        var (rewritten, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeTrue();

        // Top-level must be a LetExpression — the wrapper survived.
        rewritten.Should().BeOfType<Expression.LetExpression>();

        var letExpr = (Expression.LetExpression)rewritten;

        // The let body should now be a single CaseExpression
        // (the consolidated case).
        letExpr.Expression.Should().BeOfType<Expression.CaseExpression>();
    }

    [Fact]
    public void NestedCaseOfCase_outer_pattern_shadows_inner_pattern_blocks_rewrite()
    {
        // Regression test for the shadowing crash discovered by
        // `References_request_finds_usage_across_modules`: when the
        // outer-arm pattern binds the same names as the inner-arm
        // pattern (here `e` and `x` in both), naively emitting
        // `let (Bad e x) = Bad e x in Bad e x` shadows the inner
        // pattern's `e`/`x` bindings. The pipeline's
        // `CheckForNamingErrors` validation rejects shadowing, so
        // the rewrite is gated to skip these shapes.
        const string ModuleText =
            """
            module Test exposing (..)


            foo simplified =
                case
                    case simplified of
                        (Good a b) as good ->
                            good

                        Bad e x ->
                            Bad e x
                of
                    Good p q ->
                        wrap p

                    Bad e x ->
                        Bad e x
            """;

        var (_, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeFalse(
            "the outer Bad arm binds `e`/`x` which already exist in the inner Bad arm's scope; " +
            "consolidating would shadow them and trip CheckForNamingErrors");
    }

    [Fact]
    public void NestedCaseOfCase_duplication_required_blocks_rewrite()
    {
        // Two inner arms both produce `Good`, so consolidating would
        // require duplicating the outer `Good` arm — this rewrite is
        // explicitly scoped to no-duplication consolidations.
        const string ModuleText =
            """
            module Test exposing (..)


            foo simplified =
                case
                    case simplified of
                        (Good a b) as good ->
                            good

                        Bad e x ->
                            Good 0 0
                of
                    Good a b ->
                        wrap a

                    Bad e x ->
                        Bad e x
            """;

        var (_, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeFalse(
            "this rewrite does not duplicate outer arms across multiple inner-arm tags");
    }

    [Fact]
    public void NestedCaseOfCase_unpredictable_inner_arm_blocks_rewrite()
    {
        // One inner arm body references a let-bound name with no
        // statically inferable tag — consolidation must not fire.
        const string ModuleText =
            """
            module Test exposing (..)


            foo simplified =
                case
                    case simplified of
                        Good a b ->
                            opaque

                        Bad e x ->
                            Bad e x
                of
                    Good a b ->
                        a

                    Bad e x ->
                        x
            """;

        var (_, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeFalse();
    }

    [Theory]
    [InlineData("Just y")]
    [InlineData("let y = x in Just y")]
    public void ConditionalDuplication_pattern_binding_clashes_block_rewrite(string leaf)
    {
        var moduleText =
            $$"""
            module Test exposing (..)

            foo b x y =
                case (if b then {{leaf}} else Nothing) of
                    Just y ->
                        plus y 1

                    Nothing ->
                        0
            """;

        var (rewritten, fired) = TryConsolidateBodyOf(moduleText, "foo");

        fired.Should().BeFalse();
        rewritten.Should().Be(ParseFunctionByName(moduleText, "foo").Expression);
    }

    [Fact]
    public void ConditionalDuplication_leaf_let_binding_preserved_in_destructure()
    {
        const string ModuleText =
            """
            module Test exposing (..)

            foo b x =
                case (if b then let z = x in Just z else Nothing) of
                    Just y ->
                        plus y 1

                    Nothing ->
                        0
            """;

        var (rewritten, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeTrue();

        var ifBlock = (Expression.IfBlock)rewritten;
        var armBody = (Expression.LetExpression)ifBlock.ThenBlock;
        var destructuring = (LetDeclaration.LetDestructuring)armBody.Declarations[0];

        destructuring.Expression.Should().BeOfType<Expression.LetExpression>();

        destructuring.Expression.Should().Be(
            ((Expression.IfBlock)
            ((Expression.CaseExpression)ParseFunctionByName(ModuleText, "foo").Expression)
            .Expression).ThenBlock);
    }

    [Fact]
    public void ConditionalDuplication_total_duplication_budget_blocks_rewrite()
    {
        const string ModuleText =
            """
            module Test exposing (..)

            foo b1 b2 x =
                case (if b1 then Just x else if b2 then Just 1 else Nothing) of
                    Just y ->
                        plus y 1

                    Nothing ->
                        0
            """;

        var body = ParseFunctionByName(ModuleText, "foo").Expression;

        var rewritten =
            CaseBlockConsolidation.RewriteExpression(
                body,
                new CaseBlockConsolidation.CaseConsolidationConfig(
                    MaxArmBodyNodeCount: 16,
                    MaxTotalDuplicatedNodes: 5));

        rewritten.Should().BeSameAs(body);
    }

    [Fact]
    public void ConditionalDuplication_duplicate_constructor_arm_uses_first_match()
    {
        const string ModuleText =
            """
            module Test exposing (..)

            foo b x =
                case (if b then Just x else Nothing) of
                    Just _ ->
                        1

                    Just _ ->
                        2

                    Nothing ->
                        0
            """;

        var (rewritten, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeTrue();

        var ifBlock = (Expression.IfBlock)rewritten;
        var thenBody = (Expression.LetExpression)ifBlock.ThenBlock;
        var original = (Expression.CaseExpression)ParseFunctionByName(ModuleText, "foo").Expression;

        thenBody.Expression.Should().Be(original.Cases[0].Expression);
    }

    [Fact]
    public void ConditionalDuplication_constructor_module_mismatch_blocks_rewrite()
    {
        const string ModuleText =
            """
            module Test exposing (..)

            foo b x =
                case (if b then First.Just x else Nothing) of
                    Second.Just y ->
                        y

                    Nothing ->
                        0
            """;

        var (_, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeFalse();
    }

    [Fact]
    public void NestedCaseOfCase_outer_pattern_clashes_with_inner_let_blocks_rewrite()
    {
        const string ModuleText =
            """
            module Test exposing (..)

            foo simplified =
                case
                    case simplified of
                        Good a ->
                            let p = a in Good p

                        Bad x ->
                            Bad x
                of
                    Good p ->
                        p

                    Bad q ->
                        q
            """;

        var (_, fired) = TryConsolidateBodyOf(ModuleText, "foo");

        fired.Should().BeFalse();
    }

    [Fact]
    public void RewriteDeclarationDictionary_rewrites_abstract_declarations_directly()
    {
        const string ModuleText =
            """
            module Test exposing (..)

            foo b =
                case (if b then Good else Bad) of
                    Good ->
                        1

                    Bad ->
                        0
            """;

        var name = DeclQualifiedName.Create(["Test"], "foo");

        var declaration =
            new Declaration.FunctionDeclaration(
                new FunctionStruct(
                    Signature: null,
                    Declaration: ParseFunctionByName(ModuleText, "foo")));

        var declarations =
            ImmutableDictionary<DeclQualifiedName, Declaration>.Empty.Add(name, declaration);

        var rewritten =
            CaseBlockConsolidation.RewriteDeclarationDictionary(
                declarations,
                CaseBlockConsolidation.CaseConsolidationConfig.Default);

        var rewrittenOptimized =
            CaseBlockConsolidation.RewriteDeclarationDictionary(
                OptimizedElmSyntaxDeclarations.FromFlatDictionary(declarations),
                CaseBlockConsolidation.CaseConsolidationConfig.Default);

        rewrittenOptimized.RenderAsFlatDictionary().Should().Equal(rewritten);
        var function = (Declaration.FunctionDeclaration)rewritten[name];

        function.Function.Declaration.Expression.Should().BeOfType<Expression.IfBlock>();

        function.Function.Declaration.GetHashCode().Should().Be(
            new FunctionImplementation(
                function.Function.Declaration.Name,
                function.Function.Declaration.Arguments,
                function.Function.Declaration.Expression).GetHashCode());
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    /// <summary>
    /// Parses <paramref name="moduleText"/>, locates the function body
    /// of <paramref name="functionName"/>, and applies the local
    /// case-block consolidation rewrite to its outermost expression.
    /// Returns the rewritten expression (or
    /// the original, if no rewrite fired) together with a flag
    /// indicating whether a rewrite fired.
    /// </summary>
    private static (Expression Rewritten, bool Fired) TryConsolidateBodyOf(
        string moduleText, string functionName)
    {
        var func = ParseFunctionByName(moduleText, functionName);

        var body = func.Expression;

        var rewritten =
            CaseBlockConsolidation.RewriteExpression(
                body,
                CaseBlockConsolidation.CaseConsolidationConfig.Default);

        var fired = !ReferenceEquals(rewritten, body);

        return (rewritten, fired);
    }

    private static FunctionImplementation ParseFunctionByName(
        string moduleText, string functionName)
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Failed parsing: " + err));

        var converted =
            ConvertFromConcrete.FromFile(parsed);

        foreach (var declaration in converted.Declarations)
        {
            if (declaration is Declaration.FunctionDeclaration funcDecl &&
                funcDecl.Function.Declaration.Name == functionName)
            {
                return funcDecl.Function.Declaration;
            }
        }

        throw new Exception("Function '" + functionName + "' not found.");
    }
}
