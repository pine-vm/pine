using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using System;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Targeted tests for <see cref="CaseBlockConsolidation"/>: the two
/// local case-block consolidation rewrites described in
/// <c>explore/internal-analysis/2026-05-20-elm-syntax-case-block-consolidation.md</c>.
///
/// <para>
/// Each test parses a small Elm function body, applies
/// <see cref="CaseBlockConsolidation.TryConsolidateLocal"/> directly to
/// the outermost <see cref="SyntaxTypes.Expression.CaseExpression"/>,
/// and asserts a structural property of the result (e.g. the outer
/// case is gone; the if-chain has absorbed the arms). Snapshot-style
/// rendering via <see cref="SnapshotTestFormat.RenderQualifiedDeclarations"/>
/// is used to lock in the canonical text shape for the two primary
/// positive cases.
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
        rewritten.Should().BeOfType<SyntaxTypes.Expression.IfBlock>();
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
        rewritten.Should().BeOfType<SyntaxTypes.Expression.IfBlock>();
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
        rewritten.Should().BeOfType<SyntaxTypes.Expression.IfBlock>();

        // Walk into the else branch — should be the integer literal `0`
        // unchanged (no let-destructure wrapper, since the Nothing
        // pattern binds no variables).
        var ifBlock = (SyntaxTypes.Expression.IfBlock)rewritten;
        var elseValue = ifBlock.ElseBlock.Value;

        // After peeling, must be a non-let expression — no destructure was emitted.
        elseValue.Should().NotBeOfType<SyntaxTypes.Expression.LetExpression>();
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
        rewritten.Should().BeOfType<SyntaxTypes.Expression.CaseExpression>();

        var consolidated = (SyntaxTypes.Expression.CaseExpression)rewritten;
        consolidated.CaseBlock.Cases.Count.Should().Be(2);
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
        rewritten.Should().BeOfType<SyntaxTypes.Expression.LetExpression>();

        var letExpr = (SyntaxTypes.Expression.LetExpression)rewritten;

        // The let body should now be a single CaseExpression
        // (the consolidated case).
        letExpr.Value.Expression.Value.Should().BeOfType<SyntaxTypes.Expression.CaseExpression>();
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

    // =========================================================================
    // Helpers
    // =========================================================================

    /// <summary>
    /// Parses <paramref name="moduleText"/>, locates the function body
    /// of <paramref name="functionName"/>, and applies the local
    /// case-block consolidation rewrite to its outermost expression
    /// (after peeling parens). Returns the rewritten expression (or
    /// the original, if no rewrite fired) together with a flag
    /// indicating whether a rewrite fired.
    /// </summary>
    private static (SyntaxTypes.Expression Rewritten, bool Fired) TryConsolidateBodyOf(
        string moduleText, string functionName)
    {
        var func = ParseFunctionByName(moduleText, functionName);

        var body = func.Expression.Value;

        while (body is SyntaxTypes.Expression.ParenthesizedExpression paren)
        {
            body = paren.Expression.Value;
        }

        var rewritten =
            CaseBlockConsolidation.RewriteExpression(
                body,
                CaseBlockConsolidation.CaseConsolidationConfig.Default);

        // Peel parens off the rewritten result too — the rebuilt
        // if-chain often retains parens from the original scrutinee
        // position, which is presentation-only noise.
        var rewrittenPeeled = rewritten;

        while (rewrittenPeeled is SyntaxTypes.Expression.ParenthesizedExpression rp)
        {
            rewrittenPeeled = rp.Expression.Value;
        }

        var fired = !ReferenceEquals(rewritten, body);

        return (rewrittenPeeled, fired);
    }

    private static SyntaxTypes.FunctionImplementation ParseFunctionByName(
        string moduleText, string functionName)
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Failed parsing: " + err));

        var converted =
            SyntaxTypes.FromFullSyntaxModel.Convert(parsed);

        foreach (var declNode in converted.Declarations)
        {
            if (declNode.Value is SyntaxTypes.Declaration.FunctionDeclaration funcDecl &&
                funcDecl.Function.Declaration.Value.Name.Value == functionName)
            {
                return funcDecl.Function.Declaration.Value;
            }
        }

        throw new Exception("Function '" + functionName + "' not found.");
    }
}
