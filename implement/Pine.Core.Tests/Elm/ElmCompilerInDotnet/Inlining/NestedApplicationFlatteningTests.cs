using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Generic;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

/// <summary>
/// Coverage for nested-Application flattening — a normalization step
/// the optimization pipeline owes its consumers (the pipeline that
/// emits Pine code in <see cref="ExpressionCompiler.CompileApplication"/>).
/// 
/// <para>
/// Background: in the abstract syntax model an
/// <see cref="SyntaxTypes.Expression.Application"/> carries a dedicated head in its
/// <see cref="SyntaxTypes.Expression.Application.Function"/> field plus an
/// <see cref="SyntaxTypes.Expression.Application.Arguments"/> list, so the natural form of
/// <c>f a b c</c> is <c>Application(f, [a, b, c])</c>. However, several
/// transformations that <em>build</em> an Application from pieces
/// (most notably the pipe-operator desugaring inside
/// <see cref="ElmSyntaxOptimization.InlineApplication"/>, but also any substitution
/// that replaces a function reference by an Application-typed expression)
/// can produce nested form
/// <c>Application(Application(h, [a]), [b])</c> when the head provided to
/// the new Application is itself an Application. Both forms render
/// identically (<c>h a b</c>) in the snapshot output, but they compile
/// differently:
/// </para>
/// 
/// <list type="bullet">
/// <item>
/// Flat form (<c>Application(h, [a, b])</c>): when <c>h</c> is a
/// statically-known top-level function with arity 2, the call hits the
/// fast direct-call path and no closure is allocated.
/// </item>
/// <item>
/// Nested form (<c>Application(Application(h, [a]), [b])</c>): the inner
/// Application is recognized as a partial application of <c>h</c> with
/// arity 2 and only one supplied argument, forcing
/// <see cref="CodeGen.FunctionValueBuilder"/>
/// to emit a function-value wrapper that is then dispatched generically
/// by the outer Application — a closure allocation and a generic
/// dispatch that the direct-call path would have skipped entirely.
/// </item>
/// </list>
/// 
/// <para>
/// The optimization pipeline must therefore guarantee that no Application
/// node has another Application as its head after lowering. These tests
/// pin that invariant for the patterns most likely to introduce nested
/// form in practice.
/// </para>
/// </summary>
public class NestedApplicationFlatteningTests
{
    /// <summary>
    /// Counts every <see cref="SyntaxTypes.Expression.Application"/> node
    /// across the declarations whose head
    /// (<see cref="SyntaxTypes.Expression.Application.Function"/>) is itself an
    /// <see cref="SyntaxTypes.Expression.Application"/>.
    /// </summary>
    private static int CountNestedApplicationHeads(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var count = 0;

        foreach (var decl in declarations.Values)
        {
            if (decl is SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                CountInExpression(funcDecl.Function.Declaration.Expression, ref count);
            }
        }

        return count;
    }

    private static void CountInExpression(SyntaxTypes.Expression expr, ref int count)
    {
        if (expr is SyntaxTypes.Expression.Application app &&
            app.Function is SyntaxTypes.Expression.Application)
        {
            count++;
        }

        foreach (var child in EnumerateChildExpressions(expr))
        {
            CountInExpression(child, ref count);
        }
    }

    private static IEnumerable<SyntaxTypes.Expression> EnumerateChildExpressions(SyntaxTypes.Expression expr)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.Application app:
                yield return app.Function;

                foreach (var arg in app.Arguments)
                    yield return arg;

                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                yield return opApp.Left;
                yield return opApp.Right;
                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                yield return ifBlock.Condition;
                yield return ifBlock.ThenBlock;
                yield return ifBlock.ElseBlock;
                break;

            case SyntaxTypes.Expression.Negation neg:
                yield return neg.Expression;
                break;

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var item in listExpr.Elements)
                    yield return item;

                break;

            case SyntaxTypes.Expression.TupledExpression tupled:
                foreach (var item in tupled.Elements)
                    yield return item;

                break;

            case SyntaxTypes.Expression.LambdaExpression lambda:
                yield return lambda.Expression;
                break;

            case SyntaxTypes.Expression.LetExpression letExpr:
                foreach (var letDecl in letExpr.Declarations)
                {
                    if (letDecl is SyntaxTypes.LetDeclaration.LetFunction letFn)
                        yield return letFn.Function.Declaration.Expression;

                    if (letDecl is SyntaxTypes.LetDeclaration.LetDestructuring letDestr)
                        yield return letDestr.Expression;
                }

                yield return letExpr.Expression;
                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                yield return caseExpr.Expression;

                foreach (var caseItem in caseExpr.Cases)
                    yield return caseItem.Expression;

                break;

            case SyntaxTypes.Expression.RecordExpr rec:
                foreach (var field in rec.Fields)
                    yield return field.Value;

                break;

            case SyntaxTypes.Expression.RecordAccess access:
                yield return access.Record;
                break;

            case SyntaxTypes.Expression.RecordUpdateExpression upd:
                foreach (var field in upd.Fields)
                    yield return field.Value;

                break;

            // Leaves — no child expressions.
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.FunctionOrValue:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                break;
        }
    }

    /// <summary>
    /// <para>
    /// Source pattern: a value piped (<c>|&gt;</c>) into an
    /// already-partially-applied two-argument function. The pipe
    /// desugars to <c>map2 f x</c>, where <c>f</c> in the AST is itself
    /// an <see cref="SyntaxTypes.Expression.Application"/>
    /// (<c>partialMap g</c>). Without flattening, the desugared
    /// expression is stored as
    /// <c>Application[Application[partialMap, g], (parseInt input)]</c>.
    /// </para>
    /// 
    /// <para>
    /// After D2 the optimization pipeline must produce the flat form
    /// <c>Application[partialMap, g, (parseInt input)]</c> so the
    /// <see cref="ExpressionCompiler"/>
    /// hits the direct-call fast path for the saturated 3-arg call.
    /// </para>
    /// </summary>
    [Fact]
    public void Pipe_into_partial_application_does_not_produce_nested_application_form()
    {
        var elmModuleText =
            """"
            module App exposing (..)


            mapTwice : (Int -> Int) -> (Int -> Int) -> Int -> Int
            mapTwice f g x =
                f (g x)


            double : Int -> Int
            double n =
                Pine_kernel.int_mul_be [ n, 2 ]


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add [ n, 1 ]


            entry : Int -> Int
            entry input =
                input
                    |> mapTwice double increment
            """";

        var declarations =
            InliningTestHelper.CanonicalizeAndInlineToAbstractDeclarations(
                [elmModuleText],
                ElmSyntaxOptimization.Config.OnlyFunctions);

        var nestedHeadCount = CountNestedApplicationHeads(declarations);

        nestedHeadCount.Should().Be(
            0,
            "after pipe desugaring the call `mapTwice double increment input` " +
            "must be stored as a single flat Application[mapTwice, double, increment, input] " +
            "rather than the nested-curried form Application[Application[mapTwice, double, increment], input]; " +
            "the nested form forces ExpressionCompiler.CompileApplication to emit a closure " +
            "via FunctionValueBuilder.EmitFunctionValueWithEnvFunctions instead of taking the " +
            "direct-call fast path");
    }

    /// <summary>
    /// Symmetric case for the left-pipe operator <c>&lt;|</c> (Basics.apL).
    /// Source pattern: an already-partially-applied function applied
    /// to a value via <c>&lt;|</c>. The desugared form is structurally
    /// <c>partialMap g (parseInt input)</c>, which must end up flat.
    /// </summary>
    [Fact]
    public void LeftPipe_into_partial_application_does_not_produce_nested_application_form()
    {
        var elmModuleText =
            """"
            module App exposing (..)


            mapTwice : (Int -> Int) -> (Int -> Int) -> Int -> Int
            mapTwice f g x =
                f (g x)


            double : Int -> Int
            double n =
                Pine_kernel.int_mul_be [ n, 2 ]


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add [ n, 1 ]


            entry : Int -> Int
            entry input =
                mapTwice double increment <| input
            """";

        var declarations =
            InliningTestHelper.CanonicalizeAndInlineToAbstractDeclarations(
                [elmModuleText],
                ElmSyntaxOptimization.Config.OnlyFunctions);

        var nestedHeadCount = CountNestedApplicationHeads(declarations);

        nestedHeadCount.Should().Be(
            0,
            "after left-pipe desugaring the call must be stored as a single flat " +
            "Application rather than nested-curried form");
    }

    /// <summary>
    /// Source pattern: a let-bound partial application that is then
    /// applied to its remaining argument elsewhere in the body. Without
    /// flattening, the apply site
    /// <c>Application[Application[partialMap, g], y]</c> survives in
    /// the AST. After D2 it must be flattened to
    /// <c>Application[partialMap, g, y]</c>.
    /// </summary>
    [Fact]
    public void Let_bound_partial_application_consumed_in_body_does_not_produce_nested_application_form()
    {
        var elmModuleText =
            """"
            module App exposing (..)


            mapTwice : (Int -> Int) -> (Int -> Int) -> Int -> Int
            mapTwice f g x =
                f (g x)


            double : Int -> Int
            double n =
                Pine_kernel.int_mul_be [ n, 2 ]


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add [ n, 1 ]


            entry : Int -> Int
            entry input =
                let
                    partial =
                        mapTwice double increment
                in
                partial input
            """";

        var declarations =
            InliningTestHelper.CanonicalizeAndInlineToAbstractDeclarations(
                [elmModuleText],
                ElmSyntaxOptimization.Config.OnlyFunctions);

        var nestedHeadCount = CountNestedApplicationHeads(declarations);

        nestedHeadCount.Should().Be(
            0,
            "after let-binding inlining the call site `partial input` should be flattened " +
            "into a single Application[mapTwice, double, increment, input] rather than the " +
            "nested-curried form Application[Application[mapTwice, double, increment], input]");
    }

    /// <summary>
    /// Sanity check: an Application whose head is a
    /// <see cref="SyntaxTypes.Expression.FunctionOrValue"/> (the typical
    /// flat case) is unchanged and still flat.
    /// </summary>
    [Fact]
    public void Already_flat_application_is_not_disturbed_by_flattening()
    {
        var elmModuleText =
            """"
            module App exposing (..)


            mapTwice : (Int -> Int) -> (Int -> Int) -> Int -> Int
            mapTwice f g x =
                f (g x)


            double : Int -> Int
            double n =
                Pine_kernel.int_mul_be [ n, 2 ]


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add [ n, 1 ]


            entry : Int -> Int
            entry input =
                mapTwice double increment input
            """";

        var declarations =
            InliningTestHelper.CanonicalizeAndInlineToAbstractDeclarations(
                [elmModuleText],
                ElmSyntaxOptimization.Config.OnlyFunctions);

        var nestedHeadCount = CountNestedApplicationHeads(declarations);

        nestedHeadCount.Should().Be(
            0,
            "a saturated 4-element Application should remain a flat Application, never " +
            "rewritten into nested form (even if size-based inlining further rewrites it)");
    }
}
