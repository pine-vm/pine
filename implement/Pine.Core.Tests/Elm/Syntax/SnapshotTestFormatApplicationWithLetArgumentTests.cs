using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7;
using Pine.Core.Elm.Syntax.Concrete;
using Xunit;

using Stil4mElm = Pine.Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7;
using StilQualifiedNameRef = Pine.Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.QualifiedNameRef;

namespace Pine.Core.Tests.Elm.Syntax;

/// <summary>
/// Regression test for an <see cref="Avh4Format"/> / <see cref="SnapshotTestFormat"/>
/// defect first observed in the snapshot
/// <c>ParserFastTests.ExpectedPostOpt_testWithoutLinebreak_alpha</c>:
///
/// <para>The broken snapshot rendered the case-branch body
/// <c>ParserFastTestModule.Good (let ... in body) s1</c> with the function
/// name and the opening of the let-block collapsed onto a single line
/// (<c>ParserFastTestModule.Good (let</c>), with the trailing argument
/// <c>s1</c> abutting the closing paren of the let-block (<c>) s1</c>).
/// This is not what the avh4 elm-format binary produces for an
/// application whose argument is an intrinsically multi-line expression
/// (<c>let</c>, <c>case</c>, <c>if</c>); avh4 places each argument on its
/// own line at the application's argument indent column.</para>
///
/// <para>The root cause was that
/// <see cref="SnapshotTestFormat"/>'s recursive
/// <c>FormatExpression</c> did not recurse into the *scrutinee* of a
/// <c>case ... of</c> expression. As a result, applications nested inside
/// a case scrutinee (for example: a nested <c>case ... of</c> in scrutinee
/// position whose branch body is itself a problematic application) never
/// received the multi-line row marking that <c>FormatApplication</c>
/// assigns, and the downstream Avh4 renderer fell through to its
/// single-line application layout.</para>
/// </summary>
public class SnapshotTestFormatApplicationWithLetArgumentTests
{
    [Fact]
    public void Application_with_parenthesized_let_argument_renders_with_let_on_its_own_line()
    {
        // Build: decl = f =  Good (let (String bytes) = a in concat) s1

        var application = BuildGoodAppliedToParenthesizedLetAndS1();

        var declaration = BuildFunctionDeclarationReturning("f", application);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclaration(
                DeclQualifiedName.Create(["TestModule"], "f"),
                ToFullSyntaxModel.Convert(declaration));

        AssertMultilineApplicationLayout(rendered, "let");
    }

    [Fact]
    public void Application_with_parenthesized_let_argument_nested_in_case_scrutinee_renders_with_let_on_its_own_line()
    {
        // Build:
        //   decl = fNested =  case (case scrut of Good a s1 -> Good (let ... in body) s1
        //                                         Bad -> fallback) of
        //                          Good v -> v
        // i.e. the buggy application is the body of a case branch, and that
        // entire case is the SCRUTINEE of another case. This mirrors the
        // structure produced by the ParserFastTests post-opt pipeline where
        // the broken `Good (let)` rendering originally surfaced.

        var innerApplication = BuildGoodAppliedToParenthesizedLetAndS1();

        var innerCase =
            (Stil4mElm.Expression)
            new Stil4mElm.Expression.CaseExpression(
                new Stil4mElm.CaseBlock(
                    Expression:
                    WrapNode(
                        (Stil4mElm.Expression)
                        new Stil4mElm.Expression.FunctionOrValue([], "scrut")),
                    Cases:
                    [
                    new Stil4mElm.Case(
                        Pattern:
                        WrapNode(
                            (Stil4mElm.Pattern)
                            new Stil4mElm.Pattern.NamedPattern(
                                new StilQualifiedNameRef([], "Good"),
                                [
                                WrapNode((Stil4mElm.Pattern)new Stil4mElm.Pattern.VarPattern("a")),
                                WrapNode((Stil4mElm.Pattern)new Stil4mElm.Pattern.VarPattern("s1")),
                                ])),
                        Expression: WrapNode(innerApplication)),

                    new Stil4mElm.Case(
                        Pattern:
                        WrapNode(
                            (Stil4mElm.Pattern)
                            new Stil4mElm.Pattern.NamedPattern(
                                new StilQualifiedNameRef([], "Bad"),
                                [])),
                        Expression:
                        WrapNode(
                            (Stil4mElm.Expression)
                            new Stil4mElm.Expression.FunctionOrValue([], "fallback"))),
                    ]));

        var outerCase =
            (Stil4mElm.Expression)
            new Stil4mElm.Expression.CaseExpression(
                new Stil4mElm.CaseBlock(
                    // The inner case (containing our application) is the scrutinee.
                    Expression: WrapNode(innerCase),
                    Cases:
                    [
                    new Stil4mElm.Case(
                        Pattern:
                        WrapNode(
                            (Stil4mElm.Pattern)
                            new Stil4mElm.Pattern.NamedPattern(
                                new StilQualifiedNameRef([], "Good"),
                                [
                                WrapNode((Stil4mElm.Pattern)new Stil4mElm.Pattern.VarPattern("v")),
                                ])),
                        Expression:
                        WrapNode(
                            (Stil4mElm.Expression)
                            new Stil4mElm.Expression.FunctionOrValue([], "v"))),
                    ]));

        var declaration = BuildFunctionDeclarationReturning("fNested", outerCase);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclaration(
                DeclQualifiedName.Create(["TestModule"], "fNested"),
                ToFullSyntaxModel.Convert(declaration));

        AssertMultilineApplicationLayout(rendered, "let");
    }

    private static Stil4mElm.Expression BuildGoodAppliedToParenthesizedLetAndS1()
    {
        var letExpr =
            (Stil4mElm.Expression)
            new Stil4mElm.Expression.LetExpression(
                new Stil4mElm.Expression.LetBlock(
                    Declarations:
                    [
                    WrapNode(
                        (Stil4mElm.Expression.LetDeclaration)
                        new Stil4mElm.Expression.LetDeclaration.LetDestructuring(
                            Pattern:
                            WrapNode(
                                (Stil4mElm.Pattern)
                                new Stil4mElm.Pattern.NamedPattern(
                                    new StilQualifiedNameRef([], "String"),
                                    [
                                    WrapNode(
                                        (Stil4mElm.Pattern)
                                        new Stil4mElm.Pattern.VarPattern("bytes"))
                                    ])),
                            Expression:
                            WrapNode(
                                (Stil4mElm.Expression)
                                new Stil4mElm.Expression.FunctionOrValue([], "a"))))
                    ],
                    Expression:
                    WrapNode(
                        (Stil4mElm.Expression)
                        new Stil4mElm.Expression.FunctionOrValue([], "concat"))));

        var parenLet =
            (Stil4mElm.Expression)
            new Stil4mElm.Expression.ParenthesizedExpression(
                Expression: WrapNode(letExpr));

        return
            new Stil4mElm.Expression.Application(
                [
                WrapNode((Stil4mElm.Expression)new Stil4mElm.Expression.FunctionOrValue([], "Good")),
                WrapNode(parenLet),
                WrapNode((Stil4mElm.Expression)new Stil4mElm.Expression.FunctionOrValue([], "s1")),
                ]);
    }

    private static void AssertMultilineApplicationLayout(string rendered, string innerKeyword)
    {
        // Invariant 1: the function name `Good` and the opening of the
        // intrinsically multi-line argument must not appear on the same line.
        rendered.Should().NotContain(
            "Good (" + innerKeyword,
            "the function name and the opening of a multi-line "
            + innerKeyword + " argument must not appear on the same line; "
            + "the avh4 elm-format binary places each argument on its own "
            + "line when any argument is intrinsically multi-line.");

        // Invariant 2: the trailing argument `s1` must NOT appear on the
        // same line as the closing paren of the parenthesized inner block.
        rendered.Should().NotContain(
            ") s1",
            "subsequent arguments must appear on their own line at the "
            + "application's argument indent; they must not trail the "
            + "closing paren of a multi-line argument.");

        // Invariant 3: there must be at least one line whose trimmed
        // content starts with `(<innerKeyword>` (the opening paren of
        // the multi-line argument must sit on its own line at the
        // application's argument indent).
        var lines = rendered.Split('\n');

        var lineStartingWithParenInner =
            System.Linq.Enumerable.FirstOrDefault(
                lines,
                l =>
                {
                    var t = l.TrimStart();
                    return t.StartsWith("(" + innerKeyword);
                });

        lineStartingWithParenInner
            .Should().NotBeNull(
            "the avh4 layout for a multi-line application argument "
            + "places the opening paren on its own line at the "
            + "argument indent.");
    }

    private static Stil4mElm.Declaration BuildFunctionDeclarationReturning(
        string name,
        Stil4mElm.Expression body)
    {
        var implementation =
            new Stil4mElm.FunctionImplementation(
                Name: WrapNode(name),
                Arguments: [],
                Expression: WrapNode(body));

        return
            new Stil4mElm.Declaration.FunctionDeclaration(
                new Stil4mElm.FunctionStruct(
                    Documentation: null,
                    Signature: null,
                    Declaration: WrapNode(implementation)));
    }

    private static Node<T> WrapNode<T>(T value) =>
        new(Range: s_dummyRange, Value: value);

    private static readonly Location s_dummyLocation =
        new(Row: 1, Column: 1);

    private static readonly Range s_dummyRange =
        new(Start: s_dummyLocation, End: s_dummyLocation);
}
