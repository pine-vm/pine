using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using StilQualifiedNameRef = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.QualifiedNameRef;

namespace Pine.Core.Tests.Elm.ElmSyntax;

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
                new DeclQualifiedName(["TestModule"], "f"),
                SyntaxTypes.ToFullSyntaxModel.Convert(declaration));

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
            (SyntaxTypes.Expression)
            new SyntaxTypes.Expression.CaseExpression(
                new SyntaxTypes.CaseBlock(
                    Expression: WrapNode(
                        (SyntaxTypes.Expression)
                        new SyntaxTypes.Expression.FunctionOrValue([], "scrut")),
                    Cases:
                    [
                    new SyntaxTypes.Case(
                        Pattern: WrapNode(
                            (SyntaxTypes.Pattern)
                            new SyntaxTypes.Pattern.NamedPattern(
                                new StilQualifiedNameRef([], "Good"),
                                [
                                WrapNode((SyntaxTypes.Pattern)new SyntaxTypes.Pattern.VarPattern("a")),
                                WrapNode((SyntaxTypes.Pattern)new SyntaxTypes.Pattern.VarPattern("s1")),
                                ])),
                        Expression: WrapNode(innerApplication)),

                    new SyntaxTypes.Case(
                        Pattern: WrapNode(
                            (SyntaxTypes.Pattern)
                            new SyntaxTypes.Pattern.NamedPattern(
                                new StilQualifiedNameRef([], "Bad"),
                                [])),
                        Expression: WrapNode(
                            (SyntaxTypes.Expression)
                            new SyntaxTypes.Expression.FunctionOrValue([], "fallback"))),
                    ]));

        var outerCase =
            (SyntaxTypes.Expression)
            new SyntaxTypes.Expression.CaseExpression(
                new SyntaxTypes.CaseBlock(
                    // The inner case (containing our application) is the scrutinee.
                    Expression: WrapNode(innerCase),
                    Cases:
                    [
                    new SyntaxTypes.Case(
                        Pattern: WrapNode(
                            (SyntaxTypes.Pattern)
                            new SyntaxTypes.Pattern.NamedPattern(
                                new StilQualifiedNameRef([], "Good"),
                                [
                                WrapNode((SyntaxTypes.Pattern)new SyntaxTypes.Pattern.VarPattern("v")),
                                ])),
                        Expression: WrapNode(
                            (SyntaxTypes.Expression)
                            new SyntaxTypes.Expression.FunctionOrValue([], "v"))),
                    ]));

        var declaration = BuildFunctionDeclarationReturning("fNested", outerCase);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclaration(
                new DeclQualifiedName(["TestModule"], "fNested"),
                SyntaxTypes.ToFullSyntaxModel.Convert(declaration));

        AssertMultilineApplicationLayout(rendered, "let");
    }

    private static SyntaxTypes.Expression BuildGoodAppliedToParenthesizedLetAndS1()
    {
        var letExpr =
            (SyntaxTypes.Expression)
            new SyntaxTypes.Expression.LetExpression(
                new SyntaxTypes.Expression.LetBlock(
                    Declarations:
                    [
                    WrapNode(
                        (SyntaxTypes.Expression.LetDeclaration)
                        new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                            Pattern: WrapNode(
                                (SyntaxTypes.Pattern)
                                new SyntaxTypes.Pattern.NamedPattern(
                                    new StilQualifiedNameRef([], "String"),
                                    [
                                    WrapNode(
                                        (SyntaxTypes.Pattern)
                                        new SyntaxTypes.Pattern.VarPattern("bytes"))
                                    ])),
                            Expression: WrapNode(
                                (SyntaxTypes.Expression)
                                new SyntaxTypes.Expression.FunctionOrValue([], "a"))))
                    ],
                    Expression: WrapNode(
                        (SyntaxTypes.Expression)
                        new SyntaxTypes.Expression.FunctionOrValue([], "concat"))));

        var parenLet =
            (SyntaxTypes.Expression)
            new SyntaxTypes.Expression.ParenthesizedExpression(
                Expression: WrapNode(letExpr));

        return
            (SyntaxTypes.Expression)
            new SyntaxTypes.Expression.Application(
                [
                WrapNode((SyntaxTypes.Expression)new SyntaxTypes.Expression.FunctionOrValue([], "Good")),
                WrapNode(parenLet),
                WrapNode((SyntaxTypes.Expression)new SyntaxTypes.Expression.FunctionOrValue([], "s1")),
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

    private static SyntaxTypes.Declaration BuildFunctionDeclarationReturning(
        string name,
        SyntaxTypes.Expression body)
    {
        var implementation =
            new SyntaxTypes.FunctionImplementation(
                Name: WrapNode(name),
                Arguments: [],
                Expression: WrapNode(body));

        return
            new SyntaxTypes.Declaration.FunctionDeclaration(
                new SyntaxTypes.FunctionStruct(
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
