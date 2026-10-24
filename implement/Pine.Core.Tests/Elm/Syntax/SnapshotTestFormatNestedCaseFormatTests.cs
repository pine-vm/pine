using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7;
using Pine.Core.Elm.Syntax.Concrete;
using Xunit;

using Stil4mElmSyntax = Pine.Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7;
using StilQualifiedNameRef = Pine.Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.QualifiedNameRef;

namespace Pine.Core.Tests.Elm.Syntax;

/// <summary>
/// Regression test for an <see cref="Avh4Format"/> defect first observed in
/// the snapshot <c>ParserFastTests.ExpectedPostOpt_testWithoutLinebreak_alpha</c>:
/// when the scrutinee of a <c>case ... of</c> is itself a <c>case ... of</c>
/// (or any other intrinsically multi-line expression such as
/// <c>let ... in ...</c> — already covered — or <c>if ... then ... else ...</c>),
/// the formatter must place the <c>case</c> keyword on its own line, the
/// scrutinee indented on the following line(s), and the <c>of</c> keyword on
/// its own line aligned with <c>case</c>. The avh4 elm-format binary always
/// uses this multi-line layout in that situation.
///
/// <para>The previous behavior only triggered the multi-line case-header layout
/// when the scrutinee was a <c>LetExpression</c>; a nested <c>CaseExpression</c>
/// (or <c>IfBlock</c>) scrutinee with collapsed token locations (as the
/// snapshot-format pipeline produces via
/// <see cref="ToFullSyntaxModel.Convert(Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.File)"/>,
/// where every token gets the same dummy <c>(1,1)</c> location) was rendered
/// with both <c>case</c> keywords on the same line, e.g. <c>case case ...</c>.</para>
///
/// <para>The companion <see cref="Avh4Format.IfWithLetConditionFormatTests"/>
/// covers the symmetric situation for <c>if</c> conditions.</para>
/// </summary>
public class SnapshotTestFormatNestedCaseFormatTests
{
    [Fact]
    public void Case_with_case_scrutinee_renders_with_case_and_of_on_their_own_lines()
    {
        // Build:  decl = case (case x of Just _ -> 1; Nothing -> 2) of Just _ -> 1; Nothing -> 2
        // All token locations are collapsed to (1,1) by ToFullSyntaxModel.Convert,
        // mirroring how the post-lowering declaration dictionary is rendered.

        var innerCase =
            (Stil4mElmSyntax.Expression)
            new Stil4mElmSyntax.Expression.CaseExpression(
                new Stil4mElmSyntax.CaseBlock(
                    Expression:
                    WrapNode(
                        (Stil4mElmSyntax.Expression)
                        new Stil4mElmSyntax.Expression.FunctionOrValue([], "x")),
                    Cases:
                    [
                    new Stil4mElmSyntax.Case(
                        Pattern:
                        WrapNode(
                            (Stil4mElmSyntax.Pattern)
                            new Stil4mElmSyntax.Pattern.NamedPattern(
                                new StilQualifiedNameRef([], "Just"),
                                [
                                WrapNode(
                                    (Stil4mElmSyntax.Pattern)
                                    new Stil4mElmSyntax.Pattern.VarPattern("_"))
                                ])),
                        Expression:
                        WrapNode(
                            (Stil4mElmSyntax.Expression)
                            new Stil4mElmSyntax.Expression.Integer(1L))),

                    new Stil4mElmSyntax.Case(
                        Pattern:
                        WrapNode(
                            (Stil4mElmSyntax.Pattern)
                            new Stil4mElmSyntax.Pattern.NamedPattern(
                                new StilQualifiedNameRef([], "Nothing"),
                                [])),
                        Expression:
                        WrapNode(
                            (Stil4mElmSyntax.Expression)
                            new Stil4mElmSyntax.Expression.Integer(2L))),
                    ]));

        var outerCase =
            (Stil4mElmSyntax.Expression)
            new Stil4mElmSyntax.Expression.CaseExpression(
                new Stil4mElmSyntax.CaseBlock(
                    Expression: WrapNode(innerCase),
                    Cases:
                    [
                    new Stil4mElmSyntax.Case(
                        Pattern:
                        WrapNode(
                            (Stil4mElmSyntax.Pattern)
                            new Stil4mElmSyntax.Pattern.NamedPattern(
                                new StilQualifiedNameRef([], "Just"),
                                [
                                WrapNode(
                                    (Stil4mElmSyntax.Pattern)
                                    new Stil4mElmSyntax.Pattern.VarPattern("_"))
                                ])),
                        Expression:
                        WrapNode(
                            (Stil4mElmSyntax.Expression)
                            new Stil4mElmSyntax.Expression.Integer(10L))),

                    new Stil4mElmSyntax.Case(
                        Pattern:
                        WrapNode(
                            (Stil4mElmSyntax.Pattern)
                            new Stil4mElmSyntax.Pattern.NamedPattern(
                                new StilQualifiedNameRef([], "Nothing"),
                                [])),
                        Expression:
                        WrapNode(
                            (Stil4mElmSyntax.Expression)
                            new Stil4mElmSyntax.Expression.Integer(20L))),
                    ]));

        var declaration = BuildFunctionDeclarationReturning("f", outerCase);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclaration(
                DeclQualifiedName.Create(["TestModule"], "f"),
                ToFullSyntaxModel.Convert(declaration));

        // Two strict invariants matching what avh4 elm-format produces for a
        // case whose scrutinee is itself a case expression:
        //
        //   1. The two consecutive `case` keywords MUST NOT appear on the
        //      same line (the broken output rendered `case case ...`).
        //   2. The outer `of` keyword MUST appear on its own line at the
        //      base indent of the outer `case` (i.e. preceded only by
        //      whitespace), not trailing the last branch of the inner
        //      case (the broken output rendered `... 2 of`).
        rendered.Should().NotContain("case case");

        var lines = rendered.Split('\n');

        var outerOfLines =
            System.Linq.Enumerable.Where(
                lines,
                l =>
                {
                    var t = l.TrimStart();
                    return t == "of" || t.StartsWith("of ");
                });

        System.Linq.Enumerable.Count(outerOfLines)
            .Should().BeGreaterThanOrEqualTo(
            1,
            "the outer `of` keyword must appear on its own line, not "
            + "trailing the last branch of the inner case expression.");
    }

    [Fact]
    public void Case_with_if_scrutinee_renders_with_case_and_of_on_their_own_lines()
    {
        // Build:  decl = case (if x then 1 else 2) of  Just _ -> 1; Nothing -> 2

        var innerIf =
            (Stil4mElmSyntax.Expression)
            new Stil4mElmSyntax.Expression.IfBlock(
                Condition:
                WrapNode(
                    (Stil4mElmSyntax.Expression)
                    new Stil4mElmSyntax.Expression.FunctionOrValue([], "x")),
                ThenBlock:
                WrapNode(
                    (Stil4mElmSyntax.Expression)
                    new Stil4mElmSyntax.Expression.Integer(1L)),
                ElseBlock:
                WrapNode(
                    (Stil4mElmSyntax.Expression)
                    new Stil4mElmSyntax.Expression.Integer(2L)));

        var outerCase =
            (Stil4mElmSyntax.Expression)
            new Stil4mElmSyntax.Expression.CaseExpression(
                new Stil4mElmSyntax.CaseBlock(
                    Expression: WrapNode(innerIf),
                    Cases:
                    [
                    new Stil4mElmSyntax.Case(
                        Pattern:
                        WrapNode(
                            (Stil4mElmSyntax.Pattern)
                            new Stil4mElmSyntax.Pattern.VarPattern("v")),
                        Expression:
                        WrapNode(
                            (Stil4mElmSyntax.Expression)
                            new Stil4mElmSyntax.Expression.FunctionOrValue([], "v"))),
                    ]));

        var declaration = BuildFunctionDeclarationReturning("g", outerCase);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclaration(
                DeclQualifiedName.Create(["TestModule"], "g"),
                ToFullSyntaxModel.Convert(declaration));

        rendered.Should().NotContain("case if");
    }

    private static Stil4mElmSyntax.Declaration BuildFunctionDeclarationReturning(
        string name,
        Stil4mElmSyntax.Expression body)
    {
        var implementation =
            new Stil4mElmSyntax.FunctionImplementation(
                Name: WrapNode(name),
                Arguments: [],
                Expression: WrapNode(body));

        return
            new Stil4mElmSyntax.Declaration.FunctionDeclaration(
                new Stil4mElmSyntax.FunctionStruct(
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
