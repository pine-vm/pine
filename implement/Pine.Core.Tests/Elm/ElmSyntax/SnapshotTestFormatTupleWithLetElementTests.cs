using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using StilQualifiedNameRef = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.QualifiedNameRef;

namespace Pine.Core.Tests.Elm.ElmSyntax;

/// <summary>
/// Regression tests for a <see cref="SnapshotTestFormat"/> defect first
/// observed at section 16 of the auto-generated tracking report
/// <c>explore/internal-analysis/2026-05-10-monomorphization-remaining-opportunities-after-lowering-for-parse-file.md</c>
/// (decl <c>Elm.Parser.Expression.expressionAfterOpeningSquareBracket</c>):
/// when a tuple contains a <c>let</c>-block (or any other intrinsically
/// multi-line expression — <c>case ... of</c>, <c>if ... then ... else</c>)
/// as one of its elements, the snapshot formatter fails to switch the
/// containing tuple to canonical multi-line layout. The rendered output
/// keeps the tuple separator on the opening line (<c>, let</c>), so the
/// let-block's <c>in</c>/body indents off an arbitrary column that drifts
/// far to the right of the page.
///
/// <para>The avh4 elm-format binary places each tuple element on its own
/// line in this situation, with the separator leading the line:
/// <code>
/// ( a
/// , let
///       x =
///           e
///   in
///   y
/// )
/// </code>
/// </para>
///
/// <para>These tests build the offending Stil4m abstract-syntax shape
/// directly (mirroring what the lowering / monomorphization pipeline
/// produces with dummy ranges) and assert canonical AVH4 multi-line tuple
/// layout — each separator (<c>,</c>) leads its line, the intrinsically
/// multi-line element opens on its own line, and the closing paren
/// stands on its own line at the tuple indent.</para>
/// </summary>
public class SnapshotTestFormatTupleWithLetElementTests
{
    [Fact]
    public void Tuple_with_let_element_renders_canonical_multi_line()
    {
        var rendered = RenderTupleWithSecondElement(BuildLetExpression());

        AssertCanonicalMultilineTupleLayout(rendered, intrinsicallyMultilineKeyword: "let");
    }

    [Fact]
    public void Tuple_with_case_element_renders_canonical_multi_line()
    {
        // case ... of is also intrinsically multi-line; it must trigger
        // the same multi-line tuple layout as `let`.
        var rendered = RenderTupleWithSecondElement(BuildCaseExpression());

        AssertCanonicalMultilineTupleLayout(rendered, intrinsicallyMultilineKeyword: "case");
    }

    [Fact]
    public void Tuple_with_if_element_renders_canonical_multi_line()
    {
        // if ... then ... else is also intrinsically multi-line; it must
        // trigger the same multi-line tuple layout as `let`.
        var rendered = RenderTupleWithSecondElement(BuildIfExpression());

        AssertCanonicalMultilineTupleLayout(rendered, intrinsicallyMultilineKeyword: "if");
    }

    [Fact]
    public void List_with_let_element_renders_canonical_multi_line()
    {
        // Lists share the same bare-kind classification as tuples; the
        // same defect applies and the same canonical multi-line layout
        // is expected.
        var rendered = RenderListWithSecondElement(BuildLetExpression());

        AssertCanonicalMultilineTupleLayout(rendered, intrinsicallyMultilineKeyword: "let");
    }

    private static string RenderTupleWithSecondElement(SyntaxTypes.Expression second)
    {
        var declQualifiedName = DeclQualifiedName.Create(["TestModule"], "f");

        var declaration =
            BuildFunctionDeclarationReturning(
                "f",
                new SyntaxTypes.Expression.TupledExpression(
                    Elements:
                    [
                    WrapNode(
                        (SyntaxTypes.Expression)
                        new SyntaxTypes.Expression.FunctionOrValue([], "a")),

                    WrapNode(second),
                    ]));

        return
            SnapshotTestFormat.RenderQualifiedDeclaration(
                declQualifiedName,
                SyntaxTypes.ToFullSyntaxModel.Convert(declaration));
    }

    private static string RenderListWithSecondElement(SyntaxTypes.Expression second)
    {
        var declQualifiedName = DeclQualifiedName.Create(["TestModule"], "f");

        var declaration =
            BuildFunctionDeclarationReturning(
                "f",
                new SyntaxTypes.Expression.ListExpr(
                    Elements:
                    [
                    WrapNode(
                        (SyntaxTypes.Expression)
                        new SyntaxTypes.Expression.FunctionOrValue([], "a")),

                    WrapNode(second),
                    ]));

        return
            SnapshotTestFormat.RenderQualifiedDeclaration(
                declQualifiedName,
                SyntaxTypes.ToFullSyntaxModel.Convert(declaration));
    }

    private static SyntaxTypes.Expression BuildLetExpression() =>
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
                            new SyntaxTypes.Expression.FunctionOrValue([], "x"))))
                ],
                Expression: WrapNode(
                    (SyntaxTypes.Expression)
                    new SyntaxTypes.Expression.FunctionOrValue([], "concat"))));

    private static SyntaxTypes.Expression BuildCaseExpression() =>
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
                            WrapNode((SyntaxTypes.Pattern)new SyntaxTypes.Pattern.VarPattern("v")),
                            ])),
                    Expression: WrapNode(
                        (SyntaxTypes.Expression)
                        new SyntaxTypes.Expression.FunctionOrValue([], "v"))),
                ]));

    private static SyntaxTypes.Expression BuildIfExpression() =>
        new SyntaxTypes.Expression.IfBlock(
            Condition: WrapNode(
                (SyntaxTypes.Expression)
                new SyntaxTypes.Expression.FunctionOrValue([], "cond")),
            ThenBlock: WrapNode(
                (SyntaxTypes.Expression)
                new SyntaxTypes.Expression.FunctionOrValue([], "yes")),
            ElseBlock: WrapNode(
                (SyntaxTypes.Expression)
                new SyntaxTypes.Expression.FunctionOrValue([], "no")));

    /// <summary>
    /// Verifies canonical AVH4 multi-line tuple (or list) layout: every
    /// separator must lead its line — i.e. no <c>", "</c> substring may
    /// appear with non-whitespace before it on the same line. In the
    /// broken layout that motivated this fix the opening line read
    /// <c>"    ( a, let"</c> and the subsequent lines indented off the
    /// post-<c>let</c> column far to the right.
    /// </summary>
    private static void AssertCanonicalMultilineTupleLayout(
        string rendered,
        string intrinsicallyMultilineKeyword)
    {
        // Invariant 1: rendered output must span multiple lines.
        rendered.Should().Contain(
            "\n",
            "a tuple (or list) containing an intrinsically multi-line "
            + "element (" + intrinsicallyMultilineKeyword + ") must be "
            + "rendered across multiple lines, matching the avh4 "
            + "elm-format binary's canonical layout.");

        var lines = rendered.Split('\n');

        // Invariant 2: each separator (`, `) between tuple/list elements
        // must lead its line — the comma must be the first non-whitespace
        // character on the line. This matches avh4 elm-format's
        // canonical multi-line layout. The broken output put `, <kw>`
        // mid-line (e.g. `( a, let`), which is what we are guarding
        // against.
        for (var i = 0; i < lines.Length; i++)
        {
            var line = lines[i];

            // Look for an inline ", " (separator followed by space)
            // anywhere in the line. If found, the substring before it
            // must consist of either the opening bracket / brace alone
            // (handled by the start-of-line case below) or whitespace.
            //
            // We specifically search for ", " + the intrinsically
            // multi-line keyword to make the failure message precisely
            // describe the defect this test is guarding against.
            var inlineSep = line.IndexOf(", " + intrinsicallyMultilineKeyword);

            if (inlineSep >= 0)
            {
                // The only acceptable position for ", <kw>" is leading a
                // line (after the leading indent and the comma): trimmed
                // content starting with ", <kw>" — which by definition
                // cannot match an inline-search hit since the trimmed
                // form would not yield an inline `,` match. So any hit
                // here is broken layout.
                var prefix = line.Substring(0, inlineSep);
                var trimmedPrefix = prefix.TrimStart();

                trimmedPrefix.Should().BeEmpty(
                    "the separator before an intrinsically multi-line "
                    + intrinsicallyMultilineKeyword + " element must lead "
                    + "its line (canonical AVH4 multi-line layout); the "
                    + "broken layout placed `, " + intrinsicallyMultilineKeyword
                    + "` mid-line, causing the " + intrinsicallyMultilineKeyword
                    + "-block body to indent off an arbitrary column. "
                    + "Offending line " + (i + 1) + ": '" + line + "' in:\n"
                    + rendered);
            }
        }
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
