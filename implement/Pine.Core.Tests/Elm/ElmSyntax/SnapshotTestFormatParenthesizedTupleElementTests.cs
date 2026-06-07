using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmSyntax;

/// <summary>
/// Regression tests for a snapshot-formatter defect first observed in the
/// auto-generated tracking report
/// <c>explore/internal-analysis/2026-05-15-monomorphization-reachable-higher-order-parameters-for-parse-file.md</c>:
/// when a tuple element is a parenthesized application (or any other
/// "complex" expression wrapped in parens), <see cref="SnapshotTestFormat"/>
/// fails to switch the containing tuple to multi-line layout. The result
/// is a broken hybrid in which the outer tuple is rendered single-line
/// with <c>", "</c> separators but the inner formatted content spans
/// multiple rows, leading to elements wrapping mid-line at large
/// column offsets.
/// <para>
/// These tests build the offending Stil4m abstract-syntax shape directly
/// (mirroring what the lowering / monomorphization pipeline produces with
/// dummy ranges) and assert that the rendered output uses canonical
/// AVH4 multi-line tuple layout — each element on its own line, separator
/// at the alignment column.
/// </para>
/// </summary>
public class SnapshotTestFormatParenthesizedTupleElementTests
{
    [Fact]
    public void Tuple_with_parenthesized_application_element_renders_canonical_multi_line()
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

                    WrapNode(
                        (SyntaxTypes.Expression)
                        new SyntaxTypes.Expression.ParenthesizedExpression(
                            WrapNode(
                                (SyntaxTypes.Expression)
                                new SyntaxTypes.Expression.Application(
                                    Arguments:
                                    [
                                    WrapNode(
                                        (SyntaxTypes.Expression)
                                        new SyntaxTypes.Expression.FunctionOrValue([], "g")),
                                    WrapNode(
                                        (SyntaxTypes.Expression)
                                        new SyntaxTypes.Expression.FunctionOrValue([], "x")),
                                    WrapNode(
                                        (SyntaxTypes.Expression)
                                        new SyntaxTypes.Expression.FunctionOrValue([], "y")),
                                    WrapNode(
                                        (SyntaxTypes.Expression)
                                        new SyntaxTypes.Expression.FunctionOrValue([], "z")),
                                    ])))),

                    WrapNode(
                        (SyntaxTypes.Expression)
                        new SyntaxTypes.Expression.FunctionOrValue([], "b")),
                    ]));

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclaration(
                declQualifiedName,
                SyntaxTypes.ToFullSyntaxModel.Convert(declaration));

        // We want canonical AVH4 multi-line tuple layout when at least one
        // element is itself multi-line in the rendered output. Every
        // separator (`, ` or `,`) between elements must therefore appear
        // at the start of a line (after indentation), not in the middle
        // of a line. The previously-broken output put the second `, b`
        // on the same line as the closing paren of the inner application
        // many columns into the page.
        //
        // The two strict invariants we assert:
        //   1. The output spans multiple lines (not a single long row).
        //   2. No separator-then-newline-then-separator pattern is needed:
        //      the closing `)` of the inner application must not be
        //      followed on the same line by `, ` introducing the next
        //      tuple element.
        rendered.Should().Contain("\n");

        // Find the inner application's closing `)` and verify the
        // following text either ends the tuple or starts on a new line.
        // The broken output looked like:
        //   ( a, (g
        //          x
        //          y
        //          z
        //       ), b )
        // i.e. `), b )` on the same line. Canonical AVH4 has each
        // tuple element on its own line, with separators leading the
        // line.
        var lines = rendered.Split('\n');

        for (var i = 0; i < lines.Length; i++)
        {
            var line = lines[i];

            // After the inner application's closing paren, the only
            // non-whitespace continuation we accept is the closing `)`
            // of the outer tuple itself.
            var idx = line.IndexOf("), ");

            idx.Should().BeLessThan(
                0,
                "tuple separators must not appear after a closing paren on the same line; " +
                "canonical AVH4 multi-line tuple puts each element on its own line. " +
                "Offending line " + (i + 1) + ": '" + line + "' in:\n" + rendered);
        }
    }

    [Fact]
    public void List_with_parenthesized_application_element_renders_canonical_multi_line()
    {
        // Companion test to the tuple case: lists exhibit the same
        // defect because ShouldFormatListAsMultiline shares the same
        // bare-kind classification logic.
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

                    WrapNode(
                        (SyntaxTypes.Expression)
                        new SyntaxTypes.Expression.ParenthesizedExpression(
                            WrapNode(
                                (SyntaxTypes.Expression)
                                new SyntaxTypes.Expression.Application(
                                    Arguments:
                                    [
                                    WrapNode(
                                        (SyntaxTypes.Expression)
                                        new SyntaxTypes.Expression.FunctionOrValue([], "g")),
                                    WrapNode(
                                        (SyntaxTypes.Expression)
                                        new SyntaxTypes.Expression.FunctionOrValue([], "x")),
                                    WrapNode(
                                        (SyntaxTypes.Expression)
                                        new SyntaxTypes.Expression.FunctionOrValue([], "y")),
                                    WrapNode(
                                        (SyntaxTypes.Expression)
                                        new SyntaxTypes.Expression.FunctionOrValue([], "z")),
                                    ])))),

                    WrapNode(
                        (SyntaxTypes.Expression)
                        new SyntaxTypes.Expression.FunctionOrValue([], "b")),
                    ]));

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclaration(
                declQualifiedName,
                SyntaxTypes.ToFullSyntaxModel.Convert(declaration));

        rendered.Should().Contain("\n");

        var lines = rendered.Split('\n');

        for (var i = 0; i < lines.Length; i++)
        {
            var line = lines[i];

            var idx = line.IndexOf("), ");

            idx.Should().BeLessThan(
                0,
                "list separators must not appear after a closing paren on the same line; " +
                "canonical AVH4 multi-line list puts each element on its own line. " +
                "Offending line " + (i + 1) + ": '" + line + "' in:\n" + rendered);
        }
    }

    /// <summary>
    /// Builds a minimal Stil4m <see cref="SyntaxTypes.Declaration.FunctionDeclaration"/>
    /// with no signature and no arguments, returning the supplied body
    /// expression. All locations are set to the dummy origin used by the
    /// post-lowering declaration dictionary that <see cref="SnapshotTestFormat"/>
    /// is normally invoked against.
    /// </summary>
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
