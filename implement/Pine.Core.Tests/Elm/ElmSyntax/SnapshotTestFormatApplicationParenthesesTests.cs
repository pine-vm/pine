using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmSyntax;

/// <summary>
/// Regression tests for two <see cref="SnapshotTestFormat"/> /
/// <see cref="Avh4Format"/> rendering defects that surface when the syntax
/// tree being rendered was produced by a compiler pass (rather than parsed
/// from source) and therefore does NOT carry explicit
/// <c>ParenthesizedExpression</c> nodes around compound application
/// arguments or nested function-position applications:
///
/// <list type="number">
/// <item><description>
/// <b>Missing parentheses (no roundtrip).</b> An application whose argument
/// is itself a compound expression (for example another application) was
/// rendered without the parentheses that the argument requires. For
/// example <c>head (skip xs)</c> was rendered as <c>head skip xs</c>, which
/// re-parses as the three-argument application <c>head skip xs</c> — a
/// different expression. The rendered string therefore does not roundtrip.
/// </description></item>
/// <item><description>
/// <b>Wrong indentation.</b> Because the missing parentheses also removed
/// the grouping the layout engine relies on, the nested arguments were laid
/// out at the wrong indentation, inconsistent with avh4/elm-format.
/// </description></item>
/// </list>
///
/// <para>The fix is <see cref="ApplicationParenthesesNormalization"/>, run
/// as the first step of <see cref="SnapshotTestFormat.Format"/>: it inserts
/// the parentheses that compound application arguments require and flattens
/// redundant function-position parentheses (<c>(f a) b</c> → <c>f a b</c>),
/// matching elm-format. With the parentheses restored, the existing
/// avh4-faithful layout logic produces the correct indentation
/// automatically.</para>
/// </summary>
public class SnapshotTestFormatApplicationParenthesesTests
{
    [Fact]
    public void Compound_application_argument_is_parenthesized_and_indented_like_elm_format()
    {
        // Build, with NO explicit ParenthesizedExpression nodes (as a
        // compiler pass would emit):
        //
        //   f = Pine_kernel.head (Pine_kernel.skip [ 0, Pine_kernel.head (Pine_kernel.skip [ 1, state ]) ]) state

        var inner =
            BuildApplication(
                BuildQualified(["Pine_kernel"], "head"),
                BuildApplication(
                    BuildQualified(["Pine_kernel"], "skip"),
                    BuildListExpr(
                        BuildIntLiteral(1),
                        BuildQualified([], "state"))));

        var outer =
            BuildApplication(
                BuildQualified(["Pine_kernel"], "head"),
                BuildApplication(
                    BuildQualified(["Pine_kernel"], "skip"),
                    BuildListExpr(
                        BuildIntLiteral(0),
                        inner)),
                BuildQualified([], "state"));

        var declaration = BuildFunctionDeclarationReturning("f", outer);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclaration(
                DeclQualifiedName.Create(["TestModule"], "f"),
                SyntaxTypes.ToFullSyntaxModel.Convert(declaration));

        // Defect 1: the compound `skip [...]` argument of `head` must be
        // parenthesized, otherwise the string does not roundtrip.
        rendered.Should().Contain(
            "(Pine_kernel.skip",
            "a compound application argument must be parenthesized so the "
            + "rendered string roundtrips.");

        rendered.Should().NotContain(
            "Pine_kernel.head Pine_kernel.skip",
            "without parentheses the application re-parses with a different "
            + "structure; the rendering would not roundtrip.");

        // Defect 2: the resulting layout must match avh4/elm-format. With
        // the parentheses present, every nested argument sits on its own
        // line at the application's argument indent column, and the opening
        // paren of the multi-line argument is on its own line.
        var expected =
            """
            TestModule.f =
                Pine_kernel.head
                    (Pine_kernel.skip
                        [ 0
                        , Pine_kernel.head
                            (Pine_kernel.skip
                                [ 1, state ]
                            )
                        ]
                    )
                    state
            """;

        rendered.Trim().Should().Be(expected.Trim());
    }

    [Fact]
    public void Nested_function_position_application_is_flattened_like_elm_format()
    {
        // Build, with NO explicit parens: (f a) b  — elm-format flattens
        // the redundant function-position parens to `f a b`.

        var flattened =
            BuildApplication(
                BuildApplication(
                    BuildQualified([], "f"),
                    BuildQualified([], "a")),
                BuildQualified([], "b"));

        var declaration = BuildFunctionDeclarationReturning("g", flattened);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclaration(
                DeclQualifiedName.Create(["TestModule"], "g"),
                SyntaxTypes.ToFullSyntaxModel.Convert(declaration));

        rendered.Should().NotContain(
            "(f",
            "elm-format flattens redundant function-position parentheses: "
            + "`(f a) b` renders as `f a b`.");

        var expected =
            """
            TestModule.g =
                f
                    a
                    b
            """;

        rendered.Trim().Should().Be(expected.Trim());
    }

    private static SyntaxTypes.Expression BuildApplication(
        SyntaxTypes.Expression function,
        params SyntaxTypes.Expression[] arguments)
    {
        var nodes = new System.Collections.Generic.List<Node<SyntaxTypes.Expression>>
        {
            WrapNode(function),
        };

        foreach (var argument in arguments)
        {
            nodes.Add(WrapNode(argument));
        }

        return new SyntaxTypes.Expression.Application(nodes);
    }

    private static SyntaxTypes.Expression BuildQualified(
        System.Collections.Generic.IReadOnlyList<string> moduleName,
        string name) =>
        new SyntaxTypes.Expression.FunctionOrValue(moduleName, name);

    private static SyntaxTypes.Expression BuildIntLiteral(int value) =>
        new SyntaxTypes.Expression.Integer(value);

    private static SyntaxTypes.Expression BuildListExpr(
        SyntaxTypes.Expression first,
        SyntaxTypes.Expression second) =>
        new SyntaxTypes.Expression.ListExpr(
            [
            WrapNode(first),
            WrapNode(second),
            ]);

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
