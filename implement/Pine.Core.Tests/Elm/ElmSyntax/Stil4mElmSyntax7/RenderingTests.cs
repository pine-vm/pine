using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mElmSyntax7;

public class RenderingTests
{
    private static readonly Core.Elm.ElmSyntax.Stil4mElmSyntax7.Rendering.Config s_renderingDefaultConfig =
        Core.Elm.ElmSyntax.Stil4mElmSyntax7.Rendering.ConfigNormalizeAllLocations(
            Core.Elm.ElmSyntax.Stil4mElmSyntax7.Rendering.LineBreakingConfig.SnapshotTestsDefault);

    private static string RenderDefault(Core.Elm.ElmSyntax.Stil4mElmSyntax7.File file) =>
        Core.Elm.ElmSyntax.Stil4mElmSyntax7.Rendering.ToString(
            file,
            s_renderingDefaultConfig);

    [Fact]
    public void ToString_EmptyFile()
    {
        var file =
            new Core.Elm.ElmSyntax.Stil4mElmSyntax7.File(
                ModuleDefinition:
                NodeWithRangeZero<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Module>(
                    new Core.Elm.ElmSyntax.Stil4mElmSyntax7.Module.NormalModule(
                        ModuleData: new Core.Elm.ElmSyntax.Stil4mElmSyntax7.DefaultModuleData(
                            ModuleName: NodeWithRangeZero((IReadOnlyList<string>)["Test"]),
                            ExposingList: NodeWithRangeZero<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Exposing>(
                                new Core.Elm.ElmSyntax.Stil4mElmSyntax7.Exposing.All(s_fakeRangeZero))
                    )
                )),
                Imports: [],
                Declarations: [],
                Comments: []);

        var rendered = RenderDefault(file);

        rendered.Trim().Should().Be("module Test exposing (..)");
    }

    [Fact]
    public void Parse_and_render_scenarios()
    {
        var testCases = new[]
        {
            new
            {
                Input =
                """
                module Test exposing (..)
                """,

                Expected =
                """
                module Test exposing (..)
                """
            },

            new
            {
                Input =
                """
                module   Test   exposing   (  ..  )
                """,

                Expected =
                """
                module Test exposing (..)
                """
            },

            new
            {
                Input =
                """
                module Test exposing (..)

                import Html exposing (text)

                main =
                    text "Hello, World!"

                beta = 42
                """,

                Expected =
                """
                module Test exposing (..)

                import Html exposing (text)


                main =
                    text "Hello, World!"


                beta =
                    42
                """
            },

            new
            {
                Input =
                """
                module Test exposing (..)


                sketch a =
                    func "test" (a + 3)

                """,

                Expected =
                """
                module Test exposing (..)


                sketch a =
                    func
                        "test"
                        (a + 3)
                """,
            },
        };

        for (var i = 0; i < testCases.Length; i++)
        {
            var testCase = testCases[i];

            try
            {
                var parsed =
                    ElmSyntaxParser.ParseModuleText(testCase.Input.TrimStart())
                    .Extract(err => throw new System.Exception("Parsing failed: " + err.ToString()));

                var rendered = RenderDefault(parsed);

                rendered.Trim().Should().Be(testCase.Expected.Trim());
            }
            catch (System.Exception e)
            {
                throw new System.Exception(
                    $"Test case {i} failed during parsing. Input:\n{testCase.Input}", e);
            }
        }
    }

    private static Core.Elm.ElmSyntax.Stil4mElmSyntax7.Node<T> NodeWithRangeZero<T>(T value) =>
        new(Range: s_fakeRangeZero, Value: value);

    private static readonly Core.Elm.ElmSyntax.Stil4mElmSyntax7.Location s_fakeLocationZero =
        new(Row: 0, Column: 0);

    private static readonly Core.Elm.ElmSyntax.Stil4mElmSyntax7.Range s_fakeRangeZero =
        new(Start: s_fakeLocationZero, End: s_fakeLocationZero);
}
