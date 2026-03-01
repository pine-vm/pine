using AwesomeAssertions;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Inlining = Core.Elm.ElmCompilerInDotnet.Inlining;

public class InliningPipelineTests
{
    private static string InlineAndRenderSingleModule(
        string elmModuleText,
        IReadOnlyList<string> moduleName,
        Inlining.Config config)
    {
        var appModule =
            InliningTestHelper.CanonicalizeAndInlineAndGetSingleModule(
                [
                elmModuleText,
                ],
                moduleName,
                config);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(appModule);

        return rendered;
    }

    [Fact]
    public void Single_forward_pipe()
    {
        // 5 |> double  →  double 5

        var elmModuleText =
            """"
            module App exposing (..)

            double y =
                Pine_kernel.int_multiply [ y, 2 ]

            result =
                5 |> double

            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        var expectedElmModuleText =
            """"
            module App exposing (..)


            double y =
                Pine_kernel.int_multiply
                    [ y, 2 ]


            result =
                App.double
                    5
            """";

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }

    [Fact]
    public void Multiple_forward_pipes()
    {
        // 5 |> increment |> double  →  double (increment 5)

        var elmModuleText =
            """"
            module App exposing (..)

            increment y =
                Pine_kernel.int_add [ y, 1 ]

            double z =
                Pine_kernel.int_multiply [ z, 2 ]

            result =
                5
                    |> increment
                    |> double

            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        var expectedElmModuleText =
            """"
            module App exposing (..)


            increment y =
                Pine_kernel.int_add
                    [ y, 1 ]


            double z =
                Pine_kernel.int_multiply
                    [ z, 2 ]


            result =
                App.double
                    (App.increment
                        5
                    )
            """";

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }

    [Fact]
    public void Single_backward_pipe()
    {
        // double <| 5  →  double 5

        var elmModuleText =
            """"
            module App exposing (..)

            double y =
                Pine_kernel.int_multiply [ y, 2 ]

            result =
                double <| 5

            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        var expectedElmModuleText =
            """"
            module App exposing (..)


            double y =
                Pine_kernel.int_multiply
                    [ y, 2 ]


            result =
                App.double
                    5
            """";

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }

    [Fact]
    public void Multiple_backward_pipes()
    {
        // double <| increment <| 5  →  double (increment 5)

        var elmModuleText =
            """"
            module App exposing (..)

            increment y =
                Pine_kernel.int_add [ y, 1 ]

            double z =
                Pine_kernel.int_multiply [ z, 2 ]

            result =
                double
                    <| increment
                    <| 5

            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        var expectedElmModuleText =
            """"
            module App exposing (..)


            increment y =
                Pine_kernel.int_add
                    [ y, 1 ]


            double z =
                Pine_kernel.int_multiply
                    [ z, 2 ]


            result =
                App.double
                    (App.increment
                        5
                    )
            """";

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }

    [Fact]
    public void Single_compose_right()
    {
        // increment >> double  →  \composeArg -> double (increment composeArg)

        var elmModuleText =
            """"
            module App exposing (..)

            increment y =
                Pine_kernel.int_add [ y, 1 ]

            double z =
                Pine_kernel.int_multiply [ z, 2 ]

            composed =
                increment >> double

            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        var expectedElmModuleText =
            """"
            module App exposing (..)


            increment y =
                Pine_kernel.int_add
                    [ y, 1 ]


            double z =
                Pine_kernel.int_multiply
                    [ z, 2 ]


            composed =
                \composeArg ->
                    App.double
                        (App.increment
                            composeArg
                        )
            """";

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }

    [Fact]
    public void Single_compose_left()
    {
        // double << increment  →  \composeArg -> double (increment composeArg)

        var elmModuleText =
            """"
            module App exposing (..)

            increment y =
                Pine_kernel.int_add [ y, 1 ]

            double z =
                Pine_kernel.int_multiply [ z, 2 ]

            composed =
                double << increment

            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        var expectedElmModuleText =
            """"
            module App exposing (..)


            increment y =
                Pine_kernel.int_add
                    [ y, 1 ]


            double z =
                Pine_kernel.int_multiply
                    [ z, 2 ]


            composed =
                \composeArg ->
                    App.double
                        (App.increment
                            composeArg
                        )
            """";

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }
}
