using AwesomeAssertions;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Inlining = Core.Elm.ElmCompilerInDotnet.Inlining;

public class InliningSimpleTests
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
    public void Direct_substitution_no_new_function()
    {
        // apply f x = f x
        // result = apply (\y -> y * 2) 5
        // After inlining apply and beta-reducing: result = y * 2 [y := 5] = int_multiply [5, 2]

        var elmModuleText =
            """"
            module App exposing (..)

            apply f x =
                f x

            result =
                apply (\y -> Pine_kernel.int_multiply [ y, 2 ]) 5

            """";

        var expectedElmModuleText =
            """"
            App.apply f x =
                f
                    x


            App.result =
                Pine_kernel.int_multiply
                    [ 5, 2 ]
            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }

    [Fact]
    public void Multiple_function_arguments()
    {
        // combine f g x = f (g x)
        // result = combine (\a -> a + 1) (\b -> b * 2) 3
        // After inlining combine and beta-reducing both lambdas:
        // (\a -> a + 1) ((\b -> b * 2) 3) → (\a -> a + 1) (int_multiply [3, 2]) → int_add [int_multiply [3, 2], 1]

        var elmModuleText =
            """"
            module App exposing (..)
            
            
            combine f g x =
                f (g x)


            result =
                combine
                    (\a -> Pine_kernel.int_add [ a, 1 ])
                    (\b -> Pine_kernel.int_multiply [ b, 2 ])
                    3
            """";

        var expectedElmModuleText =
            """"
            App.combine f g x =
                f
                    (g
                        x
                    )


            App.result =
                Pine_kernel.int_add
                    [ (Pine_kernel.int_multiply
                        [ 3, 2 ]
                      ), 1 ]
            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedElmModuleText.Trim());
    }
}
