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

        var formatted =
            Core.Elm.ElmSyntax.Stil4mElmSyntax7.SnapshotTestFormat.Format(appModule);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(formatted);

        return rendered;
    }

    [Fact]
    public void Simple_identity_function_inline()
    {
        // identity f = f
        // app x = identity (\y -> y + 1) x
        // After inlining: app x = (\y -> y + 1) x

        var elmModuleText =
            """"
            module App exposing (..)


            identity f =
                f

            app x =
                identity (\y -> Pine_kernel.int_add [ y, 1 ]) x

            """";

        var expectedElmModuleText =
            """"
            module App exposing (..)


            identity f =
                f


            app x =
                (\y ->
                    Pine_kernel.int_add
                        [ y
                        , 1
                        ]
                )
                    x
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
    public void Direct_substitution_no_new_function()
    {
        // apply f x = f x
        // result = apply (\y -> y * 2) 5
        // After inlining: result = (\y -> y * 2) 5

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
            module App exposing (..)


            apply f x =
                f
                    x


            result =
                (\y ->
                    Pine_kernel.int_multiply
                        [ y
                        , 2
                        ]
                )
                    5
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
        // After inlining: result = (\a -> a + 1) ((\b -> b * 2) 3)

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
            module App exposing (..)


            combine f g x =
                f
                    (g
                        x
                    )


            result =
                (\a ->
                    Pine_kernel.int_add
                        [ a
                        , 1
                        ]
                )
                    ((\b ->
                        Pine_kernel.int_multiply
                            [ b
                            , 2
                            ]
                     )
                        3
                    )
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
