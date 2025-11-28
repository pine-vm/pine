using AwesomeAssertions;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Inlining = Core.Elm.ElmCompilerInDotnet.Inlining;

public class InliningRecursiveCrossModuleTests
{
    [Fact(Skip = "Recursive functions not supported for now")]
    public void List_map_Variant_1()
    {
        var listModuleText =
            """"
            module TestList exposing (..)


            mapHelp : (a -> b) -> List a -> List b -> List b
            mapHelp f remaining acc =
                case remaining of
                    x :: xs ->
                        mapHelp f xs (Pine_kernel.concat [ [ f x ], acc ])

                    _ ->
                        Pine_kernel.reverse acc

            """";

        var appModuleText =
            """"
            module App exposing (..)

            import TestList exposing (..)


            mapInst : List a -> List (List a)
            mapInst xs =
                mapHelp (\x -> [ x, x ]) xs []

            """";

        var expectedAppModuleText =
            """"            
            module App exposing (..)


            mapInst : List a -> List (List a)
            mapInst xs =
                mapInst_mapHelp_1 xs []


            mapInst_mapHelp_1 : List a -> List b -> List b
            mapInst_mapHelp_1 remaining acc =
                case remaining of
                    x :: xs ->
                        mapInst_mapHelp_1
                            xs
                            (Pine_kernel.concat
                                [ [ x, x ]
                                , acc
                                ]
                            )

                    _ ->
                        Pine_kernel.reverse
                            acc

            """";

        var appModule =
            InliningTestHelper.CanonicalizeAndInlineAndGetSingleModule(
                [
                listModuleText,
                appModuleText,
                ],
                ["App"],
                Inlining.Config.OnlyFunctions);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(appModule);

        rendered.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }
}
