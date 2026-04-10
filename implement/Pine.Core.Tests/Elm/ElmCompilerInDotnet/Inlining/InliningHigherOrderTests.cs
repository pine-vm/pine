using AwesomeAssertions;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Inlining = Core.Elm.ElmCompilerInDotnet.Inlining;

/// <summary>
/// Tests for higher-order function specialization in the inlining stage.
/// These tests verify the planned optimization described in InliningExpansionAnalysis.md:
/// when a recursive higher-order function is called with a known function argument,
/// the inliner creates a specialized first-order copy where the function parameter
/// is replaced with the concrete function.
/// <para>
/// Phase 1 tests (single and dual specialization) pass.
/// Tests for later phases (2, 3, 5a) contain aspirational expected output and
/// are expected to fail until their corresponding specialization phases are implemented.
/// </para>
/// </summary>
public class InliningHigherOrderTests
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

    private static string InlineAndRenderModule(
        IReadOnlyList<string> elmModulesTexts,
        IReadOnlyList<string> moduleName,
        Inlining.Config config)
    {
        var appModule =
            InliningTestHelper.CanonicalizeAndInlineAndGetSingleModule(
                elmModulesTexts,
                moduleName,
                config);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(appModule);

        return rendered;
    }

    /// <summary>
    /// Phase 1: Specialization of a recursive higher-order function called with
    /// a known function argument.
    /// <para>
    /// <c>listMap increment list</c> should produce a specialized
    /// <c>listMap__specialized__1</c> where the <c>f</c> parameter is replaced
    /// by the concrete <c>increment</c> function. The specialized function is
    /// first-order and self-recursive, enabling <c>Invoke_StackFrame_Const</c>
    /// optimization downstream.
    /// </para>
    /// </summary>
    [Fact]
    public void List_map_with_function_parameter()
    {
        var helpersModuleText =
            """"
            module Helpers exposing (..)


            listMap : (a -> b) -> List a -> List b
            listMap f list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        f first :: listMap f rest

            """";

        var appModuleText =
            """"
            module App exposing (..)

            import Helpers


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add [ n, 1 ]


            result : List Int -> List Int
            result list =
                Helpers.listMap increment list

            """";

        // After specialization: a new first-order function is generated
        // where 'f' is replaced with 'increment', and recursive calls
        // target the specialized version.
        var expectedAppModuleText =
            """"
            module App exposing (..)


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add
                    [ n, 1 ]


            result : List.List Int -> List.List Int
            result list =
                listMap__specialized__1
                    list


            listMap__specialized__1 list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        List.cons
                            (App.increment
                                first
                            )
                            (listMap__specialized__1
                                rest
                            )
            """";

        var rendered =
            InlineAndRenderModule(
                [helpersModuleText, appModuleText],
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }

    /// <summary>
    /// Phase 1: Dual specialization - two calls to the same recursive higher-order
    /// function with different function arguments produce two distinct specialized copies.
    /// <para>
    /// <c>listMap increment list</c> and <c>listMap double list</c> each get their own
    /// specialized first-order recursive function, distinguished by index.
    /// </para>
    /// </summary>
    [Fact]
    public void Dual_list_map_with_different_functions()
    {
        var helpersModuleText =
            """"
            module Helpers exposing (..)


            listMap : (a -> b) -> List a -> List b
            listMap f list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        f first :: listMap f rest

            """";

        var appModuleText =
            """"
            module App exposing (..)

            import Helpers


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add [ n, 1 ]


            double : Int -> Int
            double n =
                Pine_kernel.int_mul [ n, 2 ]


            result : List Int -> List (List Int)
            result list =
                [ Helpers.listMap increment list
                , Helpers.listMap double list
                ]

            """";

        // After specialization: two specialized functions are generated,
        // one for 'double' (index 1, alphabetically first) and one for 'increment' (index 2).
        var expectedAppModuleText =
            """"
            module App exposing (..)


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add
                    [ n, 1 ]


            double : Int -> Int
            double n =
                Pine_kernel.int_mul
                    [ n, 2 ]


            result : List.List Int -> List.List List.List Int
            result list =
                [ listMap__specialized__2
                    list
                , listMap__specialized__1
                    list
                ]


            listMap__specialized__2 list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        List.cons
                            (App.increment
                                first
                            )
                            (listMap__specialized__2
                                rest
                            )


            listMap__specialized__1 list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        List.cons
                            (App.double
                                first
                            )
                            (listMap__specialized__1
                                rest
                            )
            """";

        var rendered =
            InlineAndRenderModule(
                [helpersModuleText, appModuleText],
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }

    /// <summary>
    /// Phase 3: Closure bound in a let expression. The inliner should propagate
    /// the let-bound closure and inline its application.
    /// <para>
    /// <c>let add5 = makeAdder 5 in add5 x</c> should be reduced to
    /// <c>Pine_kernel.int_add [ 5, x ]</c> after inlining <c>makeAdder</c>
    /// and beta-reducing the closure application.
    /// </para>
    /// <para>
    /// This test will fail until Phase 3 (closure propagation and beta-reduction)
    /// is implemented. The expected output represents the ideal specialized result.
    /// </para>
    /// </summary>
    [Fact]
    public void Closure_application()
    {
        var appModuleText =
            """"
            module App exposing (..)


            makeAdder : Int -> (Int -> Int)
            makeAdder n =
                \x -> Pine_kernel.int_add [ n, x ]


            result : Int -> Int
            result x =
                let
                    add5 =
                        makeAdder 5
                in
                add5 x

            """";

        // After let-propagation and beta-reduction:
        // makeAdder 5 => \x -> Pine_kernel.int_add [ 5, x ]
        // (\x -> Pine_kernel.int_add [ 5, x ]) x => Pine_kernel.int_add [ 5, x ]
        var expectedAppModuleText =
            """"
            module App exposing (..)


            makeAdder : Int -> Int -> Int
            makeAdder n =
                \x ->
                    Pine_kernel.int_add
                        [ n
                        , x
                        ]


            result : Int -> Int
            result x =
                Pine_kernel.int_add
                    [ 5, x ]

            """";

        var rendered =
            InlineAndRenderSingleModule(
                appModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }

    /// <summary>
    /// Phase 2: Mutual recursion group specialization.
    /// <c>listMap_a</c> and <c>listMap_b</c> are mutually recursive, each receiving
    /// two function parameters that are loop-invariant. Specialization produces a
    /// pair of first-order mutually recursive functions with function params eliminated.
    /// <para>
    /// This test will fail until Phase 2 (mutual recursion group specialization)
    /// is implemented. The expected output represents the ideal specialized result.
    /// </para>
    /// </summary>
    [Fact]
    public void Mutual_recursive_list_map_with_function_parameters()
    {
        var helpersModuleText =
            """"
            module Helpers exposing (..)


            listMap_a : (a -> b) -> (a -> b) -> List a -> List b
            listMap_a mapFirst mapSecond list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        mapFirst first :: listMap_b mapFirst mapSecond rest


            listMap_b : (a -> b) -> (a -> b) -> List a -> List b
            listMap_b mapFirst mapSecond list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        mapSecond first :: listMap_a mapFirst mapSecond rest

            """";

        var appModuleText =
            """"
            module App exposing (..)

            import Helpers


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add [ n, 1 ]


            double : Int -> Int
            double n =
                Pine_kernel.int_mul [ n, 2 ]


            result : List Int -> List Int
            result list =
                Helpers.listMap_a increment double list

            """";

        // After mutual recursion group specialization:
        // A pair of specialized first-order functions replaces the generic
        // mutually recursive pair. Both mapFirst and mapSecond are substituted.
        var expectedAppModuleText =
            """"
            module App exposing (..)


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add
                    [ n, 1 ]


            double : Int -> Int
            double n =
                Pine_kernel.int_mul
                    [ n, 2 ]


            result : List.List Int -> List.List Int
            result list =
                listMap_a__specialized__1
                    list


            listMap_a__specialized__1 list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        List.cons
                            (App.increment
                                first
                            )
                            (listMap_b__specialized__1
                                rest
                            )


            listMap_b__specialized__1 list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        List.cons
                            (App.double
                                first
                            )
                            (listMap_a__specialized__1
                                rest
                            )
            """";

        var rendered =
            InlineAndRenderModule(
                [helpersModuleText, appModuleText],
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }

    /// <summary>
    /// Phase 5a: Function wrapped in a single-tag choice type.
    /// <c>listMapTagged (TaggedFunc increment) list</c> should specialize the recursive
    /// function, and the <c>case taggedFunc of TaggedFunc f -> ...</c> should resolve
    /// statically since the concrete structured value is known.
    /// <para>
    /// This test will fail until Phase 5a (tagged function unwrapping)
    /// is implemented. The expected output represents the ideal specialized result.
    /// </para>
    /// </summary>
    [Fact]
    public void Tagged_function_list_map()
    {
        var helpersModuleText =
            """"
            module Helpers exposing (..)


            type TaggedFunc a b
                = TaggedFunc (a -> b)


            listMapTagged : TaggedFunc a b -> List a -> List b
            listMapTagged taggedFunc list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        case taggedFunc of
                            TaggedFunc f ->
                                f first :: listMapTagged taggedFunc rest

            """";

        var appModuleText =
            """"
            module App exposing (..)

            import Helpers


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add [ n, 1 ]


            result : List Int -> List Int
            result list =
                Helpers.listMapTagged (Helpers.TaggedFunc increment) list

            """";

        // After specialization with tag unwrapping:
        // The case expression resolves statically because taggedFunc is known
        // to be TaggedFunc increment, so f becomes increment directly.
        // The resulting specialized function is structurally identical to
        // the plain listMap_increment case.
        var expectedAppModuleText =
            """"
            module App exposing (..)


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add
                    [ n, 1 ]


            result : List.List Int -> List.List Int
            result list =
                listMapTagged__specialized__1
                    list


            listMapTagged__specialized__1 list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        List.cons
                            (App.increment
                                first
                            )
                            (listMapTagged__specialized__1
                                rest
                            )
            """";

        var rendered =
            InlineAndRenderModule(
                [helpersModuleText, appModuleText],
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }

    [Fact]
    public void List_map_with_record_access_function_parameter()
    {
        var helpersModuleText =
            """"
            module Helpers exposing (..)


            type alias Ops a b =
                { transform : a -> b
                }


            listMapViaAccessor : (Ops a b -> a -> b) -> Ops a b -> List a -> List b
            listMapViaAccessor accessor ops list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        accessor ops first :: listMapViaAccessor accessor ops rest
            """";

        var appModuleText =
            """"
            module App exposing (..)

            import Helpers


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add [ n, 1 ]


            ops : Helpers.Ops Int Int
            ops =
                { transform = increment
                }


            result : List Int -> List Int
            result list =
                Helpers.listMapViaAccessor .transform ops list
            """";

        var expectedAppModuleText =
            """"
            module App exposing (..)


            increment : Int -> Int
            increment n =
                Pine_kernel.int_add
                    [ n, 1 ]


            ops : Helpers.Ops Int Int
            ops =
                { transform = App.increment
                }


            result : List.List Int -> List.List Int
            result list =
                listMapViaAccessor__specialized__1
                    list


            listMapViaAccessor__specialized__1 list =
                case list of
                    [] ->
                        []

                    first :: rest ->
                        List.cons
                            (App.increment
                                first
                            )
                            (listMapViaAccessor__specialized__1
                                rest
                            )
            """";

        var rendered =
            InlineAndRenderModule(
                [helpersModuleText, appModuleText],
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }

    [Fact]
    public void Parser_wrapper_alias_pattern_loop()
    {
        var helpersModuleText =
            """"
            module Helpers exposing (..)


            type Parser a
                = Parser (() -> a)


            loopMap : Parser a -> List b -> List a
            loopMap ((Parser parse) as parser) list =
                case list of
                    [] ->
                        []

                    _ :: rest ->
                        parse () :: loopMap parser rest
            """";

        var appModuleText =
            """"
            module App exposing (..)

            import Helpers


            constant : Int
            constant =
                41


            result : List String -> List Int
            result list =
                Helpers.loopMap (Helpers.Parser (\() -> constant)) list
            """";

        var expectedAppModuleText =
            """"
            module App exposing (..)


            constant : Int
            constant =
                41


            result : List.List String -> List.List Int
            result list =
                loopMap__specialized__1
                    list


            loopMap__specialized__1 list =
                case list of
                    [] ->
                        []

                    _ :: rest ->
                        List.cons
                            App.constant
                            (loopMap__specialized__1
                                rest
                            )
            """";

        var rendered =
            InlineAndRenderModule(
                [helpersModuleText, appModuleText],
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }

    [Fact]
    public void Parser_wrapper_alias_pattern_loop_via_top_level_value()
    {
        var helpersModuleText =
            """"
            module Helpers exposing (..)


            type Parser a
                = Parser (() -> a)


            loopMap : Parser a -> List b -> List a
            loopMap ((Parser parse) as parser) list =
                case list of
                    [] ->
                        []

                    _ :: rest ->
                        parse () :: loopMap parser rest
            """";

        var appModuleText =
            """"
            module App exposing (..)

            import Helpers


            constant : Int
            constant =
                41


            parser : Helpers.Parser Int
            parser =
                Helpers.Parser (\() -> constant)


            result : List String -> List Int
            result list =
                Helpers.loopMap parser list
            """";

        var expectedAppModuleText =
            """"
            module App exposing (..)


            constant : Int
            constant =
                41


            parser : Helpers.Parser Int
            parser =
                Helpers.Parser
                    (\() ->
                        App.constant
                    )


            result : List.List String -> List.List Int
            result list =
                loopMap__specialized__1
                    list


            loopMap__specialized__1 list =
                case list of
                    [] ->
                        []

                    _ :: rest ->
                        List.cons
                            App.constant
                            (loopMap__specialized__1
                                rest
                            )
            """";

        var rendered =
            InlineAndRenderModule(
                [helpersModuleText, appModuleText],
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }

    [Fact]
    public void Parser_wrapper_alias_pattern_loop_via_record_field()
    {
        var helpersModuleText =
            """"
            module Helpers exposing (..)


            type Parser a
                = Parser (() -> a)


            loopMap : Parser a -> List b -> List a
            loopMap ((Parser parse) as parser) list =
                case list of
                    [] ->
                        []

                    _ :: rest ->
                        parse () :: loopMap parser rest
            """";

        var appModuleText =
            """"
            module App exposing (..)

            import Helpers


            type alias Config =
                { parser : Helpers.Parser Int
                }


            constant : Int
            constant =
                41


            config : Config
            config =
                { parser = Helpers.Parser (\() -> constant)
                }


            result : List String -> List Int
            result list =
                Helpers.loopMap config.parser list
            """";

        var expectedAppModuleText =
            """"
            module App exposing (..)


            type alias Config =
                { parser : Helpers.Parser Int }


            constant : Int
            constant =
                41


            config : App.Config
            config =
                { parser =
                    Helpers.Parser
                        (\() ->
                            App.constant
                        )
                }


            result : List.List String -> List.List Int
            result list =
                loopMap__specialized__1
                    list


            loopMap__specialized__1 list =
                case list of
                    [] ->
                        []

                    _ :: rest ->
                        List.cons
                            App.constant
                            (loopMap__specialized__1
                                rest
                            )
            """";

        var rendered =
            InlineAndRenderModule(
                [helpersModuleText, appModuleText],
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            expectedAppModuleText.Trim());
    }
}
