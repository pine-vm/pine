using AwesomeAssertions;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Inlining = Core.Elm.ElmCompilerInDotnet.Inlining;

/// <summary>
/// Tests that transformations in ApplyOptimizationPipelineWithStageResults work correctly
/// for various orderings of declarations within let-blocks.
/// <para>
/// In Elm, let-block declarations can reference each other regardless of declaration order.
/// These tests verify that the inlining/specialization pipeline handles:
/// <list type="bullet">
/// <item>Forward references (earlier declarations depend on later ones)</item>
/// <item>Natural-order dependencies (later declarations depend on earlier ones)</item>
/// <item>Mutually recursive functions within let blocks</item>
/// </list>
/// </para>
/// </summary>
public class InliningLetBlockTests
{
    private static string InlineAndRenderSingleModule(
        string elmModuleText,
        IReadOnlyList<string> moduleName,
        Inlining.Config config)
    {
        var appModule =
            InliningTestHelper.CanonicalizeAndInlineAndGetSingleModule(
                [elmModuleText],
                moduleName,
                config);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(appModule);

        return rendered;
    }

    private static string OptimizeAndRenderSingleModule(
        string elmModuleText,
        IReadOnlyList<string> moduleName,
        Inlining.Config config)
    {
        var appModule =
            InliningTestHelper.CanonicalizeAndOptimizeAndGetSingleModule(
                [elmModuleText],
                moduleName,
                config);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(appModule);

        return rendered;
    }

    // ──────────────────────────────────────────────────────────
    // Natural order: later declarations depend on earlier ones
    // ──────────────────────────────────────────────────────────

    [Fact]
    public void Let_block_natural_order_value_chain()
    {
        // In this test, b depends on a, c depends on b — natural (top-down) order.
        var elmModuleText =
            """"
            module App exposing (..)


            result =
                let
                    a = 1

                    b = a

                    c = b
                in
                c
            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            """"
            App.result =
                let
                    a =
                        1

                    b =
                        a

                    c =
                        b
                in
                c
            """");
    }

    [Fact]
    public void Let_block_natural_order_function_calls()
    {
        // double depends on nothing, quadruple depends on double — natural order.
        var elmModuleText =
            """"
            module App exposing (..)


            result x =
                let
                    double y =
                        Pine_kernel.int_multiply [ y, 2 ]

                    quadruple z =
                        double (double z)
                in
                quadruple x
            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            """"
            App.result x =
                App.result__lifted__quadruple_2
                    x


            App.result__lifted__double_1 y =
                Pine_kernel.int_multiply
                    [ y, 2 ]


            App.result__lifted__quadruple_2 z =
                App.result__lifted__double_1
                    (App.result__lifted__double_1
                        z
                    )
            """");
    }

    // ──────────────────────────────────────────────────────────
    // Reverse order: earlier declarations depend on later ones
    // ──────────────────────────────────────────────────────────

    [Fact]
    public void Let_block_reverse_order_value_chain()
    {
        // In this test, a depends on b, b depends on c — reverse (bottom-up) order.
        // Elm allows forward references within let blocks.
        var elmModuleText =
            """"
            module App exposing (..)


            result =
                let
                    a = b

                    b = c

                    c = 1
                in
                a
            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            """"
            App.result =
                let
                    a =
                        b

                    b =
                        c

                    c =
                        1
                in
                a
            """");
    }

    [Fact]
    public void Let_block_reverse_order_function_calls()
    {
        // quadruple depends on double, but is declared before double.
        // Elm allows forward references within let blocks.
        var elmModuleText =
            """"
            module App exposing (..)


            result x =
                let
                    quadruple z =
                        double (double z)

                    double y =
                        Pine_kernel.int_multiply [ y, 2 ]
                in
                quadruple x
            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            """"
            App.result x =
                App.result__lifted__quadruple_1
                    x


            App.result__lifted__double_2 y =
                Pine_kernel.int_multiply
                    [ y, 2 ]


            App.result__lifted__quadruple_1 z =
                App.result__lifted__double_2
                    (App.result__lifted__double_2
                        z
                    )
            """");
    }

    // ──────────────────────────────────────────────────────────
    // Mixed order: interleaved dependencies
    // ──────────────────────────────────────────────────────────

    [Fact]
    public void Let_block_mixed_order_dependencies()
    {
        // a depends on c (forward ref), b depends on a (backward ref), c is independent.
        var elmModuleText =
            """"
            module App exposing (..)


            result x =
                let
                    a =
                        Pine_kernel.int_add [ c, 10 ]

                    b =
                        Pine_kernel.int_add [ a, 20 ]

                    c = x
                in
                b
            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            """"
            App.result x =
                let
                    a =
                        Pine_kernel.int_add
                            [ c, 10 ]

                    b =
                        Pine_kernel.int_add
                            [ a, 20 ]

                    c =
                        x
                in
                b
            """");
    }

    // ──────────────────────────────────────────────────────────
    // Mutually recursive functions in let blocks
    // ──────────────────────────────────────────────────────────

    [Fact]
    public void Let_block_mutually_recursive_functions()
    {
        // isEven and isOdd are mutually recursive.
        var elmModuleText =
            """"
            module App exposing (..)


            result n =
                let
                    isEven x =
                        if Pine_kernel.equal [ x, 0 ] then
                            True
                        else
                            isOdd (Pine_kernel.int_add [ x, -1 ])

                    isOdd x =
                        if Pine_kernel.equal [ x, 0 ] then
                            False
                        else
                            isEven (Pine_kernel.int_add [ x, -1 ])
                in
                isEven n
            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            """"
            App.result n =
                App.result__lifted__isEven_1
                    n


            App.result__lifted__isEven_1 x =
                if
                    Pine_kernel.equal
                        [ x, 0 ]
                then
                    Basics.True

                else
                    App.result__lifted__isOdd_2
                        (Pine_kernel.int_add
                            [ x, -1 ]
                        )


            App.result__lifted__isOdd_2 x =
                if
                    Pine_kernel.equal
                        [ x, 0 ]
                then
                    Basics.False

                else
                    App.result__lifted__isEven_1
                        (Pine_kernel.int_add
                            [ x, -1 ]
                        )
            """");
    }

    [Fact]
    public void Let_block_mutually_recursive_functions_reversed()
    {
        // Same as above, but isOdd is declared before isEven.
        var elmModuleText =
            """"
            module App exposing (..)


            result n =
                let
                    isOdd x =
                        if Pine_kernel.equal [ x, 0 ] then
                            False
                        else
                            isEven (Pine_kernel.int_add [ x, -1 ])

                    isEven x =
                        if Pine_kernel.equal [ x, 0 ] then
                            True
                        else
                            isOdd (Pine_kernel.int_add [ x, -1 ])
                in
                isEven n
            """";

        var rendered =
            InlineAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            """"
            App.result n =
                App.result__lifted__isEven_2
                    n


            App.result__lifted__isEven_2 x =
                if
                    Pine_kernel.equal
                        [ x, 0 ]
                then
                    Basics.True

                else
                    App.result__lifted__isOdd_1
                        (Pine_kernel.int_add
                            [ x, -1 ]
                        )


            App.result__lifted__isOdd_1 x =
                if
                    Pine_kernel.equal
                        [ x, 0 ]
                then
                    Basics.False

                else
                    App.result__lifted__isEven_2
                        (Pine_kernel.int_add
                            [ x, -1 ]
                        )
            """");
    }

    // ──────────────────────────────────────────────────────────
    // Let blocks with the full optimization pipeline
    // ──────────────────────────────────────────────────────────

    [Fact]
    public void Full_pipeline_let_block_natural_order()
    {
        // Tests the full optimization pipeline (specialization + inlining + lowering)
        // with let-block declarations in natural dependency order.
        var elmModuleText =
            """"
            module App exposing (..)


            result : Int
            result =
                let
                    a : Int
                    a = 1

                    b : Int
                    b = a
                in
                b
            """";

        var rendered =
            OptimizeAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            """"
            App.result : Int
            App.result =
                let
                    a : Int
                    a =
                        1

                    b : Int
                    b =
                        a
                in
                b
            """");
    }

    [Fact]
    public void Full_pipeline_let_block_reverse_order()
    {
        // Tests the full optimization pipeline (specialization + inlining + lowering)
        // with let-block declarations in reverse dependency order (forward references).
        var elmModuleText =
            """"
            module App exposing (..)


            result : Int
            result =
                let
                    b : Int
                    b = a

                    a : Int
                    a = 1
                in
                b
            """";

        var rendered =
            OptimizeAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            """"
            App.result : Int
            App.result =
                let
                    b : Int
                    b =
                        a

                    a : Int
                    a =
                        1
                in
                b
            """");
    }

    [Fact]
    public void Full_pipeline_let_block_mutually_recursive()
    {
        // Tests the full optimization pipeline with mutually recursive let functions.
        var elmModuleText =
            """"
            module App exposing (..)


            result : Int -> Bool
            result n =
                let
                    isEven : Int -> Bool
                    isEven x =
                        if Pine_kernel.equal [ x, 0 ] then
                            True
                        else
                            isOdd (Pine_kernel.int_add [ x, -1 ])

                    isOdd : Int -> Bool
                    isOdd x =
                        if Pine_kernel.equal [ x, 0 ] then
                            False
                        else
                            isEven (Pine_kernel.int_add [ x, -1 ])
                in
                isEven n
            """";

        var rendered =
            OptimizeAndRenderSingleModule(
                elmModuleText,
                ["App"],
                Inlining.Config.OnlyFunctions);

        rendered.Trim().Should().Be(
            """"
            App.result : Int -> Bool
            App.result n =
                App.result__lifted__isEven_1
                    n


            App.result__lifted__isEven_1 x =
                if
                    Pine_kernel.equal
                        [ x, 0 ]
                then
                    Basics.True

                else
                    App.result__lifted__isOdd_2
                        (Pine_kernel.int_add
                            [ x, -1 ]
                        )


            App.result__lifted__isOdd_2 x =
                if
                    Pine_kernel.equal
                        [ x, 0 ]
                then
                    Basics.False

                else
                    App.result__lifted__isEven_1
                        (Pine_kernel.int_add
                            [ x, -1 ]
                        )
            """");
    }

    // ──────────────────────────────────────────────────────────
    // Equivalence: same semantics regardless of declaration order
    // ──────────────────────────────────────────────────────────

    [Fact]
    public void Let_block_order_independence_value_chain()
    {
        // Verify that the optimization pipeline produces correct output
        // for both natural and reverse declaration order.
        var naturalOrderModule =
            """"
            module App exposing (..)


            result x =
                let
                    a = x

                    b =
                        Pine_kernel.int_multiply [ a, 2 ]

                    c =
                        Pine_kernel.int_add [ b, 1 ]
                in
                c
            """";

        var reverseOrderModule =
            """"
            module App exposing (..)


            result x =
                let
                    c =
                        Pine_kernel.int_add [ b, 1 ]

                    b =
                        Pine_kernel.int_multiply [ a, 2 ]

                    a = x
                in
                c
            """";

        var naturalRendered =
            InlineAndRenderSingleModule(
                naturalOrderModule,
                ["App"],
                Inlining.Config.OnlyFunctions);

        var reverseRendered =
            InlineAndRenderSingleModule(
                reverseOrderModule,
                ["App"],
                Inlining.Config.OnlyFunctions);

        naturalRendered.Trim().Should().Be(
            """"
            App.result x =
                let
                    a =
                        x

                    b =
                        Pine_kernel.int_multiply
                            [ a, 2 ]

                    c =
                        Pine_kernel.int_add
                            [ b, 1 ]
                in
                c
            """");

        reverseRendered.Trim().Should().Be(
            """"
            App.result x =
                let
                    c =
                        Pine_kernel.int_add
                            [ b, 1 ]

                    b =
                        Pine_kernel.int_multiply
                            [ a, 2 ]

                    a =
                        x
                in
                c
            """");
    }

    [Fact]
    public void Let_block_order_independence_function_calls()
    {
        // Verify that the optimization pipeline produces correct output
        // for let-block functions regardless of declaration order.
        var naturalOrderModule =
            """"
            module App exposing (..)


            result x =
                let
                    inc y =
                        Pine_kernel.int_add [ y, 1 ]

                    dbl y =
                        Pine_kernel.int_multiply [ y, 2 ]

                    transform z =
                        inc (dbl z)
                in
                transform x
            """";

        var reverseOrderModule =
            """"
            module App exposing (..)


            result x =
                let
                    transform z =
                        inc (dbl z)

                    dbl y =
                        Pine_kernel.int_multiply [ y, 2 ]

                    inc y =
                        Pine_kernel.int_add [ y, 1 ]
                in
                transform x
            """";

        var naturalRendered =
            InlineAndRenderSingleModule(
                naturalOrderModule,
                ["App"],
                Inlining.Config.OnlyFunctions);

        var reverseRendered =
            InlineAndRenderSingleModule(
                reverseOrderModule,
                ["App"],
                Inlining.Config.OnlyFunctions);

        naturalRendered.Trim().Should().Be(
            """"
            App.result x =
                App.result__lifted__transform_3
                    x


            App.result__lifted__dbl_2 y =
                Pine_kernel.int_multiply
                    [ y, 2 ]


            App.result__lifted__inc_1 y =
                Pine_kernel.int_add
                    [ y, 1 ]


            App.result__lifted__transform_3 z =
                App.result__lifted__inc_1
                    (App.result__lifted__dbl_2
                        z
                    )
            """");

        reverseRendered.Trim().Should().Be(
            """"
            App.result x =
                App.result__lifted__transform_1
                    x


            App.result__lifted__dbl_2 y =
                Pine_kernel.int_multiply
                    [ y, 2 ]


            App.result__lifted__inc_3 y =
                Pine_kernel.int_add
                    [ y, 1 ]


            App.result__lifted__transform_1 z =
                App.result__lifted__inc_3
                    (App.result__lifted__dbl_2
                        z
                    )
            """");
    }

    [Fact]
    public void Let_block_order_independence_mutually_recursive()
    {
        // Verify that mutually recursive let-functions produce correct output
        // regardless of which function is declared first.
        var evenFirstModule =
            """"
            module App exposing (..)


            result n =
                let
                    isEven x =
                        if Pine_kernel.equal [ x, 0 ] then
                            True
                        else
                            isOdd (Pine_kernel.int_add [ x, -1 ])

                    isOdd x =
                        if Pine_kernel.equal [ x, 0 ] then
                            False
                        else
                            isEven (Pine_kernel.int_add [ x, -1 ])
                in
                isEven n
            """";

        var oddFirstModule =
            """"
            module App exposing (..)


            result n =
                let
                    isOdd x =
                        if Pine_kernel.equal [ x, 0 ] then
                            False
                        else
                            isEven (Pine_kernel.int_add [ x, -1 ])

                    isEven x =
                        if Pine_kernel.equal [ x, 0 ] then
                            True
                        else
                            isOdd (Pine_kernel.int_add [ x, -1 ])
                in
                isEven n
            """";

        var evenFirstRendered =
            InlineAndRenderSingleModule(
                evenFirstModule,
                ["App"],
                Inlining.Config.OnlyFunctions);

        var oddFirstRendered =
            InlineAndRenderSingleModule(
                oddFirstModule,
                ["App"],
                Inlining.Config.OnlyFunctions);

        evenFirstRendered.Trim().Should().Be(
            """"
            App.result n =
                App.result__lifted__isEven_1
                    n


            App.result__lifted__isEven_1 x =
                if
                    Pine_kernel.equal
                        [ x, 0 ]
                then
                    Basics.True

                else
                    App.result__lifted__isOdd_2
                        (Pine_kernel.int_add
                            [ x, -1 ]
                        )


            App.result__lifted__isOdd_2 x =
                if
                    Pine_kernel.equal
                        [ x, 0 ]
                then
                    Basics.False

                else
                    App.result__lifted__isEven_1
                        (Pine_kernel.int_add
                            [ x, -1 ]
                        )
            """");

        oddFirstRendered.Trim().Should().Be(
            """"
            App.result n =
                App.result__lifted__isEven_2
                    n


            App.result__lifted__isEven_2 x =
                if
                    Pine_kernel.equal
                        [ x, 0 ]
                then
                    Basics.True

                else
                    App.result__lifted__isOdd_1
                        (Pine_kernel.int_add
                            [ x, -1 ]
                        )


            App.result__lifted__isOdd_1 x =
                if
                    Pine_kernel.equal
                        [ x, 0 ]
                then
                    Basics.False

                else
                    App.result__lifted__isEven_2
                        (Pine_kernel.int_add
                            [ x, -1 ]
                        )
            """");
    }
}
