using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class StaticProgramIdivTests
{
    [Fact]
    public void Parse_static_idiv()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            idiv : Int -> Int -> Int
            idiv dividend divisor =
                if Pine_kernel.equal [ divisor, 0 ] then
                    0

                else
                    let
                        ( dividendNegative, absDividend ) =
                            if Pine_kernel.int_is_sorted_asc [ 0, dividend ] then
                                ( False
                                , dividend
                                )

                            else
                                ( True
                                , Pine_kernel.int_mul [ dividend, -1 ]
                                )

                        ( divisorNegative, absDivisor ) =
                            if Pine_kernel.int_is_sorted_asc [ 0, divisor ] then
                                ( False
                                , divisor
                                )

                            else
                                ( True
                                , Pine_kernel.int_mul [ divisor, -1 ]
                                )

                        absQuotient : Int
                        absQuotient =
                            idivHelper absDividend absDivisor 0
                    in
                    if Pine_kernel.equal [ dividendNegative, divisorNegative ] then
                        absQuotient

                    else
                        Pine_kernel.int_mul [ absQuotient, -1 ]


            idivHelper : Int -> Int -> Int -> Int
            idivHelper dividend divisor quotient =
                let
                    scaledDivisor =
                        Pine_kernel.int_mul [ divisor, 17 ]
                in
                if Pine_kernel.int_is_sorted_asc [ scaledDivisor, dividend ] then
                    let
                        scaledQuotient =
                            idivHelper
                                dividend
                                scaledDivisor
                                0

                        scaledQuotientSum =
                            Pine_kernel.int_mul [ scaledQuotient, 17 ]

                        remainder =
                            Pine_kernel.int_add
                                [ dividend
                                , Pine_kernel.int_mul [ scaledQuotient, scaledDivisor, -1 ]
                                ]

                        remainderQuotient =
                            idivHelper remainder divisor 0
                    in
                    Pine_kernel.int_add [ scaledQuotientSum, remainderQuotient ]

                else if Pine_kernel.int_is_sorted_asc [ divisor, dividend ] then
                    idivHelper
                        (Pine_kernel.int_add
                            [ dividend
                            , Pine_kernel.int_mul [ divisor, -1 ]
                            ]
                        )
                        divisor
                        (Pine_kernel.int_add [ quotient, 1 ])

                else
                    quotient

            """";

        var parseCache = new PineVMParseCache();

        var (_, staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                includeDeclaration:
                declName =>
                {
                    return declName.FullName == "Test.idiv";
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.idiv param_1 param_2 =
                if
                    Pine_kernel.equal
                        [ param_2
                        , 0
                        ]
                then
                    0

                else if
                    Pine_kernel.equal
                        [ if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_1
                                ]
                          then
                            False

                          else
                            True
                        , if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_2
                                ]
                          then
                            False

                          else
                            True
                        ]
                then
                    Test.idivHelper
                        if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_1
                                ]
                        then
                            param_1

                        else
                            Pine_kernel.int_mul
                                [ param_1
                                , -1
                                ]
                        if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_2
                                ]
                        then
                            param_2

                        else
                            Pine_kernel.int_mul
                                [ param_2
                                , -1
                                ]
                        0

                else
                    Pine_kernel.int_mul
                        [ Test.idivHelper
                            if
                                Pine_kernel.int_is_sorted_asc
                                    [ 0
                                    , param_1
                                    ]
                            then
                                param_1

                            else
                                Pine_kernel.int_mul
                                    [ param_1
                                    , -1
                                    ]
                            if
                                Pine_kernel.int_is_sorted_asc
                                    [ 0
                                    , param_2
                                    ]
                            then
                                param_2

                            else
                                Pine_kernel.int_mul
                                    [ param_2
                                    , -1
                                    ]
                            0
                        , -1
                        ]


            Test.idivHelper param_1 param_2 param_3 =
                if
                    Pine_kernel.int_is_sorted_asc
                        [ Pine_kernel.int_mul
                            [ param_2
                            , 17
                            ]
                        , param_1
                        ]
                then
                    Pine_kernel.int_add
                        [ Pine_kernel.int_mul
                            [ Test.idivHelper
                                param_1
                                (Pine_kernel.int_mul
                                    [ param_2
                                    , 17
                                    ]
                                )
                                0
                            , 17
                            ]
                        , Test.idivHelper
                            (Pine_kernel.int_add
                                [ param_1
                                , Pine_kernel.int_mul
                                    [ -17
                                    , Test.idivHelper
                                        param_1
                                        (Pine_kernel.int_mul
                                            [ param_2
                                            , 17
                                            ]
                                        )
                                        0
                                    , param_2
                                    ]
                                ]
                            )
                            param_2
                            0
                        ]

                else if
                    Pine_kernel.int_is_sorted_asc
                        [ param_2
                        , param_1
                        ]
                then
                    Test.idivHelper
                        (Pine_kernel.int_add
                            [ param_1
                            , Pine_kernel.int_mul
                                [ param_2
                                , -1
                                ]
                            ]
                        )
                        param_2
                        (Pine_kernel.int_add
                            [ param_3
                            , 1
                            ]
                        )

                else
                    param_3
            """".Trim());

    }
}
