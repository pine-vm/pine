using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class CodeAnalysisTests
{
    [Fact]
    public void Parse_Fibonacci()
    {
        var elmModuleText =
            """"
            module Test exposing (..)

            fibonacci : Int -> Int
            fibonacci n =
                if Pine_kernel.int_is_sorted_asc [ n, 2 ] then
                    n

                else
                    Pine_kernel.int_add
                        [ fibonacci (Pine_kernel.int_add [ n, -2 ])
                        , fibonacci (Pine_kernel.int_add [ n, -1 ])
                        ]

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                includeDeclaration:
                declName =>
                {
                    return declName.Namespaces.SequenceEqual(["Test"]);
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.fibonacci param_1 =
                if
                    Pine_kernel.int_is_sorted_asc
                        [ param_1
                        , 2
                        ]
                then
                    param_1

                else
                    Pine_kernel.int_add
                        [ Test.fibonacci
                            (Pine_kernel.int_add
                                [ param_1
                                , -2
                                ]
                            )
                        , Test.fibonacci
                            (Pine_kernel.int_add
                                [ param_1
                                , -1
                                ]
                            )
                        ]
            """"
            .Trim());
    }

    [Fact]
    public void Parse_Factorial()
    {
        var elmModuleText =
            """"
            module Test exposing (..)

            factorial : Int -> Int
            factorial n =
                if Pine_kernel.int_is_sorted_asc [ n, 1 ] then
                    1

                else
                    Pine_kernel.int_mul
                        [ factorial (Pine_kernel.int_add [ n, -1 ])
                        , n
                        ]

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                includeDeclaration:
                declName =>
                {
                    return declName.Namespaces.SequenceEqual(["Test"]);
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.factorial param_1 =
                if
                    Pine_kernel.int_is_sorted_asc
                        [ param_1
                        , 1
                        ]
                then
                    1

                else
                    Pine_kernel.int_mul
                        [ Test.factorial
                            (Pine_kernel.int_add
                                [ param_1
                                , -1
                                ]
                            )
                        , param_1
                        ]
            """".Trim());
    }

    [Fact]
    public void Parse_Test_dictToShuffledList()
    {
        /*
         * Use a form that us unlikely to occur also in the standard libraries,
         * to avoid code analysis picking up the name of the same function in the standard library.
         * */

        var elmModuleText =
            """"
            module Test exposing (..)

            type Dict k v
                = RBEmpty_elm_builtin
                | RBNode_elm_builtin Int k v (Dict k v) (Dict k v)


            dictToShuffledList : Dict k v -> List ( k, v )
            dictToShuffledList dict =
                case dict of
                    RBEmpty_elm_builtin ->
                        []

                    RBNode_elm_builtin _ key value left right ->
                        Pine_kernel.concat [ dictToShuffledList left, dictToShuffledList right, [ ( key, value ) ] ]
            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                includeDeclaration:
                declName =>
                {
                    return declName.Namespaces.SequenceEqual(["Test"]);
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.dictToShuffledList param_1_1 =
                if
                    Pine_kernel.equal
                        [ []
                        , Dict.empty
                        ]
                then
                    []

                else
                    Pine_kernel.concat
                        [ Test.dictToShuffledList
                            param_1_1[3][1]
                        , Test.dictToShuffledList
                            param_1_1[4][1]
                        , [ [ param_1_1[1]
                            , param_1_1[2]
                            ]
                          ]
                        ]
            """"
            .Trim());
    }

    [Fact]
    public void Parse_Test_convert0OrMore_base3()
    {
        /*
         * Use a form that us unlikely to occur also in the standard libraries,
         * to avoid code analysis picking up the name of the same function in the standard library.
         * */

        var elmModuleText =
            """"
            module Test exposing (..)

            
            convert0OrMore_base3 : Int -> Int -> Int -> ( Int, Int )
            convert0OrMore_base3 soFar offset srcBytes =
                let
                    nextChar =
                        Pine_kernel.take [ 4, Pine_kernel.skip [ offset, srcBytes ] ]
                in
                if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                    -- We ran out of characters, return what we have so far
                    ( soFar, offset )

                else
                    case nextChar of
                        '0' ->
                            convert0OrMore_base3 (soFar * 3) (offset + 4) srcBytes

                        '1' ->
                            convert0OrMore_base3 (soFar * 3 + 1) (offset + 4) srcBytes

                        '2' ->
                            convert0OrMore_base3 (soFar * 3 + 2) (offset + 4) srcBytes

                        _ ->
                            ( 0, -1 )
            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                includeDeclaration:
                declName =>
                {
                    return declName.Namespaces.SequenceEqual(["Test"]);
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.convert0OrMore_base3 param_1 param_2 param_3 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.length
                            (Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_2
                                    , param_3
                                    ]
                                ]
                            )
                        , 0
                        ]
                then
                    [ param_1
                    , param_2
                    ]

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_3
                                ]
                            ]
                        , '0'
                        ]
                then
                    Test.convert0OrMore_base3
                        (Pine_kernel.int_mul
                            [ param_1
                            , 3
                            ]
                        )
                        (Pine_kernel.int_add
                            [ param_2
                            , 4
                            ]
                        )
                        param_3

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_3
                                ]
                            ]
                        , '1'
                        ]
                then
                    Test.convert0OrMore_base3
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1
                                , 3
                                ]
                            , 1
                            ]
                        )
                        (Pine_kernel.int_add
                            [ param_2
                            , 4
                            ]
                        )
                        param_3

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_3
                                ]
                            ]
                        , '2'
                        ]
                then
                    Test.convert0OrMore_base3
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1
                                , 3
                                ]
                            , 2
                            ]
                        )
                        (Pine_kernel.int_add
                            [ param_2
                            , 4
                            ]
                        )
                        param_3

                else
                    [ 0, -1 ]
            """"
            .Trim());
    }

    [Fact]
    public void Parse_Basics_compare()
    {
        var compiledEnv =
            BundledElmEnvironments.BundledElmCompilerCompiledEnvValue()
            ??
            throw new System.Exception("Failed to load Elm compiler from bundle.");

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var (staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
                parsedEnv,
                includeDeclaration:
                declName =>
                {
                    return declName.FullName == "Basics.compare";
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        wholeProgramText.Trim().Should().Be(
            """"
            Basics.compare param_1_0 param_1_1 =
                if
                    Pine_kernel.equal
                        [ param_1_0
                        , param_1_1
                        ]
                then
                    EQ

                else if
                    if
                        Pine_kernel.equal
                            [ String
                            , param_1_1[0]
                            ]
                    then
                        Pine_kernel.equal
                            [ String
                            , param_1_0[0]
                            ]

                    else
                        False
                then
                    Basics.compareStrings
                        0
                        param_1_0[1][0]
                        param_1_1[1][0]

                else if
                    if
                        Pine_kernel.equal
                            [ Elm_Float
                            , param_1_1[0]
                            ]
                    then
                        Pine_kernel.equal
                            [ Elm_Float
                            , param_1_0[0]
                            ]

                    else
                        False
                then
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.int_mul
                                [ param_1_0[1][0]
                                , param_1_1[1][1]
                                ]
                            , Pine_kernel.int_mul
                                [ param_1_1[1][0]
                                , param_1_0[1][1]
                                ]
                            ]
                    then
                        EQ

                    else if
                        Pine_kernel.int_is_sorted_asc
                            [ Pine_kernel.int_mul
                                [ param_1_0[1][0]
                                , param_1_1[1][1]
                                ]
                            , Pine_kernel.int_mul
                                [ param_1_1[1][0]
                                , param_1_0[1][1]
                                ]
                            ]
                    then
                        LT

                    else
                        GT

                else if
                    Pine_kernel.equal
                        [ Elm_Float
                        , param_1_0[0]
                        ]
                then
                    if
                        Pine_kernel.equal
                            [ param_1_0[1][0]
                            , Pine_kernel.int_mul
                                [ param_1_0[1][1]
                                , param_1_1
                                ]
                            ]
                    then
                        EQ

                    else if
                        Pine_kernel.int_is_sorted_asc
                            [ param_1_0[1][0]
                            , Pine_kernel.int_mul
                                [ param_1_0[1][1]
                                , param_1_1
                                ]
                            ]
                    then
                        LT

                    else
                        GT

                else if
                    Pine_kernel.equal
                        [ Elm_Float
                        , param_1_1[0]
                        ]
                then
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , param_1_1[1][1]
                                ]
                            , param_1_1[1][0]
                            ]
                    then
                        EQ

                    else if
                        Pine_kernel.int_is_sorted_asc
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , param_1_1[1][1]
                                ]
                            , param_1_1[1][0]
                            ]
                    then
                        LT

                    else
                        GT

                else if
                    zzz_anon_c78b4c00_dda26649
                        param_1_0
                then
                    Basics.compareList
                        param_1_0
                        param_1_1

                else if
                    Pine_kernel.int_is_sorted_asc
                        [ param_1_0
                        , param_1_1
                        ]
                then
                    LT

                else
                    GT


            Basics.compareList param_1_0 param_1_1 =
                if
                    Pine_kernel.equal
                        [ param_1_0
                        , []
                        ]
                then
                    if
                        Pine_kernel.equal
                            [ param_1_1
                            , []
                            ]
                    then
                        EQ

                    else
                        LT

                else if
                    Pine_kernel.negate
                        (Pine_kernel.equal
                            [ Pine_kernel.length
                                param_1_0
                            , 0
                            ]
                        )
                then
                    if
                        Pine_kernel.equal
                            [ param_1_1
                            , []
                            ]
                    then
                        GT

                    else if
                        Pine_kernel.negate
                            (Pine_kernel.equal
                                [ Pine_kernel.length
                                    param_1_1
                                , 0
                                ]
                            )
                    then
                        if
                            Pine_kernel.equal
                                [ Basics.compare
                                    param_1_0[0]
                                    param_1_1[0]
                                , EQ
                                ]
                        then
                            Basics.compareList
                                (Pine_kernel.skip
                                    [ 1
                                    , param_1_0
                                    ]
                                )
                                (Pine_kernel.skip
                                    [ 1
                                    , param_1_1
                                    ]
                                )

                        else
                            Basics.compare
                                param_1_0[0]
                                param_1_1[0]

                    else
                        <always_crash>

                else
                    <always_crash>


            Basics.compareStrings param_1_0 param_1_1 param_1_2 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.length
                            (Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_1_0
                                    , param_1_1
                                    ]
                                ]
                            )
                        , 0
                        ]
                then
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.length
                                (Pine_kernel.take
                                    [ 4
                                    , Pine_kernel.skip
                                        [ param_1_0
                                        , param_1_2
                                        ]
                                    ]
                                )
                            , 0
                            ]
                    then
                        EQ

                    else
                        LT

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.length
                            (Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_1_0
                                    , param_1_2
                                    ]
                                ]
                            )
                        , 0
                        ]
                then
                    GT

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_0
                                , param_1_1
                                ]
                            ]
                        , Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_0
                                , param_1_2
                                ]
                            ]
                        ]
                then
                    Basics.compareStrings
                        (Pine_kernel.int_add
                            [ param_1_0
                            , 4
                            ]
                        )
                        param_1_1
                        param_1_2

                else if
                    Pine_kernel.int_is_sorted_asc
                        [ Pine_kernel.concat
                            [ 0
                            , Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_1_0
                                    , param_1_1
                                    ]
                                ]
                            ]
                        , Pine_kernel.concat
                            [ 0
                            , Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_1_0
                                    , param_1_2
                                    ]
                                ]
                            ]
                        ]
                then
                    LT

                else
                    GT


            zzz_anon_c78b4c00_dda26649 param_1_0 =
                Pine_kernel.equal
                    [ Pine_kernel.take
                        [ 0
                        , param_1_0
                        ]
                    , []
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Parse_Test_idiv()
    {
        /*
         * Using a modified form of idiv to avoid matching compiled value from Elm core library.
         * */

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

        var (parsedEnv, staticProgram, functionMetadata) =
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
            """"
            .Trim());
    }

    [Fact]
    public void Parse_Dict_insert()
    {
        var compiledEnv =
            BundledElmEnvironments.BundledElmCompilerCompiledEnvValue()
            ??
            throw new System.Exception("Failed to load Elm compiler from bundle.");

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var (staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
                parsedEnv,
                includeDeclaration:
                declName =>
                {
                    return declName.FullName == "Dict.insert";
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        wholeProgramText.Trim().Should().Be(
            """"
            Basics.compare param_1_0 param_1_1 =
                if
                    Pine_kernel.equal
                        [ param_1_0
                        , param_1_1
                        ]
                then
                    EQ

                else if
                    if
                        Pine_kernel.equal
                            [ String
                            , param_1_1[0]
                            ]
                    then
                        Pine_kernel.equal
                            [ String
                            , param_1_0[0]
                            ]

                    else
                        False
                then
                    Basics.compareStrings
                        0
                        param_1_0[1][0]
                        param_1_1[1][0]

                else if
                    if
                        Pine_kernel.equal
                            [ Elm_Float
                            , param_1_1[0]
                            ]
                    then
                        Pine_kernel.equal
                            [ Elm_Float
                            , param_1_0[0]
                            ]

                    else
                        False
                then
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.int_mul
                                [ param_1_0[1][0]
                                , param_1_1[1][1]
                                ]
                            , Pine_kernel.int_mul
                                [ param_1_1[1][0]
                                , param_1_0[1][1]
                                ]
                            ]
                    then
                        EQ

                    else if
                        Pine_kernel.int_is_sorted_asc
                            [ Pine_kernel.int_mul
                                [ param_1_0[1][0]
                                , param_1_1[1][1]
                                ]
                            , Pine_kernel.int_mul
                                [ param_1_1[1][0]
                                , param_1_0[1][1]
                                ]
                            ]
                    then
                        LT

                    else
                        GT

                else if
                    Pine_kernel.equal
                        [ Elm_Float
                        , param_1_0[0]
                        ]
                then
                    if
                        Pine_kernel.equal
                            [ param_1_0[1][0]
                            , Pine_kernel.int_mul
                                [ param_1_0[1][1]
                                , param_1_1
                                ]
                            ]
                    then
                        EQ

                    else if
                        Pine_kernel.int_is_sorted_asc
                            [ param_1_0[1][0]
                            , Pine_kernel.int_mul
                                [ param_1_0[1][1]
                                , param_1_1
                                ]
                            ]
                    then
                        LT

                    else
                        GT

                else if
                    Pine_kernel.equal
                        [ Elm_Float
                        , param_1_1[0]
                        ]
                then
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , param_1_1[1][1]
                                ]
                            , param_1_1[1][0]
                            ]
                    then
                        EQ

                    else if
                        Pine_kernel.int_is_sorted_asc
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , param_1_1[1][1]
                                ]
                            , param_1_1[1][0]
                            ]
                    then
                        LT

                    else
                        GT

                else if
                    zzz_anon_c78b4c00_dda26649
                        param_1_0
                then
                    Basics.compareList
                        param_1_0
                        param_1_1

                else if
                    Pine_kernel.int_is_sorted_asc
                        [ param_1_0
                        , param_1_1
                        ]
                then
                    LT

                else
                    GT


            Basics.compareList param_1_0 param_1_1 =
                if
                    Pine_kernel.equal
                        [ param_1_0
                        , []
                        ]
                then
                    if
                        Pine_kernel.equal
                            [ param_1_1
                            , []
                            ]
                    then
                        EQ

                    else
                        LT

                else if
                    Pine_kernel.negate
                        (Pine_kernel.equal
                            [ Pine_kernel.length
                                param_1_0
                            , 0
                            ]
                        )
                then
                    if
                        Pine_kernel.equal
                            [ param_1_1
                            , []
                            ]
                    then
                        GT

                    else if
                        Pine_kernel.negate
                            (Pine_kernel.equal
                                [ Pine_kernel.length
                                    param_1_1
                                , 0
                                ]
                            )
                    then
                        if
                            Pine_kernel.equal
                                [ Basics.compare
                                    param_1_0[0]
                                    param_1_1[0]
                                , EQ
                                ]
                        then
                            Basics.compareList
                                (Pine_kernel.skip
                                    [ 1
                                    , param_1_0
                                    ]
                                )
                                (Pine_kernel.skip
                                    [ 1
                                    , param_1_1
                                    ]
                                )

                        else
                            Basics.compare
                                param_1_0[0]
                                param_1_1[0]

                    else
                        <always_crash>

                else
                    <always_crash>


            Basics.compareStrings param_1_0 param_1_1 param_1_2 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.length
                            (Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_1_0
                                    , param_1_1
                                    ]
                                ]
                            )
                        , 0
                        ]
                then
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.length
                                (Pine_kernel.take
                                    [ 4
                                    , Pine_kernel.skip
                                        [ param_1_0
                                        , param_1_2
                                        ]
                                    ]
                                )
                            , 0
                            ]
                    then
                        EQ

                    else
                        LT

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.length
                            (Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_1_0
                                    , param_1_2
                                    ]
                                ]
                            )
                        , 0
                        ]
                then
                    GT

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_0
                                , param_1_1
                                ]
                            ]
                        , Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_0
                                , param_1_2
                                ]
                            ]
                        ]
                then
                    Basics.compareStrings
                        (Pine_kernel.int_add
                            [ param_1_0
                            , 4
                            ]
                        )
                        param_1_1
                        param_1_2

                else if
                    Pine_kernel.int_is_sorted_asc
                        [ Pine_kernel.concat
                            [ 0
                            , Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_1_0
                                    , param_1_1
                                    ]
                                ]
                            ]
                        , Pine_kernel.concat
                            [ 0
                            , Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_1_0
                                    , param_1_2
                                    ]
                                ]
                            ]
                        ]
                then
                    LT

                else
                    GT


            Dict.insert param_1_0 param_1_1 param_1_2 =
                if
                    if
                        Pine_kernel.equal
                            [ Red
                            , Pine_kernel.head
                                (Pine_kernel.head
                                    (Pine_kernel.head
                                        (Pine_kernel.skip
                                            [ 1
                                            , zzz_anon_ea679199_24f2a3d1
                                                param_1_0
                                                param_1_1
                                                param_1_2
                                            ]
                                        )
                                    )
                                )
                            ]
                    then
                        Pine_kernel.equal
                            [ RBNode_elm_builtin
                            , Pine_kernel.head
                                (zzz_anon_ea679199_24f2a3d1
                                    param_1_0
                                    param_1_1
                                    param_1_2)
                            ]

                    else
                        False
                then
                    [ RBNode_elm_builtin
                    , [ Black
                      , Pine_kernel.head
                          (Pine_kernel.skip
                              [ 1
                              , Pine_kernel.head
                                  (Pine_kernel.skip
                                      [ 1
                                      , zzz_anon_ea679199_24f2a3d1
                                          param_1_0
                                          param_1_1
                                          param_1_2
                                      ]
                                  )
                              ]
                          )
                      , Pine_kernel.head
                          (Pine_kernel.skip
                              [ 2
                              , Pine_kernel.head
                                  (Pine_kernel.skip
                                      [ 1
                                      , zzz_anon_ea679199_24f2a3d1
                                          param_1_0
                                          param_1_1
                                          param_1_2
                                      ]
                                  )
                              ]
                          )
                      , Pine_kernel.head
                          (Pine_kernel.skip
                              [ 3
                              , Pine_kernel.head
                                  (Pine_kernel.skip
                                      [ 1
                                      , zzz_anon_ea679199_24f2a3d1
                                          param_1_0
                                          param_1_1
                                          param_1_2
                                      ]
                                  )
                              ]
                          )
                      , Pine_kernel.head
                          (Pine_kernel.skip
                              [ 4
                              , Pine_kernel.head
                                  (Pine_kernel.skip
                                      [ 1
                                      , zzz_anon_ea679199_24f2a3d1
                                          param_1_0
                                          param_1_1
                                          param_1_2
                                      ]
                                  )
                              ]
                          )
                      ]
                    ]

                else
                    zzz_anon_ea679199_24f2a3d1
                        param_1_0
                        param_1_1
                        param_1_2


            zzz_anon_c78b4c00_dda26649 param_1_0 =
                Pine_kernel.equal
                    [ Pine_kernel.take
                        [ 0
                        , param_1_0
                        ]
                    , []
                    ]


            zzz_anon_e6d15ff4_dda26649 param_1_0 param_1_1 param_1_2 param_1_3 param_1_4 =
                if
                    if
                        Pine_kernel.equal
                            [ Red
                            , param_1_4[1][0][0]
                            ]
                    then
                        Pine_kernel.equal
                            [ RBNode_elm_builtin
                            , param_1_4[0]
                            ]

                    else
                        False
                then
                    if
                        if
                            Pine_kernel.equal
                                [ Red
                                , param_1_3[1][0][0]
                                ]
                        then
                            Pine_kernel.equal
                                [ RBNode_elm_builtin
                                , param_1_3[0]
                                ]

                        else
                            False
                    then
                        [ RBNode_elm_builtin
                        , [ Red
                          , param_1_1
                          , param_1_2
                          , [ RBNode_elm_builtin
                            , [ Black
                              , param_1_3[1][1]
                              , param_1_3[1][2]
                              , param_1_3[1][3]
                              , param_1_3[1][4]
                              ]
                            ]
                          , [ RBNode_elm_builtin
                            , [ Black
                              , param_1_4[1][1]
                              , param_1_4[1][2]
                              , param_1_4[1][3]
                              , param_1_4[1][4]
                              ]
                            ]
                          ]
                        ]

                    else
                        [ RBNode_elm_builtin
                        , [ param_1_0
                          , param_1_4[1][1]
                          , param_1_4[1][2]
                          , [ RBNode_elm_builtin
                            , [ Red
                              , param_1_1
                              , param_1_2
                              , param_1_3
                              , param_1_4[1][3]
                              ]
                            ]
                          , param_1_4[1][4]
                          ]
                        ]

                else if
                    if
                        Pine_kernel.equal
                            [ Red
                            , param_1_3[1][3][1][0][0]
                            ]
                    then
                        if
                            Pine_kernel.equal
                                [ RBNode_elm_builtin
                                , param_1_3[1][3][0]
                                ]
                        then
                            if
                                Pine_kernel.equal
                                    [ Red
                                    , param_1_3[1][0][0]
                                    ]
                            then
                                Pine_kernel.equal
                                    [ RBNode_elm_builtin
                                    , param_1_3[0]
                                    ]

                            else
                                False

                        else
                            False

                    else
                        False
                then
                    [ RBNode_elm_builtin
                    , [ Red
                      , param_1_3[1][1]
                      , param_1_3[1][2]
                      , [ RBNode_elm_builtin
                        , [ Black
                          , param_1_3[1][3][1][1]
                          , param_1_3[1][3][1][2]
                          , param_1_3[1][3][1][3]
                          , param_1_3[1][3][1][4]
                          ]
                        ]
                      , [ RBNode_elm_builtin
                        , [ Black
                          , param_1_1
                          , param_1_2
                          , param_1_3[1][4]
                          , param_1_4
                          ]
                        ]
                      ]
                    ]

                else
                    [ RBNode_elm_builtin
                    , [ param_1_0
                      , param_1_1
                      , param_1_2
                      , param_1_3
                      , param_1_4
                      ]
                    ]


            zzz_anon_ea679199_24f2a3d1 param_1_0 param_1_1 param_1_2 =
                if
                    Pine_kernel.equal
                        [ RBEmpty_elm_builtin
                        , param_1_2[0]
                        ]
                then
                    [ RBNode_elm_builtin
                    , [ Red
                      , param_1_0
                      , param_1_1
                      , Dict.empty
                      , Dict.empty
                      ]
                    ]

                else if
                    Pine_kernel.equal
                        [ RBNode_elm_builtin
                        , param_1_2[0]
                        ]
                then
                    if
                        Pine_kernel.equal
                            [ LT
                            , Pine_kernel.head
                                (Basics.compare
                                    param_1_0
                                    param_1_2[1][1])
                            ]
                    then
                        zzz_anon_e6d15ff4_dda26649
                            param_1_2[1][0]
                            param_1_2[1][1]
                            param_1_2[1][2]
                            (zzz_anon_ea679199_24f2a3d1
                                param_1_0
                                param_1_1
                                param_1_2[1][3])
                            param_1_2[1][4]

                    else if
                        Pine_kernel.equal
                            [ EQ
                            , Pine_kernel.head
                                (Basics.compare
                                    param_1_0
                                    param_1_2[1][1])
                            ]
                    then
                        [ RBNode_elm_builtin
                        , [ param_1_2[1][0]
                          , param_1_2[1][1]
                          , param_1_1
                          , param_1_2[1][3]
                          , param_1_2[1][4]
                          ]
                        ]

                    else if
                        Pine_kernel.equal
                            [ GT
                            , Pine_kernel.head
                                (Basics.compare
                                    param_1_0
                                    param_1_2[1][1])
                            ]
                    then
                        zzz_anon_e6d15ff4_dda26649
                            param_1_2[1][0]
                            param_1_2[1][1]
                            param_1_2[1][2]
                            param_1_2[1][3]
                            (zzz_anon_ea679199_24f2a3d1
                                param_1_0
                                param_1_1
                                param_1_2[1][4])

                    else
                        <always_crash>

                else
                    <always_crash>
            
            """"
            .Trim());
    }

    [Fact]
    public void Parse_Test_hexStringToInt()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type String
                = String Int


            hexStringToInt : String -> Int
            hexStringToInt (String stringBytes) =
                hexStringBytesToInt 0 0 stringBytes


            hexStringBytesToInt : Int -> Int -> Int -> Int
            hexStringBytesToInt offset sum srcBytes =
                let
                    nextChar : Char
                    nextChar =
                        Pine_kernel.take [ 4, Pine_kernel.skip [ offset, srcBytes ] ]
                in
                if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                    sum

                else
                    hexStringBytesToInt
                        (Pine_kernel.int_add [ offset, 4 ])
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul [ 16, sum ]
                            , charToHex nextChar
                            ]
                        )
                        srcBytes


            charToHex : Char -> Int
            charToHex c =
                case c of
                    '0' ->
                        0

                    '1' ->
                        1

                    '2' ->
                        2

                    '3' ->
                        3

                    '4' ->
                        4

                    '5' ->
                        5

                    '6' ->
                        6

                    '7' ->
                        7

                    '8' ->
                        8

                    '9' ->
                        9

                    'a' ->
                        10

                    'b' ->
                        11

                    'c' ->
                        12

                    'd' ->
                        13

                    'e' ->
                        14

                    'f' ->
                        15

                    'A' ->
                        10

                    'B' ->
                        11

                    'C' ->
                        12

                    'D' ->
                        13

                    'E' ->
                        14

                    -- 'F'
                    _ ->
                        15
            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                includeDeclaration:
                declName =>
                {
                    return declName.FullName == "Test.hexStringToInt";
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.hexStringBytesToInt param_1 param_2 param_3 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.length
                            (Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_1
                                    , param_3
                                    ]
                                ]
                            )
                        , 0
                        ]
                then
                    param_2

                else
                    Test.hexStringBytesToInt
                        (Pine_kernel.int_add
                            [ param_1
                            , 4
                            ]
                        )
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ 16
                                , param_2
                                ]
                            , if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , '0'
                                    ]
                              then
                                0

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , '1'
                                    ]
                              then
                                1

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , '2'
                                    ]
                              then
                                2

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , '3'
                                    ]
                              then
                                3

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , '4'
                                    ]
                              then
                                4

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , '5'
                                    ]
                              then
                                5

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , '6'
                                    ]
                              then
                                6

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , '7'
                                    ]
                              then
                                7

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , '8'
                                    ]
                              then
                                8

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , '9'
                                    ]
                              then
                                9

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , 'a'
                                    ]
                              then
                                10

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , 'b'
                                    ]
                              then
                                11

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , 'c'
                                    ]
                              then
                                12

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , 'd'
                                    ]
                              then
                                13

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , 'e'
                                    ]
                              then
                                14

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , 'f'
                                    ]
                              then
                                15

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , 'A'
                                    ]
                              then
                                10

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , 'B'
                                    ]
                              then
                                11

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , 'C'
                                    ]
                              then
                                12

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , 'D'
                                    ]
                              then
                                13

                              else if
                                Pine_kernel.equal
                                    [ Pine_kernel.take
                                        [ 4
                                        , Pine_kernel.skip
                                            [ param_1
                                            , param_3
                                            ]
                                        ]
                                    , 'E'
                                    ]
                              then
                                14

                              else
                                15
                            ]
                        )
                        param_3


            Test.hexStringToInt param_1_1 =
                Test.hexStringBytesToInt
                    0
                    0
                    param_1_1[0]
            """"
            .Trim());
    }

    [Fact]
    public void Parse_Test_apply_function_from_other_module()
    {
        var elmModuleTestListText =
            """"
            module TestList exposing (..)


            repeat : Int -> a -> List a
            repeat n value =
                repeatHelp [] n value


            repeatHelp : List a -> Int -> a -> List a
            repeatHelp result n value =
                if Pine_kernel.int_is_sorted_asc [ n, 0 ] then
                    result

                else
                    repeatHelp (Pine_kernel.concat [ [ value ], result ]) (n - 1) value

            """";

        var elmModuleTestText =
            """"
            module Test exposing (..)

            import TestList

                        
            listRepeatMultiply : Int -> a -> List a
            listRepeatMultiply n item =
                TestList.repeat
                    (Pine_kernel.int_mul [ n, 7 ])
                    item

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleTestListText, elmModuleTestText],
                includeDeclaration:
                declName =>
                {
                    return declName.FullName == "Test.listRepeatMultiply";
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        wholeProgramText.Trim().Should().Be(
            """"
            List.repeat param_1 param_2 =
                List.repeatHelp
                    []
                    param_1
                    param_2


            List.repeatHelp param_1 param_2 param_3 =
                if
                    Pine_kernel.int_is_sorted_asc
                        [ param_2
                        , 0
                        ]
                then
                    param_1

                else
                    List.repeatHelp
                        (Pine_kernel.concat
                            [ [ param_3
                              ]
                            , param_1
                            ]
                        )
                        (Pine_kernel.int_add
                            [ param_2
                            , -1
                            ]
                        )
                        param_3


            Test.listRepeatMultiply param_1 param_2 =
                List.repeat
                    (Pine_kernel.int_mul
                        [ param_1
                        , 7
                        ]
                    )
                    param_2
            
            """"
            .Trim());
    }

    private static readonly IReadOnlyDictionary<DeclQualifiedName, bool> s_bundledDeclarationsSupportStatus =
        /*
         * Supported status for a subset of declarations in the bundled Elm compiler/LS.
         * True means the declaration is currently supported by static program parsing.
         * */
        new Dictionary<DeclQualifiedName, bool>
        {
            { DeclQualifiedName.Create(["Basics"], "eq"), true },

            // { DeclQualifiedName.Create(["Basics"], "neq"), true },

            { DeclQualifiedName.Create(["Basics"], "add"), true },
            { DeclQualifiedName.Create(["Basics"], "sub"), true },
            { DeclQualifiedName.Create(["Basics"], "mul"), true },

            { DeclQualifiedName.Create(["Basics"], "idiv"), true },

            // { DeclQualifiedName.Create(["Basics"], "pow"), true },

            { DeclQualifiedName.Create(["Basics"], "and"), true },
            { DeclQualifiedName.Create(["Basics"], "or"), true },
            { DeclQualifiedName.Create(["Basics"], "append"), true },

            { DeclQualifiedName.Create(["Basics"], "lt"), true },
            { DeclQualifiedName.Create(["Basics"], "gt"), true },
            { DeclQualifiedName.Create(["Basics"], "le"), true },
            { DeclQualifiedName.Create(["Basics"], "ge"), true },

            { DeclQualifiedName.Create(["Basics"], "min"), true },
            { DeclQualifiedName.Create(["Basics"], "max"), true },

            { DeclQualifiedName.Create(["Basics"], "apR"), false },
            { DeclQualifiedName.Create(["Basics"], "apL"), false },

            { DeclQualifiedName.Create(["Basics"], "composeL"), false },
            { DeclQualifiedName.Create(["Basics"], "composeR"), false },

            /*
             * 
            { DeclQualifiedName.Create(["Basics"], "identity"), true },
            { DeclQualifiedName.Create(["Basics"], "always"), true },
            { DeclQualifiedName.Create(["Basics"], "not"), true },
            */

            { DeclQualifiedName.Create(["Basics"], "compare"), true },

            { DeclQualifiedName.Create(["Basics"], "modBy"), true },
            { DeclQualifiedName.Create(["Basics"], "remainderBy"), true },

            // { DeclQualifiedName.Create(["Basics"], "negate"), true },


            { DeclQualifiedName.Create(["String"], "toInt"), true },

            { DeclQualifiedName.Create(["String"], "trim"), true },
            { DeclQualifiedName.Create(["String"], "trimLeft"), true },
            { DeclQualifiedName.Create(["String"], "trimRight"), true },

            { DeclQualifiedName.Create(["String"], "startsWith"), true },
            { DeclQualifiedName.Create(["String"], "endsWith"), true },

            { DeclQualifiedName.Create(["String"], "toLower"), false }, // Using higher-order function 'map' internally
            { DeclQualifiedName.Create(["String"], "toUpper"), false },

            // { DeclQualifiedName.Create(["String"], "indexes"), true },

            { DeclQualifiedName.Create(["Dict"], "insert"), true },
            { DeclQualifiedName.Create(["Dict"], "remove"), true },

            { DeclQualifiedName.Create(["Bitwise"], "and"), true },
            { DeclQualifiedName.Create(["Bitwise"], "or"), true },
            { DeclQualifiedName.Create(["Bitwise"], "xor"), true },

            { DeclQualifiedName.Create(["Bitwise"], "complement"), true },

            { DeclQualifiedName.Create(["Bitwise"], "shiftLeftBy"), true },
            { DeclQualifiedName.Create(["Bitwise"], "shiftRightBy"), true },
            { DeclQualifiedName.Create(["Bitwise"], "shiftRightZfBy"), true },

            { DeclQualifiedName.Create(["Json","Decode"], "parseJsonStringToValue"), true },

            { DeclQualifiedName.Create(["Pine"], "blobBytesFromChar"), true },
            { DeclQualifiedName.Create(["Pine"], "intFromValue"), true },
            { DeclQualifiedName.Create(["Pine"], "valueFromInt"), true },
        }
        .ToFrozenDictionary();

    [Fact]
    public void Parse_bundled_modules()
    {
        var compiledEnv =
            BundledElmEnvironments.BundledElmCompilerCompiledEnvValue()
            ??
            throw new System.Exception("Failed to load Elm compiler from bundle.");

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var (staticProgram, _, declsFailed) =
            Core.CodeAnalysis.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnv,
                includeDeclaration:
                declName =>
                {
                    return s_bundledDeclarationsSupportStatus.ContainsKey(declName);
                },
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        foreach (var trackedDecl in s_bundledDeclarationsSupportStatus)
        {
            if (trackedDecl.Value)
            {
                if (declsFailed.ContainsKey(trackedDecl.Key))
                {
                    throw new System.Exception(
                        $"Declaration {trackedDecl.Key.FullName} was expected to be supported, but parsing failed: {declsFailed[trackedDecl.Key]}");
                }

                staticProgram.NamedFunctions.Should().ContainKey(trackedDecl.Key);
            }
            else
            {
                /*
                 * For the declarations mentioned in the tracked set, ensure we
                 * switch the 'supported' to true as soon as parsing does not fail anymore
                 * for that declaration.
                 * */

                declsFailed.Should().ContainKey(trackedDecl.Key);
            }
        }
    }

    [Fact]
    public void Build_environment_class_for_static_program()
    {
        var testCases = new[]
        {
            new
            {
                Name = "Empty",

                expression =
                (Expression)
                Expression.LiteralInstance(PineValue.EmptyBlob),

                environment =
                PineValue.EmptyList,

                expectedResult =
                Result<string, PineValueClass>.ok(PineValueClass.Create([])),
            },

            new
            {
                Name = "Everything observed",

                expression =
                (Expression)
                new Expression.ParseAndEval(
                    encoded: Expression.EnvironmentInstance,
                    environment: Expression.ListInstance([])),

                environment =
                PineValue.List(
                    [
                    PineValue.EmptyBlob,
                    StringEncoding.ValueFromString("Testing"),
                    ]),

                expectedResult =
                Result<string, PineValueClass>.ok(
                    PineValueClass.CreateEquals(
                        PineValue.List(
                        [
                        PineValue.EmptyBlob,
                        StringEncoding.ValueFromString("Testing"),
                        ])))
            }
        };

        for (var testCaseIndex = 0; testCaseIndex < testCases.Length; ++testCaseIndex)
        {
            var testCase = testCases[testCaseIndex];

            try
            {
                var envClassResult =
                    Core.CodeAnalysis.CodeAnalysis.MinimalValueClassForStaticProgram(
                        expression: testCase.expression,
                        availableEnvironment: PineValueClass.CreateEquals(testCase.environment));

                if (envClassResult.IsErrOrNull() is { } err)
                {
                    if (testCase.expectedResult.IsErrOrNull() is { } expectedErr)
                    {
                        err.Should().Be(
                            expectedErr,
                            because: "Error message should equal expected error message.");
                    }
                    else
                    {
                        throw new System.Exception(
                            $"Expected success, but got error: {err}");
                    }
                }

                if (envClassResult.IsOkOrNull() is not { } envClass)
                {
                    throw new System.NotImplementedException(
                        "Unexpected result type: " + envClassResult.GetType());
                }

                {
                    if (testCase.expectedResult.IsErrOrNull() is { } expectedErr)
                    {
                        throw new System.Exception(
                            $"Expected error, but got success: {envClass}");
                    }
                }

                if (testCase.expectedResult.IsOkOrNull() is not { } expectedEnvClass)
                {
                    throw new System.NotImplementedException(
                        "Unexpected result type: " + testCase.expectedResult.GetType());
                }

                envClass.Should().Be(expectedEnvClass);
            }
            catch (System.Exception e)
            {
                throw new System.Exception(
                    "Failed for test case '" + testCase.Name + "' at index " + testCaseIndex,
                    innerException: e);
            }
        }
    }
}
