using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.PopularEncodings;
using Pine.Elm;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.IntegrationTests;

public class CodeAnalysisTests
{
    [Fact]
    public void Parse_Fibonacci()
    {
        var elmJsonFile =
            """
            {
                "type": "application",
                "source-directories": [
                    "src"
                ],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": {
                        "elm/core": "1.0.5"
                    },
                    "indirect": {
                    }
                },
                "test-dependencies": {
                    "direct": {
                        "elm-explorations/test": "2.2.0"
                    },
                    "indirect": {
                    }
                }
            }
            """;

        var elmModuleText =
            """
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

            """;

        var appCodeTree =
            BlobTreeWithStringPath.EmptyTree
            .SetNodeAtPathSorted(
                ["elm.json"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmJsonFile)))
            .SetNodeAtPathSorted(
                ["src", "Test.elm"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmModuleText)));

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: [["src", "Test.elm"]],
                skipLowering: true,
                skipFilteringForSourceDirs: false)
            .Extract(err => throw new System.Exception(err));

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var namesFromCompiledEnv =
            NamesFromCompiledEnv.FromCompiledEnvironment(parsedEnv, parseCache);

        var (staticProgram, declsFailed) =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnv,
                includeDeclaration:
                declName =>
                {
                    return declName.Namespaces.SequenceEqual(["Test"]);
                },
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.fibonacci param_1_0 =
                if
                    Pine_kernel.int_is_sorted_asc
                        [ param_1_0
                        , 2
                        ]
                then
                    param_1_0

                else
                    Pine_kernel.int_add
                        [ Test.fibonacci
                            (Pine_kernel.int_add
                                [ param_1_0
                                , -2
                                ]
                            )
                        , Test.fibonacci
                            (Pine_kernel.int_add
                                [ param_1_0
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
        var elmJsonFile =
            """
            {
                "type": "application",
                "source-directories": [
                    "src"
                ],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": {
                        "elm/core": "1.0.5"
                    },
                    "indirect": {
                    }
                },
                "test-dependencies": {
                    "direct": {
                        "elm-explorations/test": "2.2.0"
                    },
                    "indirect": {
                    }
                }
            }
            """;

        var elmModuleText =
            """
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

            """;

        var appCodeTree =
            BlobTreeWithStringPath.EmptyTree
            .SetNodeAtPathSorted(
                ["elm.json"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmJsonFile)))
            .SetNodeAtPathSorted(
                ["src", "Test.elm"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmModuleText)));

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: [["src", "Test.elm"]],
                skipLowering: true,
                skipFilteringForSourceDirs: false)
            .Extract(err => throw new System.Exception(err));

        var parseCache = new PineVMParseCache();

        var compiledDecl =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                compiledEnv,
                moduleName: "Test",
                declarationName: "factorial",
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(err));

        compiledDecl.Should().NotBeNull();

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var namesFromCompiledEnv =
            NamesFromCompiledEnv.FromCompiledEnvironment(parsedEnv, parseCache);

        var (staticProgram, declsFailed) =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnv,
                includeDeclaration:
                declName =>
                {
                    return declName.Namespaces.SequenceEqual(["Test"]);
                },
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.factorial param_1_0 =
                if
                    Pine_kernel.int_is_sorted_asc
                        [ param_1_0
                        , 1
                        ]
                then
                    1

                else
                    Pine_kernel.int_mul
                        [ Test.factorial
                            (Pine_kernel.int_add
                                [ param_1_0
                                , -1
                                ]
                            )
                        , param_1_0
                        ]
            """".Trim());
    }

    [Fact]
    public void Parse_Test_dictToShuffledList()
    {
        var elmJsonFile =
            """
            {
                "type": "application",
                "source-directories": [
                    "src"
                ],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": {
                        "elm/core": "1.0.5"
                    },
                    "indirect": {
                    }
                },
                "test-dependencies": {
                    "direct": {
                        "elm-explorations/test": "2.2.0"
                    },
                    "indirect": {
                    }
                }
            }
            """;

        /*
         * Use a form that us unlikely to occur also in the standard libraries,
         * to avoid code analysis picking up the name of the same function in the standard library.
         * */

        var elmModuleText =
            """
            module Test exposing (..)

            dictToShuffledList : Dict k v -> List ( k, v )
            dictToShuffledList dict =
                case dict of
                    RBEmpty_elm_builtin ->
                        []

                    RBNode_elm_builtin _ key value left right ->
                        Pine_kernel.concat [ dictToShuffledList left, dictToShuffledList right, [ ( key, value ) ] ]
            """;

        var appCodeTree =
            BlobTreeWithStringPath.EmptyTree
            .SetNodeAtPathSorted(
                ["elm.json"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmJsonFile)))
            .SetNodeAtPathSorted(
                ["src", "Test.elm"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmModuleText)));

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: [["src", "Test.elm"]],
                skipLowering: true,
                skipFilteringForSourceDirs: false)
            .Extract(err => throw new System.Exception(err));

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var namesFromCompiledEnv =
            NamesFromCompiledEnv.FromCompiledEnvironment(parsedEnv, parseCache);

        var (staticProgram, declsFailed) =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnv,
                includeDeclaration:
                declName =>
                {
                    return declName.Namespaces.SequenceEqual(["Test"]);
                },
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.dictToShuffledList param_1_0 =
                if
                    Pine_kernel.equal
                        [ RBEmpty_elm_builtin
                        , param_1_0[0]
                        ]
                then
                    []

                else if
                    Pine_kernel.equal
                        [ RBNode_elm_builtin
                        , param_1_0[0]
                        ]
                then
                    Pine_kernel.concat
                        [ Test.dictToShuffledList
                            param_1_0[1][3]
                        , Test.dictToShuffledList
                            param_1_0[1][4]
                        , [ [ param_1_0[1][1]
                            , param_1_0[1][2]
                            ]
                          ]
                        ]

                else
                    <always_crash>

            """"
            .Trim());
    }

    [Fact]
    public void Parse_Test_convert0OrMore_base3()
    {
        var elmJsonFile =
            """
            {
                "type": "application",
                "source-directories": [
                    "src"
                ],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": {
                        "elm/core": "1.0.5"
                    },
                    "indirect": {
                    }
                },
                "test-dependencies": {
                    "direct": {
                        "elm-explorations/test": "2.2.0"
                    },
                    "indirect": {
                    }
                }
            }
            """;

        /*
         * Use a form that us unlikely to occur also in the standard libraries,
         * to avoid code analysis picking up the name of the same function in the standard library.
         * */

        var elmModuleText =
            """
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
            """;

        var appCodeTree =
            BlobTreeWithStringPath.EmptyTree
            .SetNodeAtPathSorted(
                ["elm.json"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmJsonFile)))
            .SetNodeAtPathSorted(
                ["src", "Test.elm"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmModuleText)));

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: [["src", "Test.elm"]],
                skipLowering: true,
                skipFilteringForSourceDirs: false)
            .Extract(err => throw new System.Exception(err));

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var (staticProgram, declsFailed) =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnv,
                includeDeclaration:
                declName =>
                {
                    return declName.Namespaces.SequenceEqual(["Test"]);
                },
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.convert0OrMore_base3 param_1_0 param_1_1 param_1_2 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.length
                            (Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_1_1
                                    , param_1_2
                                    ]
                                ]
                            )
                        , 0
                        ]
                then
                    [ param_1_0
                    , param_1_1
                    ]

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_2
                                ]
                            ]
                        , '0'
                        ]
                then
                    Test.convert0OrMore_base3
                        (Pine_kernel.int_mul
                            [ param_1_0
                            , 3
                            ]
                        )
                        (Pine_kernel.int_add
                            [ param_1_1
                            , 4
                            ]
                        )
                        param_1_2

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_2
                                ]
                            ]
                        , '1'
                        ]
                then
                    Test.convert0OrMore_base3
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , 3
                                ]
                            , 1
                            ]
                        )
                        (Pine_kernel.int_add
                            [ param_1_1
                            , 4
                            ]
                        )
                        param_1_2

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_2
                                ]
                            ]
                        , '2'
                        ]
                then
                    Test.convert0OrMore_base3
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , 3
                                ]
                            , 2
                            ]
                        )
                        (Pine_kernel.int_add
                            [ param_1_1
                            , 4
                            ]
                        )
                        param_1_2

                else
                    [0,-1]
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

        var (staticProgram, declsFailed) =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnv,
                includeDeclaration:
                declName =>
                {
                    return declName == new DeclQualifiedName(["Basics"], "compare");
                },
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

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
        var elmJsonFile =
            """
            {
                "type": "application",
                "source-directories": [
                    "src"
                ],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": {
                        "elm/core": "1.0.5"
                    },
                    "indirect": {
                    }
                },
                "test-dependencies": {
                    "direct": {
                        "elm-explorations/test": "2.2.0"
                    },
                    "indirect": {
                    }
                }
            }
            """;

        /*
         * Using a modified form of idiv to avoid matching compiled value from Elm core library.
         * */

        var elmModuleText =
            """
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
            """;

        var appCodeTree =
            BlobTreeWithStringPath.EmptyTree
            .SetNodeAtPathSorted(
                ["elm.json"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmJsonFile)))
            .SetNodeAtPathSorted(
                ["src", "Test.elm"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmModuleText)));

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: [["src", "Test.elm"]],
                skipLowering: true,
                skipFilteringForSourceDirs: false)
            .Extract(err => throw new System.Exception(err));

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var (staticProgram, declsFailed) =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnv,
                includeDeclaration:
                declName =>
                {
                    return declName == new DeclQualifiedName(["Test"], "idiv");
                },
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.idiv param_1_0 param_1_1 =
                if
                    Pine_kernel.equal
                        [ param_1_1
                        , 0
                        ]
                then
                    0

                else if
                    Pine_kernel.equal
                        [ if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_1_0
                                ]
                          then
                            False

                          else
                            True
                        , if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_1_1
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
                                , param_1_0
                                ]
                        then
                            param_1_0

                        else
                            Pine_kernel.int_mul
                                [ param_1_0
                                , -1
                                ]
                        if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_1_1
                                ]
                        then
                            param_1_1

                        else
                            Pine_kernel.int_mul
                                [ param_1_1
                                , -1
                                ]
                        0

                else
                    Pine_kernel.int_mul
                        [ Test.idivHelper
                            if
                                Pine_kernel.int_is_sorted_asc
                                    [ 0
                                    , param_1_0
                                    ]
                            then
                                param_1_0

                            else
                                Pine_kernel.int_mul
                                    [ param_1_0
                                    , -1
                                    ]
                            if
                                Pine_kernel.int_is_sorted_asc
                                    [ 0
                                    , param_1_1
                                    ]
                            then
                                param_1_1

                            else
                                Pine_kernel.int_mul
                                    [ param_1_1
                                    , -1
                                    ]
                            0
                        , -1
                        ]


            Test.idivHelper param_1_0 param_1_1 param_1_2 =
                if
                    Pine_kernel.int_is_sorted_asc
                        [ Pine_kernel.int_mul
                            [ param_1_1
                            , 17
                            ]
                        , param_1_0
                        ]
                then
                    Pine_kernel.int_add
                        [ Pine_kernel.int_mul
                            [ Test.idivHelper
                                param_1_0
                                (Pine_kernel.int_mul
                                    [ param_1_1
                                    , 17
                                    ]
                                )
                                0
                            , 17
                            ]
                        , Test.idivHelper
                            (Pine_kernel.int_add
                                [ param_1_0
                                , Pine_kernel.int_mul
                                    [ Test.idivHelper
                                        param_1_0
                                        (Pine_kernel.int_mul
                                            [ param_1_1
                                            , 17
                                            ]
                                        )
                                        0
                                    , Pine_kernel.int_mul
                                        [ param_1_1
                                        , 17
                                        ]
                                    , -1
                                    ]
                                ]
                            )
                            param_1_1
                            0
                        ]

                else if
                    Pine_kernel.int_is_sorted_asc
                        [ param_1_1
                        , param_1_0
                        ]
                then
                    Test.idivHelper
                        (Pine_kernel.int_add
                            [ param_1_0
                            , Pine_kernel.int_mul
                                [ param_1_1
                                , -1
                                ]
                            ]
                        )
                        param_1_1
                        (Pine_kernel.int_add
                            [ param_1_2
                            , 1
                            ]
                        )

                else
                    param_1_2
            
            """"
            .Trim());
    }

    [Fact]
    public void Parse_Test_apply_function_from_other_module()
    {
        var elmJsonFile =
            """
            {
                "type": "application",
                "source-directories": [
                    "src"
                ],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": {
                        "elm/core": "1.0.5"
                    },
                    "indirect": {
                    }
                },
                "test-dependencies": {
                    "direct": {
                        "elm-explorations/test": "2.2.0"
                    },
                    "indirect": {
                    }
                }
            }
            """;

        var elmModuleTestListText =
            """
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

            """;

        var elmModuleTestText =
            """
            module Test exposing (..)

            import TestList

                        
            listRepeatMultiply : Int -> a -> List a
            listRepeatMultiply n item =
                TestList.repeat
                    (Pine_kernel.int_mul [ n, 7 ])
                    item

            """;

        var appCodeTree =
            BlobTreeWithStringPath.EmptyTree
            .SetNodeAtPathSorted(
                ["elm.json"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmJsonFile)))
            .SetNodeAtPathSorted(
                ["src", "TestList.elm"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmModuleTestListText)))
            .SetNodeAtPathSorted(
                ["src", "Test.elm"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmModuleTestText)));

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: [["src", "Test.elm"]],
                skipLowering: true,
                skipFilteringForSourceDirs: false)
            .Extract(err => throw new System.Exception(err));

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var (staticProgram, declsFailed) =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnv,
                includeDeclaration:
                declName =>
                {
                    return declName == new DeclQualifiedName(["Test"], "listRepeatMultiply");
                },
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.listRepeatMultiply param_1_0 param_1_1 =
                TestList.repeatHelp
                    []
                    (Pine_kernel.int_mul
                        [ param_1_0
                        , 7
                        ]
                    )
                    param_1_1


            TestList.repeatHelp param_1_0 param_1_1 param_1_2 =
                if
                    Pine_kernel.int_is_sorted_asc
                        [ param_1_1
                        , 0
                        ]
                then
                    param_1_0

                else
                    TestList.repeatHelp
                        (Pine_kernel.concat
                            [ [ param_1_2
                              ]
                            , param_1_0
                            ]
                        )
                        (Pine_kernel.int_add
                            [ param_1_1
                            , -1
                            ]
                        )
                        param_1_2
            
            """"
            .Trim());
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
                    CodeAnalysis.MinimalValueClassForStaticProgram(
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
