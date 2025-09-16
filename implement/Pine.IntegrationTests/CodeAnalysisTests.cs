using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.PopularEncodings;
using Pine.Elm;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Xunit;

using StaticExpression = Pine.Core.CodeAnalysis.StaticExpression<string>;

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

        var staticProgram =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnv,
                includeDeclaration: declName => declName == new DeclQualifiedName(["Test"], "fibonacci"),
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        var wholeProgramText = RenderStaticProgram(staticProgram);

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

        var staticProgram =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnv,
                includeDeclaration:
                declName =>
                {
                    return declName == new DeclQualifiedName(["Test"], "factorial");
                },
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        var wholeProgramText = RenderStaticProgram(staticProgram);

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

        var staticProgram =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnv,
                includeDeclaration: declName => declName == new DeclQualifiedName(["Test"], "dictToShuffledList"),
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        var wholeProgramText = RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.dictToShuffledList param_1_0 =
                if
                    Pine_kernel.equal
                        [ RBEmpty_elm_builtin
                        , Pine_kernel.head
                            param_1_0
                        ]
                then
                    []

                else if
                    Pine_kernel.equal
                        [ RBNode_elm_builtin
                        , Pine_kernel.head
                            param_1_0
                        ]
                then
                    Pine_kernel.concat
                        [ Test.dictToShuffledList
                            (Pine_kernel.head
                                Pine_kernel.skip
                                    [ 3
                                    , Pine_kernel.head
                                        Pine_kernel.skip
                                            [ 1
                                            , param_1_0
                                            ]
                                    ]
                            )
                        , Test.dictToShuffledList
                            (Pine_kernel.head
                                Pine_kernel.skip
                                    [ 4
                                    , Pine_kernel.head
                                        Pine_kernel.skip
                                            [ 1
                                            , param_1_0
                                            ]
                                    ]
                            )
                        , [ [ Pine_kernel.head
                                Pine_kernel.skip
                                    [ 1
                                    , Pine_kernel.head
                                        Pine_kernel.skip
                                            [ 1
                                            , param_1_0
                                            ]
                                    ]
                            , Pine_kernel.head
                                Pine_kernel.skip
                                    [ 2
                                    , Pine_kernel.head
                                        Pine_kernel.skip
                                            [ 1
                                            , param_1_0
                                            ]
                                    ]
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

        var staticProgram =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnv,
                includeDeclaration: declName => declName == new DeclQualifiedName(["Test"], "convert0OrMore_base3"),
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        var wholeProgramText = RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.convert0OrMore_base3 param_1_0 param_1_1 param_1_2 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.length
                            Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_1_1
                                    , param_1_2
                                    ]
                                ]
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
    public void Render_StaticExpression_RenderToString_Scenarios()
    {
        static StaticExpression Param_1_0() =>
            StaticExpression.BuildPathToExpression([1, 0], StaticExpression.EnvironmentInstance);

        static StaticFunctionInterface InterfaceFromParamCount(int paramCount) =>
            new(
                [
                    .. Enumerable.Range(0, paramCount)
                    .Select(i => (IReadOnlyList<int>)[1, i])
                ]);

        static StaticExpression FunctionApplicationInstance(string functionName, IReadOnlyList<StaticExpression> arguments) =>
            StaticExpression.FunctionApplicationInstance(
                functionName,
                StaticExpression.ListInstance(
                    [
                    StaticExpression.ListInstance([]),
                    StaticExpression.ListInstance(arguments)
                    ]));

        var scenarios = new[]
        {
            new
            {
                Name = "Literal_Integer",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(0),
                        StaticExpression.LiteralInstance(
                            ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1))),
                        PineValueClass.Create([])))),
                Expected = """
                decl_a =
                    1
                """
            },

            new
            {
                Name = "Literal_Boolean_True",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(0),
                        StaticExpression.LiteralInstance(
                            ElmValueEncoding.ElmValueAsPineValue(ElmValue.TrueValue)),
                        PineValueClass.Create([])))),

                Expected = """
                decl_a =
                    True
                """
            },

            new
            {
                Name = "Parameter_Ref",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        Param_1_0(),
                        PineValueClass.Create([])))),

                Expected = """
                decl_a param_1_0 =
                    param_1_0
                """
            },

            new
            {
                Name = "Literal_Char",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(0),
                        StaticExpression.LiteralInstance(
                            ElmValueEncoding.ElmValueAsPineValue(ElmValue.CharInstance((int)'4'))),
                        PineValueClass.Create([])))),
                Expected = """
                decl_a =
                    '4'
                """
            },

            new
            {
                Name = "List_Param_And_Int",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        StaticExpression.ListInstance(
                            [
                                Param_1_0(),
                                StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)))
                            ]),
                        PineValueClass.Create([])))),
                Expected = """
                decl_a param_1_0 =
                    [ param_1_0
                    , 1
                    ]
                """
            },

            new
            {
                Name = "List_Param_And_Char",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        StaticExpression.ListInstance([
                            Param_1_0(),
                            StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.CharInstance((int)'b')))
                        ]),
                        PineValueClass.Create([])))),
                Expected = """
                decl_a param_1_0 =
                    [ param_1_0
                    , 'b'
                    ]
                """
            },

            new
            {
                Name = "List_containing_function_application",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(0),
                        StaticExpression.ListInstance(
                            [
                                StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(41))),
                                FunctionApplicationInstance(
                                    functionName: "test",
                                    arguments:
                                    [
                                        StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(123))),
                                    ])
                            ]),
                        PineValueClass.Create([])))
                    .SetItem(
                        "test",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        Param_1_0(),
                        PineValueClass.Create([])))),
                Expected = """
                decl_a =
                    [ 41
                    , test
                        123
                    ]


                test param_1_0 =
                    param_1_0
                """
            },

            new
            {
                Name = "Nested_list",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(0),
                        StaticExpression.ListInstance(
                            [
                                StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(41))),
                                StaticExpression.ListInstance(
                                    [
                                        StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(71))),
                                        StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(73))),
                                    ])
                            ]),
                        PineValueClass.Create([])))),
                Expected = """
                decl_a =
                    [ 41
                    , [ 71
                      , 73
                      ]
                    ]
                """
            },

            new
            {
                Name = "Nested_list_twice",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(0),
                        StaticExpression.ListInstance(
                            [
                                StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(41))),
                                StaticExpression.ListInstance(
                                    [
                                        StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(71))),
                                        StaticExpression.ListInstance(
                                            [
                                                StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(73))),
                                                StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(79))),
                                            ])
                                    ])
                            ]),
                        PineValueClass.Create([])))),
                Expected = """
                decl_a =
                    [ 41
                    , [ 71
                      , [ 73
                        , 79
                        ]
                      ]
                    ]
                """
            },

            new
            {
                Name = "KernelApplication_With_List",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        StaticExpression.KernelApplicationInstance(
                            function: "int_is_sorted_asc",
                            input: StaticExpression.ListInstance(
                                [
                                    Param_1_0(),
                                    StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)))
                                ])),
                        PineValueClass.Create([])))),
                Expected = """
                decl_a param_1_0 =
                    Pine_kernel.int_is_sorted_asc
                        [ param_1_0
                        , 1
                        ]
                """
            },

            new
            {
                Name = "Function_Application",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(0),
                        FunctionApplicationInstance(
                            functionName: "decl_b",
                            arguments:
                            [
                                StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(123))),
                            ]),
                        PineValueClass.Create([])))
                    .SetItem(
                        "decl_b",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        Param_1_0(),
                        PineValueClass.Create([])))),

                Expected = """
                decl_a =
                    decl_b
                        123


                decl_b param_1_0 =
                    param_1_0
                """
            },

            new
            {
                Name = "FunctionApplication_With_Kernel_Arg",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        FunctionApplicationInstance(
                            functionName: "myFunc",
                            arguments:
                            [
                                StaticExpression.KernelApplicationInstance(
                                    function: "int_add",
                                    input: StaticExpression.ListInstance(
                                        [
                                            Param_1_0(),
                                            StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(-1)))
                                        ]))
                            ]),
                        PineValueClass.Create([])))
                    .SetItem(
                        "myFunc",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        Param_1_0(),
                        PineValueClass.Create([])))),

                Expected =
                """
                decl_a param_1_0 =
                    myFunc
                        (Pine_kernel.int_add
                            [ param_1_0
                            , -1
                            ]
                        )


                myFunc param_1_0 =
                    param_1_0
                
                """
                },

            new
            {
                Name = "Conditional_Complex",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        StaticExpression.ConditionalInstance(
                            condition: StaticExpression.KernelApplicationInstance(
                                function: "int_is_sorted_asc",
                                input: StaticExpression.ListInstance(
                                [
                                    Param_1_0(),
                                    StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)))
                                ])),
                            falseBranch: StaticExpression.KernelApplicationInstance(
                                function: "int_mul",
                                input: StaticExpression.ListInstance(
                                [
                                    FunctionApplicationInstance(
                                        functionName: "decl_b",
                                        arguments:
                                        [
                                            StaticExpression.KernelApplicationInstance(
                                                function: "int_add",
                                                input: StaticExpression.ListInstance(
                                                [
                                                    Param_1_0(),
                                                    StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(-1)))
                                                ]))
                                        ]),
                                    Param_1_0()
                                ])),
                            trueBranch: StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)))),
                        PineValueClass.Create([])))
                    .SetItem(
                        "decl_b",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        Param_1_0(),
                        PineValueClass.Create([])))),

                Expected =
                """
                decl_a param_1_0 =
                    if
                        Pine_kernel.int_is_sorted_asc
                            [ param_1_0
                            , 1
                            ]
                    then
                        1

                    else
                        Pine_kernel.int_mul
                            [ decl_b
                                (Pine_kernel.int_add
                                    [ param_1_0
                                    , -1
                                    ]
                                )
                            , param_1_0
                            ]


                decl_b param_1_0 =
                    param_1_0
                
                """
                },

            new
            {
                Name = "Literal_Blob",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(0),
                        StaticExpression.LiteralInstance(PineValue.Blob([0x12, 0x34, 1, 3])),
                        PineValueClass.Create([])))),
                Expected = """
                decl_a =
                    Blob 0x12340103
                """
            },

            new
            {
                Name = "List_With_Blob_Item",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(0),
                        StaticExpression.ListInstance(
                            [
                                StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1))),
                                StaticExpression.LiteralInstance(PineValue.Blob([0x12, 0x34, 1, 3])),
                                StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(2)))
                            ]),
                        PineValueClass.Create([])))),

                Expected =
                """
                decl_a =
                    [ 1
                    , Blob 0x12340103
                    , 2
                    ]
                """
            },

            new
            {
                Name = "Literal_ListValue_With_Blob_Item",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(0),
                        StaticExpression.LiteralInstance(
                            PineValue.List(
                                PineValue.Blob([0x01, 1, 3, 7]),
                                PineValue.Blob([0xAB, 1, 3, 7])
                            )),
                        PineValueClass.Create([])))),
                Expected = """
                decl_a =
                    [Blob 0x01010307, Blob 0xab010307]
                """
            },

            new
            {
                Name = "FunctionApplication_With_Blob_Arg",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(0),
                        FunctionApplicationInstance(
                            functionName: "myFunc",
                            arguments: [
                                StaticExpression.LiteralInstance(PineValue.Blob([0x12, 0x34, 1, 7]))
                            ]),
                        PineValueClass.Create([])))
                    .SetItem(
                        "myFunc",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        Param_1_0(),
                        PineValueClass.Create([])))),

                Expected =
                """
                decl_a =
                    myFunc
                        (Blob 0x12340107)


                myFunc param_1_0 =
                    param_1_0
                
                """
                },

            new
            {
                Name = "Conditional_Nested_ElseIf",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        StaticExpression.ConditionalInstance(
                            condition: StaticExpression.KernelApplicationInstance(
                                function: "int_is_sorted_asc",
                                input: StaticExpression.ListInstance(
                                [
                                    Param_1_0(),
                                    StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(0)))
                                ])),
                            falseBranch: StaticExpression.ConditionalInstance(
                                condition: StaticExpression.KernelApplicationInstance(
                                    function: "int_is_sorted_asc",
                                    input: StaticExpression.ListInstance(
                                    [
                                        Param_1_0(),
                                        StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)))
                                    ])),
                                falseBranch: StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(2))),
                                trueBranch: StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)))) ,
                            trueBranch: StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(0)))),
                        PineValueClass.Create([])))),

                Expected =
                """
                decl_a param_1_0 =
                    if
                        Pine_kernel.int_is_sorted_asc
                            [ param_1_0
                            , 0
                            ]
                    then
                        0

                    else if
                        Pine_kernel.int_is_sorted_asc
                            [ param_1_0
                            , 1
                            ]
                    then
                        1

                    else
                        2
                """
            },
        };

        for (var i = 0; i < scenarios.Length; i++)
        {
            var sc = scenarios[i];

            var actual = RenderStaticProgram(sc.Program);

            try
            {
                actual.Trim().Should()
                    .Be(
                    sc.Expected.Trim(),
                    because: $"Scenario '{sc.Name}' at index {i} should render as expected.");
            }
            catch (System.Exception e)
            {
                throw new System.Exception($"Failure in scenario '{sc.Name}' at index {i}. Actual was:\n{actual}", e);
            }
        }
    }

    static string RenderStaticProgram(StaticProgram staticProgram)
    {
        IReadOnlyList<string> namedFunctionsTexts =
            [..staticProgram.NamedFunctions
            .OrderBy(kvp => kvp.Key)
            .Select(kvp => RenderNamedFunction(staticProgram, kvp.Key, kvp.Value.body))];

        var wholeProgramText =
            string.Join(
                "\n\n",
                namedFunctionsTexts);

        return wholeProgramText;
    }

    static string RenderNamedFunction(
        StaticProgram staticProgram,
        string functionName,
        StaticExpression functionBody)
    {
        var functionInterface = staticProgram.GetFunctionApplicationRendering(functionName).FunctionInterface;

        var functionParameters = functionInterface.ParamsPaths;

        var headerText =
            (functionName + " " + string.Join(" ", functionParameters.Select(RenderParamRef))).Trim() + " =";

        return
            headerText +
            "\n" +
            StaticExpressionDisplay.RenderToString(
                functionBody,
                blobValueRenderer: StaticExpressionDisplay.DefaultBlobRenderer,
                functionApplicationRenderer: staticProgram.GetFunctionApplicationRendering,
                environmentPathReferenceRenderer: RenderParamRef(functionInterface),
                indentString: "    ",
                indentLevel: 1);
    }

    private static System.Func<IReadOnlyList<int>, string?> RenderParamRef(StaticFunctionInterface functionInterface)
    {
        return path =>
        {
            if (functionInterface.ParamsPaths.Contains(path, IntPathEqualityComparer.Instance))
            {
                return RenderParamRef(path);
            }

            return null;
        };
    }

    private static string RenderParamRef(IReadOnlyList<int> path)
    {
        return "param_" + string.Join('_', path);
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
