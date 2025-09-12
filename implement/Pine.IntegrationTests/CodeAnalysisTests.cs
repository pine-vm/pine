using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Json;
using Pine.Core.PopularEncodings;
using Pine.Elm;
using System.Collections.Generic;
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

        var compiledDecl =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                compiledEnv,
                moduleName: "Test",
                declarationName: "fibonacci",
                parseCache: parseCache)
            .Extract(err => throw new System.Exception(err));

        compiledDecl.Should().NotBeNull();

        var stubTextualRepr =
            EncodePineExpressionAsJson.ToJsonString(compiledDecl.functionRecord.InnerFunction);

        var applicationArgumentValue =
            ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(5));

        var functionApplicationRecord =
            ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(
                compiledDecl.functionRecord,
                [applicationArgumentValue])
            .Extract(err => throw new System.Exception("Failed applying function arguments: " + err));

        var namesFromCompiledEnv =
            new NamesFromCompiledEnv(compiledEnv, parseCache);

        var staticProgram =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                rootExpression: functionApplicationRecord.expression,
                rootEnvironment: functionApplicationRecord.environment,
                nameForDecl: namesFromCompiledEnv.NameFromDecl,
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();


        IReadOnlyList<string> namedFunctionsTexts =
            [..staticProgram.staticProgram.NamedFunctions
            .OrderBy(kvp => kvp.Key)
            .Select(kvp => RenderNamedFunction(kvp.Key, kvp.Value.body))];

        var wholeProgramText =
            string.Join(
                "\n\n",
                namedFunctionsTexts);

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

        var stubTextualRepr =
            EncodePineExpressionAsJson.ToJsonString(compiledDecl.functionRecord.InnerFunction);

        var applicationArgumentValue =
            ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(5));

        var functionApplicationRecord =
            ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(
                compiledDecl.functionRecord,
                [applicationArgumentValue])
            .Extract(err => throw new System.Exception("Failed applying function arguments: " + err));

        var namesFromCompiledEnv =
            new NamesFromCompiledEnv(compiledEnv, parseCache);

        var staticProgram =
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                rootExpression: functionApplicationRecord.expression,
                rootEnvironment: functionApplicationRecord.environment,
                nameForDecl: namesFromCompiledEnv.NameFromDecl,
                parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        staticProgram.Should().NotBeNull();

        IReadOnlyList<string> namedFunctionsTexts =
            [..staticProgram.staticProgram.NamedFunctions
            .OrderBy(kvp => kvp.Key)
            .Select(kvp => RenderNamedFunction(kvp.Key, kvp.Value.body))];

        var wholeProgramText =
            string.Join(
                "\n\n",
                namedFunctionsTexts);

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

    static string RenderParamRef(StaticExpression.ParameterReferenceExpression paramRef)
    {
        return "param_" + string.Join('_', paramRef.Path);
    }

    static string RenderNamedFunction(
        string functionName,
        StaticExpression functionBody)
    {
        var allParameters =
            StaticExpression.EnumerateAllDescendants(functionBody)
            .OfType<StaticExpression.ParameterReferenceExpression>()
            .Distinct()
            .OrderBy(paramRef => paramRef.Path.Count)
            .ToArray();

        var headerText =
            functionName + " " + string.Join(" ", allParameters.Select(RenderParamRef));

        return
            headerText + " =\n" +
            StaticExpressionDisplay.RenderToString(
                functionBody,
                blobValueRenderer: StaticExpressionDisplay.DefaultBlobRenderer,
                indentString: "    ",
                indentLevel: 1);
    }

    [Fact]
    public void Render_StaticExpression_RenderToString_Scenarios()
    {
        static StaticExpression Param_1_0() =>
            StaticExpression.ParameterReferenceInstance([1, 0]);

        var scenarios = new[]
        {
            new
            {
                Name = "Literal_Integer",
                Expr = StaticExpression.LiteralInstance(
                    ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1))),
                Expected = """
                1
                """
            },
            new
            {
                Name = "Parameter_Ref",
                Expr = Param_1_0(),
                Expected = """
                param_1_0
                """
            },
            new
            {
                Name = "List_Param_And_Int",
                Expr = StaticExpression.ListInstance(
                    [
                        Param_1_0(),
                        StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)))
                    ]),
                Expected = """
                [ param_1_0
                , 1
                ]
                """
            },
            new
            {
                Name = "List_containing_function_application",
                Expr = StaticExpression.ListInstance(
                    [
                        StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(41))),

                        StaticExpression.FunctionApplicationInstance(
                            functionName: "test",
                            arguments:
                            [
                                StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(123))),
                            ])
                    ]),
                Expected = """
                [ 41
                , test
                    123
                ]
                """
            },
            new
            {
                Name = "KernelApplication_With_List",
                Expr = StaticExpression.KernelApplicationInstance(
                    function: "int_is_sorted_asc",
                    input: StaticExpression.ListInstance(
                        [
                            Param_1_0(),
                            StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)))
                        ])),
                Expected = """
                Pine_kernel.int_is_sorted_asc
                    [ param_1_0
                    , 1
                    ]
                """
            },
            new
            {
                Name = "Function_Application",
                Expr =
                StaticExpression.FunctionApplicationInstance(
                    functionName: "anon_92e7ce17",
                    arguments:
                    [
                        StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(123))),
                    ])
                ,
                Expected =
                """"
                anon_92e7ce17
                    123
                """"
            },
            new
            {
                Name = "FunctionApplication_With_Kernel_Arg",
                Expr = StaticExpression.FunctionApplicationInstance(
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
                Expected = """
                myFunc
                    (Pine_kernel.int_add
                        [ param_1_0
                        , -1
                        ]
                    )
                """
            },
            new
            {
                Name = "Conditional_Complex",
                Expr = StaticExpression.ConditionalInstance(
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
                            StaticExpression.FunctionApplicationInstance(
                                functionName: "anon_92e7ce17_92a8c7c6",
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
                    trueBranch: StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1))))
                ,
                Expected = """
                if
                    Pine_kernel.int_is_sorted_asc
                        [ param_1_0
                        , 1
                        ]
                then
                    1

                else
                    Pine_kernel.int_mul
                        [ anon_92e7ce17_92a8c7c6
                            (Pine_kernel.int_add
                                [ param_1_0
                                , -1
                                ]
                            )
                        , param_1_0
                        ]
                """
            },

            new
            {
                Name = "Literal_Blob",
                Expr = StaticExpression.LiteralInstance(PineValue.Blob([0x12, 0x34, 1, 3])),
                Expected = """
                Blob 0x12340103
                """
            },

            new
            {
                Name = "List_With_Blob_Item",
                Expr = StaticExpression.ListInstance(
                    [
                        StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1))),
                        StaticExpression.LiteralInstance(PineValue.Blob([0x12, 0x34, 1, 3])),
                        StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(2)))
                    ]),
                Expected = """
                [ 1
                , Blob 0x12340103
                , 2
                ]
                """
            },

            new
            {
                Name = "Literal_ListValue_With_Blob_Item",
                Expr = StaticExpression.LiteralInstance(
                    PineValue.List(
                        PineValue.Blob([0x01, 1, 3, 7]),
                        PineValue.Blob([0xAB, 1, 3, 7])
                    )),
                Expected = """
                [Blob 0x01010307, Blob 0xab010307]
                """
            },
            new
            {
                Name = "FunctionApplication_With_Blob_Arg",
                Expr = StaticExpression.FunctionApplicationInstance(
                    functionName: "myFunc",
                    arguments: [
                        StaticExpression.LiteralInstance(PineValue.Blob([0x12, 0x34, 1, 7]))
                    ]),
                Expected = """
                myFunc
                    (Blob 0x12340107)
                """
            },
            new
            {
                Name = "Conditional_Nested_ElseIf",
                Expr = StaticExpression.ConditionalInstance(
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
                    trueBranch: StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(0))))
                ,
                Expected = """
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
            }
        };

        for (var i = 0; i < scenarios.Length; i++)
        {
            var sc = scenarios[i];

            var actual =
                StaticExpressionDisplay.RenderToString(
                    sc.Expr,
                    blobValueRenderer: StaticExpressionDisplay.DefaultBlobRenderer,
                    indentString: "    ");

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
