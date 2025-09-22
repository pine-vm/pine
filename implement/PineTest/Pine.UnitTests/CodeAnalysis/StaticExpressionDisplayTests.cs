using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

using StaticExpression = Pine.Core.CodeAnalysis.StaticExpression<string>;


namespace Pine.UnitTests.CodeAnalysis;

public class StaticExpressionDisplayTests
{

    [Fact]
    public void Render_StaticExpression_RenderToString_Scenarios()
    {
        static StaticExpression Param_1_0() =>
            StaticExpressionExtension.BuildPathToExpression([1, 0], StaticExpression.EnvironmentInstance);

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

            new
            {
                Name = "List_containing_conditional",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        StaticExpression.ListInstance(
                            [
                                StaticExpression.ConditionalInstance(
                                    condition: StaticExpression.KernelApplicationInstance(
                                        function: "int_is_sorted_asc",
                                        input: StaticExpression.ListInstance(
                                            [
                                                Param_1_0(),
                                                StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)))
                                            ])),
                                    falseBranch: StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(2))),
                                    trueBranch: StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)))),
                                StaticExpression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(3)))
                            ]),
                        PineValueClass.Create([])))),

                Expected =
                """
                decl_a param_1_0 =
                    [ if
                        Pine_kernel.int_is_sorted_asc
                            [ param_1_0
                            , 1
                            ]
                      then
                        1

                      else
                        2
                    , 3
                    ]
                """
            },

            new
            {
                Name = "KernelApplication_Nested_Head_With_Parens",
                Program =
                new StaticProgram(
                    ImmutableDictionary<string, (Expression, StaticFunctionInterface, StaticExpression<string>, PineValueClass)>.Empty
                    .SetItem(
                        "decl_a",
                        (Expression.ListInstance([]),
                        InterfaceFromParamCount(1),
                        StaticExpression.KernelApplicationInstance(
                            function: "head",
                            input: StaticExpression.KernelApplicationInstance(
                                function: "skip",
                                input: StaticExpression.ListInstance(
                                    [
                                        StaticExpression.LiteralInstance(
                                            ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1))),

                                        Param_1_0(),
                                    ]
                                )
                            )
                        ),
                        PineValueClass.Create([])))),

                Expected =
                """
                decl_a param_1_0 =
                    Pine_kernel.head
                        (Pine_kernel.skip
                            [ 1
                            , param_1_0
                            ]
                        )
                """
            },
        };

        for (var i = 0; i < scenarios.Length; i++)
        {
            var sc = scenarios[i];

            var actual = StaticExpressionDisplay.RenderStaticProgram(sc.Program, substituteEnvironmentPath: null);

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
}
