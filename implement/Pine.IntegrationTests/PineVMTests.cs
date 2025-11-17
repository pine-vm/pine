using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.IntermediateVM;
using System.Collections.Generic;
using Xunit;

using StackFrameInstructions = Pine.Core.Interpreter.IntermediateVM.StackFrameInstructions;

namespace Pine.IntegrationTests;

public class PineVMTests
{
    [Fact]
    public void Evaluate_expression()
    {
        var testCases = new[]
        {
            new
            {
                expression = (Expression)
                Expression.LiteralInstance(PineValue.Blob([1, 4, 7])),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(
                    PineValue.Blob([1, 4, 7]))
            },
            new
            {
                expression = (Expression)Expression.ListInstance([]),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(PineValue.EmptyList)
            },
            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    input: Expression.ListInstance([]),
                    function: "concat"
                ),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(PineValue.EmptyList)
            },
            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    input:
                    Expression.ListInstance(
                        [
                            Expression.ListInstance(
                                [
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(1)),
                                ]),

                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input: Expression.EnvironmentInstance
                            ),

                            new Expression.ParseAndEval(
                                encoded:
                                Expression.LiteralInstance(
                                    ExpressionEncoding.EncodeExpressionAsValue(
                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.head),
                                            input: Expression.EnvironmentInstance
                                        ))),
                                environment:
                                Expression.KernelApplicationInstance
                                (
                                    function: "skip",
                                    input: Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(
                                            IntegerEncoding.EncodeSignedInteger(1)),

                                        Expression.EnvironmentInstance,
                                        ])
                                ))
                        ]),
                    function: "concat"
                ),
                environment =
                PineValue.List(
                    [
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(3)]),
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(11)]),
                    ]),

                expected = Result<string, PineValue>.ok(PineValue.List(
                    [
                        IntegerEncoding.EncodeSignedInteger(1),
                        IntegerEncoding.EncodeSignedInteger(3),
                        IntegerEncoding.EncodeSignedInteger(11),
                    ]))
            },

            new
            {
                expression =
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.LiteralInstance(PineValue.Blob([4])),
                    falseBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),
                    trueBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13))),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(
                    IntegerEncoding.EncodeSignedInteger(13))
            },

            new
            {
                expression=
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.LiteralInstance(PineValue.Blob([2])),
                    falseBranch:
                    new Expression.ParseAndEval(
                        encoded:
                        Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input:
                                Expression.KernelApplicationInstance
                                (
                                    function: "skip",
                                    input: Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(
                                            IntegerEncoding.EncodeSignedInteger(1)),

                                        Expression.EnvironmentInstance,
                                        ])
                                )
                            ),
                        Expression.EnvironmentInstance),
                    trueBranch:
                    Expression.LiteralInstance(
                        IntegerEncoding.EncodeSignedInteger(17))),

                environment =
                PineValue.List(
                    [
                        IntegerEncoding.EncodeSignedInteger(43),
                        ExpressionEncoding.EncodeExpressionAsValue(
                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input: Expression.EnvironmentInstance
                            ))
                    ]),

                expected = Result<string, PineValue>.ok(
                    IntegerEncoding.EncodeSignedInteger(43))
            },

            new
            {
                expression=
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.LiteralInstance(PineValue.Blob([4])),
                    falseBranch:
                    new Expression.ParseAndEval(
                        encoded:
                        Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input: Expression.EnvironmentInstance
                            ),
                        Expression.EnvironmentInstance),
                    trueBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17))),

                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(
                    IntegerEncoding.EncodeSignedInteger(17))
            },

            new
            {
                expression=
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.LiteralInstance(PineValue.Blob([4])),
                    falseBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),
                    trueBranch:
                    new Expression.ParseAndEval(
                        encoded:
                        Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input:
                                Expression.KernelApplicationInstance
                                (
                                    function: "skip",
                                    input: Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(
                                            IntegerEncoding.EncodeSignedInteger(1)),

                                        Expression.EnvironmentInstance,
                                        ])
                                )
                            ),
                        Expression.EnvironmentInstance)),

                environment =
                PineValue.List(
                    [
                        IntegerEncoding.EncodeSignedInteger(41),

                        ExpressionEncoding.EncodeExpressionAsValue(
                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input: Expression.EnvironmentInstance
                            ))
                    ]),

                expected =
                Result<string, PineValue>.ok(IntegerEncoding.EncodeSignedInteger(41))
            },
        };

        foreach (var testCase in testCases)
        {
            var pineVM = SetupVM.Create();

            var evaluated =
                pineVM.EvaluateExpression(
                    testCase.expression,
                    testCase.environment);

            evaluated.Should().Be(
                testCase.expected,
                "Evaluated expression should match the expected value.");
        }
    }

    [Fact]
    public void Compile_stack_frame_instructions()
    {
        var testCases = new[]
        {
            new
            {
                expression =
                (Expression)
                Expression.LiteralInstance(PineValue.EmptyBlob),

                expected =
                WithZeroParameters(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValue.EmptyBlob),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)Expression.ListInstance(
                    [
                        Expression.LiteralInstance(PineValue.EmptyList),

                        Expression.LiteralInstance(PineValue.EmptyBlob),
                    ]),

                expected =
                WithZeroParameters(
                    [
                        StackInstruction.Push_Literal(PineValue.EmptyList),

                        StackInstruction.Push_Literal(PineValue.EmptyBlob),

                        StackInstruction.Build_List(2),

                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)Expression.ListInstance(
                    [
                        Expression.LiteralInstance(PineValue.EmptyList),

                        Expression.KernelApplicationInstance
                        (
                            function: "skip",
                            input: Expression.ListInstance(
                                [
                                Expression.LiteralInstance(
                                    IntegerEncoding.EncodeSignedInteger(1)),

                                Expression.EnvironmentInstance,
                                ])
                        ),

                        Expression.LiteralInstance(PineValue.EmptyBlob),

                        Expression.KernelApplicationInstance
                        (
                            function: "skip",
                            input: Expression.ListInstance(
                                [
                                Expression.LiteralInstance(
                                    IntegerEncoding.EncodeSignedInteger(1)),

                                Expression.EnvironmentInstance,
                                ])
                        ),
                    ]),

                /*
                 * Expect CSE, reusing the result of the kernel application.
                 * */
                expected =
                WithGenericParameters(
                    [
                    StackInstruction.Push_Literal(PineValue.EmptyList),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Skip_Const(1),

                    StackInstruction.Local_Set(1),

                    StackInstruction.Push_Literal(PineValue.EmptyBlob),

                    StackInstruction.Local_Get(1),

                    StackInstruction.Build_List(4),

                    StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ListInstance(
                    [
                        Expression.LiteralInstance(PineValue.EmptyList),
                        Expression.KernelApplicationInstance
                        (
                            function: "concat",
                            input:
                            Expression.KernelApplicationInstance
                            (
                                function: "skip",
                                input:
                                Expression.ListInstance(
                                    [
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(13)),

                                    Expression.EnvironmentInstance,
                                    ])
                            )
                        ),

                        Expression.LiteralInstance(PineValue.EmptyBlob),

                        Expression.KernelApplicationInstance
                        (
                            function: "concat",
                            input:
                            Expression.KernelApplicationInstance
                            (
                                function: "skip",
                                input:
                                Expression.ListInstance(
                                    [
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(13)),

                                    Expression.EnvironmentInstance,
                                    ])
                            )
                        ),
                    ]),

                /*
                 * Expect CSE does not separate subexpressions for which all parents are already in separate instructions.
                 * */
                expected =
                WithGenericParameters(
                    [
                    StackInstruction.Push_Literal(PineValue.EmptyList),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Skip_Const(13),

                    StackInstruction.Concat_Generic,

                    StackInstruction.Local_Set(1),

                    StackInstruction.Push_Literal(PineValue.EmptyBlob),

                    StackInstruction.Local_Get(1),

                    StackInstruction.Build_List(4),

                    StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.ParseAndEval(
                    encoded: Expression.EnvironmentInstance,
                    environment: Expression.EnvironmentInstance),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Local_Get(0),

                        StackInstruction.Parse_And_Eval_Binary,

                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ListInstance(
                    [
                        new Expression.ParseAndEval(
                            encoded: Expression.EnvironmentInstance,
                            environment: Expression.EnvironmentInstance),

                        Expression.LiteralInstance(PineValue.EmptyBlob),
                    ]),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Local_Get(0),

                        StackInstruction.Parse_And_Eval_Binary,

                        StackInstruction.Push_Literal(PineValue.EmptyBlob),

                        StackInstruction.Build_List(2),

                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ListInstance(
                    [
                        new Expression.ParseAndEval(
                            encoded: Expression.EnvironmentInstance,
                            environment: Expression.ListInstance(
                            [
                                new Expression.ParseAndEval(
                                    encoded:
                                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11)),
                                    environment:
                                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13))),
                                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),
                            ])),
                        Expression.LiteralInstance(PineValue.EmptyBlob),
                    ]),

                expected =
                WithGenericParameters(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(13)),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(11)),

                    StackInstruction.Parse_And_Eval_Binary,

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Build_List(2),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Parse_And_Eval_Binary,

                    StackInstruction.Push_Literal(PineValue.EmptyBlob),

                    StackInstruction.Build_List(2),

                    StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)Expression.ListInstance(
                    [
                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(17)),

                        Expression.ConditionalInstance(
                            condition:
                            Expression.EnvironmentInstance,
                            trueBranch:
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(21)),
                            falseBranch:
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(23))
                        ),

                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(27)),
                    ]),

                expected =
                WithGenericParameters(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Jump_If_True(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(23)),

                    StackInstruction.Jump_Unconditional(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(21)),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(27)),

                    StackInstruction.Build_List(3),

                    StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11)),
                    trueBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                    falseBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17))),

                expected =
                WithZeroParameters(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(11)),

                    StackInstruction.Jump_If_True(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Return,

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(13)),

                    StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.LiteralInstance(
                        IntegerEncoding.EncodeSignedInteger(11)),
                    trueBranch:
                    new Expression.ParseAndEval(
                        encoded:
                        Expression.KernelApplicationInstance
                            (
                                function: "skip",
                                input: Expression.ListInstance(
                                    [
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(13)),

                                    Expression.EnvironmentInstance,
                                    ])
                            ),
                        environment:
                        Expression.EnvironmentInstance),
                    falseBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17))),

                expected =
                WithGenericParameters(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(11)),

                    StackInstruction.Jump_If_True(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Return,

                    StackInstruction.Local_Get(0),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Skip_Const(13),

                    StackInstruction.Parse_And_Eval_Binary,

                    StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ListInstance(
                    [
                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(47)),

                        Expression.ConditionalInstance(
                            condition:
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11)),

                            trueBranch:
                            new Expression.ParseAndEval(
                                encoded:
                                Expression.KernelApplicationInstance
                                    (
                                        function: "skip",
                                        input: Expression.ListInstance(
                                            [
                                            Expression.LiteralInstance(
                                                IntegerEncoding.EncodeSignedInteger(13)),

                                            Expression.EnvironmentInstance,
                                            ])
                                    ),

                                environment:
                                Expression.EnvironmentInstance),

                            falseBranch:
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)))
                ]),

                expected =
                WithGenericParameters(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(47)),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(11)),

                    StackInstruction.Jump_If_True(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Jump_Unconditional(5),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Skip_Const(13),

                    StackInstruction.Parse_And_Eval_Binary,

                    StackInstruction.Build_List(2),

                    StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11)),
                    falseBranch:
                    Expression.ConditionalInstance(
                        condition:
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                        falseBranch:
                        new Expression.ParseAndEval(
                            encoded: Expression.EnvironmentInstance,
                            environment: Expression.EnvironmentInstance),
                        trueBranch:
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(21))),
                    trueBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(23))),

                expected =
                WithGenericParameters(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(11)),

                    StackInstruction.Jump_If_True(8),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(13)),

                    StackInstruction.Jump_If_True(4),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Parse_And_Eval_Binary,

                    StackInstruction.Return,

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(21)),

                    StackInstruction.Return,

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(23)),

                    StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ListInstance(
                    [
                        Expression.ConditionalInstance(
                            condition:
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11)),
                            trueBranch:
                            new Expression.ParseAndEval(
                                encoded:
                                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(41)),
                                environment:
                                Expression.EnvironmentInstance),
                            falseBranch:
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17))),

                        Expression.ConditionalInstance(
                            condition:
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                            trueBranch:
                            new Expression.ParseAndEval(
                                encoded:
                                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(43)),
                                environment:
                                Expression.EnvironmentInstance),
                            falseBranch:
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(19))),
                ]),

                expected =
                WithGenericParameters(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(11)),

                    StackInstruction.Jump_If_True(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Jump_Unconditional(4),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(41)),

                    StackInstruction.Parse_And_Eval_Binary,

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(13)),

                    StackInstruction.Jump_If_True(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(19)),

                    StackInstruction.Jump_Unconditional(4),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(43)),

                    StackInstruction.Parse_And_Eval_Binary,

                    StackInstruction.Build_List(2),

                    StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11)),
                    trueBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                    falseBranch:
                    Expression.ConditionalInstance(
                        condition:
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(21)),
                        trueBranch:
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(23)),
                        falseBranch:
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(27)))),

                expected =
                WithZeroParameters(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(11)),

                    StackInstruction.Jump_If_True(6),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(21)),

                    StackInstruction.Jump_If_True(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(27)),

                    StackInstruction.Return,

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(23)),

                    StackInstruction.Return,

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(13)),

                    StackInstruction.Return
                    ])
            },

            new
            {
                /*
                 * Fusion of kernel functions skip and head.
                 * */
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    function: nameof(KernelFunction.head),
                    input:
                    Expression.KernelApplicationInstance
                    (
                        function: "skip",
                        input: Expression.ListInstance(
                            [
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),

                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.reverse),
                                input: Expression.EnvironmentInstance
                            ),
                            ])
                    )
                ),

                expected =
                WithGenericParameters(
                    [
                    StackInstruction.Local_Get(0),

                    StackInstruction.Reverse,

                    StackInstruction.Skip_Head_Const(17),

                    StackInstruction.Return
                    ])
            },


            new
            {
                /*
                 * Fusion of nested kernel functions skip and head.
                 * */
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    function: nameof(KernelFunction.head),
                    input:
                    Expression.KernelApplicationInstance
                    (
                        function: "skip",
                        input: Expression.ListInstance(
                            [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(17)),

                            Expression.ListInstance(
                                [
                                Expression.LiteralInstance(
                                    IntegerEncoding.EncodeSignedInteger(21)),

                                Expression.KernelApplicationInstance
                                (
                                    function: nameof(KernelFunction.head),
                                    input:
                                    Expression.KernelApplicationInstance
                                    (
                                        function: "skip",
                                        input: Expression.ListInstance(
                                            [
                                            Expression.LiteralInstance(
                                                IntegerEncoding.EncodeSignedInteger(23)),

                                            Expression.KernelApplicationInstance(
                                                function: nameof(KernelFunction.negate),
                                                input: Expression.EnvironmentInstance)
                                            ])
                                    )
                                )
                                ]),
                            ])
                    )
                ),

                expected =
                WithGenericParameters(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(21)),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Negate,

                    StackInstruction.Skip_Head_Const(23),

                    StackInstruction.Build_List(2),

                    StackInstruction.Skip_Head_Const(17),

                    StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.ListInstance(
                        [
                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.int_mul),
                                input:
                                Expression.ListInstance(
                                    [
                                        Expression.EnvironmentInstance,
                                        Expression.EnvironmentInstance,
                                        Expression.EnvironmentInstance,
                                    ])
                            ),
                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.int_mul),
                                input:
                                Expression.ListInstance(
                                    [
                                        Expression.EnvironmentInstance,
                                        Expression.EnvironmentInstance,
                                        Expression.EnvironmentInstance,
                                    ])
                            ),
                        ]),
                    trueBranch:
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                    falseBranch:
                    Expression.ConditionalInstance(
                        condition:
                        Expression.KernelApplicationInstance
                        (
                            function: nameof(KernelFunction.int_mul),
                            input:
                            Expression.ListInstance(
                                [
                                    Expression.EnvironmentInstance,
                                    Expression.EnvironmentInstance,
                                    Expression.EnvironmentInstance,
                                ])
                        ),
                        trueBranch:
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(23)),
                        falseBranch:
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(27)))),

                expected =
                WithGenericParameters(
                    [
                    StackInstruction.Local_Get(0),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Int_Mul_Binary,

                    StackInstruction.Local_Get(0),

                    StackInstruction.Int_Mul_Binary,

                    StackInstruction.Local_Set(1),

                    StackInstruction.Local_Get(1),

                    StackInstruction.Build_List(2),

                    StackInstruction.Jump_If_True(6),

                    StackInstruction.Local_Get(1),

                    StackInstruction.Jump_If_True(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(27)),

                    StackInstruction.Return,

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(23)),

                    StackInstruction.Return,

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(13)),

                    StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: "int_add",
                    input:
                    Expression.ListInstance(
                        [
                            Expression.KernelApplicationInstance(
                                function: "head",
                                input:
                                Expression.KernelApplicationInstance(
                                    function: "skip",
                                    input:
                                    Expression.ListInstance(
                                        [
                                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                                            Expression.EnvironmentInstance,
                                        ]))),

                            Expression.KernelApplicationInstance(
                                function: "negate",
                                input:
                                Expression.KernelApplicationInstance(
                                    function: "head",
                                    input:
                                    Expression.KernelApplicationInstance(
                                        function: "skip",
                                        input:
                                        Expression.ListInstance(
                                            [
                                                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(2)),
                                                Expression.EnvironmentInstance,
                                            ])))),
                        ])),

                expected =
                WithParameters(
                    [[1],[2]],
                    [
                    StackInstruction.Local_Get(0),

                    StackInstruction.Local_Get(1),

                    StackInstruction.Int_Sub_Binary,

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_add),
                    input:
                    Expression.ListInstance(
                        [
                            Expression.KernelApplicationInstance(
                                function: "head",
                                input:
                                Expression.KernelApplicationInstance(
                                    function: "skip",
                                    input:
                                    Expression.ListInstance(
                                        [
                                            Expression.LiteralInstance(
                                                IntegerEncoding.EncodeSignedInteger(1)),

                                            Expression.EnvironmentInstance,
                                        ]))),

                            Expression.KernelApplicationInstance(
                                function: "int_mul",
                                input:
                                Expression.ListInstance(
                                    [
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(-1)),

                                    Expression.KernelApplicationInstance(
                                        function: "head",
                                        input:
                                        Expression.KernelApplicationInstance(
                                            function: "skip",
                                            input:
                                            Expression.ListInstance(
                                                [
                                                    Expression.LiteralInstance(
                                                        IntegerEncoding.EncodeSignedInteger(2)),

                                                    Expression.EnvironmentInstance,
                                                ])))
                                    ])),
                        ])),

                expected =
                WithParameters(
                    [[1],[2]],
                    [
                    StackInstruction.Local_Get(0),

                    StackInstruction.Local_Get(1),

                    StackInstruction.Int_Sub_Binary,

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: "equal",
                    input:
                    Expression.ListInstance(
                        [
                        Expression.KernelApplicationInstance(
                            function: "head",
                            input: Expression.EnvironmentInstance),

                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                        ])),

                expected =
                WithParameters(
                    [[0]],
                    [
                    StackInstruction.Local_Get(0),

                    StackInstruction.Equal_Binary_Const(
                        IntegerEncoding.EncodeSignedInteger(13)),

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.equal),
                    input:
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),

                        Expression.KernelApplicationInstance(
                            function: nameof(KernelFunction.head),
                            input: Expression.EnvironmentInstance),
                        ])),

                expected =
                WithParameters(
                    [[0]],
                    [
                    StackInstruction.Local_Get(0),

                    StackInstruction.Equal_Binary_Const(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: "negate",
                    input:
                    Expression.KernelApplicationInstance(
                        function: "equal",
                        input:
                        Expression.ListInstance(
                            [
                            Expression.KernelApplicationInstance(
                                function: "head",
                                input:
                                Expression.KernelApplicationInstance(
                                    function: "skip",
                                    input:
                                    Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11)),
                                        Expression.EnvironmentInstance,
                                        ])
                                )),

                            Expression.KernelApplicationInstance(
                                function: "head",
                                input:
                                Expression.KernelApplicationInstance(
                                    function: "skip",
                                    input:
                                    Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                                        Expression.EnvironmentInstance,
                                        ])
                                )),
                            ]))),

                expected =
                WithParameters(
                    [[11],[13]],
                    [
                    StackInstruction.Local_Get(0),

                    StackInstruction.Local_Get(1),

                    StackInstruction.Not_Equal_Binary,

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: "int_is_sorted_asc",
                    input:
                    Expression.ListInstance(
                        [
                        Expression.KernelApplicationInstance(
                            function: "head",
                            input:
                            Expression.KernelApplicationInstance(
                                function: "skip",
                                input:
                                Expression.ListInstance(
                                    [
                                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),
                                    Expression.EnvironmentInstance,
                                    ])
                            )),

                        Expression.KernelApplicationInstance(
                            function: "head",
                            input:
                            Expression.KernelApplicationInstance(
                                function: "skip",
                                input:
                                Expression.ListInstance(
                                    [
                                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(19)),
                                    Expression.EnvironmentInstance,
                                    ])
                            )),
                        ])),

                expected =
                WithParameters(
                    [[17],[19]],
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Local_Get(1),

                        new StackInstruction(
                            StackInstructionKind.Int_Less_Than_Or_Equal_Binary),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: "negate",
                    input:
                    Expression.KernelApplicationInstance(
                        function: "int_is_sorted_asc",
                        input:
                        Expression.ListInstance(
                            [
                            Expression.KernelApplicationInstance(
                                function: "head",
                                input:
                                Expression.KernelApplicationInstance(
                                    function: "skip",
                                    input:
                                    Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11)),
                                        Expression.EnvironmentInstance,
                                        ])
                                )),

                            Expression.KernelApplicationInstance(
                                function: "head",
                                input:
                                Expression.KernelApplicationInstance(
                                    function: "skip",
                                    input:
                                    Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                                        Expression.EnvironmentInstance,
                                        ])
                                )),
                            ]))),

                expected =
                WithParameters(
                    [[11],[13]],
                    [
                    StackInstruction.Local_Get(1),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Int_Less_Than_Binary,

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: "int_add",
                    input:
                    Expression.ListInstance(
                        [
                        Expression.KernelApplicationInstance(
                            function: "head",
                            input: Expression.EnvironmentInstance),

                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                        ])),

                expected =
                WithParameters(
                    [[0]],
                    [
                    StackInstruction.Local_Get(0),

                    StackInstruction.Int_Add_Const(13),

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: "int_mul",
                    input:
                    Expression.ListInstance(
                        [
                        Expression.KernelApplicationInstance(
                            function: "head",
                            input: Expression.EnvironmentInstance),

                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),
                        ])),

                expected =
                WithParameters(
                    [[0]],
                    [
                    StackInstruction.Local_Get(0),

                    StackInstruction.Int_Mul_Const(17),

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    function: nameof(KernelFunction.equal),
                    input: Expression.ListInstance(
                        [
                        Expression.KernelApplicationInstance
                        (
                            function: nameof(KernelFunction.head),
                            input: Expression.EnvironmentInstance
                            ),

                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(71)),
                    ])
                    ),

                expected =
                WithParameters(
                    [[0]],
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Equal_Binary_Const(
                            IntegerEncoding.EncodeSignedInteger(71)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    function: nameof(KernelFunction.equal),
                    input: Expression.ListInstance(
                        [
                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(73)),

                        Expression.KernelApplicationInstance
                        (
                            function: nameof(KernelFunction.head),
                            input: Expression.EnvironmentInstance
                            ),
                    ])
                    ),

                expected =
                WithParameters(
                    [[0]],
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Equal_Binary_Const(
                            IntegerEncoding.EncodeSignedInteger(73)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    function: nameof(KernelFunction.equal),
                    input: Expression.ListInstance(
                        [
                        Expression.KernelApplicationInstance
                        (
                            function: nameof(KernelFunction.length),
                            input: Expression.EnvironmentInstance
                            ),

                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(11)),
                    ])
                    ),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Length_Equal_Const(11),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    function: nameof(KernelFunction.equal),
                    input: Expression.ListInstance(
                        [
                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(13)),

                        Expression.KernelApplicationInstance
                        (
                            function: nameof(KernelFunction.length),
                            input: Expression.EnvironmentInstance
                            ),
                    ])
                    ),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Length_Equal_Const(13),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input: Expression.ListInstance(
                        [
                            Expression.EnvironmentInstance,

                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(13)),
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Less_Than_Or_Equal_Const(13),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(17)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Greater_Than_Or_Equal_Const(17),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(23)),

                            Expression.EnvironmentInstance,

                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(27)),
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Greater_Than_Or_Equal_Const(23),

                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Less_Than_Or_Equal_Const(27),

                        StackInstruction.Logical_And_Binary,

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(23)),

                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.head),
                                input:
                                Expression.KernelApplicationInstance
                                (
                                    function: nameof(KernelFunction.skip),
                                    input: Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(
                                            IntegerEncoding.EncodeSignedInteger(41)),

                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.head),
                                            input:
                                            Expression.KernelApplicationInstance
                                            (
                                                function: nameof(KernelFunction.skip),
                                                input: Expression.ListInstance(
                                                    [
                                                    Expression.LiteralInstance(
                                                        IntegerEncoding.EncodeSignedInteger(43)),

                                                    Expression.EnvironmentInstance,
                                                ])
                                                )
                                            ),
                                    ])
                                    )
                                ),

                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(27)),
                        ])),

                expected =
                WithParameters(
                    [[43,41]],
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Greater_Than_Or_Equal_Const(23),

                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Less_Than_Or_Equal_Const(27),

                        StackInstruction.Logical_And_Binary,

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.bit_and),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(23)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Bit_And_Const(
                            IntegerEncoding.EncodeSignedInteger(23)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.bit_and),
                    input: Expression.ListInstance(
                        [
                            Expression.EnvironmentInstance,

                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(27)),
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Bit_And_Const(
                            IntegerEncoding.EncodeSignedInteger(27)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.bit_or),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(31)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Bit_Or_Const(
                            IntegerEncoding.EncodeSignedInteger(31)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.bit_or),
                    input: Expression.ListInstance(
                        [
                            Expression.EnvironmentInstance,

                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(37)),
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Bit_Or_Const(
                            IntegerEncoding.EncodeSignedInteger(37)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.bit_shift_left),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(13)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Bit_Shift_Left_Const(13),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.bit_shift_right),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(17)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Bit_Shift_Right_Const(17),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.take),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(71)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Take_Const(71),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.skip),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(73)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Skip_Const(73),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_add),
                    Expression.ListInstance(
                        [
                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.concat),
                                input:
                                Expression.ListInstance(
                                    [
                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.take),
                                            input:
                                            Expression.ListInstance(
                                                [
                                                    Expression.LiteralInstance(
                                                        IntegerEncoding.EncodeSignedInteger(1)),

                                                    Expression.LiteralInstance(
                                                        IntegerEncoding.EncodeSignedInteger(0)),
                                                ])
                                        ),

                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.head),
                                            input: Expression.EnvironmentInstance
                                        ),
                                    ])
                            ),

                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(79)),
                        ])),

                expected =
                WithParameters(
                    [[0]],
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Unsigned_Add_Const(79),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    Expression.ListInstance(
                        [
                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.concat),
                                input:
                                Expression.ListInstance(
                                    [
                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.take),
                                            input:
                                            Expression.ListInstance(
                                                [
                                                    Expression.LiteralInstance(
                                                        IntegerEncoding.EncodeSignedInteger(1)),

                                                    Expression.LiteralInstance(
                                                        IntegerEncoding.EncodeSignedInteger(0)),
                                                ])
                                        ),

                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.head),
                                            input: Expression.EnvironmentInstance
                                        ),
                                    ])
                            ),

                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(79)),
                        ])),

                expected =
                WithParameters(
                    [[0]],
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Unsigned_Less_Than_Or_Equal_Const(79),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(83)),

                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.concat),
                                input:
                                Expression.ListInstance(
                                    [
                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.take),
                                            input:
                                            Expression.ListInstance(
                                                [
                                                    Expression.LiteralInstance(
                                                        IntegerEncoding.EncodeSignedInteger(1)),

                                                    Expression.LiteralInstance(
                                                        IntegerEncoding.EncodeSignedInteger(0)),
                                                ])
                                        ),

                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.head),
                                            input: Expression.EnvironmentInstance
                                        ),
                                    ])
                            ),
                        ])),

                expected =
                WithParameters(
                    [[0]],
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Unsigned_Greater_Than_Or_Equal_Const(83),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(83)),

                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.concat),
                                input:
                                Expression.ListInstance(
                                    [
                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.take),
                                            input:
                                            Expression.ListInstance(
                                                [
                                                    Expression.LiteralInstance(
                                                        IntegerEncoding.EncodeSignedInteger(1)),

                                                    Expression.LiteralInstance(
                                                        IntegerEncoding.EncodeSignedInteger(0)),
                                                ])
                                        ),

                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.head),
                                            input: Expression.EnvironmentInstance
                                        ),
                                    ])
                            ),

                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(107)),
                        ])),

                expected =
                WithParameters(
                    [[0]],
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Unsigned_Greater_Than_Or_Equal_Const(83),

                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Unsigned_Less_Than_Or_Equal_Const(107),

                        StackInstruction.Logical_And_Binary,

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    Expression.KernelApplicationInstance
                    (
                        function: nameof(KernelFunction.int_is_sorted_asc),
                        Expression.ListInstance(
                            [
                                Expression.LiteralInstance(
                                    IntegerEncoding.EncodeSignedInteger(83)),

                                Expression.KernelApplicationInstance
                                (
                                    function: nameof(KernelFunction.concat),
                                    input:
                                    Expression.ListInstance(
                                        [
                                            Expression.KernelApplicationInstance
                                            (
                                                function: nameof(KernelFunction.take),
                                                input:
                                                Expression.ListInstance(
                                                    [
                                                        Expression.LiteralInstance(
                                                            IntegerEncoding.EncodeSignedInteger(1)),

                                                        Expression.LiteralInstance(
                                                            IntegerEncoding.EncodeSignedInteger(0)),
                                                    ])
                                            ),

                                            Expression.KernelApplicationInstance
                                            (
                                                function: nameof(KernelFunction.head),
                                                input: Expression.EnvironmentInstance
                                            ),
                                        ])
                                ),
                        ])),

                    falseBranch:
                    Expression.LiteralInstance(
                        IntegerEncoding.EncodeSignedInteger(107)),

                    trueBranch:
                    Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_add),
                    Expression.ListInstance(
                        [
                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.concat),
                                input:
                                Expression.ListInstance(
                                    [
                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.take),
                                            input:
                                            Expression.ListInstance(
                                                [
                                                    Expression.LiteralInstance(
                                                        IntegerEncoding.EncodeSignedInteger(1)),

                                                    Expression.LiteralInstance(
                                                        IntegerEncoding.EncodeSignedInteger(0)),
                                                ])
                                        ),

                                        Expression.KernelApplicationInstance
                                        (
                                            function: nameof(KernelFunction.head),
                                            input: Expression.EnvironmentInstance
                                        ),
                                    ])
                            ),

                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(79)),
                        ]))
                    ),

                expected =
                WithParameters(
                    [[0]],
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Unsigned_Greater_Than_Or_Equal_Const(83),

                        StackInstruction.Jump_If_True(2),

                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(107)),

                        StackInstruction.Return,

                        StackInstruction.Local_Get(0),

                        StackInstruction.Int_Unsigned_Add_Const(79),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    function: nameof(KernelFunction.reverse),
                    input:
                    Expression.KernelApplicationInstance
                    (
                        function: nameof(KernelFunction.take),
                        input:
                        Expression.ListInstance(
                            [
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(3)),
                            Expression.KernelApplicationInstance
                            (
                                function: nameof(KernelFunction.reverse),
                                input:
                                Expression.EnvironmentInstance
                            ),
                            ])
                    )
                ),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Take_Last_Const(3),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance
                (
                    function: nameof(KernelFunction.skip),
                    input:
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),

                        Expression.KernelApplicationInstance
                        (
                            function: nameof(KernelFunction.int_add),
                            input:
                            Expression.ListInstance(
                                [
                                    Expression.KernelApplicationInstance
                                    (
                                        function: nameof(KernelFunction.concat),
                                        input:
                                        Expression.ListInstance(
                                            [
                                            Expression.LiteralInstance(
                                                PineValue.BlobSingleByte(0x04)),

                                            Expression.EnvironmentInstance,
                                            ])
                                    ),

                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(0)),
                                ])
                            ),
                        ])
                ),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Blob_Trim_Leading_Zeros(minRemainingCount:1),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input:
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(0)),

                        Expression.EnvironmentInstance,
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(0)),

                        StackInstruction.Starts_With_Const_At_Offset_Var(
                            PineValue.BlobSingleByte(4)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input:
                    Expression.ListInstance(
                        [
                        Expression.EnvironmentInstance,

                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(-1)),
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(0)),

                        StackInstruction.Starts_With_Const_At_Offset_Var(
                            PineValue.BlobSingleByte(2)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.equal),
                    input:
                    Expression.ListInstance(
                        [
                        Expression.KernelApplicationInstance(
                            function: nameof(KernelFunction.take),
                            input:
                            Expression.ListInstance(
                                [
                                Expression.LiteralInstance(
                                    IntegerEncoding.EncodeSignedInteger(0)),

                                Expression.EnvironmentInstance,
                                ])),

                        Expression.LiteralInstance(PineValue.EmptyBlob),
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Is_Blob_Value,

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.equal),
                    input:
                    Expression.ListInstance(
                        [
                        Expression.KernelApplicationInstance(
                            function: nameof(KernelFunction.take),
                            input:
                            Expression.ListInstance(
                                [
                                Expression.LiteralInstance(
                                    IntegerEncoding.EncodeSignedInteger(1)),

                                Expression.EnvironmentInstance,
                                ])),

                        Expression.LiteralInstance(PineValue.Blob([123])),
                        ])),

                expected =
                WithGenericParameters(
                    [
                        StackInstruction.Local_Get(0),

                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(0)),

                        StackInstruction.Starts_With_Const_At_Offset_Var(
                            PineValue.Blob([123])),

                        StackInstruction.Return,
                    ])
            },
        };

        var parseCache = new PineVMParseCache();

        for (var testCaseIndex = 0; testCaseIndex < testCases.Length; ++testCaseIndex)
        {
            var testCase = testCases[testCaseIndex];

            var compiled =
                ExpressionCompilation.CompileExpression(
                    testCase.expression,
                    specializations: [],
                    parseCache,
                    disableReduction: true,
                    skipInlining: (_, _) => false,
                    enableTailRecursionOptimization: false);

            try
            {
                compiled.Generic.Instructions.Should().HaveCount(
                    testCase.expected.Instructions.Count,
                    "Instructions count for test case " + testCaseIndex);

                for (var instructionIndex = 0; instructionIndex < testCase.expected.Instructions.Count; instructionIndex++)
                {
                    compiled.Generic.Instructions[instructionIndex].Should().Be(
                        testCase.expected.Instructions[instructionIndex],
                        "Instruction at index " + instructionIndex + " of " + compiled.Generic.Instructions.Count);
                }

                compiled.Generic.Should().BeEquivalentTo(testCase.expected);
            }
            catch (System.Exception e)
            {
                throw new System.Exception(
                    $"Failed for test case {testCaseIndex} with expression {testCase.expression}", e);
            }
        }
    }

    private static StackFrameInstructions WithZeroParameters(
        IReadOnlyList<StackInstruction> instructions)
    {
        return new StackFrameInstructions(
            Parameters: StaticFunctionInterface.ZeroParameters,
            instructions);
    }

    private static StackFrameInstructions WithGenericParameters(
        IReadOnlyList<StackInstruction> instructions)
    {
        return new StackFrameInstructions(
            Parameters: StaticFunctionInterface.Generic,
            instructions);
    }


    private static StackFrameInstructions WithParameters(
        IReadOnlyList<IReadOnlyList<int>> parameters,
        IReadOnlyList<StackInstruction> instructions)
    {
        return new StackFrameInstructions(
            Parameters: StaticFunctionInterface.FromPathsSorted(parameters),
            instructions);
    }
}
