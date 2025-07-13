using FluentAssertions;
using Pine.Core;
using Pine.Core.PopularEncodings;
using Pine.PineVM;
using Xunit;

using StackFrameInstructions = Pine.PineVM.PineVM.StackFrameInstructions;

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
                (Expression)new Expression.KernelApplication
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
                (Expression)new Expression.KernelApplication
                (
                    input:
                    Expression.ListInstance(
                        [
                            Expression.ListInstance(
                                [
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(1)),
                                ]),

                            new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.head),
                                input: Expression.EnvironmentInstance
                            ),

                            new Expression.ParseAndEval(
                                encoded:
                                Expression.LiteralInstance(
                                    ExpressionEncoding.EncodeExpressionAsValue(
                                        new Expression.KernelApplication
                                        (
                                            function: nameof(KernelFunction.head),
                                            input: Expression.EnvironmentInstance
                                        ))),
                                environment:
                                new Expression.KernelApplication
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
                        new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.head),
                                input:
                                new Expression.KernelApplication
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
                            new Expression.KernelApplication
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
                        new Expression.KernelApplication
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
                        new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.head),
                                input:
                                new Expression.KernelApplication
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
                            new Expression.KernelApplication
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
            var pineVM = new PineVM.PineVM();

            var evaluated = pineVM.EvaluateExpression(
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
                new StackFrameInstructions(
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
                new StackFrameInstructions(
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

                        new Expression.KernelApplication
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

                        new Expression.KernelApplication
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
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Literal(PineValue.EmptyList),

                    StackInstruction.Push_Environment,

                    StackInstruction.Skip_Const(1),

                    StackInstruction.Local_Set(0),

                    StackInstruction.Push_Literal(PineValue.EmptyBlob),

                    StackInstruction.Local_Get(0),

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
                        new Expression.KernelApplication
                        (
                            function: "concat",
                            input:
                            new Expression.KernelApplication
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

                        new Expression.KernelApplication
                        (
                            function: "concat",
                            input:
                            new Expression.KernelApplication
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
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Literal(PineValue.EmptyList),

                    StackInstruction.Push_Environment,

                    StackInstruction.Skip_Const(13),

                    StackInstruction.Concat_Generic,

                    StackInstruction.Local_Set(0),

                    StackInstruction.Push_Literal(PineValue.EmptyBlob),

                    StackInstruction.Local_Get(0),

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
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Push_Environment,

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
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Push_Environment,

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
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(13)),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(11)),

                    StackInstruction.Parse_And_Eval_Binary,

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Build_List(2),

                    StackInstruction.Push_Environment,

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
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Push_Environment,

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
                new StackFrameInstructions(
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
                        new Expression.KernelApplication
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
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(11)),

                    StackInstruction.Jump_If_True(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Return,

                    StackInstruction.Push_Environment,

                    StackInstruction.Push_Environment,

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
                                new Expression.KernelApplication
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
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(47)),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(11)),

                    StackInstruction.Jump_If_True(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Jump_Unconditional(5),

                    StackInstruction.Push_Environment,

                    StackInstruction.Push_Environment,

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
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(11)),

                    StackInstruction.Jump_If_True(8),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(13)),

                    StackInstruction.Jump_If_True(4),

                    StackInstruction.Push_Environment,

                    StackInstruction.Push_Environment,

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
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(11)),

                    StackInstruction.Jump_If_True(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Jump_Unconditional(4),

                    StackInstruction.Push_Environment,

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(41)),

                    StackInstruction.Parse_And_Eval_Binary,

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(13)),

                    StackInstruction.Jump_If_True(2),

                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(19)),

                    StackInstruction.Jump_Unconditional(4),

                    StackInstruction.Push_Environment,

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
                new StackFrameInstructions(
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
                new Expression.KernelApplication
                (
                    function: nameof(KernelFunction.head),
                    input:
                    new Expression.KernelApplication
                    (
                        function: "skip",
                        input: Expression.ListInstance(
                            [
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),

                            Expression.EnvironmentInstance,
                            ])
                    )
                ),

                expected =
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Environment,

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
                new Expression.KernelApplication
                (
                    function: nameof(KernelFunction.head),
                    input:
                    new Expression.KernelApplication
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

                                new Expression.KernelApplication
                                (
                                    function: nameof(KernelFunction.head),
                                    input:
                                    new Expression.KernelApplication
                                    (
                                        function: "skip",
                                        input: Expression.ListInstance(
                                            [
                                            Expression.LiteralInstance(
                                                IntegerEncoding.EncodeSignedInteger(23)),

                                            Expression.EnvironmentInstance,
                                            ])
                                    )
                                )
                                ]),
                            ])
                    )
                ),

                expected =
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Literal(
                        IntegerEncoding.EncodeSignedInteger(21)),

                    StackInstruction.Push_Environment,

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
                            new Expression.KernelApplication
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
                            new Expression.KernelApplication
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
                        new Expression.KernelApplication
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
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Environment,

                    StackInstruction.Push_Environment,

                    StackInstruction.Int_Mul_Binary,

                    StackInstruction.Push_Environment,

                    StackInstruction.Int_Mul_Binary,

                    StackInstruction.Local_Set(0),

                    StackInstruction.Local_Get(0),

                    StackInstruction.Build_List(2),

                    StackInstruction.Jump_If_True(6),

                    StackInstruction.Local_Get(0),

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
                new Expression.KernelApplication(
                    function: "int_add",
                    input:
                    Expression.ListInstance(
                        [
                            new Expression.KernelApplication(
                                function: "head",
                                input:
                                new Expression.KernelApplication(
                                    function: "skip",
                                    input:
                                    Expression.ListInstance(
                                        [
                                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                                            Expression.EnvironmentInstance,
                                        ]))),

                            new Expression.KernelApplication(
                                function: "negate",
                                input:
                                new Expression.KernelApplication(
                                    function: "head",
                                    input:
                                    new Expression.KernelApplication(
                                        function: "skip",
                                        input:
                                        Expression.ListInstance(
                                            [
                                                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(2)),
                                                Expression.EnvironmentInstance,
                                            ])))),
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Environment,

                    StackInstruction.Skip_Head_Const(1),

                    StackInstruction.Push_Environment,

                    StackInstruction.Skip_Head_Const(2),

                    StackInstruction.Int_Sub_Binary,

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.int_add),
                    input:
                    Expression.ListInstance(
                        [
                            new Expression.KernelApplication(
                                function: "head",
                                input:
                                new Expression.KernelApplication(
                                    function: "skip",
                                    input:
                                    Expression.ListInstance(
                                        [
                                            Expression.LiteralInstance(
                                                IntegerEncoding.EncodeSignedInteger(1)),

                                            Expression.EnvironmentInstance,
                                        ]))),

                            new Expression.KernelApplication(
                                function: "int_mul",
                                input:
                                Expression.ListInstance(
                                    [
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(-1)),

                                    new Expression.KernelApplication(
                                        function: "head",
                                        input:
                                        new Expression.KernelApplication(
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
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Environment,

                    StackInstruction.Skip_Head_Const(1),

                    StackInstruction.Push_Environment,

                    StackInstruction.Skip_Head_Const(2),

                    StackInstruction.Int_Sub_Binary,

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: "equal",
                    input:
                    Expression.ListInstance(
                        [
                        new Expression.KernelApplication(
                            function: "head",
                            input: Expression.EnvironmentInstance),

                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Environment,

                    StackInstruction.Head_Generic,

                    StackInstruction.Equal_Binary_Const(
                        IntegerEncoding.EncodeSignedInteger(13)),

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: "equal",
                    input:
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),

                        new Expression.KernelApplication(
                            function: "head",
                            input: Expression.EnvironmentInstance),
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Environment,

                    StackInstruction.Head_Generic,

                    StackInstruction.Equal_Binary_Const(
                        IntegerEncoding.EncodeSignedInteger(17)),

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: "negate",
                    input:
                    new Expression.KernelApplication(
                        function: "equal",
                        input:
                        Expression.ListInstance(
                            [
                            new Expression.KernelApplication(
                                function: "head",
                                input:
                                new Expression.KernelApplication(
                                    function: "skip",
                                    input:
                                    Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11)),
                                        Expression.EnvironmentInstance,
                                        ])
                                )),

                            new Expression.KernelApplication(
                                function: "head",
                                input:
                                new Expression.KernelApplication(
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
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Environment,

                    StackInstruction.Skip_Head_Const(11),

                    StackInstruction.Push_Environment,

                    StackInstruction.Skip_Head_Const(13),

                    StackInstruction.Not_Equal_Binary,

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: "int_is_sorted_asc",
                    input:
                    Expression.ListInstance(
                        [
                        new Expression.KernelApplication(
                            function: "head",
                            input:
                            new Expression.KernelApplication(
                                function: "skip",
                                input:
                                Expression.ListInstance(
                                    [
                                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),
                                    Expression.EnvironmentInstance,
                                    ])
                            )),

                        new Expression.KernelApplication(
                            function: "head",
                            input:
                            new Expression.KernelApplication(
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
                new StackFrameInstructions(
                    [
                        new StackInstruction(StackInstructionKind.Push_Environment),

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 17),

                        StackInstruction.Push_Environment,

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 19),

                        new StackInstruction(
                            StackInstructionKind.Int_Less_Than_Or_Equal_Binary),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: "negate",
                    input:
                    new Expression.KernelApplication(
                        function: "int_is_sorted_asc",
                        input:
                        Expression.ListInstance(
                            [
                            new Expression.KernelApplication(
                                function: "head",
                                input:
                                new Expression.KernelApplication(
                                    function: "skip",
                                    input:
                                    Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11)),
                                        Expression.EnvironmentInstance,
                                        ])
                                )),

                            new Expression.KernelApplication(
                                function: "head",
                                input:
                                new Expression.KernelApplication(
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
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Environment,

                    StackInstruction.Skip_Head_Const(13),

                    StackInstruction.Push_Environment,

                    StackInstruction.Skip_Head_Const(11),

                    StackInstruction.Int_Less_Than_Binary,

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: "int_add",
                    input:
                    Expression.ListInstance(
                        [
                        new Expression.KernelApplication(
                            function: "head",
                            input: Expression.EnvironmentInstance),

                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Environment,

                    StackInstruction.Head_Generic,

                    StackInstruction.Int_Add_Const(13),

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: "int_mul",
                    input:
                    Expression.ListInstance(
                        [
                        new Expression.KernelApplication(
                            function: "head",
                            input: Expression.EnvironmentInstance),

                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                    StackInstruction.Push_Environment,

                    StackInstruction.Head_Generic,

                    StackInstruction.Int_Mul_Const(17),

                    StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication
                (
                    function: nameof(KernelFunction.equal),
                    input: Expression.ListInstance(
                        [
                        new Expression.KernelApplication
                        (
                            function: nameof(KernelFunction.head),
                            input: Expression.EnvironmentInstance
                            ),

                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(71)),
                    ])
                    ),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Head_Generic,

                        StackInstruction.Equal_Binary_Const(
                            IntegerEncoding.EncodeSignedInteger(71)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication
                (
                    function: nameof(KernelFunction.equal),
                    input: Expression.ListInstance(
                        [
                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(73)),

                        new Expression.KernelApplication
                        (
                            function: nameof(KernelFunction.head),
                            input: Expression.EnvironmentInstance
                            ),
                    ])
                    ),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Head_Generic,

                        StackInstruction.Equal_Binary_Const(
                            IntegerEncoding.EncodeSignedInteger(73)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication
                (
                    function: nameof(KernelFunction.equal),
                    input: Expression.ListInstance(
                        [
                        new Expression.KernelApplication
                        (
                            function: nameof(KernelFunction.length),
                            input: Expression.EnvironmentInstance
                            ),

                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(11)),
                    ])
                    ),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Length_Equal_Const(11),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication
                (
                    function: nameof(KernelFunction.equal),
                    input: Expression.ListInstance(
                        [
                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(13)),

                        new Expression.KernelApplication
                        (
                            function: nameof(KernelFunction.length),
                            input: Expression.EnvironmentInstance
                            ),
                    ])
                    ),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Length_Equal_Const(13),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input: Expression.ListInstance(
                        [
                            Expression.EnvironmentInstance,

                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(13)),
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Int_Less_Than_Or_Equal_Const(13),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(17)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Int_Greater_Than_Or_Equal_Const(17),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
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
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Local_Set(0),

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
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(23)),

                            new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.head),
                                input:
                                new Expression.KernelApplication
                                (
                                    function: nameof(KernelFunction.skip),
                                    input: Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(
                                            IntegerEncoding.EncodeSignedInteger(41)),

                                        new Expression.KernelApplication
                                        (
                                            function: nameof(KernelFunction.head),
                                            input:
                                            new Expression.KernelApplication
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
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Skip_Head_Const(43),

                        StackInstruction.Skip_Head_Const(41),

                        StackInstruction.Local_Set(0),

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
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.bit_and),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(23)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Bit_And_Const(
                            IntegerEncoding.EncodeSignedInteger(23)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.bit_and),
                    input: Expression.ListInstance(
                        [
                            Expression.EnvironmentInstance,

                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(27)),
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Bit_And_Const(
                            IntegerEncoding.EncodeSignedInteger(27)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.bit_or),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(31)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Bit_Or_Const(
                            IntegerEncoding.EncodeSignedInteger(31)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.bit_or),
                    input: Expression.ListInstance(
                        [
                            Expression.EnvironmentInstance,

                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(37)),
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Bit_Or_Const(
                            IntegerEncoding.EncodeSignedInteger(37)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.bit_shift_left),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(13)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Bit_Shift_Left_Const(13),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.bit_shift_right),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(17)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Bit_Shift_Right_Const(17),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.take),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(71)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Take_Const(71),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.skip),
                    input: Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(73)),

                            Expression.EnvironmentInstance,
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Skip_Const(73),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.int_add),
                    Expression.ListInstance(
                        [
                            new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.concat),
                                input:
                                Expression.ListInstance(
                                    [
                                        new Expression.KernelApplication
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

                                        new Expression.KernelApplication
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
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Head_Generic,

                        StackInstruction.Int_Unsigned_Add_Const(79),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    Expression.ListInstance(
                        [
                            new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.concat),
                                input:
                                Expression.ListInstance(
                                    [
                                        new Expression.KernelApplication
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

                                        new Expression.KernelApplication
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
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Head_Generic,

                        StackInstruction.Int_Unsigned_Less_Than_Or_Equal_Const(79),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(83)),

                            new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.concat),
                                input:
                                Expression.ListInstance(
                                    [
                                        new Expression.KernelApplication
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

                                        new Expression.KernelApplication
                                        (
                                            function: nameof(KernelFunction.head),
                                            input: Expression.EnvironmentInstance
                                        ),
                                    ])
                            ),
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Head_Generic,

                        StackInstruction.Int_Unsigned_Greater_Than_Or_Equal_Const(83),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    Expression.ListInstance(
                        [
                            Expression.LiteralInstance(
                                IntegerEncoding.EncodeSignedInteger(83)),

                            new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.concat),
                                input:
                                Expression.ListInstance(
                                    [
                                        new Expression.KernelApplication
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

                                        new Expression.KernelApplication
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
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Head_Generic,

                        StackInstruction.Local_Set(0),

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
                    new Expression.KernelApplication
                    (
                        function: nameof(KernelFunction.int_is_sorted_asc),
                        Expression.ListInstance(
                            [
                                Expression.LiteralInstance(
                                    IntegerEncoding.EncodeSignedInteger(83)),

                                new Expression.KernelApplication
                                (
                                    function: nameof(KernelFunction.concat),
                                    input:
                                    Expression.ListInstance(
                                        [
                                            new Expression.KernelApplication
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

                                            new Expression.KernelApplication
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
                    new Expression.KernelApplication(
                    function: nameof(KernelFunction.int_add),
                    Expression.ListInstance(
                        [
                            new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.concat),
                                input:
                                Expression.ListInstance(
                                    [
                                        new Expression.KernelApplication
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

                                        new Expression.KernelApplication
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
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Head_Generic,

                        StackInstruction.Int_Unsigned_Greater_Than_Or_Equal_Const(83),

                        StackInstruction.Jump_If_True(2),

                        StackInstruction.Push_Literal(
                            IntegerEncoding.EncodeSignedInteger(107)),

                        StackInstruction.Return,

                        StackInstruction.Push_Environment,

                        StackInstruction.Head_Generic,

                        StackInstruction.Int_Unsigned_Add_Const(79),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication
                (
                    function: nameof(KernelFunction.reverse),
                    input:
                    new Expression.KernelApplication
                    (
                        function: nameof(KernelFunction.take),
                        input:
                        Expression.ListInstance(
                            [
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(3)),
                            new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.reverse),
                                input:
                                Expression.EnvironmentInstance
                            ),
                            ])
                    )
                ),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Take_Last_Const(3),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication
                (
                    function: nameof(KernelFunction.skip),
                    input:
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),

                        new Expression.KernelApplication
                        (
                            function: nameof(KernelFunction.int_add),
                            input:
                            Expression.ListInstance(
                                [
                                    new Expression.KernelApplication
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
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Blob_Trim_Leading_Zeros(minRemainingCount:1),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input:
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(0)),

                        Expression.EnvironmentInstance,
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Starts_With_Const(
                            PineValue.BlobSingleByte(4)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input:
                    Expression.ListInstance(
                        [
                        Expression.EnvironmentInstance,

                        Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(-1)),
                        ])),

                expected =
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Starts_With_Const(
                            PineValue.BlobSingleByte(2)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.KernelApplication(
                    function: nameof(KernelFunction.equal),
                    input:
                    Expression.ListInstance(
                        [
                        new Expression.KernelApplication(
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
                new StackFrameInstructions(
                    [
                        StackInstruction.Push_Environment,

                        StackInstruction.Starts_With_Const(
                            PineValue.EmptyBlob),

                        StackInstruction.Return,
                    ])
            },
        };

        var parseCache = new PineVMParseCache();

        for (var testCaseIndex = 0; testCaseIndex < testCases.Length; ++testCaseIndex)
        {
            var testCase = testCases[testCaseIndex];

            var compiled =
                PineVM.PineVM.CompileExpression(
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
}
