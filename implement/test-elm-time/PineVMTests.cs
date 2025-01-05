using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.PineVM;

namespace TestElmTime;

[TestClass]
public class PineVMTests
{
    [TestMethod]
    public void Evaluate_expression()
    {
        var testCases = new[]
        {
            new
            {
                expression = (Expression)new Expression.Literal(PineValue.Blob([1, 4, 7])),
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
                                    new Expression.Literal(
                                        PineValueAsInteger.ValueFromSignedInteger(1)),
                                ]),

                            new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.head),
                                input: Expression.EnvironmentInstance
                            ),

                            new Expression.ParseAndEval(
                                encoded:
                                new Expression.Literal(
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
                                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(1)),
                                        Expression.EnvironmentInstance,
                                        ])
                                ))
                        ]),
                    function: "concat"
                ),
                environment =
                PineValue.List(
                    [
                    PineValue.List([PineValueAsInteger.ValueFromSignedInteger(3)]),
                    PineValue.List([PineValueAsInteger.ValueFromSignedInteger(11)]),
                    ]),

                expected = Result<string, PineValue>.ok(PineValue.List(
                    [
                        PineValueAsInteger.ValueFromSignedInteger(1),
                        PineValueAsInteger.ValueFromSignedInteger(3),
                        PineValueAsInteger.ValueFromSignedInteger(11),
                    ]))
            },

            new
            {
                expression =
                (Expression)
                Expression.ConditionalInstance(
                    condition: new Expression.Literal(PineValue.Blob([4])),
                    falseBranch:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17)),
                    trueBranch:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13))),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(PineValueAsInteger.ValueFromSignedInteger(13))
            },

            new
            {
                expression=
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    new Expression.Literal(PineValue.Blob([2])),
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
                                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(1)),
                                        Expression.EnvironmentInstance,
                                        ])
                                )
                            ),
                        Expression.EnvironmentInstance),
                    trueBranch:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17))),

                environment =
                PineValue.List(
                    [
                        PineValueAsInteger.ValueFromSignedInteger(43),
                        ExpressionEncoding.EncodeExpressionAsValue(
                            new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.head),
                                input: Expression.EnvironmentInstance
                            ))
                    ]),

                expected = Result<string, PineValue>.ok(PineValueAsInteger.ValueFromSignedInteger(43))
            },

            new
            {
                expression=
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    new Expression.Literal(PineValue.Blob([4])),
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
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17))),

                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(PineValueAsInteger.ValueFromSignedInteger(17))
            },

            new
            {
                expression=
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    new Expression.Literal(PineValue.Blob([4])),
                    falseBranch:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17)),
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
                                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(1)),
                                        Expression.EnvironmentInstance,
                                        ])
                                )
                            ),
                        Expression.EnvironmentInstance)),
                environment =
                PineValue.List(
                    [
                        PineValueAsInteger.ValueFromSignedInteger(41),
                        ExpressionEncoding.EncodeExpressionAsValue(
                            new Expression.KernelApplication
                            (
                                function: nameof(KernelFunction.head),
                                input: Expression.EnvironmentInstance
                            ))
                    ]),

                expected = Result<string, PineValue>.ok(PineValueAsInteger.ValueFromSignedInteger(41))
            },
        };

        foreach (var testCase in testCases)
        {
            var pineVM = new PineVM();

            var evaluated = pineVM.EvaluateExpression(
                testCase.expression,
                testCase.environment);

            Assert.AreEqual(testCase.expected, evaluated);
        }
    }

    [TestMethod]
    public void Compile_stack_frame_instructions()
    {
        var testCases = new[]
        {
            new
            {
                expression =
                (Expression)new Expression.Literal(PineValue.EmptyBlob),

                expected =
                new PineVM.StackFrameInstructions(
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
                        new Expression.Literal(PineValue.EmptyList),
                        new Expression.Literal(PineValue.EmptyBlob),
                    ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValue.EmptyList),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValue.EmptyBlob),

                        new StackInstruction(
                            StackInstructionKind.BuildList,
                            TakeCount: 2),

                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)Expression.ListInstance(
                    [
                        new Expression.Literal(PineValue.EmptyList),
                        new Expression.KernelApplication
                        (
                            function: "skip",
                            input: Expression.ListInstance(
                                [
                                new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(1)),
                                Expression.EnvironmentInstance,
                                ])
                        ),
                        new Expression.Literal(PineValue.EmptyBlob),
                        new Expression.KernelApplication
                        (
                            function: "skip",
                            input: Expression.ListInstance(
                                [
                                new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(1)),
                                Expression.EnvironmentInstance,
                                ])
                        ),
                    ]),

                /*
                 * Expect CSE, reusing the result of the kernel application.
                 * */
                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValue.EmptyList),

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(1)),

                        new StackInstruction(StackInstructionKind.Skip_Binary),

                        new StackInstruction(StackInstructionKind.Local_Set, LocalIndex: 0),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValue.EmptyBlob),

                        new StackInstruction(StackInstructionKind.Local_Get, LocalIndex: 0),

                        new StackInstruction(StackInstructionKind.BuildList, TakeCount: 4),

                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ListInstance(
                    [
                        new Expression.Literal(PineValue.EmptyList),
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
                                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13)),
                                    Expression.EnvironmentInstance,
                                    ])
                            )
                        ),

                        new Expression.Literal(PineValue.EmptyBlob),

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
                                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13)),
                                    Expression.EnvironmentInstance,
                                    ])
                            )
                        ),
                    ]),

                /*
                 * Expect CSE does not separate subexpressions for which all parents are already in separate instructions.
                 * */
                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValue.EmptyList),

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(13)),

                        new StackInstruction(StackInstructionKind.Skip_Binary),

                        new StackInstruction(StackInstructionKind.Concat_List),

                        new StackInstruction(StackInstructionKind.Local_Set, LocalIndex: 0),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValue.EmptyBlob),

                        new StackInstruction(StackInstructionKind.Local_Get, LocalIndex: 0),

                        new StackInstruction(StackInstructionKind.BuildList, TakeCount: 4),

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
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.PushEnvironment,

                        StackInstruction.PushEnvironment,

                        new StackInstruction(StackInstructionKind.Parse_And_Eval),

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
                        new Expression.Literal(PineValue.EmptyBlob),
                    ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.PushEnvironment,

                        StackInstruction.PushEnvironment,

                        new StackInstruction(StackInstructionKind.Parse_And_Eval),

                        new StackInstruction(StackInstructionKind.Push_Literal, Literal: PineValue.EmptyBlob),

                        new StackInstruction(StackInstructionKind.BuildList, TakeCount: 2),

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
                                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(11)),
                                    environment:
                                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13))),
                                new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17)),
                            ])),
                        new Expression.Literal(PineValue.EmptyBlob),
                    ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(13)),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(11)),

                        new StackInstruction(StackInstructionKind.Parse_And_Eval),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(17)),

                        new StackInstruction(StackInstructionKind.BuildList, TakeCount: 2),

                        StackInstruction.PushEnvironment,

                        new StackInstruction(StackInstructionKind.Parse_And_Eval),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValue.EmptyBlob),

                        new StackInstruction(StackInstructionKind.BuildList, TakeCount: 2),

                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)Expression.ListInstance(
                    [
                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17)),
                        Expression.ConditionalInstance(
                            condition:
                            Expression.EnvironmentInstance,
                            trueBranch:
                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(21)),
                            falseBranch:
                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(23))
                        ),
                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(27)),
                    ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(17)),

                        new StackInstruction(StackInstructionKind.Push_Environment),

                        StackInstruction.Jump_If_True(offset : 2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(23)),

                        StackInstruction.Jump_Unconditional(offset: 2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(21)),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(27)),

                        new StackInstruction(
                            StackInstructionKind.BuildList,
                            TakeCount: 3),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(11)),
                    trueBranch:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13)),
                    falseBranch:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(11)),

                        StackInstruction.Jump_If_True(offset : 2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(17)),

                        StackInstruction.Jump_Unconditional(offset: 2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(13)),

                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(11)),
                    trueBranch:
                    new Expression.ParseAndEval(
                        encoded:
                        new Expression.KernelApplication
                            (
                                function: "skip",
                                input: Expression.ListInstance(
                                    [
                                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13)),
                                    Expression.EnvironmentInstance,
                                    ])
                            ),
                        environment:
                        Expression.EnvironmentInstance),
                    falseBranch:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(11)),

                        StackInstruction.Jump_If_True(offset : 2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(17)),

                        StackInstruction.Jump_Unconditional(6),

                        StackInstruction.PushEnvironment,

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(13)),

                        new StackInstruction(StackInstructionKind.Skip_Binary),

                        new StackInstruction(StackInstructionKind.Parse_And_Eval),

                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ListInstance(
                    [
                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(47)),
                        Expression.ConditionalInstance(
                            condition:
                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(11)),

                            trueBranch:
                            new Expression.ParseAndEval(
                                encoded:
                                new Expression.KernelApplication
                                    (
                                        function: "skip",
                                        input: Expression.ListInstance(
                                            [
                                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13)),
                                            Expression.EnvironmentInstance,
                                            ])
                                    ),

                                environment:
                                Expression.EnvironmentInstance),

                            falseBranch:
                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17)))
                ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(47)),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(11)),

                        StackInstruction.Jump_If_True(offset : 2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(17)),

                        StackInstruction.Jump_Unconditional(6),

                        StackInstruction.PushEnvironment,

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(13)),

                        new StackInstruction(StackInstructionKind.Skip_Binary),

                        new StackInstruction(StackInstructionKind.Parse_And_Eval),

                        new StackInstruction(StackInstructionKind.BuildList, TakeCount: 2),

                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(11)),
                    falseBranch:
                    Expression.ConditionalInstance(
                        condition:
                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13)),
                        falseBranch:
                        new Expression.ParseAndEval(
                            encoded: Expression.EnvironmentInstance,
                            environment:Expression.EnvironmentInstance),
                        trueBranch:
                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(21))),
                    trueBranch:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(23))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(11)),

                        StackInstruction.Jump_If_True(offset : 8),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(13)),

                        StackInstruction.Jump_If_True(offset : 4),

                        StackInstruction.PushEnvironment,

                        StackInstruction.PushEnvironment,

                        new StackInstruction(StackInstructionKind.Parse_And_Eval),

                        StackInstruction.Jump_Unconditional(2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(21)),

                        StackInstruction.Jump_Unconditional(2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(23)),

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
                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(11)),
                            trueBranch:
                            new Expression.ParseAndEval(
                                encoded:
                                new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(41)),
                                environment:
                                Expression.EnvironmentInstance),
                            falseBranch:
                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17))),

                        Expression.ConditionalInstance(
                            condition:
                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13)),
                            trueBranch:
                            new Expression.ParseAndEval(
                                encoded:
                                new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(43)),
                                environment:
                                Expression.EnvironmentInstance),
                            falseBranch:
                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(19))),
                ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(11)),

                        StackInstruction.Jump_If_True(offset : 2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(17)),

                        StackInstruction.Jump_Unconditional(4),

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(41)),

                        new StackInstruction(StackInstructionKind.Parse_And_Eval),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(13)),

                        StackInstruction.Jump_If_True(offset: 2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(19)),

                        StackInstruction.Jump_Unconditional(4),

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(43)),

                        new StackInstruction(StackInstructionKind.Parse_And_Eval),

                        new StackInstruction(StackInstructionKind.BuildList, TakeCount: 2),

                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                Expression.ConditionalInstance(
                    condition:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(11)),
                    trueBranch:
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13)),
                    falseBranch:
                    Expression.ConditionalInstance(
                        condition:
                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(21)),
                        trueBranch:
                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(23)),
                        falseBranch:
                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(27)))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(11)),

                        StackInstruction.Jump_If_True(offset : 6),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(21)),

                        StackInstruction.Jump_If_True(offset : 2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(27)),

                        StackInstruction.Jump_Unconditional(2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(23)),

                        StackInstruction.Jump_Unconditional(2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(13)),

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
                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17)),
                            Expression.EnvironmentInstance,
                            ])
                    )
                ),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 17),

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
                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(17)),
                            Expression.ListInstance(
                                [
                                new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(21)),
                                new Expression.KernelApplication
                                (
                                    function: nameof(KernelFunction.head),
                                    input:
                                    new Expression.KernelApplication
                                    (
                                        function: "skip",
                                        input: Expression.ListInstance(
                                            [
                                            new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(23)),
                                            Expression.EnvironmentInstance,
                                            ])
                                    )
                                )
                                ]),
                            ])
                    )
                ),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(21)),

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 23),

                        new StackInstruction(
                            StackInstructionKind.BuildList,
                            TakeCount: 2),

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 17),

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
                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(13)),
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
                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(23)),
                        falseBranch:
                        new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(27)))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.PushEnvironment,

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Int_Mul_Binary),

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Int_Mul_Binary),

                        new StackInstruction(
                            StackInstructionKind.Local_Set,
                            LocalIndex: 0),

                        new StackInstruction(StackInstructionKind.Local_Get, LocalIndex: 0),

                        new StackInstruction(
                            StackInstructionKind.BuildList,
                            TakeCount: 2),

                        StackInstruction.Jump_If_True(offset: 6),

                        new StackInstruction(StackInstructionKind.Local_Get, LocalIndex: 0),

                        StackInstruction.Jump_If_True(offset: 2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(27)),

                        StackInstruction.Jump_Unconditional(2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(23)),

                        StackInstruction.Jump_Unconditional(offset: 2),

                        new StackInstruction(
                            StackInstructionKind.Push_Literal,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(13)),

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
                                            Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(1)),
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
                                                Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(2)),
                                                Expression.EnvironmentInstance,
                                            ])))),
                        ])),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 1),

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 2),

                        new StackInstruction(
                            StackInstructionKind.Int_Sub_Binary),

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
                                input:
                                new Expression.KernelApplication(
                                    function: "skip",
                                    input:
                                    Expression.ListInstance(
                                        [
                                            Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(1)),
                                            Expression.EnvironmentInstance,
                                        ]))),

                            new Expression.KernelApplication(
                                function: "int_mul",
                                input:
                                Expression.ListInstance(
                                    [
                                    Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(-1)),

                                    new Expression.KernelApplication(
                                        function: "head",
                                        input:
                                        new Expression.KernelApplication(
                                            function: "skip",
                                            input:
                                            Expression.ListInstance(
                                                [
                                                    Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(2)),
                                                    Expression.EnvironmentInstance,
                                                ])))
                                    ])),
                        ])),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 1),

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 2),

                        new StackInstruction(
                            StackInstructionKind.Int_Sub_Binary),

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

                        Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(13)),
                        ])),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(StackInstructionKind.Push_Environment),

                        new StackInstruction(StackInstructionKind.Head_Generic),

                        new StackInstruction(
                            StackInstructionKind.Equal_Binary_Const,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(13)),

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
                        Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(17)),

                        new Expression.KernelApplication(
                            function: "head",
                            input: Expression.EnvironmentInstance),
                        ])),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(StackInstructionKind.Push_Environment),

                        new StackInstruction(StackInstructionKind.Head_Generic),

                        new StackInstruction(
                            StackInstructionKind.Equal_Binary_Const,
                            Literal: PineValueAsInteger.ValueFromSignedInteger(17)),

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
                                        Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(11)),
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
                                        Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(13)),
                                        Expression.EnvironmentInstance,
                                        ])
                                )),
                            ]))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(StackInstructionKind.Push_Environment),

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 11),

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 13),

                        new StackInstruction(
                            StackInstructionKind.Not_Equal_Binary_Var),

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
                                    Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(17)),
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
                                    Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(19)),
                                    Expression.EnvironmentInstance,
                                    ])
                            )),
                        ])),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(StackInstructionKind.Push_Environment),

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 17),

                        StackInstruction.PushEnvironment,

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
                                        Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(11)),
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
                                        Expression.LiteralInstance(PineValueAsInteger.ValueFromSignedInteger(13)),
                                        Expression.EnvironmentInstance,
                                        ])
                                )),
                            ]))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction(StackInstructionKind.Push_Environment),

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 13),

                        StackInstruction.PushEnvironment,

                        new StackInstruction(
                            StackInstructionKind.Skip_Head_Const,
                            SkipCount: 11),

                        new StackInstruction(
                            StackInstructionKind.Int_Less_Than_Binary),

                        StackInstruction.Return,
                    ])
            },
        };

        var parseCache = new PineVMParseCache();

        for (var testCaseIndex = 0; testCaseIndex < testCases.Length; ++testCaseIndex)
        {
            var testCase = testCases[testCaseIndex];

            var compiled =
                PineVM.CompileExpression(
                    testCase.expression,
                    specializations: [],
                    parseCache,
                    disableReduction: true,
                    skipInlining: (_, _) => false);

            try
            {
                Assert.AreEqual(
                    testCase.expected.Instructions.Count,
                    compiled.Generic.Instructions.Count,
                    "Instructions count");

                for (var instructionIndex = 0; instructionIndex < testCase.expected.Instructions.Count; instructionIndex++)
                {
                    Assert.AreEqual(
                        testCase.expected.Instructions[instructionIndex],
                        compiled.Generic.Instructions[instructionIndex],
                        $"Instruction at index {instructionIndex} of " + compiled.Generic.Instructions.Count);
                }

                Assert.AreEqual(testCase.expected, compiled.Generic);
            }
            catch (System.Exception e)
            {
                throw new System.Exception(
                    $"Failed for test case {testCaseIndex} with expression {testCase.expression}", e);
            }
        }
    }
}