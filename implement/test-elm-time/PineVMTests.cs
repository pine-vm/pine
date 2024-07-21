using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.PineVM;
using System;

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
                expression = (Expression)new Expression.LiteralExpression(PineValue.Blob([1, 4, 7])),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(
                    PineValue.Blob([1, 4, 7]))
            },
            new
            {
                expression = (Expression)new Expression.ListExpression([]),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(PineValue.EmptyList)
            },
            new
            {
                expression = (Expression)ExpressionEncoding.ParseKernelApplicationExpression
                (
                    functionName: "concat",
                    argument: new Expression.ListExpression([])
                ).Extract(fromErr: err => throw new Exception(err)),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(PineValue.EmptyList)
            },
            new
            {
                expression = (Expression)ExpressionEncoding.ParseKernelApplicationExpression
                (
                    functionName: "concat",
                    argument:
                    new Expression.ListExpression(
                        [
                            new Expression.ListExpression(
                                [
                                    new Expression.LiteralExpression(
                                        PineValueAsInteger.ValueFromSignedInteger(1)),
                                ]),

                            ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "list_head",
                                argument: Expression.Environment
                            ).Extract(fromErr: err => throw new Exception(err)),

                            new Expression.ParseAndEvalExpression(
                                expression:
                                new Expression.LiteralExpression(
                                    ExpressionEncoding.EncodeExpressionAsValue(
                                        ExpressionEncoding.ParseKernelApplicationExpression
                                        (
                                            functionName: "list_head",
                                            argument: Expression.Environment
                                        ).Extract(fromErr: err => throw new Exception(err)))
                                    .Extract(fromErr: err => throw new Exception(err))),
                                environment:
                                ExpressionEncoding.ParseKernelApplicationExpression
                                (
                                    functionName: "skip",
                                    argument: new Expression.ListExpression(
                                        [
                                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(1)),
                                        Expression.Environment,
                                        ])
                                ).Extract(fromErr: err => throw new Exception(err)))
                        ])
                ).Extract(fromErr: err => throw new Exception(err)),
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
                new Expression.ConditionalExpression(
                    condition: new Expression.LiteralExpression(PineValue.Blob([4])),
                    trueBranch:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                    falseBranch:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17))),
                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(PineValueAsInteger.ValueFromSignedInteger(13))
            },

            new
            {
                expression=
                (Expression)
                new Expression.ConditionalExpression(
                    condition:
                    new Expression.LiteralExpression(PineValue.Blob([2])),
                    trueBranch:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17)),
                    falseBranch:
                    new Expression.ParseAndEvalExpression(
                        expression:
                        ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "list_head",
                                argument:
                                ExpressionEncoding.ParseKernelApplicationExpression
                                (
                                    functionName: "skip",
                                    argument: new Expression.ListExpression(
                                        [
                                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(1)),
                                        Expression.Environment,
                                        ])
                                ).Extract(fromErr: err => throw new Exception(err))
                            ).Extract(fromErr: err => throw new Exception(err)),
                        Expression.Environment)),

                environment =
                PineValue.List(
                    [
                        PineValueAsInteger.ValueFromSignedInteger(43),
                        ExpressionEncoding.EncodeExpressionAsValue(
                            ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "list_head",
                                argument: Expression.Environment
                            ).Extract(fromErr: err => throw new Exception(err)))
                        .Extract(fromErr: err => throw new Exception(err))
                    ]),

                expected = Result<string, PineValue>.ok(PineValueAsInteger.ValueFromSignedInteger(43))
            },

            new
            {
                expression=
                (Expression)
                new Expression.ConditionalExpression(
                    condition:
                    new Expression.LiteralExpression(PineValue.Blob([4])),
                    trueBranch:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17)),
                    falseBranch:
                    new Expression.ParseAndEvalExpression(
                        expression:
                        ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "list_head",
                                argument: Expression.Environment
                            ).Extract(fromErr: err => throw new Exception(err)),
                        Expression.Environment)),

                environment = PineValue.EmptyList,

                expected = Result<string, PineValue>.ok(PineValueAsInteger.ValueFromSignedInteger(17))
            },

            new
            {
                expression=
                (Expression)
                new Expression.ConditionalExpression(
                    condition:
                    new Expression.LiteralExpression(PineValue.Blob([4])),
                    trueBranch:
                    new Expression.ParseAndEvalExpression(
                        expression:
                        ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "list_head",
                                argument:
                                ExpressionEncoding.ParseKernelApplicationExpression
                                (
                                    functionName: "skip",
                                    argument: new Expression.ListExpression(
                                        [
                                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(1)),
                                        Expression.Environment,
                                        ])
                                ).Extract(fromErr: err => throw new Exception(err))
                            ).Extract(fromErr: err => throw new Exception(err)),
                        Expression.Environment),
                    falseBranch:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17))),
                environment =
                PineValue.List(
                    [
                        PineValueAsInteger.ValueFromSignedInteger(41),
                        ExpressionEncoding.EncodeExpressionAsValue(
                            ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "list_head",
                                argument: Expression.Environment
                            ).Extract(fromErr: err => throw new Exception(err)))
                        .Extract(fromErr: err => throw new Exception(err))
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
                (Expression)new Expression.LiteralExpression(PineValue.EmptyBlob),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(new Expression.LiteralExpression(PineValue.EmptyBlob)),
                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)new Expression.ListExpression(
                    [
                        new Expression.LiteralExpression(PineValue.EmptyList),
                        new Expression.LiteralExpression(PineValue.EmptyBlob),
                    ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            new Expression.ListExpression(
                            [
                                new Expression.LiteralExpression(PineValue.EmptyList),
                                new Expression.LiteralExpression(PineValue.EmptyBlob),
                            ])),
                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)new Expression.ListExpression(
                    [
                        new Expression.LiteralExpression(PineValue.EmptyList),
                        ExpressionEncoding.ParseKernelApplicationExpression
                        (
                            functionName: "skip",
                            argument: new Expression.ListExpression(
                                [
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(1)),
                                Expression.Environment,
                                ])
                        ).Extract(fromErr: err => throw new Exception(err)),
                        new Expression.LiteralExpression(PineValue.EmptyBlob),
                        ExpressionEncoding.ParseKernelApplicationExpression
                        (
                            functionName: "skip",
                            argument: new Expression.ListExpression(
                                [
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(1)),
                                Expression.Environment,
                                ])
                        ).Extract(fromErr: err => throw new Exception(err)),
                    ]),

                /*
                 * Expect CSE, reusing the result of the kernel application.
                 * */
                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "skip",
                                argument: new Expression.ListExpression(
                                    [
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(1)),
                                    Expression.Environment,
                                    ])
                            ).Extract(fromErr: err => throw new Exception(err))),
                        StackInstruction.Eval(
                            new Expression.ListExpression(
                            [
                                new Expression.LiteralExpression(PineValue.EmptyList),
                                new Expression.StackReferenceExpression(offset: -1),
                                new Expression.LiteralExpression(PineValue.EmptyBlob),
                                new Expression.StackReferenceExpression(offset: -1),
                            ])),
                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.ListExpression(
                    [
                        new Expression.LiteralExpression(PineValue.EmptyList),
                        ExpressionEncoding.ParseKernelApplicationExpression
                        (
                            functionName: "concat",
                            argument:
                            ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "skip",
                                argument:
                                new Expression.ListExpression(
                                    [
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                                    Expression.Environment,
                                    ])
                            ).Extract(fromErr: err => throw new Exception(err))
                        ).Extract(fromErr: err => throw new Exception(err)),

                        new Expression.LiteralExpression(PineValue.EmptyBlob),

                        ExpressionEncoding.ParseKernelApplicationExpression
                        (
                            functionName: "concat",
                            argument:
                            ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "skip",
                                argument:
                                new Expression.ListExpression(
                                    [
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                                    Expression.Environment,
                                    ])
                            ).Extract(fromErr: err => throw new Exception(err))
                        ).Extract(fromErr: err => throw new Exception(err)),
                    ]),

                /*
                 * Expect CSE does not separate subexpressions for which all parents are already in separate instructions.
                 * */
                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "concat",
                                argument:
                                ExpressionEncoding.ParseKernelApplicationExpression
                                (
                                    functionName: "skip",
                                    argument:
                                    new Expression.ListExpression(
                                        [
                                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                                        Expression.Environment,
                                        ])
                                ).Extract(fromErr: err => throw new Exception(err))
                            ).Extract(fromErr: err => throw new Exception(err))),
                        StackInstruction.Eval(
                            new Expression.ListExpression(
                            [
                                new Expression.LiteralExpression(PineValue.EmptyList),
                                new Expression.StackReferenceExpression(offset: -1),
                                new Expression.LiteralExpression(PineValue.EmptyBlob),
                                new Expression.StackReferenceExpression(offset: -1),
                            ])),
                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.ParseAndEvalExpression(
                    expression: Expression.Environment,
                    environment: Expression.Environment),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            new Expression.ParseAndEvalExpression(
                                expression: Expression.Environment,
                                environment: Expression.Environment)),
                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.ListExpression(
                    [
                        new Expression.ParseAndEvalExpression(
                            expression: Expression.Environment,
                            environment: Expression.Environment),
                        new Expression.LiteralExpression(PineValue.EmptyBlob),
                    ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            new Expression.ParseAndEvalExpression(
                                expression: Expression.Environment,
                                environment: Expression.Environment)),
                        StackInstruction.Eval(
                            new Expression.ListExpression(
                            [
                                new Expression.StackReferenceExpression(offset: -1),
                                new Expression.LiteralExpression(PineValue.EmptyBlob),
                            ])),
                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.ListExpression(
                    [
                        new Expression.ParseAndEvalExpression(
                            expression: Expression.Environment,
                            environment: new Expression.ListExpression(
                            [
                                new Expression.ParseAndEvalExpression(
                                    expression:
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                                    environment:
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13))),
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17)),
                            ])),
                        new Expression.LiteralExpression(PineValue.EmptyBlob),
                    ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            new Expression.ParseAndEvalExpression(
                                expression:
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                                environment:
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)))),
                        StackInstruction.Eval(
                            new Expression.ParseAndEvalExpression(
                                expression: Expression.Environment,
                                environment:
                                new Expression.ListExpression(
                                [
                                    new Expression.StackReferenceExpression(offset: -1),
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17)),
                                ]))),
                        StackInstruction.Eval(
                            new Expression.ListExpression(
                            [
                                new Expression.StackReferenceExpression(offset: -1),
                                new Expression.LiteralExpression(PineValue.EmptyBlob),
                            ])),
                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.ConditionalExpression(
                    condition:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                    trueBranch:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                    falseBranch:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11))),

                        new StackInstruction.ConditionalJumpInstruction(
                            InvalidBranchOffset: 3,
                            TrueBranchOffset: 2),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17))),
                        StackInstruction.Jump(offset: 2),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13))),
                        new StackInstruction.CopyLastAssignedInstruction(),

                        StackInstruction.Eval(new Expression.StackReferenceExpression(offset: -1)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.ConditionalExpression(
                    condition:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                    trueBranch:
                    new Expression.ParseAndEvalExpression(
                        expression:
                        ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "skip",
                                argument: new Expression.ListExpression(
                                    [
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                                    Expression.Environment,
                                    ])
                            ).Extract(fromErr: err => throw new Exception(err)),
                        Expression.Environment),
                    falseBranch:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11))),

                        new StackInstruction.ConditionalJumpInstruction(
                            InvalidBranchOffset: 3,
                            TrueBranchOffset:2),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17))),
                        StackInstruction.Jump(offset: 2),

                        StackInstruction.Eval(
                            new Expression.ParseAndEvalExpression(
                            expression:
                            ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "skip",
                                argument: new Expression.ListExpression(
                                    [
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                                    Expression.Environment,
                                    ])
                            ).Extract(fromErr: err => throw new Exception(err)),
                            Expression.Environment)),

                        new StackInstruction.CopyLastAssignedInstruction(),

                        StackInstruction.Eval(new Expression.StackReferenceExpression(offset: -1)),
                        StackInstruction.Return,
                    ])
            },


            new
            {
                expression =
                (Expression)
                new Expression.ListExpression(
                    [
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(47)),
                        new Expression.ConditionalExpression(
                            condition:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                            trueBranch:
                            new Expression.ParseAndEvalExpression(
                                expression:
                                ExpressionEncoding.ParseKernelApplicationExpression
                                    (
                                        functionName: "skip",
                                        argument: new Expression.ListExpression(
                                            [
                                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                                            Expression.Environment,
                                            ])
                                    ).Extract(fromErr: err => throw new Exception(err)),
                                Expression.Environment),
                            falseBranch:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17)))
                ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11))),

                        new StackInstruction.ConditionalJumpInstruction(
                            InvalidBranchOffset: 3,
                            TrueBranchOffset: 2),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17))),
                        StackInstruction.Jump(offset: 2),

                        StackInstruction.Eval(
                            new Expression.ParseAndEvalExpression(
                            expression:
                            ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "skip",
                                argument: new Expression.ListExpression(
                                    [
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                                    Expression.Environment,
                                    ])
                            ).Extract(fromErr: err => throw new Exception(err)),
                            Expression.Environment)),

                        new StackInstruction.CopyLastAssignedInstruction(),
                        StackInstruction.Eval(new Expression.ListExpression(
                            [
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(47)),
                                new Expression.StackReferenceExpression(offset: -1),
                            ])),
                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.ConditionalExpression(
                    condition:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                    falseBranch:
                    new Expression.ConditionalExpression(
                        condition:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                        falseBranch:
                        new Expression.ParseAndEvalExpression(
                            expression: Expression.Environment,
                            environment:Expression.Environment),
                        trueBranch:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(21))),
                    trueBranch:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(23))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11))),

                        new StackInstruction.ConditionalJumpInstruction(
                            InvalidBranchOffset: 9,
                            TrueBranchOffset: 8),

                        // Outer if-false:
                        StackInstruction.Eval(new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13))),

                        new StackInstruction.ConditionalJumpInstruction(
                            InvalidBranchOffset: 3,
                            TrueBranchOffset: 2),

                        // Inner if-false:
                        StackInstruction.Eval(
                            new Expression.ParseAndEvalExpression(
                            expression: Expression.Environment,
                            environment:Expression.Environment)),
                        StackInstruction.Jump(offset: 2),

                        // Inner if-true
                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(21))),

                        // Copy from inner result to outer conditional
                        new StackInstruction.CopyLastAssignedInstruction(),

                        StackInstruction.Eval(new Expression.StackReferenceExpression(offset: -1)),

                        // End outer if-false
                        StackInstruction.Jump(offset: 2),

                        // Outer if-true
                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(23))),

                        new StackInstruction.CopyLastAssignedInstruction(),

                        StackInstruction.Eval(new Expression.StackReferenceExpression(offset: -1)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.ListExpression(
                    [
                        new Expression.ConditionalExpression(
                            condition:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                            trueBranch:
                            new Expression.ParseAndEvalExpression(
                                expression:
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(41)),
                                environment:
                                Expression.Environment),
                            falseBranch:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17))),

                        new Expression.ConditionalExpression(
                            condition:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                            trueBranch:
                            new Expression.ParseAndEvalExpression(
                                expression:
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(43)),
                                environment:
                                Expression.Environment),
                            falseBranch:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(19))),
                ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11))),

                        new StackInstruction.ConditionalJumpInstruction(
                            InvalidBranchOffset: 3,
                            TrueBranchOffset: 2),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17))),
                        StackInstruction.Jump(offset: 2),

                        StackInstruction.Eval(
                            new Expression.ParseAndEvalExpression(
                            expression:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(41)),
                            Expression.Environment)),

                        new StackInstruction.CopyLastAssignedInstruction(),

                        StackInstruction.Eval(new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13))),

                        new StackInstruction.ConditionalJumpInstruction(
                            InvalidBranchOffset: 3,
                            TrueBranchOffset: 2),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(19))),
                        StackInstruction.Jump(offset: 2),

                        StackInstruction.Eval(
                            new Expression.ParseAndEvalExpression(
                            expression:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(43)),
                            Expression.Environment)),

                        new StackInstruction.CopyLastAssignedInstruction(),

                        StackInstruction.Eval(new Expression.ListExpression(
                            [
                                new Expression.StackReferenceExpression(offset: -7),
                                new Expression.StackReferenceExpression(offset: -1),
                            ])),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.ConditionalExpression(
                    condition:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                    trueBranch:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                    falseBranch:
                    new Expression.ConditionalExpression(
                        condition:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(21)),
                        trueBranch:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(23)),
                        falseBranch:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(27)))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11))),

                        new StackInstruction.ConditionalJumpInstruction(
                            InvalidBranchOffset: 9,
                            TrueBranchOffset: 8),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(21))),

                        new StackInstruction.ConditionalJumpInstruction(
                            InvalidBranchOffset: 3,
                            TrueBranchOffset: 2),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(27))),
                        StackInstruction.Jump(offset: 2),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(23))),

                        new StackInstruction.CopyLastAssignedInstruction(),
                        StackInstruction.Eval(new Expression.StackReferenceExpression(offset: -1)),

                        StackInstruction.Jump(offset: 2),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13))),
                        new StackInstruction.CopyLastAssignedInstruction(),

                        StackInstruction.Eval(new Expression.StackReferenceExpression(offset: -1)),

                        StackInstruction.Return,
                    ])
            },

            new
            {
                /*
                 * Fusion of kernel functions skip and list_head.
                 * */
                expression =
                (Expression)
                ExpressionEncoding.ParseKernelApplicationExpression
                (
                    functionName: "list_head",
                    argument:
                    ExpressionEncoding.ParseKernelApplicationExpression
                    (
                        functionName: "skip",
                        argument: new Expression.ListExpression(
                            [
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17)),
                            Expression.Environment,
                            ])
                    ).Extract(fromErr: err => throw new Exception(err))
                ).Extract(fromErr: err => throw new Exception(err)),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            new Expression.KernelApplications_Skip_ListHead_Path_Expression(
                                SkipCounts: (int[]) [17],
                                Argument:Expression.Environment)),
                        StackInstruction.Return
                    ])
            },


            new
            {
                /*
                 * Fusion of nested kernel functions skip and list_head.
                 * */
                expression =
                (Expression)
                ExpressionEncoding.ParseKernelApplicationExpression
                (
                    functionName: "list_head",
                    argument:
                    ExpressionEncoding.ParseKernelApplicationExpression
                    (
                        functionName: "skip",
                        argument: new Expression.ListExpression(
                            [
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17)),
                            new Expression.ListExpression(
                                [
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(21)),
                                ExpressionEncoding.ParseKernelApplicationExpression
                                (
                                    functionName: "list_head",
                                    argument:
                                    ExpressionEncoding.ParseKernelApplicationExpression
                                    (
                                        functionName: "skip",
                                        argument: new Expression.ListExpression(
                                            [
                                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(23)),
                                            Expression.Environment,
                                            ])
                                    ).Extract(fromErr: err => throw new Exception(err))
                                ).Extract(fromErr: err => throw new Exception(err))
                                ]),
                            ])
                    ).Extract(fromErr: err => throw new Exception(err))
                ).Extract(fromErr: err => throw new Exception(err)),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            new Expression.KernelApplications_Skip_ListHead_Path_Expression(
                                SkipCounts: (int[])[17],
                                Argument:
                                new Expression.ListExpression(
                                [
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(21)),
                                    new Expression.KernelApplications_Skip_ListHead_Path_Expression(
                                        SkipCounts: (int[]) [23],
                                        Argument: Expression.Environment)
                                ]))),
                        StackInstruction.Return
                    ])
            },

            new
            {
                expression =
                (Expression)
                new Expression.ConditionalExpression(
                    condition:
                    new Expression.ListExpression(
                        [
                            ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "mul_int",
                                argument:
                                new Expression.ListExpression(
                                    [
                                        Expression.Environment,
                                        Expression.Environment,
                                        Expression.Environment,
                                    ])
                            ).Extract(fromErr: err => throw new Exception(err)),
                            ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "mul_int",
                                argument:
                                new Expression.ListExpression(
                                    [
                                        Expression.Environment,
                                        Expression.Environment,
                                        Expression.Environment,
                                    ])
                            ).Extract(fromErr: err => throw new Exception(err)),
                        ]),
                    trueBranch:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                    falseBranch:
                    new Expression.ConditionalExpression(
                        condition:
                        ExpressionEncoding.ParseKernelApplicationExpression
                        (
                            functionName: "mul_int",
                            argument:
                            new Expression.ListExpression(
                                [
                                    Expression.Environment,
                                    Expression.Environment,
                                    Expression.Environment,
                                ])
                        ).Extract(fromErr: err => throw new Exception(err)),
                        trueBranch:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(23)),
                        falseBranch:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(27)))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            ExpressionEncoding.ParseKernelApplicationExpression
                            (
                                functionName: "mul_int",
                                argument:
                                new Expression.ListExpression(
                                    [
                                        Expression.Environment,
                                        Expression.Environment,
                                        Expression.Environment,
                                    ])
                            ).Extract(fromErr: err => throw new Exception(err))),

                        StackInstruction.Eval(
                            new Expression.ListExpression(
                                [
                                    new Expression.StackReferenceExpression(offset: -1),
                                    new Expression.StackReferenceExpression(offset: -1),
                                ])),

                        new StackInstruction.ConditionalJumpInstruction(
                            InvalidBranchOffset: 9,
                            TrueBranchOffset: 8),

                        StackInstruction.Eval(
                            new Expression.StackReferenceExpression(offset: -3)),

                        new StackInstruction.ConditionalJumpInstruction(
                            InvalidBranchOffset: 3,
                            TrueBranchOffset: 2),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(27))),
                        StackInstruction.Jump(offset: 2),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(23))),

                        new StackInstruction.CopyLastAssignedInstruction(),
                        StackInstruction.Eval(new Expression.StackReferenceExpression(offset: -1)),

                        StackInstruction.Jump(offset: 2),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13))),
                        new StackInstruction.CopyLastAssignedInstruction(),

                        StackInstruction.Eval(new Expression.StackReferenceExpression(offset: -1)),

                        StackInstruction.Return,
                    ])
            },
        };

        var parseCache = new PineVMCache();

        foreach (var testCase in testCases)
        {
            var compiled = PineVM.CompileExpression(
                testCase.expression,
                specializations: [],
                parseCache,
                disableReduction: true);

            Assert.AreEqual(
                testCase.expected.Instructions.Count,
                compiled.Generic.Instructions.Count,
                "Instructions count");

            for (var i = 0; i < testCase.expected.Instructions.Count; i++)
            {
                Assert.AreEqual(
                    testCase.expected.Instructions[i],
                    compiled.Generic.Instructions[i],
                    $"Instruction at index {i} of " + compiled.Generic.Instructions.Count);
            }

            Assert.AreEqual(testCase.expected, compiled.Generic);
        }
    }
}