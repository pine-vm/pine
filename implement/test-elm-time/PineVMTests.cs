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
                    ifTrue:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                    ifFalse:
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
                    ifTrue:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17)),
                    ifFalse:
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
                    ifTrue:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17)),
                    ifFalse:
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
                    ifTrue:
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
                    ifFalse:
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
                    ifTrue:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                    ifFalse:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            new Expression.ConditionalExpression(
                                condition:
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                                ifTrue:
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                                ifFalse:
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17)))),
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
                    ifTrue:
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
                    ifFalse:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11))),

                        new StackInstruction.ConditionalJumpInstruction(
                            IfFalseOffset:2,
                            IfTrueOffset:4),
                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValue.EmptyList)),
                        StackInstruction.Jump(offset: 4),
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
                            ifTrue:
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
                            ifFalse:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17)))
                ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11))),

                        new StackInstruction.ConditionalJumpInstruction(
                            IfFalseOffset:2,
                            IfTrueOffset:4),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValue.EmptyList)),
                        StackInstruction.Jump(offset: 4),

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
                    ifFalse:
                    new Expression.ConditionalExpression(
                        condition:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                        ifFalse:
                        new Expression.ParseAndEvalExpression(
                            expression: Expression.Environment,
                            environment:Expression.Environment),
                        ifTrue:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(21))),
                    ifTrue:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(23))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11))),

                        new StackInstruction.ConditionalJumpInstruction(
                            IfFalseOffset: 2,
                            IfTrueOffset: 12),

                        // Outer if-invalid
                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValue.EmptyList)),
                        StackInstruction.Jump(offset: 12),

                        // Outer if-false:
                        StackInstruction.Eval(new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13))),

                        new StackInstruction.ConditionalJumpInstruction(
                            IfFalseOffset: 2,
                            IfTrueOffset: 4),

                        // Inner if-invalid
                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValue.EmptyList)),
                        StackInstruction.Jump(offset: 4),

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
                            ifTrue:
                            new Expression.ParseAndEvalExpression(
                                expression:
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(41)),
                                environment:
                                Expression.Environment),
                            ifFalse:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(17))),

                        new Expression.ConditionalExpression(
                            condition:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                            ifTrue:
                            new Expression.ParseAndEvalExpression(
                                expression:
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(43)),
                                environment:
                                Expression.Environment),
                            ifFalse:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(19))),
                ]),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11))),

                        new StackInstruction.ConditionalJumpInstruction(
                            IfFalseOffset:2,
                            IfTrueOffset:4),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValue.EmptyList)),
                        StackInstruction.Jump(offset: 4),

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
                            IfFalseOffset:2,
                            IfTrueOffset:4),

                        StackInstruction.Eval(
                            new Expression.LiteralExpression(PineValue.EmptyList)),
                        StackInstruction.Jump(offset: 4),

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
                                new Expression.StackReferenceExpression(offset: -9),
                                new Expression.StackReferenceExpression(offset: -1),
                            ])),

                        StackInstruction.Return,
                    ])
            },

            /*
             * 
             * 2024-06-07
             * Switch to approach to add new stack frame for each condition branch by default.
             * 
            new
            {
                expression =
                (Expression)
                new Expression.ConditionalExpression(
                    condition:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                    ifTrue:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                    ifFalse:
                    new Expression.ConditionalExpression(
                        condition:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(21)),
                        ifTrue:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(23)),
                        ifFalse:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(27)))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        new StackInstruction.ConditionalJumpInstruction(
                            Condition:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                            IfTrueOffset:4),
                        new StackInstruction.ConditionalJumpInstruction(
                            Condition:
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(21)),
                            IfTrueOffset:1),
                        StackInstruction.Return(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(27))),
                        StackInstruction.Return(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(23))),
                        StackInstruction.Return(
                            new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13))),
                    ],
                    ParseAndEvalExpression:null)
            },
            */

            new
            {
                expression =
                (Expression)
                new Expression.ConditionalExpression(
                    condition:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                    ifTrue:
                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                    ifFalse:
                    new Expression.ConditionalExpression(
                        condition:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(21)),
                        ifTrue:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(23)),
                        ifFalse:
                        new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(27)))),

                expected =
                new PineVM.StackFrameInstructions(
                    [
                        StackInstruction.Eval(
                            new Expression.ConditionalExpression(
                                condition:
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(11)),
                                ifTrue:
                                new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(13)),
                                ifFalse:
                                new Expression.ConditionalExpression(
                                    condition:
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(21)),
                                    ifTrue:
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(23)),
                                    ifFalse:
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(27))))),
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
                            new Expression.KernelApplications_Skip_ListHead_Expression(
                                skipCount: 17,
                                argument:Expression.Environment)),
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
                            new Expression.KernelApplications_Skip_ListHead_Expression(
                                skipCount: 17,
                                argument:
                                new Expression.ListExpression(
                                [
                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(21)),
                                    new Expression.KernelApplications_Skip_ListHead_Expression(
                                        skipCount: 23,
                                        argument:Expression.Environment)
                                ]))),
                        StackInstruction.Return
                    ])
            },
        };

        foreach (var testCase in testCases)
        {
            var compiled = PineVM.InstructionsFromExpressionLessCache(testCase.expression);

            Assert.AreEqual(
                testCase.expected.Instructions.Count,
                compiled.Instructions.Count,
                "Instructions count");

            for (var i = 0; i < testCase.expected.Instructions.Count; i++)
            {
                Assert.AreEqual(
                    testCase.expected.Instructions[i],
                    compiled.Instructions[i],
                    $"Instruction at index {i}");
            }

            Assert.AreEqual(testCase.expected, compiled);
        }
    }
}