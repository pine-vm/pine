using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.Core.PopularEncodings;
using Pine.PineVM;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class PineIRCompilerTests
{
    [TestMethod]
    public void Compile_tail_recursion_to_loop()
    {
        var recursiveFunctionSum =
            Expression.ConditionalInstance(
                condition:
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
                            input: SkipHeadPathInExpression(Expression.EnvironmentInstance, [1, 1])
                        ),
                        ])
                ),
                trueBranch:
                SkipHeadPathInExpression(Expression.EnvironmentInstance, [1, 0]),

                falseBranch:
                new Expression.ParseAndEval(
                    encoded:
                    SkipHeadPathInExpression(Expression.EnvironmentInstance, [0, 0]),
                    environment:
                    Expression.ListInstance(
                        [
                            SkipHeadPathInExpression(Expression.EnvironmentInstance, [0]),

                            Expression.ListInstance(
                                [
                                new Expression.KernelApplication
                                (
                                    function: nameof(KernelFunction.int_add),
                                    input:
                                    Expression.ListInstance(
                                        [
                                        SkipHeadPathInExpression(Expression.EnvironmentInstance, [1, 1, 0]),

                                        SkipHeadPathInExpression(Expression.EnvironmentInstance, [1, 0]),
                                        ])
                                ),

                                new Expression.KernelApplication
                                (
                                    function: nameof(KernelFunction.skip),
                                    input:
                                    Expression.ListInstance(
                                        [
                                        Expression.LiteralInstance(
                                            IntegerEncoding.EncodeSignedInteger(1)),

                                        SkipHeadPathInExpression(Expression.EnvironmentInstance, [1, 1])
                                        ])
                                ),
                                ]),
                        ]))
                );

        var recursiveFunctionSumValue =
            ExpressionEncoding.EncodeExpressionAsValue(recursiveFunctionSum);

        var compilationEnvClass =
              EnvConstraintId.Create(
                  [new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        key:
                        [0,0],
                        value:
                        recursiveFunctionSumValue)
                  ]);

        /*
         * Before implementing any tail-recursion optimization:

        Compiled instructions for test case 0:

        Push_Environment
        Skip_Head_Const (1)
        Local_Set (0)
        Skip_Head_Const (1)
        Local_Set (1)
        Length
        Equal_Binary_Const (Blob [2] (int 13))
        Jump_If_True_Const (17)
        Push_Environment
        Head_Generic
        Local_Set (2)
        Local_Get (1)
        Head_Generic
        Local_Get (0)
        Head_Generic
        Int_Add_Binary
        Local_Get (1)
        Push_Literal (Blob [2] (int 1))
        Skip_Binary
        Build_List (2)
        Build_List (2)
        Local_Get (2)
        Head_Generic
        Parse_And_Eval_Binary
        Jump_Const (3)
        Local_Get (0)
        Head_Generic

        * *

      var  expectedInstructions =
        new PineVM.StackFrameInstructions(
            [
            StackInstruction.Push_Environment,

            StackInstruction.Skip_Head_Const(1),

            StackInstruction.Local_Set(0),

            StackInstruction.Skip_Head_Const(1),

            StackInstruction.Local_Set(1),

            StackInstruction.Length,

            StackInstruction.Equal_Binary_Const(
                IntegerEncoding.EncodeSignedInteger(13)),

            StackInstruction.Jump_If_True(17),

            StackInstruction.Push_Environment,

            StackInstruction.Head_Generic,

            StackInstruction.Local_Set(2),

            StackInstruction.Local_Get(1),

            StackInstruction.Head_Generic,

            StackInstruction.Local_Get(0),

            StackInstruction.Head_Generic,

            StackInstruction.Int_Add_Binary,

            StackInstruction.Local_Get(1),

            StackInstruction.Push_Literal(
                IntegerEncoding.EncodeSignedInteger(1)),

            StackInstruction.Skip_Binary,

            StackInstruction.Build_List(2),

            StackInstruction.Build_List(2),

            StackInstruction.Local_Get(2),

            StackInstruction.Head_Generic,

            StackInstruction.Parse_And_Eval_Binary,

            StackInstruction.Jump_Unconditional(3),

            StackInstruction.Local_Get(0),

            StackInstruction.Head_Generic,
            ])
        */


        /*
         * First stage of optimization, replace the whole environment instead of individual items:
         * 
        Push_Environment
        Local_Set (0)
        Pop
        Local_Get (0)
        Skip_Head_Const (1)
        Local_Set (1)
        Skip_Head_Const (1)
        Local_Set (2)
        Length
        Equal_Binary_Const (Blob [2] (int 13))
        Jump_If_True_Const (17)
        Local_Get (0)
        Head_Generic
        Local_Set (3)
        Local_Get (2)
        Head_Generic
        Local_Get (1)
        Head_Generic
        Int_Add_Binary
        Local_Get (2)
        Push_Literal (Blob [2] (int 1))
        Skip_Binary
        Build_List (2)
        Build_List (2)
        Local_Set (0)
        Pop
        Jump_Const (-23)
        Jump_Const (3)
        Local_Get (1)
        Head_Generic
        */

        var expectedInstructions =
            new PineVM.StackFrameInstructions(
                [
                StackInstruction.Push_Environment,

                // Use a form storing the environment in a local, so stack depth is zero when we loop:
                StackInstruction.Local_Set(0),

                StackInstruction.Pop,

                // Start of the section we loop over:
                StackInstruction.Local_Get(0),

                StackInstruction.Skip_Head_Const(1),

                StackInstruction.Local_Set(1),

                StackInstruction.Skip_Head_Const(1),

                StackInstruction.Local_Set(2),

                StackInstruction.Length,

                StackInstruction.Equal_Binary_Const(
                    IntegerEncoding.EncodeSignedInteger(13)),

                StackInstruction.Jump_If_True(17),

                StackInstruction.Local_Get(0),

                StackInstruction.Head_Generic,

                StackInstruction.Local_Set(3),

                StackInstruction.Local_Get(2),

                StackInstruction.Head_Generic,

                StackInstruction.Local_Get(1),

                StackInstruction.Head_Generic,

                StackInstruction.Int_Add_Binary,

                StackInstruction.Local_Get(2),

                StackInstruction.Push_Literal(
                    IntegerEncoding.EncodeSignedInteger(1)),

                StackInstruction.Skip_Binary,

                StackInstruction.Build_List(2),

                StackInstruction.Build_List(2),

                StackInstruction.Local_Set(0),

                StackInstruction.Pop,

                StackInstruction.Jump_Unconditional(-23),

                StackInstruction.Jump_Unconditional(3),

                StackInstruction.Local_Get(1),

                StackInstruction.Head_Generic,
            ]);

        var parseCache = new PineVMParseCache();

        var compiled =
            PineIRCompiler.CompileExpression(
                recursiveFunctionSum,
                rootExprAlternativeForms: [],
                envClass: compilationEnvClass,
                parseCache);

        var actualInstructionsText =
            string.Join(
                "\n",
                compiled.Instructions.Select(instruction => instruction.ToString()));

        System.Console.WriteLine(
            $"Compiled instructions:\n{actualInstructionsText}");

        compiled.Instructions.Count.Should().Be(expectedInstructions.Instructions.Count, "Instructions count");

        for (var instructionIndex = 0; instructionIndex < expectedInstructions.Instructions.Count; instructionIndex++)
        {
            compiled.Instructions[instructionIndex].Should().Be(
                expectedInstructions.Instructions[instructionIndex],
                $"Instruction at index {instructionIndex} of " + compiled.Instructions.Count);
        }

        var compilationEnvClasses =
            ImmutableDictionary<Expression, IReadOnlyList<EnvConstraintId>>.Empty
            .SetItem(recursiveFunctionSum, [compilationEnvClass]);

        var pineVM =
            new PineVM(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: compilationEnvClasses,
                enableTailRecursionOptimization: true,
                disableReductionInCompilation: true);

        var rootEnvironment =
            PineValue.List(
                [
                    PineValue.List([recursiveFunctionSumValue]),

                    PineValue.List(
                        [
                            IntegerEncoding.EncodeSignedInteger(1_071),

                            PineValue.List(
                                [
                                ..Enumerable.Range(0, 31)
                                .Select(i => IntegerEncoding.EncodeSignedInteger(i))
                                ]),
                        ]),
                ]);

        var evalResult =
            pineVM.EvaluateExpressionOnCustomStack(
                recursiveFunctionSum,
                rootEnvironment: rootEnvironment,
                config: new PineVM.EvaluationConfig(ParseAndEvalCountLimit: null));

        if (evalResult.IsErrOrNull() is { } evalErr)
        {
            throw new System.Exception("Failed eval: " + evalErr);
        }

        if (evalResult.IsOkOrNull() is not { } evalReport)
        {
            throw new System.NotImplementedException(
                "Unknown eval result type: " + evalResult);
        }

        var resultAsInteger =
            IntegerEncoding.ParseSignedIntegerRelaxed(evalReport.ReturnValue);

        if (resultAsInteger.IsErrOrNull() is { } resultErr)
        {
            throw new System.Exception("Failed to convert result to integer: " + resultErr);
        }

        if (resultAsInteger.IsOkOrNullable() is not { } resultInteger)
        {
            throw new System.NotImplementedException(
                "Unknown result value type: " + resultAsInteger);
        }

        System.Console.WriteLine(
            "Evaluated result: " + resultInteger);

        System.Console.WriteLine(
            "Eval invocation count: " + evalReport.InvocationCount);

        System.Console.WriteLine(
            "Eval instruction count: " + evalReport.InstructionCount);

        System.Console.WriteLine(
            "Eval loop iteration count: " + evalReport.LoopIterationCount);

        resultInteger.Should().Be(
            1_071 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17,
            "Evaluated result");

        evalReport.InvocationCount.Should().BeLessThan(4);

        evalReport.InstructionCount.Should().BeGreaterThan(300);

        evalReport.LoopIterationCount.Should().BeGreaterThan(7);
    }

    static Expression SkipHeadPathInExpression(Expression source, System.ReadOnlySpan<int> path)
    {
        if (path.Length is 0)
        {
            return source;
        }

        var pathItem = path[0];

        var exprAfterSkip =
            pathItem < 1
            ?
            source
            :
            new Expression.KernelApplication
            (
                function: nameof(KernelFunction.skip),
                input:
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(
                            IntegerEncoding.EncodeSignedInteger(pathItem)),

                        source,
                    ])
            );

        return
            SkipHeadPathInExpression(
                new Expression.KernelApplication
                (
                    function: nameof(KernelFunction.head),
                    input: exprAfterSkip
                ),
                path[1..]);
    }
}