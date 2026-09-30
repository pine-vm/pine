using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class CompileExpressionTests
{
    [Fact]
    public void Compile_head_after_constant_skip_from_local_uses_fused_instruction()
    {
        var expression =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.head),
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.skip),
                    Expression.ListInst(
                        [
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(6)),
                        Expression.EnvironmentInstance
                        ])));

        var instructions =
            ExpressionCompilation.ControlFlowGraphFromExpression(
                expression,
                rootExprAlternativeForms: [],
                envClass: null,
                parametersAsLocals: StaticFunctionInterface.Generic,
                new PineVMParseCache())
            .LowerToStackInstructions();

        instructions.Should().Equal(
            StackInstruction.Local_Get_Skip_Head_Const(localIndex: 0, skipCount: 6),
            StackInstruction.Return);
    }

    [Fact]
    public void Compile_head_after_constant_skip_from_conditional_does_not_fuse_into_one_branch()
    {
        var comparedLiteral = PineValue.Blob([7]);
        var falseBranchLiteral = PineValue.List([PineValue.EmptyBlob, PineValue.EmptyList, PineValue.EmptyBlob]);

        var conditional =
            Expression.ConditionalInst(
                condition:
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.equal),
                    Expression.ListInst(
                        [
                        Expression.EnvironmentInstance,
                        Expression.LitralInst(comparedLiteral),
                        ])),
                falseBranch: Expression.LitralInst(falseBranchLiteral),
                trueBranch: Expression.EnvironmentInstance);

        var expression =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.head),
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.skip),
                    Expression.ListInst(
                        [
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(2)),
                        conditional
                        ])));

        var instructions =
            ExpressionCompilation.ControlFlowGraphFromExpression(
                expression,
                rootExprAlternativeForms: [],
                envClass: null,
                parametersAsLocals: StaticFunctionInterface.Generic,
                new PineVMParseCache())
            .LowerToStackInstructions();

        /*
         * The skip applies to the value from both branches, so it must follow the join
         * instead of being fused into the local read at the end of the true branch.
         * */
        instructions.Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Jump_If_Equal(3, comparedLiteral),
            StackInstruction.Push_Literal(falseBranchLiteral),
            StackInstruction.Jump_Unconditional(2),
            StackInstruction.Local_Get(0),
            StackInstruction.Skip_Head_Const(2),
            StackInstruction.Return);
    }

    [Fact]
    public void Compile_stack_frame_instructions_from_files()
    {
        var parseCache = new PineVMParseCache();

        var results =
            TestResultSummary.RunFileBasedTestCases(
                "CompileStackFrameInstructions",
                caseDir =>
                {
                    var expressionJson = File.ReadAllText(Path.Combine(caseDir, "expression.json"));
                    var expression = EncodePineExpressionAsJson.SingleFromJsonString(expressionJson);

                    var expectedInstructionsText =
                        File.ReadAllText(Path.Combine(caseDir, "instructions.txt")).TrimEnd();

                    var compiled =
                        ExpressionCompilation.CompileExpression(
                            expression,
                            specializations: [],
                            parseCache,
                            disableReduction: true,
                            skipInlining: (_, _) => false,
                            enableTailRecursionOptimization: false);

                    var compiledInstructionsText =
                        InstructionsToText(compiled.Generic.Instructions);

                    return (expected: expectedInstructionsText, actual: compiledInstructionsText);
                },
                trimWhitespace: true);

        var summary = TestResultSummary.RenderSummary(results);

        results.Where(r => !r.Passed).Should().BeEmpty(summary);
    }

    [Fact]
    public void Compile_switch_over_slice_of_blob()
    {
        AssertSwitchOverSliceCompilation(
            firstLiteral: PineValue.Blob([4, 5]),
            secondLiteral: PineValue.Blob([6, 7]));
    }

    [Fact]
    public void Compile_switch_over_slice_of_list()
    {
        AssertSwitchOverSliceCompilation(
            firstLiteral:
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(4),
                IntegerEncoding.EncodeSignedInteger(5),
                ]),
            secondLiteral:
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(6),
                IntegerEncoding.EncodeSignedInteger(7),
                ]));
    }

    [Theory]
    [InlineData(1)]
    [InlineData(4)]
    [InlineData(0)]
    [InlineData(-1)]
    public void Compile_switch_over_scaled_slice(int multiplier)
    {
        AssertSwitchOverSliceCompilation(
            firstLiteral: PineValue.Blob([4, 5]),
            secondLiteral: PineValue.Blob([6, 7]),
            multiplier: multiplier);
    }

    [Fact]
    public void Slice_switch_rejects_duplicate_literals()
    {
        var literal = PineValue.Blob([4, 5]);

        var action =
            () =>
            StackInstruction.Switch_Jump_If_Slice_Skip_Var_Equal_Const(
                [
                new SliceSwitchCase(literal, 1),
                new SliceSwitchCase(literal, 2)
                ],
                skipCountMultiplier: 1);

        action.Should()
            .Throw<ArgumentException>()
            .WithMessage("*distinct*");
    }

    private static void AssertSwitchOverSliceCompilation(
        PineValue firstLiteral,
        PineValue secondLiteral,
        int? multiplier = null)
    {
        var rawSkipCountExpression =
            (Expression)
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.head),
                input: Expression.EnvironmentInstance);

        var skipCountExpression =
            multiplier is { } factor
            ?
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.int_mul),
                input:
                Expression.ListInst(
                    [
                    rawSkipCountExpression,
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(factor))
                    ]))
            :
            rawSkipCountExpression;

        var sourceExpression =
            (Expression)
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.head),
                input:
                Expression.BuiltinInst(
                    function: nameof(BuiltinFunction.skip),
                    input:
                    Expression.ListInst(
                        [
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(1)),
                        Expression.EnvironmentInstance,
                        ])));

        var slicedExpression =
            (Expression)
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.take),
                input:
                Expression.ListInst(
                    [
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(2)),
                    Expression.BuiltinInst(
                        function: nameof(BuiltinFunction.skip),
                        input:
                        Expression.ListInst(
                            [
                            skipCountExpression,
                            sourceExpression,
                            ])),
                    ]));

        Expression EqualTo(PineValue literal) =>
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.equal),
                input:
                Expression.ListInst(
                    [
                    slicedExpression,
                    Expression.LitralInst(literal),
                    ]));

        var expression =
            (Expression)
            Expression.ConditionalInst(
                condition: EqualTo(firstLiteral),
                falseBranch:
                Expression.ConditionalInst(
                    condition: EqualTo(secondLiteral),
                    falseBranch: Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(0)),
                    trueBranch: Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(2))),
                trueBranch: Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(1)));

        var compiled =
            ExpressionCompilation.CompileExpression(
                expression,
                specializations: [],
                new PineVMParseCache(),
                disableReduction: true,
                skipInlining: (_, _) => false,
                enableTailRecursionOptimization: false);

        var switchInstruction =
            compiled.Generic.Instructions
            .Single(
                instruction =>
                instruction.Kind is
                StackInstructionKind.Switch_Jump_If_Slice_Skip_Var_Equal_Const);

        switchInstruction.SwitchJumpTable.Should().BeNull();
        switchInstruction.IntegerLiteral.Should().Be(new BigInteger(multiplier ?? 1));
        switchInstruction.SliceSwitchCases.Should().HaveCount(2);

        compiled.Generic.Instructions.Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Int_Mul_Const);

        switchInstruction.SliceSwitchCases
            .Select(switchCase => switchCase.Literal)
            .Should().Equal(firstLiteral, secondLiteral);

        var graph =
            ExpressionCompilation.ControlFlowGraphFromExpression(
                expression,
                rootExprAlternativeForms: [],
                envClass: null,
                parametersAsLocals: StaticFunctionInterface.FromExpression(expression),
                new PineVMParseCache());

        var switchTerminator =
            graph.Blocks
            .Select(block => block.Terminator)
            .OfType<PineControlFlowTerminator.Switch>()
            .Single();

        switchTerminator.Kind.Should().Be(PineSwitchKind.SliceSkipVarEqual);
        switchTerminator.PopCount.Should().Be(2);
        switchTerminator.SkipCountMultiplier.Should().Be(new BigInteger(multiplier ?? 1));

        switchTerminator.Cases
            .Select(switchCase => switchCase.Literal)
            .Should().Equal(firstLiteral, secondLiteral);

        graph.LowerToStackInstructions()
            .Single(
            instruction =>
            instruction.Kind is
            StackInstructionKind.Switch_Jump_If_Slice_Skip_Var_Equal_Const)
            .SliceSwitchCases.Should().Equal(switchInstruction.SliceSwitchCases);

        var instructionDetails = StackInstruction.GetDetails(switchInstruction);

        instructionDetails.PopCount.Should().Be(2);
        instructionDetails.PushCount.Should().Be(0);
        instructionDetails.Display().DetailLines.Should().HaveCount(2);

        var pineVM =
            Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null);

        PineValue PrefixLiteral(PineValue literal) =>
            literal switch
            {
                PineValue.BlobValue blob =>
                PineValue.Blob([.. Enumerable.Repeat((byte)9, Math.Max(0, multiplier ?? 1)), .. blob.Bytes.Span]),

                PineValue.ListValue list =>
                PineValue.List(
                    [
                    .. Enumerable.Repeat(IntegerEncoding.EncodeSignedInteger(9), Math.Max(0, multiplier ?? 1)),
                    .. list.Items.Span
                    ]),

                _ =>
                throw new System.NotImplementedException()
            };

        var evaluations =
            new[]
            {
                (Source: PrefixLiteral(firstLiteral), SkipCount: (PineValue)IntegerEncoding.EncodeSignedInteger(1), Expected: 1),
                (Source: PrefixLiteral(secondLiteral), SkipCount: (PineValue)IntegerEncoding.EncodeSignedInteger(1), Expected: 2),
                (Source: firstLiteral, SkipCount: (PineValue)IntegerEncoding.EncodeSignedInteger(1), Expected: multiplier is null or > 0 ? 0 : 1),
                (Source: PrefixLiteral(firstLiteral), SkipCount: PineValue.EmptyList, Expected: 0),
            };

        foreach (var evaluation in evaluations)
        {
            var environment =
                PineValue.List(
                    [
                    evaluation.SkipCount,
                    evaluation.Source,
                    ]);

            pineVM.EvaluateExpression(expression, environment)
                .Should()
                .Be(Result<string, PineValue>.ok(IntegerEncoding.EncodeSignedInteger(evaluation.Expected)));
        }
    }

    private static string InstructionsToText(IReadOnlyList<StackInstruction> instructions) =>
        string.Join("\n", instructions.Select(i => i.ToString()));
}
