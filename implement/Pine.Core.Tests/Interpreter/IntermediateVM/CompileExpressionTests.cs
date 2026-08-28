using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Json;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class CompileExpressionTests
{

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

    private static void AssertSwitchOverSliceCompilation(
        PineValue firstLiteral,
        PineValue secondLiteral)
    {
        var skipCountExpression =
            (Expression)
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.head),
                input: Expression.EnvironmentInstance);

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
            .Single(instruction =>
                instruction.Kind is
                StackInstructionKind.Switch_Jump_If_Slice_Skip_Var_Equal_Const);

        switchInstruction.SwitchJumpTable.Should().HaveCount(2);

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
                PineValue.Blob([9, .. blob.Bytes.Span]),

                PineValue.ListValue list =>
                PineValue.List([IntegerEncoding.EncodeSignedInteger(9), .. list.Items.Span]),

                _ =>
                throw new System.NotImplementedException()
            };

        var evaluations =
            new[]
            {
                (Source: PrefixLiteral(firstLiteral), Expected: 1),
                (Source: PrefixLiteral(secondLiteral), Expected: 2),
                (Source: firstLiteral, Expected: 0),
            };

        foreach (var evaluation in evaluations)
        {
            var environment =
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(1),
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
