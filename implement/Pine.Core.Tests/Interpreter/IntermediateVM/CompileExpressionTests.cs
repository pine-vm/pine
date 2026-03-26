using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
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

    private static string InstructionsToText(IReadOnlyList<StackInstruction> instructions) =>
        string.Join("\n", instructions.Select(i => i.ToString()));
}
