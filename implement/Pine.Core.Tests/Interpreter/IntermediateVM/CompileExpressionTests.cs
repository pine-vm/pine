using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Json;
using System;
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
        var testDataDir = TestDataDirectory();

        var caseDirs =
            Directory.GetDirectories(testDataDir)
            .Order()
            .ToArray();

        caseDirs.Should().NotBeEmpty(
            "Expected test case directories in " + testDataDir);

        var parseCache = new PineVMParseCache();

        for (var testCaseIndex = 0; testCaseIndex < caseDirs.Length; ++testCaseIndex)
        {
            var caseDir = caseDirs[testCaseIndex];
            var caseName = Path.GetFileName(caseDir);

            var expressionJsonPath = Path.Combine(caseDir, "expression.json");
            var instructionsTxtPath = Path.Combine(caseDir, "instructions.txt");

            var expressionJson = File.ReadAllText(expressionJsonPath);
            var expression = EncodePineExpressionAsJson.SingleFromJsonString(expressionJson);

            var expectedInstructionsText =
                File.ReadAllText(instructionsTxtPath).TrimEnd();

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

            try
            {
                compiledInstructionsText.Should().Be(
                    expectedInstructionsText,
                    $"Instructions for test case {testCaseIndex} ({caseName})");
            }
            catch (Exception e)
            {
                throw new Exception(
                    $"Failed for test case {testCaseIndex} ({caseName}) " +
                    $"with expression {expression}",
                    e);
            }
        }
    }

    private static string InstructionsToText(IReadOnlyList<StackInstruction> instructions) =>
        string.Join("\n", instructions.Select(i => i.ToString()));

    private static string TestDataDirectory()
    {
        // Walk up from the test assembly directory to find the source project directory.
        // The test data lives in the source tree alongside the .csproj file.
        var dir = new DirectoryInfo(AppContext.BaseDirectory);

        while (dir is not null)
        {
            var candidate = Path.Combine(dir.FullName, "TestData", "CompileStackFrameInstructions");

            if (Directory.Exists(candidate))
                return candidate;

            dir = dir.Parent;
        }

        throw new DirectoryNotFoundException(
            "Could not find TestData/CompileStackFrameInstructions directory " +
            "in any parent of " + AppContext.BaseDirectory);
    }
}
