using AwesomeAssertions;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Elm;
using Pine.IntermediateVM;
using System;
using Xunit;

namespace Pine.IntegrationTests;

public class ParseElmInteractiveSubmissionTests
{
    [Fact]
    public void Parse_Elm_Interactive_submission()
    {
        var testCases = new[]
        {
            new
            {
                SubmissionText = "123",
                ExpectedResponse = "ExpressionSubmission (Integer 123)"
            },
            new
            {
                SubmissionText = "test",
                ExpectedResponse = "ExpressionSubmission (FunctionOrValue [] \"test\")"
            },
            new
            {
                SubmissionText = "greet \"World\"",
                ExpectedResponse = "ExpressionSubmission (Application [ Node { end = { column = 10, row = 7 }, start = { column = 5, row = 7 } } (FunctionOrValue [] \"greet\"), Node { end = { column = 18, row = 7 }, start = { column = 11, row = 7 } } (Literal \"World\") ])"
            },
            new
            {
                SubmissionText = "replicate 2 3",
                ExpectedResponse = "ExpressionSubmission (Application [ Node { end = { column = 14, row = 7 }, start = { column = 5, row = 7 } } (FunctionOrValue [] \"replicate\"), Node { end = { column = 16, row = 7 }, start = { column = 15, row = 7 } } (Integer 2), Node { end = { column = 18, row = 7 }, start = { column = 17, row = 7 } } (Integer 3) ])"
            },
            new
            {
                SubmissionText = "test = 123",
                ExpectedResponse = "DeclarationSubmission (FunctionDeclaration { declaration = Node { end = { column = 11, row = 6 }, start = { column = 1, row = 6 } } { arguments = [], expression = Node { end = { column = 11, row = 6 }, start = { column = 8, row = 6 } } (Integer 123), name = Node { end = { column = 5, row = 6 }, start = { column = 1, row = 6 } } \"test\" }, documentation = Nothing, signature = Nothing })"
            }
        };

        for (var i = 0; i < testCases.Length; i++)
        {
            var testCase = testCases[i];

            TestParsingSubmissionText(testCase.SubmissionText, testCase.ExpectedResponse);
        }
    }

    static readonly ElmCompilerCache elmCompilerCache = new();

    public static ElmValue TestParsingSubmissionText(
        string submissionText,
        string expectedExpressionString)
    {
        var parseClock = System.Diagnostics.Stopwatch.StartNew();

        var parsedModulePineValue =
            ParseElmInteractiveSubmissionTextToPineValue(submissionText)
            .Extract(err => throw new Exception(err));

        Console.WriteLine(
            "Parsed Elm Interactive submission text in " +
            CommandLineInterface.FormatIntegerForDisplay(parseClock.ElapsedMilliseconds) +
            " milliseconds");

        var responseAsElmValue =
            elmCompilerCache.PineValueDecodedAsElmValue(parsedModulePineValue)
            .Extract(err => throw new Exception(err));

        var responseAsExpression =
            ElmValue.RenderAsElmExpression(responseAsElmValue).expressionString;

        responseAsExpression.Should().Be(
            expectedExpressionString,
            "Submission parsed as expression syntax");

        return responseAsElmValue;
    }

    public static Result<string, PineValue> ParseElmInteractiveSubmissionTextToPineValue(string submissionText)
    {
        var elmCompilerFromBundle =
            BundledElmEnvironments.BundledElmCompilerCompiledEnvValue();

        elmCompilerFromBundle.Should().NotBeNull("Elm compiler environment not found in bundled environments");

        var elmCompiler =
            ElmCompilerInElm.ElmCompilerFromEnvValue(elmCompilerFromBundle)
            .Extract(err => throw new Exception(err));

        var pineVMCache = new InvocationCache();

        var pineVM =
            SetupVM.Create(evalCache: pineVMCache);

        return elmCompiler.ParseElmInteractiveSubmissionText(submissionText, pineVM);
    }
}
