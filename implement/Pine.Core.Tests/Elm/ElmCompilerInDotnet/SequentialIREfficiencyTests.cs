using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class SequentialIREfficiencyTests
{
    [Fact]
    public void Case_block_with_two_literal_uncons_patterns()
    {
        const string ElmModuleText =
            """
            module Test exposing (..)


            testFunction param =
                case param of
                    71 :: 73 :: other ->
                        other

                    _ ->
                        param
            """;

        var parsedEnvironment =
            ElmCompilerTestHelper.CompileElmModules(
                [ElmModuleText],
                disableInlining: false).parsedEnv;

        var functionValue =
            parsedEnvironment.Modules
            .Single(module => module.moduleName is "Test")
            .moduleContent.FunctionDeclarations["testFunction"];

        var parseCache = new PineVMParseCache();

        var functionRecord =
            FunctionRecord.ParseFunctionRecordTagged(functionValue, parseCache)
            .Extract(error => throw new Exception(error));

        var renderedFrames =
            RenderFrameAndDependencies(
                functionRecord.InnerFunction,
                parseCache);

        renderedFrames.Should().Be(
            """
            8820d122 (8):
            0: Local_Get (0)
            1: Take_Const (2)
            2: Jump_If_Equal_Const (List [2] (2) , 3)
            3: Local_Get (0)
            4: Return
            5: Local_Get (0)
            6: Skip_Const (2)
            7: Return
            """);
    }

    private static string RenderFrameAndDependencies(
        Expression rootExpression,
        PineVMParseCache parseCache)
    {
        var frames = CompileFrameAndDependencies(rootExpression, parseCache);

        string RenderFrame(PineValue encodedExpression)
        {
            var frameInstructions = frames.FramesByEncodedExpression[encodedExpression];

            var idHash =
                Convert.ToHexStringLower(PineValueHashTree.ComputeHash(encodedExpression).Span)
                [..8];

            var instructionsText =
                StackInstructionTraceRenderer.RenderStackFrameInstructions(frameInstructions);

            return
                string.Concat(
                    idHash,
                    " (",
                    frameInstructions.Instructions.Count.ToString(),
                    "):\n",
                    instructionsText);
        }

        return
            string.Join(
                "\n\n",
                frames.RenderOrder.Select(RenderFrame));
    }

    private static SequentialIRFrames CompileFrameAndDependencies(
        Expression rootExpression,
        PineVMParseCache parseCache)
    {
        var framesByEncodedExpression = new Dictionary<PineValue, StackFrameInstructions>();
        var renderOrder = new List<PineValue>();
        var queuedExpressions = new HashSet<PineValue>();
        var pendingExpressions = new Queue<(Expression expression, PineValue encodedExpression)>();

        Enqueue(
            rootExpression,
            ExpressionEncoding.EncodeExpressionAsValue(rootExpression));

        while (pendingExpressions.TryDequeue(out var pending))
        {
            framesByEncodedExpression.Add(
                pending.encodedExpression,
                ExpressionCompilation.CompileExpression(
                    pending.expression,
                    specializations: [],
                    parseCache,
                    disableReduction: false,
                    enableTailRecursionOptimization: true,
                    skipInlining: (_, _) => false).Generic);

            renderOrder.Add(pending.encodedExpression);

            foreach (var eval in
                Expression.EnumerateSelfAndDescendants(pending.expression)
                .OfType<Expression.Eval>())
            {
                if (eval.Encoded is not Expression.Litral encodedLiteral)
                    continue;

                var childExpression =
                    parseCache.ParseExpression(encodedLiteral.Value)
                    .Extract(error => throw new Exception(error));

                Enqueue(childExpression, encodedLiteral.Value);
            }
        }

        return new(framesByEncodedExpression, renderOrder);

        void Enqueue(Expression expression, PineValue encodedExpression)
        {
            if (queuedExpressions.Add(encodedExpression))
                pendingExpressions.Enqueue((expression, encodedExpression));
        }
    }

    private sealed record SequentialIRFrames(
        IReadOnlyDictionary<PineValue, StackFrameInstructions> FramesByEncodedExpression,
        IReadOnlyList<PineValue> RenderOrder);
}
