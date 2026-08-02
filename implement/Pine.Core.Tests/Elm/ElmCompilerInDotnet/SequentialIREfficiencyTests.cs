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
            ff5a3d917543c32dd0923a70c3d7848c88817f68973641f1326e62eb62d732ca
             0: Local_Get (0)
             1: Skip_Head_Const (1)
             2: Jump_If_Equal_Const (Blob [2] (0x0449 | int 73) , 3)
             3: Push_Literal (Blob [1] (0x02))
             4: Jump_Const (6)
             5: Local_Get (0)
             6: Skip_Const (1)
             7: Local_Set (1)
             8: Length_Equal_Const (0)
             9: Equal_Binary_Const (Blob [1] (0x02))
            10: Jump_If_Equal_Const (Blob [1] (0x04) , 3)
            11: Push_Literal (Blob [1] (0x02))
            12: Jump_Const (9)
            13: Local_Get (0)
            14: Head_Generic
            15: Jump_If_Equal_Const (Blob [2] (0x0447 | int 71) , 3)
            16: Push_Literal (Blob [1] (0x02))
            17: Jump_Const (4)
            18: Local_Get (0)
            19: Length_Equal_Const (0)
            20: Equal_Binary_Const (Blob [1] (0x02))
            21: Jump_If_Equal_Const (Blob [1] (0x04) , 3)
            22: Local_Get (0)
            23: Return
            24: Local_Get (0)
            25: Skip_Const (1)
            26: Local_Set (1)
            27: Skip_Const (1)
            28: Return
            """);
    }

    private static string RenderFrameAndDependencies(
        Expression rootExpression,
        PineVMParseCache parseCache)
    {
        var frames = CompileFrameAndDependencies(rootExpression, parseCache);

        return
            string.Join(
                "\n\n",
                frames.RenderOrder.Select(
                    encodedExpression =>
                    Convert.ToHexStringLower(
                        PineValueHashTree.ComputeHash(encodedExpression).Span) +
                    "\n" +
                    StackInstructionTraceRenderer.RenderStackFrameInstructions(
                        frames.FramesByEncodedExpression[encodedExpression])));
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
