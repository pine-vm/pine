using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System;
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

        var renderedFrame =
            RenderFrame(
                functionRecord.InnerFunction,
                parseCache);

        renderedFrame.Should().Be(
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

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Single_wrapping_tag_immediately_consumed_in_equality_checks(bool disableInlining)
    {
        const string ElmModuleText =
            """
            module Test exposing (..)


            testFunction offset source =
                case String.slice offset 13 source of
                    "alfa" ->
                        71

                    "beta" ->
                        73

                    _ ->
                        79
                    
            """;

        /*
         * Pine should optimize this so that the `String` wrapper tag produced by `String.slice` is eliminated:
         * 
         * 1. Based the Pine expression of `String.slice`, prove that every returning branch adds the `String` wrapper.
         * The optimization functionality in Pine must build a model of the expression return type, a constraint which
         * applies independent of the value in `Environment`
         * 
         * 2. Recognize that all consumers of the return value immediately consume the `String` wrapper tag, and that there is no
         * case in which the whole value including the `String` tag escapes.
         * 
         * This means that we can locally eliminate the `String` wrapper tag, which in turn reduces allocations.
         * 
         * To implement that, Pine must use a specialized version of the `String.slice` function/expression which does not add the tag,
         * and then on the other side adapt the values used in the equality checks to be unwrapped values.
         * 
         * The application of this optimization must not depend on inlining: It should apply in both inlined and non-inlined cases.
         * 
         * Since the new Elm compiler embeds applied non-recursive functions as literal values, all information needed to develop the
         * proofs are available in the parent expression `testFunction` without considering the `Environment`.
         * 
         * A tradeoff that we accept here is that this specialization increases number of distinct compiled expressions in non-inlined cases.
         * */

        var parsedEnvironment =
            ElmCompilerTestHelper.CompileElmModules(
                [ElmModuleText],
                disableInlining).parsedEnv;

        var functionValue =
            parsedEnvironment.Modules
            .Single(module => module.moduleName is "Test")
            .moduleContent.FunctionDeclarations["testFunction"];

        var parseCache = new PineVMParseCache();

        var functionRecord =
            FunctionRecord.ParseFunctionRecordTagged(functionValue, parseCache)
            .Extract(error => throw new Exception(error));

        var renderedFrame =
            RenderFrame(
                functionRecord.InnerFunction,
                parseCache);

        renderedFrame.Should().Be(
            """
            56cb6426 (52):
             0: Local_Get (0)
             1: Int_Greater_Than_Or_Equal_Const (0)
             2: Local_Get (0)
             3: Int_Less_Than_Or_Equal_Const (13)
             4: Logical_And_Binary
             5: Jump_If_Equal_Const (Blob [1] (0x04) , 27)
             6: Local_Get (1)
             7: Skip_Head_Const (1)
             8: Head_Generic
             9: Local_Set (2)
            10: Local_Get (0)
            11: Int_Mul_Const (4)
            12: Push_Literal (Blob [16] (0x0000004500000076000000610000006c | UTF32 "Eval"))
            13: Push_Literal (List [2] (79))
            14: Push_Literal (Blob [16] (0x0000004c000000690000007300000074 | UTF32 "List"))
            15: Push_Literal (List [2] (2))
            16: Push_Literal (Blob [24] (0x0000004c000000690000007400000072000000610000006c | UTF32 "Litral"))
            17: Local_Get (2)
            18: Build_List (2)
            19: Push_Literal (List [1] (1))
            20: Build_List (4)
            21: Build_List (3)
            22: Local_Set (3)
            23: Eval_Binary
            24: Local_Set (4)
            25: Push_Literal (Blob [2] (0x0434 | int 52))
            26: Local_Get (3)
            27: Eval_Binary
            28: Local_Get (4)
            29: Int_Sub_Binary
            30: Slice_Skip_Var_Take_Var
            31: Jump_Const (13)
            32: Local_Get (1)
            33: Skip_Head_Const (1)
            34: Head_Generic
            35: Local_Get (0)
            36: Int_Mul_Const (4)
            37: Push_Literal (Blob [2] (0x040d | int 13))
            38: Local_Get (0)
            39: Int_Mul_Const (-1)
            40: Build_List (2)
            41: Int_Add_Generic
            42: Int_Mul_Const (4)
            43: Slice_Skip_Var_Take_Var
            44: Local_Set (2)
            45: Switch_Jump_If_Equal_Const (2)
              case Blob [16] (0x000000610000006c0000006600000061 | UTF32 "alfa"): jump 3
              case Blob [16] (0x00000062000000650000007400000061 | UTF32 "beta"): jump 5
            46: Push_Literal (Blob [2] (0x044f | int 79))
            47: Return
            48: Push_Literal (Blob [2] (0x0447 | int 71))
            49: Return
            50: Push_Literal (Blob [2] (0x0449 | int 73))
            51: Return
            """);
    }

    private static string RenderFrame(
        Expression rootExpression,
        PineVMParseCache parseCache)
    {
        var encodedExpression = ExpressionEncoding.EncodeExpressionAsValue(rootExpression);

        var frameInstructions =
            ExpressionCompilation.CompileExpression(
                rootExpression,
                specializations: [],
                parseCache,
                disableReduction: false,
                enableTailRecursionOptimization: true,
                skipInlining: (_, _) => false).Generic;

        var idHash =
            Convert.ToHexStringLower(PineValueHashTree.ComputeHash(encodedExpression).Span)
            [..8];

        return
            string.Concat(
                idHash,
                " (",
                frameInstructions.Instructions.Count.ToString(),
                "):\n",
                StackInstructionTraceRenderer.RenderStackFrameInstructions(frameInstructions));
    }
}
