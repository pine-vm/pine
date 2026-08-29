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
            43aa790e (44):
             0: Local_Get (0)
             1: Int_Greater_Than_Or_Equal_Const (0)
             2: Local_Get (0)
             3: Int_Less_Than_Or_Equal_Const (13)
             4: Logical_And_Binary
             5: Jump_If_Equal_Const (Blob [1] (0x04) , 21)
             6: Local_Get (1)
             7: Skip_Head_Const (2)
             8: Local_Set (2)
             9: Local_Get (0)
            10: Int_Mul_Const (4)
            11: Local_Get (2)
            12: Build_List_With_Prefix (1 , 1)
              Blob [24] (0x0000004c000000690000007400000072000000610000006c | UTF32 "Litral")
            13: Push_Literal (List [1] (1))
            14: Build_List_With_Prefix (2 , 2)
              Blob [16] (0x0000004c000000690000007300000074 | UTF32 "List")
              List [2] (2)
            15: Build_List_With_Prefix (2 , 1)
              Blob [16] (0x0000004500000076000000610000006c | UTF32 "Eval")
              List [2] (79)
            16: Local_Set (3)
            17: Eval_Binary
            18: Local_Set (4)
            19: Push_Literal (Blob [2] (0x0434 | int 52))
            20: Local_Get (3)
            21: Eval_Binary
            22: Local_Get (4)
            23: Int_Sub_Binary
            24: Slice_Skip_Var_Take_Var
            25: Jump_Const (11)
            26: Local_Get (1)
            27: Skip_Head_Const (2)
            28: Local_Get (0)
            29: Int_Mul_Const (4)
            30: Local_Get (0)
            31: Int_Mul_Const (-1)
            32: Build_List_With_Prefix (1 , 1)
              Blob [2] (0x040d | int 13)
            33: Int_Add_Generic
            34: Int_Mul_Const (4)
            35: Slice_Skip_Var_Take_Var
            36: Local_Set (2)
            37: Switch_Jump_If_Equal_Const (2)
              case Blob [16] (0x000000610000006c0000006600000061 | UTF32 "alfa"): jump 3
              case Blob [16] (0x00000062000000650000007400000061 | UTF32 "beta"): jump 5
            38: Push_Literal (Blob [2] (0x044f | int 79))
            39: Return
            40: Push_Literal (Blob [2] (0x0447 | int 71))
            41: Return
            42: Push_Literal (Blob [2] (0x0449 | int 73))
            43: Return
            """);
    }

    [Fact]
    public void String_startsWithUpper_using_String_slice_first_char()
    {
        const string ElmModuleText =
            """
            module Test exposing (..)


            testFunction : String -> String
            testFunction name =
                case String.slice 0 1 name of
                    "A" ->
                        "yes"

                    "B" ->
                        "yes"

                    other ->
                        other
            
            """;

        /*
         * In this scenario Pine must eliminate the branch in `String.slice` checking if `start` offset is >= 0 and `end` offset is >= `start`
         * 
         * Pine must statically prove these conditions from the `String.slice` implementation are always true for the given arguments,
         * and emit a specialized version of `String.slice` which does not include the branch.
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
            02085566 (10):
            0: Local_Get (0)
            1: Skip_Head_Const (2)
            2: Take_Const (4)
            3: Local_Set (1)
            4: Switch_Jump_If_Equal_Const (2)
              case Blob [4] (0x00000041 | UTF32 "A"): jump 4
              case Blob [4] (0x00000042 | UTF32 "B"): jump 4
            5: Local_Get (1)
            6: Build_List_With_Prefix (2 , 1)
              Blob [52] (0x0000003c00000043000000680000006f0000006900000063000000650000005f... | UTF32 "\u003CChoice_Type\u003E")
              Blob [24] (0x000000530000007400000072000000690000006e00000067 | UTF32 "String")
            7: Return
            8: Push_Literal (List [3] (3))
            9: Return
            """);
    }

    [Fact]
    public void String_slice_single_char_at_offset_proven_to_be_non_negative()
    {
        const string ElmModuleText =
            """
            module Test exposing (..)

            
            testFunction : String -> Int -> Bool
            testFunction source offset =
                if
                    offset >= 0
                then
                    case String.slice offset (offset + 1) source of
                        "A" ->
                            True

                        "B" ->
                            True

                        _ ->
                            False

                else
                    False
            
            """;

        /*
         * In this scenario Pine must eliminate the branch in `String.slice` checking if `start` offset is >= 0 and `end` offset is >= `start`
         * 
         * Pine must statically prove these conditions from the `String.slice` implementation are always true for the given arguments,
         * and emit a specialized version of `String.slice` which does not include the branch.
         * 
         * The condition and branch at the root should make it trivial to prove that the `start` offset is always >= 0.
         * To prove that the `end` offset is always >= `start`, the system must integrate knowledge of the semantics of `Pine_builtin.int_add`.
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
            2386c060 (22):
             0: Local_Get (1)
             1: Push_Literal (Blob [2] (0x0400 | int 0))
             2: Slice_Skip_Var_Equal_Const (Blob [1] (0x04))
             3: Jump_If_Equal_Const (Blob [1] (0x04) , 3)
             4: Push_Literal (Blob [1] (0x02))
             5: Return
             6: Local_Get (0)
             7: Skip_Head_Const (2)
             8: Local_Get (1)
             9: Int_Mul_Const (4)
            10: Local_Get (1)
            11: Local_Get (1)
            12: Int_Sub_Binary
            13: Int_Add_Const (1)
            14: Int_Mul_Const (4)
            15: Slice_Skip_Var_Take_Var
            16: Local_Set (2)
            17: Switch_Jump_If_Equal_Const (2)
              case Blob [4] (0x00000041 | UTF32 "A"): jump 3
              case Blob [4] (0x00000042 | UTF32 "B"): jump 3
            18: Push_Literal (Blob [1] (0x02))
            19: Return
            20: Push_Literal (Blob [1] (0x04))
            21: Return
            """);
    }

    [Fact]
    public void String_left_variable_of_String_dropLeft_compiles_to_fused_slice()
    {
        const string ElmModuleText =
            """
            module Test exposing (..)


            testFunction takeCount skipCount source =
                String.left takeCount (String.dropLeft skipCount source)
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
            bacd3d53 (8):
            0: Local_Get (2)
            1: Local_Get (1)
            2: Int_Mul_Const (4)
            3: Local_Get (0)
            4: Int_Mul_Const (4)
            5: Slice_Skip_Var_Take_Var
            6: Build_List_With_Prefix (2 , 1)
              Blob [52] (0x0000003c00000043000000680000006f0000006900000063000000650000005f... | UTF32 "\u003CChoice_Type\u003E")
              Blob [24] (0x000000530000007400000072000000690000006e00000067 | UTF32 "String")
            7: Return
            """);
    }

    [Fact]
    public void String_left_constant_of_String_dropLeft_compiles_to_fused_slice()
    {
        const string ElmModuleText =
            """
            module Test exposing (..)


            testFunction skipCount source =
                String.left 3 (String.dropLeft skipCount source)
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
            081793dc (6):
            0: Local_Get (1)
            1: Local_Get (0)
            2: Int_Mul_Const (4)
            3: Slice_Skip_Var_Take_Const (12)
            4: Build_List_With_Prefix (2 , 1)
              Blob [52] (0x0000003c00000043000000680000006f0000006900000063000000650000005f... | UTF32 "\u003CChoice_Type\u003E")
              Blob [24] (0x000000530000007400000072000000690000006e00000067 | UTF32 "String")
            5: Return
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
