using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Tests demonstrating the runtime overhead from generic incremental
/// application of function arguments in compiled Elm code.
/// <para>
/// When an Elm function is compiled to Pine, calling it involves
/// wrapper expressions that use <c>ParseAndEval</c>: the function
/// value is an encoded expression that must be parsed and evaluated
/// for each argument. For a function with N parameters applied to
/// N arguments, the caller performs N separate parse-and-eval steps,
/// each one parsing the wrapper expression, building list-based
/// environments, and evaluating through the wrapper before reaching
/// the actual function body. This is the same overhead factor visible
/// at scale in <see cref="ElmParserExpressionTests.Expression_empty_list"/>
/// (89K instructions, 1.6K invocations for parsing "[]").
/// </para>
/// <para>
/// The PineVM's compile-time reduction can eliminate this overhead for
/// simple expressions by evaluating them at compile time. However, for
/// functions that are too complex for the reduction budget (as in the
/// parser), the overhead remains at runtime. These tests use
/// <c>disableReductionInCompilation: true</c> to expose the raw
/// per-call overhead, making the cost structure clearly visible for
/// guiding optimization of the PineVM's internal representations.
/// </para>
/// </summary>
public class FunctionApplicationOverheadTests
{
    private const string HelpersModuleText =
        """"
        module Helpers exposing (..)


        apply : (a -> b) -> a -> b
        apply f x =
            f x


        apply2 : (a -> b -> c) -> a -> b -> c
        apply2 f a b =
            f a b


        applyTwice : (a -> a) -> a -> a
        applyTwice f x =
            f (f x)


        listMap : (a -> b) -> List a -> List b
        listMap f list =
            case list of
                [] ->
                    []

                first :: rest ->
                    f first :: listMap f rest


        listMap_a : (a -> b) -> (a -> b) -> List a -> List b
        listMap_a mapFirst mapSecond list =
            case list of
                [] ->
                    []

                first :: rest ->
                    mapFirst first :: listMap_b mapFirst mapSecond rest


        listMap_b : (a -> b) -> (a -> b) -> List a -> List b
        listMap_b mapFirst mapSecond list =
            case list of
                [] ->
                    []

                first :: rest ->
                    mapSecond first :: listMap_a mapFirst mapSecond rest


        type TaggedFunc a b
            = TaggedFunc (a -> b)


        listMapTagged : TaggedFunc a b -> List a -> List b
        listMapTagged taggedFunc list =
            case list of
                [] ->
                    []

                first :: rest ->
                    case taggedFunc of
                        TaggedFunc f ->
                            f first :: listMapTagged taggedFunc rest


        {-| Maps consecutive triples from a list through a three-argument function.
        Any remaining elements that don't form a complete triple are discarded.
        -}
        listMapTriple : (a -> a -> a -> b) -> List a -> List b
        listMapTriple f list =
            case list of
                a :: b :: c :: rest ->
                    f a b c :: listMapTriple f rest

                _ ->
                    []
        """";

    private const string TestModuleText =
        """"
        module TestModule exposing (..)

        import Helpers


        increment : Int -> Int
        increment n =
            Pine_kernel.int_add [ n, 1 ]


        add : Int -> Int -> Int
        add a b =
            Pine_kernel.int_add [ a, b ]


        {-| Cross-module call to a higher-order function.
        'Helpers.apply' receives 'increment' as a function parameter
        and applies it generically via ParseAndEval.
        -}
        testApplyIncrement : Int -> Int
        testApplyIncrement n =
            Helpers.apply increment n


        {-| Cross-module call to a higher-order function with two arguments.
        'Helpers.apply2' receives 'add' as a function parameter and
        applies it incrementally via two ParseAndEval steps.
        -}
        testApply2Add : Int -> Int -> Int
        testApply2Add a b =
            Helpers.apply2 add a b


        {-| Cross-module call where the function parameter is applied twice.
        -}
        testApplyTwiceIncrement : Int -> Int
        testApplyTwiceIncrement n =
            Helpers.applyTwice increment n


        {-| Cross-module recursive map applying a function parameter
        at each step.
        -}
        testListMapIncrement : List Int -> List Int
        testListMapIncrement list =
            Helpers.listMap increment list


        {-| A closure: 'makeAdder n' returns a function that captures 'n'.
        Calling the returned closure requires the generic ParseAndEval path
        because the closure is a dynamically created function value.
        -}
        makeAdder : Int -> (Int -> Int)
        makeAdder n =
            \x -> Pine_kernel.int_add [ n, x ]


        testMakeAdder : Int -> Int
        testMakeAdder x =
            let
                add5 =
                    makeAdder 5
            in
            add5 x


        {-| Chains two closures via Helpers.apply to compound the overhead.
        -}
        testChainedClosures : Int -> Int
        testChainedClosures x =
            let
                add3 =
                    makeAdder 3

                add7 =
                    makeAdder 7
            in
            Helpers.apply add7 (Helpers.apply add3 x)


        double : Int -> Int
        double n =
            Pine_kernel.int_mul [ n, 2 ]


        {-| Applies the higher-order helper 'Helpers.listMap' twice on the same
        input list, each time with a different function argument ('increment'
        and 'double'). Returns a list containing the two mapped results.
        -}
        testDualListMap : List Int -> List (List Int)
        testDualListMap list =
            [ Helpers.listMap increment list
            , Helpers.listMap double list
            ]


        {-| Mutual recursive map applying alternating function parameters.
        'listMap_a' applies 'mapFirst' to the head and delegates the rest
        to 'listMap_b', which applies 'mapSecond' and calls back to 'listMap_a'.
        -}
        testMutualListMap : List Int -> List Int
        testMutualListMap list =
            Helpers.listMap_a increment double list


        {-| Applies the mutual recursive 'Helpers.listMap_a' twice on the same
        input list, each time with a different pair of function arguments.
        Returns a list containing the two mapped results.
        -}
        testDualMutualListMap : List Int -> List (List Int)
        testDualMutualListMap list =
            [ Helpers.listMap_a increment double list
            , Helpers.listMap_a double increment list
            ]


        {-| Map using a function wrapped in a single-tag choice type.
        The function must be unwrapped (pattern matched) before each application,
        adding overhead from the generic ParseAndEval path on top of the
        tag-unwrapping cost.
        -}
        testListMapTagged : List Int -> List Int
        testListMapTagged list =
            Helpers.listMapTagged (Helpers.TaggedFunc increment) list


        {-| Applies the tagged-function map twice with different wrapped functions.
        -}
        testDualListMapTagged : List Int -> List (List Int)
        testDualListMapTagged list =
            [ Helpers.listMapTagged (Helpers.TaggedFunc increment) list
            , Helpers.listMapTagged (Helpers.TaggedFunc double) list
            ]


        addThree : Int -> Int -> Int -> Int
        addThree a b c =
            Pine_kernel.int_add [ a, b, c ]


        {-| Maps consecutive triples from the input list through a
        three-argument function. Demonstrates how the generic compilation
        overhead compounds with each additional argument application.
        -}
        testListMapTripleAddThree : List Int -> List Int
        testListMapTripleAddThree list =
            Helpers.listMapTriple addThree list
        """";

    private static readonly ElmInteractiveEnvironment.ParsedInteractiveEnvironment s_parsedEnv =
        ElmCompilerTestHelper.CompileElmModules(
            [HelpersModuleText, TestModuleText],
            disableInlining: false).parsedEnv;

    private static PineValue GetTestFunction(string name) =>
        s_parsedEnv.Modules
        .First(m => m.moduleName is "TestModule")
        .moduleContent.FunctionDeclarations[name];

    /// <summary>
    /// VM with compile-time reduction disabled, exposing the raw overhead
    /// from function wrapper parsing and evaluation. The standard profiling
    /// VM can eliminate this overhead for simple cases via compile-time
    /// evaluation, but cannot do so for complex functions like the
    /// elm-syntax parser. Disabling reduction here reveals the per-call
    /// cost structure that optimization should target.
    /// </summary>
    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: _ => { },
            compilationEnvClasses: null,
            disableReductionInCompilation: true,
            selectPrecompiled: null,
            skipInlineForExpression: _ => false,
            enableTailRecursionOptimization: false,
            parseCache: null,
            precompiledLeaves:
            ImmutableDictionary<PineValue, System.Func<PineValue, PineValue?>>.Empty,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null);

    /// <summary>
    /// Demonstrates overhead from applying a function parameter to one argument.
    /// <c>testApplyIncrement n = Helpers.apply increment n</c> ideally needs
    /// ~1 instruction (the int_add inside increment), but the generic wrapper
    /// mechanism adds 19 instructions, 3 ParseAndEval invocations, and 4
    /// list constructions for the environment management.
    /// </summary>
    [Fact]
    public void Apply_function_parameter_unary()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testApplyIncrement"),
                ElmValue.Integer(10),
                s_vm);

        value.Should().Be(ElmValue.Integer(11));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 11
            InvocationCount: 2
            BuildListCount: 1
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates overhead from applying a two-argument function parameter.
    /// <c>testApply2Add a b = Helpers.apply2 add a b</c> applies <c>add</c>
    /// through the generic path with two incremental argument applications.
    /// Each additional argument roughly doubles the wrapper overhead compared
    /// to the unary case: 41 instructions and 5 invocations vs. 19 and 3.
    /// </summary>
    [Fact]
    public void Apply_function_parameter_binary()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileBinary(
                GetTestFunction("testApply2Add"),
                ElmValue.Integer(3),
                ElmValue.Integer(4),
                s_vm);

        value.Should().Be(ElmValue.Integer(7));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 32
            InvocationCount: 4
            BuildListCount: 8
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates compounding overhead when a function parameter is
    /// applied twice. <c>applyTwice increment 10</c> calls <c>increment</c>
    /// through the generic path twice: 27 instructions and 4 invocations
    /// for what amounts to adding 2 to a number.
    /// </summary>
    [Fact]
    public void Apply_function_parameter_twice()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testApplyTwiceIncrement"),
                ElmValue.Integer(10),
                s_vm);

        value.Should().Be(ElmValue.Integer(12));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 18
            InvocationCount: 3
            BuildListCount: 2
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates overhead scaling with list length when applying a function
    /// parameter recursively. Mapping <c>increment</c> over a 3-element list
    /// requires 92 instructions and 9 invocations. Each element adds
    /// a generic function application through ParseAndEval.
    /// </summary>
    [Fact]
    public void List_map_with_function_parameter()
    {
        var inputList =
            ElmValue.ListInstance(
                [ElmValue.Integer(1), ElmValue.Integer(2), ElmValue.Integer(3)]);

        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testListMapIncrement"),
                inputList,
                s_vm);

        value.Should().Be(
            ElmValue.ListInstance(
                [ElmValue.Integer(2), ElmValue.Integer(3), ElmValue.Integer(4)]));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 63
            InvocationCount: 6
            BuildListCount: 5
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates how the overhead from generic function parameter application
    /// compounds with each additional argument. <c>listMapTriple addThree list</c>
    /// takes consecutive triples from a 20-element list and applies a 3-argument
    /// function (<c>addThree a b c = int_add [a, b, c]</c>) to each triple.
    /// Because the generic compilation wraps each argument application in a
    /// separate ParseAndEval step, each triple application incurs roughly triple
    /// the per-element overhead compared to the unary <c>listMap increment</c>
    /// case. With ~7 triples from 20 items (actually 6, since 20/3 = 6 rem 2),
    /// this magnifies the overhead significantly.
    /// <para>
    /// This test serves as a guide for optimization: reducing the per-argument
    /// overhead should produce a proportionally larger improvement here than in
    /// the unary case, making improvements clearly visible in instruction counts.
    /// </para>
    /// <para>
    /// The test also tracks which <see cref="StackFrameInstructions"/> are entered
    /// during execution. The total and unique frame counts reveal how many distinct
    /// compiled instruction sequences the VM traverses. Snapshot assertions on the
    /// first five entered frames expose the concrete instruction sequences, making
    /// the overhead structure visible and guiding optimization.
    /// </para>
    /// </summary>
    [Fact]
    public void List_map_triple_with_function_parameter()
    {
        var enteredFrames = new List<EnteredStackFrame>();

        var vm =
            Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: _ => { },
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves:
                ImmutableDictionary<PineValue, System.Func<PineValue, PineValue?>>.Empty,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                reportEnteredStackFrame:
                (in EnteredStackFrame entered) =>
                enteredFrames.Add(entered));

        var inputList =
            ElmValue.ListInstance(
                [.. Enumerable.Range(1, 20).Select(i => ElmValue.Integer(i))]);

        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testListMapTripleAddThree"),
                inputList,
                vm);

        // 20 items processed in triples: (1+2+3), (4+5+6), (7+8+9), (10+11+12), (13+14+15), (16+17+18)
        // Last 2 items (19,20) are dropped because they don't form a complete triple.
        value.Should().Be(
            ElmValue.ListInstance(
                [
                ElmValue.Integer(6),
                ElmValue.Integer(15),
                ElmValue.Integer(24),
                ElmValue.Integer(33),
                ElmValue.Integer(42),
                ElmValue.Integer(51),
                ]));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 225
            InvocationCount: 9
            BuildListCount: 8
            LoopIterationCount: 0
            """);

        // --- StackFrameInstructions tracking ---

        var totalFrameCount = enteredFrames.Count;

        var uniqueFrameInstructions =
            enteredFrames
            .Select(f => f.Instructions)
            .Distinct()
            .ToList();

        var uniqueFrameCount = uniqueFrameInstructions.Count;

        totalFrameCount.Should().Be(10, "total entered frame count");
        uniqueFrameCount.Should().Be(4, "unique frame instruction count");

        // Snapshot assertions for the first five entered frames.
        enteredFrames.Should().HaveCountGreaterThanOrEqualTo(5);

        // Snapshot assertions for the first five entered frames.
        // Each assertion captures the rendered instruction listing for that frame,
        // making the overhead structure visible and guiding optimization.

        var frame0 =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                enteredFrames[0].Instructions);

        enteredFrames[0].StackFrameDepth.Should().Be(1);

        frame0.Should().Be(
            """
            0: Push_Literal (List [20] (20))
            1: Push_Literal (List [2] (831))
            2: Parse_And_Eval_Binary
            3: Return
            """);

        var frame1 =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                enteredFrames[1].Instructions);

        enteredFrames[1].StackFrameDepth.Should().Be(1);

        frame1.Should().Be(
            """
            0: Push_Literal (List [0] (0))
            1: Local_Get (0)
            2: Build_List (2)
            3: Push_Literal (List [2] (814))
            4: Parse_And_Eval_Binary
            5: Return
            """);

        var frame2 =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                enteredFrames[2].Instructions);

        enteredFrames[2].StackFrameDepth.Should().Be(1);

        frame2.Should().Be(
            """
            0: Push_Literal (List [1] (391))
            1: Local_Get (0)
            2: Build_List (2)
            3: Push_Literal (List [2] (390))
            4: Parse_And_Eval_Binary
            5: Return
            """);

        var frame3 =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                enteredFrames[3].Instructions);

        enteredFrames[3].StackFrameDepth.Should().Be(1);

        frame3.Should().Be(
            """
             0: Local_Get (1)
             1: Skip_Const (1)
             2: Local_Set (2)
             3: Skip_Const (1)
             4: Local_Set (3)
             5: Length
             6: Jump_If_Equal_Const (Blob [2] (0x0400 | int 0) , 5)
             7: Local_Get (2)
             8: Length_Equal_Const (0)
             9: Equal_Binary_Const (Blob [1] (0x02))
            10: Jump_Const (2)
            11: Push_Literal (Blob [1] (0x02))
            12: Jump_If_Equal_Const (Blob [1] (0x04) , 3)
            13: Push_Literal (Blob [1] (0x02))
            14: Jump_Const (4)
            15: Local_Get (1)
            16: Length_Equal_Const (0)
            17: Equal_Binary_Const (Blob [1] (0x02))
            18: Jump_If_Equal_Const (Blob [1] (0x04) , 3)
            19: Push_Literal (List [0] (0))
            20: Return
            21: Local_Get (1)
            22: Head_Generic
            23: Local_Get (1)
            24: Skip_Head_Const (1)
            25: Int_Add_Binary
            26: Local_Get (2)
            27: Skip_Head_Const (1)
            28: Int_Add_Binary
            29: Local_Get (0)
            30: Local_Get (3)
            31: Skip_Const (1)
            32: Build_List (2)
            33: Local_Get (0)
            34: Head_Generic
            35: Parse_And_Eval_Binary
            36: Prepend_List_Items (1)
            37: Return
            """);

        var frame4 =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                enteredFrames[4].Instructions);

        enteredFrames[4].StackFrameDepth.Should().Be(2);

        frame4.Should().Be(
            """
             0: Local_Get (1)
             1: Skip_Const (1)
             2: Local_Set (2)
             3: Skip_Const (1)
             4: Local_Set (3)
             5: Length
             6: Jump_If_Equal_Const (Blob [2] (0x0400 | int 0) , 5)
             7: Local_Get (2)
             8: Length_Equal_Const (0)
             9: Equal_Binary_Const (Blob [1] (0x02))
            10: Jump_Const (2)
            11: Push_Literal (Blob [1] (0x02))
            12: Jump_If_Equal_Const (Blob [1] (0x04) , 3)
            13: Push_Literal (Blob [1] (0x02))
            14: Jump_Const (4)
            15: Local_Get (1)
            16: Length_Equal_Const (0)
            17: Equal_Binary_Const (Blob [1] (0x02))
            18: Jump_If_Equal_Const (Blob [1] (0x04) , 3)
            19: Push_Literal (List [0] (0))
            20: Return
            21: Local_Get (1)
            22: Head_Generic
            23: Local_Get (1)
            24: Skip_Head_Const (1)
            25: Int_Add_Binary
            26: Local_Get (2)
            27: Skip_Head_Const (1)
            28: Int_Add_Binary
            29: Local_Get (0)
            30: Local_Get (3)
            31: Skip_Const (1)
            32: Build_List (2)
            33: Local_Get (0)
            34: Head_Generic
            35: Parse_And_Eval_Binary
            36: Prepend_List_Items (1)
            37: Return
            """);
    }

    /// <summary>
    /// Demonstrates overhead from calling a closure. <c>makeAdder 5</c>
    /// returns a closure that captures <c>n=5</c>. Applying that closure
    /// requires the generic ParseAndEval path because the closure is a
    /// dynamically created function value: 75 instructions and 7 invocations
    /// for adding 5 to a number.
    /// </summary>
    [Fact]
    public void Closure_application()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testMakeAdder"),
                ElmValue.Integer(10),
                s_vm);

        value.Should().Be(ElmValue.Integer(15));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 11
            InvocationCount: 2
            BuildListCount: 1
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates how overhead compounds when chaining closures through
    /// the generic application path. Two closures applied via
    /// <c>Helpers.apply</c> result in 152 instructions and 14 invocations
    /// for adding 10 to a number (3 + 7).
    /// </summary>
    [Fact]
    public void Chained_closures_with_apply()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testChainedClosures"),
                ElmValue.Integer(10),
                s_vm);

        value.Should().Be(ElmValue.Integer(20));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 41
            InvocationCount: 8
            BuildListCount: 5
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates overhead from applying a higher-order helper function twice
    /// with different function arguments. <c>testDualListMap</c> maps a 10-element
    /// list with <c>increment</c> and again with <c>double</c>, returning a list
    /// of two results. Each <c>Helpers.listMap</c> call traverses the full list
    /// using the generic ParseAndEval path for each element's function application,
    /// so the overhead scales with both the list length and the number of distinct
    /// higher-order applications.
    /// </summary>
    [Fact]
    public void Dual_list_map_with_different_functions()
    {
        var inputList =
            ElmValue.ListInstance(
                [.. Enumerable.Range(1, 10).Select(i => ElmValue.Integer(i))]);

        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testDualListMap"),
                inputList,
                s_vm);

        var expectedIncrementedList =
            ElmValue.ListInstance(
                [.. Enumerable.Range(2, 10).Select(i => ElmValue.Integer(i))]);

        var expectedDoubledList =
            ElmValue.ListInstance(
                [.. Enumerable.Range(1, 10).Select(i => ElmValue.Integer(i * 2))]);

        value.Should().Be(
            ElmValue.ListInstance(
                [expectedIncrementedList, expectedDoubledList]));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 330
            InvocationCount: 24
            BuildListCount: 24
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates overhead from mutually recursive higher-order functions.
    /// <c>listMap_a</c> and <c>listMap_b</c> alternate: <c>listMap_a</c> applies
    /// <c>mapFirst</c> to the head and delegates the tail to <c>listMap_b</c>,
    /// which applies <c>mapSecond</c> and calls back to <c>listMap_a</c>.
    /// The function arguments are loop-invariant (passed unchanged through
    /// every recursive step), so this is a candidate for specialization.
    /// </summary>
    [Fact]
    public void Mutual_recursive_list_map_with_function_parameters()
    {
        var inputList =
            ElmValue.ListInstance(
                [ElmValue.Integer(1), ElmValue.Integer(2), ElmValue.Integer(3)]);

        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testMutualListMap"),
                inputList,
                s_vm);

        // listMap_a with increment/double on [1,2,3]:
        // index 0 (listMap_a): increment 1 = 2
        // index 1 (listMap_b): double 2 = 4
        // index 2 (listMap_a): increment 3 = 4
        value.Should().Be(
            ElmValue.ListInstance(
                [ElmValue.Integer(2), ElmValue.Integer(4), ElmValue.Integer(4)]));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 63
            InvocationCount: 6
            BuildListCount: 5
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates overhead from applying a mutually recursive higher-order
    /// helper twice with different function argument pairs.
    /// <c>testDualMutualListMap</c> calls <c>listMap_a</c> twice on a
    /// 10-element list — first with <c>(increment, double)</c> and then
    /// with <c>(double, increment)</c> — returning both results in a list.
    /// </summary>
    [Fact]
    public void Dual_mutual_recursive_list_map_with_different_functions()
    {
        var inputList =
            ElmValue.ListInstance(
                [.. Enumerable.Range(1, 10).Select(i => ElmValue.Integer(i))]);

        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testDualMutualListMap"),
                inputList,
                s_vm);

        // listMap_a with (increment, double) on [1..10]:
        // Even indices (0,2,4,6,8) use increment, odd indices (1,3,5,7,9) use double
        var expectedIncDoubleList =
            ElmValue.ListInstance(
                [.. Enumerable.Range(1, 10).Select((n, i) => ElmValue.Integer(i % 2 == 0 ? n + 1 : n * 2))]);

        // listMap_a with (double, increment) on [1..10]:
        // Even indices use double, odd indices use increment
        var expectedDoubleIncList =
            ElmValue.ListInstance(
                [.. Enumerable.Range(1, 10).Select((n, i) => ElmValue.Integer(i % 2 == 0 ? n * 2 : n + 1))]);

        value.Should().Be(
            ElmValue.ListInstance(
                [expectedIncDoubleList, expectedDoubleIncList]));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 330
            InvocationCount: 24
            BuildListCount: 24
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates overhead from applying a function wrapped in a choice type tag.
    /// <c>listMapTagged</c> receives a <c>TaggedFunc increment</c> value and must
    /// pattern-match to unwrap the function before each application. This adds
    /// the tag-unwrapping cost on top of the generic ParseAndEval overhead for
    /// each element.
    /// </summary>
    [Fact]
    public void Tagged_function_list_map()
    {
        var inputList =
            ElmValue.ListInstance(
                [ElmValue.Integer(1), ElmValue.Integer(2), ElmValue.Integer(3)]);

        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testListMapTagged"),
                inputList,
                s_vm);

        value.Should().Be(
            ElmValue.ListInstance(
                [ElmValue.Integer(2), ElmValue.Integer(3), ElmValue.Integer(4)]));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 63
            InvocationCount: 6
            BuildListCount: 5
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates overhead from applying <c>listMapTagged</c> twice with
    /// different tagged functions on a 10-element list. Each call wraps the
    /// mapping function in a <c>TaggedFunc</c> choice type, so the per-element
    /// overhead includes both the tag unwrapping and the generic function
    /// application through ParseAndEval.
    /// </summary>
    [Fact]
    public void Dual_tagged_function_list_map()
    {
        var inputList =
            ElmValue.ListInstance(
                [.. Enumerable.Range(1, 10).Select(i => ElmValue.Integer(i))]);

        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testDualListMapTagged"),
                inputList,
                s_vm);

        var expectedIncrementedList =
            ElmValue.ListInstance(
                [.. Enumerable.Range(2, 10).Select(i => ElmValue.Integer(i))]);

        var expectedDoubledList =
            ElmValue.ListInstance(
                [.. Enumerable.Range(1, 10).Select(i => ElmValue.Integer(i * 2))]);

        value.Should().Be(
            ElmValue.ListInstance(
                [expectedIncrementedList, expectedDoubledList]));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 330
            InvocationCount: 24
            BuildListCount: 24
            LoopIterationCount: 0
            """);
    }
}
