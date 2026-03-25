using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
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
        """";

    private static readonly ElmInteractiveEnvironment.ParsedInteractiveEnvironment s_parsedEnv =
        ElmCompilerTestHelper.CompileElmModules(
            [HelpersModuleText, TestModuleText], disableInlining: true);

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
    /// mechanism adds 40 instructions, 5 ParseAndEval invocations, and 12
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

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 40
            InvocationCount: 5
            BuildListCount: 12
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates overhead from applying a two-argument function parameter.
    /// <c>testApply2Add a b = Helpers.apply2 add a b</c> applies <c>add</c>
    /// through the generic path with two incremental argument applications.
    /// Each additional argument roughly doubles the wrapper overhead compared
    /// to the unary case: 97 instructions and 9 invocations vs. 40 and 5.
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

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 97
            InvocationCount: 9
            BuildListCount: 38
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates compounding overhead when a function parameter is
    /// applied twice. <c>applyTwice increment 10</c> calls <c>increment</c>
    /// through the generic path twice: 52 instructions and 7 invocations
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

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 52
            InvocationCount: 7
            BuildListCount: 14
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates overhead scaling with list length when applying a function
    /// parameter recursively. Mapping <c>increment</c> over a 3-element list
    /// requires 121 instructions and 12 invocations. Each element adds
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

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 121
            InvocationCount: 12
            BuildListCount: 22
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates overhead from calling a closure. <c>makeAdder 5</c>
    /// returns a closure that captures <c>n=5</c>. Applying that closure
    /// requires the generic ParseAndEval path because the closure is a
    /// dynamically created function value: 76 instructions and 7 invocations
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

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 76
            InvocationCount: 7
            BuildListCount: 31
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Demonstrates how overhead compounds when chaining closures through
    /// the generic application path. Two closures applied via
    /// <c>Helpers.apply</c> result in 154 instructions and 14 invocations
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

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 154
            InvocationCount: 14
            BuildListCount: 62
            LoopIterationCount: 0
            """);
    }
}
