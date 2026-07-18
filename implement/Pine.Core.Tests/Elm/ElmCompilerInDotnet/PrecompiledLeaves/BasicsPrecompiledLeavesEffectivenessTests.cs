using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

/// <summary>
/// Tests verifying the effectiveness of the <c>Basics</c>-related precompiled leaves
/// (<see cref="CoreBasicsPrecompiledLeaves"/>) on the
/// <see cref="Core.Interpreter.IntermediateVM.PineVM"/>.
/// <para>
/// See <see cref="PrecompiledLeavesEffectivenessTests"/> for the general description of the
/// precompiled-leaves mechanism and the shared test structure this file follows.
/// </para>
/// </summary>
public class BasicsPrecompiledLeavesEffectivenessTests
{
    /// <summary>
    /// Self-contained Elm module exercising <c>Basics.compare</c> (via <c>&lt;</c>),
    /// <c>Basics.eq</c> (via <c>==</c>), <c>Basics.idiv</c> (via <c>//</c>), <c>Basics.gcd</c>
    /// (called qualified, since it is not part of the real <c>elm/core</c> export list), and
    /// <c>Basics.modBy</c>.
    /// </summary>
    private const string TestModuleText =
        """"
        module BasicsPrecompiledLeavesTestModule exposing (..)


        isLessThan : List Int -> List Int -> Bool
        isLessThan a b =
            a < b


        isEqual : List Float -> List Float -> Bool
        isEqual a b =
            a == b


        idivide : Int -> Int -> Int
        idivide a b =
            a // b


        gcdOf : Int -> Int -> Int
        gcdOf a b =
            Basics.gcd a b


        modByOf : Int -> Int -> Int
        modByOf a b =
            modBy a b
        """"
        ;

    // ---------- compilation + environment ----------

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.ElmKernelModulesDefault.Value;

                var treeWithTest =
                    kernelModulesTree.SetNodeAtPathSorted(
                        ["BasicsPrecompiledLeavesTestModule.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals(
                            "BasicsPrecompiledLeavesTestModule.elm",
                            StringComparison.OrdinalIgnoreCase))
                    .Select(b => (IReadOnlyList<string>)b.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        treeWithTest,
                        rootFilePaths: rootFilePaths)
                    .Map(r => r.compiledEnvValue)
                    .Extract(err => throw new Exception("Failed compiling: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing: " + err));
            });

    private static PineValue GetTestFunction(string name) =>
        s_env.Value.Modules
        .First(m => m.moduleName is "BasicsPrecompiledLeavesTestModule")
        .moduleContent.FunctionDeclarations[name];

    // ---------- VM construction ----------

    private static Core.Interpreter.IntermediateVM.PineVM CreateVM(
        IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> precompiledLeaves) =>
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
            precompiledLeaves: precompiledLeaves,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null);

    // ---------- helpers ----------

    private static ElmValue IntList(IEnumerable<long> items) =>
        ElmValue.ListInstance([.. items.Select(i => ElmValue.Integer(i))]);

    /// <summary>
    /// Builds a list of Elm <c>Float</c> values. Used (instead of <see cref="IntList"/>) for the
    /// <c>Basics.eq</c> effectiveness test: the compiler lowers <c>==</c> directly to
    /// <c>Pine_kernel.equal</c> (bypassing <c>Basics.eq</c> and its precompiled leaf entirely)
    /// whenever the operand type statically proves primitive equality suffices, which is the
    /// case for <c>List Int</c>. <c>Float</c> is excluded from that proof (different
    /// numerator/denominator pairs can represent the same value), which is exactly what forces
    /// the general recursive <c>eqDeep</c> path to be exercised.
    /// </summary>
    private static ElmValue FloatList(IEnumerable<double> items) =>
        ElmValue.ListInstance([.. items.Select(i => (ElmValue)ElmValue.ElmFloat.Convert(i))]);

    private static (ElmValue value, PerformanceCounters counters) Apply(
        PineValue functionValue,
        ElmValue arg0,
        ElmValue arg1,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileBinary(
            functionValue,
            arg0,
            arg1,
            vm);

    /// <summary>
    /// Aggregate of the per-area precompiled-leaf dictionaries available from
    /// inside the <c>Pine.Core</c> project, exposed via
    /// <see cref="IntermediateVM.SetupVM.DefaultPrecompiledLeaves"/>.
    /// </summary>
    private static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultPrecompiledLeaves =>
        IntermediateVM.SetupVM.DefaultPrecompiledLeaves;

    // ---------- tests ----------

    /// <summary>
    /// 2 × 2 matrix, mirroring the structure of the analogous test in
    /// <see cref="PrecompiledLeavesEffectivenessTests"/>: simple vs. complex (deep-recursion)
    /// inputs, crossed with an empty precompiled-leaves dictionary vs.
    /// <see cref="CoreBasicsPrecompiledLeaves.DefaultLeaves"/> registered for
    /// <c>Basics.compare</c>.
    /// </summary>
    [Fact]
    public void Basics_compare_leaf_short_circuits_recursion_in_lt()
    {
        var functionValue = GetTestFunction("isLessThan");

        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves =
            CreateVM(DefaultPrecompiledLeaves);

        // Simple inputs: lists differ at position 0, so the recursive
        // comparison terminates immediately.
        var simpleA = IntList([1]);
        var simpleB = IntList([2]);

        // Complex inputs: lists differ only at position 15 of a 30-element
        // list, forcing the recursive comparison to walk through 15 list
        // cells before noticing the difference.
        var complexA = IntList(Enumerable.Range(0, 30).Select(i => (long)i));

        var complexB =
            IntList(
                Enumerable.Range(0, 30)
                .Select(i => i is 15 ? (long)i + 1 : i));

        var simpleNoLeaves = Apply(functionValue, simpleA, simpleB, vmWithoutLeaves);
        var complexNoLeaves = Apply(functionValue, complexA, complexB, vmWithoutLeaves);

        var simpleWithLeaves = Apply(functionValue, simpleA, simpleB, vmWithLeaves);
        var complexWithLeaves = Apply(functionValue, complexA, complexB, vmWithLeaves);

        // All four combinations must agree on the comparison result.
        simpleNoLeaves.value.Should().Be(ElmValue.TrueValue);
        complexNoLeaves.value.Should().Be(ElmValue.TrueValue);
        simpleWithLeaves.value.Should().Be(ElmValue.TrueValue);
        complexWithLeaves.value.Should().Be(ElmValue.TrueValue);

        // Without leaves: the complex case must show evidence of recursion
        // through Basics.compare — strictly more invocations / loop iterations
        // AND strictly more instructions than the simple case.
        (complexNoLeaves.counters.InvocationCount + complexNoLeaves.counters.LoopIterationCount)
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InvocationCount + simpleNoLeaves.counters.LoopIterationCount,
            because:
            "without precompiled leaves the deeper-recursion comparison must cost more invocations or loop iterations");

        complexNoLeaves.counters.InstructionCount
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InstructionCount,
            because: "without precompiled leaves the deeper-recursion comparison must execute more instructions");

        // With leaves: the precompiled leaf for Basics.compare short-circuits
        // the entire recursive walk, so the simple and complex cases must show
        // identical invocation, loop-iteration and instruction counts.
        complexWithLeaves.counters.InvocationCount
            .Should().Be(
            simpleWithLeaves.counters.InvocationCount,
            because:
            "with the precompiled leaf, both comparisons collapse to a single VM-level invocation regardless of input complexity");

        complexWithLeaves.counters.LoopIterationCount
            .Should().Be(
            simpleWithLeaves.counters.LoopIterationCount,
            because: "with the precompiled leaf, the loop iteration count must not depend on input complexity");

        complexWithLeaves.counters.InstructionCount
            .Should().Be(
            simpleWithLeaves.counters.InstructionCount,
            because: "with the precompiled leaf, the instruction count must not depend on input complexity");
    }

    /// <summary>
    /// Same 2 × 2 matrix as <see cref="Basics_compare_leaf_short_circuits_recursion_in_lt"/>,
    /// but for <c>Basics.eq</c> (deep structural equality via <c>==</c>). Both the simple and
    /// complex inputs are UNEQUAL (rather than the complex case being fully equal): a top-level
    /// <c>Pine_kernel.equal [a, b]</c> structural check inside <c>eqDeep</c> would otherwise
    /// short-circuit fully-equal complex lists to <c>True</c> in a single step regardless of
    /// list length, defeating the purpose of the test. By making both cases unequal — the
    /// simple one at position 0, the complex one only at the last position of a 30-element
    /// list — the recursive <c>eqDeep</c>/<c>listsEqualRecursive</c> walk is forced to actually
    /// visit each list cell to prove inequality, so the complex case costs strictly more without
    /// the leaf; with the leaf, both cases collapse to a single short-circuited invocation.
    /// </summary>
    [Fact]
    public void Basics_eq_leaf_short_circuits_recursion_in_eq()
    {
        var functionValue = GetTestFunction("isEqual");

        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves =
            CreateVM(DefaultPrecompiledLeaves);

        // Simple inputs: lists differ at position 0, so the recursive
        // equality check terminates immediately.
        var simpleA = FloatList([1.5]);
        var simpleB = FloatList([2.5]);

        // Complex inputs: 30-element lists, equal everywhere except the last
        // position, forcing the recursive equality check to walk through 29
        // list cells before noticing the difference.
        var complexA = FloatList(Enumerable.Range(0, 30).Select(i => i + 0.5));

        var complexB =
            FloatList(
                Enumerable.Range(0, 30)
                .Select(i => i is 29 ? i + 1.5 : i + 0.5));

        var simpleNoLeaves = Apply(functionValue, simpleA, simpleB, vmWithoutLeaves);
        var complexNoLeaves = Apply(functionValue, complexA, complexB, vmWithoutLeaves);

        var simpleWithLeaves = Apply(functionValue, simpleA, simpleB, vmWithLeaves);
        var complexWithLeaves = Apply(functionValue, complexA, complexB, vmWithLeaves);

        simpleNoLeaves.value.Should().Be(ElmValue.FalseValue);
        complexNoLeaves.value.Should().Be(ElmValue.FalseValue);
        simpleWithLeaves.value.Should().Be(ElmValue.FalseValue);
        complexWithLeaves.value.Should().Be(ElmValue.FalseValue);

        (complexNoLeaves.counters.InvocationCount + complexNoLeaves.counters.LoopIterationCount)
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InvocationCount + simpleNoLeaves.counters.LoopIterationCount,
            because:
            "without precompiled leaves the deeper-recursion equality check must cost more invocations or loop iterations");

        complexNoLeaves.counters.InstructionCount
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InstructionCount,
            because: "without precompiled leaves the deeper-recursion equality check must execute more instructions");

        complexWithLeaves.counters.InvocationCount
            .Should().Be(
            simpleWithLeaves.counters.InvocationCount,
            because:
            "with the precompiled leaf, both equality checks collapse to a single VM-level invocation regardless of input complexity");

        complexWithLeaves.counters.LoopIterationCount
            .Should().Be(
            simpleWithLeaves.counters.LoopIterationCount,
            because: "with the precompiled leaf, the loop iteration count must not depend on input complexity");

        complexWithLeaves.counters.InstructionCount
            .Should().Be(
            simpleWithLeaves.counters.InstructionCount,
            because: "with the precompiled leaf, the instruction count must not depend on input complexity");
    }

    /// <summary>
    /// Same 2 × 2 matrix, but for <c>Basics.idiv</c> (the <c>//</c> operator). Without the leaf,
    /// dividing a much larger dividend costs strictly more invocations/instructions than dividing
    /// a small one (the scaling-by-16 recursive algorithm needs more steps); with the
    /// <c>idivHelper</c> leaf registered, both collapse to a single short-circuited invocation.
    /// </summary>
    [Fact]
    public void Basics_idiv_leaf_short_circuits_recursion()
    {
        var functionValue = GetTestFunction("idivide");

        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves =
            CreateVM(DefaultPrecompiledLeaves);

        var simpleDividend = ElmValue.Integer(7);
        var simpleDivisor = ElmValue.Integer(2);

        var complexDividend = ElmValue.Integer(100_000_000);
        var complexDivisor = ElmValue.Integer(3);

        var simpleNoLeaves = Apply(functionValue, simpleDividend, simpleDivisor, vmWithoutLeaves);
        var complexNoLeaves = Apply(functionValue, complexDividend, complexDivisor, vmWithoutLeaves);

        var simpleWithLeaves = Apply(functionValue, simpleDividend, simpleDivisor, vmWithLeaves);
        var complexWithLeaves = Apply(functionValue, complexDividend, complexDivisor, vmWithLeaves);

        simpleNoLeaves.value.Should().Be(ElmValue.Integer(3));
        complexNoLeaves.value.Should().Be(ElmValue.Integer(33_333_333));
        simpleWithLeaves.value.Should().Be(ElmValue.Integer(3));
        complexWithLeaves.value.Should().Be(ElmValue.Integer(33_333_333));

        (complexNoLeaves.counters.InvocationCount + complexNoLeaves.counters.LoopIterationCount)
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InvocationCount + simpleNoLeaves.counters.LoopIterationCount,
            because:
            "without precompiled leaves dividing a much larger dividend must cost more invocations or loop iterations");

        complexNoLeaves.counters.InstructionCount
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InstructionCount,
            because: "without precompiled leaves dividing a much larger dividend must execute more instructions");

        complexWithLeaves.counters.InvocationCount
            .Should().Be(
            simpleWithLeaves.counters.InvocationCount,
            because:
            "with the idivHelper precompiled leaf, both divisions collapse to a single VM-level invocation regardless of magnitude");

        complexWithLeaves.counters.LoopIterationCount
            .Should().Be(
            simpleWithLeaves.counters.LoopIterationCount,
            because: "with the precompiled leaf, the loop iteration count must not depend on the dividend's magnitude");

        complexWithLeaves.counters.InstructionCount
            .Should().Be(
            simpleWithLeaves.counters.InstructionCount,
            because: "with the precompiled leaf, the instruction count must not depend on the dividend's magnitude");
    }

    /// <summary>
    /// Same 2 × 2 matrix, but for <c>Basics.gcd</c> (called qualified from the test module, since
    /// <c>gcd</c> is not part of the real <c>elm/core</c> export list). Without the leaf,
    /// computing the GCD of two adjacent large Fibonacci numbers (the worst case for the
    /// Euclidean algorithm) costs strictly more invocations/instructions than a small,
    /// few-steps case; with the leaf registered, both collapse to a single short-circuited
    /// invocation.
    /// </summary>
    [Fact]
    public void Basics_gcd_leaf_short_circuits_recursion()
    {
        var functionValue = GetTestFunction("gcdOf");

        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves =
            CreateVM(DefaultPrecompiledLeaves);

        var simpleA = ElmValue.Integer(12);
        var simpleB = ElmValue.Integer(8);

        // Two adjacent large Fibonacci numbers: the worst case for the Euclidean algorithm,
        // requiring many recursive steps (each step reduces the pair by only the minimal
        // amount possible).
        var complexA = ElmValue.Integer(832_040);
        var complexB = ElmValue.Integer(514_229);

        var simpleNoLeaves = Apply(functionValue, simpleA, simpleB, vmWithoutLeaves);
        var complexNoLeaves = Apply(functionValue, complexA, complexB, vmWithoutLeaves);

        var simpleWithLeaves = Apply(functionValue, simpleA, simpleB, vmWithLeaves);
        var complexWithLeaves = Apply(functionValue, complexA, complexB, vmWithLeaves);

        simpleNoLeaves.value.Should().Be(ElmValue.Integer(4));
        complexNoLeaves.value.Should().Be(ElmValue.Integer(1));
        simpleWithLeaves.value.Should().Be(ElmValue.Integer(4));
        complexWithLeaves.value.Should().Be(ElmValue.Integer(1));

        (complexNoLeaves.counters.InvocationCount + complexNoLeaves.counters.LoopIterationCount)
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InvocationCount + simpleNoLeaves.counters.LoopIterationCount,
            because:
            "without precompiled leaves the many-step Euclidean-algorithm case must cost more invocations or loop iterations");

        complexNoLeaves.counters.InstructionCount
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InstructionCount,
            because: "without precompiled leaves the many-step Euclidean-algorithm case must execute more instructions");

        complexWithLeaves.counters.InvocationCount
            .Should().Be(
            simpleWithLeaves.counters.InvocationCount,
            because:
            "with the gcd precompiled leaf, both computations collapse to a single VM-level invocation regardless of the number of Euclidean-algorithm steps");

        complexWithLeaves.counters.LoopIterationCount
            .Should().Be(
            simpleWithLeaves.counters.LoopIterationCount,
            because:
            "with the precompiled leaf, the loop iteration count must not depend on the number of Euclidean-algorithm steps");

        complexWithLeaves.counters.InstructionCount
            .Should().Be(
            simpleWithLeaves.counters.InstructionCount,
            because:
            "with the precompiled leaf, the instruction count must not depend on the number of Euclidean-algorithm steps");
    }

    /// <summary>
    /// Verifies that <c>Basics.modBy</c> transitively benefits from the <c>idivHelper</c>
    /// precompiled leaf even though <c>modBy</c> has no dedicated leaf of its own: its only
    /// variable-cost part is the nested <c>idiv</c> call reached via
    /// <c>modBy -&gt; remainderBy -&gt; idiv</c>. Registering only the <c>idivHelper</c> leaf
    /// (not a modBy- or eq-specific leaf) already makes the performance counters constant across
    /// small and large inputs.
    /// </summary>
    [Fact]
    public void ModBy_transitively_benefits_from_idiv_leaf_short_circuit()
    {
        var functionValue = GetTestFunction("modByOf");

        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithOnlyIdivLeaf =
            CreateVM(
                ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty
                .Add(CoreBasicsPrecompiledLeaves.IdivLeafKey, CoreBasicsPrecompiledLeaves.IdivLeafDelegate));

        var simpleModulus = ElmValue.Integer(2);
        var simpleValue = ElmValue.Integer(7);

        var complexModulus = ElmValue.Integer(3);
        var complexValue = ElmValue.Integer(100_000_000);

        var simpleNoLeaves = Apply(functionValue, simpleModulus, simpleValue, vmWithoutLeaves);
        var complexNoLeaves = Apply(functionValue, complexModulus, complexValue, vmWithoutLeaves);

        var simpleWithIdivLeaf = Apply(functionValue, simpleModulus, simpleValue, vmWithOnlyIdivLeaf);
        var complexWithIdivLeaf = Apply(functionValue, complexModulus, complexValue, vmWithOnlyIdivLeaf);

        simpleNoLeaves.value.Should().Be(ElmValue.Integer(1));
        complexNoLeaves.value.Should().Be(ElmValue.Integer(1));
        simpleWithIdivLeaf.value.Should().Be(ElmValue.Integer(1));
        complexWithIdivLeaf.value.Should().Be(ElmValue.Integer(1));

        // Without any leaves: the larger value must show evidence of recursion through the
        // nested idivHelper — strictly more invocations/loop iterations and instructions.
        (complexNoLeaves.counters.InvocationCount + complexNoLeaves.counters.LoopIterationCount)
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InvocationCount + simpleNoLeaves.counters.LoopIterationCount,
            because:
            "without precompiled leaves modBy's nested idiv recursion must cost more invocations or loop iterations for a larger value");

        complexNoLeaves.counters.InstructionCount
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InstructionCount,
            because:
            "without precompiled leaves modBy's nested idiv recursion must execute more instructions for a larger value");

        // With only the idivHelper leaf registered (no dedicated modBy leaf): the performance
        // counters must already be constant, since modBy's only variable-cost part is the
        // nested idiv call.
        complexWithIdivLeaf.counters.InvocationCount
            .Should().Be(
            simpleWithIdivLeaf.counters.InvocationCount,
            because:
            "modBy's only variable-cost part is the nested idiv call, so the idivHelper leaf alone must make invocation counts constant");

        complexWithIdivLeaf.counters.LoopIterationCount
            .Should().Be(
            simpleWithIdivLeaf.counters.LoopIterationCount,
            because:
            "modBy's only variable-cost part is the nested idiv call, so the idivHelper leaf alone must make loop-iteration counts constant");

        complexWithIdivLeaf.counters.InstructionCount
            .Should().Be(
            simpleWithIdivLeaf.counters.InstructionCount,
            because:
            "modBy's only variable-cost part is the nested idiv call, so the idivHelper leaf alone must make instruction counts constant");
    }
}
