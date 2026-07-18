using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
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
/// Tests verifying the effectiveness of precompiled leaves on the
/// <see cref="Core.Interpreter.IntermediateVM.PineVM"/>.
/// <para>
/// A "precompiled leaf" is a .NET delegate registered under the Pine value
/// encoding of an inner function body. When the VM encounters a
/// <c>ParseAndEval</c> whose <c>encoded</c> resolves to a registered key, it
/// invokes the delegate directly instead of stepping through the bytecode for
/// the function body. The expected payoff is that the recursive structure of
/// the function (e.g. the iteration in <c>Basics.compare</c> when walking
/// nested lists) collapses into a single VM-level invocation.
/// </para>
/// <para>
/// The structure of the tests in this file mirrors
/// <c>ApplicationTests/ParserFastTests.cs</c>: Elm module source text embedded
/// as a constant, compiled once via <see cref="Lazy{T}"/>, then exercised
/// through wrapper functions whose result and runtime cost snapshot are
/// asserted on.
/// </para>
/// </summary>
public class PrecompiledLeavesEffectivenessTests
{
    /// <summary>
    /// Self-contained Elm module whose only declared function uses the
    /// <c>(&lt;)</c> comparison operator on two values whose comparison
    /// requires recursion through <c>Basics.compare</c>.
    /// </summary>
    private const string TestModuleText =
        """"
        module PrecompiledLeavesTestModule exposing (..)


        accessField : { r | m : Int } -> Int
        accessField record =
            record.m


        updateField : { r | m : Int } -> { r | m : Int }
        updateField record =
            { record | m = 999 }
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
                        ["PrecompiledLeavesTestModule.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("PrecompiledLeavesTestModule.elm", StringComparison.OrdinalIgnoreCase))
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
        .First(m => m.moduleName is "PrecompiledLeavesTestModule")
        .moduleContent.FunctionDeclarations[name];

    // ---------- VM construction ----------

    /// <summary>
    /// Builds an intermediate VM with all optimizations disabled (for repeatable
    /// profiling) and the supplied precompiled-leaves dictionary.
    /// </summary>
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

    private static (ElmValue value, PerformanceCounters counters) ApplyUnary(
        PineValue functionValue,
        ElmValue arg0,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
            functionValue,
            arg0,
            vm);

    /// <summary>
    /// Aggregate of the per-area precompiled-leaf dictionaries available from
    /// inside the <c>Pine.Core</c> project, exposed via
    /// <see cref="IntermediateVM.SetupVM.DefaultPrecompiledLeaves"/>.
    /// </summary>
    private static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultPrecompiledLeaves =>
        IntermediateVM.SetupVM.DefaultPrecompiledLeaves;

    // ---------- record access / record update leaves ----------

    /// <summary>
    /// Builds an Elm record value <c>{ name0 = value0, ... }</c> with integer field values.
    /// </summary>
    private static ElmValue IntRecord(IEnumerable<(string name, long value)> fields) =>
        new ElmValue.ElmRecord(
            [.. fields.Select(f => (f.name, ElmValue.Integer(f.value)))]);

    /// <summary>
    /// Analog of <see cref="Basics_compare_leaf_short_circuits_recursion_in_lt"/> targeting
    /// the runtime record-access function emitted for a row-polymorphic record parameter.
    /// The wrapper <c>accessField record = record.m</c> has an open record type, so the
    /// compiler emits the runtime field-lookup fallback
    /// (<see cref="RecordRuntime.PineFunctionForRecordAccessAsValue"/>)
    /// instead of a static index-based access. Exercised on the 2 × 2 matrix:
    /// <list type="bullet">
    /// <item><description>
    /// Dimension α — field position: the accessed field near the front of a small record
    /// vs. the same field in the middle of a record with many more fields before it.
    /// </description></item>
    /// <item><description>
    /// Dimension β — VM configuration: VM with an empty precompiled-leaves dictionary vs.
    /// VM with <see cref="DefaultPrecompiledLeaves"/> (which registers the record-access leaf).
    /// </description></item>
    /// </list>
    /// Asserts that without the leaf the middle-of-a-large-record access incurs strictly
    /// more invocations / loop iterations and strictly more instructions than the
    /// near-the-front access, while with the leaf both collapse to identical counters —
    /// i.e. accessing a field in the middle of a record does not become more expensive as
    /// the number of record fields grows.
    /// </summary>
    [Fact]
    public void Record_access_leaf_short_circuits_recursion_in_access()
    {
        var functionValue = GetTestFunction("accessField");

        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves = CreateVM(DefaultPrecompiledLeaves);

        // Simple input: the accessed field "m" sits near the front of the sorted
        // field stream, so the runtime lookup terminates almost immediately.
        var simpleRecord =
            IntRecord([("m", 1), ("n", 2)]);

        // Complex input: 15 fields sort before "m" (names "a00".."a14"), forcing the
        // runtime lookup to walk through 15 field pairs before matching.
        var complexRecord =
            IntRecord(
                [
                ..Enumerable.Range(0, 15).Select(i => ($"a{i:D2}", (long)i)),
                ("m", 1L),
                ..Enumerable.Range(0, 15).Select(i => ($"n{i:D2}", (long)i)),
                ]);

        var simpleNoLeaves = ApplyUnary(functionValue, simpleRecord, vmWithoutLeaves);
        var complexNoLeaves = ApplyUnary(functionValue, complexRecord, vmWithoutLeaves);

        var simpleWithLeaves = ApplyUnary(functionValue, simpleRecord, vmWithLeaves);
        var complexWithLeaves = ApplyUnary(functionValue, complexRecord, vmWithLeaves);

        // All four combinations must agree on the accessed field value.
        var expected = ElmValue.Integer(1);

        simpleNoLeaves.value.Should().Be(expected);
        complexNoLeaves.value.Should().Be(expected);
        simpleWithLeaves.value.Should().Be(expected);
        complexWithLeaves.value.Should().Be(expected);

        // Without leaves: the deeper field position must show evidence of recursion
        // through the runtime lookup — strictly more invocations / loop iterations AND
        // strictly more instructions than the shallow position.
        (complexNoLeaves.counters.InvocationCount + complexNoLeaves.counters.LoopIterationCount)
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InvocationCount + simpleNoLeaves.counters.LoopIterationCount,
            because:
            "without precompiled leaves accessing a field deeper in the record must cost more invocations or loop iterations");

        complexNoLeaves.counters.InstructionCount
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InstructionCount,
            because: "without precompiled leaves accessing a field deeper in the record must execute more instructions");

        // With leaves: the precompiled leaf short-circuits the entire recursive walk, so
        // the shallow and deep field accesses must show identical counters.
        complexWithLeaves.counters.InvocationCount
            .Should().Be(
            simpleWithLeaves.counters.InvocationCount,
            because:
            "with the precompiled leaf, both accesses collapse to a single VM-level invocation regardless of field position");

        complexWithLeaves.counters.LoopIterationCount
            .Should().Be(
            simpleWithLeaves.counters.LoopIterationCount,
            because: "with the precompiled leaf, the loop iteration count must not depend on field position");

        complexWithLeaves.counters.InstructionCount
            .Should().Be(
            simpleWithLeaves.counters.InstructionCount,
            because: "with the precompiled leaf, the instruction count must not depend on field position");
    }

    /// <summary>
    /// Analog of <see cref="Record_access_leaf_short_circuits_recursion_in_access"/>
    /// targeting the runtime record-update function. The wrapper
    /// <c>updateField record = { record | m = 999 }</c> has an open record type, so the
    /// compiler emits the runtime field-update fallback
    /// (<see cref="RecordRuntime.PineFunctionForRecordUpdateAsValue"/>)
    /// which walks every field pair to reconstruct the record. Exercised on the 2 × 2 matrix:
    /// <list type="bullet">
    /// <item><description>
    /// Dimension α — record size: a small record vs. a record with many more fields.
    /// </description></item>
    /// <item><description>
    /// Dimension β — VM configuration: VM with an empty precompiled-leaves dictionary vs.
    /// VM with <see cref="DefaultPrecompiledLeaves"/> (which registers the record-update leaf).
    /// </description></item>
    /// </list>
    /// Asserts that without the leaf the larger record incurs strictly more invocations /
    /// loop iterations and strictly more instructions than the small record, while with the
    /// leaf both collapse to identical counters — i.e. updating a field in the middle of a
    /// record does not become more expensive as the number of record fields grows.
    /// </summary>
    [Fact]
    public void Record_update_leaf_short_circuits_recursion_in_update()
    {
        var functionValue = GetTestFunction("updateField");

        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves = CreateVM(DefaultPrecompiledLeaves);

        // Simple input: a small record so the runtime update reconstructs few fields.
        var simpleRecord =
            IntRecord([("m", 1), ("n", 2)]);

        // Complex input: the updated field "m" sits in the middle of a record with many
        // more fields, forcing the runtime update to walk through every field pair.
        var complexRecord =
            IntRecord(
                [
                ..Enumerable.Range(0, 15).Select(i => ($"a{i:D2}", (long)i)),
                ("m", 1L),
                ..Enumerable.Range(0, 15).Select(i => ($"n{i:D2}", (long)i)),
                ]);

        var simpleNoLeaves = ApplyUnary(functionValue, simpleRecord, vmWithoutLeaves);
        var complexNoLeaves = ApplyUnary(functionValue, complexRecord, vmWithoutLeaves);

        var simpleWithLeaves = ApplyUnary(functionValue, simpleRecord, vmWithLeaves);
        var complexWithLeaves = ApplyUnary(functionValue, complexRecord, vmWithLeaves);

        // The updated records must agree with the expected results.
        var expectedSimple = IntRecord([("m", 999), ("n", 2)]);

        var expectedComplex =
            IntRecord(
                [
                ..Enumerable.Range(0, 15).Select(i => ($"a{i:D2}", (long)i)),
                ("m", 999L),
                ..Enumerable.Range(0, 15).Select(i => ($"n{i:D2}", (long)i)),
                ]);

        simpleNoLeaves.value.Should().Be(expectedSimple);
        complexNoLeaves.value.Should().Be(expectedComplex);
        simpleWithLeaves.value.Should().Be(expectedSimple);
        complexWithLeaves.value.Should().Be(expectedComplex);

        // Without leaves: the larger record must show evidence of recursion through the
        // runtime update — strictly more invocations / loop iterations AND strictly more
        // instructions than the small record.
        (complexNoLeaves.counters.InvocationCount + complexNoLeaves.counters.LoopIterationCount)
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InvocationCount + simpleNoLeaves.counters.LoopIterationCount,
            because:
            "without precompiled leaves updating a field in a larger record must cost more invocations or loop iterations");

        complexNoLeaves.counters.InstructionCount
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InstructionCount,
            because: "without precompiled leaves updating a field in a larger record must execute more instructions");

        // With leaves: the precompiled leaf short-circuits the entire recursive walk, so
        // the small and large record updates must show identical counters.
        complexWithLeaves.counters.InvocationCount
            .Should().Be(
            simpleWithLeaves.counters.InvocationCount,
            because:
            "with the precompiled leaf, both updates collapse to a single VM-level invocation regardless of record size");

        complexWithLeaves.counters.LoopIterationCount
            .Should().Be(
            simpleWithLeaves.counters.LoopIterationCount,
            because: "with the precompiled leaf, the loop iteration count must not depend on record size");

        complexWithLeaves.counters.InstructionCount
            .Should().Be(
            simpleWithLeaves.counters.InstructionCount,
            because: "with the precompiled leaf, the instruction count must not depend on record size");
    }
}
