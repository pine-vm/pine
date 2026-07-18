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
/// Analog of <c>PrecompiledLeavesEffectivenessTests</c> targeting the
/// <see href="https://github.com/danfishgold/base64-bytes/tree/ee966331d3819f56244145ed485ab13b0dc4f45a">
/// danfishgold/base64-bytes</see> functions <c>Base64.Encode.toBytes</c> and
/// <c>Base64.Decode.fromBytes</c>.
/// <para>
/// Each test exercises a 2 × 2 matrix:
/// <list type="bullet">
/// <item><description>
/// Dimension α — input size: a short sequence (a handful of bytes) vs. a
/// sequence of circa 100 bytes, which forces the recursive Base64 chunking
/// loop to iterate many times.
/// </description></item>
/// <item><description>
/// Dimension β — VM configuration: a VM with an empty precompiled-leaves
/// dictionary vs. a VM whose precompiled-leaves dictionary registers a
/// short-circuit .NET implementation of the Base64 function (keyed by the
/// encoded inner body of that function, gated on the function's captured
/// <c>envFunctions</c>).
/// </description></item>
/// </list>
/// In front of the performance-counter assertions, the tests assert (using
/// <c>Should().Be(</c>) that all four matrix cells return the expected value.
/// They then assert that without the leaf the larger input costs strictly more
/// invocations / loop iterations and instructions, while with the leaf the two
/// input sizes collapse to identical counters (a single short-circuited
/// invocation).
/// </para>
/// </summary>
public class Base64PrecompiledLeavesEffectivenessTests
{
    /// <summary>
    /// Self-contained Elm module whose functions wrap the two danfishgold
    /// base64-bytes entry points under test.
    /// </summary>
    private const string TestModuleText =
        """"
        module Base64PrecompiledLeavesTestModule exposing (..)


        import Base64.Decode
        import Base64.Encode
        import Bytes exposing (Bytes)


        encodeToBytes : String -> Maybe Bytes
        encodeToBytes b64 =
            Base64.Encode.toBytes b64


        decodeFromBytes : Bytes -> Maybe String
        decodeFromBytes bytes =
            Base64.Decode.fromBytes bytes
        """"
        ;

    // ---------- compilation + environment ----------

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.ElmKernelModulesDefault.Value;

                var srcTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value
                    .GetNodeAtPath(["src"])
                    ?? throw new Exception("Did not find src");

                var base64File = srcTree.GetNodeAtPath(["Base64.elm"]);
                var base64DecodeFile = srcTree.GetNodeAtPath(["Base64", "Decode.elm"]);
                var base64EncodeFile = srcTree.GetNodeAtPath(["Base64", "Encode.elm"]);

                var treeWithBase64 = kernelModulesTree;

                if (base64File is not null)
                    treeWithBase64 = treeWithBase64.SetNodeAtPathSorted(["Base64.elm"], base64File);

                if (base64DecodeFile is not null)
                    treeWithBase64 = treeWithBase64.SetNodeAtPathSorted(["Base64", "Decode.elm"], base64DecodeFile);

                if (base64EncodeFile is not null)
                    treeWithBase64 = treeWithBase64.SetNodeAtPathSorted(["Base64", "Encode.elm"], base64EncodeFile);

                var treeWithTest =
                    treeWithBase64.SetNodeAtPathSorted(
                        ["Base64PrecompiledLeavesTestModule.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("Base64PrecompiledLeavesTestModule.elm", StringComparison.OrdinalIgnoreCase))
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
        .First(m => m.moduleName is "Base64PrecompiledLeavesTestModule")
        .moduleContent.FunctionDeclarations[name];

    // ---------- VM construction ----------

    /// <summary>
    /// Builds an intermediate VM with all optimizations disabled (for repeatable
    /// profiling). When <paramref name="enableDefaultPrecompiledLeaves"/> is
    /// <see langword="true"/>, the VM registers the default precompiled leaves from
    /// <see cref="IntermediateVM.SetupVM.DefaultPrecompiledLeaves"/> (which include
    /// the Base64 conversion leaves under test); otherwise it runs with an empty
    /// precompiled-leaves dictionary.
    /// </summary>
    private static Core.Interpreter.IntermediateVM.PineVM CreateVM(
        bool enableDefaultPrecompiledLeaves) =>
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
            enableDefaultPrecompiledLeaves
            ?
            IntermediateVM.SetupVM.DefaultPrecompiledLeaves
            :
            ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null);

    // ---------- helpers ----------

    private static (ElmValue value, PerformanceCounters counters) ApplyUnary(
        PineValue functionValue,
        ElmValue arg,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(functionValue, arg, vm);

    private static (PineValue result, PerformanceCounters report) ApplyUnaryPine(
        PineValue functionValue,
        PineValue arg,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyGenericPineWithProfiling(functionValue, [arg], vm);

    // ---------- tests ----------

    /// <summary>
    /// Analog of <c>Basics_compare_leaf_short_circuits_recursion_in_lt</c> /
    /// <c>Dict_get_leaf_short_circuits_recursion_in_get</c> for
    /// <c>Base64.Encode.toBytes</c>. The short input decodes a handful of bytes;
    /// the large input decodes a circa-100-byte sequence, forcing many iterations
    /// of the recursive base64 chunking loop when the leaf is absent.
    /// </summary>
    [Fact]
    public void Base64_Encode_toBytes_leaf_short_circuits_recursion()
    {
        var functionValue = GetTestFunction("encodeToBytes");

        var vmWithoutLeaves =
            CreateVM(enableDefaultPrecompiledLeaves: false);

        var vmWithLeaves = CreateVM(enableDefaultPrecompiledLeaves: true);

        // Short input: 3 bytes worth of base64.
        var simpleBytes = new byte[] { 77, 97, 110 };

        // Large input: circa 100 bytes worth of base64 (99 bytes here, an exact
        // multiple of 3 so the base64 string has no padding).
        var complexBytes = Enumerable.Range(0, 99).Select(i => (byte)(i * 7 % 256)).ToArray();

        var simpleBase64 =
            ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(Convert.ToBase64String(simpleBytes)));

        var complexBase64 =
            ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(Convert.ToBase64String(complexBytes)));

        // Compare the results at the Pine value level (content-based equality);
        // the Elm value comparison is unsuitable here because the hash code of
        // the byte-payload variant is identity-based rather than content-based.
        var expectedSimple =
            ElmValueEncoding.ElmValueAsPineValue(
                ElmValue.TagInstance("Just", [new ElmValue.ElmBytes(simpleBytes)]));

        var expectedComplex =
            ElmValueEncoding.ElmValueAsPineValue(
                ElmValue.TagInstance("Just", [new ElmValue.ElmBytes(complexBytes)]));

        var simpleNoLeaves = ApplyUnaryPine(functionValue, simpleBase64, vmWithoutLeaves);
        var complexNoLeaves = ApplyUnaryPine(functionValue, complexBase64, vmWithoutLeaves);

        var simpleWithLeaves = ApplyUnaryPine(functionValue, simpleBase64, vmWithLeaves);
        var complexWithLeaves = ApplyUnaryPine(functionValue, complexBase64, vmWithLeaves);

        // All four combinations must agree on the decoded result.
        simpleNoLeaves.result.Should().Be(expectedSimple);
        complexNoLeaves.result.Should().Be(expectedComplex);
        simpleWithLeaves.result.Should().Be(expectedSimple);
        complexWithLeaves.result.Should().Be(expectedComplex);

        // Without leaves: the larger input must show evidence of recursion —
        // strictly more invocations / loop iterations AND strictly more
        // instructions than the short input.
        (complexNoLeaves.report.InvocationCount + complexNoLeaves.report.LoopIterationCount)
            .Should().BeGreaterThan(
            simpleNoLeaves.report.InvocationCount + simpleNoLeaves.report.LoopIterationCount,
            because:
            "without the precompiled leaf the larger base64 input must cost more invocations or loop iterations");

        complexNoLeaves.report.InstructionCount
            .Should().BeGreaterThan(
            simpleNoLeaves.report.InstructionCount,
            because: "without the precompiled leaf the larger base64 input must execute more instructions");

        // With leaves: the precompiled leaf short-circuits the entire recursive
        // walk, so the short and large inputs must show identical counters.
        complexWithLeaves.report.InvocationCount
            .Should().Be(
            simpleWithLeaves.report.InvocationCount,
            because:
            "with the precompiled leaf, both decodings collapse to a single VM-level invocation regardless of input size");

        complexWithLeaves.report.LoopIterationCount
            .Should().Be(
            simpleWithLeaves.report.LoopIterationCount,
            because: "with the precompiled leaf, the loop iteration count must not depend on input size");

        complexWithLeaves.report.InstructionCount
            .Should().Be(
            simpleWithLeaves.report.InstructionCount,
            because: "with the precompiled leaf, the instruction count must not depend on input size");
    }

    /// <summary>
    /// Analog of <c>Basics_compare_leaf_short_circuits_recursion_in_lt</c> /
    /// <c>Dict_get_leaf_short_circuits_recursion_in_get</c> for
    /// <c>Base64.Decode.fromBytes</c>. The short input encodes a handful of bytes;
    /// the large input encodes a circa-100-byte sequence, forcing many iterations
    /// of the recursive base64 decoding loop when the leaf is absent.
    /// </summary>
    [Fact]
    public void Base64_Decode_fromBytes_leaf_short_circuits_recursion()
    {
        var functionValue = GetTestFunction("decodeFromBytes");

        var vmWithoutLeaves =
            CreateVM(enableDefaultPrecompiledLeaves: false);

        var vmWithLeaves = CreateVM(enableDefaultPrecompiledLeaves: true);

        // Short input: a handful of bytes.
        var simpleBytes = new byte[] { 77, 97, 110 };

        // Large input: circa 100 bytes.
        var complexBytes = Enumerable.Range(0, 99).Select(i => (byte)(i * 7 % 256)).ToArray();

        var simpleArg = new ElmValue.ElmBytes(simpleBytes);
        var complexArg = new ElmValue.ElmBytes(complexBytes);

        var expectedSimple =
            ElmValue.TagInstance("Just", [ElmValue.StringInstance(Convert.ToBase64String(simpleBytes))]);

        var expectedComplex =
            ElmValue.TagInstance("Just", [ElmValue.StringInstance(Convert.ToBase64String(complexBytes))]);

        var simpleNoLeaves = ApplyUnary(functionValue, simpleArg, vmWithoutLeaves);
        var complexNoLeaves = ApplyUnary(functionValue, complexArg, vmWithoutLeaves);

        var simpleWithLeaves = ApplyUnary(functionValue, simpleArg, vmWithLeaves);
        var complexWithLeaves = ApplyUnary(functionValue, complexArg, vmWithLeaves);

        // All four combinations must agree on the encoded result.
        simpleNoLeaves.value.Should().Be(expectedSimple);
        complexNoLeaves.value.Should().Be(expectedComplex);
        simpleWithLeaves.value.Should().Be(expectedSimple);
        complexWithLeaves.value.Should().Be(expectedComplex);

        // Without leaves: the larger input must show evidence of recursion —
        // strictly more invocations / loop iterations AND strictly more
        // instructions than the short input.
        (complexNoLeaves.counters.InvocationCount + complexNoLeaves.counters.LoopIterationCount)
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InvocationCount + simpleNoLeaves.counters.LoopIterationCount,
            because:
            "without the precompiled leaf the larger bytes input must cost more invocations or loop iterations");

        complexNoLeaves.counters.InstructionCount
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InstructionCount,
            because: "without the precompiled leaf the larger bytes input must execute more instructions");

        // With leaves: the precompiled leaf short-circuits the entire recursive
        // walk, so the short and large inputs must show identical counters.
        complexWithLeaves.counters.InvocationCount
            .Should().Be(
            simpleWithLeaves.counters.InvocationCount,
            because:
            "with the precompiled leaf, both encodings collapse to a single VM-level invocation regardless of input size");

        complexWithLeaves.counters.LoopIterationCount
            .Should().Be(
            simpleWithLeaves.counters.LoopIterationCount,
            because: "with the precompiled leaf, the loop iteration count must not depend on input size");

        complexWithLeaves.counters.InstructionCount
            .Should().Be(
            simpleWithLeaves.counters.InstructionCount,
            because: "with the precompiled leaf, the instruction count must not depend on input size");
    }
}
