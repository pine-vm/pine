using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Encodings;
using Pine.Core.Files;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.Json;
using System.Threading;
using Xunit;

using VM = Pine.Core.Runtime.IntermediateVM.PineVM;

namespace Pine.Core.Tests.DotNet;

/// <summary>Opt-in execution-only comparison; not a wall-clock assertion in the ordinary test suite.</summary>
public class ParseFileBenchmarks
{
    private sealed record Workload(string Path, int Utf8Bytes, int Lines, PineValue Environment);

    private sealed record Sample(
        string Workload, string Backend, string Mode, string Phase, int Iterations,
        double Milliseconds, long AllocatedBytes, int Gen0, int Gen1, int Gen2);

    [Fact(Explicit = true)]
    public void ParseFile_PineVM_versus_generated_DotNet()
    {
        const int warmupIterations = 5;
        const int rounds = 9;
        const int iterationsPerSample = 3;

        var setup = Stopwatch.StartNew();
        var (function, compilation) = ElmToCSharpCompilationTests.CompileParser("parseFile");
        var sourceCompilationMilliseconds = setup.Elapsed.TotalMilliseconds;

        using var native =
            PineCSharpAssembly.Load(compilation)
            .Extract(errors => throw new InvalidOperationException(string.Join("\n", errors)));

        var assemblyCompilationMilliseconds = setup.Elapsed.TotalMilliseconds - sourceCompilationMilliseconds;
        var sourceTree = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        string[][] paths =
            [
                ["pine-elm-syntax", "src", "ElmSyntax", "Concrete", "Range.elm"],
                ["pine-elm-syntax", "src", "ElmSyntax", "Concrete", "Expression.elm"],
                ["src", "LanguageService.elm"],
                ["pine-elm-syntax", "src", "ElmSyntax", "Concrete", "Parser", "FromString.elm"]
            ];

        var workloads =
            paths.Select(
                path =>
                {
                    var file =
                        sourceTree.GetNodeAtPath(path) as FileTree.FileNode ??
                        throw new FileNotFoundException("Missing benchmark module: " + string.Join("/", path));

                    var text = Encoding.UTF8.GetString(file.Bytes.Span);
                    var argument = ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(text));

                    var environment =
                        ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(function, [argument])
                        .Extract(error => throw new InvalidOperationException(error)).environment;

                    return
                        new Workload(
                            string.Join("/", path),
                            file.Bytes.Length,
                            text.Count(c => c == '\n') + 1,
                            environment);
                }).ToArray();

        var retained = Core.Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(function.InnerFunction);

        var constraint =
            PineValueClass.Create(
                [new KeyValuePair<IReadOnlyList<int>, PineValue>([0], PineValue.List(function.EnvFunctions))]);

        var vm =
            VM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: VM.EvaluationConfig.Unbounded,
                reportFunctionApplication: null,
                compilationEnvClasses:
                new Dictionary<Core.Runtime.InternalExpression.Expression, IReadOnlyList<PineValueClass>>
                {
                    [retained] = [constraint]
                },
                disableReductionInCompilation: false,
                selectPrecompiled: null,
                skipInlineForExpression: null,
                enableTailRecursionOptimization: true,
                parseCache: new(),
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null);

        var samples = new List<Sample>();
        var expected = new Dictionary<string, PineValue>(StringComparer.Ordinal);

        foreach (var workload in workloads)
        {
            Func<PineValueInProcess> interpreted =
                () =>
                vm.EvaluateExpressionOnCustomStack(
                    function.InnerFunction,
                    workload.Environment,
                    VM.EvaluationConfig.Unbounded)
                .Extract(error => throw new InvalidOperationException(error.ToString())).ReturnValue;

            Func<PineValueInProcess> compiled =
                () =>
                native.Evaluate(PineValueInProcess.Create(workload.Environment), CancellationToken.None) ??
                throw new InvalidOperationException("Native entry rejected the fixed parser environment.");

            Console.WriteLine("First calls: " + workload.Path);
            var before = Measure(workload.Path, "PineVM", interpreted, materialize: false, "first-call", 1, samples);
            var after = Measure(workload.Path, "DotNet", compiled, materialize: false, "first-call", 1, samples);
            var reference = before.Evaluate();
            RequireParsedFile(reference, workload.Path);

            if (!reference.Equals(after.Evaluate()))
                throw new InvalidOperationException("Backend disagreement for " + workload.Path);

            expected.Add(workload.Path, reference);
        }

        foreach (var workload in workloads)
        {
            for (var iteration = 0; iteration < warmupIterations; iteration++)
            {
                vm.EvaluateExpressionOnCustomStack(
                    function.InnerFunction,
                    workload.Environment,
                    VM.EvaluationConfig.Unbounded)
                    .Extract(error => throw new InvalidOperationException(error.ToString())).ReturnValue.Evaluate();

                (native.Evaluate(PineValueInProcess.Create(workload.Environment)) ??
                throw new InvalidOperationException("Native entry rejected the fixed parser environment.")).Evaluate();
            }
        }
        // Allow background tiered compilation time before steady-state sampling.
        Thread.Sleep(2000);

        foreach (var materialize in new[] { false, true })
        {
            foreach (var workload in workloads)
            {
                Func<PineValueInProcess> interpreted =
                    () =>
                    vm.EvaluateExpressionOnCustomStack(
                        function.InnerFunction,
                        workload.Environment,
                        VM.EvaluationConfig.Unbounded)
                    .Extract(error => throw new InvalidOperationException(error.ToString())).ReturnValue;

                Func<PineValueInProcess> compiled =
                    () =>
                    native.Evaluate(PineValueInProcess.Create(workload.Environment)) ??
                    throw new InvalidOperationException("Native entry rejected the fixed parser environment.");

                for (var round = 0; round < rounds; round++)
                {
                    var order =
                        round % 2 == 0
                        ?
                        new[] { ("PineVM", interpreted), ("DotNet", compiled) }
                        :
                        new[] { ("DotNet", compiled), ("PineVM", interpreted) };

                    foreach (var (backend, parse) in order)
                    {
                        var result =
                            Measure(workload.Path, backend, parse, materialize, "warm", iterationsPerSample, samples);

                        if (!expected[workload.Path].Equals(result.Evaluate()))
                            throw new InvalidOperationException("Backend disagreement after warmup: " + workload.Path);
                    }
                }

                Console.WriteLine(
                    "Completed: " + workload.Path + (materialize ? " + materialization" : " (parse only)"));
            }
        }

        var output =
            Path.GetFullPath(
                Path.Combine(
                    "artifacts",
                    "test-logs",
                    "ParseFileBenchmarks",
                    DateTime.UtcNow.ToString("yyyy-MM-ddTHH-mm-ss") + ".json"));

        Directory.CreateDirectory(Path.GetDirectoryName(output)!);

        File.WriteAllText(
            output,
            JsonSerializer.Serialize(
                new
                {
                    Runtime = RuntimeInformation.FrameworkDescription,
                    OS = RuntimeInformation.OSDescription,
                    Architecture = RuntimeInformation.ProcessArchitecture.ToString(),
                    Environment.ProcessorCount,
                    GCSettings.IsServerGC,
                    TieredCompilation =
                    Environment.GetEnvironmentVariable("DOTNET_TieredCompilation") ?? "runtime default",
                    TieredPGO = Environment.GetEnvironmentVariable("DOTNET_TieredPGO") ?? "runtime default",
                    WarmupIterations = warmupIterations,
                    Rounds = rounds,
                    IterationsPerSample = iterationsPerSample,
                    SourceCompilationMilliseconds = sourceCompilationMilliseconds,
                    AssemblyCompilationMilliseconds = assemblyCompilationMilliseconds,
                    GeneratedMethods = compilation.Methods.Count,
                    Workloads = workloads.Select(workload => new { workload.Path, workload.Utf8Bytes, workload.Lines }),
                    Samples = samples
                },
                new JsonSerializerOptions { WriteIndented = true }));

        Console.WriteLine("Benchmark results written to: " + output);

        foreach (var group in samples.Where(sample => sample.Phase == "warm").GroupBy(sample => (sample.Workload, sample.Mode)))
        {
            var interpreted =
                Median(
                    group.Where(sample => sample.Backend == "PineVM").Select(
                        sample => sample.Milliseconds / sample.Iterations));

            var compiled =
                Median(
                    group.Where(sample => sample.Backend == "DotNet").Select(
                        sample => sample.Milliseconds / sample.Iterations));

            Console.WriteLine(
                $"{group.Key}: PineVM {interpreted:F3} ms, .NET {compiled:F3} ms, {interpreted / compiled:F2}x");
        }
    }

    private static PineValueInProcess Measure(
        string workload,
        string backend,
        Func<PineValueInProcess> parse,
        bool materialize,
        string phase,
        int iterations,
        List<Sample> samples)
    {
        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();
        var gen0 = GC.CollectionCount(0);
        var gen1 = GC.CollectionCount(1);
        var gen2 = GC.CollectionCount(2);
        var allocated = GC.GetAllocatedBytesForCurrentThread();
        var started = Stopwatch.GetTimestamp();
        PineValueInProcess? result = null;

        for (var iteration = 0; iteration < iterations; iteration++)
        {
            result = parse();

            if (materialize)
                result.Evaluate();

            GC.KeepAlive(result);
        }

        var elapsed = Stopwatch.GetElapsedTime(started).TotalMilliseconds;
        allocated = GC.GetAllocatedBytesForCurrentThread() - allocated;

        samples.Add(
            new(
                workload,
                backend,
                materialize ? "parse+materialize" : "parse-only",
                phase,
                iterations,
                elapsed,
                allocated,
                GC.CollectionCount(0) - gen0,
                GC.CollectionCount(1) - gen1,
                GC.CollectionCount(2) - gen2));

        return result!;
    }

    private static void RequireParsedFile(PineValue value, string path)
    {
        if (value is not PineValue.ListValue list || list.Items.Length != 3 ||
            !list.Items.Span[1].Equals(StringEncoding.ValueFromString("Ok")))
        {
            throw new InvalidOperationException("Parser failed on benchmark module " + path);
        }
    }

    private static double Median(IEnumerable<double> values)
    {
        var sorted = values.Order().ToArray();
        return sorted[sorted.Length / 2];
    }
}
