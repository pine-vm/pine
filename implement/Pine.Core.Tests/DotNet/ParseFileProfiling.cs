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
using System.ComponentModel;
using System.Diagnostics;
using System.Diagnostics.Tracing;
using System.IO;
using System.Linq;
using System.Runtime;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.Json;
using System.Threading;
using Xunit;

namespace Pine.Core.Tests.DotNet;

/// <summary>Explicit native-only profiling with trace regions starting after compilation and warmup.</summary>
public class ParseFileProfiling
{
    private sealed record Workload(string Path, int Utf8Bytes, int Lines, PineValue Environment, int Iterations);

    private sealed record Region(
        int Id, string Workload, string Mode, int Iterations, uint ThreadId,
        double WallMilliseconds, double ThreadCpuMilliseconds, double ProcessCpuMilliseconds,
        long AllocatedBytes, int Gen0, int Gen1, int Gen2);

    [Fact(Explicit = true)]
    public void ParseFile_DotNet_profile_regions()
    {
        if (!OperatingSystem.IsWindows())
            throw new PlatformNotSupportedException("This profile records Windows thread CPU time.");

        var output =
            Environment.GetEnvironmentVariable("PINE_PARSEFILE_PROFILE_OUTPUT") ??
            throw new InvalidOperationException("Set PINE_PARSEFILE_PROFILE_OUTPUT to an absolute JSON result path.");

        var ready = Environment.GetEnvironmentVariable("PINE_PARSEFILE_PROFILE_READY");

        if (!Path.IsPathFullyQualified(output) || ready is not null && !Path.IsPathFullyQualified(ready))
            throw new ArgumentException("Profiling output and ready paths must be absolute.");

        using var process = Process.GetCurrentProcess();
        var setup = Stopwatch.StartNew();
        NamesFromCompiledEnv? names = null;

        var (function, compilation) =
            ElmToCSharpCompilationTests.CompileParser(
                "parseFile",
                (environment, cache) => names = NamesFromCompiledEnv.FromCompiledEnvironment(environment, cache));

        var workers =
            compilation.Methods.Select(
                method => new
                {
                    method.Name,
                    method.Function.Index,
                    Names =
                    names!.NamesForDecl(
                        ExpressionEncoding.EncodeExpressionAsValue(method.Function.Expression),
                        method.Function.EnvironmentConstraint ?? PineValueClass.Empty)?.ToArray() ?? [],
                    ParameterPaths = method.Parameters.ParamsPaths
                }).ToArray();

        using var native =
            PineCSharpAssembly.Load(compilation)
            .Extract(errors => throw new InvalidOperationException(string.Join("\n", errors)));

        var setupMilliseconds = setup.Elapsed.TotalMilliseconds;
        var sources = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        (string[] Path, int Iterations)[] paths =
            [
                (["pine-elm-syntax", "src", "ElmSyntax", "Concrete", "Range.elm"], 1500),
                (["pine-elm-syntax", "src", "ElmSyntax", "Concrete", "Expression.elm"], 250),
                (["src", "LanguageService.elm"], 40),
                (["pine-elm-syntax", "src", "ElmSyntax", "Concrete", "Parser", "FromString.elm"], 16)
            ];

        var workloads =
            paths.Select(
                item =>
                {
                    var source =
                        sources.GetNodeAtPath(item.Path) as FileTree.FileNode ??
                        throw new FileNotFoundException(string.Join("/", item.Path));

                    var text = Encoding.UTF8.GetString(source.Bytes.Span);
                    var argument = ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(text));

                    var environment =
                        ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(function, [argument])
                        .Extract(error => throw new InvalidOperationException(error)).environment;

                    return
                        new Workload(
                            string.Join("/", item.Path),
                            source.Bytes.Length,
                            text.Count(c => c == '\n') + 1,
                            environment,
                            item.Iterations);
                }).ToArray();

        PineValueInProcess Parse(Workload workload) =>
            native.Evaluate(PineValueInProcess.Create(workload.Environment), CancellationToken.None) ??
            throw new InvalidOperationException("The native entry rejected the fixed parser environment.");

        var expected = new Dictionary<string, PineValue>();

        foreach (var workload in workloads)
        {
            for (var iteration = 0; iteration < 5; iteration++)
            {
                var value = Parse(workload).Evaluate();

                if (value is not PineValue.ListValue list || list.Items.Length != 3 ||
                    !list.Items.Span[1].Equals(StringEncoding.ValueFromString("Ok")))
                {
                    throw new InvalidOperationException("Parser failed on " + workload.Path);
                }

                expected[workload.Path] = value;
            }
        }

        Thread.Sleep(2000);
        Directory.CreateDirectory(Path.GetDirectoryName(output)!);

        if (ready is not null)
        {
            Directory.CreateDirectory(Path.GetDirectoryName(ready)!);
            File.WriteAllText(ready, JsonSerializer.Serialize(new { process.Id, ReadyUtc = DateTime.UtcNow }));
            var waiting = Stopwatch.StartNew();

            while (!ProfileEvents.Log.IsEnabled())
            {
                if (waiting.Elapsed > TimeSpan.FromMinutes(2))
                    throw new TimeoutException("No profiler enabled Pine-ParseFile-NativeProfile within two minutes.");

                Thread.Sleep(50);
            }

            Thread.Sleep(500);
        }

        var regions = new List<Region>();

        foreach (var materialize in new[] { false, true })
        {
            foreach (var workload in workloads)
            {
                GC.Collect();
                GC.WaitForPendingFinalizers();
                GC.Collect();
                var id = regions.Count;
                var mode = materialize ? "parse+materialize" : "parse-only";
                var threadId = GetCurrentThreadId();
                var gen0 = GC.CollectionCount(0);
                var gen1 = GC.CollectionCount(1);
                var gen2 = GC.CollectionCount(2);
                ProfileEvents.Log.RegionBegin(id);
                var threadCpu = ThreadCpuTicks();
                var processCpu = process.TotalProcessorTime;
                var allocated = GC.GetAllocatedBytesForCurrentThread();
                var started = Stopwatch.GetTimestamp();
                PineValueInProcess? result = null;

                for (var iteration = 0; iteration < workload.Iterations; iteration++)
                {
                    result = Parse(workload);

                    if (materialize)
                        result.Evaluate();

                    GC.KeepAlive(result);
                }

                var elapsed = Stopwatch.GetElapsedTime(started).TotalMilliseconds;
                var allocatedBytes = GC.GetAllocatedBytesForCurrentThread() - allocated;
                var cpuMilliseconds = (ThreadCpuTicks() - threadCpu) / 10000.0;
                var processMilliseconds = (process.TotalProcessorTime - processCpu).TotalMilliseconds;
                ProfileEvents.Log.RegionEnd(id);

                regions.Add(
                    new(
                        id,
                        workload.Path,
                        mode,
                        workload.Iterations,
                        threadId,
                        elapsed,
                        cpuMilliseconds,
                        processMilliseconds,
                        allocatedBytes,
                        GC.CollectionCount(0) - gen0,
                        GC.CollectionCount(1) - gen1,
                        GC.CollectionCount(2) - gen2));

                if (!expected[workload.Path].Equals(result!.Evaluate()))
                    throw new InvalidOperationException("Native result changed in region " + id);

                Console.WriteLine($"Region {id}: {workload.Path} {mode}: {elapsed / workload.Iterations:F3} ms/parse");
            }
        }

        File.WriteAllText(
            output,
            JsonSerializer.Serialize(
                new
                {
                    Runtime = RuntimeInformation.FrameworkDescription,
                    OS = RuntimeInformation.OSDescription,
                    Architecture = RuntimeInformation.ProcessArchitecture.ToString(),
                    GCSettings.IsServerGC,
                    Environment.ProcessorCount,
                    SetupMilliseconds = setupMilliseconds,
                    WarmupIterations = 5,
                    Workloads =
                    workloads.Select(
                        workload => new { workload.Path, workload.Utf8Bytes, workload.Lines, workload.Iterations }),
                    Workers = workers,
                    Regions = regions
                },
                new JsonSerializerOptions { WriteIndented = true }));

        Console.WriteLine("Profile counters written to: " + output);
        ProfileEvents.Log.CaptureFinished();

        if (ready is not null)
            Thread.Sleep(5000);
    }

    private static long ThreadCpuTicks()
    {
        if (!GetThreadTimes(GetCurrentThread(), out _, out _, out var kernel, out var user))
            throw new Win32Exception(Marshal.GetLastWin32Error());

        return kernel + user;
    }

    [DllImport("kernel32.dll")]
    private static extern nint GetCurrentThread();

    [DllImport("kernel32.dll")]
    private static extern uint GetCurrentThreadId();

    [DllImport("kernel32.dll", SetLastError = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    private static extern bool GetThreadTimes(
        nint thread,
        out long creation,
        out long exit,
        out long kernel,
        out long user);

    [EventSource(Name = "Pine-ParseFile-NativeProfile")]
    private sealed class ProfileEvents : EventSource
    {
        public static readonly ProfileEvents Log = new();

        [Event(1)]
        public void RegionBegin(int id) => WriteEvent(1, id);

        [Event(2)]
        public void RegionEnd(int id) => WriteEvent(2, id);

        [Event(3)]
        public void CaptureFinished() => WriteEvent(3);
    }
}
