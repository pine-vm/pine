using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.Testing;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Diagnostics.Tracing;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.Json;
using Xunit;

using Syntax = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

[CollectionDefinition(CompilationProfilingCollection.Name, DisableParallelization = true)]
public sealed class CompilationProfilingCollection
{
    public const string Name = "Elm compilation profiling";
}

/// <summary>An opt-in, compilation-only baseline using the real parser project's test roots and watch-mode caches.</summary>
[Collection(CompilationProfilingCollection.Name)]
public sealed class IncrementalCompilationProfilingTests(ITestOutputHelper output)
{
    private static readonly string[] s_editPath = ["src", "ElmSyntax", "Concrete", "Parser", "StringParsing.elm"];

    [Fact(Explicit = true)]
    public void Elm_in_elm_incremental_compilation_wall_clock_and_trace()
    {
        var project =
            BundledFiles.CompilerSourceContainerFilesDefault.Value.GetNodeAtPath(["pine-elm-syntax"])
            ?? throw new InvalidOperationException("Missing bundled pine-elm-syntax project.");

        var editableFile =
            project.GetNodeAtPath(s_editPath) as FileTree.FileNode
            ?? throw new InvalidOperationException("Missing StringParsing.elm in the profiling project.");

        var original = Encoding.UTF8.GetString(editableFile.Bytes.Span);
        var firstEdit = EditDeclaration(original, "isDigit", "\"9\"", "\":\"");
        var secondEdit = EditDeclaration(firstEdit, "isUpperCharacter", "\"Z\"", "\"[\"");

        var projects =
            new[]
            {
                (Name: "warmup-1", Tree: project, Changed: (string?)null),
                (Name: "warmup-2", Tree: project, Changed: (string?)null),
                (Name: "incremental-1", Tree: project.SetNodeAtPathSorted(s_editPath, FileTree.File(Encoding.UTF8.GetBytes(firstEdit))), Changed: (string?)"isDigit"),
                (Name: "incremental-2", Tree: project.SetNodeAtPathSorted(s_editPath, FileTree.File(Encoding.UTF8.GetBytes(secondEdit))), Changed: (string?)"isUpperCharacter")
            };

        var testPaths =
            project.EnumerateFilesTransitive()
            .Where(file => file.path[0] is "tests" && file.path[^1].EndsWith(".elm", StringComparison.Ordinal))
            .Select(file => file.path).ToArray();

        testPaths.Should().NotBeEmpty();
        var configuration = ElmTestRunner.DefaultResolutionConfiguration.Value with { Offline = true };

        var provider =
            new ElmDelegatePackageProvider(
                (name, version) =>
                throw new InvalidOperationException(
                    $"The profiling fixture must resolve offline from bundled substitutions, not {name} {version}."));

        var session = new ElmTestRunCacheSession(ImmutableDictionary<PineValue, PrecompiledLeaf>.Empty);
        var measurements = new List<RoundMeasurement>();
        CompilationPipelineStageResults? previous = null;
        PineValue? previousEnvironment = null;
        IReadOnlyList<DeclQualifiedName>? previousRoots = null;
        var cancellation = TestContext.Current.CancellationToken;

        foreach (var (phase, tree, changedDeclaration) in projects)
        {
            cancellation.ThrowIfCancellationRequested();
            var canonicalEntriesBefore = session.DeclarationCanonicalization.Count;

            var (build, preparation) =
                Measure(
                    phase,
                    "prepare",
                    () =>
                    ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                        tree,
                        ["elm.json"],
                        testPaths,
                        configuration,
                        provider,
                        cancellation,
                        projectDirectory: null,
                        sourceParseCache: session.SourceParsing)
                    .GetAwaiter().GetResult());

            var roots =
                testPaths.Select(
                    path => DeclQualifiedName.Create(
                        build.CompilerModuleNames[string.Join("/", path)].Split('.'),
                        "suite"))
                .Append(
                    DeclQualifiedName.Create(
                        build.CompilerModuleNames["elm-packages/elm-explorations/test/src/PineTestBridge.elm"].Split(
                            '.'),
                        "prepare"))
                .Order().ToArray();

            var (compiled, compilation) = Measure(phase, "compile", () => Compile(build, roots, session));
            var current = compiled.pipelineStageResults;

            var target =
                changedDeclaration is null
                ?
                null
                :
                DeclQualifiedName.Create(
                    build.CompilerModuleNames[string.Join("/", s_editPath)].Split('.'),
                    changedDeclaration);

            var changedCanonical =
                previous is null
                ?
                current.CanonicalDeclarations.Keys.ToArray()
                :
                [
                .. current.CanonicalDeclarations
                .Where(entry => !entry.Value.Equals(previous.CanonicalDeclarations[entry.Key]))
                .Select(entry => entry.Key)
                ];

            var changedFunctions =
                previous is null
                ?
                current.CompilerProgram.Functions.Count
                :
                current.CompilerProgram.Functions.Count(
                    entry => !entry.Value.Equals(previous.CompilerProgram.Functions[entry.Key]));

            var addedCanonicalEntries = session.DeclarationCanonicalization.Count - canonicalEntriesBefore;
            var changedRoots = roots.Length;

            if (previous is not null)
            {
                roots.Should().Equal(previousRoots!);
                current.CanonicalDeclarations.Keys.Should().BeEquivalentTo(previous.CanonicalDeclarations.Keys);
                current.CompilerProgram.Functions.Keys.Should().BeEquivalentTo(previous.CompilerProgram.Functions.Keys);
                var oldExports = Exports(previousEnvironment!);
                var newExports = Exports(compiled.compiledEnvValue);
                changedRoots = roots.Count(name => !newExports[name].Equals(oldExports[name]));

                if (target is null)
                {
                    changedCanonical.Should().BeEmpty();
                    changedFunctions.Should().Be(0);
                    changedRoots.Should().Be(0);
                    addedCanonicalEntries.Should().Be(0);
                }
                else
                {
                    changedCanonical.Should().Equal([target]);
                    changedFunctions.Should().Be(1);
                    addedCanonicalEntries.Should().Be(1);
                    changedRoots.Should().BeGreaterThan(0).And.BeLessThan(roots.Length);
                }
            }

            var measurement =
                new RoundMeasurement(
                    phase,
                    preparation.TotalMilliseconds,
                    compilation.TotalMilliseconds,
                    current.CanonicalDeclarations.Count,
                    current.CompilerProgram.Functions.Count,
                    changedCanonical.Length,
                    changedFunctions,
                    changedRoots,
                    roots.Length - changedRoots,
                    session.SourceParsing.Count,
                    session.DeclarationCanonicalization.Count,
                    addedCanonicalEntries);

            measurements.Add(measurement);

            output.WriteLine(
                FormattableString.Invariant(
                    $"{phase}: prepare={measurement.PreparationMilliseconds:F3} ms; compile={measurement.CompilationMilliseconds:F3} ms"));

            output.WriteLine(
                $"changed declarations={measurement.ChangedCanonicalDeclarations}; changed functions={measurement.ChangedFunctions}; " +
                $"changed/unchanged roots={measurement.ChangedRoots}/{measurement.UnchangedRoots}; canonical misses={measurement.AddedCanonicalEntries}");

            previous = current;
            previousEnvironment = compiled.compiledEnvValue;
            previousRoots = roots;
        }

        var directory = Path.GetFullPath(Path.Combine("artifacts", "test-logs", "compiler-profiling"));
        Directory.CreateDirectory(directory);

        var reportPath =
            Path.Combine(
                directory,
                $"elm-in-elm-incremental-{DateTime.UtcNow:yyyyMMddTHHmmssfff}-{Environment.ProcessId}.json");

        File.WriteAllText(
            reportPath,
            JsonSerializer.Serialize(
                new
                {
                    Scenario = "elm-in-elm/pine-elm-syntax",
                    BuildConfiguration,
                    Runtime = RuntimeInformation.FrameworkDescription,
                    Architecture = RuntimeInformation.ProcessArchitecture.ToString(),
                    Environment.ProcessorCount,
                    Stopwatch.Frequency,
                    PhaseEventsEnabled = CompilationEvents.Log.IsEnabled(),
                    Rounds = measurements
                },
                new JsonSerializerOptions { WriteIndented = true }));

        output.WriteLine("Compilation timing report written to: " + reportPath);
    }

    private sealed record RoundMeasurement(
        string Phase, double PreparationMilliseconds, double CompilationMilliseconds,
        int CanonicalDeclarations, int Functions, int ChangedCanonicalDeclarations, int ChangedFunctions,
        int ChangedRoots, int UnchangedRoots, int SourceCacheEntries, int CanonicalCacheEntries, int AddedCanonicalEntries);

    private static string BuildConfiguration =>
#if DEBUG
        "Debug";
#else

        "Release";
#endif

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static (PineValue compiledEnvValue, CompilationPipelineStageResults pipelineStageResults) Compile(
        ElmResolvedBuild build, IReadOnlyList<DeclQualifiedName> roots, ElmTestRunCacheSession session) =>
        ElmCompiler.CompileResolvedEnvironment(build, roots, session.SourceParsing, session.DeclarationCanonicalization)
        .Extract(error => throw new InvalidOperationException("Incremental compilation failed: " + error));

    private static (T Value, TimeSpan Elapsed) Measure<T>(string phase, string stage, Func<T> action)
    {
        CompilationEvents.Log.PhaseStart(phase, stage);
        var start = Stopwatch.GetTimestamp();
        var succeeded = false;

        try
        {
            var value = action();
            var elapsed = Stopwatch.GetElapsedTime(start);
            succeeded = true;
            return (value, elapsed);
        }
        finally
        {
            CompilationEvents.Log.PhaseStop(phase, stage, succeeded ? "succeeded" : "failed");
        }
    }

    private static Dictionary<DeclQualifiedName, PineValue> Exports(PineValue environment) =>
        ElmInteractiveEnvironment.ParseInteractiveEnvironment(environment)
        .Extract(error => throw new InvalidOperationException(error)).Modules
        .SelectMany(
            module => module.moduleContent.FunctionDeclarations.Select(
                entry =>
                KeyValuePair.Create(DeclQualifiedName.Create(module.moduleName.Split('.'), entry.Key), entry.Value)))
        .ToDictionary();

    private static string EditDeclaration(string source, string declarationName, string before, string after)
    {
        before.Length.Should().Be(after.Length, "edits must preserve unrelated declaration source ranges");

        var parsed =
            ElmSyntaxParser.ParseModuleText(source).Extract(
                error => throw new InvalidOperationException(error.ToString()));

        var declaration =
            parsed.Declarations.Single(
                node => node.Value is Syntax.Declaration.FunctionDeclaration function &&
                    function.Function.Declaration.Value.Name.Value == declarationName);

        var text = ElmSyntaxSource.Slice(source, declaration.Range);

        text.Split(before, StringSplitOptions.None).Should().HaveCount(
            2,
            "the edit must identify one literal in one declaration");

        var sourceOffset = source.IndexOf(text, StringComparison.Ordinal);
        sourceOffset.Should().BeGreaterThanOrEqualTo(0);

        return
            source[..sourceOffset] + text.Replace(before, after, StringComparison.Ordinal) +
            source[(sourceOffset + text.Length)..];
    }

    [EventSource(Name = "Pine-ElmCompiler-IncrementalCompilation")]
    private sealed class CompilationEvents : EventSource
    {
        public static readonly CompilationEvents Log = new();

        [Event(1, Opcode = EventOpcode.Start)]
        public void PhaseStart(string phase, string stage) => WriteEvent(1, phase, stage);

        [Event(2, Opcode = EventOpcode.Stop)]
        public void PhaseStop(string phase, string stage, string outcome) =>
            WriteEvent(2, phase, stage, outcome);
    }
}
