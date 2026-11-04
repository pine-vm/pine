using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Testing;
using Pine.Core.Files;
using Pine.Core.Functional;
using Pine.Core.IO;
using System.Diagnostics;
using System.Reflection;
using System.Runtime;
using System.Runtime.InteropServices;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using Concrete = Pine.Core.Elm.Syntax.Concrete;

namespace ElmReviewProfiling;

internal static class WarmRerun
{
    private const string WorkloadRevision = "73dfa9a03352a19fde51ffe3e6ea02bdb0f7010f";

    private const string OriginalExpression =
        "\\res -> qualifiedToString (qualify wrapper.wrap.fn res) ++ \" []\"";

    private static readonly JsonSerializerOptions s_json = new() { WriteIndented = true };

    private static readonly string[] s_failurePath =
        ["Task", "Task.sequence", "should replace Task.sequence [] by Task.succeed []"];

    public static int Run(
        string checkout, string outputDirectory, int rounds, bool waitForProfiler,
        bool analyzeDependencies, bool offline, bool languageServer)
    {
        if (languageServer && (analyzeDependencies || !offline))
            throw new ArgumentException("--language-server cannot be combined with --dependencies or --online.");

        checkout = Path.TrimEndingDirectorySeparator(Path.GetFullPath(checkout));
        outputDirectory = Path.TrimEndingDirectorySeparator(Path.GetFullPath(outputDirectory));
        var comparison = OperatingSystem.IsWindows() ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;

        if (outputDirectory.Equals(checkout, comparison) ||
            outputDirectory.StartsWith(checkout + Path.DirectorySeparatorChar, comparison))
        {
            throw new ArgumentException("Write artifacts outside the workload checkout.");
        }

        if (Directory.Exists(outputDirectory) && Directory.EnumerateFileSystemEntries(outputDirectory).Any())
            throw new ArgumentException("Use a new or empty output directory; existing artifacts are not overwritten.");

        if (Git(checkout, "rev-parse", "HEAD") != WorkloadRevision)
            throw new ArgumentException("Expected elm-review checkout at " + WorkloadRevision);

        var editPath = Path.Combine(checkout, "tests", "Simplify.elm");
        var originalBytes = File.ReadAllBytes(editPath);
        var original = File.ReadAllText(editPath);

        if (original.Split(OriginalExpression, StringSplitOptions.None).Length != 2)
            throw new InvalidOperationException("Expected exactly one unmodified rule mutation site in " + editPath);

        var exposure =
            (typeof(ElmTestRunner).GetMethod("IsDeclarationExposed", BindingFlags.Static | BindingFlags.NonPublic)
            ?? throw new InvalidOperationException("Update profiling root enumeration for this ElmTestRunner version."))
            .CreateDelegate<Func<Concrete.Module, string, bool>>();

        Directory.CreateDirectory(outputDirectory);
        var coreAssembly = typeof(ElmTestRunner).Assembly;
        var repository = FindRepository(AppContext.BaseDirectory);
        Save("metadata.json", new
        {
            StartedUtc = DateTime.UtcNow,
            ProcessId = Environment.ProcessId,
            PineRevision = Git(repository, "rev-parse", "HEAD"),
            PineWorkingTree = Git(repository, "status", "--porcelain"),
            WorkloadRevision,
            WorkloadWorkingTree = Git(checkout, "status", "--porcelain"),
            Runtime = RuntimeInformation.FrameworkDescription,
            OS = RuntimeInformation.OSDescription,
            RuntimeInformation.ProcessArchitecture,
            Environment.ProcessorCount,
            GCSettings.IsServerGC,
            CoreAssemblySha256 = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(coreAssembly.Location))),
            CoreModuleVersionId = coreAssembly.ManifestModule.ModuleVersionId,
            Workers = 1,
            Seed = 12345,
            FuzzRuns = 100,
            Rounds = rounds,
            Mode = languageServer ? "language-server" : "core",
            Offline = languageServer ? (bool?)null : offline,
            Cache = "Generational defaults, core native leaves, no background learner"
        });

        using var session = languageServer ? null : new ElmTestRunCacheSession(generationOptions: new());
        var projectUri = new Uri(checkout + Path.DirectorySeparatorChar).AbsoluteUri;
        using var languageService =
            languageServer
            ?
            new LanguageServerTestingService(
                (_, token) =>
                {
                    token.ThrowIfCancellationRequested();
                    var tree = FileTree.FromSetOfFilesWithStringPath(
                        Filesystem.GetFilesFromDirectory(
                            checkout, filterByRelativeName: path => !path.Any(p => p is ".git" or "elm-stuff"))
                        .Select(file => (file.path, file.content)));
                    return Task.FromResult(new ElmTestSourceSnapshot(projectUri, tree, new Dictionary<string, int>()));
                },
                log: Console.Error.WriteLine)
            :
            null;
        using var process = Process.GetCurrentProcess();
        using var cancellation = new CancellationTokenSource();
        var records = new List<object>();
        var lastWritten = originalBytes;
        var restored = true;
        var cancelled = false;
        var settings = new ElmTestExecutionSettings(12345, 100);
        var filter = new ElmTestFilter("TaskTest.elm");

        void Cancel(object? sender, ConsoleCancelEventArgs args)
        {
            args.Cancel = true;
            cancellation.Cancel();
        }

        Console.CancelKeyPress += Cancel;
        Console.WriteLine($"PID {Environment.ProcessId}; artifacts: {outputDirectory}");

        try
        {
            RunRound("warmup", measured: false);
            Console.WriteLine("READY: warmup passed; subsequent rounds are measured edited reruns.");
            Console.Out.Flush();

            if (waitForProfiler)
            {
                var gate = Path.Combine(outputDirectory, "continue");
                Console.WriteLine("Attach diagnostics, then create: " + gate);

                while (!File.Exists(gate))
                {
                    cancellation.Token.ThrowIfCancellationRequested();
                    Thread.Sleep(100);
                }
            }

            for (var index = 1; index <= rounds; index++)
            {
                cancellation.Token.ThrowIfCancellationRequested();

                if (!File.ReadAllBytes(editPath).AsSpan().SequenceEqual(lastWritten))
                    throw new InvalidOperationException("Workload changed concurrently; refusing to overwrite " + editPath);

                var edited = original.Replace(
                    OriginalExpression,
                    OriginalExpression.Replace("\" []\"", "\" [" + new string(' ', index) + "]\""),
                    StringComparison.Ordinal);
                lastWritten = Encoding.UTF8.GetBytes(edited);
                File.WriteAllBytes(editPath, lastWritten);
                RunRound("edit-" + index, measured: true);
            }
        }
        catch (OperationCanceledException) when (cancellation.IsCancellationRequested)
        {
            Console.Error.WriteLine("Profiling cancelled. Completed rounds are retained.");
            cancelled = true;
        }
        finally
        {
            Console.CancelKeyPress -= Cancel;

            if (File.ReadAllBytes(editPath).AsSpan().SequenceEqual(lastWritten))
            {
                File.WriteAllBytes(editPath, originalBytes);
            }
            else
            {
                restored = false;
                Console.Error.WriteLine(
                    "Workload changed concurrently and was NOT restored; inspect " + editPath);
            }

            Save("harness-results.json", records);
        }

        return !restored ? 1 : cancelled ? 130 : 0;

        void RunRound(string label, bool measured)
        {
            var phases = new List<object>();
            var totalWatch = Stopwatch.StartNew();
            Console.WriteLine($"BEGIN {label} {DateTime.UtcNow:O}");
            if (languageService is not null)
            {
                var inventory = Measure("lsp-discover", () => languageService.DiscoverAsync(
                    new(projectUri), cancellation.Token).GetAwaiter().GetResult());
                if (!inventory.Complete)
                    throw new InvalidOperationException("Language-service discovery failed: " + inventory.Error);

                var protocolItems = inventory.Items ??
                    throw new InvalidOperationException("Complete language-service inventory has no items.");
                var selectedItems = protocolItems.Where(item =>
                    item.Selection == ElmTestDiscoverySelection.Included &&
                    item.Kind is ElmTestDiscoveryItemKind.Unit or ElmTestDiscoveryItemKind.Fuzz &&
                    filter.Matches(new(item.FilePath, item.DescriptionPath, item.Label))).ToArray();
                if (selectedItems.Length != 79)
                    throw new InvalidOperationException($"Expected 79 selected tests, got {selectedItems.Length}.");

                var run = Measure("lsp-run", () => languageService.RunAsync(
                    new(projectUri, inventory.Version, label,
                        Include: selectedItems.Select(item => item.Id).ToArray(),
                        Seed: settings.Seed, FuzzRuns: settings.FuzzRuns, DiscoveryId: inventory.DiscoveryId),
                    cancellation.Token).GetAwaiter().GetResult());
                var failed = run.Results.Where(result => result.State != "passed").ToArray();
                if (run.Status != "completed" || run.Results.Count != 79 || failed.Length != (measured ? 1 : 0) ||
                    failed.Any(result =>
                        result.State != "failed" ||
                        !selectedItems.Where(item => item.Id == result.TestId).Any(item =>
                            item.DescriptionPath.Append(item.Label).SequenceEqual(s_failurePath))))
                {
                    throw new InvalidOperationException("Unexpected language-service outcomes: " +
                        JsonSerializer.Serialize(run));
                }

                var empty = Measure("lsp-empty-selection", () => languageService.RunAsync(
                    new(projectUri, inventory.Version, label + "-empty", Include: [],
                        Seed: settings.Seed + 1, FuzzRuns: settings.FuzzRuns, DiscoveryId: inventory.DiscoveryId),
                    cancellation.Token).GetAwaiter().GetResult());
                if (empty.Status != "completed" || empty.Results.Count != 0)
                    throw new InvalidOperationException("Unexpected empty-selection result: " + JsonSerializer.Serialize(empty));

                records.Add(new
                {
                    Label = label,
                    Measured = measured,
                    Mode = "language-server",
                    WallMs = totalWatch.Elapsed.TotalMilliseconds,
                    Phases = phases,
                    Items = protocolItems.Count,
                    Selected = selectedItems.Length,
                    Failures = failed,
                    inventory.Version,
                    inventory.ResolutionFingerprint,
                    inventory.DiscoveryId
                });
                Save("harness-results.json", records);
                Console.WriteLine("RESULT " + JsonSerializer.Serialize(records[^1]));
                Console.WriteLine($"END {label} {DateTime.UtcNow:O}");
                return;
            }

            var coreSession = session ?? throw new InvalidOperationException("No core cache session.");
            var before = coreSession.GetCacheStatisticsSnapshot();
            var (build, roots) = Measure("load-resolve-parse", LoadBuild);
            var discovery =
                Measure("discover", () =>
                    ElmTestDiscovery.DiscoverTests(build, roots, cancellation.Token, settings));
            var selected =
                discovery.Items.Where(item =>
                    item.Selection == ElmTestDiscoverySelection.Included &&
                    item.Kind is ElmTestDiscoveryItemKind.Unit or ElmTestDiscoveryItemKind.Fuzz &&
                    filter.Matches(item.ListedTest, item.DiscoveryIndex ?? 0)).ToArray();

            if (selected.Length != 79)
                throw new InvalidOperationException($"Expected 79 selected tests, got {selected.Length}.");

            var entries = Measure("compile-selected-closures", () =>
            {
                using var invocation = coreSession.BeginCompilationInvocation();
                return discovery.PrepareExecution(selected.Select(item => item.Id),
                    cancellationToken: cancellation.Token, compilationCaches: coreSession.CompilationCaches);
            });
            var completed = Measure("execute", () =>
                selected.Select(item =>
                    ElmTestRunner.ExecuteDiscoveredTest(
                        item, entries[item.Id], coreSession, cancellation.Token)).ToArray());
            var failures = completed.Where(test => test.Kind != CompletedTestKind.Passed).ToArray();

            if (failures.Length != (measured ? 1 : 0) ||
                failures.Any(test =>
                    test.Kind != CompletedTestKind.Failed || test.EvaluationError is not null ||
                    !test.Path.SequenceEqual(s_failurePath)))
            {
                throw new InvalidOperationException(
                    "Unexpected test outcomes: " + JsonSerializer.Serialize(
                        failures.Select(test => new { test.Path, test.Kind, test.EvaluationError })));
            }

            var after = coreSession.GetCacheStatisticsSnapshot();
            records.Add(new
            {
                Label = label,
                Measured = measured,
                Utc = DateTime.UtcNow,
                WallMs = totalWatch.Elapsed.TotalMilliseconds,
                Phases = phases,
                InterpreterPreparationMs = discovery.InterpreterPreparationDuration.TotalMilliseconds,
                DiscoveryMs = discovery.DiscoveryDuration.TotalMilliseconds,
                Roots = roots.Length,
                Modules = build.CompilerModuleSyntax.Count,
                Items = discovery.Items.Length,
                Selected = selected.Length,
                Failures = failures.Select(test => new
                {
                    test.Path,
                    test.Kind,
                    Message = test.Failure is MessageFailure message ? message.Message : test.Failure?.ToString()
                }).ToArray(),
                ParseEntries = coreSession.SourceParsing.Count,
                CanonicalCacheEntries = coreSession.DeclarationCanonicalization.Count,
                OperatorCacheEntries = coreSession.CompilationCaches.OperatorLifetime?.Count,
                SemanticCacheEntries = coreSession.CompilationCaches.SemanticLifetime?.Count,
                CacheBefore = new { before.Totals, before.SharedCacheGenerations },
                CacheAfter = new { after.Totals, after.SharedCacheGenerations },
                VmReducedEntries = coreSession.VmCaches.ReducedExpressions.Count,
                build.Resolution.Fingerprint,
                Packages = build.Resolution.Packages.OrderBy(pair => pair.Key, StringComparer.Ordinal).Select(pair => new
                {
                    Name = pair.Key,
                    pair.Value.Identity.Version,
                    pair.Value.SubstitutionImplementationId
                }).ToArray(),
                DependencyAnalysis = analyzeDependencies ? AnalyzeDependencies(discovery, roots) : null
            });
            Save("harness-results.json", records);
            Console.WriteLine("RESULT " + JsonSerializer.Serialize(records[^1]));
            Console.WriteLine($"END {label} {DateTime.UtcNow:O}");

            T Measure<T>(string phase, Func<T> run)
            {
                cancellation.Token.ThrowIfCancellationRequested();
                process.Refresh();
                var cpu = process.TotalProcessorTime;
                var allocated = GC.GetTotalAllocatedBytes();
                var collections = Enumerable.Range(0, 3).Select(GC.CollectionCount).ToArray();
                var pause = GC.GetTotalPauseDuration();
                Console.WriteLine($"PHASE {label} {phase} {DateTime.UtcNow:O}");
                var watch = Stopwatch.StartNew();
                var value = run();
                watch.Stop();
                process.Refresh();
                var stats = new
                {
                    Name = phase,
                    WallMs = watch.Elapsed.TotalMilliseconds,
                    CpuMs = (process.TotalProcessorTime - cpu).TotalMilliseconds,
                    AllocatedBytes = GC.GetTotalAllocatedBytes() - allocated,
                    GcCounts = Enumerable.Range(0, 3).Select(g => GC.CollectionCount(g) - collections[g]).ToArray(),
                    GcPauseMs = (GC.GetTotalPauseDuration() - pause).TotalMilliseconds,
                    WorkingSet = process.WorkingSet64,
                    ManagedHeap = GC.GetTotalMemory(false)
                };
                phases.Add(stats);
                Console.WriteLine("PHASE-END " + JsonSerializer.Serialize(stats));
                return value;
            }
        }

        (ElmResolvedBuild Build, DeclQualifiedName[] Roots) LoadBuild()
        {
            var coreSession = session ?? throw new InvalidOperationException("No core cache session.");
            var files = Filesystem.GetFilesFromDirectory(
                checkout, filterByRelativeName: path => !path.Any(p => p is ".git" or "elm-stuff"))
                .Select(file => (file.path, file.content)).ToArray();
            var tree = FileTree.FromSetOfFilesWithStringPath(files);
            var nested = files.Where(file => file.path.Count > 1 && file.path[^1] == "elm.json")
                .Select(file => file.path.Take(file.path.Count - 1).ToArray()).ToArray();
            var modules = files.Where(file =>
                file.path.Count > 1 && file.path[0] == "tests" &&
                file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) &&
                !nested.Any(path => file.path.Take(path.Length).SequenceEqual(path)))
                .Select(file =>
                {
                    var text = Encoding.UTF8.GetString(file.content.Span);

                    if (!coreSession.SourceParsing.TryGetValue(text, out var result))
                    {
                        result = ElmSyntaxParser.ParseModuleText(text);

                        if (result.IsOkOrNull() is not null)
                            coreSession.SourceParsing[text] = result;
                    }

                    var parsed = result.Extract(error => throw new InvalidOperationException(error.ToString()));
                    var names = parsed.Declarations.Select(declaration => declaration.Value)
                        .OfType<Concrete.Declaration.FunctionDeclaration>()
                        .Where(declaration =>
                            declaration.Function.Declaration.Value.Arguments.Count == 0 &&
                            exposure(parsed.ModuleDefinition.Value, declaration.Function.Declaration.Value.Name.Value))
                        .Select(declaration => declaration.Function.Declaration.Value.Name.Value).ToArray();
                    return (file.path, parsed, names);
                })
                .OrderBy(module => string.Join('.',
                    Concrete.Module.GetModuleName(module.parsed.ModuleDefinition.Value).Value),
                    StringComparer.Ordinal).ToArray();
            var build = ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                tree, ["elm.json"], modules.Select(module => module.path).ToArray(),
                ElmTestRunner.DefaultResolutionConfiguration.Value with { Offline = offline },
                provider: null, cancellationToken: cancellation.Token, projectDirectory: checkout,
                sourceParseCache: coreSession.SourceParsing).GetAwaiter().GetResult();
            var roots = modules.SelectMany(module => module.names.Select(name =>
                DeclQualifiedName.Create(
                    build.CompilerModuleNames[string.Join('/', module.path)].Split('.'), name))).ToArray();
            return (build, roots);
        }

        void Save(string filename, object value) =>
            File.WriteAllText(Path.Combine(outputDirectory, filename), JsonSerializer.Serialize(value, s_json));
    }

    private static object AnalyzeDependencies(ElmTestDiscoveryResult discovery, DeclQualifiedName[] roots)
    {
        // Optional introspection, not a production dependency on the current closure layout.
        var property = typeof(ElmTestDiscoveryResult).GetProperty(
            "CompileExecution", BindingFlags.Instance | BindingFlags.NonPublic);
        var target = (property?.GetValue(discovery) as Delegate)?.Target
            ?? throw new InvalidOperationException("Update --dependencies for the current discovery implementation.");
        var canonical = target.GetType().GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public)
            .Select(field => field.GetValue(target)).OfType<DeclarationCanonicalizationResult>().Single();
        var changed = canonical.Declarations.Keys.Single(name =>
            name.FullName == "Simplify.listOfWrapperSequenceChecks");
        var affected = new HashSet<DeclQualifiedName> { changed };
        bool grew;

        do
        {
            grew = false;

            foreach (var (name, dependencies) in canonical.Dependencies)
            {
                if (dependencies.Any(affected.Contains))
                    grew |= affected.Add(name);
            }
        } while (grew);

        return new
        {
            CanonicalDeclarations = canonical.Declarations.Count,
            Edges = canonical.Dependencies.Sum(pair => pair.Value.Count),
            Changed = changed.FullName,
            ReverseAffectedDeclarations = affected.Count,
            AffectedRoots = roots.Where(affected.Contains).Select(name => name.FullName).ToArray(),
            UnaffectedRoots = roots.Count(name => !affected.Contains(name))
        };
    }

    private static string FindRepository(string directory)
    {
        for (var current = new DirectoryInfo(directory); current is not null; current = current.Parent)
        {
            if (Directory.Exists(Path.Combine(current.FullName, ".git")) ||
                File.Exists(Path.Combine(current.FullName, ".git")))
            {
                return current.FullName;
            }
        }

        throw new InvalidOperationException("Run the profiling tool from its repository build, not a detached copy.");
    }

    private static string Git(string directory, params string[] arguments)
    {
        var start = new ProcessStartInfo("git")
        {
            WorkingDirectory = directory,
            RedirectStandardOutput = true,
            UseShellExecute = false
        };

        foreach (var argument in arguments)
            start.ArgumentList.Add(argument);

        using var process = Process.Start(start) ?? throw new InvalidOperationException("Failed starting git.");
        var output = process.StandardOutput.ReadToEnd();
        process.WaitForExit();

        if (process.ExitCode != 0)
            throw new InvalidOperationException("git failed in " + directory);

        return output.Trim();
    }
}
