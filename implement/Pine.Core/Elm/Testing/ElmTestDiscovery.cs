using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using Pine.Core.Encodings;
using Pine.Core.Files;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Internal;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using Concrete = Pine.Core.Elm.Syntax.Concrete;
using Abstract = Pine.Core.Elm.Syntax.Abstract;
using Interpreter = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;

namespace Pine.Core.Elm.Testing;

/// <summary>
/// Discovers actual Elm test values without compiling to Pine or running assertion/fuzz callbacks.
/// Test construction and runner preparation use the syntax interpreter and the resolved test library.
/// </summary>
public static class ElmTestDiscovery
{
    private static readonly Lazy<Concrete.File> s_nativeBasics = new(() =>
    {
        var file = BundledFiles.ElmKernelModulesDefault.Value.EnumerateFilesTransitive()
            .Single(file => file.path[^1] == "Basics.elm");
        return Parse(Encoding.UTF8.GetString(file.fileContent.Span), "bundled/Basics.elm");
    });

    /// <summary>Discovers a filesystem project, projecting AST paths onto its CST for navigation by test ID.</summary>
    public static ElmTestDiscoveryResult DiscoverTests(
        string appDirectory,
        ElmDependencyResolutionConfiguration? resolutionConfiguration = null,
        IElmPackageProvider? packageProvider = null,
        CancellationToken cancellationToken = default)
    {
        if (!Directory.Exists(appDirectory))
            throw new DirectoryNotFoundException("Elm project directory not found: " + appDirectory);

        var watch = Stopwatch.StartNew();
        var tree = FileTree.FromSetOfFilesWithStringPath(
            Filesystem.GetFilesFromDirectory(appDirectory,
                filterByRelativeName: path => !path.Any(segment => segment is ".git" or "elm-stuff"))
                .Select(file => (file.path, file.content)));

        return DiscoverTests(tree, resolutionConfiguration, packageProvider, cancellationToken, appDirectory)
            with
        { Duration = watch.Elapsed };
    }

    /// <summary>Discovers an in-memory project. Resolution and source errors are reported, not empty successes.</summary>
    public static ElmTestDiscoveryResult DiscoverTests(
        FileTree projectTree,
        ElmDependencyResolutionConfiguration? resolutionConfiguration = null,
        IElmPackageProvider? packageProvider = null,
        CancellationToken cancellationToken = default,
        string? projectDirectory = null)
    {
        cancellationToken.ThrowIfCancellationRequested();
        var watch = Stopwatch.StartNew();
        var files = projectTree.EnumerateFilesTransitive().ToArray();
        var nestedProjects = files.Where(file => file.path.Count > 1 && file.path[^1] == "elm.json")
            .Select(file => file.path.Take(file.path.Count - 1).ToArray()).ToArray();

        var roots = files.Where(file => file.path.Count > 1 && file.path[0] == "tests" &&
                file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) &&
                !file.path.Any(segment => segment is ".git" or "elm-stuff") &&
                !nestedProjects.Any(nested => file.path.Take(nested.Length).SequenceEqual(nested)))
            .Select(file => (file.path, Parsed: Parse(Encoding.UTF8.GetString(file.fileContent.Span), string.Join('/', file.path))))
            .ToArray();

        if (roots.Length == 0)
            return new([], ImmutableDictionary<string, ElmTestSourceLocation>.Empty) { Duration = watch.Elapsed };

        resolutionConfiguration ??= ElmTestRunner.DefaultResolutionConfiguration.Value;
        if (!resolutionConfiguration.IncludeTests)
            throw new ArgumentException("Elm test discovery requires IncludeTests = true.", nameof(resolutionConfiguration));

        var build = ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
            projectTree, ["elm.json"], [.. roots.Select(root => root.path)], resolutionConfiguration,
            packageProvider, cancellationToken, projectDirectory).GetAwaiter().GetResult();

        var declarations = roots.OrderBy(root => ModuleName(root.Parsed), StringComparer.Ordinal)
            .SelectMany(root => root.Parsed.Declarations.Select(declaration => declaration.Value)
                .OfType<Concrete.Declaration.FunctionDeclaration>()
                .Where(declaration => declaration.Function.Declaration.Value.Arguments.Count == 0 &&
                    ElmTestRunner.IsDeclarationExposed(root.Parsed.ModuleDefinition.Value,
                        declaration.Function.Declaration.Value.Name.Value))
                .Select(declaration => DeclQualifiedName.Create(
                    build.CompilerModuleNames[string.Join('/', root.path)].Split('.'),
                    declaration.Function.Declaration.Value.Name.Value)))
            .ToArray();

        var sourcePreparationDuration = watch.Elapsed;
        return DiscoverTests(build, declarations, cancellationToken) with
        {
            SourcePreparationDuration = sourcePreparationDuration,
            Duration = watch.Elapsed
        };
    }

    /// <summary>
    /// Discovers selected zero-argument roots in an already resolved environment.
    /// Only demanded declarations are canonicalized; unrelated declaration bodies are not evaluated.
    /// </summary>
    public static ElmTestDiscoveryResult DiscoverTests(
        ElmResolvedBuild build,
        IReadOnlyList<DeclQualifiedName> testRoots,
        CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();
        if (!build.Resolution.Succeeded)
            throw new ElmDependencyResolutionException(build.Resolution);

        var watch = Stopwatch.StartNew();
        var modules = build.CompilerModuleSyntax;
        if (modules.Count == 0)
            throw new ArgumentException("Test discovery requires resolved compiler module syntax.", nameof(build));

        // The compiler supplies Basics natively; the syntax interpreter needs its source implementation.
        if (!modules.ContainsKey("Basics"))
            modules = modules.Add("Basics", s_nativeBasics.Value);
        var nativeRoots = modules["Basics"].Declarations.Select(node => node.Value)
            .OfType<Concrete.Declaration.FunctionDeclaration>()
            .Select(function => DeclQualifiedName.Create(["Basics"], function.Function.Declaration.Value.Name.Value));

        const string bridgePath = "elm-packages/elm-explorations/test/src/PineTestBridge.elm";
        if (!build.CompilerModuleNames.TryGetValue(bridgePath, out var bridgeModule))
            throw new InvalidOperationException("The configured test substitution does not implement Pine's test discovery bridge.");
        var bridge = DeclQualifiedName.Create(bridgeModule.Split('.'), "prepare");

        var canonical = Canonicalization.CanonicalizeDeclarations(
            [.. modules.Values], [.. testRoots, bridge, .. nativeRoots],
            implicitImports: null, cache: null, canonicalizedDependencies: null,
            preserveOperatorApplications: true)
            .Extract(error => throw new InvalidOperationException("Failed preparing test discovery: " + error));

        var prepared = new Dictionary<DeclQualifiedName, Interpreter.PreparedDeclaration>();
        foreach (var type in modules["Basics"].Declarations.Select(node => node.Value)
            .OfType<Concrete.Declaration.ChoiceTypeDeclaration>())
        {
            prepared.Add(DeclQualifiedName.Create(["Basics"], type.TypeDeclaration.Name.Value),
                Interpreter.PrepareDeclaration(Abstract.ConvertFromConcrete.FromDeclaration(type)));
        }
        var sourceBodies = new Dictionary<DeclQualifiedName, Concrete.Node<Concrete.Expression>>();
        var observers = new Dictionary<Interpreter.PreparedExpression, Action<PineValueInProcess>>(ReferenceEqualityComparer.Instance);
        var valueSources = new Dictionary<PineValueInProcess, SourceSite>(ReferenceEqualityComparer.Instance);

        foreach (var (name, result) in canonical.Declarations)
        {
            cancellationToken.ThrowIfCancellationRequested();
            if (result.Errors.Count > 0)
                throw new InvalidOperationException("Failed canonicalizing test discovery declaration " + name.FullName +
                    ": " + string.Join("; ", result.Errors));

            // Keep the AST shape: lowering/reduction would invalidate its paths into the original CST.
            var declaration = Interpreter.PrepareDeclaration(Abstract.ConvertFromConcrete.FromDeclaration(result.Value.Value));
            prepared.Add(name, declaration);
            if (declaration is not Interpreter.PreparedDeclaration.FunctionDeclaration function)
                continue;

            var moduleName = string.Join('.', name.Namespaces);
            if (!build.CompilerModuleOrigins.TryGetValue(moduleName, out var origin))
            {
                if (moduleName == "Basics")
                    continue;
                throw new InvalidOperationException("Missing module provenance for test discovery: " + moduleName);
            }
            var concreteFunction = modules[moduleName].Declarations.Select(node => node.Value)
                .OfType<Concrete.Declaration.FunctionDeclaration>()
                .Single(candidate => candidate.Function.Declaration.Value.Name.Value == name.DeclName);
            sourceBodies[name] = concreteFunction.Function.Declaration.Value.Expression;

            // Library internals should not steal navigation from the source that constructs a test.
            if (origin is ElmModuleOrigin.Substitution
                { ReplacedPackage.Name: "elm/core" or "elm-explorations/test" or "elm/random" })
                continue;

            foreach (var (expression, path) in Interpreter.EnumerateExpressionPaths(function.Function.Declaration.Expression))
            {
                var site = new SourceSite(name, path);
                observers[expression] = value =>
                {
                    if (TestTag(value) is not null)
                        valueSources.TryAdd(value, site);
                };
            }
        }

        var program = new Interpreter.Prepared(prepared);
        var config = Interpreter.EvaluationConfig.Default with
        {
            ExpressionObservers = observers,
            CancellationToken = cancellationToken
        };
        var interpreterPreparationDuration = watch.Elapsed;
        var tests = new List<(ElmDiscoveredTest Test, SourceSite Source, bool Only)>();
        var hasOnly = false;

        foreach (var root in testRoots)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var value = Interpreter.Interpret(root, [], program, config)
                .Extract(error => throw new InvalidOperationException("Failed constructing Elm tests in " + root.FullName + ": " + error));
            if (TestTag(value) is null)
                continue;

            var origins = CollectSources(value, [], new SourceSite(root, ElmSyntaxExpressionPath.Root), false, false, valueSources);
            var runners = Interpreter.Interpret(bridge,
                [PineValueInProcess.Create(IntegerEncoding.EncodeSignedInteger(100)),
                 PineValueInProcess.Create(IntegerEncoding.EncodeSignedInteger(0)), value],
                program, config with { ExpressionObservers = null })
                .Extract(error => throw new InvalidOperationException("Failed preparing Elm tests in " + root.FullName + ": " + error));

            var tag = ChoiceTag(runners) ?? throw new InvalidOperationException("Invalid test discovery bridge result.");
            if (tag is not ("Plain" or "Only" or "Skipping" or "Invalid"))
                throw new InvalidOperationException("Unsupported test discovery bridge result: " + tag);
            var only = tag == "Only";
            hasOnly |= only;
            var rootOrigin = build.CompilerModuleOrigins[string.Join('.', root.Namespaces)];
            var index = 0;

            if (tag == "Invalid")
            {
                throw new InvalidOperationException("Invalid Elm test preparation in " + root.FullName + ": " + ElmString(runners.GetElementAt(2)));
            }

            var selectedSources = new Queue<LeafSource>(origins.Where(candidate => !candidate.Skipped && (!only || candidate.Only)));
            foreach (var runner in ListItems(runners.GetElementAt(2)))
            {
                cancellationToken.ThrowIfCancellationRequested();
                var labels = ListItems(Field(runner, "labels")).Select(ElmString).Reverse().ToImmutableArray();
                var kind = ElmString(Field(runner, "kind"));
                var ordinal = index++;
                var sourceLabels = labels;
                if (labels.Length == 0)
                    labels = [root.DeclName];

                if (!selectedSources.TryDequeue(out var source) || source.Kind != kind || !source.Labels.SequenceEqual(sourceLabels))
                    throw new InvalidOperationException("Cannot associate discovered test with its construction source: " + root.FullName + "/" + string.Join('/', labels));
                if (kind == "empty")
                    continue;

                var id = "elm-test:" + Uri.EscapeDataString(rootOrigin.SourcePath) + "#" +
                    Uri.EscapeDataString(root.DeclName) + "/" + ordinal;
                tests.Add((new ElmDiscoveredTest(id, new ListedTest(rootOrigin.SourcePath, labels.RemoveAt(labels.Length - 1), labels[^1]), kind, tests.Count + 1),
                    source.Source, only));
            }
            if (selectedSources.Count != 0)
                throw new InvalidOperationException("Test discovery bridge omitted construction sources in " + root.FullName);
        }

        var active = tests.Where(test => !hasOnly || test.Only).ToArray();
        var locations = active.ToImmutableDictionary(test => test.Test.Id, test =>
        {
            var node = test.Source.Path.Resolve(sourceBodies[test.Source.Declaration]) ??
                throw new InvalidOperationException("AST source path does not match its CST: " + test.Source.Declaration.FullName);
            return new ElmTestSourceLocation(
                build.CompilerModuleOrigins[string.Join('.', test.Source.Declaration.Namespaces)],
                test.Source.Declaration.DeclName, test.Source.Path, node.Range);
        }, StringComparer.Ordinal);

        return new([.. active.Select(test => test.Test)], locations)
        {
            InterpreterPreparationDuration = interpreterPreparationDuration,
            DiscoveryDuration = watch.Elapsed - interpreterPreparationDuration,
            Duration = watch.Elapsed
        };
    }

    private sealed record SourceSite(DeclQualifiedName Declaration, ElmSyntaxExpressionPath Path);
    private sealed record LeafSource(ImmutableArray<string> Labels, string Kind, SourceSite Source, bool Only, bool Skipped);

    private static IEnumerable<LeafSource> CollectSources(
        PineValueInProcess value, ImmutableArray<string> labels, SourceSite inherited, bool only, bool skipped,
        IReadOnlyDictionary<PineValueInProcess, SourceSite> sources)
    {
        var source = sources.GetValueOrDefault(value) ?? inherited;
        var tag = TestTag(value) ?? throw new InvalidOperationException("Invalid Elm Test value.");
        switch (tag)
        {
            case "ElmTestVariant__UnitTest":
            case "ElmTestVariant__FuzzTest":
                yield return new(labels, tag == "ElmTestVariant__UnitTest" ? "unit" : "fuzz", source, only, skipped);
                break;
            case "PineTodo":
            case "PineEmptyGroup":
                yield return new(labels.Add(ElmString(value.GetElementAt(2))), tag == "PineTodo" ? "todo" : "empty", source, only, skipped);
                break;
            case "ElmTestVariant__Labeled":
                foreach (var leaf in CollectSources(value.GetElementAt(3), labels.Add(ElmString(value.GetElementAt(2))), source, only, skipped, sources))
                    yield return leaf;
                break;
            case "ElmTestVariant__Skipped":
            case "ElmTestVariant__Only":
            case "PineFuzzCount":
                foreach (var leaf in CollectSources(value.GetElementAt(tag == "PineFuzzCount" ? 3 : 2), labels, source,
                    only || tag == "ElmTestVariant__Only", skipped || tag == "ElmTestVariant__Skipped", sources))
                    yield return leaf;
                break;
            case "ElmTestVariant__Batch":
                foreach (var child in ListItems(value.GetElementAt(2)))
                    foreach (var leaf in CollectSources(child, labels, source, only, skipped, sources))
                        yield return leaf;
                break;
            default:
                throw new InvalidOperationException("Unsupported Elm Test variant: " + tag);
        }
    }

    private static string? TestTag(PineValueInProcess value)
    {
        var tag = ChoiceTag(value);
        return tag is not null && (tag.StartsWith("ElmTestVariant__", StringComparison.Ordinal) ||
            tag is "PineTodo" or "PineEmptyGroup" or "PineFuzzCount") ? tag : null;
    }

    private static string? ChoiceTag(PineValueInProcess value)
    {
        if (value.UnevaluatedStructuralItemsOrNull() is not { Count: >= 2 } items)
            return null;
        if (items[0].EvaluatedOrNull != ElmValue.ElmChoiceTypeTagNameAsValue)
            return null;
        return StringEncoding.StringFromValue(items[1].Evaluate())
            .Extract(error => throw new InvalidOperationException("Invalid choice tag: " + error));
    }

    private static IEnumerable<PineValueInProcess> ListItems(PineValueInProcess list)
    {
        for (var index = 0; index < list.GetLength(); index++)
            yield return list.GetElementAt(index);
    }

    private static PineValueInProcess Field(PineValueInProcess record, string name)
    {
        if (record.GetLength() % 2 != 1 || !ElmValue.IsFlatRecordTypeTag(record.GetElementAt(0).Evaluate()))
            throw new InvalidOperationException("Invalid test runner record.");
        for (var index = 1; index < record.GetLength(); index += 2)
            if (StringEncoding.StringFromValue(record.GetElementAt(index).Evaluate()).Extract(
                error => throw new InvalidOperationException(error)) == name)
                return record.GetElementAt(index + 1);
        throw new InvalidOperationException("Missing test runner field: " + name);
    }

    private static string ElmString(PineValueInProcess value) =>
        ElmValueEncoding.PineValueAsElmValue(value.Evaluate(), null, null)
            .Extract(error => throw new InvalidOperationException("Invalid test label: " + error)) is ElmValue.ElmString text
            ? text.Value : throw new InvalidOperationException("Expected an Elm string in test discovery.");

    private static Concrete.File Parse(string text, string path)
    {
        var parsed = ElmSyntaxParser.ParseModuleText(text)
            .Extract(error => throw new InvalidOperationException("Failed parsing Elm test module '" + path + "': " + error));
        if (parsed.IncompleteDeclarations.Count > 0 || parsed.AdditionalParseErrors.Count > 0)
            throw new InvalidOperationException("Incomplete Elm syntax in test module '" + path + "'.");
        return parsed;
    }

    private static string ModuleName(Concrete.File file) =>
        string.Join('.', Concrete.Module.GetModuleName(file.ModuleDefinition.Value).Value);
}

/// <summary>A runnable leaf discovered without compilation. IDs are project-relative root/leaf ordinals, not labels.</summary>
public sealed record ElmDiscoveredTest(string Id, ListedTest ListedTest, string Kind, int DiscoveryIndex);

/// <summary>Source provenance and one-based, end-exclusive Elm syntax range of a test's construction expression.</summary>
public sealed record ElmTestSourceLocation(
    ElmModuleOrigin ModuleOrigin,
    string DeclarationName,
    ElmSyntaxExpressionPath ExpressionPath,
    Concrete.Range Range)
{
    /// <summary>Original source path, including the package prefix for external helpers.</summary>
    public string FilePath => ModuleOrigin.SourcePath;
}

/// <summary>
/// Discovery and navigation over one immutable set of parsed sources. Resolve IDs on this result, not a newer CST.
/// Reordering/inserting leaves within a root changes ordinal identities; hosts own document freshness.
/// </summary>
public sealed record ElmTestDiscoveryResult(
    ImmutableArray<ElmDiscoveredTest> Tests,
    ImmutableDictionary<string, ElmTestSourceLocation> SourceLocations)
{
    /// <summary>Total wall time, including source preparation when discovery starts from a project.</summary>
    public TimeSpan Duration { get; init; }
    /// <summary>Project loading, parsing and dependency resolution.</summary>
    public TimeSpan SourcePreparationDuration { get; init; }
    /// <summary>Demand canonicalization and unlowered interpreter preparation.</summary>
    public TimeSpan InterpreterPreparationDuration { get; init; }
    /// <summary>Test construction, runner preparation and location association, excluding assertion execution.</summary>
    public TimeSpan DiscoveryDuration { get; init; }

    /// <summary>Gets navigation for a discovered ID. Unknown IDs fail explicitly.</summary>
    public ElmTestSourceLocation GetSourceLocation(string testId) =>
        SourceLocations.TryGetValue(testId, out var source) ? source :
            throw new KeyNotFoundException("Unknown Elm test ID in this discovery result: " + testId);

    /// <summary>
    /// Converts a discovered ID to the existing PineVM runner's exact selector.
    /// Use with the same project sources; rediscover after edits before selecting by ordinal.
    /// </summary>
    public string GetExecutionFilter(string testId)
    {
        GetSourceLocation(testId);
        var test = Tests.Single(test => test.Id == testId);
        return ElmTestFilter.ExactSelector(test.ListedTest, test.DiscoveryIndex, Tests.Select(test => test.ListedTest));
    }
}
