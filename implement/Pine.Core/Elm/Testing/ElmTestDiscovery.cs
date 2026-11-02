using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Encodings;
using Pine.Core.Files;
using Pine.Core.Internal;
using Pine.Core.IO;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json.Serialization;
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
    private static readonly Lazy<Concrete.File> s_nativeBasics =
        new(
            () =>
            {
                var file =
                    BundledFiles.ElmKernelModulesDefault.Value.EnumerateFilesTransitive()
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

        var tree =
            FileTree.FromSetOfFilesWithStringPath(
                Filesystem.GetFilesFromDirectory(
                    appDirectory,
                    filterByRelativeName: path => !path.Any(segment => segment is ".git" or "elm-stuff"))
                .Select(file => (file.path, file.content)));

        return
            DiscoverTests(tree, resolutionConfiguration, packageProvider, cancellationToken, appDirectory)
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

        var nestedProjects =
            files.Where(file => file.path.Count > 1 && file.path[^1] == "elm.json")
            .Select(file => file.path.Take(file.path.Count - 1).ToArray()).ToArray();

        var roots =
            files.Where(
                file => file.path.Count > 1 && file.path[0] == "tests" &&
                    file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) &&
                    !file.path.Any(segment => segment is ".git" or "elm-stuff") &&
                    !nestedProjects.Any(nested => file.path.Take(nested.Length).SequenceEqual(nested)))
            .Select(
                file =>
                (file.path, Parsed: Parse(Encoding.UTF8.GetString(file.fileContent.Span), string.Join('/', file.path))))
            .ToArray();

        if (roots.Length == 0)
            return new([], hasOnly: false) { Duration = watch.Elapsed };

        resolutionConfiguration ??= ElmTestRunner.DefaultResolutionConfiguration.Value;

        if (!resolutionConfiguration.IncludeTests)
        {
            throw new ArgumentException(
                "Elm test discovery requires IncludeTests = true.",
                nameof(resolutionConfiguration));
        }

        var build =
            ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                projectTree,
                ["elm.json"],
                [.. roots.Select(root => root.path)],
                resolutionConfiguration,
                packageProvider,
                cancellationToken,
                projectDirectory).GetAwaiter().GetResult();

        var declarations =
            roots.OrderBy(root => ModuleName(root.Parsed), StringComparer.Ordinal)
            .SelectMany(
                root => root.Parsed.Declarations.Select(declaration => declaration.Value)
                .OfType<Concrete.Declaration.FunctionDeclaration>()
                .Where(
                    declaration => declaration.Function.Declaration.Value.Arguments.Count == 0 &&
                        ElmTestRunner.IsDeclarationExposed(
                            root.Parsed.ModuleDefinition.Value,
                            declaration.Function.Declaration.Value.Name.Value))
                .Select(
                    declaration => DeclQualifiedName.Create(
                        build.CompilerModuleNames[string.Join('/', root.path)].Split('.'),
                        declaration.Function.Declaration.Value.Name.Value)))
            .ToArray();

        var sourcePreparationDuration = watch.Elapsed;

        return
            DiscoverTests(build, declarations, cancellationToken) with
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
        CancellationToken cancellationToken = default,
        ElmTestExecutionSettings? executionSettings = null)
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

        var nativeRoots =
            modules["Basics"].Declarations.Select(node => node.Value)
            .OfType<Concrete.Declaration.FunctionDeclaration>()
            .Select(function => DeclQualifiedName.Create(["Basics"], function.Function.Declaration.Value.Name.Value));

        const string bridgePath = "elm-packages/elm-explorations/test/src/PineTestBridge.elm";

        if (!build.CompilerModuleNames.TryGetValue(bridgePath, out var bridgeModule))
        {
            throw new InvalidOperationException(
                "The configured test substitution does not implement Pine's test discovery bridge.");
        }

        var bridge = DeclQualifiedName.Create(bridgeModule.Split('.'), "prepare");

        var canonical =
            Canonicalization.CanonicalizeDeclarations(
                [.. modules.Values],
                [.. testRoots, bridge, .. nativeRoots],
                implicitImports: null,
                cache: null,
                canonicalizedDependencies: null,
                preserveOperatorApplications: true)
            .Extract(error => throw new InvalidOperationException("Failed preparing test discovery: " + error));

        var prepared = new Dictionary<DeclQualifiedName, Interpreter.PreparedDeclaration>();

        foreach (var type in modules["Basics"].Declarations.Select(node => node.Value)
            .OfType<Concrete.Declaration.ChoiceTypeDeclaration>())
        {
            prepared.Add(
                DeclQualifiedName.Create(["Basics"], type.TypeDeclaration.Name.Value),
                Interpreter.PrepareDeclaration(Abstract.ConvertFromConcrete.FromDeclaration(type)));
        }

        var sourceBodies = new Dictionary<DeclQualifiedName, Concrete.Node<Concrete.Expression>>();

        var observers =
            new Dictionary<Interpreter.PreparedExpression, Action<PineValueInProcess>>(
                ReferenceEqualityComparer.Instance);

        var valueSources = new Dictionary<PineValueInProcess, SourceSite>(ReferenceEqualityComparer.Instance);
        var describeValues = new HashSet<PineValueInProcess>(ReferenceEqualityComparer.Instance);
        var sourcePaths = build.CompilerModuleOrigins.ToDictionary(entry => entry.Key, entry => entry.Value.SourcePath);
        var sourceModules = modules.Values.ToArray();

        foreach (var (name, result) in canonical.Declarations)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (result.Errors.Count > 0)
            {
                var chain =
                    ElmCompiler.BuildSourceDependencyChain(
                        canonical.DependencyChains[name],
                        sourceModules,
                        sourcePaths,
                        canonical.References);

                var diagnostic =
                    new CompilationError.CanonicalizationErrors(
                        [
                            .. result.Errors.Select(
                                error => ElmCompiler.CompleteCanonicalizationDiagnostic(
                                    new(name.FullName, sourcePaths[string.Join('.', name.Namespaces)], error, chain),
                                    sourceModules,
                                    sourcePaths))
                        ]);

                throw new ElmCompilationException("Failed compiling Elm tests: " + diagnostic);
            }

            // Keep the AST shape: lowering/reduction would invalidate its paths into the original CST.
            var declaration =
                Interpreter.PrepareDeclaration(Abstract.ConvertFromConcrete.FromDeclaration(result.Value.Value));

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

            var concreteFunction =
                modules[moduleName].Declarations.Select(node => node.Value)
                .OfType<Concrete.Declaration.FunctionDeclaration>()
                .Single(candidate => candidate.Function.Declaration.Value.Name.Value == name.DeclName);

            sourceBodies[name] = concreteFunction.Function.Declaration.Value.Expression;

            // Invalid describes can return an anonymous failing unit instead of a labeled batch.
            if (origin is ElmModuleOrigin.Substitution { ReplacedPackage.Name: "elm-explorations/test", ModuleName: "Test" } &&
                name.DeclName == "describe")
            {
                observers[function.Function.Declaration.Expression] = value => describeValues.Add(value);
            }

            // Library internals should not steal navigation from the source that constructs a test.
            if (origin is ElmModuleOrigin.Substitution
                { ReplacedPackage.Name: "elm/core" or "elm-explorations/test" or "elm/random" })
            {
                continue;
            }

            foreach (var (expression, path) in Interpreter.EnumerateExpressionPaths(function.Function.Declaration.Expression))
            {
                var site = new SourceSite(name, path);

                observers[expression] =
                    value =>
                    {
                        if (TestTag(value) is not null)
                            valueSources.TryAdd(value, site);
                    };
            }
        }

        var program = new Interpreter.Prepared(prepared);

        var config =
            Interpreter.EvaluationConfig.Default with
            {
                ExpressionObservers = observers,
                CancellationToken = cancellationToken
            };

        var interpreterPreparationDuration = watch.Elapsed;
        var inventory = new List<DiscoveryItem>();
        var executableLeaves = new List<DiscoveryItem>();
        var hasOnly = false;
        var hasSkipped = false;
        executionSettings ??= new(0, 100);

        foreach (var root in testRoots)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var value =
                Interpreter.Interpret(root, [], program, config)
                .Extract(
                    error =>
                    throw new InvalidOperationException("Failed constructing Elm tests in " + root.FullName + ": " + error));

            if (TestTag(value) is null)
                continue;

            var rootOrigin = build.CompilerModuleOrigins[string.Join('.', root.Namespaces)];

            var idPrefix =
                "elm-test:" + Uri.EscapeDataString(rootOrigin.SourcePath) + "#" + Uri.EscapeDataString(root.DeclName);

            var rootLeaves = new List<DiscoveryItem>();
            var groupIndex = 0;

            CollectItems(
                value,
                [],
                new SourceSite(root, ElmSyntaxExpressionPath.Root),
                parent: null,
                only: false,
                skipped: false,
                fuzzRuns: executionSettings.FuzzRuns);

            var runners =
                Interpreter.Interpret(
                    bridge,
                    [
                        PineValueInProcess.Create(IntegerEncoding.EncodeSignedInteger(executionSettings.FuzzRuns)),
                        PineValueInProcess.Create(IntegerEncoding.EncodeSignedInteger(executionSettings.Seed)), value
                    ],
                    program,
                    config with { ExpressionObservers = null })
                .Extract(
                    error =>
                    throw new InvalidOperationException("Failed preparing Elm tests in " + root.FullName + ": " + error));

            var tag =
                ChoiceTag(runners) ?? throw new InvalidOperationException("Invalid test discovery bridge result.");

            if (tag is not ("Plain" or "Only" or "Skipping" or "Invalid"))
                throw new InvalidOperationException("Unsupported test discovery bridge result: " + tag);

            var only = tag == "Only";
            hasOnly |= only;
            hasSkipped |= tag == "Skipping";

            if (tag == "Invalid")
            {
                throw new InvalidOperationException(
                    "Invalid Elm test preparation in " + root.FullName + ": " + ElmString(runners.GetElementAt(2)));
            }

            var selectedSources =
                new Queue<DiscoveryItem>(rootLeaves.Where(candidate => !candidate.Skipped && (!only || candidate.Only)));

            foreach (var runner in ListItems(runners.GetElementAt(2)))
            {
                cancellationToken.ThrowIfCancellationRequested();
                var labels = ListItems(Field(runner, "labels")).Select(ElmString).Reverse().ToImmutableArray();
                var kind = ElmString(Field(runner, "kind"));

                if (!selectedSources.TryDequeue(out var source) || source.RunnerKind != kind ||
                    !source.Labels.SequenceEqual(labels))
                {
                    throw new InvalidOperationException(
                        "Cannot associate discovered test with its construction source: " + root.FullName + "/" +
                        string.Join('/', labels));
                }

                source.SelectedByRunner = true;
                source.RootOnly = only;
                source.Runner = Field(runner, "run");

                if (kind == "fuzz")
                {
                    source.Fuzz =
                        new(
                            executionSettings,
                            ElmString(Field(runner, "seedState")),
                            checked((uint)IntegerEncoding.ParseSignedIntegerStrict(Field(runner, "runs").Evaluate())
                            .Extract(error => throw new InvalidOperationException(error))));
                }

                if (kind == "empty")
                    continue;

                source.DiscoveryIndex = executableLeaves.Count + 1;
                executableLeaves.Add(source);
            }

            if (selectedSources.Count != 0)
                throw new InvalidOperationException("Test discovery bridge omitted construction sources in " + root.FullName);

            void CollectItems(
                PineValueInProcess current,
                ImmutableArray<string> labels,
                SourceSite inherited,
                DiscoveryItem? parent,
                bool only,
                bool skipped,
                uint fuzzRuns)
            {
                cancellationToken.ThrowIfCancellationRequested();
                var source = valueSources.GetValueOrDefault(current) ?? inherited;
                var currentTag = TestTag(current) ?? throw new InvalidOperationException("Invalid Elm Test value.");

                if (describeValues.Contains(current) && currentTag != "PineEmptyGroup")
                {
                    var groupLabels =
                        currentTag == "ElmTestVariant__Labeled"
                        ?
                        labels.Add(ElmString(current.GetElementAt(2)))
                        :
                        labels;

                    parent = AddItem(ElmTestDiscoveryItemKind.Group, groupLabels, source, parent, only, skipped);
                }

                switch (currentTag)
                {
                    case "ElmTestVariant__UnitTest":
                    case "ElmTestVariant__FuzzTest":
                        var leaf =
                            AddItem(
                                currentTag == "ElmTestVariant__UnitTest"
                                ?
                                ElmTestDiscoveryItemKind.Unit
                                :
                                ElmTestDiscoveryItemKind.Fuzz,
                                labels,
                                source,
                                parent,
                                only,
                                skipped);

                        leaf.RunnerKind = currentTag == "ElmTestVariant__UnitTest" ? "unit" : "fuzz";
                        leaf.FuzzRuns = currentTag == "ElmTestVariant__FuzzTest" ? fuzzRuns : null;
                        rootLeaves.Add(leaf);
                        break;

                    case "PineTodo":
                    case "PineEmptyGroup":
                        var terminal =
                            AddItem(
                                currentTag == "PineTodo"
                                ?
                                ElmTestDiscoveryItemKind.Todo
                                :
                                ElmTestDiscoveryItemKind.Group,
                                labels.Add(ElmString(current.GetElementAt(2))),
                                source,
                                parent,
                                only,
                                skipped);

                        terminal.RunnerKind = currentTag == "PineTodo" ? "todo" : "empty";
                        terminal.IsEmptyGroup = currentTag == "PineEmptyGroup";
                        rootLeaves.Add(terminal);
                        break;

                    case "ElmTestVariant__Labeled":
                        CollectItems(
                            current.GetElementAt(3),
                            labels.Add(ElmString(current.GetElementAt(2))),
                            source,
                            parent,
                            only,
                            skipped,
                            fuzzRuns);

                        break;

                    case "ElmTestVariant__Skipped":
                    case "ElmTestVariant__Only":
                        CollectItems(
                            current.GetElementAt(2),
                            labels,
                            source,
                            parent,
                            only || currentTag == "ElmTestVariant__Only",
                            skipped || currentTag == "ElmTestVariant__Skipped",
                            fuzzRuns);

                        break;

                    case "PineFuzzCount":
                        var count =
                            IntegerEncoding.ParseSignedIntegerStrict(current.GetElementAt(2).Evaluate())
                            .Extract(
                                error =>
                                throw new InvalidOperationException("Invalid fuzz count in test discovery: " + error));

                        CollectItems(
                            current.GetElementAt(3),
                            labels,
                            source,
                            parent,
                            only,
                            skipped,
                            checked((uint)count));

                        break;

                    case "ElmTestVariant__Batch":
                        foreach (var child in ListItems(current.GetElementAt(2)))
                            CollectItems(child, labels, source, parent, only, skipped, fuzzRuns);

                        break;

                    default:
                        throw new InvalidOperationException("Unsupported Elm Test variant: " + currentTag);
                }
            }

            DiscoveryItem AddItem(
                ElmTestDiscoveryItemKind kind,
                ImmutableArray<string> labels,
                SourceSite source,
                DiscoveryItem? parent,
                bool only,
                bool skipped)
            {
                var path = labels.Length == 0 ? [root.DeclName] : labels;

                var item =
                    new DiscoveryItem(
                        idPrefix +
                        (kind == ElmTestDiscoveryItemKind.Group ? "/group/" + groupIndex++ : "/" + rootLeaves.Count),
                        parent,
                        new ListedTest(rootOrigin.SourcePath, path.RemoveAt(path.Length - 1), path[^1]),
                        rootOrigin.ModuleName,
                        root.DeclName,
                        kind,
                        labels,
                        source,
                        only,
                        skipped);

                inventory.Add(item);
                parent?.Children.Add(item);
                return item;
            }
        }

        // Selection is aggregated bottom-up so a group with only skipped children also has a skipped icon.
        foreach (var item in inventory.AsEnumerable().Reverse())
        {
            item.Selection =
                item.Skipped
                ?
                ElmTestDiscoverySelection.Skipped
                :
                item.Children.Count > 0
                ?
                item.Children.Any(child => child.Selection == ElmTestDiscoverySelection.Included)
                ?
                ElmTestDiscoverySelection.Included
                :
                item.Children.Any(child => child.Selection == ElmTestDiscoverySelection.ExcludedByOnly)
                ?
                ElmTestDiscoverySelection.ExcludedByOnly
                :
                ElmTestDiscoverySelection.Skipped
                :
                item.SelectedByRunner && (!hasOnly || item.RootOnly)
                ?
                ElmTestDiscoverySelection.Included
                :
                ElmTestDiscoverySelection.ExcludedByOnly;
        }

        var listedLeaves = executableLeaves.Select(leaf => leaf.ListedTest).ToArray();

        return
            new(
                [
                    .. inventory.Select(
                        item =>
                        {
                            var node =
                                item.Source.Path.Resolve(sourceBodies[item.Source.Declaration]) ??
                                throw new InvalidOperationException(
                                    "AST source path does not match its CST: " + item.Source.Declaration.FullName);

                            var location =
                                new ElmTestSourceLocation(
                                    build.CompilerModuleOrigins[string.Join('.', item.Source.Declaration.Namespaces)],
                                    item.Source.Declaration.DeclName,
                                    item.Source.Path,
                                    node.Range);

                            return
                                new ElmTestDiscoveryItem(
                                    item.Id,
                                    item.Parent?.Id,
                                    item.ListedTest,
                                    item.RootModuleName,
                                    item.RootDeclarationName,
                                    item.Kind,
                                    item.Selection,
                                    item.Only,
                                    item.IsEmptyGroup,
                                    item.FuzzRuns,
                                    item.DiscoveryIndex,
                                    item.Selection == ElmTestDiscoverySelection.Included &&
                                    item.DiscoveryIndex is { } index
                                    ?
                                    ElmTestFilter.ExactSelector(item.ListedTest, index, listedLeaves)
                                    :
                                    null,
                                    location);
                        })
                ],
                hasOnly)
            {
                HasSkipped = hasSkipped,
                CompileExecution =
                ids =>
                {
                    var selected =
                        inventory.Where(
                            item => ids.Contains(item.Id) &&
                                item.Selection == ElmTestDiscoverySelection.Included &&
                                item.Kind is ElmTestDiscoveryItemKind.Unit or ElmTestDiscoveryItemKind.Fuzz).ToArray();

                    if (selected.Length == 0)
                        return ImmutableDictionary<string, ElmTestExecutionEntry>.Empty;

                    var evaluations =
                        Interpreter.CompileApplicationsToPine(
                            [
                                .. selected.Select(
                                    item =>
                                    (item.Runner ??
                                    throw new InvalidOperationException("Discovered test has no runner: " + item.Id),
                                    (IReadOnlyList<PineValueInProcess>)[
                                    PineValueInProcess.Create(PineValue.EmptyList)
                                    ]))
                            ],
                            canonical.Declarations.ToImmutableDictionary(
                                entry => entry.Key,
                                entry => Abstract.ConvertFromConcrete.FromDeclaration(entry.Value.Value.Value)),
                            cancellationToken);

                    return
                        selected.Select(
                            (item, index) => new ElmTestExecutionEntry(item.Id, evaluations[index], item.Fuzz))
                        .ToImmutableDictionary(entry => entry.Id, StringComparer.Ordinal);
                },
                InterpreterPreparationDuration = interpreterPreparationDuration,
                DiscoveryDuration = watch.Elapsed - interpreterPreparationDuration,
                Duration = watch.Elapsed
            };
    }

    private sealed record SourceSite(DeclQualifiedName Declaration, ElmSyntaxExpressionPath Path);

    private sealed class DiscoveryItem(
        string id, DiscoveryItem? parent, ListedTest listedTest, string rootModuleName, string rootDeclarationName,
        ElmTestDiscoveryItemKind kind, ImmutableArray<string> labels, SourceSite source, bool only, bool skipped)
    {
        public string Id { get; } = id;

        public DiscoveryItem? Parent { get; } = parent;

        public ListedTest ListedTest { get; } = listedTest;

        public string RootModuleName { get; } = rootModuleName;

        public string RootDeclarationName { get; } = rootDeclarationName;

        public ElmTestDiscoveryItemKind Kind { get; } = kind;

        public ImmutableArray<string> Labels { get; } = labels;

        public SourceSite Source { get; } = source;

        public bool Only { get; } = only;

        public bool Skipped { get; } = skipped;

        public List<DiscoveryItem> Children { get; } = [];

        public string? RunnerKind { get; set; }

        public bool IsEmptyGroup { get; set; }

        public uint? FuzzRuns { get; set; }

        public int? DiscoveryIndex { get; set; }

        public bool SelectedByRunner { get; set; }

        public bool RootOnly { get; set; }

        public ElmTestDiscoverySelection Selection { get; set; }

        public PineValueInProcess? Runner { get; set; }

        public ElmFuzzResult? Fuzz { get; set; }
    }

    private static string? TestTag(PineValueInProcess value)
    {
        var tag = ChoiceTag(value);

        return
            tag is not null &&
            (tag.StartsWith("ElmTestVariant__", StringComparison.Ordinal) ||
            tag is "PineTodo" or "PineEmptyGroup" or "PineFuzzCount")
            ?
            tag
            :
            null;
    }

    private static string? ChoiceTag(PineValueInProcess value)
    {
        if (value.UnevaluatedStructuralItemsOrNull() is not { Count: >= 2 } items)
            return null;

        if (items[0].EvaluatedOrNull != ElmValue.ElmChoiceTypeTagNameAsValue)
            return null;

        return
            StringEncoding.StringFromValue(items[1].Evaluate())
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
        {
            if (StringEncoding.StringFromValue(record.GetElementAt(index).Evaluate()).Extract(
                error => throw new InvalidOperationException(error)) == name)
            {
                return record.GetElementAt(index + 1);
            }
        }

        throw new InvalidOperationException("Missing test runner field: " + name);
    }

    private static string ElmString(PineValueInProcess value) =>
        ElmValueEncoding.PineValueAsElmValue(value.Evaluate(), null, null)
            .Extract(error => throw new InvalidOperationException("Invalid test label: " + error)) is ElmValue.ElmString text
        ?
        text.Value
        :
        throw new InvalidOperationException("Expected an Elm string in test discovery.");

    private static Concrete.File Parse(string text, string path)
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText(text)
            .Extract(
                error => throw new InvalidOperationException("Failed parsing Elm test module '" + path + "': " + error));

        if (parsed.IncompleteDeclarations.Count > 0 || parsed.AdditionalParseErrors.Count > 0)
            throw new InvalidOperationException("Incomplete Elm syntax in test module '" + path + "'.");

        return parsed;
    }

    private static string ModuleName(Concrete.File file) =>
        string.Join('.', Concrete.Module.GetModuleName(file.ModuleDefinition.Value).Value);
}

/// <summary>A selected leaf discovered without compilation, with a required navigation target.</summary>
public sealed record ElmDiscoveredTest(
    string Id, ListedTest ListedTest, string Kind, int DiscoveryIndex, ElmTestSourceLocation SourceLocation);

/// <summary>Executable handoff for a discovered test; no interpreter state is needed by PineVM.</summary>
public sealed record ElmTestExecutionEntry(string Id, PineEvaluation Evaluation, ElmFuzzResult? Fuzz);

/// <summary>Semantic kind used to distinguish group, unit, fuzz and todo icons without inspecting labels.</summary>
[JsonConverter(typeof(JsonStringEnumConverter<ElmTestDiscoveryItemKind>))]
public enum ElmTestDiscoveryItemKind
{
    /// <summary>A describe group, including an empty group.</summary>
    Group,
    /// <summary>An ordinary assertion callback.</summary>
    Unit,
    /// <summary>A fuzz test.</summary>
    Fuzz,
    /// <summary>A test placeholder.</summary>
    Todo
}

/// <summary>Effective selection before any host-supplied filter. Skip takes precedence over only.</summary>
[JsonConverter(typeof(JsonStringEnumConverter<ElmTestDiscoverySelection>))]
public enum ElmTestDiscoverySelection
{
    /// <summary>A selected test, or a group containing selected tests.</summary>
    Included,
    /// <summary>An explicitly skipped test/group, or a group whose children are all skipped.</summary>
    Skipped,
    /// <summary>Not skipped, but excluded by Test.only elsewhere in the suite.</summary>
    ExcludedByOnly
}

/// <summary>
/// A complete discovery inventory entry in parent-before-child order, including skipped and excluded entries.
/// Parent IDs refer to actual describe groups, not groups reconstructed by matching description text.
/// The required source location navigates to the construction expression, possibly in an imported factory.
/// </summary>
public sealed record ElmTestDiscoveryItem(
    string Id,
    string? ParentId,
    ListedTest ListedTest,
    string RootModuleName,
    string RootDeclarationName,
    ElmTestDiscoveryItemKind Kind,
    ElmTestDiscoverySelection Selection,
    bool IsOnly,
    bool IsEmptyGroup,
    uint? FuzzRuns,
    int? DiscoveryIndex,
    string? ExecutionFilter,
    ElmTestSourceLocation SourceLocation);

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
public sealed record ElmTestDiscoveryResult
{
    /// <summary>Creates a discovery inventory and its backwards-compatible selected-leaf and navigation views.</summary>
    public ElmTestDiscoveryResult(ImmutableArray<ElmTestDiscoveryItem> items, bool hasOnly)
    {
        Items = items;
        HasOnly = hasOnly;

        Tests =
            [
            .. items.Where(item => item.ExecutionFilter is not null).Select(
                item =>
                new ElmDiscoveredTest(
                    item.Id,
                    item.ListedTest,
                    item.Kind switch
                    {
                        ElmTestDiscoveryItemKind.Unit => "unit",
                        ElmTestDiscoveryItemKind.Fuzz => "fuzz",
                        ElmTestDiscoveryItemKind.Todo => "todo",

                        ElmTestDiscoveryItemKind.Group =>
                        throw new InvalidOperationException("A discovery group cannot have a leaf execution filter."),

                        _ =>
                        throw new NotImplementedException(
                            nameof(ElmTestDiscoveryResult) + " does not handle item kind: " + item.Kind)
                    },
                    item.DiscoveryIndex ??
                    throw new InvalidOperationException("An executable test must have a discovery index."),
                    item.SourceLocation))
            ];

        SourceLocations =
            items.ToImmutableDictionary(item => item.Id, item => item.SourceLocation, StringComparer.Ordinal);
    }

    /// <summary>All discovered tests and groups, including skipped/only-excluded entries and empty groups.</summary>
    public ImmutableArray<ElmTestDiscoveryItem> Items { get; }

    /// <summary>Whether the test library found effective Test.only selection in any root.</summary>
    public bool HasOnly { get; }

    /// <summary>Whether runner preparation reports an incomplete suite due to Test.skip.</summary>
    [JsonIgnore]
    public bool HasSkipped { get; init; }

    internal Func<IReadOnlySet<string>, ImmutableDictionary<string, ElmTestExecutionEntry>>? CompileExecution { get; init; }

    /// <summary>Compiles selected runnable tests to independent Pine evaluations without replaying construction.</summary>
    public ImmutableDictionary<string, ElmTestExecutionEntry> PrepareExecution(IEnumerable<string>? testIds = null)
    {
        var ids = (testIds ?? Tests.Select(test => test.Id)).ToHashSet(StringComparer.Ordinal);

        foreach (var id in ids)
            GetExecutionFilter(id);

        if (ids.Count == 0)
            return ImmutableDictionary<string, ElmTestExecutionEntry>.Empty;

        return (CompileExecution ?? throw new InvalidOperationException("This discovery result has no prepared code."))(ids);
    }

    /// <summary>The selected nonempty leaves. CLI listings also show skipped leaves from <see cref="Items"/>.</summary>
    [JsonIgnore]
    public ImmutableArray<ElmDiscoveredTest> Tests { get; }

    /// <summary>Navigation for every inventory ID, including groups and skipped tests.</summary>
    [JsonIgnore]
    public ImmutableDictionary<string, ElmTestSourceLocation> SourceLocations { get; }

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
        SourceLocations.TryGetValue(testId, out var source)
        ?
        source
        :
        throw new KeyNotFoundException("Unknown Elm test ID in this discovery result: " + testId);

    /// <summary>
    /// Converts a discovered ID to the existing PineVM runner's exact selector.
    /// Use with the same project sources; rediscover after edits before selecting by ordinal.
    /// </summary>
    public string GetExecutionFilter(string testId)
    {
        GetSourceLocation(testId);

        return
            Items.Single(item => item.Id == testId).ExecutionFilter ??
            throw new InvalidOperationException("Elm discovery item is a group or is not selected for execution: " + testId);
    }
}
