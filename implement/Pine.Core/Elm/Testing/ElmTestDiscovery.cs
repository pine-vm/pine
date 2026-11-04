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
        CancellationToken cancellationToken = default,
        ElmTestDiscoveryCaches? caches = null)
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

        var sourceLoadingDuration = watch.Elapsed;

        var discovery =
            DiscoverTests(
                tree,
                resolutionConfiguration,
                packageProvider,
                cancellationToken,
                appDirectory,
                caches: caches);

        return
            discovery with
            {
                SourceResolutionDuration = sourceLoadingDuration + discovery.SourceResolutionDuration,
                Duration = watch.Elapsed
            };
    }

    /// <summary>Discovers an in-memory project. Resolution and source errors are reported, not empty successes.</summary>
    public static ElmTestDiscoveryResult DiscoverTests(
        FileTree projectTree,
        ElmDependencyResolutionConfiguration? resolutionConfiguration = null,
        IElmPackageProvider? packageProvider = null,
        CancellationToken cancellationToken = default,
        string? projectDirectory = null,
        IReadOnlyList<string>? manifestPath = null,
        ElmTestExecutionSettings? executionSettings = null,
        ElmTestDiscoveryCaches? caches = null)
    {
        cancellationToken.ThrowIfCancellationRequested();
        var watch = Stopwatch.StartNew();
        var files = projectTree.EnumerateFilesTransitive().ToArray();
        manifestPath ??= ["elm.json"];
        var projectPath = manifestPath.Take(manifestPath.Count - 1).ToArray();
        var testsPath = projectPath.Append("tests").ToArray();

        var nestedProjects =
            files.Where(
                file => file.path.Count > manifestPath.Count && file.path[^1] == "elm.json" &&
                    file.path.Take(projectPath.Length).SequenceEqual(projectPath))
            .Select(file => file.path.Take(file.path.Count - 1).ToArray()).ToArray();

        var roots =
            files.Where(
                file => file.path.Count > testsPath.Length &&
                    file.path.Take(testsPath.Length).SequenceEqual(testsPath) &&
                    file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) &&
                    !file.path.Any(segment => segment is ".git" or "elm-stuff") &&
                    !nestedProjects.Any(nested => file.path.Take(nested.Length).SequenceEqual(nested)))
            .Select(
                file =>
                (file.path, Parsed: Parse(Encoding.UTF8.GetString(file.fileContent.Span), string.Join('/', file.path))))
            .ToArray();

        if (roots.Length == 0)
        {
            var manifest = ElmDependencyResolver.ReadManifest(projectTree, manifestPath);
            ElmDependencyResolver.ValidateManifestShape(manifest, string.Join('/', manifestPath));
            var duration = watch.Elapsed;

            return
                new([], hasOnly: false)
                {
                    SourceResolutionDuration = duration,
                    Duration = duration,
                    SourceFiles = projectTree
                };
        }

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
                manifestPath,
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

        var sourceResolutionDuration = watch.Elapsed;

        return
            DiscoverTests(build, declarations, cancellationToken, executionSettings, caches) with
            {
                SourceResolutionDuration = sourceResolutionDuration,
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
        ElmTestExecutionSettings? executionSettings = null,
        ElmTestDiscoveryCaches? caches = null)
    {
        cancellationToken.ThrowIfCancellationRequested();
        using var invocation = caches?.BeginInvocation();

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
                cache: caches?.CanonicalDeclarations,
                canonicalizedDependencies: null,
                preserveOperatorApplications: true)
            .Extract(error => throw new InvalidOperationException("Failed preparing test discovery: " + error));

        var prepared = new Dictionary<DeclQualifiedName, Interpreter.PreparedDeclaration>();
        var sourceDeclarations = new Dictionary<DeclQualifiedName, Abstract.Declaration>();

        PreparedSourceDeclaration PrepareSource(DeclQualifiedName name, Concrete.Declaration declaration)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var input = PreparedDeclarationInput.FromConcrete(name, declaration);

            if (caches?.PreparedDeclarations.TryGetValue(input, out var cached) is true)
                return cached;

            var source = PreparedSourceDeclaration.Prepare(input.Syntax);
            cancellationToken.ThrowIfCancellationRequested();
            caches?.PreparedDeclarations.Set(input, source);
            return source;
        }

        foreach (var type in modules["Basics"].Declarations.Select(node => node.Value)
            .OfType<Concrete.Declaration.ChoiceTypeDeclaration>())
        {
            var name = DeclQualifiedName.Create(["Basics"], type.TypeDeclaration.Name.Value);
            prepared.Add(name, PrepareSource(name, type).Prepared);
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
            var source = PrepareSource(name, result.Value.Value);
            var declaration = source.Prepared;

            prepared.Add(name, declaration);
            sourceDeclarations.Add(name, source.Syntax);

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

            foreach (var (expression, path) in source.ExpressionPaths)
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

        // Keep the resolver index keyed to one immutable snapshot across root and bridge calls.
        var program = new Interpreter.Prepared(prepared.ToImmutableDictionary());

        var config =
            Interpreter.EvaluationConfig.Default with
            {
                ExpressionObservers = observers,
                CancellationToken = cancellationToken
            };

        var declarationPreparationDuration = watch.Elapsed;
        var inventory = new List<DiscoveryItem>();
        var executableLeaves = new List<DiscoveryItem>();
        var fuzzRoots = new List<DiscoveryRoot>();
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
                fuzzRuns: null);

            var constructedRoot = new DiscoveryRoot(root, value, rootLeaves);
            var (tag, runners) = PrepareRunners(constructedRoot, executionSettings, cancellationToken);
            var only = tag == "Only";
            hasOnly |= only;
            hasSkipped |= tag == "Skipping";

            if (rootLeaves.Any(item => item.Kind == ElmTestDiscoveryItemKind.Fuzz))
                fuzzRoots.Add(constructedRoot);

            foreach (var runner in runners)
            {
                var source = runner.Source;
                source.SelectedByRunner = true;
                source.RootOnly = only;
                source.Runner = runner.Runner;
                source.Fuzz = runner.Fuzz;

                if (source.IsEmptyGroup)
                    continue;

                source.DiscoveryIndex = executableLeaves.Count + 1;
                executableLeaves.Add(source);
            }

            void CollectItems(
                PineValueInProcess current,
                ImmutableArray<string> labels,
                SourceSite inherited,
                DiscoveryItem? parent,
                bool only,
                bool skipped,
                uint? fuzzRuns)
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

                        if (currentTag == "ElmTestVariant__FuzzTest")
                        {
                            leaf.FuzzRuns = fuzzRuns ?? executionSettings.FuzzRuns;
                            leaf.FuzzRunsOverride = fuzzRuns;
                        }

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

        var constructionCompleted = watch.Elapsed;

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

        var fullPathCounts = ElmTestFilter.CountFullPaths(executableLeaves.Select(leaf => leaf.ListedTest));
        var executionGate = new SemaphoreSlim(1, 1);

        var compilationDeclarations =
            new Lazy<ImmutableDictionary<DeclQualifiedName, Abstract.Declaration>>(
                () => sourceDeclarations.ToImmutableDictionary());

        var discovery =
            new ElmTestDiscoveryResult(
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
                                    ElmTestFilter.ExactSelector(item.ListedTest, index, fullPathCounts)
                                    :
                                    null,
                                    location);
                        })
                ],
                hasOnly)
            {
                HasSkipped = hasSkipped,
                SourceFiles = build.OriginalSources,
                ResolutionFingerprint = build.Resolution.Fingerprint,
                ExecutionSettings = executionSettings,
                FuzzRunOverrides =
                inventory.Where(item => item.FuzzRunsOverride is not null)
                .ToImmutableDictionary(item => item.Id, item => item.FuzzRunsOverride!.Value, StringComparer.Ordinal),
                CompileExecution =
                (ids, settings, token, caches) =>
                {
                    executionGate.Wait(token);

                    try
                    {
                        var selected =
                            inventory.Where(
                                item => ids.Contains(item.Id) &&
                                    item.Selection == ElmTestDiscoverySelection.Included &&
                                    item.Kind is ElmTestDiscoveryItemKind.Unit or ElmTestDiscoveryItemKind.Fuzz).ToArray();

                        if (selected.Length == 0)
                            return ImmutableDictionary<string, ElmTestExecutionEntry>.Empty;

                        var runners =
                            selected.ToDictionary(
                                item => item.Id,
                                item => new SeededRunner(
                                    item,
                                    item.Runner ??
                                    throw new InvalidOperationException("Discovered test has no runner: " + item.Id),
                                    item.Fuzz),
                                StringComparer.Ordinal);

                        if (settings != executionSettings)
                        {
                            foreach (var root in fuzzRoots.Where(
                                root => root.Leaves.Any(
                                    item =>
                                    item.Kind == ElmTestDiscoveryItemKind.Fuzz && ids.Contains(item.Id))))
                            {
                                // Seed the entire root to preserve sibling/skip/only seed distribution.
                                foreach (var runner in PrepareRunners(root, settings, token).Runners)
                                {
                                    if (runner.Source.Kind == ElmTestDiscoveryItemKind.Fuzz &&
                                        ids.Contains(runner.Source.Id))
                                    {
                                        runners[runner.Source.Id] = runner;
                                    }
                                }
                            }
                        }

                        var evaluations =
                            Interpreter.CompileApplicationsToPine(
                                [
                                    .. selected.Select(
                                        item =>
                                        (runners[item.Id].Runner,
                                        (IReadOnlyList<PineValueInProcess>)[
                                        PineValueInProcess.Create(PineValue.EmptyList)
                                        ]))
                                ],
                                compilationDeclarations.Value,
                                token,
                                caches);

                        return
                            selected.Select(
                                (item, index) =>
                                new ElmTestExecutionEntry(item.Id, evaluations[index], runners[item.Id].Fuzz))
                            .ToImmutableDictionary(entry => entry.Id, StringComparer.Ordinal);
                    }
                    finally
                    {
                        executionGate.Release();
                    }
                }
            };

        var duration = watch.Elapsed;

        return
            discovery with
            {
                DeclarationPreparationDuration = declarationPreparationDuration,
                TestConstructionDuration = constructionCompleted - declarationPreparationDuration,
                InventoryProjectionDuration = duration - constructionCompleted,
                Duration = duration
            };

        (string Tag, IReadOnlyList<SeededRunner> Runners) PrepareRunners(
            DiscoveryRoot root, ElmTestExecutionSettings settings, CancellationToken token)
        {
            token.ThrowIfCancellationRequested();

            var value =
                Interpreter.Interpret(
                    bridge,
                    [
                        PineValueInProcess.Create(IntegerEncoding.EncodeSignedInteger(settings.FuzzRuns)),
                        PineValueInProcess.Create(IntegerEncoding.EncodeSignedInteger(settings.Seed)), root.Value
                    ],
                    program,
                    Interpreter.EvaluationConfig.Default with { CancellationToken = token })
                .Extract(
                    error => throw new InvalidOperationException(
                        "Failed preparing Elm tests in " + root.Name.FullName + ": " + error));

            var tag = ChoiceTag(value) ?? throw new InvalidOperationException("Invalid test discovery bridge result.");

            if (tag is not ("Plain" or "Only" or "Skipping" or "Invalid"))
                throw new InvalidOperationException("Unsupported test discovery bridge result: " + tag);

            if (tag == "Invalid")
            {
                throw new InvalidOperationException(
                    "Invalid Elm test preparation in " + root.Name.FullName + ": " + ElmString(value.GetElementAt(2)));
            }

            var sources =
                new Queue<DiscoveryItem>(
                    root.Leaves.Where(item => !item.Skipped && (tag != "Only" || item.Only)));

            var runners = new List<SeededRunner>();

            foreach (var runner in ListItems(value.GetElementAt(2)))
            {
                token.ThrowIfCancellationRequested();
                var labels = ListItems(Field(runner, "labels")).Select(ElmString).Reverse().ToArray();
                var kind = ElmString(Field(runner, "kind"));

                if (!sources.TryDequeue(out var source) || source.RunnerKind != kind ||
                    !source.Labels.SequenceEqual(labels))
                {
                    throw new InvalidOperationException(
                        "Cannot associate discovered test with its construction source: " + root.Name.FullName + "/" +
                        string.Join('/', labels));
                }

                var fuzz =
                    kind == "fuzz"
                    ?
                    new ElmFuzzResult(
                        settings,
                        ElmString(Field(runner, "seedState")),
                        checked((uint)IntegerEncoding.ParseSignedIntegerStrict(Field(runner, "runs").Evaluate())
                        .Extract(error => throw new InvalidOperationException(error))))
                    :
                    null;

                runners.Add(new(source, Field(runner, "run"), fuzz));
            }

            if (sources.Count != 0)
            {
                throw new InvalidOperationException(
                    "Test discovery bridge omitted construction sources in " + root.Name.FullName);
            }

            return (tag, runners);
        }
    }

    private sealed record SourceSite(DeclQualifiedName Declaration, ElmSyntaxExpressionPath Path);

    private sealed record DiscoveryRoot(
        DeclQualifiedName Name, PineValueInProcess Value, IReadOnlyList<DiscoveryItem> Leaves);

    private sealed record SeededRunner(DiscoveryItem Source, PineValueInProcess Runner, ElmFuzzResult? Fuzz);

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

        public uint? FuzzRunsOverride { get; set; }

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

        ItemsById = items.ToImmutableDictionary(item => item.Id, StringComparer.Ordinal);
    }

    /// <summary>All discovered tests and groups, including skipped/only-excluded entries and empty groups.</summary>
    public ImmutableArray<ElmTestDiscoveryItem> Items { get; private init; }

    internal ImmutableDictionary<string, ElmTestDiscoveryItem> ItemsById { get; private init; }

    /// <summary>Whether the test library found effective Test.only selection in any root.</summary>
    public bool HasOnly { get; }

    /// <summary>Whether runner preparation reports an incomplete suite due to Test.skip.</summary>
    [JsonIgnore]
    public bool HasSkipped { get; init; }

    /// <summary>Exact original sources for navigation, including resolved packages and substitutions.</summary>
    [JsonIgnore]
    public FileTree SourceFiles { get; init; } = FileTree.EmptyTree;

    /// <summary>Fingerprint of the exact dependency resolution and compiler used for discovery.</summary>
    [JsonIgnore]
    public string? ResolutionFingerprint { get; init; }

    internal Func<IReadOnlySet<string>, ElmTestExecutionSettings, CancellationToken, SourceCompilationCaches?,
        ImmutableDictionary<string, ElmTestExecutionEntry>>? CompileExecution
    { get; init; }

    internal ImmutableDictionary<string, uint> FuzzRunOverrides { get; init; } =
        ImmutableDictionary<string, uint>.Empty;

    /// <summary>Default execution settings for this view of the constructed inventory.</summary>
    [JsonIgnore]
    public ElmTestExecutionSettings ExecutionSettings { get; init; } = new(0, 100);

    /// <summary>
    /// Changes default settings without replaying construction. Selected fuzz roots are reseeded during preparation.
    /// </summary>
    public ElmTestDiscoveryResult WithExecutionSettings(
        ElmTestExecutionSettings settings,
        CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();
        ArgumentOutOfRangeException.ThrowIfZero(settings.FuzzRuns);

        if (settings == ExecutionSettings)
            return this;

        if (!Items.IsEmpty && CompileExecution is null)
            throw new InvalidOperationException("This discovery result has no resolved source environment.");

        var items =
            Items.Select(
                item =>
                item.Kind == ElmTestDiscoveryItemKind.Fuzz
                ?
                item with { FuzzRuns = FuzzRunOverrides.GetValueOrDefault(item.Id, settings.FuzzRuns) }
                :
                item).ToImmutableArray();

        return
            this with
            {
                Items = items,
                ItemsById = items.ToImmutableDictionary(item => item.Id, StringComparer.Ordinal),
                ExecutionSettings = settings
            };
    }

    /// <summary>Compiles selected runnable tests to independent Pine evaluations without replaying construction.</summary>
    public ImmutableDictionary<string, ElmTestExecutionEntry> PrepareExecution(
        IEnumerable<string>? testIds = null,
        ElmTestExecutionSettings? executionSettings = null,
        CancellationToken cancellationToken = default,
        SourceCompilationCaches? compilationCaches = null)
    {
        cancellationToken.ThrowIfCancellationRequested();
        executionSettings ??= ExecutionSettings;
        ArgumentOutOfRangeException.ThrowIfZero(executionSettings.FuzzRuns);
        var ids = (testIds ?? Tests.Select(test => test.Id)).ToHashSet(StringComparer.Ordinal);

        foreach (var id in ids)
            GetExecutionFilter(id);

        if (ids.Count == 0)
            return ImmutableDictionary<string, ElmTestExecutionEntry>.Empty;

        return
            (CompileExecution ?? throw new InvalidOperationException("This discovery result has no prepared code."))
            (ids, executionSettings, cancellationToken, compilationCaches);
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
    public TimeSpan SourceResolutionDuration { get; init; }

    /// <summary>Demand canonicalization, unlowered interpreter preparation and source-observer setup.</summary>
    public TimeSpan DeclarationPreparationDuration { get; init; }

    /// <summary>Root interpretation, test-tree traversal and runner seeding, excluding assertion execution.</summary>
    public TimeSpan TestConstructionDuration { get; init; }

    /// <summary>Global selection, selector generation, source navigation and inventory indexing.</summary>
    public TimeSpan InventoryProjectionDuration { get; init; }

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
            ItemsById[testId].ExecutionFilter ??
            throw new InvalidOperationException("Elm discovery item is a group or is not selected for execution: " + testId);
    }
}
