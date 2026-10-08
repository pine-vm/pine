using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;

using Syntax = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Elm;

/// <summary>Fetches a resolved graph and enforces source ownership before entering the compiler.</summary>
public static class ElmResolvedBuildPreparation
{
    /// <summary>Resolves, fetches and verifies sources, then validates and isolates package imports.</summary>
    /// <param name="projectTree">A tree containing the selected manifest and project sources.</param>
    /// <param name="manifestPath">The one selected elm.json, relative to the supplied tree.</param>
    /// <param name="rootFilePaths">Selected source paths relative to the supplied tree, whose import closure is validated.</param>
    /// <param name="configuration">Compiler, test scope, locks and authoritative package replacements.</param>
    /// <param name="provider">Package data provider; defaults to the registry with the configured offline policy.</param>
    /// <param name="cancellationToken">Cancels resolution and source acquisition without reporting a constraint conflict.</param>
    /// <param name="projectDirectory">
    /// Optional filesystem project root for loading declared parent sources. Requires a project-rooted tree and manifest path ["elm.json"].
    /// </param>
    /// <exception cref="ElmDependencyResolutionException">Retains the report when resolution or source validation fails.</exception>
    public static Task<ElmResolvedBuild> PrepareAsync(
        FileTree projectTree,
        IReadOnlyList<string> manifestPath,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        ElmDependencyResolutionConfiguration configuration,
        IElmPackageProvider? provider = null,
        CancellationToken cancellationToken = default,
        string? projectDirectory = null) =>
        PrepareSourcesAsync(
            projectTree,
            manifestPath,
            rootFilePaths,
            configuration,
            provider,
            cancellationToken,
            projectDirectory,
            declarationDemand: false);

    /// <summary>
    /// Resolves and verifies all available sources without requiring their module-import closure.
    /// Unavailable imports are isolated and retained as diagnostics; declaration demand determines their relevance.
    /// </summary>
    /// <param name="projectTree">A tree containing the selected manifest and project sources.</param>
    /// <param name="manifestPath">The one selected elm.json, relative to the supplied tree.</param>
    /// <param name="rootFilePaths">Caller-selected source paths, not compilation units or declaration roots. May be empty.</param>
    /// <param name="configuration">Compiler, test scope, locks and authoritative package replacements.</param>
    /// <param name="provider">Package data provider; defaults to the registry with the configured offline policy.</param>
    /// <param name="cancellationToken">Cancels resolution and source acquisition.</param>
    /// <param name="projectDirectory">Optional filesystem project root for loading declared parent sources.</param>
    public static Task<ElmResolvedBuild> PrepareForDeclarationDemandAsync(
        FileTree projectTree,
        IReadOnlyList<string> manifestPath,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        ElmDependencyResolutionConfiguration configuration,
        IElmPackageProvider? provider = null,
        CancellationToken cancellationToken = default,
        string? projectDirectory = null) =>
        PrepareSourcesAsync(
            projectTree,
            manifestPath,
            rootFilePaths,
            configuration,
            provider,
            cancellationToken,
            projectDirectory,
            declarationDemand: true);

    private static async Task<ElmResolvedBuild> PrepareSourcesAsync(
        FileTree projectTree,
        IReadOnlyList<string> manifestPath,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        ElmDependencyResolutionConfiguration configuration,
        IElmPackageProvider? provider,
        CancellationToken cancellationToken,
        string? projectDirectory,
        bool declarationDemand)
    {
        provider ??=
            new ElmRegistryPackageProvider(configuration.Offline, compilerVersion: configuration.CompilerVersion);

        var report =
            await ElmDependencyResolver.ResolveAsync(projectTree, manifestPath, configuration, provider, cancellationToken);

        if (!report.Succeeded)
            throw new ElmDependencyResolutionException(report);

        FileTree appSources;

        try
        {
            if (projectDirectory is not null)
            {
                if (manifestPath.Count != 1 || manifestPath[0] != "elm.json")
                {
                    throw new ArgumentException(
                        "projectDirectory requires a source tree rooted at the selected project's directory.");
                }

                var loaded = IncludeParentSources(projectTree, projectDirectory, report.Manifest!, cancellationToken);
                projectTree = loaded.tree;
                manifestPath = [.. loaded.prefix, "elm.json"];
                rootFilePaths = [.. rootFilePaths.Select(path => (IReadOnlyList<string>)[.. loaded.prefix, .. path])];
            }

            appSources = SelectProjectSources(projectTree, manifestPath, report.Manifest!, configuration.IncludeTests);
        }
        catch (Exception exception) when (exception is JsonException or IOException or UnauthorizedAccessException)
        {
            var failure =
                new ElmResolutionFailure(
                    ElmResolutionFailureKind.InvalidManifest,
                    "",
                    "Cannot load the selected project's source directories: " + exception.Message,
                    report.Requirements,
                    [
                    .. report.Packages.Values.Select(package => package.Identity)
                    ])
                {
                    ExceptionDetail = exception.ToString()
                };

            throw new ElmDependencyResolutionException(report with { Failures = [failure], Fingerprint = null }, exception);
        }

        var packages = ImmutableDictionary.CreateBuilder<string, FileTree>(StringComparer.Ordinal);
        var fingerprints = ImmutableDictionary.CreateBuilder<string, string>(StringComparer.Ordinal);
        var traces = report.Trace.ToBuilder();
        var combined = appSources;

        foreach (var package in report.Packages.Values.OrderBy(item => item.Identity.Name, StringComparer.Ordinal))
        {
            cancellationToken.ThrowIfCancellationRequested();
            FileTree sources;

            try
            {
                if (package.SubstitutionImplementationId is not null)
                {
                    sources =
                        configuration.Substitutions.Single(
                            item =>
                            item.PackageName == package.Identity.Name &&
                            item.ImplementationId == package.SubstitutionImplementationId &&
                            item.Versions.Contains(package.Identity.Version)).Sources;
                }
                else
                {
                    sources = await provider.GetSourcesAsync(package.Identity, cancellationToken);

                    var sourceManifestText =
                        sources.GetNodeAtPath(["elm.json"]) is FileTree.FileNode manifestFile
                        ?
                        Encoding.UTF8.GetString(manifestFile.Bytes.Span)
                        :
                        "Source tree contains no elm.json.";

                    traces.Add(
                        new(
                            traces.Count,
                            "SourceManifest",
                            package.Identity.Name,
                            package.Identity.Version,
                            sourceManifestText,
                            package.Origin,
                            [
                            .. report.Requirements.Where(item => item.PackageName == package.Identity.Name)
                            ],
                            [.. report.Packages.Values.Select(item => item.Identity)]));

                    var sourceManifest = ElmDependencyResolver.ReadManifest(sources, ["elm.json"]);

                    if (sourceManifest.Name != package.Identity.Name ||
                        sourceManifest.Version != package.Identity.Version.ToString() ||
                        MetadataFingerprint(sourceManifest) != MetadataFingerprint(package.Manifest!))
                    {
                        throw new ElmPackageProviderException(
                            ElmResolutionFailureKind.InvalidManifest,
                            package.Origin,
                            $"Source archive for '{package.Identity}' declares '{sourceManifest.Name}@{sourceManifest.Version}' " +
                            $"with Elm requirement '{sourceManifest.ElmVersion}' and disagrees with the metadata used to resolve its dependencies. " +
                            "Check the package cache and upstream release; do not compile an unverified graph.");
                    }
                }
            }
            catch (Exception exception) when (exception is ElmPackageProviderException or JsonException or IOException or
                HttpRequestException or UnauthorizedAccessException or FormatException or InvalidOperationException)
            {
                var failure =
                    new ElmResolutionFailure(
                        exception is ElmPackageProviderException providerException
                        ?
                        providerException.Kind
                        :
                        exception is JsonException or FormatException
                        ?
                        ElmResolutionFailureKind.InvalidManifest
                        :
                        ElmResolutionFailureKind.ProviderFailure,
                        package.Identity.Name,
                        exception.Message,
                        [.. report.Requirements.Where(item => item.PackageName == package.Identity.Name)],
                        [
                        .. report.Packages.Values.Select(item => item.Identity)
                        ])
                    {
                        ExceptionDetail = exception.ToString()
                    };

                throw new ElmDependencyResolutionException(
                    report with { Failures = [failure], Trace = traces.ToImmutable(), Fingerprint = null },
                    exception);
            }

            packages.Add(package.Identity.Name, sources);
            var fingerprint = ElmDependencyResolver.SourceFingerprint(sources);
            fingerprints.Add(package.Identity.Name, fingerprint);

            traces.Add(
                new(
                    traces.Count,
                    "Sources",
                    package.Identity.Name,
                    package.Identity.Version,
                    "SHA-256:" + fingerprint,
                    package.Origin,
                    [.. report.Requirements.Where(item => item.PackageName == package.Identity.Name)],
                    [.. report.Packages.Values.Select(item => item.Identity)]));

            combined = AddPackageSources(combined, package.Identity.Name, sources);
        }

        report =
            report with
            {
                Trace = traces.ToImmutable(),
                Fingerprint =
                ElmDependencyResolver.JsonFingerprint(
                    JsonSerializer.Serialize(
                        new
                        {
                            Resolution = report.Fingerprint,
                            AppSources = ElmDependencyResolver.SourceFingerprint(appSources),
                            PackageSources = fingerprints.OrderBy(item => item.Key, StringComparer.Ordinal),
                        })),
            };

        var build =
            new ElmResolvedBuild(
                combined,
                [.. rootFilePaths.Select(path => path.ToImmutableArray())],
                report,
                fingerprints.ToImmutable(),
                packages.ToImmutable())
            {
                ProjectSources = appSources,
                ProjectSourceFingerprint = ElmDependencyResolver.SourceFingerprint(appSources),
            };

        return
            declarationDemand
            ?
            PrepareDeclarationSources(build, appSources)
            :
            ValidateImports(build, appSources);
    }

    /// <summary>Adds package-relative Elm sources under an owner-specific elm-packages directory.</summary>
    public static FileTree AddPackageSources(FileTree tree, string packageName, FileTree sources)
    {
        foreach (var file in sources.EnumerateFilesTransitive())
            if (file.path.Count >= 2 && file.path[0] is "src" &&
                file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            {
                tree =
                    tree.SetNodeAtPathSorted(
                        ["elm-packages", .. packageName.Split('/'), .. file.path],
                        FileTree.File(file.fileContent));
            }

        return tree;
    }

    /// <summary>Selects declared source directories and optional tests, excluding unrelated nested projects.</summary>
    public static FileTree SelectProjectSources(
        FileTree tree,
        IReadOnlyList<string> manifestPath,
        Elm019.ElmJsonStructure manifest,
        bool includeTests)
    {
        var directory = manifestPath.Take(manifestPath.Count - 1).ToArray();

        var directories =
            manifest.Type is "package"
            ?
            new[] { new Elm019.ElmJsonStructure.RelativeDirectory(0, ["src"]) }
            :
            [.. manifest.ParsedSourceDirectories];

        var prefixes = directories.Select(sourceDirectory => MapSourceDirectory(directory, sourceDirectory)).ToList();

        if (includeTests)
            prefixes.Add([.. directory, "tests"]);

        var nestedProjects =
            tree.EnumerateFilesTransitive()
            .Where(
                file => file.path[^1] is "elm.json" && !file.path.SequenceEqual(manifestPath) &&
                    !IsUnder(directory, [.. file.path.Take(file.path.Count - 1)]))
            .Select(file => file.path.Take(file.path.Count - 1).ToArray())
            .ToArray();

        return
            FileTree.FromSetOfFilesWithStringPath(
                tree.EnumerateFilesTransitive().Where(
                    file => file.path.SequenceEqual(manifestPath) ||
                        file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) &&
                        prefixes.Any(prefix => IsUnder(file.path, prefix)) &&
                        !nestedProjects.Any(nested =>
                        IsUnder(file.path, nested) && !prefixes.Any(prefix => IsUnder(prefix, nested))))
                .Select(file => ((IReadOnlyList<string>)file.path, file.fileContent)));
    }

    internal static string[] MapSourceDirectory(
        IReadOnlyList<string> manifestDirectory, Elm019.ElmJsonStructure.RelativeDirectory sourceDirectory)
    {
        if (sourceDirectory.ParentLevel > manifestDirectory.Count)
        {
            throw new JsonException(
                "Source directory escapes the supplied source tree. Supply a tree containing the project's parent directories.");
        }

        return
            [
            .. manifestDirectory.Take(manifestDirectory.Count - sourceDirectory.ParentLevel),
            .. sourceDirectory.Subdirectories
            ];
    }

    private static (FileTree tree, string[] prefix) IncludeParentSources(
        FileTree projectFiles, string projectDirectory, Elm019.ElmJsonStructure manifest, CancellationToken cancellationToken)
    {
        var directories = manifest.ParsedSourceDirectories.ToArray();
        var parentLevels = directories.Select(directory => directory.ParentLevel).DefaultIfEmpty(0).Max();

        if (parentLevels is 0)
            return (projectFiles, []);

        var baseDirectory = Path.GetFullPath(projectDirectory);

        for (var level = 0; level < parentLevels; ++level)
            baseDirectory =
                Directory.GetParent(baseDirectory)?.FullName
                ?? throw new JsonException("A source-directory traverses above the filesystem root.");

        var prefix =
            Path.GetRelativePath(baseDirectory, projectDirectory).Split(
                Path.DirectorySeparatorChar,
                Path.AltDirectorySeparatorChar);

        var combined =
            FileTree.FromSetOfFilesWithStringPath(
                projectFiles.EnumerateFilesTransitive().Select(
                    file =>
                    ((IReadOnlyList<string>)[.. prefix, .. file.path], file.fileContent)));

        foreach (var directory in directories.Where(directory => directory.ParentLevel > 0))
        {
            var relative = MapSourceDirectory(prefix, directory);
            var physical = Path.Combine([baseDirectory, .. relative]);

            if (!Directory.Exists(physical))
                throw new DirectoryNotFoundException($"Declared source directory '{physical}' does not exist.");

            foreach (var file in Filesystem.GetFilesFromDirectory(
                physical,
                path => !path.Any(segment => segment is ".git" or "elm-stuff")))
            {
                cancellationToken.ThrowIfCancellationRequested();

                if (file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) || file.path[^1] is "elm.json")
                    combined = combined.SetNodeAtPathSorted([.. relative, .. file.path], FileTree.File(file.content));
            }
        }

        return (combined, prefix);
    }

    private static bool IsUnder(IReadOnlyList<string> path, IReadOnlyList<string> prefix) =>
        path.Count >= prefix.Count && path.Take(prefix.Count).SequenceEqual(prefix);

    private static string MetadataFingerprint(Elm019.ElmJsonStructure manifest) =>
        JsonSerializer.Serialize(
            new
            {
                manifest.Type,
                manifest.Name,
                manifest.Version,
                manifest.ElmVersion,
                ExposedModules = (manifest.ExposedModules ?? []).Order(StringComparer.Ordinal),
                Dependencies =
                (manifest.Dependencies.Flat ?? ImmutableDictionary<string, string>.Empty).OrderBy(
                    item => item.Key,
                    StringComparer.Ordinal),
            });

    private static ElmResolvedBuild PrepareDeclarationSources(ElmResolvedBuild build, FileTree appSources)
    {
        var byPath = new Dictionary<string, PreparedModule>(StringComparer.Ordinal);

        void Add(FileTree sources, string? owner)
        {
            foreach (var file in sources.EnumerateFilesTransitive())
            {
                if (!file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) ||
                    owner is not null && (file.path.Count < 2 || file.path[0] != "src"))
                    continue;

                var path =
                    (owner is null ? "" : "elm-packages/" + owner + "/") + string.Join("/", file.path);

                var parsed =
                    ElmSyntaxParser.ParseModuleText(Encoding.UTF8.GetString(file.fileContent.Span))
                    .Extract(
                        error => throw new InvalidOperationException($"Cannot parse Elm source '{path}': {error}"));

                var name = string.Join(".", Syntax.Module.GetModuleName(parsed.ModuleDefinition.Value).Value);

                if (!byPath.TryAdd(path, new(name, path, owner, parsed)))
                {
                    Fail(
                        name,
                        $"Source path '{path}' is owned by both '{byPath[path].Owner ?? "the selected project"}' " +
                        $"and '{owner ?? "the selected project"}'. Package sources must not overwrite project sources.");
                }
            }
        }

        Add(appSources, null);

        foreach (var package in build.PackageSources.OrderBy(item => item.Key, StringComparer.Ordinal))
            Add(package.Value, package.Key);

        var modules =
            byPath.Values.GroupBy(module => module.Name).ToDictionary(
                group => group.Key,
                group => group.OrderBy(module => module.Path, StringComparer.Ordinal).ToArray(),
                StringComparer.Ordinal);

        var coreNames =
            ElmCompilerInDotnet.ImplicitImportConfig.Default.ModuleImports
            .Select(import => string.Join(".", import.ModuleName)).ToHashSet(StringComparer.Ordinal);

        if (build.Resolution.Packages.TryGetValue("elm/core", out var core))
            coreNames.UnionWith(core.ExposedModules);

        coreNames.UnionWith(byPath.Values.Where(module => module.Owner is "elm/core").Select(module => module.Name));

        foreach (var module in byPath.Values.OrderBy(module => module.Path, StringComparer.Ordinal))
        {
            if (modules[module.Name].Count(other => other.Owner == module.Owner) != 1)
            {
                Fail(
                    module.Name,
                    $"Module '{module.Name}' is declared more than once in '{module.Owner ?? "the selected project"}': " +
                    string.Join(
                        ", ",
                        modules[module.Name].Where(other => other.Owner == module.Owner).Select(other => other.Path)));
            }

            if (module.Owner is null && coreNames.Contains(module.Name))
            {
                Fail(
                    module.Name,
                    $"Project module '{module.Name}' in '{module.Path}' conflicts with an elm/core compiler module. " +
                    "Rename the project module to preserve the compiler's implicit core imports.");
            }
        }

        foreach (var selectedPath in build.RootFilePaths)
        {
            var path = string.Join("/", selectedPath);

            if (!byPath.TryGetValue(path, out var module) || module.Owner is not null)
            {
                Fail(
                    "",
                    $"Selected source '{path}' is not an Elm source in the selected project's source directories or tests directory.");
            }
        }

        // Reserve every source spelling, not just demanded modules, to keep identities stable across root selection.
        var reservedNames = modules.Keys.Concat(coreNames).ToHashSet(StringComparer.Ordinal);
        var compilerNames = ImmutableDictionary.CreateBuilder<string, string>(StringComparer.Ordinal);

        foreach (var module in byPath.Values.OrderBy(module => module.Path, StringComparer.Ordinal))
            compilerNames.Add(
                module.Path,
                module.Owner is null or "elm/core"
                ?
                module.Name
                :
                ReserveName("PinePackage.P" + ElmDependencyResolver.Fingerprint(module.Owner) + "." + module.Name));

        var directPackages =
            build.Resolution.Requirements
            .Where(
                item =>
                item.DeclaringPackage is null &&
                item.Scope is ElmDependencyScope.Direct or ElmDependencyScope.TestDirect)
            .Select(item => item.PackageName).ToHashSet(StringComparer.Ordinal);

        var importNames = new Dictionary<(string path, string name), string>();
        var diagnostics = ImmutableArray.CreateBuilder<ElmResolvedBuild.ImportDiagnostic>();

        var exportedApi =
            byPath.ToDictionary(item => item.Key, item => GetExposedApi(item.Value.Parsed), StringComparer.Ordinal);

        // List's type is compiler-native even though the replacement source only declares its functions.
        foreach (var module in byPath.Values.Where(module => module.Owner is "elm/core" && module.Name is "List"))
            exportedApi[module.Path].Add(("List", true, false));

        foreach (var module in byPath.Values.Where(module => module.Owner is "elm/core" && module.Name is "Basics"))
            exportedApi[module.Path].UnionWith(NativeExposedApi("Basics").Where(api => api.type));

        foreach (var module in byPath.Values.OrderBy(module => module.Path, StringComparer.Ordinal))
        {
            var visiblePackages =
                module.Owner is null
                ?
                directPackages
                :
                build.Resolution.Packages[module.Owner].Dependencies.Select(item => item.PackageName).ToHashSet(
                    StringComparer.Ordinal);

            foreach (var import in module.Parsed.Imports)
            {
                var name = string.Join(".", import.Value.ModuleName.Value);
                var candidates = modules.GetValueOrDefault(name) ?? [];

                var visible =
                    candidates.Where(
                        candidate =>
                        candidate.Owner == module.Owner ||
                        candidate.Owner is not null && visiblePackages.Contains(candidate.Owner) &&
                        build.Resolution.Packages[candidate.Owner].ExposedModules.Contains(name)).ToArray();

                if (name is "Basics" or "Debug" && visible.Length is 0 &&
                    (visiblePackages.Contains("elm/core") || module.Owner is "elm/core"))
                {
                    ValidateExposing(
                        import,
                        module,
                        name,
                        NativeExposedApi(name),
                        "the elm/core compiler-native module");

                    continue;
                }

                if (visible.Length is not 1)
                {
                    var reason =
                        visible.Length > 1
                        ?
                        "is ambiguous between " + string.Join(", ", visible.Select(candidate => candidate.Path))
                        :
                        candidates.Length > 0
                        ?
                        "exists only in private modules or indirect/undeclared dependencies: " +
                        string.Join(", ", candidates.Select(candidate => candidate.Path))
                        :
                        "has no implementation in the resolved environment";

                    var substitutions =
                        string.Join(
                            ", ",
                            build.Resolution.Packages.Values
                            .Where(package => package.SubstitutionImplementationId is not null)
                            .OrderBy(package => package.Identity.Name, StringComparer.Ordinal)
                            .Select(package => package.Identity + "=" + package.SubstitutionImplementationId));

                    diagnostics.Add(
                        new(
                            module.Path,
                            module.Name,
                            name,
                            import.Range,
                            $"Import '{name}' in '{module.Path}' {reason}. " +
                            $"Active substitutions: [{substitutions}]. Add the exposing package as a direct dependency " +
                            "or supply a substitution implementing the required module/API; substituted packages are not searched upstream."));

                    if (!importNames.ContainsKey((module.Path, name)))
                    {
                        importNames.Add(
                            (module.Path, name),
                            ReserveName("PineUnavailable.P" + ElmDependencyResolver.Fingerprint(module.Path + "\n" + name) + "." + name));
                    }

                    continue;
                }

                var target = visible[0];
                importNames[(module.Path, name)] = compilerNames[target.Path];

                ValidateExposing(import, module, name, exportedApi[target.Path], target.Path);
            }
        }

        var compilerSyntax =
            byPath.Values.ToImmutableDictionary(
                module => compilerNames[module.Path],
                module => RewriteModuleSyntax(
                    module.Parsed,
                    module.Name,
                    compilerNames[module.Path],
                    name => importNames.GetValueOrDefault((module.Path, name))),
                StringComparer.Ordinal);

        var prepared =
            FileTree.FromSetOfFilesWithStringPath(
                build.Sources.EnumerateFilesTransitive().Select(
                    file =>
                    {
                        var path = string.Join("/", file.path);

                        return
                            ((IReadOnlyList<string>)file.path,
                            byPath.TryGetValue(path, out var module) &&
                            (compilerNames[path] != module.Name ||
                            module.Parsed.Imports.Any(
                                import =>
                                importNames.TryGetValue(
                                    (path, string.Join(".", import.Value.ModuleName.Value)),
                                    out var target) &&
                                target != string.Join(".", import.Value.ModuleName.Value)))
                            ?
                            (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(
                                Avh4Format.FormatToString(compilerSyntax[compilerNames[path]]))
                            :
                            file.fileContent);
                    }));

        return
            build with
            {
                Sources = prepared,
                CompilerModuleNames = compilerNames.ToImmutable(),
                CompilerModuleSourcePaths =
                compilerNames.ToImmutableDictionary(item => item.Value, item => item.Key, StringComparer.Ordinal),
                CompilerModuleSyntax = compilerSyntax,
                ImportDiagnostics = diagnostics.ToImmutable(),
            };

        string ReserveName(string proposed)
        {
            var name = proposed;

            for (var index = 1; !reservedNames.Add(name); ++index)
                name = proposed + ".PineIdentity" + index;

            return name;
        }

        void ValidateExposing(
            Syntax.Node<Syntax.Import> import,
            PreparedModule module,
            string name,
            HashSet<(string name, bool type, bool constructors)> api,
            string targetPath)
        {
            if (import.Value.ExposingList?.ExposingList.Value is Syntax.Exposing.Explicit exposing)
            {
                foreach (var exposed in exposing.Nodes)
                {
                    var requested = ExposedApi(exposed.Value);

                    if (!api.Contains(requested))
                    {
                        diagnostics.Add(
                            new(
                                module.Path,
                                module.Name,
                                name,
                                import.Range,
                                $"Import '{name}' in '{module.Path}' requests '{requested.name}" +
                                (requested.constructors ? "(..)" : "") +
                                $"' but '{targetPath}' does not expose that API. " +
                                "Check the module's exposing list and the replacement implementation's supported API."));
                    }
                }
            }
        }

        void Fail(string name, string message) =>
            throw new ElmDependencyResolutionException(
                build.Resolution with
                {
                    Failures =
                    [
                    new(
                        ElmResolutionFailureKind.InvalidModuleImport,
                        name,
                        message,
                        build.Resolution.Requirements,
                        [
                        .. build.Resolution.Packages.Values.Select(package => package.Identity)
                        ])
                    ],
                    Fingerprint = null,
                });
    }

    private static HashSet<(string name, bool type, bool constructors)> NativeExposedApi(string moduleName)
    {
        if (moduleName is "Debug")
            return [("log", false, false), ("toString", false, false), ("todo", false, false)];

        var implicitImports = ElmCompilerInDotnet.ImplicitImportConfig.Default;

        var api =
            implicitImports.ValueImports
            .Where(item => item.Value.SequenceEqual(["Basics"]))
            .Select(item => (name: item.Key, type: false, constructors: false)).ToHashSet();

        api.UnionWith(
            implicitImports.OperatorToFunction.Where(item => item.Value.ModuleName.SequenceEqual(["Basics"]))
            .Select(item => (item.Key, false, false)));

        api.UnionWith(
            [
            ("Int", true, false), ("Float", true, false), ("Bool", true, false), ("Never", true, false), ("Order", true, false),
            ("Bool", true, true), ("Order", true, true),
            ("True", false, false), ("False", false, false), ("LT", false, false), ("EQ", false, false), ("GT", false, false)
            ]);

        return api;
    }

    private static HashSet<(string name, bool type, bool constructors)> GetExposedApi(Syntax.File file)
    {
        var declarations = new HashSet<(string name, bool type, bool constructors)>();

        foreach (var declaration in file.Declarations)
            switch (declaration.Value)
            {
                case Syntax.Declaration.FunctionDeclaration function:
                    declarations.Add((function.Function.Declaration.Value.Name.Value, false, false));
                    break;

                case Syntax.Declaration.ChoiceTypeDeclaration choice:
                    declarations.Add((choice.TypeDeclaration.Name.Value, true, false));
                    declarations.Add((choice.TypeDeclaration.Name.Value, true, true));
                    break;

                case Syntax.Declaration.AliasDeclaration alias:
                    declarations.Add((alias.TypeAlias.Name.Value, true, false));
                    break;

                case Syntax.Declaration.InfixDeclaration infix:
                    declarations.Add((infix.Infix.Operator.Value, false, false));
                    break;

                case Syntax.Declaration.PortDeclaration port:
                    declarations.Add((port.Signature.Name.Value, false, false));
                    break;

                default:
                    throw new NotImplementedException(
                        $"{nameof(GetExposedApi)} does not handle declaration variant: {declaration.Value.GetType().Name}");
            }

        var exposing =
            file.ModuleDefinition.Value switch
            {
                Syntax.Module.NormalModule normal => normal.ModuleData.ExposingList.Value,
                Syntax.Module.PortModule port => port.ModuleData.ExposingList.Value,
                Syntax.Module.EffectModule effect => effect.ModuleData.ExposingList.Value,

                _ =>
                throw new NotImplementedException(
                    $"{nameof(GetExposedApi)} does not handle module variant: {file.ModuleDefinition.Value.GetType().Name}"),
            };

        switch (exposing)
        {
            case Syntax.Exposing.All:
                return declarations;

            case Syntax.Exposing.Explicit explicitExposing:
                var api =
                    explicitExposing.Nodes
                    .Select(node => ExposedApi(node.Value)).Where(declarations.Contains).ToHashSet();

                api.UnionWith(
                    api.Where(item => item.constructors).Select(item => (item.name, item.type, false)).ToArray());

                return api;

            default:
                throw new NotImplementedException(
                    $"{nameof(GetExposedApi)} does not handle exposing variant: {exposing.GetType().Name}");
        }
    }

    private static (string name, bool type, bool constructors) ExposedApi(Syntax.TopLevelExpose expose) =>
        expose switch
        {
            Syntax.TopLevelExpose.FunctionExpose function => (function.Name, false, false),
            Syntax.TopLevelExpose.InfixExpose infix => (infix.Name, false, false),
            Syntax.TopLevelExpose.TypeOrAliasExpose type => (type.Name, true, false),
            Syntax.TopLevelExpose.TypeExpose type => (type.ExposedType.Name, true, type.ExposedType.Open is not null),

            _ =>
            throw new NotImplementedException(
                $"{nameof(ExposedApi)} does not handle expose variant: {expose.GetType().Name}"),
        };

    private static ReadOnlyMemory<byte> RewriteModule(
        Syntax.File parsed, string originalModuleName, string compilerModuleName, Func<string, string?> importedCompilerName) =>
        Encoding.UTF8.GetBytes(
            Avh4Format.FormatToString(
                RewriteModuleSyntax(parsed, originalModuleName, compilerModuleName, importedCompilerName)));

    private static Syntax.File RewriteModuleSyntax(
        Syntax.File parsed, string originalModuleName, string compilerModuleName, Func<string, string?> importedCompilerName)
    {
        var originalName = Syntax.Module.GetModuleName(parsed.ModuleDefinition.Value);
        var renamed = originalName with { Value = compilerModuleName.Split('.') };
        var qualifierAliases = new Dictionary<string, string>(StringComparer.Ordinal);

        var usedAliases =
            parsed.Imports.Select(
                import => string.Join(".", import.Value.ModuleAlias?.Alias.Value ?? import.Value.ModuleName.Value))
            .Append(originalModuleName).ToHashSet(StringComparer.Ordinal);

        Syntax.Module definition =
            parsed.ModuleDefinition.Value switch
            {
                Syntax.Module.NormalModule normal =>
                normal with { ModuleData = normal.ModuleData with { ModuleName = renamed } },

                Syntax.Module.PortModule port =>
                port with { ModuleData = port.ModuleData with { ModuleName = renamed } },

                Syntax.Module.EffectModule effect =>
                effect with { ModuleData = effect.ModuleData with { ModuleName = renamed } },

                _ =>
                throw new NotImplementedException(
                    $"{nameof(RewriteModuleSyntax)} does not handle module variant: {parsed.ModuleDefinition.Value.GetType().Name}"),
            };

        var rewritten =
            parsed with
            {
                ModuleDefinition = parsed.ModuleDefinition with { Value = definition },
                Imports =
                [
                .. parsed.Imports.Select(
                    import =>
                    {
                        var name = string.Join(".", import.Value.ModuleName.Value);
                        var target = importedCompilerName(name);

                        if (target is null || target == name)
                            return import;

                        var alias = import.Value.ModuleAlias;

                        if (alias is null)
                        {
                            var aliasName = name;

                            if (import.Value.ModuleName.Value.Count > 1)
                            {
                                if (!qualifierAliases.TryGetValue(name, out aliasName))
                                {
                                    var index = 1;

                                    do
                                    {
                                        aliasName = "PineDependency" + index++;
                                    }
                                    while (!usedAliases.Add(aliasName));

                                    qualifierAliases.Add(name, aliasName);
                                }
                            }

                            alias =
                                (import.Value.ImportTokenLocation, import.Value.ModuleName with { Value = [aliasName] });
                        }

                        return
                            import with
                            {
                                Value =
                                import.Value with
                                {
                                    ModuleName = import.Value.ModuleName with { Value = target.Split('.') },
                                    ModuleAlias = alias,
                                },
                            };
                    })
                ],
            };

        if (originalModuleName != compilerModuleName &&
            !parsed.Imports.Any(import =>
                string.Join(".", import.Value.ModuleAlias?.Alias.Value ?? import.Value.ModuleName.Value) == originalModuleName))
            qualifierAliases[originalModuleName] = compilerModuleName;

        return
            qualifierAliases.Count is 0
            ?
            rewritten
            :
            new ElmModuleQualifierRewriting(qualifierAliases).Rewrite(rewritten);
    }

    private static ElmResolvedBuild ValidateImports(ElmResolvedBuild build, FileTree appSources)
    {
        var modules = new Dictionary<string, List<Module>>(StringComparer.Ordinal);
        var byPath = new Dictionary<string, Module>(StringComparer.Ordinal);

        void Add(FileTree sources, string? owner)
        {
            foreach (var file in sources.EnumerateFilesTransitive())
            {
                if (!file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) ||
                    owner is not null && (file.path.Count < 2 || file.path[0] != "src"))
                    continue;

                var text = Encoding.UTF8.GetString(file.fileContent.Span);

                if (ElmModule.ParseModuleName(text).IsOkOrNull() is not { } name)
                    continue;

                var moduleName = string.Join(".", name);

                var path =
                    owner is null
                    ?
                    string.Join("/", file.path)
                    :
                    "elm-packages/" + owner + "/" + string.Join("/", file.path);

                var module =
                    new Module(
                        moduleName,
                        path,
                        owner,
                        [
                        .. ElmModule.ParseModuleImportedModulesNames(text).Select(import => string.Join(".", import))
                        ]);

                if (!modules.TryGetValue(moduleName, out var entries))
                    modules.Add(moduleName, entries = []);

                entries.Add(module);
                byPath.Add(path, module);
            }
        }

        Add(appSources, null);

        foreach (var package in build.PackageSources)
            Add(package.Value, package.Key);

        var directPackages =
            build.Resolution.Requirements
            .Where(
                item =>
                item.DeclaringPackage is null &&
                item.Scope is ElmDependencyScope.Direct or ElmDependencyScope.TestDirect)
            .Select(item => item.PackageName).ToHashSet(StringComparer.Ordinal);

        var visited = new HashSet<string>(StringComparer.Ordinal);
        var resolvedImports = new Dictionary<(string path, string name), Module>();
        var queue = new Queue<(Module module, ImmutableArray<string> chain)>();

        foreach (var root in build.RootFilePaths)
        {
            var path = string.Join("/", root);

            if (!byPath.TryGetValue(path, out var module))
            {
                Fail(
                    "",
                    $"Compilation root '{path}' is not an Elm source in the selected project's source directories or tests directory.");
            }
            else
            {
                if (modules[module.Name].Count(item => item.Owner == module.Owner) != 1)
                {
                    Fail(
                        module.Name,
                        $"Compilation root '{path}' has duplicate module name '{module.Name}' in the selected project's sources.");
                }

                queue.Enqueue((module, [module.Path]));
            }
        }

        // Implicit compiler imports also need a validated source closure, including non-substituted core packages.
        foreach (var module in byPath.Values)
            if (module.Owner is not null &&
                (module.Owner is "elm/core" ||
                build.Resolution.Packages[module.Owner].SubstitutionImplementationId is not null))
                queue.Enqueue((module, [module.Path]));

        while (queue.TryDequeue(out var next))
        {
            if (!visited.Add(next.module.Path))
                continue;

            if (modules[next.module.Name].Count(module => module.Owner == next.module.Owner) != 1)
            {
                Fail(
                    next.module.Name,
                    $"Module '{next.module.Name}' is declared more than once in '{next.module.Owner ?? "the selected project"}'. " +
                    $"Import path: {string.Join(" -> ", next.chain)}.");
            }

            if (next.module.Owner is null && modules[next.module.Name].Any(module => module.Owner is "elm/core"))
            {
                Fail(
                    next.module.Name,
                    $"Project module '{next.module.Name}' in '{next.module.Path}' conflicts with an elm/core compiler module. " +
                    "Rename the project module to preserve the compiler's implicit core imports.");
            }

            var visiblePackages =
                next.module.Owner is null
                ?
                directPackages
                :
                build.Resolution.Packages[next.module.Owner].Dependencies.Select(item => item.PackageName).ToHashSet(
                    StringComparer.Ordinal);

            foreach (var import in next.module.Imports)
            {
                var candidates = modules.GetValueOrDefault(import) ?? [];

                var visible =
                    candidates.Where(
                        module =>
                        module.Owner == next.module.Owner ||
                        module.Owner is not null && visiblePackages.Contains(module.Owner) &&
                        build.Resolution.Packages[module.Owner].ExposedModules.Contains(import)).ToArray();

                // Basics and Debug are compiler-native modules, without corresponding package source files.
                if (import is "Basics" or "Debug" && visible.Length is 0 &&
                    (visiblePackages.Contains("elm/core") || next.module.Owner is "elm/core"))
                    continue;

                if (visible.Length is not 1)
                {
                    var reason =
                        visible.Length > 1
                        ?
                        "is ambiguous between " + string.Join(", ", visible.Select(module => module.Path))
                        :
                        candidates.Count > 0
                        ?
                        "exists only in private modules or indirect/undeclared dependencies: " +
                        string.Join(", ", candidates.Select(module => module.Path))
                        :
                        "has no implementation in the resolved environment";

                    var substitutions =
                        string.Join(
                            ", ",
                            build.Resolution.Packages.Values
                            .Where(package => package.SubstitutionImplementationId is not null)
                            .Select(package => package.Identity + "=" + package.SubstitutionImplementationId));

                    Fail(
                        import,
                        $"Import '{import}' in '{next.module.Path}' {reason}.\n" +
                        $"Import path: {string.Join(" -> ", next.chain)} -> {import}.\n" +
                        $"Active substitutions: [{substitutions}]. Add the exposing package as a direct dependency " +
                        "or supply a substitution implementing the required module/API; substituted packages are not searched upstream.");
                }
                else
                {
                    resolvedImports[(next.module.Path, import)] = visible[0];
                    queue.Enqueue((visible[0], next.chain.Add(visible[0].Path)));
                }
            }
        }

        var compilerNames =
            byPath.Values.Where(module => visited.Contains(module.Path))
            .ToImmutableDictionary(
                module => module.Path,
                module => modules[module.Name].Count(item => visited.Contains(item.Path)) <= 1 ||
                    module.Owner is null or "elm/core"
                ?
                module.Name
                :
                "PinePackage.P" + ElmDependencyResolver.Fingerprint(module.Owner ?? module.Path) + "." + module.Name);

        var prepared =
            FileTree.FromSetOfFilesWithStringPath(
                build.Sources.EnumerateFilesTransitive()
                .Where(
                    file => !file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) ||
                        visited.Contains(string.Join("/", file.path)))
                .Select(
                    file =>
                    {
                        var path = string.Join("/", file.path);

                        if (!compilerNames.TryGetValue(path, out var newName))
                            return ((IReadOnlyList<string>)file.path, file.fileContent);

                        var module = byPath[path];

                        var importsChanged =
                            module.Imports.Any(
                                import =>
                                resolvedImports.TryGetValue((path, import), out var target) &&
                                compilerNames[target.Path] != import);

                        if (newName == module.Name && !importsChanged)
                            return ((IReadOnlyList<string>)file.path, file.fileContent);

                        var parsed =
                            ElmSyntaxParser.ParseModuleText(Encoding.UTF8.GetString(file.fileContent.Span))
                            .Extract(
                                error => throw new InvalidOperationException(
                                    $"Cannot disambiguate module '{module.Name}' in '{path}': {error}"));

                        return
                            ((IReadOnlyList<string>)file.path,
                            RewriteModule(
                                parsed,
                                module.Name,
                                newName,
                                name =>
                                resolvedImports.TryGetValue((path, name), out var target)
                                ?
                                compilerNames[target.Path]
                                :
                                null));
                    }));

        return
            build with
            {
                Sources = prepared,
                CompilerModuleNames = compilerNames,
                CompilerModuleSourcePaths =
                compilerNames.ToImmutableDictionary(item => item.Value, item => item.Key, StringComparer.Ordinal),
            };

        void Fail(string name, string message)
        {
            var failure =
                new ElmResolutionFailure(
                    ElmResolutionFailureKind.InvalidModuleImport,
                    name,
                    message,
                    build.Resolution.Requirements,
                    [.. build.Resolution.Packages.Values.Select(package => package.Identity)]);

            throw new ElmDependencyResolutionException(build.Resolution with { Failures = [failure], Fingerprint = null });
        }
    }

    private sealed record Module(string Name, string Path, string? Owner, ImmutableArray<string> Imports);

    private sealed record PreparedModule(string Name, string Path, string? Owner, Syntax.File Parsed);
}
