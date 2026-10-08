using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

using Syntax = Pine.Core.Elm.ElmSyntax.SyntaxModel;
using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>A source-located frontend diagnostic, independent of rendering and executable emission.</summary>
public sealed record ElmCompilerDiagnostic(
    string FilePath,
    Syntax.Range Range,
    string Message,
    string Code,
    DeclQualifiedName? Declaration = null)
{
    /// <summary>Source definitions, references, and import locations explaining the diagnostic.</summary>
    public IReadOnlyList<ElmCompilerDiagnosticRelatedLocation> RelatedLocations { get; init; } = [];
}

/// <summary>A source location related to a frontend error, without coupling the compiler to an editor protocol.</summary>
public sealed record ElmCompilerDiagnosticRelatedLocation(string FilePath, Syntax.Range Range, string Message);

/// <summary>Source declaration information; unavailable type information is accompanied by diagnostics.</summary>
public sealed record ElmDeclarationQueryResult(
    string FilePath,
    Syntax.Node<Syntax.Declaration> Declaration,
    TypeInference.InferredType? Type,
    IReadOnlyList<ElmCompilerDiagnostic> Diagnostics);

public partial class ElmCompiler
{
    /// <summary>Reports application errors without selecting entry points, emitting code, or evaluating values.</summary>
    public static IReadOnlyList<ElmCompilerDiagnostic> AnalyzeApplication(
        FileTree sources,
        bool includeBundledKernelModules = true,
        IDictionary<string, Result<ElmSyntaxParseError, Syntax.File>>? sourceParseCache = null,
        IDictionary<(Syntax.Node<Syntax.Declaration> Declaration, string Scope),
            DeclarationCanonicalizationCacheEntry>? canonicalizationCache = null) =>
        AnalyzeSources(
            sources,
            sources.EnumerateFilesTransitive()
            .Where(file => file.path[0] is not "elm-packages")
            .Select(file => string.Join("/", file.path)).ToHashSet(StringComparer.Ordinal),
            includeBundledKernelModules,
            sourceParseCache,
            canonicalizationCache);

    /// <summary>Analyzes selected project sources while preserving resolved package visibility.</summary>
    public static IReadOnlyList<ElmCompilerDiagnostic> AnalyzeApplication(
        ElmResolvedBuild build,
        IDictionary<string, Result<ElmSyntaxParseError, Syntax.File>>? sourceParseCache = null,
        IDictionary<(Syntax.Node<Syntax.Declaration> Declaration, string Scope),
            DeclarationCanonicalizationCacheEntry>? canonicalizationCache = null)
    {
        var applicationPaths =
            build.ProjectSources.EnumerateFilesTransitive()
            .Select(file => string.Join("/", file.path)).ToHashSet(StringComparer.Ordinal);

        var diagnostics =
            AnalyzeSources(
                build.Sources,
                applicationPaths,
                false,
                sourceParseCache,
                canonicalizationCache,
                build.CompilerModuleSyntax,
                validateSourceImports: false);

        return
            [
            .. diagnostics,
            .. build.ImportDiagnostics.Where(diagnostic => applicationPaths.Contains(diagnostic.FilePath))
            .Select(
                diagnostic => new ElmCompilerDiagnostic(
                    diagnostic.FilePath,
                    diagnostic.ImportRange,
                    diagnostic.Message,
                    "elm-import"))
            ];
    }

    /// <summary>Queries a declaration without compiling it, retaining its definition even when analysis finds errors.</summary>
    public static Result<string, ElmDeclarationQueryResult> QueryDeclaration(
        FileTree sources,
        DeclQualifiedName name,
        bool includeBundledKernelModules = true,
        IDictionary<string, Result<ElmSyntaxParseError, Syntax.File>>? sourceParseCache = null,
        IDictionary<(Syntax.Node<Syntax.Declaration> Declaration, string Scope),
            DeclarationCanonicalizationCacheEntry>? canonicalizationCache = null,
        IReadOnlyDictionary<string, Syntax.File>? parsedSources = null)
    {
        var parseDiagnostics = new List<ElmCompilerDiagnostic>();

        var parsed =
            ReadSourceFiles(sources, includeBundledKernelModules, sourceParseCache, parseDiagnostics, parsedSources);

        var candidates =
            parsed.SelectMany(
                source => source.File.Declarations
                .Where(
                    node =>
                    DeclQualifiedName.Create(
                        Syntax.Module.GetModuleName(source.File.ModuleDefinition.Value).Value,
                        Canonicalization.GetDeclarationName(node.Value)).Equals(name))
                .Select(node => (source.Path, Declaration: node)))
            .ToList();

        if (candidates.Count is 0)
        {
            var errors =
                parseDiagnostics.Where(
                    diagnostic =>
                    diagnostic.FilePath.EndsWith(string.Join("/", name.Namespaces) + ".elm", StringComparison.Ordinal));

            return
                $"Cannot establish the declaration '{name.FullName}'." +
                string.Concat(errors.Select(error => "\n" + error.Message));
        }

        if (candidates.Count is not 1)
            return $"Declaration '{name.FullName}' is ambiguous between multiple definitions.";

        var result =
            Canonicalization.CanonicalizeDeclarations(
                [.. parsed.Select(source => source.File)],
                [name],
                cache: canonicalizationCache);

        if (result.IsErrOrNull() is { } error)
            return error;

        var canonicalized = result.Extract(error => throw new InvalidOperationException(error));
        var diagnostics = DeclarationDiagnostics(parsed, canonicalized);
        var declarations = AbstractDeclarations(canonicalized);
        diagnostics.AddRange(TypeDiagnostics(parsed, canonicalized, declarations));
        var type = InferDeclarationType(name, declarations);

        if (type is null && diagnostics.Count is 0 &&
            candidates[0].Declaration.Value is Syntax.Declaration.FunctionDeclaration)
        {
            diagnostics.Add(
                new(
                    candidates[0].Path,
                    candidates[0].Declaration.Range,
                    "The current partial inference cannot establish this declaration's type.",
                    "elm-inference",
                    name));
        }

        return
            new ElmDeclarationQueryResult(
                candidates[0].Path,
                candidates[0].Declaration,
                diagnostics.Count is 0 ? type : null,
                diagnostics);
    }

    /// <summary>Queries prepared sources without losing original definition and diagnostic ranges.</summary>
    public static Result<string, ElmDeclarationQueryResult> QueryDeclaration(
        ElmResolvedBuild build,
        DeclQualifiedName name,
        IDictionary<string, Result<ElmSyntaxParseError, Syntax.File>>? sourceParseCache = null,
        IDictionary<(Syntax.Node<Syntax.Declaration> Declaration, string Scope),
            DeclarationCanonicalizationCacheEntry>? canonicalizationCache = null) =>
        QueryDeclaration(
            build.Sources,
            name,
            false,
            sourceParseCache,
            canonicalizationCache,
            build.CompilerModuleSyntax);

    private static IReadOnlyList<ElmCompilerDiagnostic> AnalyzeSources(
        FileTree sources,
        IReadOnlySet<string> applicationPaths,
        bool includeBundledKernelModules,
        IDictionary<string, Result<ElmSyntaxParseError, Syntax.File>>? sourceParseCache,
        IDictionary<(Syntax.Node<Syntax.Declaration> Declaration, string Scope),
            DeclarationCanonicalizationCacheEntry>? canonicalizationCache,
        IReadOnlyDictionary<string, Syntax.File>? parsedSources = null,
        bool validateSourceImports = true)
    {
        var diagnostics = new List<ElmCompilerDiagnostic>();

        var parsed =
            ReadSourceFiles(sources, includeBundledKernelModules, sourceParseCache, diagnostics, parsedSources);

        var files = parsed.Select(source => source.File).ToList();

        var duplicateScopes =
            parsed.GroupBy(
                source =>
                string.Join(".", Syntax.Module.GetModuleName(source.File.ModuleDefinition.Value).Value))
            .Where(group => group.Count() > 1).Select(group => group.Key).ToHashSet(StringComparer.Ordinal);

        var candidates =
            parsed.SelectMany(
                source => source.File.Declarations.Select(
                    node =>
                    (source.Path,
                    Node: node,
                    Name: DeclQualifiedName.Create(
                        Syntax.Module.GetModuleName(source.File.ModuleDefinition.Value).Value,
                        Canonicalization.GetDeclarationName(node.Value)))))
            .ToList();

        var duplicates =
            candidates.GroupBy(candidate => candidate.Name)
            .Where(group => group.Count() > 1).Select(group => group.Key).ToHashSet();

        foreach (var source in parsed.Where(source => applicationPaths.Contains(source.Path)))
        {
            var namespaceName = string.Join(".", Syntax.Module.GetModuleName(source.File.ModuleDefinition.Value).Value);

            if (duplicateScopes.Contains(namespaceName))
            {
                diagnostics.Add(
                    new(
                        source.Path,
                        source.File.ModuleDefinition.Range,
                        "Duplicate module namespace '" + namespaceName + "'.",
                        "elm-name"));
            }

            foreach (var error in Canonicalization.ValidateImports(
                validateSourceImports ? source.File : source.File with { Imports = [] },
                files))
                diagnostics.Add(new(source.Path, error.Range, RenderCanonicalizationError(error), "elm-name"));
        }

        foreach (var candidate in candidates.Where(
            candidate =>
            applicationPaths.Contains(candidate.Path) && duplicates.Contains(candidate.Name)))
            diagnostics.Add(
                new(
                    candidate.Path,
                    candidate.Node.Range,
                    "Duplicate declaration '" + candidate.Name.FullName + "'.",
                    "elm-name",
                    candidate.Name));

        var roots =
            candidates.Where(
                candidate =>
                applicationPaths.Contains(candidate.Path) &&
                !duplicates.Contains(candidate.Name) &&
                !duplicateScopes.Contains(string.Join(".", candidate.Name.Namespaces)))
            .Select(candidate => candidate.Name).Distinct().ToArray();

        var result = Canonicalization.CanonicalizeDeclarations(files, roots, cache: canonicalizationCache);

        if (result.IsErrOrNull() is { } failure)
        {
            throw new InvalidOperationException(
                "Application declaration enumeration produced invalid canonicalization roots: " + failure);
        }

        var canonicalized = result.Extract(error => throw new InvalidOperationException(error));
        diagnostics.AddRange(DeclarationDiagnostics(parsed, canonicalized));

        foreach (var root in roots)
        {
            if (canonicalized.Declarations[root].Errors.Count is not 0 ||
                FindNamingError(root, canonicalized) is not { } cause)
                continue;

            var definition = candidates.Single(candidate => candidate.Name.Equals(root));

            var causePath =
                parsed.First(
                    source =>
                    Syntax.Module.GetModuleName(source.File.ModuleDefinition.Value).Value.SequenceEqual(
                        cause.Name.Namespaces)).Path;

            diagnostics.Add(
                new(
                    definition.Path,
                    definition.Node.Range,
                    $"Cannot complete analysis of '{root.FullName}': required declaration '{cause.Name.FullName}' " +
                    $"at {causePath}:{cause.Error.Range.Start.Row}:{cause.Error.Range.Start.Column} has naming errors. " +
                    RenderCanonicalizationError(cause.Error),
                    "elm-dependency",
                    root)
                {
                    RelatedLocations =
                    [
                    new(
                        causePath,
                        cause.Error.Range,
                        $"Naming error in required declaration '{cause.Name.FullName}'.")
                    ]
                });
        }

        var declarations = AbstractDeclarations(canonicalized);
        diagnostics.AddRange(TypeDiagnostics(parsed, canonicalized, declarations));

        return
            [
            .. diagnostics.Where(diagnostic => applicationPaths.Contains(diagnostic.FilePath))
            .Distinct().OrderBy(diagnostic => diagnostic.FilePath, StringComparer.Ordinal)
            .ThenBy(diagnostic => diagnostic.Range.Start.Row)
            .ThenBy(diagnostic => diagnostic.Range.Start.Column)
            ];
    }

    private static IReadOnlyList<ElmCompilerDiagnostic> TypeDiagnostics(
        IReadOnlyList<(string Path, Syntax.File File)> parsed,
        DeclarationCanonicalizationResult canonicalized,
        ImmutableDictionary<DeclQualifiedName, Abstract.Declaration> declarations)
    {
        var diagnostics = new List<ElmCompilerDiagnostic>();

        var aliases =
            BuildModuleShellsFromFlatDeclarations(declarations)
            .SelectMany(
                file => TypeInference.BuildTypeAliasDefinitions(
                    file,
                    string.Join(".", Abstract.Module.GetModuleName(file.ModuleDefinition))))
            .ToImmutableDictionary();

        var functionTypes = BuildFunctionTypes(declarations);

        foreach (var (name, canonical) in canonicalized.Declarations)
        {
            if (HasNamingErrors(name, canonicalized) ||
                canonical.Value.Value is not Syntax.Declaration.FunctionDeclaration sourceFunction ||
                declarations[name] is not Abstract.Declaration.FunctionDeclaration function ||
                function.Function.Signature is null)
                continue;

            var moduleName = string.Join(".", name.Namespaces);
            var parameters = function.Function.Declaration.Arguments;

            var parameterNames =
                parameters.Select((pattern, index) => (pattern, index))
                .Where(entry => entry.pattern is Abstract.Pattern.VarPattern)
                .ToDictionary(entry => ((Abstract.Pattern.VarPattern)entry.pattern).Name, entry => entry.index);

            var inferred =
                TypeInference.InferExpressionType(
                    function.Function.Declaration.Expression,
                    parameterNames,
                    ExtractParameterTypes(function.Function, null, functionTypes, moduleName)
                    .ToDictionary(
                        entry => entry.Key,
                        entry => TypeInference.ExpandTypeAliases(entry.Value, aliases, name.Namespaces)),
                    null,
                    moduleName,
                    functionTypes);

            var declared =
                TypeInference.ExpandTypeAliases(TypeInference.GetFunctionReturnType(function), aliases, name.Namespaces);

            if (TypeInference.TryUnify(declared, inferred).IsErrOrNull() is { } typeError)
            {
                var path =
                    parsed.First(
                        source =>
                        Syntax.Module.GetModuleName(source.File.ModuleDefinition.Value).Value.SequenceEqual(
                            name.Namespaces)).Path;

                diagnostics.Add(
                    new(
                        path,
                        sourceFunction.Function.Declaration.Value.Expression.Range,
                        typeError,
                        "elm-type",
                        name));
            }
        }

        return diagnostics;
    }

    private static bool HasNamingErrors(DeclQualifiedName root, DeclarationCanonicalizationResult canonicalized) =>
        FindNamingError(root, canonicalized) is not null;

    private static (DeclQualifiedName Name, CanonicalizationError Error)? FindNamingError(
        DeclQualifiedName root, DeclarationCanonicalizationResult canonicalized)
    {
        var pending = new Queue<DeclQualifiedName>();
        var visited = new HashSet<DeclQualifiedName>();
        pending.Enqueue(root);

        while (pending.TryDequeue(out var name))
        {
            if (!visited.Add(name))
                continue;

            if (canonicalized.Declarations[name].Errors.Count > 0)
                return (name, canonicalized.Declarations[name].Errors[0]);

            foreach (var dependency in (canonicalized.Dependencies.GetValueOrDefault(name) ?? []).Order())
                pending.Enqueue(dependency);
        }

        return null;
    }

    private static Result<ElmSyntaxParseError, Syntax.File> ParseSource(
        string text,
        IDictionary<string, Result<ElmSyntaxParseError, Syntax.File>>? cache)
    {
        if (cache is not null && cache.TryGetValue(text, out var cached))
            return cached;

        var result = ElmSyntaxParser.ParseModuleText(text);

        cache?[text] = result;

        return result;
    }

    private static List<(string Path, Syntax.File File)> ReadSourceFiles(
        FileTree sources,
        bool includeBundledKernelModules,
        IDictionary<string, Result<ElmSyntaxParseError, Syntax.File>>? cache,
        List<ElmCompilerDiagnostic> diagnostics,
        IReadOnlyDictionary<string, Syntax.File>? parsedSources = null)
    {
        var combined =
            includeBundledKernelModules
            ?
            FileTree.MergeFiles(sources, ElmInElm.BundledFiles.ElmKernelModulesDefault.Value)
            :
            sources;

        var files = new List<(string Path, Syntax.File File)>();

        foreach (var source in combined.EnumerateFilesTransitive()
            .Where(source => source.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase)))
        {
            var path = string.Join("/", source.path);
            var text = Encoding.UTF8.GetString(source.fileContent.Span);
            var header = ElmSyntaxParser.ParseModuleHeader(text).IsOkOrNull();

            var result =
                parsedSources is not null && header is not null &&
                parsedSources.TryGetValue(string.Join(".", header.ModuleName), out var supplied)
                ?
                Result<ElmSyntaxParseError, Syntax.File>.ok(supplied)
                :
                ParseSource(text, cache);

            if (result.IsErrOrNullable() is { } error)
            {
                diagnostics.Add(new(path, error.Region, ElmSyntaxErrorRenderer.RenderConcise(error), "elm-syntax"));
                continue;
            }

            var file = result.Extract(error => throw new InvalidOperationException(error.ToString()));

            foreach (var recoveredError in file.AdditionalParseErrors.Concat(
                file.IncompleteDeclarations.Select(node => node.Value.ParseError)))
                diagnostics.Add(
                    new(
                        path,
                        recoveredError.Region,
                        ElmSyntaxErrorRenderer.RenderConcise(recoveredError),
                        "elm-syntax"));

            files.Add((path, file));
        }

        return files;
    }

    private static List<ElmCompilerDiagnostic> DeclarationDiagnostics(
        IReadOnlyList<(string Path, Syntax.File File)> sources,
        DeclarationCanonicalizationResult canonicalized)
    {
        var files = sources.Select(source => source.File).ToArray();

        var paths =
            sources.GroupBy(
                source =>
                string.Join(".", Syntax.Module.GetModuleName(source.File.ModuleDefinition.Value).Value))
            .ToDictionary(group => group.Key, group => group.First().Path, StringComparer.Ordinal);

        var diagnostics = new List<ElmCompilerDiagnostic>();

        foreach (var (name, canonical) in canonicalized.Declarations)
        {
            if (canonical.Errors.Count is 0)
                continue;

            var path = paths[string.Join(".", name.Namespaces)];

            var chain =
                BuildSourceDependencyChain(
                    canonicalized.DependencyChains[name],
                    files,
                    paths,
                    canonicalized.References);

            foreach (var error in canonical.Errors)
            {
                var diagnostic =
                    CompleteCanonicalizationDiagnostic(
                        new(name.FullName, path, error, chain),
                        files,
                        paths);

                diagnostics.Add(
                    new(
                        path,
                        error.Range,
                        CompilationError.CanonicalizationErrors.RenderDiagnostic(
                            diagnostic,
                            includeDependencyChain: false),
                        "elm-name",
                        name)
                    {
                        RelatedLocations = DiagnosticRelatedLocations(diagnostic)
                    });
            }
        }

        return diagnostics;
    }

    private static IReadOnlyList<ElmCompilerDiagnosticRelatedLocation> DiagnosticRelatedLocations(
        CompilationError.CanonicalizationDiagnostic diagnostic)
    {
        var related = new List<ElmCompilerDiagnosticRelatedLocation>();

        for (var index = 0; index < diagnostic.DependencyChain.Count; index++)
        {
            var item = diagnostic.DependencyChain[index];

            if (item.FilePath is { } path && item.DeclarationRange is { } declarationRange)
                related.Add(new(path, declarationRange, "Declaration '" + item.DeclarationName + "'."));

            if (index > 0 && diagnostic.DependencyChain[index - 1].FilePath is { } referencePath)
            {
                if (item.ReferenceRange is { } referenceRange)
                {
                    related.Add(
                        new(
                            referencePath,
                            referenceRange,
                            $"'{item.ReferencedBy}' references '{item.ReferencedName ?? item.DeclarationName}'."));
                }

                if (item.Import?.Range is { } importRange)
                    related.Add(new(referencePath, importRange, "Import providing the referenced declaration's scope."));
            }
        }

        if (diagnostic.TargetSourcePath is { } targetPath && diagnostic.TargetSourceRange is { } targetRange)
            related.Add(new(targetPath, targetRange, "Supplied target source searched during name resolution."));

        if (diagnostic.ReferenceImport?.Range is { } failedImport)
            related.Add(new(diagnostic.FilePath, failedImport, "Import relevant to the unresolved reference."));

        return [.. related.Distinct()];
    }

    private static ImmutableDictionary<DeclQualifiedName, Abstract.Declaration> AbstractDeclarations(
        DeclarationCanonicalizationResult canonicalized) =>
        canonicalized.Declarations.ToImmutableDictionary(
            entry => entry.Key,
            entry => Abstract.ConvertFromConcrete.FromDeclaration(entry.Value.Value.Value));

    private static Dictionary<DeclQualifiedName, FunctionTypeInfo> BuildFunctionTypes(
        IReadOnlyDictionary<DeclQualifiedName, Abstract.Declaration> declarations) =>
        declarations.Where(entry => entry.Value is Abstract.Declaration.FunctionDeclaration)
        .ToDictionary(
            entry => entry.Key,
            entry => new FunctionTypeInfo(
                TypeInference.GetFunctionReturnType((Abstract.Declaration.FunctionDeclaration)entry.Value),
                TypeInference.GetFunctionParameterTypes((Abstract.Declaration.FunctionDeclaration)entry.Value)));

    private static TypeInference.InferredType? InferDeclarationType(
        DeclQualifiedName name,
        IReadOnlyDictionary<DeclQualifiedName, Abstract.Declaration> declarations)
    {
        if (declarations[name] is not Abstract.Declaration.FunctionDeclaration function)
            return null;

        if (function.Function.Signature is { } signature)
            return TypeInference.TypeAnnotationToInferredType(signature.TypeAnnotation);

        var knownTypes = BuildFunctionTypes(declarations);

        var parameterNames =
            function.Function.Declaration.Arguments
            .Select((pattern, index) => (pattern, index))
            .Where(entry => entry.pattern is Abstract.Pattern.VarPattern)
            .ToDictionary(entry => ((Abstract.Pattern.VarPattern)entry.pattern).Name, entry => entry.index);

        var parameterTypes =
            ExtractParameterTypes(
                function.Function,
                null,
                knownTypes,
                string.Join(".", name.Namespaces));

        if (function.Function.Declaration.Arguments.Any(
            pattern =>
            pattern is not Abstract.Pattern.VarPattern parameter || !parameterTypes.ContainsKey(parameter.Name)))
            return null;

        var returnType =
            TypeInference.InferExpressionType(
                function.Function.Declaration.Expression,
                parameterNames,
                parameterTypes,
                null,
                string.Join(".", name.Namespaces),
                knownTypes);

        return
            returnType is TypeInference.InferredType.UnknownType
            ?
            null
            :
            TypeInference.BuildFunctionType(function.Function.Declaration.Arguments, parameterTypes, returnType);
    }
}
