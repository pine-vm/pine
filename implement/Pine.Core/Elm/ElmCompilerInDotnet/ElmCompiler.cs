using Pine.Core.CodeAnalysis;
using Pine.Core.Files;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

using SyntaxModelTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;
using ElmSyntaxAbstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using IrTypes = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Canonical source syntax for diagnostics and the actual compiler-owned result consumed by emission.
/// </summary>
public record CompilationPipelineStageResults(
    IReadOnlyList<SyntaxModelTypes.File> Canonicalized,
    ImmutableDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration> CanonicalDeclarations,
    CompilerModel.Program CompilerProgram)
{
    /// <summary>
    /// Projects the actual optimized functions to emission IR without repeating any lowering or name resolution.
    /// </summary>
    public ImmutableDictionary<CompilerModel.FunctionId, IrTypes.FunctionStruct> FunctionsForEmission =>
        CompilerModel.ToIntermediateRepresentation.Convert(CompilerProgram);

    /// <summary>
    /// Fully qualified declarations selected as compilation roots before dependency filtering.
    /// </summary>
    public IReadOnlySet<DeclQualifiedName> RootDeclarationNames { get; init; } =
        ImmutableHashSet<DeclQualifiedName>.Empty;

    /// <summary>Original source references used to explain dependencies after lowering.</summary>
    public IReadOnlyDictionary<DeclQualifiedName, IReadOnlyList<DeclarationReference>> SourceReferences { get; init; } =
        ImmutableDictionary<DeclQualifiedName, IReadOnlyList<DeclarationReference>>.Empty;

    /// <summary>Compiler-qualified source namespaces mapped to their original file paths.</summary>
    public IReadOnlyDictionary<string, string> SourceFilePaths { get; init; } =
        ImmutableDictionary<string, string>.Empty;
}

/// <summary>
/// Methods for compiling Elm source code and environments into Pine values, using the standard packaging for
/// Elm functions and modules as found at
/// <see href="https://github.com/pine-vm/pine/blob/391100e6734a50d2bede29ee49bca1afc8868fed/implement/pine/Elm/elm-compiler/src/FirCompiler.elm#L2179-L2242"></see>
/// and
/// <see href="https://github.com/pine-vm/pine/blob/391100e6734a50d2bede29ee49bca1afc8868fed/implement/pine/Elm/elm-compiler/src/ElmCompiler.elm"></see>
/// <para>
/// For an overview of the compiler implementation, see the file `elm-compiler-implementation-guide.md`
/// </para>
/// </summary>
public partial class ElmCompiler
{
    private sealed record CanonicalizationBoundaryResult(
        IReadOnlyList<SyntaxModelTypes.File> ConcreteModules,
        IReadOnlySet<DeclQualifiedName> RootDeclarations)
    {
        public ImmutableDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration> CanonicalDeclarations { get; init; } =
            [];

        public IReadOnlyDictionary<DeclQualifiedName, IReadOnlyList<DeclarationReference>> References { get; init; } =
            ImmutableDictionary<DeclQualifiedName, IReadOnlyList<DeclarationReference>>.Empty;

        public IReadOnlyDictionary<string, string> FilePaths { get; init; } = ImmutableDictionary<string, string>.Empty;
    }

    /// <summary>
    /// Default configuration for the Elm syntax transformations run as part of the standard compilation pipeline.
    /// </summary>
    public static readonly ElmSyntaxOptimizationConfig.SyntaxOptimizationEnabled SyntaxOptimizationConfigDefault =
        new();

    /// <summary>
    /// Names of Elm modules that are implemented natively in the .NET assembly and must not be
    /// compiled from Elm source files (e.g. bundled elm-kernel-modules).
    /// The .NET assembly ships its own implementation of these modules via classes such as
    /// <see cref="CoreLibraryModule.CoreBasics"/>, superseding the corresponding .elm source.
    /// </summary>
    private static readonly FrozenSet<string> s_nativelyImplementedModuleNames =
        FrozenSet.Create(
            [
            "Basics",
            "Debug",
            ]);

    /// <summary>
    /// Compiles an interactive Elm environment from the given source tree.
    /// </summary>
    public static Result<string, CompilationPipelineStageResults> LowerToCompilerModel(
        FileTree appCodeTree,
        IReadOnlyList<DeclQualifiedName> rootDeclarations,
        ElmSyntaxOptimizationConfig? syntaxOptimization = null,
        bool includeBundledKernelModules = true,
        IDictionary<string, Result<ElmSyntax.ElmSyntaxParseError, SyntaxModelTypes.File>>? sourceParseCache = null,
        Canonicalization.DeclarationCache? canonicalizationCache = null,
        IReadOnlyDictionary<string, SyntaxModelTypes.File>? parsedSources = null)
    {
        syntaxOptimization ??= new ElmSyntaxOptimizationConfig.SyntaxOptimizationEnabled();

        var canonicalizationResult =
            ParseAndCanonicalizeForLowering(
                appCodeTree,
                rootDeclarations,
                includeBundledKernelModules,
                sourceParseCache,
                canonicalizationCache,
                parsedSources);

        if (canonicalizationResult.IsErrOrNull() is { } error)
            return error.ToString();

        var canonicalized =
            canonicalizationResult.Extract(error => throw new InvalidOperationException(error.ToString()));

        var flat = canonicalized.CanonicalDeclarations;
        var prepared = flat;

        if (syntaxOptimization is ElmSyntaxOptimizationConfig.SyntaxOptimizationEnabled enabled)
        {
            var lowering =
                BuiltinOperatorLowering.Apply(
                    prepared,
                    new BuiltinOperatorLowering.Configuration(LowerBuiltinOperators: true, LowerElmCoreBasics: true));

            if (lowering.IsErrOrNull() is { } lowerError)
                return lowerError;

            prepared = lowering.Extract(error => throw new InvalidOperationException(error));

            if (enabled.RemoveDebugLogApplications)
            {
                prepared =
                    prepared.ToImmutableDictionary(
                        entry => entry.Key,
                        entry => DebugLogRemoval.RewriteDeclaration(entry.Value));
            }
        }

        var program = CompilerModel.LambdaLifting.Lift(CompilerModel.FromCanonicalSyntax.Convert(prepared));
        CompilerModel.ProgramValidation.Validate(program);

        return
            new CompilationPipelineStageResults(canonicalized.ConcreteModules, flat, program)
            {
                RootDeclarationNames = canonicalized.RootDeclarations,
                SourceReferences = canonicalized.References,
                SourceFilePaths = canonicalized.FilePaths
            };
    }

    private static Result<CompilationError, CanonicalizationBoundaryResult>
        ParseAndCanonicalizeForLowering(
        FileTree appCodeTree,
        IReadOnlyList<DeclQualifiedName> rootDeclarations,
        bool includeBundledKernelModules,
        IDictionary<string, Result<ElmSyntax.ElmSyntaxParseError, SyntaxModelTypes.File>>? sourceParseCache,
        Canonicalization.DeclarationCache? canonicalizationCache,
        IReadOnlyDictionary<string, SyntaxModelTypes.File>? parsedSources)
    {
        if (rootDeclarations.Count is 0)
            return new CanonicalizationBoundaryResult([], ImmutableHashSet<DeclQualifiedName>.Empty);

        // Resolved builds already supply the replacement sources selected by their configuration.
        var appCodeTreeWithKernelModules =
            FileTree.MergeFiles(
                left: appCodeTree,
                right:
                includeBundledKernelModules
                ?
                ElmInElm.BundledFiles.ElmKernelModulesDefault.Value
                :
                FileTree.EmptyTree);

        var elmModuleFiles =
            appCodeTreeWithKernelModules.EnumerateFilesTransitive()
            .Where(file => file.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .ToImmutableArray();

        // Step 1: Parse all modules, building a map from file path to module name.
        var successfullyParsedModules = new List<SyntaxModelTypes.File>();

        var parseFailures = new Dictionary<string, string>();

        var moduleNameToFilePath = new Dictionary<string, string>();

        foreach (var moduleFile in elmModuleFiles)
        {
            var moduleText =
                Encoding.UTF8.GetString(moduleFile.fileContent.Span);

            // First try to get the module name from the header
            var headerResult =
                ElmSyntax.ElmSyntaxParser.ParseModuleHeader(moduleText);

            if (headerResult.IsErrOrNull() is not null)
            {
                // Can't even parse the header - skip this file entirely
                continue;
            }

            if (headerResult.IsOkOrNull() is not { } header)
            {
                throw new NotImplementedException(
                    "Unexpected header result type: " + headerResult.GetType().Name);
            }

            var moduleNameFlattened = string.Join(".", header.ModuleName);

            // Record the path→name mapping before any module-level filtering.
            moduleNameToFilePath[moduleNameFlattened] = string.Join('/', moduleFile.path);

            // Now try full parsing
            var parseResult =
                parsedSources is not null && parsedSources.TryGetValue(moduleNameFlattened, out var supplied)
                ?
                Result<ElmSyntax.ElmSyntaxParseError, SyntaxModelTypes.File>.ok(supplied)
                :
                ParseSource(moduleText, sourceParseCache);

            if (parseResult.IsErrOrNullable() is { } parseErr)
            {
                parseFailures.Add(moduleNameFlattened, ElmSyntax.ElmSyntaxParseError.RenderDisplayString(parseErr));
                continue;
            }

            if (parseResult.IsOkOrNull() is not { } parseModuleOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseResult.GetType().Name);
            }

            if (parseModuleOk.IncompleteDeclarations.Count is not 0)
            {
                var firstIncompleteDeclaration = parseModuleOk.IncompleteDeclarations[0];

                parseFailures.Add(
                    moduleNameFlattened,
                    ElmSyntax.ElmSyntaxParseError.RenderDisplayString(firstIncompleteDeclaration.Value.ParseError));

                continue;
            }

            successfullyParsedModules.Add(parseModuleOk);
        }

        foreach (var root in rootDeclarations)
        {
            var sourceNamespace = string.Join(".", root.Namespaces);

            if (parseFailures.TryGetValue(sourceNamespace, out var parseFailure))
            {
                return
                    new CompilationError.Message(
                        $"Cannot canonicalize root declaration '{root.FullName}': source '{moduleNameToFilePath[sourceNamespace]}' failed to parse.\n{parseFailure}");
            }
        }

        var canonicalizationResult =
            Canonicalization.CanonicalizeDeclarations(
                successfullyParsedModules,
                rootDeclarations,
                cache: canonicalizationCache);

        if (canonicalizationResult.IsErrOrNull() is { } canonErr)
        {
            return new CompilationError.Message(canonErr);
        }

        if (canonicalizationResult.IsOkOrNull() is not { } canonicalizedDeclarations)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + canonicalizationResult.GetType().Name);
        }

        foreach (var root in rootDeclarations)
        {
            if (canonicalizedDeclarations.Declarations[root].Value.Value is not SyntaxModelTypes.Declaration.FunctionDeclaration)
            {
                return
                    new CompilationError.Message(
                        $"Root declaration '{root.FullName}' is not an executable function or value definition.");
            }
        }

        var moduleErrors = new List<CompilationError.CanonicalizationDiagnostic>();

        foreach (var (declarationName, (_, errors, _)) in canonicalizedDeclarations.Declarations)
        {
            if (errors.Count > 0)
            {
                var moduleNameStr = string.Join(".", declarationName.Namespaces);
                var filePath = moduleNameToFilePath[moduleNameStr];

                var dependencyChain =
                    BuildSourceDependencyChain(
                        canonicalizedDeclarations.DependencyChains[declarationName],
                        successfullyParsedModules,
                        moduleNameToFilePath,
                        canonicalizedDeclarations.References);

                moduleErrors.AddRange(
                    errors.Select(
                        error =>
                        CompleteCanonicalizationDiagnostic(
                            new CompilationError.CanonicalizationDiagnostic(
                                declarationName.FullName,
                                filePath,
                                error is CanonicalizationError.UnresolvedReference { Target: { } target } unresolved &&
                                parseFailures.TryGetValue(string.Join(".", target.Namespaces), out var parseFailure)
                                ?
                                unresolved with
                                {
                                    ResolutionDetail =
                                    $"The supplied source '{moduleNameToFilePath[string.Join(".", target.Namespaces)]}' " +
                                    $"for '{string.Join(".", target.Namespaces)}' failed to parse:\n{parseFailure}"
                                }
                                :
                                error,
                                dependencyChain),
                            successfullyParsedModules,
                            moduleNameToFilePath)));
            }
        }

        if (moduleErrors.Count > 0)
        {
            return new CompilationError.CanonicalizationErrors(moduleErrors);
        }

        var canonicalizedModules =
            successfullyParsedModules
            .Select(
                source => source with
                {
                    Declarations =
                    [
                    .. canonicalizedDeclarations.Declarations
                    .Where(
                        entry => entry.Key.Namespaces.SequenceEqual(
                            SyntaxModelTypes.Module.GetModuleName(source.ModuleDefinition.Value).Value))
                    .OrderBy(entry => entry.Value.Value.Range.Start.Row)
                    .Select(entry => entry.Value.Value)
                    .Distinct()
                    ]
                })
            .Where(source => source.Declarations.Count > 0)
            .ToList();

        return
            new CanonicalizationBoundaryResult(
                ConcreteModules: canonicalizedModules,
                RootDeclarations: rootDeclarations.ToImmutableHashSet())
            {
                CanonicalDeclarations =
                canonicalizedDeclarations.Declarations.ToImmutableDictionary(
                    entry => entry.Key,
                    entry => ElmSyntaxAbstract.ConvertFromConcrete.FromDeclaration(entry.Value.Value.Value)),
                References = canonicalizedDeclarations.References,
                FilePaths = moduleNameToFilePath
            };
    }

    private static IReadOnlyList<CompilationError.DeclarationDependencyChainItem> BuildSourceDependencyChain(
        IReadOnlyList<DeclQualifiedName> declarations,
        IReadOnlyList<SyntaxModelTypes.File> sources,
        IReadOnlyDictionary<string, string> paths,
        IReadOnlyDictionary<DeclQualifiedName, IReadOnlyList<DeclarationReference>> references)
    {
        var chain = new List<CompilationError.DeclarationDependencyChainItem>();

        for (var index = 0; index < declarations.Count; index++)
        {
            var declaration = declarations[index];
            var moduleName = string.Join(".", declaration.Namespaces);

            var source =
                sources.FirstOrDefault(
                    file =>
                    SyntaxModelTypes.Module.GetModuleName(file.ModuleDefinition.Value).Value.SequenceEqual(
                        declaration.Namespaces));

            var definition =
                source?.Declarations.FirstOrDefault(node =>
                Canonicalization.GetDeclarationName(node.Value) == declaration.DeclName);

            CompilationError.ImportOrigin? origin = null;
            DeclarationReference? reference = null;

            if (index > 0)
            {
                var previous = declarations[index - 1];

                reference =
                    (references.GetValueOrDefault(previous) ?? [])
                    .Where(item => item.Declaration.Equals(declaration))
                    .OrderBy(item => item.Range.Start.Row).ThenBy(item => item.Range.Start.Column)
                    .FirstOrDefault();

                if (!previous.Namespaces.SequenceEqual(declaration.Namespaces))
                {
                    var previousSource =
                        sources.FirstOrDefault(
                            file =>
                            SyntaxModelTypes.Module.GetModuleName(file.ModuleDefinition.Value).Value.SequenceEqual(
                                previous.Namespaces));

                    var import =
                        previousSource?.Imports.FirstOrDefault(node =>
                        string.Join(".", node.Value.ModuleName.Value) == moduleName);

                    if (import is not null)
                    {
                        origin =
                            new(
                                CompilationError.ImportKind.ExplicitImport,
                                import.Range,
                                import.Value.ModuleAlias is { } alias ? string.Join(".", alias.Alias.Value) : null,
                                RenderExposing(import.Value.ExposingList));
                    }
                    else if (ImplicitImportConfig.Default.ModuleImports.Any(
                        module => module.ModuleName.SequenceEqual(declaration.Namespaces)))
                    {
                        origin = new(CompilationError.ImportKind.ImplicitImport, null, null, null);
                    }
                }
            }

            chain.Add(
                new(
                    declaration.FullName,
                    index is 0 ? null : declarations[index - 1].FullName,
                    index is 0)
                {
                    FilePath = paths.GetValueOrDefault(moduleName),
                    DeclarationRange = definition is { Range.Start.Row: > 0 } ? definition.Range : null,
                    ReferenceRange = reference?.Range,
                    ReferencedName = reference?.ReferencedName.FullName,
                    IsTypeReference = reference?.IsTypeReference ?? false,
                    Import = origin
                });
        }

        return chain;
    }

    private static CompilationError.CanonicalizationDiagnostic CompleteCanonicalizationDiagnostic(
        CompilationError.CanonicalizationDiagnostic diagnostic,
        IReadOnlyList<SyntaxModelTypes.File> sources,
        IReadOnlyDictionary<string, string> paths)
    {
        if (diagnostic.Error is not CanonicalizationError.UnresolvedReference { Target: { } target } unresolved)
            return diagnostic;

        var targetSource =
            sources.FirstOrDefault(
                source =>
                SyntaxModelTypes.Module.GetModuleName(source.ModuleDefinition.Value).Value.SequenceEqual(
                    target.Namespaces));

        var owningName = QualifiedNameHelper.FromQualifiedNameString(diagnostic.DeclarationName);

        var declaringSource =
            sources.FirstOrDefault(
                source =>
                SyntaxModelTypes.Module.GetModuleName(source.ModuleDefinition.Value).Value.SequenceEqual(
                    owningName.Namespaces));

        var import =
            declaringSource?.Imports.FirstOrDefault(node => node.Value.ModuleName.Value.SequenceEqual(target.Namespaces));

        var targetDefinition =
            targetSource?.Declarations.FirstOrDefault(node =>
            unresolved.IsTypeReference
            ? node.Value is SyntaxModelTypes.Declaration.ChoiceTypeDeclaration or SyntaxModelTypes.Declaration.AliasDeclaration &&
                Canonicalization.GetDeclarationName(node.Value) == target.DeclName
            : node.Value switch
            {
                SyntaxModelTypes.Declaration.FunctionDeclaration function => function.Function.Declaration.Value.Name.Value == target.DeclName,
                SyntaxModelTypes.Declaration.PortDeclaration port => port.Signature.Name.Value == target.DeclName,
                SyntaxModelTypes.Declaration.InfixDeclaration infix => infix.Infix.Operator.Value == target.DeclName,
                SyntaxModelTypes.Declaration.AliasDeclaration alias => alias.TypeAlias.Name.Value == target.DeclName &&
                    alias.TypeAlias.TypeAnnotation.Value is SyntaxModelTypes.TypeAnnotation.Record,
                SyntaxModelTypes.Declaration.ChoiceTypeDeclaration choice => choice.TypeDeclaration.Constructors.Any(constructor =>
                    constructor.Value.Name.Value == target.DeclName),
                _ => throw new NotImplementedException(
                    $"{nameof(CompleteCanonicalizationDiagnostic)} does not handle declaration variant: {node.Value.GetType().Name}")
            });

        return
            diagnostic with
            {
                TargetSourcePath = paths.GetValueOrDefault(string.Join(".", target.Namespaces)),
                TargetSourceRange = targetDefinition?.Range ?? targetSource?.ModuleDefinition.Range,
                ReferenceImport =
                import is not null
                ?
                new(
                    CompilationError.ImportKind.ExplicitImport,
                    import.Range,
                    import.Value.ModuleAlias is { } alias ? string.Join(".", alias.Alias.Value) : null,
                    RenderExposing(import.Value.ExposingList))
                :
                ImplicitImportConfig.Default.ModuleImports.FirstOrDefault(module => module.ModuleName.SequenceEqual(target.Namespaces)) is { } implicitImport
                ?
                new(CompilationError.ImportKind.ImplicitImport, null, implicitImport.Alias, null)
                :
                null
            };
    }

    /// <summary>
    /// Compiles explicit declaration roots without evaluating their values.
    /// Caller-owned parsing dictionaries and declaration caches can reuse unchanged work.
    /// Parsing dictionaries require caller synchronization; DeclarationCache supports concurrent callers.
    /// </summary>
    public static Result<string, (PineValue compiledEnvValue, CompilationPipelineStageResults pipelineStageResults)> CompileInteractiveEnvironment(
        FileTree appCodeTree,
        IReadOnlyList<DeclQualifiedName> rootDeclarations,
        ElmSyntaxOptimizationConfig? syntaxOptimization = null,
        bool disableGenericApplicationChainConsolidation = false,
        bool includeBundledKernelModules = true,
        IDictionary<string, Result<ElmSyntax.ElmSyntaxParseError, SyntaxModelTypes.File>>? sourceParseCache = null,
        Canonicalization.DeclarationCache? canonicalizationCache = null,
        IReadOnlyDictionary<string, SyntaxModelTypes.File>? parsedSources = null)
    {
        syntaxOptimization ??= SyntaxOptimizationConfigDefault;

        var loweringResult =
            LowerToCompilerModel(
                appCodeTree,
                rootDeclarations,
                syntaxOptimization,
                includeBundledKernelModules,
                sourceParseCache,
                canonicalizationCache,
                parsedSources);

        if (loweringResult.IsErrOrNull() is { } loweringErr)
            return loweringErr;

        if (loweringResult.IsOkOrNull() is not { } pipelineStageResults)
            throw new NotImplementedException("Unexpected result type: " + loweringResult.GetType().Name);

        return
            EmitCompilerProgram(
                pipelineStageResults.CompilerProgram ??
                throw new InvalidOperationException("Missing compiler-owned lowering result."),
                pipelineStageResults,
                disableGenericApplicationChainConsolidation);
    }

    /// <summary>Compiles sources prepared with project-scoped resolution and import visibility validation.</summary>
    public static Result<string, (PineValue compiledEnvValue, CompilationPipelineStageResults pipelineStageResults)> CompileResolvedEnvironment(
        ElmResolvedBuild build,
        IReadOnlyList<DeclQualifiedName> rootDeclarations,
        IDictionary<string, Result<ElmSyntax.ElmSyntaxParseError, SyntaxModelTypes.File>>? sourceParseCache = null,
        Canonicalization.DeclarationCache? canonicalizationCache = null)
    {
        var result =
            CompileInteractiveEnvironment(
                build.Sources,
                rootDeclarations,
                includeBundledKernelModules: false,
                sourceParseCache: sourceParseCache,
                canonicalizationCache: canonicalizationCache,
                parsedSources: build.CompilerModuleSyntax);

        if (result.IsErrOrNull() is not { } error)
            return result;

        var replacements =
            build.Resolution.Packages.Values
            .Where(package => package.SubstitutionImplementationId is not null)
            .OrderBy(package => package.Identity.Name, StringComparer.Ordinal)
            .ToArray();

        if (replacements.Length is 0)
            return error;

        return
            error + "\n\nSelected package replacements (authoritative source/API providers):\n" +
            string.Join(
                "\n",
                replacements.Select(
                    package =>
                    "  " + package.Identity + " -> " + package.SubstitutionImplementationId)) +
            "\nMissing replacement APIs are not automatically searched upstream.";
    }

    private static IReadOnlyList<CompilationError.DeclarationDependencyChainItem>
        FindDeclarationDependencyChain(
        IReadOnlySet<string> rootDeclarations,
        string targetDeclaration,
        IReadOnlyDictionary<string, IReadOnlySet<string>> directDependencies)
    {
        var pending = new Queue<string>();
        var predecessor = new Dictionary<string, string?>(StringComparer.Ordinal);

        foreach (var root in rootDeclarations.Order(StringComparer.Ordinal))
        {
            if (!directDependencies.ContainsKey(root) || !predecessor.TryAdd(root, null))
                continue;

            pending.Enqueue(root);
        }

        while (pending.TryDequeue(out var declaration))
        {
            if (declaration == targetDeclaration)
                break;

            if (!directDependencies.TryGetValue(declaration, out var dependencies))
                continue;

            foreach (var dependency in dependencies.Order(StringComparer.Ordinal))
            {
                if (predecessor.TryAdd(dependency, declaration))
                    pending.Enqueue(dependency);
            }
        }

        if (!predecessor.ContainsKey(targetDeclaration))
            return [];

        var names = new List<string>();

        for (string? current = targetDeclaration; current is not null; current = predecessor[current])
            names.Add(current);

        names.Reverse();

        return
            [
            .. names
            .Select(
                (name, index) =>
                new CompilationError.DeclarationDependencyChainItem(
                    name,
                    ReferencedBy: index is 0 ? null : names[index - 1],
                    IsCompilationRoot: index is 0))
            ];
    }

    internal static ImmutableDictionary<string, TypeInference.InferredType> ExtractParameterTypes(
        ElmSyntaxAbstract.FunctionStruct function,
        IReadOnlyDictionary<DeclQualifiedName, FunctionTypeInfo>? choiceTagTypes,
        IReadOnlyDictionary<DeclQualifiedName, FunctionTypeInfo>? functionTypes,
        string currentModuleName)
    {
        var parameterTypes = ImmutableDictionary<string, TypeInference.InferredType>.Empty;

        var constructorArgumentTypes =
            choiceTagTypes?
            .ToDictionary(
                entry => entry.Key,
                entry => entry.Value.ParameterTypes);

        if (function.Signature is { } signature)
        {
            var typeAnnotation = signature.TypeAnnotation;
            var parameters = function.Declaration.Arguments;

            // Walk through the function type annotation to extract parameter types
            var currentType = typeAnnotation;

            var paramIndex = 0;

            while (currentType is ElmSyntaxAbstract.TypeAnnotation.FunctionTypeAnnotation funcType &&
                paramIndex < parameters.Count)
            {
                var paramPattern = parameters[paramIndex];
                var paramTypeAnnotation = funcType.ArgumentType;

                // Extract binding types from the pattern
                // This handles both simple VarPattern and complex patterns like TuplePattern
                parameterTypes =
                    TypeInference.ExtractPatternBindingTypes(paramPattern, paramTypeAnnotation, parameterTypes);

                currentType = funcType.ReturnType;
                paramIndex++;
            }
        }

        // Also extract types from NamedPattern parameters using constructor argument types
        // This handles cases where the function has no type signature but uses choice type patterns
        if (constructorArgumentTypes is not null)
        {
            var parameters = function.Declaration.Arguments;

            foreach (var paramNode in parameters)
            {
                parameterTypes =
                    TypeInference.ExtractPatternBindingTypesWithConstructors(
                        paramNode,
                        constructorArgumentTypes,
                        parameterTypes);
            }

            // Also analyze the function body for tag applications that constrain parameter types
            // For example, in `alfa a b = let c = TagAlfa a in ...`, we infer that `a` is Int
            var functionBody = function.Declaration.Expression;

            parameterTypes =
                TypeInference.ExtractTypeConstraintsFromTagApplications(
                    functionBody,
                    constructorArgumentTypes,
                    parameterTypes);
        }

        // Also analyze the function body for function applications that constrain parameter types
        // For example, in `beta a b = let c = alfa a in ...` where `alfa : Int -> String`, we infer that `a` is Int
        if (functionTypes is not null)
        {
            var functionBody = function.Declaration.Expression;

            parameterTypes =
                TypeInference.ExtractTypeConstraintsFromFunctionApplications(
                    functionBody,
                    functionTypes,
                    currentModuleName,
                    parameterTypes);
        }

        return parameterTypes;
    }

    /// <summary>
    /// Renders a <see cref="CanonicalizationError"/> as a human-readable string.
    /// </summary>
    public static string RenderCanonicalizationError(CanonicalizationError error) =>
        error switch
        {
            CanonicalizationError.UnresolvedReference unresolved =>
            $"Cannot resolve {(unresolved.IsTypeReference ? "type reference" : "reference")} '{unresolved.Name}'",

            CanonicalizationError.NamingClash clash =>
            clash.ShadowedRange is { } shadowedRange
            ?
            $"Name '{clash.Name}' at {clash.Range.Start.Row}:{clash.Range.Start.Column} is shadowing an existing declaration at {shadowedRange.Start.Row}:{shadowedRange.Start.Column}"
            :
            $"Name '{clash.Name}' at {clash.Range.Start.Row}:{clash.Range.Start.Column} is shadowing an existing declaration",

            CanonicalizationError.AmbiguousImport ambiguous =>
            $"Name '{ambiguous.Name}' is exposed by multiple imports: {string.Join(", ", ambiguous.ImportingModules)}",

            _ =>
            throw new NotImplementedException(
                "Unexpected error type: " + error.GetType())
        };

    private static string? RenderExposing(
        (SyntaxModelTypes.Location ExposingTokenLocation,
        SyntaxModelTypes.Node<SyntaxModelTypes.Exposing> ExposingList)? exposing) =>
        exposing?.ExposingList.Value switch
        {
            null =>
            null,

            SyntaxModelTypes.Exposing.All =>
            "(..)",

            SyntaxModelTypes.Exposing.Explicit explicitExposing =>
            "(" +
            string.Join(
                ", ",
                explicitExposing.Nodes.Nodes.Select(
                    node =>
                    node.Value switch
                    {
                        SyntaxModelTypes.TopLevelExpose.InfixExpose infix =>
                        "(" + infix.Name + ")",

                        SyntaxModelTypes.TopLevelExpose.FunctionExpose function =>
                        function.Name,

                        SyntaxModelTypes.TopLevelExpose.TypeOrAliasExpose typeOrAlias =>
                        typeOrAlias.Name,

                        SyntaxModelTypes.TopLevelExpose.TypeExpose type =>
                        type.ExposedType.Name + (type.ExposedType.Open is null ? string.Empty : "(..)"),

                        _ =>
                        node.Value.ToString() ?? node.Value.GetType().Name
                    })) +
            ")",

            _ =>
            exposing.Value.ExposingList.Value.ToString()
        };

    /// <summary>
    /// Flattens a list of modules into an immutable dictionary of declarations keyed by
    /// their fully qualified name.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration>
        FlattenModulesToDeclarationDictionary(
        IReadOnlyList<ElmSyntaxAbstract.File> modules)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, ElmSyntaxAbstract.Declaration>();

        foreach (var module in modules)
        {
            var moduleName = ElmSyntaxAbstract.Module.GetModuleName(module.ModuleDefinition);

            foreach (var decl in module.Declarations)
            {
                var declName = GetDeclarationName(decl);

                if (declName is null)
                    continue;

                var qualifiedName =
                    DeclQualifiedName.Create(
                        moduleName,
                        declName);

                builder[qualifiedName] = decl;
            }
        }

        return builder.ToImmutable();
    }

    internal static ImmutableDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration>
        FilterFunctionDeclarationsToRootDependencies(
        ImmutableDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration> declarations,
        IReadOnlySet<DeclQualifiedName> roots)
    {
        var includedFunctions = new HashSet<DeclQualifiedName>();

        var pendingFunctions =
            new Stack<DeclQualifiedName>(
                roots
                .Where(
                    name =>
                    declarations.TryGetValue(name, out var declaration) &&
                    declaration is ElmSyntaxAbstract.Declaration.FunctionDeclaration));

        while (pendingFunctions.TryPop(out var functionName))
        {
            if (!includedFunctions.Add(functionName))
                continue;

            var functionDeclaration =
                (ElmSyntaxAbstract.Declaration.FunctionDeclaration)declarations[functionName];

            foreach (var referencedName in
                ElmSyntaxOptimization.CollectFunctionReferences(
                    functionDeclaration.Function.Declaration.Expression))
            {
                if (declarations.TryGetValue(referencedName, out var referencedDeclaration) &&
                    referencedDeclaration is ElmSyntaxAbstract.Declaration.FunctionDeclaration)
                {
                    pendingFunctions.Push(referencedName);
                }
            }
        }

        return
            declarations
            .Where(
                declaration =>
                declaration.Value is not ElmSyntaxAbstract.Declaration.FunctionDeclaration ||
                includedFunctions.Contains(declaration.Key))
            .ToImmutableDictionary();
    }

    /// <summary>
    /// Extracts the declaration name from a syntax declaration.
    /// </summary>
    internal static string? GetDeclarationName(ElmSyntaxAbstract.Declaration decl) =>
        decl switch
        {
            ElmSyntaxAbstract.Declaration.FunctionDeclaration funcDecl =>
            funcDecl.Function.Declaration.Name,

            ElmSyntaxAbstract.Declaration.ChoiceTypeDeclaration typeDecl =>
            typeDecl.TypeDeclaration.Name,

            ElmSyntaxAbstract.Declaration.AliasDeclaration aliasDecl =>
            aliasDecl.TypeAlias.Name,

            ElmSyntaxAbstract.Declaration.PortDeclaration portDecl =>
            portDecl.Signature.Name,

            ElmSyntaxAbstract.Declaration.InfixDeclaration infixDecl =>
            infixDecl.Infix.Operator,

            _ =>
            throw new NotImplementedException(
                "Unexpected declaration type: " + decl.GetType()),
        };

    /// <summary>
    /// Compiles an <see cref="AppCompilationUnits"/> bundle into a Pine value.
    /// </summary>
    /// <remarks>
    /// Not yet implemented.
    /// </remarks>
    public static PineValue Compile(
        AppCompilationUnits compilationUnits)
    {
        throw new NotImplementedException();
    }
}
