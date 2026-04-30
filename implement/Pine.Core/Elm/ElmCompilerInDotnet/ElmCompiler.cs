using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Files;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

using SyntaxModelTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Holds the intermediate results of each stage in the Elm compilation pipeline.
/// Stages before the optimization pipeline (Canonicalized, LambdaLifted) keep the per-module
/// representation because downstream pipeline stages still operate on whole modules.
/// Stages produced by the optimization pipeline (Specialized, Inlined) and the final result
/// (ModulesForCompilation) use a flat declaration dictionary keyed by qualified name,
/// removing the module container.
/// </summary>
/// <param name="Canonicalized">
/// Modules after canonicalization: all names are resolved to fully-qualified forms.
/// </param>
/// <param name="LambdaLifted">
/// Modules after the initial lambda lifting pass: closures are transformed into top-level functions.
/// </param>
/// <param name="Specialized">
/// Flat declaration dictionary after specialization: specialized function variants are created
/// for known argument patterns.
/// This is <c>null</c> when <c>disableInlining</c> is true, as the optimization pipeline is skipped.
/// </param>
/// <param name="Inlined">
/// Flat declaration dictionary after higher-order inlining: functions that receive function
/// arguments are inlined.
/// This is <c>null</c> when <c>disableInlining</c> is true, as the optimization pipeline is skipped.
/// </param>
/// <param name="ModulesForCompilation">
/// The final list of modules passed into the compilation backend.
/// When the optimization pipeline is enabled, this is the result after operator lowering.
/// When disabled, this equals <see cref="LambdaLifted"/>.
/// </param>
public record CompilationPipelineStageResults(
    IReadOnlyList<SyntaxTypes.File> Canonicalized,
    IReadOnlyList<SyntaxTypes.File> LambdaLifted,
    ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>? Specialized,
    ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>? Inlined,
    IReadOnlyList<SyntaxTypes.File> ModulesForCompilation,
    ImmutableList<OptimizationIterationStageResults>? OptimizationIterations = null);

/// <summary>
/// Holds the intermediate results of each stage within a single optimization iteration
/// (specialization, higher-order inlining, size-based inlining).
/// Each stage result is a flat declaration dictionary keyed by qualified name.
/// </summary>
public record OptimizationIterationStageResults(
    int Round,
    ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> AfterSpecialization,
    ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> AfterHigherOrderInlining,
    ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> AfterSizeBasedInlining);

/// <summary>
/// Holds the intermediate results of each stage in the optimization pipeline
/// (specialization, inlining, lambda re-lifting, operator lowering).
/// Each stage result is a flat declaration dictionary keyed by qualified name.
/// The <see cref="Iterations"/> list contains per-iteration intermediate results
/// when the pipeline runs more than one round.
/// </summary>
internal record OptimizationPipelineStageResults(
    ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> AfterSpecialization,
    ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> AfterHigherOrderInlining,
    ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> AfterLambdaLifting,
    ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> AfterLowering,
    ImmutableList<OptimizationIterationStageResults> Iterations);

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
public class ElmCompiler
{
    private static readonly FrozenSet<string> s_pineKernelModuleNamesDefault =
        FrozenSet.Create(
            [
            "Pine_builtin",
            "Pine_kernel",
            ]);

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
            ]);

    public static Result<string, (PineValue compiledEnvValue, CompilationPipelineStageResults pipelineStageResults)> CompileInteractiveEnvironment(
        FileTree appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        bool disableInlining,
        int maxOptimizationRounds = 1,
        bool disableGenericApplicationChainConsolidation = false)
    {
        var elmModuleFiles =
            appCodeTree.EnumerateFilesTransitive()
            .Where(file => file.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .ToImmutableArray();

        // Step 1: Parse all modules, building a map from file path to module name.
        var successfullyParsedModules = new Dictionary<string, SyntaxTypes.File>();

        var parseFailures = new HashSet<string>();

        var filePathToModuleName =
            new Dictionary<IReadOnlyList<string>, string>(
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

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
            filePathToModuleName[moduleFile.path] = moduleNameFlattened;

            // Natively-implemented modules (e.g. "Basics") are superseded by C# code.
            // Do not parse or compile them from Elm source.
            if (s_nativelyImplementedModuleNames.Contains(moduleNameFlattened))
                continue;

            // Now try full parsing
            var parseResult =
                ElmSyntax.ElmSyntaxParser.ParseModuleText(moduleText);

            if (parseResult.IsErrOrNull() is not null)
            {
                parseFailures.Add(moduleNameFlattened);
                continue;
            }

            if (parseResult.IsOkOrNull() is not { } parseModuleOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseResult.GetType().Name);
            }

            var parseModuleAst =
                SyntaxTypes.FromFullSyntaxModel.Convert(parseModuleOk);

            successfullyParsedModules[moduleNameFlattened] = parseModuleAst;
        }

        // Step 2: Compute the transitive dependency closure of the root modules.
        // Only modules reachable from roots that parsed successfully are compiled.
        // Modules outside the dependency graph (unreachable from the selected roots)
        // are completely ignored — their parse failures or other issues are irrelevant.
        // If a reachable module has an import that failed to parse, that is an error:
        // a required dependency is broken.
        var rootModuleNames =
            rootFilePaths
            .Select(path =>
                filePathToModuleName.TryGetValue(path, out var name) ? name : null)
            .Where(name => name is not null)
            .ToHashSet()!;

        // Start the closure with roots that parsed successfully.
        // Roots that failed to parse are silently skipped (they were never needed).
        var modulesToCompile =
            new HashSet<string>(rootModuleNames.Where(successfullyParsedModules.ContainsKey));

        {
            bool changed;

            do
            {
                changed = false;

                foreach (var moduleName in modulesToCompile.ToList())
                {
                    if (!successfullyParsedModules.TryGetValue(moduleName, out var parsedModule))
                        continue;

                    // Collect both explicit imports and implicit imports as dependencies.
                    var importedNames =
                        parsedModule.Imports
                        .Select(import => string.Join(".", import.Value.ModuleName.Value));

                    var implicitModuleNames =
                        ImplicitImportConfig.Default.ModuleImports
                        .Select(m => string.Join(".", m.ModuleName));

                    foreach (var importedName in importedNames.Concat(implicitModuleNames).Distinct())
                    {
                        if (s_nativelyImplementedModuleNames.Contains(importedName))
                            continue; // Natively implemented in .NET; not a compilation dep.

                        if (parseFailures.Contains(importedName))
                        {
                            // A module in the dep graph depends on a module that failed to parse.
                            return
                                "Module '" + importedName +
                                "' is required by '" + moduleName +
                                "' but failed to parse.";
                        }

                        // Unknown modules (not in our file tree) are external packages; skip.
                        if (successfullyParsedModules.ContainsKey(importedName) &&
                            !modulesToCompile.Contains(importedName))
                        {
                            modulesToCompile.Add(importedName);
                            changed = true;
                        }
                    }
                }
            }
            while (changed);
        }

        // Step 3: Canonicalize the dependency modules and surface any errors.
        var modulesForCanonicalization =
            modulesToCompile
            .Where(successfullyParsedModules.ContainsKey)
            .Select(name => successfullyParsedModules[name])
            .ToList();

        var canonicalizationResult =
            Canonicalization.CanonicalizeAllowingErrors(modulesForCanonicalization);

        if (canonicalizationResult.IsErrOrNull() is { } canonErr)
        {
            return canonErr;
        }

        if (canonicalizationResult.IsOkOrNull() is not { } canonicalizedModulesDict)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + canonicalizationResult.GetType().Name);
        }

        // Surface canonicalization errors for any module in the dependency set.
        var moduleErrors = new List<string>();

        foreach (var (moduleName, (_, errors, _)) in canonicalizedModulesDict)
        {
            if (errors.Count > 0)
            {
                var moduleNameStr = string.Join(".", moduleName);
                var errMessages = string.Join("\n", errors.Select(RenderCanonicalizationError));
                moduleErrors.Add("In module " + moduleNameStr + ":\n" + errMessages);
            }
        }

        if (moduleErrors.Count > 0)
        {
            return string.Join("\n\n", moduleErrors);
        }

        // All dependency modules canonicalized successfully.
        var canonicalizedModules =
            canonicalizedModulesDict.Values.Select(v => v.File).ToList();

        // Lambda lifting stage: Transform closures into top-level functions
        var lambdaLiftedModules =
            canonicalizedModules
            .Select(LambdaLifting.LiftLambdas)
            .ToList();

        if (CheckForNamingErrors(lambdaLiftedModules, "Lambda lifting (initial)") is { } lambdaLiftShadowErr)
            return lambdaLiftShadowErr;

        if (CheckForSyntaxNodesDisallowedInOptimization(lambdaLiftedModules, "Lambda lifting (initial)") is { } lambdaLiftDisallowedErr)
            return lambdaLiftDisallowedErr;

        OptimizationPipelineStageResults? optimizationResults = null;

        List<SyntaxTypes.File> modulesForCompilation;

        if (disableInlining)
        {
            modulesForCompilation = lambdaLiftedModules;
        }
        else
        {
            var pipelineResult =
                ApplyOptimizationPipelineWithStageResults(lambdaLiftedModules, maxRounds: maxOptimizationRounds);

            if (pipelineResult.IsErrOrNull() is { } pipelineErr)
                return pipelineErr;

            optimizationResults =
                pipelineResult.Extract(err => throw new NotImplementedException());

            // Reconstruct modules from the flat declaration dictionary for the compilation backend.
            modulesForCompilation =
                ReconstructModulesFromFlatDict(
                    optimizationResults.AfterLowering,
                    lambdaLiftedModules);
        }

        var allFunctions =
            new Dictionary<SyntaxModelTypes.QualifiedNameRef, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)>();

        foreach (var elmModuleSyntax in modulesForCompilation)
        {
            var moduleName =
                SyntaxTypes.Module.GetModuleName(elmModuleSyntax.ModuleDefinition.Value).Value;

            var moduleNameFlattened =
                string.Join(".", moduleName);

            var declarations =
                elmModuleSyntax.Declarations
                .Select(declNode => declNode.Value)
                .OfType<SyntaxTypes.Declaration.FunctionDeclaration>();

            foreach (var declaration in declarations)
            {
                var functionName =
                    declaration.Function.Declaration.Value.Name.Value;

                var qualifiedName =
                    QualifiedNameHelper.ToQualifiedNameRef(moduleName, functionName);

                allFunctions[qualifiedName] = (moduleNameFlattened, functionName, declaration);
            }
        }

        // Build function type metadata dictionary for type inference
        var functionTypes = new Dictionary<SyntaxModelTypes.QualifiedNameRef, FunctionTypeInfo>();

        foreach (var (qualifiedName, (_, _, declaration)) in allFunctions)
        {
            functionTypes[qualifiedName] =
                new FunctionTypeInfo(
                    TypeInference.GetFunctionReturnType(declaration),
                    TypeInference.GetFunctionParameterTypes(declaration));
        }

        // Build choice type tag argument types dictionary keyed by qualified constructor name.
        // We use qualified names to avoid cross-module collisions such as
        // ParserFast.Good vs Parser.Advanced.Good.
        var choiceTagArgumentTypes =
            new Dictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>();

        foreach (var elmModuleSyntax in modulesForCompilation)
        {
            var moduleName =
                SyntaxTypes.Module.GetModuleName(elmModuleSyntax.ModuleDefinition.Value).Value;

            var typeDeclarations =
                elmModuleSyntax.Declarations
                .Select(declNode => declNode.Value)
                .OfType<SyntaxTypes.Declaration.CustomTypeDeclaration>();

            foreach (var typeDecl in typeDeclarations)
            {
                foreach (var ctorNode in typeDecl.TypeDeclaration.Constructors)
                {
                    var ctor = ctorNode.Value;
                    var ctorName = ctor.Name.Value;

                    var qualifiedCtorName =
                        QualifiedNameHelper.ToQualifiedNameRef(moduleName, ctorName);

                    var argTypes = new List<TypeInference.InferredType>();

                    foreach (var argNode in ctor.Arguments)
                    {
                        var argType = TypeInference.TypeAnnotationToInferredType(argNode.Value);
                        argTypes.Add(argType);
                    }

                    choiceTagArgumentTypes[qualifiedCtorName] = argTypes;
                }
            }
        }

        // Build record type alias constructors dictionary
        // A type alias for a record type creates an implicit constructor function
        // where argument order matches the field order in the type alias declaration
        var recordTypeAliasConstructors = new Dictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>>();

        foreach (var elmModuleSyntax in modulesForCompilation)
        {
            var moduleName =
                SyntaxTypes.Module.GetModuleName(elmModuleSyntax.ModuleDefinition.Value).Value;

            var aliasDeclarations =
                elmModuleSyntax.Declarations
                .Select(declNode => declNode.Value)
                .OfType<SyntaxTypes.Declaration.AliasDeclaration>();

            foreach (var aliasDecl in aliasDeclarations)
            {
                var aliasName = aliasDecl.TypeAlias.Name.Value;
                var typeAnnotation = aliasDecl.TypeAlias.TypeAnnotation.Value;

                // Check if the type alias is for a record type
                if (typeAnnotation is SyntaxTypes.TypeAnnotation.Record recordType)
                {
                    // Extract field names in declaration order
                    var fieldNames =
                        recordType.RecordDefinition.Fields
                        .Select(f => f.Value.FieldName.Value)
                        .ToList();

                    var qualifiedName = QualifiedNameHelper.ToQualifiedNameRef(moduleName, aliasName);
                    recordTypeAliasConstructors[qualifiedName] = fieldNames;
                }
            }
        }

        // Create initial compilation context with all available functions
        var initialContext =
            new ModuleCompilationContext(
                allFunctions,
                CompiledFunctionsCache: [],
                PineKernelModuleNames: s_pineKernelModuleNamesDefault,
                FunctionTypes: functionTypes,
                ChoiceTagArgumentTypes: choiceTagArgumentTypes,
                RecordTypeAliasConstructors: recordTypeAliasConstructors);

        // Pre-compute dependency layouts and SCCs for all functions BEFORE compilation
        var (dependencyLayouts, functionToScc, sccsInDependencyOrder) =
            ComputeDependencyLayoutsAndSccs(allFunctions, initialContext);

        // Create compilation context with pre-computed layouts and SCCs
        var compilationContext =
            initialContext
            .WithDependencyLayouts(dependencyLayouts);

        // Second pass: Compile all SCCs in dependency order
        // This ensures all dependencies are compiled before they are needed
        foreach (var scc in sccsInDependencyOrder)
        {
            // Skip if all members are already compiled
            if (scc.Members.All(m => compilationContext.TryGetCompiledFunctionValue(m) is not null))
                continue;

            var sharedLayout = scc.GetLayout();
            var sccMembersSet = new HashSet<string>(scc.Members);

            // Skip explicitly if any layout dependency outside this SCC was not compiled.
            // This happens when a transitive dependency was excluded from the compilation roots.
            var allLayoutDepsCompiled =
                sharedLayout
                .Where(dep => !sccMembersSet.Contains(dep))
                .All(dep => compilationContext.TryGetCompiledFunctionValue(dep) is not null);

            if (!allLayoutDepsCompiled)
                continue;

            var compileSccResult = CompileSCC(scc, compilationContext, disableGenericApplicationChainConsolidation);

            if (compileSccResult.IsErrOrNull() is { } compileSccErr)
            {
                return
                    "Failed to compile SCC [" + string.Join(", ", scc.Members) + "]: " + compileSccErr;
            }

            if (compileSccResult.IsOkOrNull() is not { } compileSccOk)
            {
                throw new NotImplementedException(
                    "Unexpected result type: " + compileSccResult.GetType());
            }

            compilationContext = compileSccOk;
        }

        // Third pass: Build module values from compiled functions
        var compiledModuleEntries = new List<PineValue>();

        foreach (var parsedModule in modulesForCompilation)
        {
            var moduleNameFlattened =
                string.Join('.', SyntaxTypes.Module.GetModuleName(parsedModule.ModuleDefinition.Value).Value);

            var moduleValue = BuildModuleValue(parsedModule, moduleNameFlattened, compilationContext);

            var namedModuleEntry =
                PineValue.List(
                    [
                    StringEncoding.ValueFromString(moduleNameFlattened),
                    moduleValue
                    ]);

            compiledModuleEntries.Add(namedModuleEntry);
        }

        var compiledEnvValue =
            PineValue.List(
                [
                ..compiledModuleEntries
                ]);

        var pipelineStageResults =
            new CompilationPipelineStageResults(
                Canonicalized: canonicalizedModules,
                LambdaLifted: lambdaLiftedModules,
                Specialized: optimizationResults?.AfterSpecialization,
                Inlined: optimizationResults?.AfterHigherOrderInlining,
                ModulesForCompilation: modulesForCompilation,
                OptimizationIterations: optimizationResults?.Iterations);

        return (compiledEnvValue, pipelineStageResults);
    }

    /// <summary>
    /// Pre-computes the dependency layouts and SCC mappings for all functions.
    /// For mutually recursive functions (strongly connected components), all functions
    /// share the same layout ordering as per the implementation guide.
    /// </summary>
    /// <param name="allFunctions">Dictionary of all functions keyed by qualified name.</param>
    /// <param name="context">The module compilation context.</param>
    /// <returns>
    /// A tuple containing:
    /// - Dictionary mapping qualified function names to their dependency layouts
    /// - Dictionary mapping qualified function names to their SCC
    /// - List of SCCs in dependency order (dependencies first)
    /// </returns>
    public static (IReadOnlyDictionary<string, IReadOnlyList<string>> layouts, IReadOnlyDictionary<string, FunctionScc> functionToScc, IReadOnlyList<FunctionScc> sccsInDependencyOrder)
        ComputeDependencyLayoutsAndSccs(
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)> allFunctions,
        ModuleCompilationContext context)
    {
        var allQualifiedFunctionNames =
            allFunctions.Keys
            .Select(QualifiedNameHelper.ToQualifiedNameString)
            .ToHashSet(StringComparer.Ordinal);

        // First pass: compute direct dependencies for each function
        // Only include dependencies that are in allFunctions (we can't compile external functions)
        var directDependencies =
            allFunctions
            .ToImmutableDictionary(
                kvp => QualifiedNameHelper.ToQualifiedNameString(kvp.Key),
                kvp =>
                {
                    var (moduleName, functionName, declaration) = kvp.Value;
                    var functionBody = declaration.Function.Declaration.Value.Expression.Value;
                    var dependencies = AnalyzeFunctionDependencies(functionBody, moduleName, context);
                    // Filter to only include functions that are in allFunctions
                    IReadOnlySet<string> filtered =
                        dependencies
                        .Where(d => allQualifiedFunctionNames.Contains(d))
                        .ToHashSet();

                    return filtered;
                });

        // Detect strongly connected components (SCCs) - groups of mutually recursive functions
        // Tarjan's algorithm returns SCCs in topological order (dependencies first)
        var sccsFromTarjan = FindStronglyConnectedComponents([.. directDependencies.Keys], directDependencies);

        // Build FunctionScc records - SCCs are already in dependency order from Tarjan
        var sccsInDependencyOrder = new List<FunctionScc>();

        // Compute transitive dependencies for each function
        var transitiveDependencies =
            directDependencies
            .ToDictionary(
                kvp => kvp.Key,
                kvp => ComputeTransitiveDependencies(kvp.Key, directDependencies));

        // Build SCCs and layouts
        var dependencyLayouts = new Dictionary<string, IReadOnlyList<string>>();

        var functionToScc = new Dictionary<string, FunctionScc>();

        foreach (var scc in sccsFromTarjan)
        {
            // Sort SCC members alphabetically for consistent ordering
            var sortedSccMembers =
                scc
                .Order(StringComparer.Ordinal)
                .ToImmutableList();

            // Compute the additional dependencies for this SCC
            // These are transitive dependencies that are not SCC members
            var allOtherDeps = new HashSet<string>();

            foreach (var member in scc)
            {
                if (transitiveDependencies.TryGetValue(member, out var deps))
                {
                    foreach (var d in deps)
                    {
                        if (!scc.Contains(d))
                        {
                            allOtherDeps.Add(d);
                        }
                    }
                }
            }

            var sortedOtherDeps =
                allOtherDeps
                .Order(StringComparer.Ordinal)
                .ToImmutableList();

            // Determine recursion: a multi-member SCC is always recursive
            // (mutual recursion). A single-member SCC is recursive iff its
            // member directly references itself.
            //
            // Non-recursive single-member SCCs still use the uniform
            // WithEnvFunctions wrapper shape (Approach A1), but the runtime
            // env-functions list is empty (per §7.6b).
            bool isRecursive;

            if (sortedSccMembers.Count > 1)
            {
                isRecursive = true;
            }
            else
            {
                var soleMember = sortedSccMembers[0];

                isRecursive =
                    directDependencies.TryGetValue(soleMember, out var soleMemberDeps)
                    && soleMemberDeps.Contains(soleMember);
            }

            // Create SCC with members and additional dependencies.
            // §7.6b: the runtime layout is members-only (see FunctionScc.GetLayout).
            // AdditionalDependencies is retained on the record for compilation
            // ordering only (Tarjan already gives us topological order, but we
            // keep the data for diagnostics and for the audit history).
            var sccRecord = new FunctionScc(sortedSccMembers, sortedOtherDeps, isRecursive);

            var sharedLayout = sccRecord.GetLayout();

            foreach (var member in scc)
            {
                functionToScc[member] = sccRecord;
                dependencyLayouts[member] = sharedLayout;
            }

            sccsInDependencyOrder.Add(sccRecord);
        }

        // No need to reverse - Tarjan already returns SCCs in dependency order

        return (dependencyLayouts, functionToScc, sccsInDependencyOrder);
    }

    /// <summary>
    /// Finds strongly connected components (SCCs) in the function call graph.
    /// Uses Tarjan's algorithm.
    /// </summary>
    private static List<HashSet<string>> FindStronglyConnectedComponents(
        IReadOnlyList<string> functions,
        ImmutableDictionary<string, IReadOnlySet<string>> dependencies)
    {
        var index = 0;
        var stack = new Stack<string>();
        var indices = new Dictionary<string, int>();
        var lowLinks = new Dictionary<string, int>();
        var onStack = new HashSet<string>();
        var sccs = new List<HashSet<string>>();

        void StrongConnect(string v)
        {
            indices[v] = index;
            lowLinks[v] = index;
            index++;
            stack.Push(v);
            onStack.Add(v);

            // Consider successors
            if (dependencies.TryGetValue(v, out var deps))
            {
                foreach (var w in deps)
                {
                    // Only consider functions we know about
                    if (!functions.Contains(w))
                        continue;

                    if (!indices.TryGetValue(w, out var value))
                    {
                        // Successor w has not yet been visited; recurse on it
                        StrongConnect(w);

                        lowLinks[v] = Math.Min(lowLinks[v], lowLinks[w]);
                    }
                    else if (onStack.Contains(w))
                    {
                        // Successor w is in stack S and hence in the current SCC
                        lowLinks[v] = Math.Min(lowLinks[v], value);
                    }
                }
            }

            // If v is a root node, pop the stack and generate an SCC
            if (lowLinks[v] == indices[v])
            {
                var scc = new HashSet<string>();
                string w;

                do
                {
                    w = stack.Pop();
                    onStack.Remove(w);
                    scc.Add(w);
                }
                while (w != v);

                sccs.Add(scc);
            }
        }

        foreach (var v in functions)
        {
            if (!indices.ContainsKey(v))
            {
                StrongConnect(v);
            }
        }

        return sccs;
    }

    /// <summary>
    /// Computes the transitive closure of dependencies for a function.
    /// This includes all functions that the function directly or indirectly calls,
    /// excluding the function itself (which is handled separately as index 0).
    /// </summary>
    private static HashSet<string> ComputeTransitiveDependencies(
        string functionName,
        ImmutableDictionary<string, IReadOnlySet<string>> directDependencies)
    {
        var visited = new HashSet<string>();
        var result = new HashSet<string>();
        var stack = new Stack<string>();

        // Mark self as visited to avoid including it in transitive dependencies
        // (self is always at index 0 in the layout)
        visited.Add(functionName);

        // Start with direct dependencies of the function
        if (directDependencies.TryGetValue(functionName, out var directDeps))
        {
            foreach (var dep in directDeps)
            {
                // Skip self-references
                if (dep != functionName)
                {
                    stack.Push(dep);
                }
            }
        }

        while (stack.Count > 0)
        {
            var current = stack.Pop();

            if (!visited.Add(current))
            {
                continue;
            }

            result.Add(current);

            // Add dependencies of this function
            if (directDependencies.TryGetValue(current, out var currentDeps))
            {
                foreach (var dep in currentDeps)
                {
                    if (!visited.Contains(dep))
                    {
                        stack.Push(dep);
                    }
                }
            }
        }

        return result;
    }

    /// <summary>
    /// Builds the module value from already-compiled functions.
    /// All functions should have been compiled in the SCC compilation pass.
    /// </summary>
    private static PineValue BuildModuleValue(
        SyntaxTypes.File parsedModule,
        string currentModuleName,
        ModuleCompilationContext context)
    {
        // Get declarations in this module (for building the final module value)
        var declarations =
            parsedModule.Declarations
            .Select(declNode => declNode.Value)
            .OfType<SyntaxTypes.Declaration.FunctionDeclaration>()
            .ToList();

        // Collect compiled functions for this module (in declaration order)
        var compiledFunctions = new List<(string, PineValue)>();

        foreach (var declaration in declarations)
        {
            var funcName = declaration.Function.Declaration.Value.Name.Value;

            var qualifiedName = currentModuleName + "." + funcName;

            var compiledValue =
                context.TryGetCompiledFunctionValue(qualifiedName)
                ?? throw new InvalidOperationException($"Function {qualifiedName} was not compiled.");

            compiledFunctions.Add((funcName, compiledValue));
        }

        var compiledFunctionDeclaration =
            new ElmModuleInCompilation(
                FunctionDeclarations: compiledFunctions);

        return EmitModuleValue(compiledFunctionDeclaration);
    }

    /*
    type alias ElmModuleInCompilation =
        { functionDeclarations : List ( String, Pine.Value )
        , typeDeclarations : List ( String, ElmModuleTypeDeclaration )
        }
     * */

    private record ElmModuleInCompilation(
        IReadOnlyList<(string declName, PineValue)> FunctionDeclarations);

    /// <summary>
    /// Compiles all functions in a strongly connected component (SCC) together.
    /// This ensures that all mutually recursive functions share the same envFunctionsList
    /// with correct references to each other.
    /// 
    /// Since SCCs are compiled in dependency order, all dependencies of the current SCC
    /// are already compiled and can be retrieved from the context.
    /// </summary>
    private static Result<CompilationError, ModuleCompilationContext> CompileSCC(
        FunctionScc scc,
        ModuleCompilationContext context,
        bool disableGenericApplicationChainConsolidation = false)
    {
        var sharedLayout = scc.GetLayout();
        var sccMembers = scc.Members;

        // Phase 1 of Approach A1: always use WithEnvFunctions layout.
        // All functions — recursive and non-recursive — have parameters at
        // env[1+i], with env[0] holding the SCC-members env-functions list
        // (empty for non-recursive single-member SCCs per §7.6b).

        // Pre-compute index lookup for efficient access
        var layoutIndexMap = new Dictionary<string, int>();

        for (var i = 0; i < sharedLayout.Count; i++)
        {
            layoutIndexMap[sharedLayout[i]] = i;
        }

        var parseCache = new PineVMParseCache();

        // Phase 1: Compile all function bodies in the SCC (without building wrappers yet)
        var compiledBodies = new Dictionary<string, (Expression body, int paramCount)>();

        foreach (var memberName in sccMembers)
        {
            if (context.TryGetFunctionInfo(memberName) is not { } funcInfo)
                continue;

            var declaration = funcInfo.declaration;
            var moduleName = funcInfo.moduleName;
            var functionBody = declaration.Function.Declaration.Value.Expression.Value;
            var arguments = declaration.Function.Declaration.Value.Arguments;
            var paramCount = arguments.Count;

            // Build parameter mappings
            var parameterNames = new Dictionary<string, int>();

            var localBindings = new Dictionary<string, Expression>();

            for (var i = 0; i < arguments.Count; i++)
            {
                var argPattern = arguments[i].Value;

                if (argPattern is SyntaxTypes.Pattern.VarPattern varPattern)
                {
                    parameterNames[varPattern.Name] = i;
                }
                else
                {
                    var paramExpr = BuiltinHelpers.BuildPathToParameter(i);
                    var analysis = PatternCompiler.AnalyzePattern(argPattern, paramExpr);

                    foreach (var kvp in analysis.Bindings)
                    {
                        localBindings[kvp.Key] = kvp.Value;
                    }
                }
            }

            // Extract parameter types
            var parameterTypes =
                ExtractParameterTypes(
                    declaration.Function,
                    context.ChoiceTagArgumentTypes,
                    context.FunctionTypes,
                    moduleName);

            // Create expression context
            var expressionContext =
                new ExpressionCompilationContext(
                    ParameterNames: parameterNames,
                    ParameterTypes: parameterTypes,
                    CurrentModuleName: moduleName,
                    CurrentFunctionName: declaration.Function.Declaration.Value.Name.Value,
                    LocalBindings: localBindings.Count > 0 ? localBindings : null,
                    LocalBindingTypes: null,
                    DependencyLayout: sharedLayout,
                    ModuleCompilationContext: context,
                    FunctionTypes: context.FunctionTypes);

            // Compile the body
            var compileBodyResult = ExpressionCompiler.Compile(functionBody, expressionContext);

            if (compileBodyResult.IsErrOrNull() is { } compileErr)
            {
                return CompilationError.Scoped("Failed compiling declaration '" + memberName + "'", compileErr);
            }

            if (compileBodyResult.IsOkOrNull() is not { } compiledBody)
            {
                throw new NotImplementedException(
                    "Unexpected result type: " + compileBodyResult.GetType());
            }

            compiledBody =
                ReducePineExpression.ReduceExpressionBottomUp(
                    compiledBody,
                    parseCache,
                    disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation);

            compiledBodies[memberName] = (compiledBody, paramCount);
        }

        var encodedBodies =
            compiledBodies
            .ToDictionary(
                kvp => kvp.Key,
                kvp => ExpressionEncoding.EncodeExpressionAsValue(kvp.Value.body));

        // Phase 2: Build the envFunctionsList for SCC members.
        // §7.6b: for recursive SCCs sharedLayout is the member list, so
        // envFunctionsList contains the encoded bodies of the members in the
        // same order. Cross-SCC dependencies are fetched at the *call sites*
        // via Literal(callee.EncodedBody) and Literal(List(callee.EnvFunctions)),
        // not threaded through current_env[0] of every transitive caller.
        // For non-recursive single-member SCCs sharedLayout is empty, so
        // envFunctionsList is empty too — but the wrapper still uses the
        // uniform WithEnvFunctions shape with env[0] holding the empty list
        // (Approach A1).
        var envFunctionsList =
            sharedLayout
            .Select(
                (declName, depIndex) =>
                {
                    if (encodedBodies.TryGetValue(declName, out var encodedBodyDep))
                    {
                        return encodedBodyDep;
                    }

                    // Defensive: every entry of sharedLayout is an SCC member,
                    // and we just compiled all members above. If we ever reach
                    // this branch the layout/SCC bookkeeping is out of sync.
                    throw new InvalidOperationException(
                        $"SCC member {declName} missing encoded body when compiling SCC " +
                        $"{string.Join(", ", scc.Members)}. " +
                        "All SCC members should be compiled in Phase 1 before reaching Phase 2.");
                })
            .ToList();

        // Phase 3: Build final wrappers and cache all compiled functions
        foreach (var memberName in sccMembers)
        {
            if (!compiledBodies.TryGetValue(memberName, out var bodyInfo))
                continue;

            // Phase 1 of Approach A1: always emit WithEnvFunctions wrapper.
            // For non-recursive single-member SCCs, envFunctionsList is empty
            // (per §7.6b), so this produces a wrapper with env[0] = List([]).
            var wrapper =
                FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                    bodyInfo.body,
                    bodyInfo.paramCount,
                    envFunctionsList);

            var encodedBody = encodedBodies[memberName];

            context =
                context.WithCompiledFunction(
                    memberName,
                    wrapper,
                    encodedBody,
                    sharedLayout,
                    parameterCount: bodyInfo.paramCount,
                    envFunctions: envFunctionsList);
        }

        return context;
    }

    private static IReadOnlySet<string> AnalyzeFunctionDependencies(
        SyntaxTypes.Expression expression,
        string currentModuleName,
        ModuleCompilationContext context)
    {
        var dependencies = new HashSet<string>();

        void AnalyzeExpression(SyntaxTypes.Expression expr)
        {
            switch (expr)
            {
                case SyntaxTypes.Expression.Application application:

                    if (application.Arguments.Count >= 2 &&
                        application.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue funcRef)
                    {
                        // Skip Pine_kernel functions
                        if (funcRef.ModuleName.Count is 1 && context.IsPineKernelModule(funcRef.ModuleName[0]))
                        {
                            // Still recurse into arguments
                            foreach (var arg in application.Arguments)
                            {
                                AnalyzeExpression(arg.Value);
                            }

                            break;
                        }

                        // Skip choice type tag constructors
                        if (ElmValueEncoding.StringIsValidTagName(funcRef.Name))
                        {
                            // Still recurse into arguments
                            foreach (var arg in application.Arguments)
                            {
                                AnalyzeExpression(arg.Value);
                            }

                            break;
                        }

                        // Determine qualified name
                        string qualifiedName;

                        if (funcRef.ModuleName.Count > 0)
                        {
                            qualifiedName = string.Join(".", funcRef.ModuleName) + "." + funcRef.Name;
                        }
                        else
                        {
                            qualifiedName = currentModuleName + "." + funcRef.Name;
                        }

                        dependencies.Add(qualifiedName);
                    }

                    // Recurse into arguments
                    foreach (var arg in application.Arguments)
                    {
                        AnalyzeExpression(arg.Value);
                    }

                    break;

                case SyntaxTypes.Expression.OperatorApplication operatorApp:
                    AnalyzeExpression(operatorApp.Left.Value);
                    AnalyzeExpression(operatorApp.Right.Value);
                    break;

                case SyntaxTypes.Expression.IfBlock ifBlock:
                    AnalyzeExpression(ifBlock.Condition.Value);
                    AnalyzeExpression(ifBlock.ThenBlock.Value);
                    AnalyzeExpression(ifBlock.ElseBlock.Value);
                    break;

                case SyntaxTypes.Expression.LetExpression letExpr:
                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                                AnalyzeExpression(letFunc.Function.Declaration.Value.Expression.Value);
                                break;

                            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                                AnalyzeExpression(letDestr.Expression.Value);
                                break;
                        }
                    }

                    AnalyzeExpression(letExpr.Value.Expression.Value);
                    break;

                case SyntaxTypes.Expression.ListExpr listExpr:
                    foreach (var elem in listExpr.Elements)
                    {
                        AnalyzeExpression(elem.Value);
                    }

                    break;

                case SyntaxTypes.Expression.ParenthesizedExpression parenthesized:
                    AnalyzeExpression(parenthesized.Expression.Value);
                    break;

                case SyntaxTypes.Expression.Negation negation:
                    AnalyzeExpression(negation.Expression.Value);
                    break;

                case SyntaxTypes.Expression.CaseExpression caseExpr:
                    AnalyzeExpression(caseExpr.CaseBlock.Expression.Value);

                    foreach (var caseItem in caseExpr.CaseBlock.Cases)
                    {
                        AnalyzeExpression(caseItem.Expression.Value);
                    }

                    break;

                case SyntaxTypes.Expression.TupledExpression tupledExpr:
                    foreach (var elem in tupledExpr.Elements)
                    {
                        AnalyzeExpression(elem.Value);
                    }

                    break;

                case SyntaxTypes.Expression.RecordExpr recordExpr:
                    foreach (var field in recordExpr.Fields)
                    {
                        AnalyzeExpression(field.Value.valueExpr.Value);
                    }

                    break;

                case SyntaxTypes.Expression.RecordUpdateExpression recordUpdateExpr:
                    foreach (var setter in recordUpdateExpr.Fields)
                    {
                        AnalyzeExpression(setter.Value.valueExpr.Value);
                    }

                    break;

                case SyntaxTypes.Expression.RecordAccess recordAccess:
                    AnalyzeExpression(recordAccess.Record.Value);
                    break;

                case SyntaxTypes.Expression.LambdaExpression lambdaExpr:
                    AnalyzeExpression(lambdaExpr.Lambda.Expression.Value);
                    break;

                case SyntaxTypes.Expression.FunctionOrValue funcOrValue:

                    if (funcOrValue.Name.Length < 0)
                    {
                        break;
                    }

                    if (char.IsUpper(funcOrValue.Name[0]))
                    {
                        // This must be a choice type tag constructor or record constructor - cannot contribute to dependencies
                        break;
                    }

                    {
                        // Handle standalone function references (escaping functions)
                        // This is when a function is used as a value, not applied

                        string qualifiedName;

                        if (funcOrValue.ModuleName.Count > 0)
                        {
                            // Skip Pine_kernel module functions
                            if (funcOrValue.ModuleName.Count is 1 &&
                                context.IsPineKernelModule(funcOrValue.ModuleName[0]))
                            {
                                break;
                            }

                            qualifiedName = string.Join(".", funcOrValue.ModuleName) + "." + funcOrValue.Name;
                        }
                        else
                        {
                            qualifiedName = currentModuleName + "." + funcOrValue.Name;
                        }

                        // Only add if it's a known function (not a local variable)
                        if (context.TryGetFunctionInfo(qualifiedName) is not null)
                        {
                            dependencies.Add(qualifiedName);
                        }
                    }

                    break;
            }
        }

        AnalyzeExpression(expression);

        return dependencies;
    }

    private static ImmutableDictionary<string, TypeInference.InferredType> ExtractParameterTypes(
        SyntaxTypes.FunctionStruct function,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>? constructorArgumentTypes,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, FunctionTypeInfo>? functionTypes,
        string currentModuleName)
    {
        var parameterTypes = ImmutableDictionary<string, TypeInference.InferredType>.Empty;

        if (function.Signature?.Value is { } signature)
        {
            var typeAnnotation = signature.TypeAnnotation.Value;
            var parameters = function.Declaration.Value.Arguments;

            // Walk through the function type annotation to extract parameter types
            var currentType = typeAnnotation;

            var paramIndex = 0;

            while (currentType is SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation funcType &&
                paramIndex < parameters.Count)
            {
                var paramPattern = parameters[paramIndex].Value;
                var paramTypeAnnotation = funcType.ArgumentType.Value;

                // Extract binding types from the pattern
                // This handles both simple VarPattern and complex patterns like TuplePattern
                parameterTypes =
                    TypeInference.ExtractPatternBindingTypes(paramPattern, paramTypeAnnotation, parameterTypes);

                currentType = funcType.ReturnType.Value;
                paramIndex++;
            }
        }

        // Also extract types from NamedPattern parameters using constructor argument types
        // This handles cases where the function has no type signature but uses choice type patterns
        if (constructorArgumentTypes is not null)
        {
            var parameters = function.Declaration.Value.Arguments;

            foreach (var paramNode in parameters)
            {
                parameterTypes =
                    TypeInference.ExtractPatternBindingTypesWithConstructors(
                        paramNode.Value,
                        constructorArgumentTypes,
                        parameterTypes);
            }

            // Also analyze the function body for tag applications that constrain parameter types
            // For example, in `alfa a b = let c = TagAlfa a in ...`, we infer that `a` is Int
            var functionBody = function.Declaration.Value.Expression.Value;

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
            var functionBody = function.Declaration.Value.Expression.Value;

            parameterTypes =
                TypeInference.ExtractTypeConstraintsFromFunctionApplications(
                    functionBody,
                    functionTypes,
                    currentModuleName,
                    parameterTypes);
        }

        return parameterTypes;
    }

    private static Expression CompileExpression(
        SyntaxTypes.Expression expression,
        ExpressionCompilationContext context)
    {
        var result = ExpressionCompiler.Compile(expression, context);

        if (result.IsErrOrNull() is { } error)
        {
            throw new NotImplementedException(error.ToString());
        }

        return result.IsOkOrNull()!;
    }

    private static PineValue EmitPlainValueDeclaration(PineValue value)
    {
        return
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                Expression.LiteralInstance(value),
                parameterCount: 0,
                envFunctions: []);
    }

    private static PineValue EmitModuleValue(
        ElmModuleInCompilation compiledModule)
    {
        /*
        emitModuleValue : ElmModuleInCompilation -> Pine.Value
        emitModuleValue parsedModule =
            let
                typeDescriptions : List ( String, Pine.Value )
                typeDescriptions =
                    List.foldr
                        (\( typeName, typeDeclaration ) aggregate ->
                            ( typeName, emitTypeDeclarationValue typeDeclaration ) :: aggregate
                        )
                        []
                        parsedModule.typeDeclarations
            in
            Pine.ListValue
                (List.map Pine.valueFromContextExpansionWithName
                    (List.concat [ parsedModule.functionDeclarations, typeDescriptions ])
                )
         * */

        IReadOnlyList<(string declName, PineValue declValue)> typeDescriptions = [];

        PineValue[] entries =
            [
            .. compiledModule.FunctionDeclarations
            .Concat(typeDescriptions)
            .Select(tuple => ValueFromContextExpansionWithName(tuple.declName, tuple.Item2))
            ];

        return PineValue.List(entries);
    }

    private static PineValue ValueFromContextExpansionWithName(
        string name,
        PineValue pineValue)
    {
        /*
        valueFromContextExpansionWithName : ( String, Value ) -> Value
        valueFromContextExpansionWithName ( declName, declValue ) =
            ListValue [ computeValueFromString declName, declValue ]
         * */

        return PineValue.List([StringEncoding.ValueFromString(name), pineValue]);
    }

    private static Result<string, OptimizationPipelineStageResults> ApplyOptimizationPipelineWithStageResults(
        List<SyntaxTypes.File> lambdaLiftedModules,
        int maxRounds)
    {
        // Flatten the input modules into a declaration dictionary.
        // The pipeline operates on this flat representation throughout.
        var currentDecls = FlattenModulesToDeclarationDictionary(lambdaLiftedModules);

        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> afterSpecialization = currentDecls;
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> afterHigherOrderInlining = currentDecls;
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> afterLambdaLifting = currentDecls;

        var iterationResults = ImmutableList.CreateBuilder<OptimizationIterationStageResults>();

        for (var round = 0; round < maxRounds; round++)
        {
            var declsBefore = currentDecls;

            // Phase 1: Specialization — create specialized versions of functions for known argument patterns.
            // Lambda lifting is combined: the transformation stage lifts any lambdas/local functions it introduces.
            {
                var result =
                    ElmSyntaxSpecialization.Apply(currentDecls, Inlining.Config.OnlyFunctions);

                if (result.IsErrOrNull() is { } specErr)
                    return $"Specialization (round {round}) failed: " + specErr;

                if (result.IsOkOrNull() is not { } specDict)
                    throw new NotImplementedException("Unexpected result type");

                currentDecls = specDict;
            }

            afterSpecialization = currentDecls;

            if (CheckForNamingErrors(currentDecls, $"Specialization (round {round})") is { } specShadowErr)
                return specShadowErr;

            if (CheckForSyntaxNodesDisallowedInOptimization(currentDecls, $"Specialization (round {round})") is { } specDisallowedErr)
                return specDisallowedErr;

            // Phase 2: Higher-order inlining — inline functions that receive function arguments.
            // Lambda lifting is combined: the transformation stage lifts any lambdas/local functions it introduces.
            {
                var result =
                    ElmSyntaxInlining.Apply(currentDecls, Inlining.Config.OnlyFunctions);

                if (result.IsErrOrNull() is { } hoInlineErr)
                    return $"Higher-order inlining (round {round}) failed: " + hoInlineErr;

                if (result.IsOkOrNull() is not { } hoInlineDict)
                    throw new NotImplementedException("Unexpected result type");

                currentDecls = hoInlineDict;
            }

            afterHigherOrderInlining = currentDecls;

            if (CheckForNamingErrors(currentDecls, $"Higher-order inlining (round {round})") is { } hoInlineShadowErr)
                return hoInlineShadowErr;

            if (CheckForSyntaxNodesDisallowedInOptimization(currentDecls, $"Higher-order inlining (round {round})") is { } hoInlineDisallowedErr)
                return hoInlineDisallowedErr;

            // Phase 3: Size-based inlining and plain value inlining.
            // Runs after higher-order inlining to avoid cascading.
            // Inlines small wrapper functions (≤10 AST nodes, no complex expressions) and
            // plain zero-parameter declarations with simple bodies.
            // Lambda lifting is combined: the transformation stage lifts any lambdas/local functions it introduces.
            {
                var result =
                    ElmSyntaxInlining.Apply(currentDecls, Inlining.Config.SmallFunctionsAndPlainValues);

                if (result.IsErrOrNull() is { } sizeInlineErr)
                    return $"Size-based inlining (round {round}) failed: " + sizeInlineErr;

                if (result.IsOkOrNull() is not { } sizeInlineDict)
                    throw new NotImplementedException("Unexpected result type");

                currentDecls = sizeInlineDict;
            }

            afterLambdaLifting = currentDecls;

            if (CheckForNamingErrors(currentDecls, $"Size-based inlining (round {round})") is { } sizeInlineShadowErr)
                return sizeInlineShadowErr;

            if (CheckForSyntaxNodesDisallowedInOptimization(currentDecls, $"Size-based inlining (round {round})") is { } sizeInlineDisallowedErr)
                return sizeInlineDisallowedErr;

            // Record this iteration's intermediate results.
            iterationResults.Add(
                new OptimizationIterationStageResults(
                    Round: round,
                    AfterSpecialization: afterSpecialization,
                    AfterHigherOrderInlining: afterHigherOrderInlining,
                    AfterSizeBasedInlining: afterLambdaLifting));

            // Convergence check: if the output matches the input, no further rounds are needed.
            if (currentDecls.Count == declsBefore.Count &&
                currentDecls.All(kvp => declsBefore.TryGetValue(kvp.Key, out var prev) && prev.Equals(kvp.Value)))
            {
                break;
            }
        }

        // Phase 4: Operator lowering — convert operators to Pine built-in calls.
        // Runs once after the convergence loop since lowering does not expose new inlining opportunities.
        {
            var result = BuiltinOperatorLowering.Apply(currentDecls);

            if (result.IsErrOrNull() is { } lowerErr)
                return "Operator lowering failed: " + lowerErr;

            if (result.IsOkOrNull() is not { } lowerDict)
                throw new NotImplementedException("Unexpected result type");

            currentDecls = lowerDict;
        }

        if (CheckForNamingErrors(currentDecls, "Operator lowering") is { } lowerShadowErr)
            return lowerShadowErr;

        return
            new OptimizationPipelineStageResults(
                AfterSpecialization: afterSpecialization,
                AfterHigherOrderInlining: afterHigherOrderInlining,
                AfterLambdaLifting: afterLambdaLifting,
                AfterLowering: currentDecls,
                Iterations: iterationResults.ToImmutable());
    }

    /// <summary>
    /// Checks for naming clashes and shadowings in a flat declaration dictionary
    /// by running naming error detection directly on the declarations without
    /// reconstructing module files or running full canonicalization.
    /// Returns an error message if any problems are found, or null if the declarations are clean.
    /// Problems in the output of a compilation stage indicate a defect in that stage
    /// (e.g., inlining introduced a naming conflict).
    /// </summary>
    private static string? CheckForNamingErrors(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        string stageName)
    {
        var (errors, shadowings) =
            NamingErrorDetection.DetectNamingErrorsInFlatDict(declarations);

        return FormatNamingErrorProblems(errors, shadowings, stageName);
    }

    /// <summary>
    /// Checks for naming clashes and shadowings in a list of module files
    /// by running naming error detection directly on the declarations without
    /// running full canonicalization.
    /// Returns an error message if any problems are found, or null if the declarations are clean.
    /// Problems in the output of a compilation stage indicate a defect in that stage
    /// (e.g., lambda lifting introduced a naming conflict).
    /// </summary>
    private static string? CheckForNamingErrors(
        IReadOnlyList<SyntaxTypes.File> modules,
        string stageName)
    {
        var flatDecls = FlattenModulesToDeclarationDictionary(modules);

        return CheckForNamingErrors(flatDecls, stageName);
    }

    /// <summary>
    /// Checks that declarations do not contain syntax nodes that are disallowed in the
    /// optimization pipeline (e.g., lambda expressions and let-functions that should have
    /// been lifted to top-level functions).
    /// Returns an error message if any disallowed nodes are found, or null if clean.
    /// </summary>
    private static string? CheckForSyntaxNodesDisallowedInOptimization(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        string stageName)
    {
        var disallowedLocations = new List<(DeclQualifiedName DeclName, string Detail)>();

        foreach (var (qualifiedName, decl) in declarations)
        {
            SyntaxTypes.ReportSyntaxNodes.ReportViaCallback(
                decl,
                reportLambda:
                _ =>
                disallowedLocations.Add((qualifiedName, "lambda expression")),
                reportLetBlock:
                _ => { },
                reportLetFunction:
                letFunc =>
                {
                    // Only flag LetFunction nodes that have parameters (actual function declarations).
                    // Zero-parameter LetFunction nodes represent value bindings that should not be lifted.
                    if (letFunc.Function.Declaration.Value.Arguments.Count > 0)
                    {
                        disallowedLocations.Add(
                            (qualifiedName, "let-function '" + letFunc.Function.Declaration.Value.Name.Value + "'"));
                    }
                });
        }

        if (disallowedLocations.Count is 0)
            return null;

        var grouped =
            disallowedLocations
            .GroupBy(loc => loc.DeclName)
            .OrderBy(g => g.Key)
            .Select(
                g =>
                {
                    var lambdaCount = g.Count(loc => loc.Detail is "lambda expression");
                    var letFuncCount = g.Count(loc => loc.Detail.StartsWith("let-function", StringComparison.Ordinal));
                    var parts = new List<string>();

                    if (lambdaCount > 0)
                        parts.Add(lambdaCount + " lambda expression(s)");

                    if (letFuncCount > 0)
                        parts.Add(letFuncCount + " let-function(s)");

                    return
                        g.Key.DeclName + " in " + string.Join(".", g.Key.Namespaces) +
                        ": " + string.Join(", ", parts);
                });

        return
            stageName + " produced declarations containing " +
            disallowedLocations.Count + " disallowed node(s):\n" +
            string.Join("\n", grouped);
    }

    /// <summary>
    /// Checks that module declarations do not contain syntax nodes that are disallowed after
    /// lambda lifting.
    /// Returns an error message if any disallowed nodes are found, or null if clean.
    /// </summary>
    private static string? CheckForSyntaxNodesDisallowedInOptimization(
        IReadOnlyList<SyntaxTypes.File> modules,
        string stageName)
    {
        var flatDecls = FlattenModulesToDeclarationDictionary(modules);

        return CheckForSyntaxNodesDisallowedInOptimization(flatDecls, stageName);
    }

    /// <summary>
    /// Formats naming errors and shadowings into a human-readable problem string.
    /// Returns null if there are no problems.
    /// </summary>
    private static string? FormatNamingErrorProblems(
        IReadOnlyList<CanonicalizationError> errors,
        ImmutableDictionary<string, ShadowingLocation> shadowings,
        string stageName)
    {
        var problems = new List<string>();

        // Check for shadowings
        if (shadowings.Count > 0)
        {
            var allShadowedNames = shadowings.Keys.ToList();

            problems.Add(
                stageName + " produced " + allShadowedNames.Count +
                " shadowing(s): " + string.Join(", ", allShadowedNames));
        }

        // Check for all error types using the typed error hierarchy.
        {
            var allNamingClashes = new List<string>();

            foreach (var error in errors)
            {
                switch (error)
                {
                    case CanonicalizationError.NamingClash clash:
                        allNamingClashes.Add(clash.Name);
                        break;
                }
            }

            if (allNamingClashes.Count > 0)
            {
                var distinctNames = allNamingClashes.Distinct().OrderBy(n => n);

                problems.Add(
                    stageName + " produced " + allNamingClashes.Count +
                    " naming clash(es): " + string.Join(", ", distinctNames));
            }
        }

        if (problems.Count is 0)
            return null;

        return string.Join("\n", problems);
    }

    /// <summary>
    /// Renders a <see cref="CanonicalizationError"/> as a human-readable string.
    /// </summary>
    private static string RenderCanonicalizationError(CanonicalizationError error) =>
        error switch
        {
            CanonicalizationError.UnresolvedReference unresolved =>
            $"Cannot find '{unresolved.Name}'",

            CanonicalizationError.NamingClash clash =>
            clash.ShadowedRange is { } shadowedRange
            ?
            $"Name '{clash.Name}' at {clash.Range.Start.Row}:{clash.Range.Start.Column} is shadowing an existing declaration at {shadowedRange.Start.Row}:{shadowedRange.Start.Column}"
            :
            $"Name '{clash.Name}' at {clash.Range.Start.Row}:{clash.Range.Start.Column} is shadowing an existing declaration",

            CanonicalizationError.AmbiguousImport ambiguous =>
            $"Name '{ambiguous.Name}' is exposed by multiple imports: {string.Join(", ", ambiguous.ImportingModules)}",

            _ =>
            $"Unknown canonicalization error at {error.Range}"
        };

    /// <summary>
    /// Flattens a list of modules into an immutable dictionary of declarations keyed by
    /// their fully qualified name.
    /// </summary>
    internal static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        FlattenModulesToDeclarationDictionary(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var module in modules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;

            foreach (var declNode in module.Declarations)
            {
                var decl = declNode.Value;
                var declName = GetDeclarationName(decl);

                if (declName is null)
                    continue;

                var qualifiedName =
                    new DeclQualifiedName(
                        Namespaces: moduleName,
                        DeclName: declName);

                builder[qualifiedName] = decl;
            }
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Reconstructs a list of module files from a flat declaration dictionary and the original
    /// module shells. For each original module, the declarations list is rebuilt by replacing
    /// function declarations with those from the flat dictionary (which may have been transformed
    /// by optimization passes) while preserving type declarations, alias declarations, and other
    /// non-function declarations from the original modules.
    /// New declarations that were added by the pipeline (e.g., specializations) are appended
    /// to their respective module.
    /// </summary>
    internal static List<SyntaxTypes.File> ReconstructModulesFromFlatDict(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> flatDecls,
        IReadOnlyList<SyntaxTypes.File> originalModules)
    {
        // Track which declarations from the flat dict we've placed into a module.
        var placedKeys = new HashSet<DeclQualifiedName>();

        var result = new List<SyntaxTypes.File>(originalModules.Count);

        foreach (var originalModule in originalModules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(originalModule.ModuleDefinition.Value).Value;

            var newDeclarations = new List<SyntaxModelTypes.Node<SyntaxTypes.Declaration>>();

            // Walk the original declarations in order, replacing function declarations
            // from the flat dict and keeping non-function declarations as-is.
            foreach (var declNode in originalModule.Declarations)
            {
                var declName = GetDeclarationName(declNode.Value);

                if (declName is not null)
                {
                    var qualifiedName =
                        new DeclQualifiedName(
                            Namespaces: moduleName,
                            DeclName: declName);

                    if (flatDecls.TryGetValue(qualifiedName, out var replacementDecl))
                    {
                        newDeclarations.Add(declNode with { Value = replacementDecl });
                        placedKeys.Add(qualifiedName);
                        continue;
                    }
                }

                // Declaration not in the flat dict (e.g., was removed or is unknown type).
                // Keep the original.
                newDeclarations.Add(declNode);
            }

            // Append any new declarations for this module that weren't in the original.
            // Sort by declaration name for deterministic ordering in snapshot tests.
            var newModuleDecls =
                flatDecls
                .Where(kvp => !placedKeys.Contains(kvp.Key) && kvp.Key.Namespaces.SequenceEqual(moduleName))
                .OrderBy(kvp => kvp.Key)
                .ToList();

            foreach (var (key, decl) in newModuleDecls)
            {
                newDeclarations.Add(
                    new SyntaxModelTypes.Node<SyntaxTypes.Declaration>(
                        ElmSyntaxTransformations.s_zeroRange,
                        decl));

                placedKeys.Add(key);
            }

            result.Add(
                originalModule with
                {
                    Declarations = newDeclarations
                });
        }

        return result;
    }

    /// <summary>
    /// Extracts the declaration name from a syntax declaration.
    /// </summary>
    internal static string? GetDeclarationName(SyntaxTypes.Declaration decl) =>
        decl switch
        {
            SyntaxTypes.Declaration.FunctionDeclaration funcDecl =>
            funcDecl.Function.Declaration.Value.Name.Value,

            SyntaxTypes.Declaration.CustomTypeDeclaration typeDecl =>
            typeDecl.TypeDeclaration.Name.Value,

            SyntaxTypes.Declaration.AliasDeclaration aliasDecl =>
            aliasDecl.TypeAlias.Name.Value,

            SyntaxTypes.Declaration.PortDeclaration portDecl =>
            portDecl.Signature.Name.Value,

            SyntaxTypes.Declaration.InfixDeclaration infixDecl =>
            infixDecl.Infix.Operator.Value,

            _ =>
            null
        };

    public static PineValue Compile(
        AppCompilationUnits compilationUnits)
    {
        throw new NotImplementedException();
    }
}
