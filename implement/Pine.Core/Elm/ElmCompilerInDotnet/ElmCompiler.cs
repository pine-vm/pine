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
using AbstractSyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Holds the intermediate results of each stage in the Elm compilation pipeline.
/// The <see cref="Canonicalized"/> stage keeps the per-module representation; the
/// <typeparamref name="LoweredT"/> payload captures whatever lowering result a caller
/// produces from the canonicalized declaration dictionary (e.g. the standard
/// lambda-lift + specialize + inline pipeline output via
/// <see cref="DefaultLoweredResults"/>, or any alternative lowering wired in via the
/// generic overload of
/// <see cref="ElmCompiler.LowerToElmSyntaxForCompilation{LoweredT}"/>). The final
/// <see cref="ModulesForCompilation"/> always uses the per-module representation
/// expected by the compilation backend.
/// </summary>
/// <typeparam name="LoweredT">
/// Caller-defined lowering payload produced by the lowering delegate passed to
/// <see cref="ElmCompiler.LowerToElmSyntaxForCompilation{LoweredT}"/>.
/// </typeparam>
/// <param name="Canonicalized">
/// Modules after canonicalization: all names are resolved to fully-qualified forms.
/// </param>
/// <param name="Lowered">
/// The caller-defined lowering payload. Replaces the previous <c>LambdaLifted</c>
/// and <c>Specialized</c> properties; the standard pipeline surfaces those
/// individual stages via <see cref="DefaultLoweredResults"/>.
/// </param>
/// <param name="ModulesForCompilation">
/// The final list of modules passed into the compilation backend, reconstructed
/// from the flat declaration dictionary returned by the caller-provided
/// <c>extractFilteredDeclarations</c> delegate.
/// </param>
/// <param name="OptimizationIterations">
/// When the standard optimization pipeline runs more than one round, this list
/// captures the intermediate results of each round for inspection and debugging.
/// Populated only by the back-compat overload of
/// <see cref="ElmCompiler.LowerToElmSyntaxForCompilation"/>; the generic overload
/// leaves this <c>null</c> (callers can surface equivalent per-round data
/// through their own <typeparamref name="LoweredT"/> payload).
/// </param>
public record CompilationPipelineStageResults<LoweredT>(
    IReadOnlyList<SyntaxTypes.File> Canonicalized,
    LoweredT Lowered,
    IReadOnlyList<SyntaxTypes.File> ModulesForCompilation,
    ImmutableList<OptimizationIterationStageResults>? OptimizationIterations = null);

/// <summary>
/// Standard lowering payload produced by the back-compat overload of
/// <see cref="ElmCompiler.LowerToElmSyntaxForCompilation"/>. Surfaces the
/// pre-optimization <see cref="LambdaLifted"/> module list and the
/// post-specialization declaration snapshot via <see cref="Specialized"/>.
/// </summary>
/// <param name="LambdaLifted">
/// Modules after the initial lambda lifting pass: closures are transformed into top-level functions.
/// </param>
/// <param name="Specialized">
/// Declarations after specialization: specialized function variants are created
/// for known argument patterns. This is <c>null</c> when <c>disableInlining</c>
/// is true, as the optimization pipeline is skipped.
/// </param>
/// <param name="FilteredDeclarations">
/// Flat declaration dictionary handed to the compilation backend after
/// reachability-filtering from the root declarations. Surfaced here so the
/// back-compat overload can return it from the
/// <c>extractFilteredDeclarations</c> delegate.
/// </param>
public record DefaultLoweredResults(
    IReadOnlyList<SyntaxTypes.File> LambdaLifted,
    OptimizedElmSyntaxDeclarations? Specialized,
    ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> FilteredDeclarations);

/// <summary>
/// Holds the intermediate results of each stage within a single optimization iteration.
/// </summary>
public record OptimizationIterationStageResults(
    int Round,
    OptimizedElmSyntaxDeclarations AfterSpecialization);

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
    /// <summary>
    /// Default configuration for the Elm syntax transformations run as part of the standard compilation pipeline.
    /// </summary>
    public static readonly ElmSyntaxOptimizationConfig.SyntaxOptimizationEnabled SyntaxOptimizationConfigDefault =
        new ElmSyntaxOptimizationConfig.SyntaxOptimizationEnabled(
            MaxOptimizationRounds: OptimizationRoundsDefault,
            SizeBasedInliningConfigOverride: null,
            MaxSizeBasedInliningRounds: OptimizationRoundsDefault,
            RunSpecializationBeforeLambdaLifting: true,
            InlineLetDestructureThunks: true);

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
            "Debug",
            ]);

    /// <summary>
    /// Default value for the number of rounds of the optimization pipeline to run.
    /// </summary>
    public const int OptimizationRoundsDefault = 4;

    /// <summary>
    /// Compiles an interactive Elm environment from the given source tree.
    /// <para>
    /// Parses every <c>.elm</c> file reachable from <paramref name="rootFilePaths"/>,
    /// canonicalizes and lambda-lifts the result, optionally runs the optimization
    /// pipeline (specialization, higher-order inlining, size-based inlining,
    /// operator lowering) and emits the compiled <see cref="PineValue"/> environment
    /// alongside <see cref="CompilationPipelineStageResults{LoweredT}"/> capturing the
    /// intermediate stage outputs for inspection and debugging.
    /// </para>
    /// </summary>
    /// <param name="appCodeTree">
    /// Source tree containing the application's Elm files. The bundled
    /// elm-core kernel modules are merged in automatically, so callers should
    /// only supply application/package sources.
    /// </param>
    /// <param name="rootFilePaths">
    /// Entry-point file paths used as roots of the compilation closure. Only
    /// modules transitively reachable from these are compiled.
    /// </param>
    /// <param name="syntaxOptimization">
    /// Configures the Elm syntax optimization stage. Use
    /// <see cref="ElmSyntaxOptimizationConfig.SyntaxOptimizationDisabled"/> to skip
    /// the optimization pipeline (specialization, inlining, operator lowering)
    /// entirely; the compiled environment is then produced directly from the
    /// lambda-lifted output. Use
    /// <see cref="ElmSyntaxOptimizationConfig.SyntaxOptimizationEnabled"/> to run the
    /// pipeline; its carried flags configure the individual stages (number of
    /// optimization and size-based inlining rounds, an optional size-based inlining
    /// configuration override, the experimental pre-lifting specialization rounds,
    /// and the experimental let-destructure-thunk inlining cleanup). Modelling these
    /// as a choice type makes nonsensical combinations (such as requesting
    /// pre-lifting specialization while the pipeline is disabled) unrepresentable.
    /// When <c>null</c>, defaults to
    /// <see cref="ElmSyntaxOptimizationConfig.SyntaxOptimizationEnabled"/> with its
    /// default flags.
    /// </param>
    /// <summary>
    /// Runs the parse → canonicalize → lambda-lift → optimize/specialize/inline
    /// pipeline on <paramref name="appCodeTree"/> and returns the
    /// <see cref="CompilationPipelineStageResults"/> ready to be handed to the
    /// Pine emission backend. This is the lowering half of
    /// <see cref="CompileInteractiveEnvironment"/>; emission to Pine is the
    /// expensive remaining half.
    /// <para>
    /// Diagnostic callers that only need to inspect the post-lowering Elm
    /// syntax (e.g. monomorphization-opportunity reports, snapshot-format
    /// dumps, declaration-size rankings) can call this method directly to
    /// avoid the per-SCC <see cref="CompileSCC"/> /
    /// <c>ExpressionEncoding.EncodeExpressionAsValue</c> work, which in
    /// profiling dominates compile time (&gt;99 %) even with
    /// <c>PineExpressionEncodingCache</c> enabled.
    /// </para>
    /// <para>
    /// See the parameter documentation on
    /// <see cref="CompileInteractiveEnvironment"/> for the meaning of each
    /// knob — they are forwarded verbatim.
    /// </para>
    /// </summary>
    public static Result<string, CompilationPipelineStageResults<DefaultLoweredResults>> LowerToElmSyntaxForCompilation(
        FileTree appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        ElmSyntaxOptimizationConfig? syntaxOptimization = null)
    {
        syntaxOptimization ??= new ElmSyntaxOptimizationConfig.SyntaxOptimizationEnabled();
        // Capture per-round optimization snapshots and the post-lambda-lift module list
        // from inside the lowering delegate so the back-compat overload can promote
        // them onto the final result (the generic overload itself leaves
        // OptimizationIterations null and reconstructs ModulesForCompilation from the
        // canonicalized module shells).
        ImmutableList<OptimizationIterationStageResults>? capturedIterations = null;
        IReadOnlyList<SyntaxTypes.File>? capturedLambdaLifted = null;

        var genericResult =
            LowerToElmSyntaxForCompilation<DefaultLoweredResults>(
                appCodeTree,
                rootFilePaths,
                lower: (flatCanonicalized, rootDeclarationNames) =>
                {
                    var rootModuleNames =
                        rootDeclarationNames
                        .Select(name => string.Join('.', name.Namespaces))
                        .ToHashSet();

                    var canonicalizedModulesForLowering =
                        BuildModuleShellsFromFlatDeclarations(flatCanonicalized);

                    var standardResult =
                        RunStandardLoweringPipeline(
                            canonicalizedModulesForLowering,
                            rootModuleNames,
                            syntaxOptimization);

                    if (standardResult.IsErrOrNull() is { } stdErr)
                        return stdErr;

                    if (standardResult.IsOkOrNullable() is not { } stdOkWrapper)
                    {
                        throw new NotImplementedException(
                            "Unexpected result type: " + standardResult.GetType().Name);
                    }

                    var (lowered, iterations) = stdOkWrapper;

                    capturedIterations = iterations;
                    capturedLambdaLifted = lowered.LambdaLifted;

                    return lowered;
                },
                extractFilteredDeclarations: lowered => lowered.FilteredDeclarations);

        if (genericResult.IsErrOrNull() is { } genericErr)
            return genericErr;

        if (genericResult.IsOkOrNull() is not { } genericOk)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + genericResult.GetType().Name);
        }

        // Use the lambda-lifted module list as the reconstruction shell to preserve
        // historical behaviour: function declarations that are not reachable from the
        // roots (and so were dropped from FilteredDeclarations) are kept as their
        // post-lift form rather than reverting to the canonicalized pre-lift form.
        // The generic overload defaults to using the canonicalized module list as the
        // shell, so we replace ModulesForCompilation here.
        var modulesForCompilation =
            capturedLambdaLifted is null
            ?
            genericOk.ModulesForCompilation
            :
            ReconstructModulesFromFlatDict(
                genericOk.Lowered.FilteredDeclarations,
                capturedLambdaLifted);

        return
            genericOk with
            {
                ModulesForCompilation = modulesForCompilation,
                OptimizationIterations = capturedIterations,
            };
    }

    /// <summary>
    /// Generic lowering overload. Performs the parse + canonicalization stages, then
    /// hands control to the caller-supplied <paramref name="lower"/> delegate, which
    /// receives the flat canonicalized declaration dictionary together with the set
    /// of root-declaration qualified names (so it can decide which specializations
    /// to emit for the applications inside the root declarations). The returned
    /// <typeparamref name="LoweredT"/> is then projected back to a flat declaration
    /// dictionary via <paramref name="extractFilteredDeclarations"/>; the resulting
    /// declarations are reconstructed into the per-module shape expected by the
    /// compilation backend.
    /// </summary>
    /// <typeparam name="LoweredT">
    /// Caller-defined lowering payload returned by <paramref name="lower"/>.
    /// </typeparam>
    /// <param name="appCodeTree">
    /// Source tree containing the application's Elm files. The bundled elm-core
    /// kernel modules are merged in automatically.
    /// </param>
    /// <param name="rootFilePaths">
    /// Entry-point file paths used as roots of the compilation closure. Only modules
    /// transitively reachable from these are compiled.
    /// </param>
    /// <param name="lower">
    /// Delegate that lowers the canonicalized declaration dictionary into a
    /// caller-defined <typeparamref name="LoweredT"/>. Receives the set of
    /// fully-qualified names of declarations that belong to the root modules, so
    /// the lowering can decide which specializations need to be created for the
    /// applications appearing in the root declarations.
    /// </param>
    /// <param name="extractFilteredDeclarations">
    /// Delegate projecting <typeparamref name="LoweredT"/> to the flat declaration
    /// dictionary that should be reconstructed into the per-module shape handed to
    /// the compilation backend.
    /// </param>
    public static Result<string, CompilationPipelineStageResults<LoweredT>> LowerToElmSyntaxForCompilation<LoweredT>(
        FileTree appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        Func<
            ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>,
            IReadOnlySet<DeclQualifiedName>,
            Result<string, LoweredT>> lower,
        Func<LoweredT, ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>> extractFilteredDeclarations)
    {
        var canonicalizationResult = ParseAndCanonicalizeForLowering(appCodeTree, rootFilePaths);

        if (canonicalizationResult.IsErrOrNull() is { } canonErr)
            return canonErr;

        if (canonicalizationResult.IsOkOrNullable() is not { } canonicalizationOk)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + canonicalizationResult.GetType().Name);
        }

        var (canonicalizedModules, rootModuleNames) = canonicalizationOk;

        var flatCanonicalized = FlattenModulesToDeclarationDictionary(canonicalizedModules);

        IReadOnlySet<DeclQualifiedName> rootDeclarationNames =
            flatCanonicalized.Keys
            .Where(key => rootModuleNames.Contains(string.Join('.', key.Namespaces)))
            .ToHashSet();

        var lowerResult = lower(flatCanonicalized, rootDeclarationNames);

        if (lowerResult.IsErrOrNull() is { } lowerErr)
        {
            return "Failed lowering: " + lowerErr;
        }

        if (lowerResult is not Result<string, LoweredT>.Ok loweredOk)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + lowerResult.GetType().Name);
        }

        var loweredValue = loweredOk.Value;

        var filteredDeclarations = extractFilteredDeclarations(loweredValue);

        var modulesForCompilation =
            ReconstructModulesFromFlatDict(
                filteredDeclarations,
                canonicalizedModules);

        return
            new CompilationPipelineStageResults<LoweredT>(
                Canonicalized: canonicalizedModules,
                Lowered: loweredValue,
                ModulesForCompilation: modulesForCompilation,
                OptimizationIterations: null);
    }

    /// <summary>
    /// Parses the Elm source files reachable from <paramref name="rootFilePaths"/>,
    /// computes the transitive dependency closure and canonicalizes the result.
    /// Shared between the back-compat and generic overloads of
    /// <see cref="LowerToElmSyntaxForCompilation"/>.
    /// </summary>
    private static Result<string, (List<SyntaxTypes.File> CanonicalizedModules, HashSet<string> RootModuleNames)>
        ParseAndCanonicalizeForLowering(
        FileTree appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths)
    {
        // Centralize the hardcoded elm/core kernel module addition here so consumers of
        // ElmCompilerInDotnet pass only app/package sources and do not duplicate this merge.
        var appCodeTreeWithKernelModules =
            FileTree.MergeFiles(
                left: appCodeTree,
                right: ElmInElm.BundledFiles.ElmKernelModulesDefault.Value);

        var elmModuleFiles =
            appCodeTreeWithKernelModules.EnumerateFilesTransitive()
            .Where(file => file.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .ToImmutableArray();

        // Step 1: Parse all modules, building a map from file path to module name.
        var successfullyParsedModules = new Dictionary<string, SyntaxTypes.File>();

        var parseFailures = new Dictionary<string, string>();

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

                parseFailures.Add(moduleNameFlattened, ElmSyntax.ElmSyntaxParseError.RenderDisplayString(firstIncompleteDeclaration.Value.ParseError));
                continue;
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
            .Select(
                path =>
                filePathToModuleName.TryGetValue(path, out var name) ? name : null)
            .Where(name => name is not null)
            .ToHashSet();

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

                        if (parseFailures.TryGetValue(importedName, out var parseErr))
                        {
                            // A module in the dep graph depends on a module that failed to parse.

                            return
                                "Module '" + importedName +
                                "' is required by '" + moduleName +
                                "' but failed to parse: " + parseErr;
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

        return (canonicalizedModules, rootModuleNames);
    }

    /// <summary>
    /// Standard lowering pipeline used by the back-compat overload of
    /// <see cref="LowerToElmSyntaxForCompilation"/>. Runs the canonical
    /// lambda-lift + specialize + inline + (optional) operator-lowering pipeline on
    /// <paramref name="canonicalizedModules"/> and returns a
    /// <see cref="DefaultLoweredResults"/> capturing the post-lambda-lift module
    /// list, the post-specialization snapshot, and the reachability-filtered
    /// declaration dictionary. The optional per-round optimization snapshots are
    /// returned alongside so the caller can surface them through
    /// <see cref="CompilationPipelineStageResults{LoweredT}.OptimizationIterations"/>.
    /// </summary>
    private static Result<string, (DefaultLoweredResults Lowered, ImmutableList<OptimizationIterationStageResults>? Iterations)>
        RunStandardLoweringPipeline(
        List<SyntaxTypes.File> canonicalizedModules,
        IReadOnlySet<string> rootModuleNames,
        ElmSyntaxOptimizationConfig syntaxOptimization)
    {
        var optimizationEnabled =
            syntaxOptimization as ElmSyntaxOptimizationConfig.SyntaxOptimizationEnabled;

        // Lambda lifting stage: Transform closures into top-level functions.
        // The experimental knob `RunSpecializationBeforeLambdaLifting` interposes a single
        // combined specialization+inlining round on the canonicalized (pre-lifting) flat
        // declaration dictionary; the inner pass internally re-runs LambdaLifting as one of
        // its post-passes, so its output is already in the same lambda-lifted shape that
        // the standard `Select(LiftLambdas)` would have produced — but with anonymous-lambda
        // arguments to higher-order callees already substituted into `__specialized__N`
        // siblings before the lift packs their captured environment into a tuple parameter.
        List<SyntaxTypes.File> lambdaLiftedModules;

        if (optimizationEnabled is { RunSpecializationBeforeLambdaLifting: true })
        {
            var preLiftingFlat = FlattenModulesToDeclarationDictionary(canonicalizedModules);

            var preLiftingDecls = OptimizedElmSyntaxDeclarations.FromFlatDictionary(preLiftingFlat);

            // Convergence-bounded loop: each iteration runs a combined
            // specialization+inlining round (which internally re-runs LambdaLifting
            // as its post-pass, so the output is in lambda-lifted form), with
            // early exit on declaration-dictionary equality.
            for (var preRound = 0; preRound < optimizationEnabled.MaxPreLiftingSpecializationRounds; preRound++)
            {
                var declsBeforePreRound = preLiftingDecls;

                var preLiftingSpecResult =
                    ElmSyntaxOptimization.SpecializeAndInlineDeclarations(
                        preLiftingDecls,
                        ElmSyntaxOptimization.Config.OnlyFunctions,
                        ElmSyntaxOptimization.RewriteConfig.Combined);

                if (preLiftingSpecResult.IsErrOrNull() is { } preLiftErr)
                    return "Pre-lifting specialization (round " + preRound + ") failed: " + preLiftErr;

                if (preLiftingSpecResult.IsOkOrNull() is not { } preLiftingSpecDecls)
                    throw new NotImplementedException("Unexpected result type");

                preLiftingDecls = preLiftingSpecDecls;

                if (preLiftingDecls.Equals(declsBeforePreRound))
                    break;
            }

            // The pre-lifting rounds' internal LiftLambdas post-pass already produced lifted
            // output, so we can hand the reconstructed modules straight to the canonical
            // optimization pipeline without re-running `LiftLambdas` per module.
            lambdaLiftedModules =
                ReconstructModulesFromFlatDict(
                    preLiftingDecls.RenderAsFlatDictionary(),
                    canonicalizedModules);
        }
        else
        {
            lambdaLiftedModules =
                [.. canonicalizedModules.Select(LambdaLifting.LiftLambdas)];
        }

        if (ValidateStage(lambdaLiftedModules, "Lambda lifting (initial)") is { } lambdaLiftErr)
            return lambdaLiftErr;

        OptimizationPipelineStageResults? optimizationResults = null;

        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> filteredDeclarations;

        if (optimizationEnabled is null)
        {
            // No optimization pipeline: the filtered declaration set is simply the flat
            // representation of the lambda-lifted modules (no reachability filtering is
            // required because the back-compat path historically used the
            // lambda-lifted module list verbatim as the compilation input in this case).
            filteredDeclarations =
                FlattenModulesToDeclarationDictionary(lambdaLiftedModules);
        }
        else
        {
            var pipelineResult =
                ApplyOptimizationPipelineWithStageResults(
                    lambdaLiftedModules,
                    optimizationRounds: optimizationEnabled.MaxOptimizationRounds,
                    sizeBasedInliningConfigOverride: optimizationEnabled.SizeBasedInliningConfigOverride,
                    inliningRounds: optimizationEnabled.MaxSizeBasedInliningRounds);

            if (pipelineResult.IsErrOrNull() is { } pipelineErr)
                return pipelineErr;

            optimizationResults =
                pipelineResult.Extract(err => throw new NotImplementedException());

            // Optional final cleanup pass implementing §F.4 gap (1) — see
            // explore/internal-analysis/2026-05-19-optimize-elm-syntax-to-monomorphize-and-eliminate-higher-order-parameters.md
            // §H. Runs after the main pipeline so the established
            // optimization order is unchanged for callers that do not opt
            // in. Iterates LetDestructureThunkInlining + a literal-only
            // WrapUnwrapCancellation pass to a fixed point, capped at a
            // small bound to bound worst-case wall-clock cost. Each round
            // also re-runs the combined specialize+inline pipeline so any
            // newly-exposed `f (knownA, knownB, knownC)` tuple-argument
            // call site picks up its `TupleUnwrap` specialization
            // (§F.4 gap (4)) — without the second specialization pass the
            // catalog never sees the post-inlining shape.
            if (optimizationEnabled.InlineLetDestructureThunks)
            {
                var afterLowering = optimizationResults.AfterLowering;

                for (var cleanupRound = 0; cleanupRound < OptimizationRoundsDefault; cleanupRound++)
                {
                    var before = afterLowering;

                    var inlinedFlat =
                        LetDestructureThunkInlining.RewriteDeclarationDictionary(
                            afterLowering.RenderAsFlatDictionary());

                    var cancelledFlat =
                        WrapUnwrapCancellation.RewriteDeclarationDictionary(inlinedFlat);

                    afterLowering = OptimizedElmSyntaxDeclarations.FromFlatDictionary(cancelledFlat);

                    // Re-run the combined specialize+inline pipeline so a
                    // TuplePattern callee whose argument now resolves to a
                    // tuple of concrete function references (e.g. after
                    // thunk-inlining + WUC peels through the wrapper layers)
                    // gets a __specialized__N variant emitted by the
                    // catalog. Without this round, the discovery walker
                    // never sees the post-cleanup shape.
                    //
                    // Disable the WrapperReturnStripping post-pass here:
                    // the input dictionary already carries `__stripped`
                    // siblings emitted during the standard pipeline. Their
                    // forwarding originals have since been further
                    // simplified by other passes (e.g. another round of
                    // inlining), so re-running WRS would discover a
                    // structural divergence between the existing sibling
                    // body and the body a fresh plan would emit, and abort
                    // with the "previous pass appears to have emitted a
                    // different sibling under the same name" guard. The
                    // existing siblings remain correct (they reflect the
                    // semantics at the point they were emitted) and the
                    // catalog round here only needs specialization +
                    // inlining + lambda lifting + application
                    // normalization, not another WRS round. See §H.6 of
                    // 2026-05-19-optimize-elm-syntax-to-monomorphize-and-
                    // eliminate-higher-order-parameters.md.
                    var specResult =
                        ElmSyntaxOptimization.SpecializeAndInlineDeclarations(
                            afterLowering,
                            ElmSyntaxOptimization.Config.OnlyFunctions,
                            ElmSyntaxOptimization.RewriteConfig.Combined,
                            ElmSyntaxOptimization.StageToggles.Default with
                            {
                                WrapperReturnStrippingEnabled = false,
                            });

                    if (specResult.IsErrOrNull() is { } specErr)
                        return "Post-thunk-inlining specialization (round " + cleanupRound + ") failed: " + specErr;

                    if (specResult.IsOkOrNull() is { } specDecls)
                        afterLowering = specDecls;

                    if (afterLowering.Equals(before))
                        break;
                }

                optimizationResults = optimizationResults with { AfterLowering = afterLowering };
            }

            var declarationsAfterLowering =
                optimizationResults.AfterLowering.RenderAsFlatDictionary();


            bool IncludeDeclarationAsRoot(DeclQualifiedName declQualifiedName)
            {
                // Temp addition to account for some platform interfaces depending on additional function declarations.
                if (TempIncludedRootDeclarations.Contains(declQualifiedName))
                    return true;

                return rootModuleNames.Contains(string.Join('.', declQualifiedName.Namespaces));
            }

            var rootDeclarations =
                declarationsAfterLowering
                .Where(kv => IncludeDeclarationAsRoot(kv.Key))
                .ToImmutableDictionary();

            var reachableFromEntryPoints =
                OptimizationOpportunityFinder.ComputeReachableDeclarations(
                    declarationsAfterLowering,
                    [.. rootDeclarations.Keys]);

            filteredDeclarations =
                declarationsAfterLowering
                .Where(kv => reachableFromEntryPoints.Contains(kv.Key))
                .ToImmutableDictionary(kv => kv.Key, kv => kv.Value);
        }

        return
            (new DefaultLoweredResults(
                LambdaLifted: lambdaLiftedModules,
                Specialized: optimizationResults?.AfterSpecialization,
                FilteredDeclarations: filteredDeclarations),
            optimizationResults?.Iterations);
    }

    internal static readonly ImmutableHashSet<DeclQualifiedName> TempIncludedRootDeclarations =
        ImmutableHashSet<DeclQualifiedName>.Empty
        .Add(DeclQualifiedName.Create(["Backend", "MigrateState"], "migrate"))
        .Add(DeclQualifiedName.Create(["Json", "Encode"], "encode"))
        .Add(DeclQualifiedName.Create(["Json", "Decode"], "value"))
        .Add(DeclQualifiedName.Create(["Json", "Decode"], "decodeValue"))
        .Add(DeclQualifiedName.Create(["Json", "Decode"], "decodeString"));

    public static Result<string, (PineValue compiledEnvValue, CompilationPipelineStageResults<DefaultLoweredResults> pipelineStageResults)> CompileInteractiveEnvironment(
        FileTree appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        ElmSyntaxOptimizationConfig? syntaxOptimization = null,
        bool disableGenericApplicationChainConsolidation = false,
        IReadOnlyList<DeclQualifiedName>? rootDeclarationsAsPlainValues = null)
    {
        syntaxOptimization ??= SyntaxOptimizationConfigDefault;

        var loweringResult =
            LowerToElmSyntaxForCompilation(
                appCodeTree,
                rootFilePaths,
                syntaxOptimization);

        if (loweringResult.IsErrOrNull() is { } loweringErr)
            return loweringErr;

        if (loweringResult.IsOkOrNull() is not { } pipelineStageResults)
            throw new NotImplementedException("Unexpected result type: " + loweringResult.GetType().Name);

        return
            EmitCompiledEnvironmentFromPipelineResults(
                pipelineStageResults,
                disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation,
                rootDeclarationsAsPlainValues: rootDeclarationsAsPlainValues);
    }

    /// <summary>
    /// Generic counterpart to <see cref="CompileInteractiveEnvironment"/>. Performs the
    /// parse + canonicalization stages, hands control to the caller-supplied
    /// <paramref name="lower"/> delegate (which receives the flat canonicalized
    /// declaration dictionary together with the set of root-declaration qualified
    /// names so it can decide which specializations to emit for the applications
    /// inside the root declarations), then emits the compiled <see cref="PineValue"/>
    /// environment from the resulting modules.
    /// </summary>
    public static Result<string, (PineValue compiledEnvValue, CompilationPipelineStageResults<LoweredT> pipelineStageResults)>
        CompileInteractiveEnvironment<LoweredT>(
        FileTree appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        Func<
                ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>,
                IReadOnlySet<DeclQualifiedName>,
                Result<string, LoweredT>> lower,
        Func<LoweredT, ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>> extractFilteredDeclarations,
        bool disableGenericApplicationChainConsolidation = false,
        IReadOnlyList<DeclQualifiedName>? rootDeclarationsAsPlainValues = null)
    {
        var loweringResult =
            LowerToElmSyntaxForCompilation(
                appCodeTree,
                rootFilePaths,
                lower: lower,
                extractFilteredDeclarations: extractFilteredDeclarations);

        if (loweringResult.IsErrOrNull() is { } loweringErr)
            return loweringErr;

        if (loweringResult.IsOkOrNull() is not { } pipelineStageResults)
            throw new NotImplementedException("Unexpected result type: " + loweringResult.GetType().Name);

        return
            EmitCompiledEnvironmentFromPipelineResults(
                pipelineStageResults,
                disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation,
                rootDeclarationsAsPlainValues: rootDeclarationsAsPlainValues);
    }

    /// <summary>
    /// Shared emission helper: takes the post-lowering
    /// <see cref="CompilationPipelineStageResults{LoweredT}"/> and produces the compiled
    /// <see cref="PineValue"/> interactive environment. Used by both the back-compat
    /// and generic <c>CompileInteractiveEnvironment</c> overloads so the
    /// emission/encoding logic exists in exactly one place.
    /// </summary>
    private static Result<string, (PineValue compiledEnvValue, CompilationPipelineStageResults<LoweredT> pipelineStageResults)>
        EmitCompiledEnvironmentFromPipelineResults<LoweredT>(
        CompilationPipelineStageResults<LoweredT> pipelineStageResults,
        bool disableGenericApplicationChainConsolidation,
        IReadOnlyList<DeclQualifiedName>? rootDeclarationsAsPlainValues)
    {
        var modulesForCompilation = pipelineStageResults.ModulesForCompilation;


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

        var abstractAllFunctions =
            allFunctions.ToDictionary(
                kvp => kvp.Key,
                kvp =>
                (kvp.Value.moduleName,
                kvp.Value.functionName,
                (AbstractSyntaxTypes.Declaration.FunctionDeclaration)ElmSyntaxAbstractConversion.FromDeclaration(kvp.Value.declaration)));

        // Build function type metadata dictionary for type inference
        var functionTypes = new Dictionary<SyntaxModelTypes.QualifiedNameRef, FunctionTypeInfo>();

        foreach (var (qualifiedName, (_, _, declaration)) in abstractAllFunctions)
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
                        var argType =
                            TypeInference.TypeAnnotationToInferredType(
                                ElmSyntaxAbstractConversion.FromTypeAnnotation(argNode.Value));

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
                abstractAllFunctions,
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

        var reducedExpressionCache =
            new Dictionary<(Expression, ReductionConfig), Expression>();

        var expressionEncodingCache = new PineExpressionEncodingCache();

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

            var compileSccResult =
                CompileSCC(
                    scc,
                    compilationContext,
                    reducedExpressionCache,
                    expressionEncodingCache,
                    disableGenericApplicationChainConsolidation);

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

        // Zero-parameter declarations represent Elm values, so the module value embeds
        // their evaluated results directly instead of a function-record wrapper.
        // This lets consumers read the declaration value without first evaluating it.
        var declarationsAsPlainValues =
            compilationContext.CompiledFunctionsCache
            .Where(entry => entry.Value.ParameterCount is 0)
            .Select(entry => entry.Key)
            .ToHashSet();

        if (rootDeclarationsAsPlainValues is { Count: > 0 } rootDeclsAsPlainValues)
        {
            foreach (var rootDecl in rootDeclsAsPlainValues)
            {
                declarationsAsPlainValues.Add(
                    QualifiedNameHelper.FromQualifiedNameString(rootDecl.FullName));
            }
        }

        if (declarationsAsPlainValues.Count > 0)
        {
            var plainValueParseCache = new PineVMParseCache();

            var plainValueInterpreter =
                new Interpreter.DirectInterpreter(plainValueParseCache, evalCache: null);

            foreach (var qualifiedName in declarationsAsPlainValues)
            {
                var qualifiedNameStr = qualifiedName.ToString();

                var compiledInfo = compilationContext.TryGetCompiledFunctionInfo(qualifiedName);

                if (compiledInfo is null)
                {
                    return
                        "Root declaration '" + qualifiedNameStr +
                        "' requested as plain value was not compiled.";
                }

                if (compiledInfo.ParameterCount is not 0)
                {
                    return
                        "Root declaration '" + qualifiedNameStr +
                        "' has " + compiledInfo.ParameterCount +
                        " parameters; only zero-parameter declarations can be emitted as plain values.";
                }

                var wrapperValue = compiledInfo.CompiledValue;

                var parseWrapperResult = plainValueParseCache.ParseExpression(wrapperValue);

                if (parseWrapperResult.IsErrOrNull() is { } parseWrapperErr)
                {
                    return
                        "Failed parsing wrapper expression for root declaration '" +
                        qualifiedNameStr + "': " + parseWrapperErr;
                }

                if (parseWrapperResult.IsOkOrNull() is not { } wrapperExpression)
                {
                    throw new NotImplementedException(
                        "Unexpected result type: " + parseWrapperResult.GetType());
                }

                var evalResult =
                    plainValueInterpreter.EvaluateExpression(
                        wrapperExpression,
                        PineValue.EmptyList);

                if (evalResult.IsErrOrNull() is { } evalErr)
                {
                    return
                        "Failed evaluating root declaration '" + qualifiedNameStr +
                        "' as plain value: " + evalErr;
                }

                if (evalResult.IsOkOrNull() is not { } plainValue)
                {
                    throw new NotImplementedException(
                        "Unexpected result type: " + evalResult.GetType());
                }

                compilationContext =
                    compilationContext.WithCompiledFunction(
                        qualifiedNameStr,
                        plainValue,
                        compiledInfo.EncodedBody,
                        compiledInfo.DependencyLayout,
                        parameterCount: compiledInfo.ParameterCount,
                        envFunctions: compiledInfo.EnvFunctions);
            }
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
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache,
        PineExpressionEncodingCache expressionEncodingCache,
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
            var functionBody = declaration.Function.Declaration.Expression;
            var arguments = declaration.Function.Declaration.Arguments;
            var paramCount = arguments.Count;

            // Build parameter mappings
            var parameterNames = new Dictionary<string, int>();

            var localBindings = new Dictionary<string, Expression>();

            // Walk the function's signature (if any) to assemble each
            // parameter's inferred type by index, so record-pattern
            // parameters can be compiled with the correct field-name
            // layout for name-based field access.
            var parameterTypesByIndex = new TypeInference.InferredType?[arguments.Count];

            {
                if (declaration.Function.Signature is { } sig)
                {
                    var currentType = sig.TypeAnnotation;

                    for (var pi = 0; pi < arguments.Count
                        && currentType is AbstractSyntaxTypes.TypeAnnotation.FunctionTypeAnnotation funcType; pi++)
                    {
                        parameterTypesByIndex[pi] =
                            TypeInference.TypeAnnotationToInferredType(funcType.ArgumentType);

                        currentType = funcType.ReturnType;
                    }
                }
            }

            for (var i = 0; i < arguments.Count; i++)
            {
                var argPattern = arguments[i];

                if (argPattern is AbstractSyntaxTypes.Pattern.VarPattern varPattern)
                {
                    parameterNames[varPattern.Name] = i;
                }
                else
                {
                    var paramExpr = BuiltinHelpers.BuildPathToParameter(i);

                    var analysis =
                        PatternCompiler.AnalyzePattern(
                            argPattern,
                            paramExpr,
                            scrutineeType: parameterTypesByIndex[i],
                            recordTypeAliasFields: context.RecordTypeAliasConstructors,
                            choiceTagArgumentTypes: context.ChoiceTagArgumentTypes);

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
                    CurrentFunctionName: declaration.Function.Declaration.Name,
                    LocalBindings: localBindings.Count > 0 ? localBindings : null,
                    LocalBindingTypes: null,
                    DependencyLayout: sharedLayout,
                    ModuleCompilationContext: context,
                    ParseCache: parseCache,
                    ReducedExpressionCache: reducedExpressionCache,
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
                    ReductionConfig.Default
                    with
                    {
                        DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation
                    },
                    parseCache,
                    reducedExpressionCache: reducedExpressionCache);

            compiledBodies[memberName] = (compiledBody, paramCount);
        }

        var encodedBodies =
            compiledBodies
            .ToDictionary(
                kvp => kvp.Key,
                kvp => ExpressionEncoding.EncodeExpressionAsValue(kvp.Value.body, expressionEncodingCache));

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
                    envFunctionsList,
                    encodeExprCache: expressionEncodingCache);

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
                    return;

                case SyntaxTypes.Expression.IfBlock ifBlock:
                    AnalyzeExpression(ifBlock.Condition.Value);
                    AnalyzeExpression(ifBlock.ThenBlock.Value);
                    AnalyzeExpression(ifBlock.ElseBlock.Value);
                    return;

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
                    return;

                case SyntaxTypes.Expression.ListExpr listExpr:
                    foreach (var elem in listExpr.Elements)
                    {
                        AnalyzeExpression(elem.Value);
                    }

                    return;

                case SyntaxTypes.Expression.ParenthesizedExpression parenthesized:
                    AnalyzeExpression(parenthesized.Expression.Value);
                    return;

                case SyntaxTypes.Expression.Negation negation:
                    AnalyzeExpression(negation.Expression.Value);
                    return;

                case SyntaxTypes.Expression.CaseExpression caseExpr:
                    AnalyzeExpression(caseExpr.CaseBlock.Expression.Value);

                    foreach (var caseItem in caseExpr.CaseBlock.Cases)
                    {
                        AnalyzeExpression(caseItem.Expression.Value);
                    }

                    return;

                case SyntaxTypes.Expression.TupledExpression tupledExpr:
                    foreach (var elem in tupledExpr.Elements)
                    {
                        AnalyzeExpression(elem.Value);
                    }

                    return;

                case SyntaxTypes.Expression.RecordExpr recordExpr:
                    foreach (var field in recordExpr.Fields)
                    {
                        AnalyzeExpression(field.Value.valueExpr.Value);
                    }

                    return;

                case SyntaxTypes.Expression.RecordUpdateExpression recordUpdateExpr:
                    foreach (var setter in recordUpdateExpr.Fields)
                    {
                        AnalyzeExpression(setter.Value.valueExpr.Value);
                    }

                    return;

                case SyntaxTypes.Expression.RecordAccess recordAccess:
                    AnalyzeExpression(recordAccess.Record.Value);
                    return;

                case SyntaxTypes.Expression.LambdaExpression lambdaExpr:
                    AnalyzeExpression(lambdaExpr.Lambda.Expression.Value);
                    return;

                case SyntaxTypes.Expression.FunctionOrValue funcOrValue:

                    if (funcOrValue.Name.Length < 0)
                    {
                        throw new InvalidOperationException(
                            "Function or value name cannot be empty.");
                    }

                    if (char.IsUpper(funcOrValue.Name[0]))
                    {
                        // This must be a choice type tag constructor or record constructor - cannot contribute to dependencies
                        return;
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
                                return;
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

                    return;

                case SyntaxTypes.Expression.PrefixOperator:
                case SyntaxTypes.Expression.UnitExpr:
                case SyntaxTypes.Expression.Literal:
                case SyntaxTypes.Expression.Integer:
                case SyntaxTypes.Expression.CharLiteral:
                case SyntaxTypes.Expression.Hex:
                case SyntaxTypes.Expression.Floatable:
                case SyntaxTypes.Expression.RecordAccessFunction:
                case SyntaxTypes.Expression.GLSLExpression:
                    return;

                default:
                    throw new NotImplementedException(
                        $"Unhandled expression type: {expr.GetType().Name}");
            }
        }

        AnalyzeExpression(expression);

        return dependencies;
    }

    private static ImmutableDictionary<string, TypeInference.InferredType> ExtractParameterTypes(
        AbstractSyntaxTypes.FunctionStruct function,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>? constructorArgumentTypes,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, FunctionTypeInfo>? functionTypes,
        string currentModuleName)
    {
        var parameterTypes = ImmutableDictionary<string, TypeInference.InferredType>.Empty;

        if (function.Signature is { } signature)
        {
            var typeAnnotation = signature.TypeAnnotation;
            var parameters = function.Declaration.Arguments;

            // Walk through the function type annotation to extract parameter types
            var currentType = typeAnnotation;

            var paramIndex = 0;

            while (currentType is AbstractSyntaxTypes.TypeAnnotation.FunctionTypeAnnotation funcType &&
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
        int optimizationRounds,
        ElmSyntaxOptimization.SmallFunctionsConfig? sizeBasedInliningConfigOverride = null,
        int inliningRounds = OptimizationRoundsDefault)
    {
        var sizeBasedInliningConfig =
            sizeBasedInliningConfigOverride is null
            ?
            ElmSyntaxOptimization.Config.SmallFunctionsAndPlainValues
            :
            ElmSyntaxOptimization.Config.SmallFunctionsAndPlainValues with
            {
                SmallFunctions = sizeBasedInliningConfigOverride
            };

        // Flatten the input modules into a declaration dictionary and lift into the
        // optimization-stage record. The pipeline operates on this structured representation
        // throughout — each downstream flat-dict stage is invoked by rendering through
        // `RenderAsFlatDictionary` and re-lifting the result via `FromFlatDictionary`.
        var initialFlat = FlattenModulesToDeclarationDictionary(lambdaLiftedModules);
        var currentDecls = OptimizedElmSyntaxDeclarations.FromFlatDictionary(initialFlat);

        var afterSpecialization = currentDecls;

        var iterationResults = ImmutableList.CreateBuilder<OptimizationIterationStageResults>();

        for (var round = 0; round < optimizationRounds; round++)
        {
            var declsBefore = currentDecls;

            // Phase 1: Specialization — create specialized versions of functions for known argument patterns.
            // Lambda lifting is combined: the transformation stage lifts any lambdas/local functions it introduces.
            {
                var result =
                    ElmSyntaxOptimization.SpecializeAndInlineDeclarations(
                        currentDecls,
                        ElmSyntaxOptimization.Config.OnlyFunctions,
                        rewriteConfig: ElmSyntaxOptimization.RewriteConfig.Combined);

                if (result.IsErrOrNull() is { } specErr)
                    return $"Specialization (round {round}) failed: " + specErr;

                if (result.IsOkOrNull() is not { } specDict)
                    throw new NotImplementedException("Unexpected result type");

                currentDecls = specDict;
            }

            afterSpecialization = currentDecls;

            {
                if (ValidateStage(currentDecls, $"Specialization (round {round})") is { } specErr)
                    return specErr;
            }

            // Record this iteration's intermediate results.
            // Size-based inlining is intentionally NOT a per-round sub-stage anymore: it runs
            // exactly once after the convergence loop terminates (see Phase 3 below).
            iterationResults.Add(
                new OptimizationIterationStageResults(
                    Round: round,
                    AfterSpecialization: afterSpecialization));

            // Convergence check: if the loop body produced no change, no further rounds are needed.
            if (currentDecls.Equals(declsBefore))
            {
                break;
            }
        }

        // Phase 2b: Final combined specialization-and-inlining pass.
        // Originally introduced to compensate for Phase 2 running in RewriteConfig.InliningOnly
        // (which gated specialization off). Phase 2 now runs in RewriteConfig.Combined via
        // Inlining.SpecializeAndInlineDeclarationsCombined, so this pass is no longer load-bearing for closing the
        // §5.2 gap in the per-round case. It is, however, still required to guarantee that
        // specialization runs at least once when the convergence loop body executes zero
        // times (i.e. when maxRounds is 0): without it, the 0-rounds bundle differs in
        // observable behaviour from the 1-round bundle, which the
        // References_request_finds_usage_across_modules_optimization_pipeline_iterations
        // semantic-preservation test detects as a regression.
        // See explore/internal-analysis/2026-05-16-skipWhileWithoutLinebreakHelp-alpha-regression.md
        // §5.4 for the rationale of Phase 2 using Combined.
        {
            var result =
                ElmSyntaxOptimization.OptimizeRounds(
                    currentDecls,
                    ElmSyntaxOptimization.Config.OnlyFunctions,
                    rounds: 1);

            if (result.IsErrOrNull() is { } combinedErr)
                return "Combined specialization+inlining (final) failed: " + combinedErr;

            if (result.IsOkOrNull() is not { } combinedDict)
                throw new NotImplementedException("Unexpected result type");

            currentDecls = combinedDict;
        }

        {
            if (ValidateStage(currentDecls, "Combined specialization+inlining (final)") is { } combinedErr2)
                return combinedErr2;
        }

        // Phase 2c: Case-block consolidation.
        // Per the design in
        // explore/internal-analysis/2026-05-20-elm-syntax-case-block-consolidation.md
        // §"Where in the optimization pipeline does it make sense to insert this transform?",
        // this rewrite runs between Phase 2b (combined specialization+inlining)
        // and Phase 3 (size-based inlining): by this point all
        // higher-order parameter elimination and specialization have
        // produced their final declaration set, but no information has
        // yet been destroyed by size-based inlining.
        //
        // Bounded fixpoint: the pass is idempotent on a single
        // expression, but applying it once can expose cascading
        // consolidations across declarations (e.g. one consolidation
        // surfaces a new case-of-case shape elsewhere). Convergence is
        // detected via declaration-dictionary equality.
        for (var consolidationRound = 0; consolidationRound < optimizationRounds; consolidationRound++)
        {
            var declsBeforeRound = currentDecls;

            currentDecls =
                CaseBlockConsolidation.RewriteDeclarationDictionary(
                    currentDecls,
                    CaseBlockConsolidation.CaseConsolidationConfig.Default);

            if (ValidateStage(currentDecls, $"Case-block consolidation (round {consolidationRound})") is { } consolErr)
                return consolErr;

            if (currentDecls.Equals(declsBeforeRound))
                break;
        }

        // Phase 2d: Locally cancellable let destructuring.
        // Per the design in
        // explore/internal-analysis/2026-05-20-elm-syntax-case-block-consolidation.md
        // §3 "Locally Cancellable Let Destructuring", this rewrite peels
        // the outermost constructor tag from `let <NamedPattern> = <ctor app>` shapes
        // exposed by the prior optimization passes. It runs after
        // Phase 2c so case-block consolidation has had a chance to
        // surface the matching tag-on-tag let bindings.
        //
        // Bounded fixpoint: each peel may expose another tag-on-tag
        // match in a nested position; the pass is naturally cascading
        // (RewriteExpression re-runs locally after a successful peel)
        // but we still re-run at the dictionary level to catch
        // cross-declaration cascades. Convergence is detected via
        // declaration-dictionary equality.
        for (var letCancelRound = 0; letCancelRound < optimizationRounds; letCancelRound++)
        {
            var declsBeforeRound = currentDecls;

            currentDecls =
                LetDestructuringCancellation.RewriteDeclarationDictionary(currentDecls);

            if (ValidateStage(currentDecls, $"Let-destructuring cancellation (round {letCancelRound})") is { } cancelErr)
                return cancelErr;

            if (currentDecls.Equals(declsBeforeRound))
                break;
        }

        // Phase 3: Size-based inlining and plain value inlining.
        // Per the design discussion in
        // explore/elm-compiler-specializing-function-declarations.md
        // ("Should size-based inlining be moved to the very end of lowering?"), this pass runs
        // AFTER the { specialization ; higher-order inlining } convergence loop has terminated.
        // Inside the loop these two passes interact in a well-understood way (specialization
        // creates concrete callees, higher-order inlining consumes them) and reach a fixpoint
        // without size-based inlining mediating between rounds.
        //
        // The trailing pass itself is run as its own bounded convergence loop: up to
        // <paramref name="maxSizeBasedInliningRounds"/> iterations, each applying the same
        // size-gated inlining configuration (the gating by AST node count is unchanged across
        // rounds). Convergence is detected via declaration-dictionary equality so the loop
        // exits early as soon as a round produces no further change.
        //
        // Inlines small wrapper functions (≤10 AST nodes by default, no complex expressions) and
        // plain zero-parameter declarations with simple bodies, exposing literal expression
        // structure to the downstream Pine-expression lowering stages.
        // Lambda lifting is combined: the transformation stage lifts any lambdas/local
        // functions it introduces.
        for (var trailingRound = 0; trailingRound < inliningRounds; trailingRound++)
        {
            var declsBeforeTrailingRound = currentDecls;

            {
                var result =
                    ElmSyntaxOptimization.SpecializeAndInlineDeclarationsCombined(
                        currentDecls,
                        sizeBasedInliningConfig);

                if (result.IsErrOrNull() is { } sizeInlineErr)
                    return $"Size-based inlining (trailing round {trailingRound}) failed: " + sizeInlineErr;

                if (result.IsOkOrNull() is not { } sizeInlineDict)
                    throw new NotImplementedException("Unexpected result type");

                currentDecls = sizeInlineDict;
            }

            {
                if (ValidateStage(currentDecls, $"Size-based inlining (trailing round {trailingRound})") is { } trailingSizeInlineErr)
                    return trailingSizeInlineErr;
            }

            // Convergence check: if the round body produced no change, no further rounds are needed.
            if (currentDecls.Equals(declsBeforeTrailingRound))
            {
                break;
            }
        }

        // Fix B from
        // explore/internal-analysis/2026-05-17-wrapper-then-intermediate-failing-test-analysis.md:
        // run one more WrapUnwrapCancellation pass after Phase 3 size-based inlining.
        // Phase 3 can inline a small wrapper decl whose body is `Ctor <fn>`
        // into a let-destructure RHS, producing the
        // `let (Ctor p) = let bindings in Ctor <fn> in p ...` shape that
        // is cancellable iff the matcher peels inner LetExpression
        // wrappers (Fix A) AND we re-run cancellation after Phase 3
        // (Fix B). Without this pass, the §3.3 residual
        // `higher-order-parameter: <decl> -> p` finding in the
        // `Wrapper_then_intermediate_around_recursive_higher_order_helper_…`
        // fixture survives.
        // <para>
        // Sibling-aware overload: <c>__stripped</c> siblings emitted by
        // Phase-1+2 <see cref="WrapperReturnStripping"/> persist into
        // the post-Phase-3 dictionary, so we reconstruct the sibling
        // registry from the current dictionary via
        // <see cref="WrapperReturnStripping.ReconstructSiblingRegistry"/>
        // and pass it in. This enables Shape A'/B' (sibling-aware
        // cancellation) for call sites that were exposed only after
        // Phase 3 inlining unwrapped a wrapper-form intermediate —
        // notably the
        // <c>let (Parser p) = f args in p more_args</c>
        // shape where <c>f</c> has a registered <c>f__stripped</c>.
        // The earlier comment that this had to be literal-only is
        // out of date now that the reconstruction helper exists.
        // </para>
        {
            var siblingRegistry =
                WrapperReturnStripping.ReconstructSiblingRegistry(currentDecls);

            currentDecls =
                ElmSyntaxOptimization.ApplyWrapUnwrapCancellation(
                    currentDecls,
                    ElmSyntaxOptimization.Config.WrapUnwrapCancellationOnly,
                    siblingRegistry);
        }

        // Phase 4: Operator lowering — convert operators to Pine built-in calls.
        // Runs once after the convergence loop since lowering does not expose new inlining opportunities.
        {
            var result = BuiltinOperatorLowering.Apply(currentDecls.RenderAsFlatDictionary());

            if (result.IsErrOrNull() is { } lowerErr)
                return "Operator lowering failed: " + lowerErr;

            if (result.IsOkOrNull() is not { } lowerDict)
                throw new NotImplementedException("Unexpected result type");

            currentDecls = OptimizedElmSyntaxDeclarations.FromFlatDictionary(lowerDict);
        }

        {
            if (CheckForNamingErrors(currentDecls, "Operator lowering") is { } lowerShadowErr)
                return lowerShadowErr;

            return
                new OptimizationPipelineStageResults(
                    AfterSpecialization: afterSpecialization,
                    AfterLowering: currentDecls,
                    Iterations: iterationResults.ToImmutable());
        }
    }

    /// <summary>
    /// <see cref="OptimizedElmSyntaxDeclarations"/>-flavoured overload of
    /// <see cref="CheckForNamingErrors(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration}, string)"/>.
    /// </summary>
    private static string? CheckForNamingErrors(
        OptimizedElmSyntaxDeclarations declarations,
        string stageName) =>
        CheckForNamingErrors(declarations.RenderAsFlatDictionary(), stageName);

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
    /// <see cref="OptimizedElmSyntaxDeclarations"/>-flavoured overload of
    /// <see cref="CheckForSyntaxNodesDisallowedInOptimization(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration}, string)"/>.
    /// </summary>
    private static string? CheckForSyntaxNodesDisallowedInOptimization(
        OptimizedElmSyntaxDeclarations declarations,
        string stageName) =>
        CheckForSyntaxNodesDisallowedInOptimization(declarations.RenderAsFlatDictionary(), stageName);

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
    /// Checks that all <see cref="SyntaxTypes.Expression.FunctionOrValue"/>
    /// references in <paramref name="declarations"/> are either fully
    /// qualified (have a module name) or refer to a local binding.
    /// Returns a human-readable error string aggregating up to the
    /// first few violations, or <see langword="null"/> if every
    /// declaration satisfies the invariant.
    /// <para>
    /// This is the post-stage formulation of the invariant enforced
    /// in-line by <c>SnapshotTestFormat.RenderQualifiedDeclarations</c>
    /// (via <c>ValidateFullyQualifiedReferences</c>). Running it as
    /// part of <see cref="ValidateStage(OptimizedElmSyntaxDeclarations, string)"/>
    /// catches a regression at the stage that introduced it, rather
    /// than at the downstream rendering site of an unrelated test.
    /// See §11.10 in
    /// <c>explore/internal-analysis/2026-05-18-eliminate-higher-order-parameters-in-focused-tests.md</c>.
    /// </para>
    /// </summary>
    private static string? CheckForUnqualifiedReferences(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        string stageName)
    {
        const int maxReportedViolations = 5;
        List<string>? violations = null;

        foreach (var (declName, decl) in declarations)
        {
            try
            {
                ElmSyntax.SnapshotTestFormat.ValidateFullyQualifiedReferences(declName, decl);
            }
            catch (InvalidOperationException ex)
            {
                violations ??= [];

                if (violations.Count < maxReportedViolations)
                    violations.Add(ex.Message);

                else
                {
                    violations.Add("(further violations suppressed)");
                    break;
                }
            }
        }

        if (violations is null)
            return null;

        return
            stageName + " produced " + violations.Count +
            " unqualified-reference invariant violation(s):\n" +
            string.Join("\n", violations);
    }

    private static string? CheckForUnqualifiedReferences(
        OptimizedElmSyntaxDeclarations declarations,
        string stageName) =>
        CheckForUnqualifiedReferences(declarations.RenderAsFlatDictionary(), stageName);

    /// <summary>
    /// Runs the standard pair of post-stage validation checks
    /// (<see cref="CheckForNamingErrors(OptimizedElmSyntaxDeclarations, string)"/> and
    /// <see cref="CheckForSyntaxNodesDisallowedInOptimization(OptimizedElmSyntaxDeclarations, string)"/>)
    /// against the supplied declarations and returns the first error message produced,
    /// or <see langword="null"/> if both checks pass. Factored out of
    /// <c>ApplyOptimizationPipelineWithStageResults</c> so each pipeline stage transition
    /// reads as a single guard call instead of repeating the two-check boilerplate.
    /// </summary>
    private static string? ValidateStage(
        OptimizedElmSyntaxDeclarations declarations,
        string stageName)
    {
        if (CheckForNamingErrors(declarations, stageName) is { } namingErr)
            return namingErr;

        if (CheckForSyntaxNodesDisallowedInOptimization(declarations, stageName) is { } disallowedErr)
            return disallowedErr;

        if (CheckForUnqualifiedReferences(declarations, stageName) is { } unqualifiedErr)
            return unqualifiedErr;

        return null;
    }

    /// <summary>
    /// Module-list flavoured overload of
    /// <see cref="ValidateStage(OptimizedElmSyntaxDeclarations, string)"/>.
    /// </summary>
    private static string? ValidateStage(
        IReadOnlyList<SyntaxTypes.File> modules,
        string stageName)
    {
        if (CheckForNamingErrors(modules, stageName) is { } namingErr)
            return namingErr;

        if (CheckForSyntaxNodesDisallowedInOptimization(modules, stageName) is { } disallowedErr)
            return disallowedErr;

        return null;
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
    public static string RenderCanonicalizationError(CanonicalizationError error) =>
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
            throw new NotImplementedException(
                "Unexpected error type: " + error.GetType())
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
                    DeclQualifiedName.Create(
                        moduleName,
                        declName);

                builder[qualifiedName] = decl;
            }
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Builds a minimal set of module shells (one per distinct namespace appearing in
    /// <paramref name="flatDecls"/>) suitable as input to the per-module lambda-lifting
    /// and optimization stages. The synthesized shells contain only the declarations
    /// from <paramref name="flatDecls"/> in deterministic <see cref="DeclQualifiedName"/>
    /// order — imports, comments and exposing lists are left empty because the
    /// downstream stages do not consult them.
    /// </summary>
    internal static List<SyntaxTypes.File> BuildModuleShellsFromFlatDeclarations(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> flatDecls)
    {
        var declsByNamespace =
            new Dictionary<IReadOnlyList<string>, List<KeyValuePair<DeclQualifiedName, SyntaxTypes.Declaration>>>(
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        foreach (var entry in flatDecls.OrderBy(kvp => kvp.Key))
        {
            if (!declsByNamespace.TryGetValue(entry.Key.Namespaces, out var bucket))
            {
                bucket = [];
                declsByNamespace[entry.Key.Namespaces] = bucket;
            }

            bucket.Add(entry);
        }

        var orderedNamespaces =
            declsByNamespace.Keys
            .OrderBy(ns => string.Join('.', ns), StringComparer.Ordinal)
            .ToList();

        var result = new List<SyntaxTypes.File>(orderedNamespaces.Count);

        foreach (var ns in orderedNamespaces)
        {
            var moduleDefinition =
                new SyntaxModelTypes.Node<SyntaxTypes.Module>(
                    ElmSyntaxTransformations.s_zeroRange,
                    new SyntaxTypes.Module.NormalModule(
                        new SyntaxTypes.DefaultModuleData(
                            ModuleName: new SyntaxModelTypes.Node<IReadOnlyList<string>>(
                                ElmSyntaxTransformations.s_zeroRange,
                                ns),
                            ExposingList: new SyntaxModelTypes.Node<SyntaxTypes.Exposing>(
                                ElmSyntaxTransformations.s_zeroRange,
                                new SyntaxTypes.Exposing.All(ElmSyntaxTransformations.s_zeroRange)))));

            var declarations =
                declsByNamespace[ns]
                .Select(
                    kvp =>
                    new SyntaxModelTypes.Node<SyntaxTypes.Declaration>(
                        ElmSyntaxTransformations.s_zeroRange,
                        kvp.Value))
                .ToList();

            result.Add(
                new SyntaxTypes.File(
                    ModuleDefinition: moduleDefinition,
                    Imports: [],
                    Declarations: declarations,
                    Comments: []));
        }

        return result;
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

                if (declName is null)
                    continue;

                var qualifiedName =
                    DeclQualifiedName.Create(
                        moduleName,
                        declName);

                if (flatDecls.TryGetValue(qualifiedName, out var replacementDecl))
                {
                    newDeclarations.Add(declNode with { Value = replacementDecl });
                    placedKeys.Add(qualifiedName);
                    continue;
                }

                if (declNode.Value is SyntaxTypes.Declaration.FunctionDeclaration)
                {
                    // Function declarations are only kept if appearing in the dependency tree from the entry points.
                    continue;
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
