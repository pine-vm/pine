using Pine.Core;
using Pine.Core.Elm;
using Pine.Elm019;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using ElmInteractiveEnvironment = Pine.Core.CodeAnalysis.ElmInteractiveEnvironment;

namespace Pine.Elm;

public class ElmCompiler
{
    private static readonly ConcurrentDictionary<BlobTreeWithStringPath, Task<Result<string, ElmCompiler>>> buildCompilerFromSource = new();

    static public readonly Lazy<BlobTreeWithStringPath> CompilerSourceContainerFilesDefault =
        new(() => PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(
            LoadElmCompilerSourceCodeFiles()
            .Extract(error => throw new NotImplementedException(nameof(LoadElmCompilerSourceCodeFiles) + ": " + error))));

    static public readonly Lazy<BlobTreeWithStringPath> ElmCoreAndKernelModuleFilesDefault =
        new(() =>
        CompilerSourceContainerFilesDefault.Value.GetNodeAtPath(["elm-kernel-modules"])
        ?? throw new Exception("Did not find node elm-kernel-modules"));

    static public readonly Lazy<IReadOnlyDictionary<IReadOnlyList<string>, string>> ElmCoreAndKernelModulesByName =
        new(() =>
        ElmCoreAndKernelModuleFilesDefault.Value
        .EnumerateBlobsTransitive()
        .Where(blob => blob.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
        .ToDictionary(
            blobAtPath =>
            ElmTime.ElmSyntax.ElmModule.ParseModuleName(Encoding.UTF8.GetString(blobAtPath.blobContent.Span))
            .Extract(err => throw new Exception(err)),

            blobAtPath =>
            Encoding.UTF8.GetString(blobAtPath.blobContent.Span),

            comparer:
            EnumerableExtension.EqualityComparer<IReadOnlyList<string>>()));

    static public readonly Lazy<BlobTreeWithStringPath> CompilerSourceFilesDefault =
        new(() => ElmCompilerFileTreeFromBundledFileTree(CompilerSourceContainerFilesDefault.Value));

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadElmCompilerSourceCodeFiles() =>
        DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ["Elm", "elm-compiler"],
            assembly: typeof(ElmCompiler).Assembly);

    public static IReadOnlyList<string> CompilerPackageSources =>
    [
        /*
         * Maybe.Extra is used in the overall app but not in the Elm compiler.
         * 
        "https://github.com/elm-community/maybe-extra/tree/2a9e2c6143dcee04180b265c840b6052b0a053c2/",
        */

        /*
         * elm-syntax/tree/f7d9be0a1f346b22dfaa7b55679659874c72714b contains a module List.Extra
         * 
        "https://github.com/elm-community/list-extra/tree/5a083cf0400260537adef75f96fbd48bfcedc7c0/",
        */

        /*
        "https://github.com/Viir/result-extra/tree/e3b2e4358ac701d66e75ccbfdc4256513dc70694",
        */

        /*
         * 2024-10-25: We are bundling these now so no need to load them from the internet.
        "https://github.com/Viir/elm-bigint/tree/d452b489c5795f8deed19658a7b8f7bf5ef1e9a4/",
        */

        /*
        * Remove usages of Json.Decode and Json.Encode to speed up bootstrapping of the Elm compiler.
        * Also, remove parsing portion and minimize dependencies in general for the first stage of bootstrapping.
        * */
        /*
         * 2024-10-25: We are bundling these now so no need to load them from the internet. 
        "https://github.com/Viir/elm-syntax/tree/6e7011a54323c046624ebddf0802d40366aae3bb/"
        */
    ];

    public PineValue CompilerEnvironment { get; }

    public ElmInteractiveEnvironment.FunctionRecord CompileParsedInteractiveSubmission { get; }

    public ElmInteractiveEnvironment.FunctionRecord ExpandElmInteractiveEnvironmentWithModules { get; }

    public ElmInteractiveEnvironment.FunctionRecord ParseElmModuleSyntax { get; }

    public ElmInteractiveEnvironment.FunctionRecord? ParseInteractiveSubmission { get; }

    public LanguageServiceInterfaceStruct? LanguageServiceInterface { get; }

    public record LanguageServiceInterfaceStruct(
        ElmInteractiveEnvironment.FunctionRecord InitState,
        ElmInteractiveEnvironment.FunctionRecord HandleRequestInCurrentWorkspace);

    private static readonly Core.CodeAnalysis.PineVMParseCache s_parseCache = new();

    private ElmCompiler(
        PineValue compilerEnvironment,
        ElmInteractiveEnvironment.FunctionRecord compileParsedInteractiveSubmission,
        ElmInteractiveEnvironment.FunctionRecord expandElmInteractiveEnvironmentWithModules,
        ElmInteractiveEnvironment.FunctionRecord parseElmModuleSyntax,
        ElmInteractiveEnvironment.FunctionRecord? parseInteractiveSubmission,
        LanguageServiceInterfaceStruct? languageServiceInterface)
    {
        CompilerEnvironment = compilerEnvironment;
        CompileParsedInteractiveSubmission = compileParsedInteractiveSubmission;
        ExpandElmInteractiveEnvironmentWithModules = expandElmInteractiveEnvironmentWithModules;
        ParseElmModuleSyntax = parseElmModuleSyntax;
        ParseInteractiveSubmission = parseInteractiveSubmission;
        LanguageServiceInterface = languageServiceInterface;
    }

    public static Task<Result<string, ElmCompiler>> GetElmCompilerAsync(
        BlobTreeWithStringPath compilerSourceFiles)
    {
        return buildCompilerFromSource.GetOrAdd(
            compilerSourceFiles,
            valueFactory:
            compilerSourceFiles => Task.Run(() => BuildCompilerFromSourceFiles(compilerSourceFiles)));
    }

    /// <summary>
    /// Compile an Elm compiler from the given tree of Elm source files.
    /// Typical applications for compiling the Elm compiler from source are verifying bootstrapping or
    /// experimenting with modifications to the emit stage.
    /// </summary>
    public static Result<string, ElmCompiler> BuildCompilerFromSourceFiles(
        BlobTreeWithStringPath compilerSourceFiles,
        ElmCompiler? overrideElmCompiler = null)
    {
        var compilerWithPackagesTree =
            ElmCompilerFileTreeFromBundledFileTree(compilerSourceFiles);

        return
            LoadOrCompileInteractiveEnvironment(
                compilerWithPackagesTree,
                rootFilePaths: DefaultCompilerTreeRootModuleFilePaths,
                skipLowering: true,
                overrideElmCompiler: overrideElmCompiler)
            .AndThen(compiledEnv =>
            {
                return ElmCompilerFromEnvValue(compiledEnv);
            });
    }

    public static BlobTreeWithStringPath ElmCompilerFileTreeFromBundledFileTree(
        BlobTreeWithStringPath bundledFileTree) =>
        ElmCompilerFileTreeFromBundledFileTree(
            bundledFileTree,
            rootModuleFileNames: DefaultCompilerTreeRootModuleFilePaths);

    public static IReadOnlyList<IReadOnlyList<string>> DefaultCompilerTreeRootModuleFilePaths =>
        [
        ["src", "ElmCompiler.elm"],
        ["elm-syntax", "src", "Elm", "Parser.elm"],
        ["src", "ElmInteractiveSubmissionParser.elm"],
        ["src", "LanguageService.elm"],
        ["src", "CompileElmAppMain.elm"],
        ];

    public static BlobTreeWithStringPath ElmCompilerFileTreeFromBundledFileTree(
        BlobTreeWithStringPath bundledFileTree,
        IReadOnlyList<IReadOnlyList<string>> rootModuleFileNames)
    {
        var compilerPackageSourcesTrees =
            CompilerPackageSources
            .Select(LoadFromGitHubOrGitLab.LoadFromUrl)
            .ListCombine()
            .Extract(err => throw new Exception(err));

        var compilerPackageSourcesFiles =
            compilerPackageSourcesTrees
            .SelectMany(tree => tree.tree.EnumerateBlobsTransitive())
            .Where(blobAtPath =>
            blobAtPath.path.First() == "src" && blobAtPath.path.Last().ToLower().EndsWith(".elm"));

        var compilerAppCodeSourceFiles =
            bundledFileTree.EnumerateBlobsTransitive()
            .Where(blobAtPath => blobAtPath.path.Last().ToLower().EndsWith(".elm"))
            .ToImmutableArray();

        var compilerProgramOnlyElmJson =
            BlobTreeWithStringPath.FilterNodesByPath(
                bundledFileTree,
                nodePath => nodePath.SequenceEqual(["elm.json"]));

        var allAvailableElmFiles =
            compilerAppCodeSourceFiles
            .Concat(compilerPackageSourcesFiles)
            .Select(blobAtPath => (blobAtPath, moduleText: Encoding.UTF8.GetString(blobAtPath.blobContent.Span)))
            .ToImmutableArray();

        var rootElmFiles =
            allAvailableElmFiles
            .Where(c => rootModuleFileNames.Any(root => c.blobAtPath.path.SequenceEqual(root)))
            .ToImmutableArray();

        var elmModulesTextsForElmCompiler =
            ElmTime.ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: [.. rootElmFiles.Select(file => file.moduleText)],
                availableModulesTexts: [.. allAvailableElmFiles.Select(file => file.moduleText)]);

        var elmModulesForElmCompiler =
            elmModulesTextsForElmCompiler
            .Select(moduleText => allAvailableElmFiles.First(c => c.moduleText == moduleText).blobAtPath)
            .ToImmutableArray();

        var compilerWithPackagesTree =
            elmModulesForElmCompiler
            .Aggregate(
                seed: compilerProgramOnlyElmJson,
                func: (aggregate, elmModule) =>
                aggregate.SetNodeAtPathSorted(elmModule.path, BlobTreeWithStringPath.Blob(elmModule.blobContent)));

        return compilerWithPackagesTree;
    }

    public static Result<string, PineValue> LoadOrCompileInteractiveEnvironment(
        BlobTreeWithStringPath appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        bool skipLowering,
        ElmCompiler? overrideElmCompiler = null)
    {
        if (rootFilePaths.Count is 0 &&
            overrideElmCompiler is null &&
            BundledElmEnvironments.BundledElmEnvironmentFromFileTree(appCodeTree) is { } fromBundle)
        {
            return Result<string, PineValue>.ok(fromBundle);
        }

        if (rootFilePaths.Count is not 0)
        {
            var appCodeFilteredForRoots =
                FilterTreeForCompilationRoots(
                    appCodeTree,
                    rootFilePaths);

            return
                LoadOrCompileInteractiveEnvironment(
                    appCodeFilteredForRoots,
                    rootFilePaths: [],
                    skipLowering: skipLowering,
                    overrideElmCompiler);
        }

        /*
         * TODO: Load from cache
         * */

        return
            CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: rootFilePaths,
                skipLowering: skipLowering,
                skipFilteringForSourceDirs: false,
                overrideElmCompiler: overrideElmCompiler);
    }

    public static Result<string, PineValue> CompileInteractiveEnvironment(
        BlobTreeWithStringPath appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        bool skipLowering,
        bool skipFilteringForSourceDirs,
        ElmCompiler? overrideElmCompiler = null)
    {
        Result<string, PineValue> ContinueWithBundledCompiler()
        {
            var elmCompilerFromBundle =
                BundledElmEnvironments.BundledElmCompilerCompiledEnvValue()
                ??
                throw new Exception("Failed to load Elm compiler from bundle.");

            var overrideElmCompiler =
                ElmCompilerFromEnvValue(elmCompilerFromBundle)
                .Extract(err => throw new Exception(err));

            return
                CompileInteractiveEnvironment(
                    appCodeTree,
                    rootFilePaths: rootFilePaths,
                    skipLowering: skipLowering,
                    skipFilteringForSourceDirs: skipFilteringForSourceDirs,
                    overrideElmCompiler: overrideElmCompiler);
        }

        var compilerSourceFilesDefault = CompilerSourceFilesDefault.Value;

        if (overrideElmCompiler is null && appCodeTree.Equals(compilerSourceFilesDefault))
        {
            return ContinueWithBundledCompiler();
        }

        if (!skipLowering && CheckIfAppUsesLowering(appCodeTree))
        {
            var loweringResult =
                ElmTime.ElmAppCompilation.AsCompletelyLoweredElmApp(
                    PineValueComposition.TreeToFlatDictionaryWithPathComparer(appCodeTree),
                    workingDirectoryRelative: [],
                    ElmTime.ElmAppInterfaceConfig.Default
                    with
                    {
                        CompilationRootFilePath = rootFilePaths.Single()
                    });

            if (loweringResult.IsErrOrNull() is { } loweringErr)
            {
                throw new Exception(
                    "Failed lowering with " + loweringErr.Count + " errors:\n" +
                    ElmTime.ElmAppCompilation.CompileCompilationErrorsDisplayText(loweringErr));
            }

            if (loweringResult.IsOkOrNull() is not { } loweringOk)
            {
                throw new Exception("Unexpected result type: " + loweringResult);
            }

            var loweredTree =
                PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(loweringOk.Result.CompiledFiles);

            var loweredTreeCleaned =
                ElmTime.ElmTimeJsonAdapter.CleanUpFromLoweredForJavaScript(loweredTree);

            return
                CompileInteractiveEnvironment(
                    loweredTreeCleaned,
                    rootFilePaths: rootFilePaths,
                    skipLowering: true,
                    skipFilteringForSourceDirs: skipFilteringForSourceDirs,
                    overrideElmCompiler: overrideElmCompiler);
        }

        var defaultCompilerResult =
            overrideElmCompiler is null
            ?
            GetElmCompilerAsync(compilerSourceFilesDefault).Result
            :
            overrideElmCompiler;

        if (defaultCompilerResult.IsErrOrNull() is { } err)
        {
            return "Failed getting default compiler: " + err;
        }

        if (defaultCompilerResult.IsOkOrNull() is not { } defaultCompiler)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + defaultCompilerResult.GetType());
        }

        var appCodeFilteredForRoots =
            rootFilePaths.Count is 0
            ?
            appCodeTree
            :
            FilterTreeForCompilationRoots(
                appCodeTree,
                rootFilePaths);

        return
            ElmTime.ElmInteractive.InteractiveSessionPine.CompileInteractiveEnvironment(
                appCodeTree:
                appCodeFilteredForRoots is null
                ?
                null
                :
                AppCompilationUnits.WithoutPackages(appCodeFilteredForRoots),
                overrideSkipLowering: true,
                entryPointsFilePaths: rootFilePaths,
                skipFilteringForSourceDirs: skipFilteringForSourceDirs,
                elmCompiler: defaultCompiler);
    }

    public static BlobTreeWithStringPath FilterTreeForCompilationRoots(
        BlobTreeWithStringPath tree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths)
    {
        var allAvailableElmFiles =
            tree
            .EnumerateBlobsTransitive()
            .Where(blobAtPath => blobAtPath.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(blobAtPath => (blobAtPath, moduleText: Encoding.UTF8.GetString(blobAtPath.blobContent.Span)))
            .ToImmutableArray();

        var rootElmFiles =
            allAvailableElmFiles
            .Where(c => rootFilePaths.Any(root => c.blobAtPath.path.SequenceEqual(root)))
            .ToImmutableArray();

        var elmModulesIncluded =
            ElmTime.ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: [.. rootElmFiles.Select(file => file.moduleText)],
                availableModulesTexts: [.. allAvailableElmFiles.Select(file => file.moduleText)]);

        var filePathsExcluded =
            allAvailableElmFiles
            .Where(elmFile => !elmModulesIncluded.Any(included => elmFile.moduleText == included))
            .Select(elmFile => elmFile.blobAtPath.path)
            .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        return
            BlobTreeWithStringPath.FilterNodesByPath(
                tree,
                nodePath =>
                !filePathsExcluded.Contains(nodePath));
    }

    public static Result<string, ElmCompiler> ElmCompilerFromEnvValue(PineValue compiledEnv)
    {
        var parseCompileSubmissionResult =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: compiledEnv,
                moduleName: "ElmCompiler",
                declarationName: "compileParsedInteractiveSubmission",
                s_parseCache);

        {
            if (parseCompileSubmissionResult.IsErrOrNull() is { } err)
            {
                return "Failed parsing function to compile interactive submission: " + err;
            }
        }

        if (parseCompileSubmissionResult.IsOkOrNullable() is not { } compileParsedInteractiveSubmission)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseCompileSubmissionResult.GetType());
        }

        var parseExpandEnvWithModulesResult =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: compiledEnv,
                moduleName: "ElmCompiler",
                declarationName: "expandElmInteractiveEnvironmentWithModules",
                s_parseCache);

        {
            if (parseExpandEnvWithModulesResult.IsErrOrNull() is { } err)
            {
                return "Failed parsing function to expand Elm interactive environment with modules: " + err;
            }
        }

        if (parseExpandEnvWithModulesResult.IsOkOrNullable() is not { } expandElmInteractiveEnvironmentWithModules)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseExpandEnvWithModulesResult.GetType());
        }

        var parseParseToFileResult =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: compiledEnv,
                moduleName: "Elm.Parser",
                declarationName: "parseToFile",
                s_parseCache);

        {
            if (parseParseToFileResult.IsErrOrNull() is { } err)
            {
                return "Failed parsing function to parse Elm file: " + err;
            }
        }

        if (parseParseToFileResult.IsOkOrNullable() is not { } parseToFile)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseParseToFileResult.GetType());
        }

        var parseParseInteractiveSubmissionResult =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: compiledEnv,
                moduleName: "ElmInteractiveSubmissionParser",
                declarationName: "parseInteractiveSubmissionFromString",
                s_parseCache);

        /*
        Currently not required because module ElmInteractiveSubmissionParser is not shipped with older versions we sometimes load here.

        {
            if (parseParseInteractiveSubmissionResult.IsErrOrNull() is { } err)
            {
                return "Failed parsing function to parse interactive submission: " + err;
            }
        }

        if (parseParseInteractiveSubmissionResult.IsOkOrNullable() is not { } parseInteractiveSubmission)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseParseInteractiveSubmissionResult.GetType());
        }
        */

        var parseInteractiveSubmission =
            parseParseInteractiveSubmissionResult
            .Unpack(
                fromErr: _ => null,
                fromOk: ok => ok.functionRecord);

        LanguageServiceInterfaceStruct? languageServiceInterface = null;

        {
            var parseInitStateResult =
                ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                    interactiveEnvironment: compiledEnv,
                    moduleName: "LanguageService",
                    declarationName: "initLanguageServiceState",
                    s_parseCache);

            var parseHandleRequest =
                ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                    interactiveEnvironment: compiledEnv,
                    moduleName: "LanguageService",
                    declarationName: "handleRequestInCurrentWorkspace",
                    s_parseCache);

            if (parseInitStateResult.IsOkOrNullable() is { } parseInitOk &&
                parseHandleRequest.IsOkOrNullable() is { } parseHandleRequestOk)
            {
                languageServiceInterface =
                    new LanguageServiceInterfaceStruct(
                        parseInitOk.functionRecord,
                        parseHandleRequestOk.functionRecord);
            }
        }

        return new ElmCompiler(
            compiledEnv,
            compileParsedInteractiveSubmission:
            compileParsedInteractiveSubmission.functionRecord,
            expandElmInteractiveEnvironmentWithModules:
            expandElmInteractiveEnvironmentWithModules.functionRecord,
            parseElmModuleSyntax:
            parseToFile.functionRecord,
            parseInteractiveSubmission:
            parseInteractiveSubmission,
            languageServiceInterface:
            languageServiceInterface);
    }

    public Result<string, PineValue> ParseElmModuleText(
        string elmModuleText,
        Core.PineVM.IPineVM pineVM)
    {
        var elmModuleTextEncoded =
            ElmValueEncoding.ElmValueAsPineValue(
                ElmValue.StringInstance(elmModuleText));

        var parseToFileResultValue =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                ParseElmModuleSyntax,
                [elmModuleTextEncoded]);

        if (parseToFileResultValue.IsErrOrNull() is { } err)
            return "Failed to apply function: " + err;

        if (parseToFileResultValue.IsOkOrNull() is not { } applyFunctionOk)
            throw new Exception("Unexpected result type: " + parseToFileResultValue.GetType().FullName);

        return
            ElmValueInterop.ParseElmResultValue(
                applyFunctionOk,
                err:
                errValue =>
                Result<string, PineValue>.err(
                    "Failed to parse Elm module text: 'Err': " +
                        ElmValueEncoding.PineValueAsElmValue(errValue, null, null)
                        .Unpack(
                            fromErr: err => "Failed to parse as Elm value: " + err,
                            fromOk: elmValue => ElmValue.RenderAsElmExpression(elmValue).expressionString)),
                ok:
                okValue => okValue,
                invalid:
                (err) => throw new Exception("Invalid Elm result value: " + err));
    }

    public Result<string, PineValue> ParseElmInteractiveSubmissionText(
        string submissionText,
        Core.PineVM.IPineVM pineVM)
    {
        var submissionTextEncoded =
            ElmValueEncoding.ElmValueAsPineValue(
                ElmValue.StringInstance(submissionText));

        var parseResultValue =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                ParseInteractiveSubmission,
                [submissionTextEncoded]);

        if (parseResultValue.IsErrOrNull() is { } err)
            return "Failed to apply function: " + err;

        if (parseResultValue.IsOkOrNull() is not { } applyFunctionOk)
            throw new Exception("Unexpected result type: " + parseResultValue.GetType().FullName);

        return
            ElmValueInterop.ParseElmResultValue(
                applyFunctionOk,
                err:
                errValue =>
                Result<string, PineValue>.err(
                    "Failed to parse submission text: 'Err': " +
                        ElmValueEncoding.PineValueAsElmValue(errValue, null, null)
                        .Unpack(
                            fromErr: err => "Failed to parse as Elm value: " + err,
                            fromOk: elmValue => ElmValue.RenderAsElmExpression(elmValue).expressionString)),
                ok:
                okValue => okValue,
                invalid:
                (err) => throw new Exception("Invalid Elm result value: " + err));
    }

    public static bool CheckIfAppUsesLowering(BlobTreeWithStringPath appCode)
    {
        var allAvailableElmFiles =
            appCode
            .EnumerateBlobsTransitive()
            .Where(blobAtPath => blobAtPath.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(blobAtPath => (blobAtPath, moduleText: Encoding.UTF8.GetString(blobAtPath.blobContent.Span)))
            .ToImmutableArray();

        foreach (var file in allAvailableElmFiles)
        {
            var elmModuleName =
                ElmTime.ElmSyntax.ElmModule.ParseModuleName(file.moduleText)
                .Extract(err => throw new Exception("Failed parsing module name: " + err));

            if (elmModuleName.First() is "CompilationInterface")
            {
                return true;
            }
        }

        return false;
    }

    public static BlobTreeWithStringPath FilterTreeForCompilationRoots(
        BlobTreeWithStringPath tree,
        IReadOnlySet<IReadOnlyList<string>> rootFilePaths,
        bool skipFilteringForSourceDirs)
    {
        var trees =
            rootFilePaths
            .Select(rootFilePath =>
            FilterTreeForCompilationRoot(
                tree,
                rootFilePath,
                skipFilteringForSourceDirs: skipFilteringForSourceDirs))
            .ToImmutableArray();

        return
            trees
            .Aggregate(
                seed: BlobTreeWithStringPath.EmptyTree,
                BlobTreeWithStringPath.MergeBlobs);
    }

    public static BlobTreeWithStringPath FilterTreeForCompilationRoot(
        BlobTreeWithStringPath tree,
        IReadOnlyList<string> rootFilePath,
        bool skipFilteringForSourceDirs)
    {
        var keepElmModuleAtFilePath =
            skipFilteringForSourceDirs
            ?
            _ => true
            :
            BuildPredicateFilePathIsInSourceDirectory(tree, rootFilePath);

        var allAvailableElmFiles =
            tree
            .EnumerateBlobsTransitive()
            .Where(blobAtPath => blobAtPath.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .ToImmutableArray();

        var availableElmFiles =
            allAvailableElmFiles
            .Where(blobAtPath => keepElmModuleAtFilePath(blobAtPath.path))
            .ToImmutableArray();

        var rootElmFiles =
            availableElmFiles
            .Where(c => c.path.SequenceEqual(rootFilePath))
            .ToImmutableArray();

        var elmModulesIncluded =
            ElmTime.ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: [.. rootElmFiles.Select(file => Encoding.UTF8.GetString(file.blobContent.Span))],
                availableModulesTexts: [.. availableElmFiles.Select(file => Encoding.UTF8.GetString(file.blobContent.Span))]);

        var filePathsExcluded =
            allAvailableElmFiles
            .Where(elmFile => !elmModulesIncluded.Any(included => Encoding.UTF8.GetString(elmFile.blobContent.Span) == included))
            .Select(elmFile => elmFile.path)
            .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        return
            BlobTreeWithStringPath.FilterNodesByPath(
                tree,
                nodePath =>
                !filePathsExcluded.Contains(nodePath));
    }

    private static Func<IReadOnlyList<string>, bool> BuildPredicateFilePathIsInSourceDirectory(
        BlobTreeWithStringPath tree,
        IReadOnlyList<string> rootFilePath)
    {
        if (FindElmJsonForEntryPoint(tree, rootFilePath) is not { } elmJsonForEntryPoint)
        {
            throw new Exception(
                "Failed to find elm.json for entry point: " + string.Join("/", rootFilePath));
        }

        IReadOnlyList<ElmJsonStructure.RelativeDirectory> sourceDirectories =
            [.. elmJsonForEntryPoint.elmJsonParsed.ParsedSourceDirectories];

        bool filePathIsInSourceDirectory(IReadOnlyList<string> filePath)
        {
            foreach (var sourceDirectory in sourceDirectories)
            {
                if (sourceDirectory.ParentLevel is not 0)
                {
                    throw new NotImplementedException(
                        "ParentLevel in elm.json source-directories not implemented");
                }

                if (filePath.Count < sourceDirectory.Subdirectories.Count)
                {
                    continue;
                }

                if (filePath.Take(sourceDirectory.Subdirectories.Count).SequenceEqual(sourceDirectory.Subdirectories))
                {
                    return true;
                }
            }

            return false;
        }

        return filePathIsInSourceDirectory;
    }

    public static (IReadOnlyList<string> filePath, ElmJsonStructure elmJsonParsed)?
        FindElmJsonForEntryPoint(
        BlobTreeWithStringPath sourceFiles,
        IReadOnlyList<string> entryPointFilePath)
    {
        // Collect all elm.json files from the tree, storing each parsed ElmJsonStructure along with its path:
        var elmJsonFiles =
            sourceFiles
            .EnumerateBlobsTransitive()
            .SelectMany(pathAndContent =>
            {
                if (!pathAndContent.path.Last().EndsWith("elm.json", StringComparison.OrdinalIgnoreCase))
                {
                    return [];
                }

                var elmJsonContent = pathAndContent.blobContent;

                try
                {
                    var elmJsonParsed =
                        System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonContent.Span);

                    return
                        new[]
                        {
                            (filePath: (IReadOnlyList<string>)pathAndContent.path, elmJsonParsed)
                        };
                }
                catch (Exception e)
                {
                    return [];
                }
            })
            .ToImmutableDictionary(
                keySelector: entry => entry.filePath,
                elementSelector: entry => entry.elmJsonParsed,
                keyComparer: EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        // Walk upwards from the directory of entryPointFilePath to find the "closest" elm.json
        // that includes the entryPointFilePath in one of its source-directories:
        var currentDirectory = DirectoryOf(entryPointFilePath);

        while (true)
        {
            // See if there is an elm.json directly in this directory:

            IReadOnlyList<string> elmJsonFilePath = [.. currentDirectory, "elm.json"];

            if (elmJsonFiles.TryGetValue(elmJsonFilePath, out var elmJsonParsed) && elmJsonParsed is not null)
            {
                // We found an elm.json in the current directory; now check if it includes the entry point
                // by verifying that entryPointFilePath is under one of its source-directories:
                if (ElmJsonIncludesEntryPoint(
                    currentDirectory, elmJsonParsed, entryPointFilePath))
                {
                    return (elmJsonFilePath, elmJsonParsed);
                }
            }

            // If we are at the root (no parent to move up to), stop:
            if (currentDirectory.Count is 0)
            {
                return null;
            }

            // Move up one level:
            currentDirectory = [.. currentDirectory.Take(currentDirectory.Count - 1)];
        }
    }

    /// <summary>
    /// Returns all but the last segment of filePath (i.e. the directory path).
    /// </summary>
    private static IReadOnlyList<string> DirectoryOf(IReadOnlyList<string> filePath)
    {
        if (filePath.Count is 0)
            return filePath;

        return [.. filePath.Take(filePath.Count - 1)];
    }

    /// <summary>
    /// Checks whether the given elm.json file includes entryPointFilePath in one of its source-directories.
    /// Since source-directories in elm.json are relative to the directory containing elm.json,
    /// we build absolute paths and compare.
    /// </summary>
    private static bool ElmJsonIncludesEntryPoint(
        IReadOnlyList<string> elmJsonDirectory,
        ElmJsonStructure elmJson,
        IReadOnlyList<string> entryPointFilePath)
    {
        // For each source directory in elm.json, build its absolute path (relative to elm.jsonDirectory),
        // and check whether entryPointFilePath starts with that path.

        foreach (var sourceDir in elmJson.ParsedSourceDirectories)
        {
            // Combine the elmJsonDirectory with the subdirectories from sourceDir
            // to get the absolute path to the "source directory":
            IReadOnlyList<string> absSourceDir =
                [.. elmJsonDirectory, .. sourceDir.Subdirectories];

            // Check if entryPointFilePath is "under" absSourceDir:
            if (entryPointFilePath.Count >= absSourceDir.Count &&
                entryPointFilePath
                .Take(absSourceDir.Count)
                .SequenceEqual(absSourceDir))
            {
                // The entry point sits in one of the source-directories recognized by this elm.json
                return true;
            }
        }

        return false;
    }

    public static Result<string, ElmCompiler> LoadCompilerFromBundleFile(string filePath)
    {
        using var sourceFile =
            new System.IO.FileStream(
                path: filePath,
                System.IO.FileMode.Open,
                System.IO.FileAccess.Read);

        var envDictResult =
            BundledElmEnvironmentsJson.LoadBundledCompiledEnvironments(
                sourceFile,
                gzipDecompress: filePath.EndsWith(".gzip", StringComparison.OrdinalIgnoreCase));

        {
            if (envDictResult.IsErrOrNull() is { } err)
            {
                return "Failed loading Elm compiler from bundle: " + err;
            }
        }

        if (envDictResult.IsOkOrNull() is not { } envDict)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + envDictResult.GetType());
        }

        var compiledEnv =
            envDict.Values
            .OfType<PineValue.ListValue>()
            .OrderByDescending(list => list.NodesCount)
            .First();

        return ElmCompilerFromEnvValue(compiledEnv);
    }
}
