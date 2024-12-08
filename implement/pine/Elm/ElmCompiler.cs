using ElmTime.ElmInteractive;
using ElmTime.JavaScript;
using Pine.Core;
using Pine.Core.Elm;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Pine.Elm;

public class ElmCompiler
{
    private static readonly ConcurrentDictionary<TreeNodeWithStringPath, Task<Result<string, ElmCompiler>>> buildCompilerFromSource = new();

    static public readonly Lazy<TreeNodeWithStringPath> CompilerSourceContainerFilesDefault =
        new(() => PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(
            LoadElmCompilerSourceCodeFiles()
            .Extract(error => throw new NotImplementedException(nameof(LoadElmCompilerSourceCodeFiles) + ": " + error))));

    static public readonly Lazy<TreeNodeWithStringPath> ElmCoreAndKernelModuleFilesDefault =
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

    static public readonly Lazy<TreeNodeWithStringPath> CompilerSourceFilesDefault =
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

    private static readonly PineVM.PineVMParseCache parseCache = new();

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
        TreeNodeWithStringPath compilerSourceFiles)
    {
        return buildCompilerFromSource.GetOrAdd(
            compilerSourceFiles,
            valueFactory:
            compilerSourceFiles => Task.Run(() => BuildCompilerFromSourceFiles(compilerSourceFiles)));
    }

    public static Result<string, ElmCompiler> BuildCompilerFromSourceFiles(
        TreeNodeWithStringPath compilerSourceFiles,
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

    public static TreeNodeWithStringPath ElmCompilerFileTreeFromBundledFileTree(
        TreeNodeWithStringPath bundledFileTree) =>
        ElmCompilerFileTreeFromBundledFileTree(
            bundledFileTree,
            rootModuleFileNames: DefaultCompilerTreeRootModuleFilePaths);

    public static IReadOnlyList<IReadOnlyList<string>> DefaultCompilerTreeRootModuleFilePaths =>
        [
        ["src", "ElmCompiler.elm"],
        ["elm-syntax", "src", "Elm", "Parser.elm"],
        ["src", "ElmInteractiveSubmissionParser.elm"],
        ["src", "LanguageService.elm"],
        ];

    public static TreeNodeWithStringPath ElmCompilerFileTreeFromBundledFileTree(
        TreeNodeWithStringPath bundledFileTree,
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
            TreeNodeWithStringPath.FilterNodesByPath(
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
                aggregate.SetNodeAtPathSorted(elmModule.path, TreeNodeWithStringPath.Blob(elmModule.blobContent)));

        return compilerWithPackagesTree;
    }

    public static TreeNodeWithStringPath MergeElmCoreModules(
        TreeNodeWithStringPath compilerWithPackagesTree)
    {
        using var compileElmPreparedJavaScriptEngine =
            JavaScriptEngineFromElmCompilerSourceFiles(CompilerSourceContainerFilesDefault.Value);

        var defaultElmCoreModulesTexts =
            ElmTime.ElmInteractive.ElmInteractive.GetDefaultElmCoreModulesTexts(compileElmPreparedJavaScriptEngine);

        var defaultElmCoreModules =
            defaultElmCoreModulesTexts
            .Select(moduleText =>
            (moduleName: ElmTime.ElmSyntax.ElmModule.ParseModuleName(moduleText).Extract(err => throw new Exception(err)),
            moduleText))
            .ToImmutableArray();

        var compilerWithCoreModules =
            defaultElmCoreModules
            .Aggregate(
                seed: compilerWithPackagesTree,
                func: (aggregate, elmModule) =>
                {
                    IReadOnlyList<string> filePath =
                    ["src"
                    , .. elmModule.moduleName.SkipLast(1)
                    , elmModule.moduleName.Last() + ".elm"
                    ];

                    var fileContent = Encoding.UTF8.GetBytes(elmModule.moduleText);

                    return aggregate.SetNodeAtPathSorted(filePath, TreeNodeWithStringPath.Blob(fileContent));
                });

        return compilerWithCoreModules;
    }

    public static Result<string, PineValue> LoadOrCompileInteractiveEnvironment(
        TreeNodeWithStringPath appCodeTree,
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
                overrideElmCompiler: overrideElmCompiler);
    }

    public static Result<string, PineValue> CompileInteractiveEnvironment(
        TreeNodeWithStringPath appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        bool skipLowering,
        ElmCompiler? overrideElmCompiler = null)
    {
        /*
         * 2024-10-27:
         * Change to depend less often on JavaScript to build an environment.
         * Instead of always going the JavaScript route, default to the new Pine-based route.
         * */

        Result<string, PineValue> continueOnJavaScript()
        {
            return
                ElmTime.ElmInteractive.ElmInteractive.CompileInteractiveEnvironment(
                    appCodeTree: appCodeTree,
                    rootFilePaths: rootFilePaths,
                    skipLowering: skipLowering,
                    compilationCacheBefore: ElmTime.ElmInteractive.ElmInteractive.CompilationCache.Empty)
                .Map(result => result.compileResult);
        }

        if (!skipLowering)
        {
            // We need to port some more dependencies before migrating lowering away from JavaScript.

            return continueOnJavaScript();
        }

        var compilerSourceFilesDefault = CompilerSourceFilesDefault.Value;

        if (overrideElmCompiler is null && appCodeTree.Equals(compilerSourceFilesDefault))
        {
            return continueOnJavaScript();
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
            InteractiveSessionPine.CompileInteractiveEnvironment(
                appCodeTree: appCodeFilteredForRoots,
                overrideSkipLowering: true,
                elmCompiler: defaultCompiler);
    }

    public static TreeNodeWithStringPath FilterTreeForCompilationRoots(
        TreeNodeWithStringPath tree,
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
            TreeNodeWithStringPath.FilterNodesByPath(
                tree,
                nodePath =>
                !filePathsExcluded.Contains(nodePath));
    }

    public static IJavaScriptEngine JavaScriptEngineFromElmCompilerSourceFiles(
        TreeNodeWithStringPath compilerSourceFiles)
    {
        var javaScriptEngine =
            JavaScriptEngineFromJavaScriptEngineSwitcher.ConstructJavaScriptEngine();

        ElmTime.ElmInteractive.ElmInteractive.PrepareJavaScriptEngineToEvaluateElm(
            compileElmProgramCodeFiles: compilerSourceFiles,
            javaScriptEngine);

        return javaScriptEngine;
    }

    public static Result<string, ElmCompiler> ElmCompilerFromEnvValue(PineValue compiledEnv)
    {
        var parseCompileSubmissionResult =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: compiledEnv,
                moduleName: "ElmCompiler",
                declarationName: "compileParsedInteractiveSubmission",
                parseCache);

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
                parseCache);

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
                parseCache);

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
                parseCache);

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
                    parseCache);

            var parseHandleRequest =
                ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                    interactiveEnvironment: compiledEnv,
                    moduleName: "LanguageService",
                    declarationName: "handleRequestInCurrentWorkspace",
                    parseCache);

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
            ElmInteractive.ElmValueEncoding.ElmValueAsPineValue(
                ElmInteractive.ElmValue.StringInstance(elmModuleText));

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
            ElmInteractive.ElmValueInterop.ParseElmResultValue(
                applyFunctionOk,
                err:
                errValue =>
                Result<string, PineValue>.err(
                    "Failed to parse Elm module text: 'Err': " +
                        ElmInteractive.ElmValueEncoding.PineValueAsElmValue(errValue, null, null)
                        .Unpack(
                            fromErr: err => "Failed to parse as Elm value: " + err,
                            fromOk: elmValue => ElmInteractive.ElmValue.RenderAsElmExpression(elmValue).expressionString)),
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
            ElmInteractive.ElmValueEncoding.ElmValueAsPineValue(
                ElmInteractive.ElmValue.StringInstance(submissionText));

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
            ElmInteractive.ElmValueInterop.ParseElmResultValue(
                applyFunctionOk,
                err:
                errValue =>
                Result<string, PineValue>.err(
                    "Failed to parse submission text: 'Err': " +
                        ElmInteractive.ElmValueEncoding.PineValueAsElmValue(errValue, null, null)
                        .Unpack(
                            fromErr: err => "Failed to parse as Elm value: " + err,
                            fromOk: elmValue => ElmInteractive.ElmValue.RenderAsElmExpression(elmValue).expressionString)),
                ok:
                okValue => okValue,
                invalid:
                (err) => throw new Exception("Invalid Elm result value: " + err));
    }

    public static bool CheckIfAppUsesLowering(TreeNodeWithStringPath appCode)
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
}
