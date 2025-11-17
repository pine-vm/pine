using Pine.Core;
using Pine.Core.CommonEncodings;
using Pine.Core.DotNet;
using Pine.Core.Elm;
using Pine.Core.Files;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using ElmInteractiveEnvironment = Pine.Core.CodeAnalysis.ElmInteractiveEnvironment;

using FunctionRecord = Pine.Core.CodeAnalysis.FunctionRecord;

namespace Pine.Elm;

public class ElmCompiler
{
    private static readonly ConcurrentDictionary<FileTree, Task<Result<string, ElmCompiler>>> buildCompilerFromSource = new();

    static public readonly Lazy<FileTree> CompilerSourceContainerFilesDefault =
        new(() => FileTree.FromSetOfFilesWithStringPath(
            LoadElmCompilerSourceCodeFiles()
            .Extract(error => throw new NotImplementedException(nameof(LoadElmCompilerSourceCodeFiles) + ": " + error))));

    static public readonly Lazy<FileTree> ElmCoreAndKernelModuleFilesDefault =
        new(() =>
        CompilerSourceContainerFilesDefault.Value.GetNodeAtPath(["elm-kernel-modules"])
        ?? throw new Exception("Did not find node elm-kernel-modules"));

    static public readonly Lazy<IReadOnlyDictionary<IReadOnlyList<string>, string>> ElmCoreAndKernelModulesByName =
        new(() =>
        ElmCoreAndKernelModuleFilesDefault.Value
        .EnumerateFilesTransitive()
        .Where(blob => blob.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
        .ToDictionary(
            blobAtPath =>
            Core.Elm.ElmSyntax.ElmModule.ParseModuleName(Encoding.UTF8.GetString(blobAtPath.fileContent.Span))
            .Extract(err => throw new Exception(err)),

            blobAtPath =>
            Encoding.UTF8.GetString(blobAtPath.fileContent.Span),

            comparer:
            EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>()));

    static public readonly Lazy<FileTree> CompilerSourceFilesDefault =
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

    public FunctionRecord CompileParsedInteractiveSubmission { get; }

    public FunctionRecord ExpandElmInteractiveEnvironmentWithModules { get; }

    public FunctionRecord ParseElmModuleSyntax { get; }

    public FunctionRecord? ParseInteractiveSubmission { get; }

    public LanguageServiceInterfaceStruct? LanguageServiceInterface { get; }

    public record LanguageServiceInterfaceStruct(
        FunctionRecord InitState,
        FunctionRecord HandleRequestInCurrentWorkspace);

    private static readonly Core.CodeAnalysis.PineVMParseCache s_parseCache = new();

    private ElmCompiler(
        PineValue compilerEnvironment,
        FunctionRecord compileParsedInteractiveSubmission,
        FunctionRecord expandElmInteractiveEnvironmentWithModules,
        FunctionRecord parseElmModuleSyntax,
        FunctionRecord? parseInteractiveSubmission,
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
        FileTree compilerSourceFiles)
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
        FileTree compilerSourceFiles,
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

    public static FileTree ElmCompilerFileTreeFromBundledFileTree(
        FileTree bundledFileTree) =>
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

    public static FileTree ElmCompilerFileTreeFromBundledFileTree(
        FileTree bundledFileTree,
        IReadOnlyList<IReadOnlyList<string>> rootModuleFileNames)
    {
        var compilerPackageSourcesTrees =
            CompilerPackageSources
            .Select(LoadFromGitHubOrGitLab.LoadFromUrl)
            .ListCombine()
            .Extract(err => throw new Exception(err));

        var compilerPackageSourcesFiles =
            compilerPackageSourcesTrees
            .SelectMany(tree => tree.tree.EnumerateFilesTransitive())
            .Where(blobAtPath =>
            blobAtPath.path.First() == "src" && blobAtPath.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase));

        var compilerAppCodeSourceFiles =
            bundledFileTree.EnumerateFilesTransitive()
            .Where(blobAtPath => blobAtPath.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .ToImmutableArray();

        var compilerProgramOnlyElmJson =
            FileTree.FilterNodesByPath(
                bundledFileTree,
                nodePath => nodePath.SequenceEqual(["elm.json"]));

        var allAvailableElmFiles =
            compilerAppCodeSourceFiles
            .Concat(compilerPackageSourcesFiles)
            .Select(blobAtPath => (blobAtPath, moduleText: Encoding.UTF8.GetString(blobAtPath.fileContent.Span)))
            .ToImmutableArray();

        var rootElmFiles =
            allAvailableElmFiles
            .Where(c => rootModuleFileNames.Any(root => c.blobAtPath.path.SequenceEqual(root)))
            .ToImmutableArray();

        var elmModulesTextsForElmCompiler =
            Core.Elm.ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
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
                aggregate.SetNodeAtPathSorted(elmModule.path, FileTree.File(elmModule.fileContent)));

        return compilerWithPackagesTree;
    }

    public static Result<string, PineValue> LoadOrCompileInteractiveEnvironment(
        FileTree appCodeTree,
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
                ElmAppDependencyResolution.FilterTreeForCompilationRoots(
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
        FileTree appCodeTree,
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
                    FileTreeExtensions.ToFlatDictionaryWithPathComparer(appCodeTree),
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
                FileTree.FromSetOfFilesWithStringPath(loweringOk.Result.CompiledFiles);

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
            ElmAppDependencyResolution.FilterTreeForCompilationRoots(
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

    public static bool CheckIfAppUsesLowering(FileTree appCode)
    {
        var allAvailableElmFiles =
            appCode
            .EnumerateFilesTransitive()
            .Where(blobAtPath => blobAtPath.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(blobAtPath => (blobAtPath, moduleText: Encoding.UTF8.GetString(blobAtPath.fileContent.Span)))
            .ToImmutableArray();

        foreach (var file in allAvailableElmFiles)
        {
            var elmModuleName =
                Core.Elm.ElmSyntax.ElmModule.ParseModuleName(file.moduleText)
                .Extract(err => throw new Exception("Failed parsing module name: " + err));

            if (elmModuleName.First() is "CompilationInterface")
            {
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

        System.IO.Stream decompressedStream = sourceFile;

        if (filePath.EndsWith(".gzip", StringComparison.OrdinalIgnoreCase))
        {
            decompressedStream = new System.IO.Compression.GZipStream(sourceFile, System.IO.Compression.CompressionMode.Decompress);
        }

        using var stream = new System.IO.MemoryStream();

        decompressedStream.CopyTo(stream);
        stream.Seek(0, System.IO.SeekOrigin.Begin);

        var fileContent = stream.ToArray();

        var compiledEnv = ValueBinaryEncodingClassic.DecodeRoot(fileContent);

        return ElmCompilerFromEnvValue(compiledEnv);
    }
}
