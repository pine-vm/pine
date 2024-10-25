using ElmTime.ElmInteractive;
using ElmTime.JavaScript;
using Pine.Core;
using Pine.Core.Elm;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
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

        "https://github.com/Viir/elm-bigint/tree/d452b489c5795f8deed19658a7b8f7bf5ef1e9a4/",

            /*
             * Remove usages of Json.Decode and Json.Encode to speed up bootstrapping of the Elm compiler.
             * Also, remove parsing portion and minimize dependencies in general for the first stage of bootstrapping.
             * */
            "https://github.com/Viir/elm-syntax/tree/6e7011a54323c046624ebddf0802d40366aae3bb/"
    ];

    public TreeNodeWithStringPath CompilerSourceFiles { get; }

    public PineValue CompilerEnvironment { get; }

    public ElmInteractiveEnvironment.FunctionRecord CompileParsedInteractiveSubmission { get; }

    public ElmInteractiveEnvironment.FunctionRecord ExpandElmInteractiveEnvironmentWithModules { get; }

    private static readonly PineVM.PineVMParseCache parseCache = new();

    private ElmCompiler(
        TreeNodeWithStringPath compilerSourceFiles,
        PineValue compilerEnvironment,
        ElmInteractiveEnvironment.FunctionRecord compileParsedInteractiveSubmission,
        ElmInteractiveEnvironment.FunctionRecord expandElmInteractiveEnvironmentWithModules)
    {
        CompilerSourceFiles = compilerSourceFiles;
        CompilerEnvironment = compilerEnvironment;
        CompileParsedInteractiveSubmission = compileParsedInteractiveSubmission;
        ExpandElmInteractiveEnvironmentWithModules = expandElmInteractiveEnvironmentWithModules;
    }

    public static Task<Result<string, ElmCompiler>> GetElmCompilerAsync(
        TreeNodeWithStringPath compilerSourceFiles)
    {
        return buildCompilerFromSource.GetOrAdd(
            compilerSourceFiles,
            valueFactory:
            compilerSourceFiles => Task.Run(() => BuildElmCompiler(compilerSourceFiles)));
    }

    public static Result<string, ElmCompiler> BuildElmCompiler(
        TreeNodeWithStringPath compilerSourceFiles)
    {
        var compilerWithPackagesTree =
            ElmCompilerFileTreeFromBundledFileTree(compilerSourceFiles);

        return
            LoadOrCompileInteractiveEnvironment(compilerWithPackagesTree)
            .AndThen(compiledEnv =>
            {
                return ElmCompilerFromEnvValue(
                    compilerSourceFiles,
                    compiledEnv);
            });
    }

    public static TreeNodeWithStringPath ElmCompilerFileTreeFromBundledFileTree(
        TreeNodeWithStringPath bundledFileTree) =>
        ElmCompilerFileTreeFromBundledFileTree(
            bundledFileTree,
            rootModuleFileNames: [["src", "ElmCompiler.elm"]]);

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
            TreeNodeWithStringPath.FilterNodes(
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

        var compilerWithCoreModules =
            MergeElmCoreModules(compilerWithPackagesTree);

        return compilerWithCoreModules;
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
        TreeNodeWithStringPath compilerSourceFiles)
    {
        if (BundledElmEnvironments.BundledElmEnvironmentFromFileTree(compilerSourceFiles) is { } fromBundle)
        {
            return Result<string, PineValue>.ok(fromBundle);
        }

        /*
         * TODO: Load from cache
         * */

        return CompileInteractiveEnvironment(compilerSourceFiles);
    }

    public static Result<string, PineValue> CompileInteractiveEnvironment(
        TreeNodeWithStringPath compilerSourceFiles)
    {
        return
            ElmTime.ElmInteractive.ElmInteractive.CompileInteractiveEnvironment(
                initialState: null,
                appCodeTree: compilerSourceFiles,
                compilationCacheBefore: ElmTime.ElmInteractive.ElmInteractive.CompilationCache.Empty)
            .Map(result => result.compileResult);
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

    public static Result<string, ElmCompiler> ElmCompilerFromEnvValue(
        TreeNodeWithStringPath compilerSourceFiles,
        PineValue compiledEnv)
    {
        return
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: compiledEnv,
                moduleName: "ElmCompiler",
                declarationName: "compileParsedInteractiveSubmission",
                parseCache)
            .MapError(err => "Failed parsing function to compile interactive submission")
            .AndThen(compileParsedInteractiveSubmission =>
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: compiledEnv,
                moduleName: "ElmCompiler",
                declarationName: "expandElmInteractiveEnvironmentWithModules",
                parseCache)
            .Map(expandElmInteractiveEnvironmentWithModules =>
            new ElmCompiler(
                compilerSourceFiles,
                compiledEnv,
                compileParsedInteractiveSubmission:
                compileParsedInteractiveSubmission.functionRecord,
                expandElmInteractiveEnvironmentWithModules:
                expandElmInteractiveEnvironmentWithModules.functionRecord)));
    }
}
