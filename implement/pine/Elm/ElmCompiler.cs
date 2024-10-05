using ElmTime.ElmInteractive;
using ElmTime.JavaScript;
using Pine.Core;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Pine.Elm;

public class ElmCompiler : IDisposable
{
    private static readonly ConcurrentDictionary<TreeNodeWithStringPath, Task<Result<string, ElmCompiler>>> buildCompilerFromSource = new();

    static public readonly Lazy<TreeNodeWithStringPath> CompilerSourceFilesDefault =
        new(() => PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(
            ElmTime.ElmInteractive.ElmInteractive.LoadCompileElmProgramCodeFiles()
            .Extract(error => throw new NotImplementedException(nameof(ElmTime.ElmInteractive.ElmInteractive.LoadCompileElmProgramCodeFiles) + ": " + error))));

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

    public IJavaScriptEngine CompileElmPreparedJavaScriptEngine { get; }

    public PineValue CompilerEnvironment { get; }

    public ElmInteractiveEnvironment.FunctionRecord CompileParsedInteractiveSubmission { get; }

    private static readonly PineVM.PineVMParseCache parseCache = new();

    private ElmCompiler(
        IJavaScriptEngine compileElmPreparedJavaScriptEngine,
        PineValue compilerEnvironment,
        ElmInteractiveEnvironment.FunctionRecord compileParsedInteractiveSubmission)
    {
        CompileElmPreparedJavaScriptEngine = compileElmPreparedJavaScriptEngine;
        CompilerEnvironment = compilerEnvironment;
        CompileParsedInteractiveSubmission = compileParsedInteractiveSubmission;
    }

    public static Task<Result<string, ElmCompiler>> GetElmCompilerAsync(
        TreeNodeWithStringPath compilerSourceFiles)
    {
        return buildCompilerFromSource.GetOrAdd(
            compilerSourceFiles,
            valueFactory:
            compilerSourceFiles => Task.Run(() => BuildElmCompiler(compilerSourceFiles)));
    }

    public void Dispose()
    {
        CompileElmPreparedJavaScriptEngine?.Dispose();

        GC.SuppressFinalize(this);
    }

    public static Result<string, ElmCompiler> BuildElmCompiler(TreeNodeWithStringPath compilerSourceFiles)
    {
        var compileElmPreparedJavaScriptEngine =
            ElmTime.ElmInteractive.ElmInteractive.PrepareJavaScriptEngineToEvaluateElm(
                compileElmProgramCodeFiles: compilerSourceFiles,
                JavaScriptEngineFromJavaScriptEngineSwitcher.ConstructJavaScriptEngine());

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
            compilerSourceFiles.EnumerateBlobsTransitive()
            .Where(blobAtPath => blobAtPath.path.Last().ToLower().EndsWith(".elm"))
            .ToImmutableArray();

        var elmCoreLibraryModulesTexts =
            ElmTime.ElmInteractive.ElmInteractive.GetDefaultElmCoreModulesTexts(compileElmPreparedJavaScriptEngine);

        var elmModulesTexts = elmCoreLibraryModulesTexts;

        var compilerProgramOnlyElmJson =
            TreeNodeWithStringPath.FilterNodes(
                compilerSourceFiles,
                nodePath => nodePath.SequenceEqual(["elm.json"]));

        var allAvailableElmFiles =
            compilerAppCodeSourceFiles
            .Concat(compilerPackageSourcesFiles)
            .Select(blobAtPath => (blobAtPath, moduleText: Encoding.UTF8.GetString(blobAtPath.blobContent.Span)))
            .ToImmutableArray();

        var rootElmFile =
            allAvailableElmFiles
            .First(c => c.blobAtPath.path.SequenceEqual(["src", "ElmCompiler.elm"]));

        var elmModulesTextsForElmCompiler =
            ElmTime.ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: [rootElmFile.moduleText],
                availableModulesTexts: [.. allAvailableElmFiles.Select(f => f.moduleText)]);

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

        return
            InteractiveSessionPine.CompileInteractiveEnvironment(
                compileElmPreparedJavaScriptEngine,
                lastCompilationCache: ElmTime.ElmInteractive.ElmInteractive.CompilationCache.Empty,
                initialState: null,
                compilerWithPackagesTree)
            .AndThen(compiledEnv =>
            {
                return
                ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                        interactiveEnvironment: compiledEnv.compileResult,
                        moduleName: "ElmCompiler",
                        declarationName: "compileParsedInteractiveSubmission",
                        parseCache)
                .Map(parseModuleResponse =>
                new ElmCompiler(
                    compileElmPreparedJavaScriptEngine,
                    compiledEnv.compileResult,
                    parseModuleResponse.functionRecord));
            });
    }
}
