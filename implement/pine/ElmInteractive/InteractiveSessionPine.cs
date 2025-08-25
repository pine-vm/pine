using Pine;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.PineVM;
using Pine.Core.PopularEncodings;
using Pine.Elm;
using Pine.Elm019;
using Pine.PineVM;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using static ElmTime.ElmInteractive.IInteractiveSession;

namespace ElmTime.ElmInteractive;

public class InteractiveSessionPine : IInteractiveSession
{
    private readonly System.Threading.Lock submissionLock = new();

    private System.Threading.Tasks.Task<Result<string, PineValue>> buildPineEvalContextTask;

    private readonly Result<string, ElmCompiler> buildCompilerResult;

    private readonly IPineVM pineVM;

    private static readonly IPineVM compileEnvPineVM =
        new LockingPineVM(
            new PineVMWithPersistentCache(
                new FileStoreFromSystemIOFile(
                    System.IO.Path.Combine(Filesystem.CacheDirectory, "elm-compiler-vm", Program.AppVersionId))));

    /*
     * TODO: Move these caches to a dedicated scope to enable for better control over reuse/disposal.
     * */

    static readonly ElmCompilerCache elmCompilerCache = new();

    static readonly ConcurrentDictionary<string, Result<string, KeyValuePair<IReadOnlyList<string>, PineValue>>> TryParseModuleTextCache = new();

    public InteractiveSessionPine(
        BlobTreeWithStringPath compilerSourceFiles,
        BlobTreeWithStringPath? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        bool caching,
        DynamicPGOShare? autoPGO)
        :
        this(
            compilerSourceFiles: compilerSourceFiles,
            appCodeTree:
            appCodeTree is null ? null : AppCompilationUnits.WithoutPackages(appCodeTree),
            overrideSkipLowering: overrideSkipLowering,
            entryPointsFilePaths: entryPointsFilePaths,
            BuildPineVM(
                caching: caching,
                autoPGO: autoPGO,
                new PineVM.EvaluationConfig(ParseAndEvalCountLimit: 10_000_000)))
    {
    }

    public InteractiveSessionPine(
        BlobTreeWithStringPath compilerSourceFiles,
        AppCompilationUnits? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        bool caching,
        DynamicPGOShare? autoPGO)
        :
        this(
            compilerSourceFiles: compilerSourceFiles,
            appCodeTree: appCodeTree,
            overrideSkipLowering: overrideSkipLowering,
            entryPointsFilePaths: entryPointsFilePaths,
            BuildPineVM(
                caching: caching,
                autoPGO: autoPGO,
                new PineVM.EvaluationConfig(ParseAndEvalCountLimit: 10_000_000)))
    {
    }

    public InteractiveSessionPine(
        BlobTreeWithStringPath compilerSourceFiles,
        BlobTreeWithStringPath? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        IPineVM pineVM)
        :
        this(
            compilerSourceFiles: compilerSourceFiles,
            appCodeTree:
            appCodeTree is null ? null : AppCompilationUnits.WithoutPackages(appCodeTree),
            overrideSkipLowering: overrideSkipLowering,
            entryPointsFilePaths: entryPointsFilePaths,
            pineVM)
    {
    }

    public InteractiveSessionPine(
        BlobTreeWithStringPath compilerSourceFiles,
        AppCompilationUnits? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        IPineVM pineVM)
        :
        this(
            compilerSourceFiles: compilerSourceFiles,
            appCodeTree: appCodeTree,
            overrideSkipLowering: overrideSkipLowering,
            entryPointsFilePaths: entryPointsFilePaths,
            (pineVM, pineVMCache: null))
    {
    }

    public InteractiveSessionPine(
        BlobTreeWithStringPath compilerSourceFiles,
        AppCompilationUnits? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        (IPineVM pineVM, PineVMCache? pineVMCache) pineVMAndCache)
    {
        pineVM = pineVMAndCache.pineVM;

        buildCompilerResult =
            ElmCompiler.GetElmCompilerAsync(compilerSourceFiles).Result;

        buildPineEvalContextTask =
            System.Threading.Tasks.Task.Run(() =>
            CompileInteractiveEnvironment(
                appCodeTree: appCodeTree,
                overrideSkipLowering: overrideSkipLowering,
                entryPointsFilePaths: entryPointsFilePaths,
                skipFilteringForSourceDirs: false));
    }

    public static (IPineVM, PineVMCache?) BuildPineVM(
        bool caching,
        DynamicPGOShare? autoPGO,
        PineVM.EvaluationConfig? evaluationConfig = null)
    {
        var cache = caching ? new PineVMCache() : null;

        return (BuildPineVM(cache, autoPGO, evaluationConfig), cache);
    }

    public static IPineVM BuildPineVM(
        PineVMCache? cache,
        DynamicPGOShare? autoPGO,
        PineVM.EvaluationConfig? evaluationConfig)
    {
        if (autoPGO is not null)
        {
            return
                autoPGO.GetVMAutoUpdating(evalCache: cache?.EvalCache);
        }

        return
            new PineVM(
                evalCache: cache?.EvalCache,
                evaluationConfigDefault: evaluationConfig,
                reportFunctionApplication: null);
    }


    private Result<string, PineValue> CompileInteractiveEnvironment(
        AppCompilationUnits? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        bool skipFilteringForSourceDirs)
    {
        if (buildCompilerResult.IsOkOrNull() is not { } elmCompiler)
        {
            if (buildCompilerResult.IsErrOrNull() is { } err)
            {
                return "Failed to build Elm compiler: " + err;
            }

            throw new NotImplementedException(
                "Unexpected compiler result type: " + buildCompilerResult.GetType());
        }

        return
            CompileInteractiveEnvironment(
                appCodeTree,
                overrideSkipLowering: overrideSkipLowering,
                entryPointsFilePaths: entryPointsFilePaths,
                skipFilteringForSourceDirs: skipFilteringForSourceDirs,
                elmCompiler,
                compileEnvPineVM);
    }

    public static Result<string, PineValue>
        CompileInteractiveEnvironment(
        AppCompilationUnits? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        bool skipFilteringForSourceDirs,
        ElmCompiler elmCompiler) =>
        CompileInteractiveEnvironment(
            appCodeTree,
            overrideSkipLowering: overrideSkipLowering,
            entryPointsFilePaths: entryPointsFilePaths,
            skipFilteringForSourceDirs: skipFilteringForSourceDirs,
            elmCompiler,
            compileEnvPineVM);

    public static Result<string, PineValue>
        CompileInteractiveEnvironment(
        AppCompilationUnits? appCodeTree,
        bool? overrideSkipLowering,
        IReadOnlyList<IReadOnlyList<string>>? entryPointsFilePaths,
        bool skipFilteringForSourceDirs,
        ElmCompiler elmCompiler,
        IPineVM pineVM)
    {
        var skipLowering =
            overrideSkipLowering ??
            !ElmCompiler.CheckIfAppUsesLowering(appCodeTree?.AppFiles ?? BlobTreeWithStringPath.EmptyTree);

        var appSourceFiles =
            appCodeTree ?? AppCompilationUnits.WithoutPackages(BlobTreeWithStringPath.EmptyTree);

        var initialStateElmValue =
            ElmValueInterop.PineValueEncodedAsInElmCompiler(PineValue.EmptyList);

        var initialStateElmValueInCompiler =
            ElmValueEncoding.ElmValueAsPineValue(initialStateElmValue);

        var compiledNewEnvInCompiler = initialStateElmValueInCompiler;

        var defaultKernelModulesTree =
            ElmCompiler.ElmCoreAndKernelModuleFilesDefault.Value;

        var appCodeModules =
            appSourceFiles.AppFiles
            .EnumerateBlobsTransitive()
            .Where(blob => blob.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(blob =>
            {
                var moduleText = System.Text.Encoding.UTF8.GetString(blob.blobContent.Span);

                var moduleName =
                ElmSyntax.ElmModule.ParseModuleName(moduleText)
                .Extract(err => throw new Exception("Failed parsing module name: " + err));

                return new KeyValuePair<IReadOnlyList<string>, ReadOnlyMemory<byte>>(moduleName, blob.blobContent);
            })
            .ToImmutableDictionary(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        ReadOnlyMemory<byte>? replaceKernelModule(
            IImmutableList<string> path,
            ReadOnlyMemory<byte> blobContent)
        {
            if (!path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            {
                return null;
            }

            if (ModuleNameFromFileContent(blobContent.Span) is not { } moduleName)
            {
                return null;
            }

            if (appCodeModules.TryGetValue(moduleName, out var appCodeModule) && 0 < appCodeModule.Length)
            {
                return appCodeModule;
            }

            return null;
        }

        var mergedKernelModulesTree =
            PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(
                defaultKernelModulesTree.EnumerateBlobsTransitive()
                .Select(blob =>
                (blob.path,
                replaceKernelModule(blob.path, blob.blobContent) is { } overrideContent
                ?
                overrideContent
                :
                blob.blobContent)));

        var mergedKernelModulesTreeBlobs =
            mergedKernelModulesTree
            .EnumerateBlobsTransitive()
            .ToImmutableArray();

        var appFilesAfterKernelModules =
            PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(
                appSourceFiles.AppFiles.EnumerateBlobsTransitive()
                .Where(blob =>
                !mergedKernelModulesTreeBlobs.Any(
                    kernelBlob =>
                    kernelBlob.blobContent.Span.SequenceEqual(blob.blobContent.Span))));

        var compileKernelModulesResult =
            CompileInteractiveEnvironmentUnitEncodedInCompiler(
                compiledNewEnvInCompiler,
                mergedKernelModulesTree,
                entryPointsFilePaths: null,
                elmCompiler,
                pineVM: pineVM);

        if (compileKernelModulesResult.IsErrOrNull() is { } compileKernelModulesErr)
        {
            return "Failed to compile kernel modules: " + compileKernelModulesErr;
        }

        if (compileKernelModulesResult.IsOkOrNull() is not { } compiledNewEnvInCompilerUnit)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + compileKernelModulesResult.GetType());
        }

        compiledNewEnvInCompiler = compiledNewEnvInCompilerUnit;

        foreach (var package in appSourceFiles.Packages)
        {
            var packageName = package.elmJson.Name;

            if (packageName is "elm/core" ||
                packageName is "elm/json" ||
                packageName is "elm/bytes" ||
                packageName is "elm/parser" ||
                packageName is "elm/url" ||
                packageName is "elm/time")
            {
                continue;
            }

            var compilePackageResult =
                CompileInteractiveEnvironmentPackageEncodedInCompiler(
                    compiledNewEnvInCompiler,
                    package.files,
                    package.elmJson,
                    elmCompiler,
                    pineVM: pineVM);

            if (compilePackageResult.IsErrOrNull() is { } compilePackageErr)
            {
                return "Compiling package " + package.elmJson.Name + " failed: " + compilePackageErr;
            }

            if (compilePackageResult.IsOkOrNull() is not { } compilePackageOk)
            {
                throw new NotImplementedException(
                    "Unexpected result type: " + compilePackageResult.GetType());
            }

            compiledNewEnvInCompiler = compilePackageOk;
        }

        var orderedModules =
            AppSourceFileTreesForIncrementalCompilation(
                appFilesAfterKernelModules,
                skipLowering: skipLowering,
                entryPointsFilePaths:
                entryPointsFilePaths?.ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>()),
                skipFilteringForSourceDirs: skipFilteringForSourceDirs)
            .ToImmutableArray();

        foreach (var compilationIncrement in orderedModules)
        {
            var parseResult =
                CachedTryParseModuleText(
                    compilationIncrement.sourceModule.ModuleText,
                    elmCompiler,
                    pineVM);

            if (parseResult.IsErrOrNull() is { } parseErr)
            {
                return
                    "Failed parsing module " +
                    string.Join(".", compilationIncrement.sourceModule.ModuleName) + ": " + parseErr;
            }

            if (parseResult.IsOkOrNullable() is not { } parsedModule)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseResult.GetType());
            }

            var parsedModuleNameFlat = string.Join(".", parsedModule.Key);

            var compileModuleResult =
                CompileOneElmModule(
                    compiledNewEnvInCompiler,
                    compilationIncrement.sourceModule.ModuleText,
                    parsedModule.Value,
                    elmCompiler,
                    pineVM: pineVM);

            if (compileModuleResult.IsOkOrNull() is not { } compileModuleOk)
            {
                return
                    "Compiling module " + parsedModuleNameFlat + " failed: " +
                    compileModuleResult.Unpack(fromErr: err => err, fromOk: _ => "no err");
            }

            compiledNewEnvInCompiler = compileModuleOk;
        }

        var asElmValueResult =
            elmCompilerCache.PineValueDecodedAsElmValue(compiledNewEnvInCompiler);

        if (asElmValueResult.IsErrOrNull() is { } asElmValueErr)
        {
            return "Failed to decode environment from compiler as Elm value: " + asElmValueErr;
        }

        if (asElmValueResult.IsOkOrNull() is not { } compiledNewEnvInCompilerElm)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + asElmValueResult.GetType());
        }

        var compiledNewEnvValueResult =
            elmCompilerCache.DecodeElmValueFromCompiler(compiledNewEnvInCompilerElm);

        if (compiledNewEnvValueResult.IsErrOrNull() is { } compiledNewEnvValueErr)
        {
            return "Failed to decode environment from Elm value: " + compiledNewEnvValueErr;
        }

        if (compiledNewEnvValueResult.IsOkOrNull() is not { } compiledNewEnvValue)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + compiledNewEnvValueResult.GetType());
        }

        return compiledNewEnvValue;
    }


    public static Result<string, PineValue>
        CompileInteractiveEnvironmentPackageEncodedInCompiler(
        PineValue initialStateElmValueInCompiler,
        BlobTreeWithStringPath packageFiles,
        ElmJsonStructure elmJson,
        ElmCompiler elmCompiler,
        IPineVM pineVM)
    {
        IReadOnlyDictionary<string, IReadOnlyList<string>> fileNameFromModuleName =
            packageFiles.EnumerateBlobsTransitive()
            .Where(blob => blob.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(blob =>
            {
                var moduleText = System.Text.Encoding.UTF8.GetString(blob.blobContent.Span);
                var moduleName =
                ElmSyntax.ElmModule.ParseModuleName(moduleText)
                .Extract(err => throw new Exception("Failed parsing module name: " + err));

                return new KeyValuePair<string, IReadOnlyList<string>>(
                    string.Join(".", moduleName),
                    blob.path);
            })
            .ToImmutableDictionary();

        var entryPointsFilePaths =
            elmJson.ExposedModules
            .Select((moduleNameFlat) => fileNameFromModuleName[moduleNameFlat])
            .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        var compilationUnitResult =
            CompileInteractiveEnvironmentUnitEncodedInCompiler(
                initialStateElmValueInCompiler,
                packageFiles,
                entryPointsFilePaths: entryPointsFilePaths,
                elmCompiler,
                pineVM);

        if (compilationUnitResult.IsErrOrNull() is { } compilationUnitErr)
        {
            return "Failed to compile package: " + compilationUnitErr;
        }

        if (compilationUnitResult.IsOkOrNull() is not { } compiledNewEnvInCompilerUnit)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + compilationUnitResult.GetType());
        }

        // TODO: Filter non-exposed modules.

        return compiledNewEnvInCompilerUnit;
    }


    public static Result<string, PineValue>
        CompileInteractiveEnvironmentUnitEncodedInCompiler(
        PineValue initialStateElmValueInCompiler,
        BlobTreeWithStringPath sourceFiles,
        IReadOnlySet<IReadOnlyList<string>>? entryPointsFilePaths,
        ElmCompiler elmCompiler,
        IPineVM pineVM)
    {
        var compiledNewEnvInCompiler = initialStateElmValueInCompiler;

        var modulesToCompile =
            ElmInteractive.ModulesFilePathsAndTextsFromAppCodeTree(
                sourceFiles,
                skipLowering: true,
                entryPointsFilePaths: entryPointsFilePaths,
                skipFilteringForSourceDirs: true,
                mergeKernelModules: false);

        var modulesToCompileTexts =
            modulesToCompile
            .Where(sm => !ElmInteractive.ShouldIgnoreSourceFile(sm.filePath, sm.fileContent))
            .Select(sm => sm.moduleText)
            .ToImmutableArray();

        var modulesTextsOrdered =
            ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: modulesToCompileTexts,
                availableModulesTexts: []);

        var modulesToCompileOrdered =
            modulesTextsOrdered
            .Select(mt => modulesToCompile.First(c => c.moduleText == mt))
            .ToImmutableArray();

        foreach (var compilationIncrement in modulesToCompileOrdered)
        {
            var moduleName =
                ElmSyntax.ElmModule.ParseModuleName(compilationIncrement.moduleText)
                .Extract(err => throw new Exception("Failed parsing module name: " + err));

            var parseResult =
                CachedTryParseModuleText(
                    compilationIncrement.moduleText,
                    elmCompiler,
                    pineVM);

            if (parseResult.IsErrOrNull() is { } parseErr)
            {
                return
                    "Failed parsing module " +
                    string.Join(".", moduleName) + ": " + parseErr;
            }

            if (parseResult.IsOkOrNullable() is not { } parsedModule)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseResult.GetType());
            }

            var parsedModuleNameFlat = string.Join(".", parsedModule.Key);

            var compileModuleResult =
                CompileOneElmModule(
                    compiledNewEnvInCompiler,
                    compilationIncrement.moduleText,
                    parsedModule.Value,
                    elmCompiler,
                    pineVM: pineVM);

            if (compileModuleResult.IsOkOrNull() is not { } compileModuleOk)
            {
                return
                    "Compiling module " + parsedModuleNameFlat + " failed: " +
                    compileModuleResult.Unpack(fromErr: err => err, fromOk: _ => "no err");
            }

            compiledNewEnvInCompiler = compileModuleOk;
        }

        return compiledNewEnvInCompiler;
    }

    public static BlobTreeWithStringPath MergeDefaultElmCoreAndKernelModules(
        BlobTreeWithStringPath appCodeTree) =>
        MergeDefaultElmCoreAndKernelModules(
            appCodeTree,
            ElmCompiler.ElmCoreAndKernelModuleFilesDefault.Value);

    public static BlobTreeWithStringPath MergeDefaultElmCoreAndKernelModules(
        BlobTreeWithStringPath appCodeTree,
        BlobTreeWithStringPath elmCoreAndKernelModuleFilesDefault)
    {
        var appCodeTreeModuleNames =
            appCodeTree.EnumerateBlobsTransitive()
            .SelectMany((blob) =>
            {
                var fileName = blob.path.Last();

                if (!fileName.EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
                    return (IEnumerable<IReadOnlyList<string>>)[];

                if (ModuleNameFromFileContent(blob.blobContent.Span) is not { } moduleName)
                {
                    return [];
                }

                return [moduleName];
            })
            .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        return
            elmCoreAndKernelModuleFilesDefault
            .EnumerateBlobsTransitive()
            .Aggregate(
                seed:
                appCodeTree,

                func:
                (aggregate, nextBlob) =>
                {
                    if (aggregate.GetNodeAtPath(nextBlob.path) is not null)
                        return aggregate;

                    if (ModuleNameFromFileContent(nextBlob.blobContent.Span) is { } moduleName)
                    {
                        if (appCodeTreeModuleNames.Contains(moduleName))
                            return aggregate;
                    }

                    return
                    aggregate.SetNodeAtPathSorted(
                        nextBlob.path,
                        BlobTreeWithStringPath.Blob(nextBlob.blobContent));
                });
    }

    static IReadOnlyList<string>? ModuleNameFromFileContent(ReadOnlySpan<byte> fileContent)
    {
        var blobChars = new char[fileContent.Length];

        if (!System.Text.Encoding.UTF8.TryGetChars(fileContent, blobChars, out var charsWritten))
            return null;

        if (ElmSyntax.ElmModule.ParseModuleName(
            new string(blobChars.AsSpan()[..charsWritten])).IsOkOrNull() is not { } moduleName)
        {
            return null;
        }

        return moduleName;
    }

    public record ParsedModule(
        IReadOnlyList<string> FilePath,
        IReadOnlyList<string> ModuleName,
        string ModuleText);

    public static IEnumerable<(BlobTreeWithStringPath tree, ParsedModule sourceModule)>
        AppSourceFileTreesForIncrementalCompilation(
        BlobTreeWithStringPath appSourceFiles,
        bool skipLowering,
        IReadOnlySet<IReadOnlyList<string>>? entryPointsFilePaths,
        bool skipFilteringForSourceDirs)
    {
        var compileableSourceModules =
            ElmInteractive.ModulesFilePathsAndTextsFromAppCodeTree(
                appSourceFiles,
                skipLowering: skipLowering,
                entryPointsFilePaths: entryPointsFilePaths,
                skipFilteringForSourceDirs: skipFilteringForSourceDirs,
                mergeKernelModules: false) ?? [];

        var baseTree =
            compileableSourceModules
            .Aggregate(
                seed: appSourceFiles,
                func: (tree, compileableModule) => tree.RemoveNodeAtPath(compileableModule.filePath) ?? throw new Exception());

        var compileableSourceModulesTexts =
            compileableSourceModules
            .Where(sm => !ElmInteractive.ShouldIgnoreSourceFile(sm.filePath, sm.fileContent))
            .Select(sm => sm.moduleText)
            .ToImmutableArray();

        var modulesTextsOrdered =
            ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: compileableSourceModulesTexts,
                availableModulesTexts: []);

        var compileableSourceModulesOrdered =
            modulesTextsOrdered
            .Select(mt => compileableSourceModules.First(c => c.moduleText == mt))
            .ToImmutableArray();

        var mutatedTree = baseTree;

        foreach (var sourceModule in compileableSourceModulesOrdered)
        {
            var moduleName =
                ElmSyntax.ElmModule.ParseModuleName(sourceModule.moduleText)
                .Extract(err => throw new Exception("Failed parsing module name: " + err));

            mutatedTree =
                mutatedTree.SetNodeAtPathSorted(sourceModule.filePath, BlobTreeWithStringPath.Blob(sourceModule.fileContent));

            yield return
                (mutatedTree,
                new ParsedModule(
                    FilePath: sourceModule.filePath,
                    ModuleName: moduleName,
                    ModuleText: sourceModule.moduleText));
        }
    }

    static Result<string, KeyValuePair<IReadOnlyList<string>, PineValue>> CachedTryParseModuleText(
        string moduleText,
        ElmCompiler elmCompiler,
        IPineVM pineVM) =>
        TryParseModuleTextCache
        .GetOrAdd(
            moduleText,
            valueFactory:
            moduleText => TryParseModuleText(moduleText, elmCompiler, pineVM));

    static Result<string, KeyValuePair<IReadOnlyList<string>, PineValue>> TryParseModuleText(
        string moduleText,
        ElmCompiler elmCompiler,
        IPineVM pineVM)
    {
        var parseNameResult =
            ElmSyntax.ElmModule.ParseModuleName(moduleText);

        {
            if (parseNameResult.IsErrOrNull() is { } parseNameErr)
            {
                return
                    "Failed parsing name for module " +
                    moduleText.Split('\n', '\r').FirstOrDefault() + ": " + parseNameErr;
            }
        }

        if (parseNameResult.IsOkOrNull() is not { } moduleName)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseNameResult.GetType());
        }

        {
            var fromDotnetResult =
                Pine.ElmSyntax.ElmSyntaxParser.ParseModuleTextAsElmSyntaxElmValue(moduleText);

            // .NET based implementation of ElmSyntaxParser does not yet cover all syntax.

            if (fromDotnetResult.IsOkOrNull() is { } parsedOk)
            {
                var asPineValue =
                    ElmValueEncoding.ElmValueAsPineValue(parsedOk);

                return
                    new KeyValuePair<IReadOnlyList<string>, PineValue>(
                        moduleName,
                        asPineValue);
            }
        }

        var parseModuleResult =
            elmCompiler.ParseElmModuleText(moduleText, pineVM);

        {
            if (parseModuleResult.IsErrOrNull() is { } parseModuleErr)
            {
                return
                    "Failed parsing module " +
                    string.Join(".", moduleName) + ": " + parseModuleErr;
            }
        }

        if (parseModuleResult.IsOkOrNull() is not { } parsedModule)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseModuleResult.GetType());
        }

        return new KeyValuePair<IReadOnlyList<string>, PineValue>(moduleName, parsedModule);
    }

    public static Result<string, PineValue> CompileOneElmModule(
        PineValue prevEnvValue,
        string moduleText,
        PineValue parsedModuleValue,
        ElmCompiler elmCompiler,
        IPineVM pineVM)
    {
        var applyFunctionResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                elmCompiler.ExpandElmInteractiveEnvironmentWithModules,
                arguments:
                [
                    prevEnvValue,
                    PineValue.List([ParsedElmFileRecordValue(moduleText, parsedModuleValue)])
                ]
                );

        if (applyFunctionResult.IsErrOrNull() is { } applyErr)
            return "Failed to apply function: " + applyErr;

        if (applyFunctionResult.IsOkOrNull() is not { } applyFunctionOk)
            throw new Exception("Unexpected result type: " + applyFunctionResult.GetType().FullName);

        var parseAsTagResult = ElmValueEncoding.ParseAsTag(applyFunctionOk);

        if (parseAsTagResult.IsErrOrNull() is { } parseAsTagErr)
            return "Failed to parse result as tag: " + parseAsTagErr;

        if (parseAsTagResult.IsOkOrNullable() is not { } parseAsTagOk)
            throw new Exception("Unexpected result type: " + parseAsTagResult.GetType().FullName);

        if (parseAsTagOk.tagName is not "Ok")
        {
            return
                "Failed to extract environment: Tag not 'Ok': " +
                elmCompilerCache.PineValueDecodedAsElmValue(applyFunctionOk)
                .Unpack(
                    fromErr: err => "Failed to parse as Elm value: " + err,
                    fromOk: elmValue => ElmValue.RenderAsElmExpression(elmValue).expressionString);
        }

        if (parseAsTagOk.tagArguments.Length is not 1)
        {
            return
                "Failed to extract environment: Expected one element in the list, got " +
                parseAsTagOk.tagArguments.Length;
        }

        var parseAsRecordResult =
            ElmValueEncoding.ParsePineValueAsRecordTagged(parseAsTagOk.tagArguments.Span[0]);

        if (parseAsRecordResult.IsErrOrNull() is { } parseAsRecordError)
            return "Failed to parse as record: " + parseAsRecordError;

        if (parseAsRecordResult.IsOkOrNull() is not { } recordFields)
            throw new Exception("Unexpected result type: " + parseAsRecordResult.GetType().FullName);

        var environmentValueField =
            recordFields
            .SingleOrDefault(f => f.fieldName is "environment")
            .fieldValue;

        if (environmentValueField is not PineValue environmentValue)
            return "Failed to extract environment: not a Pine value: " + environmentValueField;

        return environmentValue;
    }

    static PineValue ParsedElmFileRecordValue(
        string fileText,
        PineValue parsedModuleValue) =>
        ElmValueEncoding.ElmRecordAsPineValue(
            [
            ("fileText", ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(fileText)))
            ,("parsedModule", parsedModuleValue)
            ]);

    public Result<string, SubmissionResponse> Submit(string submission)
    {
        var inspectionLog = new List<string>();

        return
            Submit(submission, inspectionLog.Add)
            .Map(r => new SubmissionResponse(r, inspectionLog));
    }

    public static Result<string, PineValue> ParseInteractiveSubmission(
        ElmCompiler elmCompiler,
        IPineVM pineVM,
        string submission,
        Action<string>? addInspectionLogEntry)
    {
        var clock = System.Diagnostics.Stopwatch.StartNew();

        void logDuration(string label) =>
            addInspectionLogEntry?.Invoke(
                label + " duration: " + CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms");

        var response = elmCompiler.ParseElmInteractiveSubmissionText(submission, pineVM);

        logDuration("parse via Elm compiler");

        return response;
    }

    public Result<string, ElmInteractive.EvaluatedStruct> Submit(
        string submission,
        Action<string>? addInspectionLogEntry)
    {
        lock (submissionLock)
        {
            var clock = System.Diagnostics.Stopwatch.StartNew();

            void logDuration(string label) =>
                addInspectionLogEntry?.Invoke(
                    label + " duration: " + CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms");

            if (buildCompilerResult.IsErrOrNull() is { } buildCompilerErr)
            {
                return "Failed to build Elm compiler: " + buildCompilerErr;
            }

            if (buildCompilerResult.IsOkOrNull() is not { } elmCompiler)
            {
                throw new NotImplementedException(
                    "Unexpected build compiler result type: " + buildCompilerResult.GetType());
            }

            if (buildPineEvalContextTask.Result.IsErrOrNull() is { } buildPineEvalContextErr)
            {
                return "Failed to build initial Pine eval context: " + buildPineEvalContextErr;
            }

            if (buildPineEvalContextTask.Result.IsOkOrNull() is not { } buildPineEvalContextOk)
            {
                throw new NotImplementedException(
                    "Unexpected build Pine eval context result type: " + buildPineEvalContextTask.Result.GetType());
            }

            clock.Restart();

            var parseSubmissionResult =
                ParseInteractiveSubmission(
                    elmCompiler,
                    pineVM,
                    submission: submission,
                    addInspectionLogEntry: compileEntry => addInspectionLogEntry?.Invoke("Parse: " + compileEntry));

            if (parseSubmissionResult.IsErrOrNull() is { } parseSubmissionErr)
            {
                return "Failed to parse submission: " + parseSubmissionErr;
            }

            if (parseSubmissionResult.IsOkOrNull() is not { } parsedSubmissionOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse submission result type: " + parseSubmissionResult.GetType());
            }

            var parseSubmissionAsElmValueResult =
                elmCompilerCache.PineValueDecodedAsElmValue(parsedSubmissionOk);

            if (parseSubmissionAsElmValueResult.IsErrOrNull() is { } parseSubmissionAsElmValueErr)
            {
                return "Failed parsing submission response as Elm value: " + parseSubmissionAsElmValueErr;
            }

            if (parseSubmissionAsElmValueResult.IsOkOrNull() is not { } parsedSubmissionAsElmValue)
            {
                throw new NotImplementedException(
                    "Unexpected parse submission as Elm value result type: " + parseSubmissionAsElmValueResult.GetType());
            }

            logDuration("parse");

            clock.Restart();

            var environmentEncodedForCompiler =
                elmCompilerCache.EncodeValueForCompiler(buildPineEvalContextOk);

            logDuration("compile - encode environment");

            clock.Restart();

            var compileParsedResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM,
                    elmCompiler.CompileParsedInteractiveSubmission,
                    arguments:
                    [
                        environmentEncodedForCompiler,
                        parsedSubmissionOk
                    ]);

            if (compileParsedResult.IsErrOrNull() is { } compileParsedErr)
            {
                return "Failed compiling parsed submission (" +
                    parsedSubmissionAsElmValue + "): " + compileParsedErr;
            }

            if (compileParsedResult.IsOkOrNull() is not { } compileParsedOk)
            {
                return
                    "Failed compiling parsed submission (" +
                    parsedSubmissionAsElmValue + "): " +
                    compileParsedResult.Unpack(err => err, ok => "Not an err");
            }

            logDuration("compile - apply");

            clock.Restart();

            var compileParsedOkAsElmValueResult =
                elmCompilerCache.PineValueDecodedAsElmValue(compileParsedOk);

            if (compileParsedOkAsElmValueResult.IsErrOrNull() is { } compileParsedOkAsElmValueErr)
            {
                return "Failed decoding compiled expression: " + compileParsedOkAsElmValueErr;
            }

            if (compileParsedOkAsElmValueResult.IsOkOrNull() is not { } compileParsedOkAsElmValue)
            {
                throw new NotImplementedException(
                    "Unexpected compile parsed ok as Elm value result type: " + compileParsedOkAsElmValueResult.GetType());
            }

            logDuration("compile - decode result");

            clock.Restart();

            if (compileParsedOkAsElmValue is not ElmValue.ElmTag compiledResultTag)
            {
                return "Unexpected return value: No tag: " + compileParsedOkAsElmValue;
            }

            if (compiledResultTag.TagName is not "Ok" || compiledResultTag.Arguments.Count is not 1)
            {
                return "Failed compile: " + compiledResultTag;
            }

            var decodeExpressionResult =
                elmCompilerCache.DecodeExpressionFromElmValue(compiledResultTag.Arguments[0]);

            if (decodeExpressionResult.IsErrOrNull() is { } decodeExpressionErr)
            {
                return "Failed decoding compiled expression: " + decodeExpressionErr;
            }

            if (decodeExpressionResult.IsOkOrNull() is not { } resultingExpr)
            {
                throw new NotImplementedException(
                    "Unexpected decode expression result type: " + decodeExpressionResult.GetType());
            }

            logDuration("compile - decode expression");

            clock.Restart();

            var evalResult = pineVM.EvaluateExpression(resultingExpr, buildPineEvalContextOk);

            logDuration("eval");

            if (evalResult.IsErrOrNull() is { } evalErr)
            {
                return "Failed to evaluate compiled expression in PineVM: " + evalErr;
            }

            if (evalResult.IsOkOrNull() is not { } evalOk)
            {
                throw new NotImplementedException(
                    "Unexpected eval result type: " + evalResult.GetType());
            }

            if (evalOk is not PineValue.ListValue evalResultListComponent)
            {
                return "Type mismatch: Pine expression not evaluated to a list: " + evalOk;
            }

            if (evalResultListComponent.Items.Length is not 2)
            {
                return
                    "Type mismatch: Pine expression evaluated to a list with unexpected number of elements: " +
                    evalResultListComponent.Items.Length +
                    " instead of 2";
            }

            buildPineEvalContextTask = System.Threading.Tasks.Task.FromResult(
                Result<string, PineValue>.ok(evalResultListComponent.Items.Span[0]));

            clock.Restart();

            var parseSubmissionResponseResult =
                ElmInteractive.SubmissionResponseFromResponsePineValue(
                    response: evalResultListComponent.Items.Span[1]);

            logDuration("parse-result");

            if (parseSubmissionResponseResult.IsErrOrNull() is { } parseSubmissionResponseErr)
            {
                return "Failed to parse submission response: " + parseSubmissionResponseErr;
            }

            if (parseSubmissionResponseResult.IsOkOrNull() is not { } parseSubmissionResponseOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse submission response result type: " + parseSubmissionResponseResult.GetType());
            }

            return parseSubmissionResponseOk;
        }
    }

    public PineValue CurrentEnvironmentValue()
    {
        lock (submissionLock)
        {
            return buildPineEvalContextTask.Result.Extract(err => throw new Exception(err));
        }
    }

    void IDisposable.Dispose()
    {
    }

    public static Result<string, Pine.CompilePineToDotNet.CompileCSharpClassResult> CompileForProfiledScenarios(
        BlobTreeWithStringPath compileElmProgramCodeFiles,
        IReadOnlyList<TestElmInteractive.Scenario> scenarios,
        Pine.CompilePineToDotNet.SyntaxContainerConfig syntaxContainerConfig,
        int limitNumber,
        bool enableEvalExprCache)
    {
        var expressionsProfiles =
            scenarios
            .Select(scenario =>
            CollectExpressionsToOptimizeFromScenario(
                compileElmProgramCodeFiles: compileElmProgramCodeFiles,
                scenario: scenario,
                enableEvalExprCache: enableEvalExprCache))
            .ToImmutableArray();

        var aggregateExpressionsProfiles =
            ProfilingPineVM.AggregateExpressionUsageProfiles(expressionsProfiles);

        var expressionsToCompile =
            DynamicPGOShare.FilterAndRankExpressionProfilesForCompilation(aggregateExpressionsProfiles)
            .Take(limitNumber)
            .Select(expressionAndProfile => expressionAndProfile.Key)
            .ToImmutableList();

        return
            Pine.CompilePineToDotNet.CompileToCSharp.CompileExpressionsToCSharpClass(
                expressionsToCompile,
                syntaxContainerConfig);
    }

    public static IReadOnlyDictionary<ExpressionUsageAnalysis, ExpressionUsageProfile> CollectExpressionsToOptimizeFromScenario(
        BlobTreeWithStringPath compileElmProgramCodeFiles,
        TestElmInteractive.Scenario scenario,
        bool enableEvalExprCache)
    {
        var cache =
            enableEvalExprCache ? new PineVMCache() : null;

        var profilingVM =
            new ProfilingPineVM(
                evalCache: cache?.EvalCache);

        var profilingSession = new InteractiveSessionPine(
            compilerSourceFiles: compileElmProgramCodeFiles,
            appCodeTree: (AppCompilationUnits?)null,
            overrideSkipLowering: false,
            entryPointsFilePaths: null,
            profilingVM.PineVM);

        foreach (var step in scenario.Steps)
            profilingSession.Submit(step.step.Submission);

        var exprUsageSamples = profilingVM.ExprEnvUsagesFlat;

        var expressionUsages =
            exprUsageSamples
            .Select(sample => sample.Value.Analysis.Value.ToMaybe())
            .WhereNotNothing()
            .SelectMany(analysis => analysis)
            .ToImmutableList();

        var pineExpressionsToOptimize =
            ProfilingPineVM.UsageProfileDictionaryFromListOfUsages(expressionUsages);

        return pineExpressionsToOptimize;
    }


    public class PineVMWithPersistentCache : IPineVM
    {
        private readonly Dictionary<(Expression, PineValue), PineValue> evalCache = [];

        private readonly Dictionary<EvalCacheEntryKey, PineValue> vmEvalCache = [];

        private readonly IPineVM pineVM;

        private readonly IFileStore fileStore;

        public PineVMWithPersistentCache(IFileStore fileStore)
        {
            this.fileStore = fileStore;

            pineVM = new PineVM(evalCache: vmEvalCache);
        }

        public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment)
        {
            if (evalCache.TryGetValue((expression, environment), out var cachedResult))
            {
                return cachedResult;
            }

            var fileName = FileNameFromKey(expression, environment);

            try
            {
                if (fileStore.GetFileContent([fileName]) is { } fileContent)
                {
                    var loadedValue = PineValueBinaryEncoding.DecodeRoot(fileContent);

                    evalCache[(expression, environment)] = loadedValue;

                    return loadedValue;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Failed to read or parse cache file: " + e);
            }

            var evalResult = pineVM.EvaluateExpression(expression, environment);

            vmEvalCache.Clear();

            if (evalResult.IsOkOrNull() is { } evalOk)
            {
                evalCache[(expression, environment)] = evalOk;

                System.Threading.Tasks.Task.Run(() =>
                {
                    try
                    {
                        using var stream = new System.IO.MemoryStream();

                        PineValueBinaryEncoding.Encode(stream, evalOk);

                        stream.Seek(0, System.IO.SeekOrigin.Begin);

                        var fileContent = stream.ToArray();

                        fileStore.SetFileContent([fileName], fileContent);
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("Failed to write cache file: " + e);
                    }
                });
            }

            return evalResult;
        }

        public string FileNameFromKey(Expression expression, PineValue environment)
        {
            var exprHash =
                encodedExprCache.GetOrAdd(
                    expression,
                    valueFactory:
                    expr => ComputeHash(ExpressionEncoding.EncodeExpressionAsValue(expr)));

            var envHash = ComputeHash(environment);

            return
                Convert.ToHexStringLower(exprHash[..8].Span) + "_x_" +
                Convert.ToHexStringLower(envHash[..8].Span);
        }

        readonly ConcurrentDictionary<Expression, ReadOnlyMemory<byte>> encodedExprCache = new();

        readonly ConcurrentDictionary<PineValue, ReadOnlyMemory<byte>> valueHashCache = new();

        public ReadOnlyMemory<byte> ComputeHash(PineValue pineValue)
        {
            {
                if (valueHashCache.TryGetValue(pineValue, out var hash))
                {
                    return hash;
                }
            }

            {
                var hash = PineValueHashTree.ComputeHash(pineValue, other => ComputeHash(other));

                valueHashCache[pineValue] = hash;

                return hash;
            }
        }
    }
}
