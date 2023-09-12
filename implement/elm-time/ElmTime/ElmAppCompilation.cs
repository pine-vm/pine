using ElmTime.Elm019;
using ElmTime.JavaScript;
using Pine;
using Pine.Json;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

using CompilationResult =
    Pine.Result<
        System.Collections.Generic.IReadOnlyList<ElmTime.ElmAppCompilation.LocatedCompilationError>,
        ElmTime.ElmAppCompilation.CompilationSuccess>;

using CompilationIterationResult =
    Pine.Result<
        System.Collections.Generic.IReadOnlyList<ElmTime.CompilerSerialInterface.LocatedCompilationError>,
        ElmTime.ElmAppCompilation.CompilationIterationSuccess>;

namespace ElmTime
{
    public record ElmAppInterfaceConfig(IReadOnlyList<string> compilationRootFilePath)
    {
        public static ElmAppInterfaceConfig Default => new
        (
            compilationRootFilePath: ["src", "Backend", "Main.elm"]
        );
    }

    public struct ElmAppInterfaceConvention
    {
        public const string InitialStateFunctionName = "interfaceToHost_initState";

        public const string ProcessSerializedEventFunctionName = "interfaceToHost_processEvent";

        public static ImmutableList<string> CompilationInterfaceModuleNamePrefixes => ["CompilationInterface"];
    }

    public class ElmAppCompilation
    {
        private static readonly System.Diagnostics.Stopwatch cacheItemTimeSource = System.Diagnostics.Stopwatch.StartNew();

        private static readonly ConcurrentDictionary<string, (CompilationResult compilationResult, TimeSpan lastUseTime)> ElmAppCompilationCache = new();

        private static void ElmAppCompilationCacheRemoveOlderItems(long retainedSizeLimit) =>
            Cache.RemoveItemsToLimitRetainedSize(
                ElmAppCompilationCache,
                item => 1000 + EstimateCacheItemSizeInMemory(item.Value.compilationResult),
                item => item.Value.lastUseTime,
                retainedSizeLimit);

        public static CompilationResult AsCompletelyLoweredElmApp(
            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> sourceFiles,
            IReadOnlyList<string> workingDirectoryRelative,
            ElmAppInterfaceConfig interfaceConfig)
        {
            var sourceFilesHash =
                CommonConversion.StringBase16(PineValueHashTree.ComputeHashSorted(PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(sourceFiles)));

            var compilationHash =
                CommonConversion.StringBase16(CommonConversion.HashSHA256(Encoding.UTF8.GetBytes(
                    System.Text.Json.JsonSerializer.Serialize(new
                    {
                        sourceFilesHash,
                        interfaceConfig,
                    }))));

            CompilationResult compileNew() =>
                AsCompletelyLoweredElmApp(
                    sourceFiles,
                    workingDirectoryRelative: workingDirectoryRelative,
                    compilationRootFilePath: interfaceConfig.compilationRootFilePath,
                    interfaceToHostRootModuleName: InterfaceToHostRootModuleName.Split('.').ToImmutableList());

            (CompilationResult compilationResult, TimeSpan lastUseTime) BuildNextCacheEntry(
                CompilationResult? previousEntryCompilationResult)
            {
                if (previousEntryCompilationResult is CompilationResult.Ok ok)
                    return (previousEntryCompilationResult, cacheItemTimeSource.Elapsed);

                return (compileNew(), cacheItemTimeSource.Elapsed);
            }

            var (compilationResult, _) =
                ElmAppCompilationCache.AddOrUpdate(
                    compilationHash,
                    _ => BuildNextCacheEntry(null),
                    (_, previousEntry) => BuildNextCacheEntry(previousEntry.compilationResult));

            ElmAppCompilationCacheRemoveOlderItems(50_000_000);

            return compilationResult;
        }

        public record CompilationSuccess(
            CompilationIterationSuccess result,
            IImmutableList<CompilationIterationReport> iterationsReports);

        public record CompilationIterationSuccess(
            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> compiledFiles,
            Result<string, CompilerSerialInterface.ElmMakeEntryPointKind> rootModuleEntryPointKind);

        public record StackFrame(
            IImmutableList<(CompilerSerialInterface.DependencyKey key, ReadOnlyMemory<byte> value)> discoveredDependencies,
            CompilationIterationReport iterationReport);

        private static CompilationResult AsCompletelyLoweredElmApp(
            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> sourceFiles,
            IReadOnlyList<string> workingDirectoryRelative,
            IReadOnlyList<string> compilationRootFilePath,
            IReadOnlyList<string> interfaceToHostRootModuleName) =>
            AsCompletelyLoweredElmApp(
                sourceFiles,
                workingDirectoryRelative: workingDirectoryRelative,
                compilationRootFilePath: compilationRootFilePath,
                interfaceToHostRootModuleName,
                ImmutableStack<StackFrame>.Empty);

        private static CompilationResult AsCompletelyLoweredElmApp(
            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> sourceFiles,
            IReadOnlyList<string> workingDirectoryRelative,
            IReadOnlyList<string> compilationRootFilePath,
            IReadOnlyList<string> interfaceToHostRootModuleName,
            IImmutableStack<StackFrame> stack)
        {
            if (10 < stack.Count())
                throw new Exception("Iteration stack depth > 10");

            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var dependencies =
                stack.SelectMany(frame => frame.discoveredDependencies)
                .ToImmutableList();

            var compilerElmProgramCodeFiles =
                CachedCompilerElmProgramCodeFilesForElmBackend.Value
                .Extract(error => throw new Exception(nameof(CachedCompilerElmProgramCodeFilesForElmBackend) + ": " + error));

            var (compilationResult, compilationReport) = CachedElmAppCompilationIteration(
                compilerElmProgramCodeFiles: compilerElmProgramCodeFiles,
                sourceFiles: sourceFiles,
                compilationRootFilePath: compilationRootFilePath,
                interfaceToHostRootModuleName: interfaceToHostRootModuleName,
                dependencies: dependencies);

            var currentIterationReport = new CompilationIterationReport
            (
                compilation: compilationReport,
                dependenciesReports: null,
                totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds
            );

            return
                compilationResult
                .Unpack(
                    fromOk: compilationSuccess =>
                    {
                        return
                            CompilationResult.ok(
                                new CompilationSuccess(
                                    result: compilationSuccess,
                                    stack.Select(frame => frame.iterationReport).ToImmutableList().Add(currentIterationReport)));
                    },
                    fromErr: compilationErrors =>
                    {
                        var compilationErrorsMissingElmMakeDependencies =
                            compilationErrors
                            .SelectMany(error =>
                            {
                                var dependencyKey = error.error?.MissingDependencyError?.FirstOrDefault();

                                if (dependencyKey != null)
                                    return ImmutableList.Create((error, dependencyKey));

                                return [];
                            })
                            .ToImmutableList();

                        var otherErrors =
                            compilationErrors.Except(compilationErrorsMissingElmMakeDependencies.Select(e => e.error))
                            .ToImmutableList();

                        if (0 < otherErrors.Count)
                        {
                            return CompilationResult.err(
                                otherErrors.Select(error => new LocatedCompilationError(error.location, error: CompilationError.AsCompilationError(error.error))).ToImmutableList());
                        }

                        Result<string, ReadOnlyMemory<byte>> ElmMake(
                            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> elmCodeFiles,
                            IImmutableList<string> pathToFileWithElmEntryPoint,
                            bool makeJavascript,
                            bool enableDebug)
                        {
                            var elmMakeCommandAppendix =
                                enableDebug ? "--debug" : null;

                            return
                            Elm019Binaries.ElmMake(
                                elmCodeFiles,
                                workingDirectoryRelative: workingDirectoryRelative,
                                pathToFileWithElmEntryPoint: pathToFileWithElmEntryPoint,
                                outputFileName: "output." + (makeJavascript ? "js" : "html"),
                                elmMakeCommandAppendix: elmMakeCommandAppendix)
                            .Map(makeOk => makeOk.producedFile);
                        }

                        var newDependencies =
                            compilationErrorsMissingElmMakeDependencies
                            .Select(error =>
                            {
                                var dependencyTotalStopwatch = System.Diagnostics.Stopwatch.StartNew();

                                TimedReport<CompilationIterationDependencyReport> completeDependencyReport(CompilationIterationDependencyReport dependencyReport) =>
                                    new(report: dependencyReport, totalTimeSpentMilli: (int)dependencyTotalStopwatch.ElapsedMilliseconds);

                                var dependencyKey = error.dependencyKey;

                                var elmMakeRequest = dependencyKey.ElmMakeDependency?.FirstOrDefault();

                                if (elmMakeRequest != null)
                                {
                                    Result<string, ReadOnlyMemory<byte>> buildResultValue()
                                    {
                                        try
                                        {
                                            var elmMakeRequestFiles =
                                                elmMakeRequest.files
                                                .ToImmutableDictionary(
                                                    entry => entry.path.ToImmutableList(),
                                                    entry => (ReadOnlyMemory<byte>)Convert.FromBase64String(entry.content.AsBase64),
                                                    keyComparer: EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

                                            return
                                            ElmMake(
                                                elmCodeFiles: elmMakeRequestFiles,
                                                pathToFileWithElmEntryPoint: elmMakeRequest.entryPointFilePath.ToImmutableList(),
                                                makeJavascript: elmMakeRequest.outputType.ElmMakeOutputTypeJs != null,
                                                enableDebug: elmMakeRequest.enableDebug);
                                        }
                                        catch (Exception e)
                                        {
                                            return Result<string, ReadOnlyMemory<byte>>.err("Failed with runtime exception: " + e);
                                        }
                                    }

                                    return (
                                        (key: dependencyKey, result: buildResultValue()),
                                        completeDependencyReport(new CompilationIterationDependencyReport(dependencyKeySummary: "ElmMake")));
                                }

                                throw new Exception("Protocol error: Unknown type of dependency: " + DescribeCompilationError(error.error));
                            })
                            .ToImmutableList();

                        currentIterationReport =
                            currentIterationReport with
                            {
                                dependenciesReports = newDependencies.Select(depAndReport => depAndReport.Item2).ToImmutableList(),
                                totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds
                            };

                        var newDependenciesWithError =
                            newDependencies.Where(dep => !dep.Item1.result.IsOk()).ToImmutableList();

                        /*
                         * TODO: Instead of returning here, probably forward the result back into the compiler for packaging, for example adding location info.
                         * This implies expansion of the dependencies API to model the error cases.
                         * */
                        if (0 < newDependenciesWithError.Count)
                        {
                            return CompilationResult.err(
                                newDependenciesWithError.Select(dep => new LocatedCompilationError(
                                    location: null,
                                    error: new CompilationError(
                                        DependencyError:
                                        dep.Item2.report.dependencyKeySummary + " " +
                                        dep.Item1.result.Unpack(fromErr: error => error, fromOk: _ => throw new NotImplementedException()))))
                                .ToImmutableList());
                        }

                        var newStackFrame =
                            new StackFrame(
                                discoveredDependencies:
                                newDependencies.Select(depAndReport =>
                                (depAndReport.Item1.key, depAndReport.Item1.result.Extract(error => throw new Exception(error)))).ToImmutableList(),
                                iterationReport: currentIterationReport);

                        return AsCompletelyLoweredElmApp(
                            sourceFiles: sourceFiles,
                            workingDirectoryRelative: workingDirectoryRelative,
                            compilationRootFilePath: compilationRootFilePath,
                            interfaceToHostRootModuleName: interfaceToHostRootModuleName,
                            stack: stack.Push(newStackFrame));
                    });
        }

        private static readonly ConcurrentDictionary<string, (CompilationIterationResult compilationResult, TimeSpan lastUseTime)> ElmAppCompilationIterationCache = new();

        private static void ElmAppCompilationIterationCacheRemoveOlderItems(long retainedSizeLimit) =>
            Cache.RemoveItemsToLimitRetainedSize(
                ElmAppCompilationIterationCache,
                item => 1000 + EstimateCacheItemSizeInMemory(item.Value.compilationResult),
                item => item.Value.lastUseTime,
                retainedSizeLimit);

        private static (CompilationIterationResult, CompilationIterationCompilationReport report) CachedElmAppCompilationIteration(
            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> compilerElmProgramCodeFiles,
            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> sourceFiles,
            IReadOnlyList<string> compilationRootFilePath,
            IReadOnlyList<string> interfaceToHostRootModuleName,
            IReadOnlyList<(CompilerSerialInterface.DependencyKey key, ReadOnlyMemory<byte> value)> dependencies)
        {
            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();
            var serializeStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var sourceFilesJson =
                sourceFiles
                .Select(appCodeFile => new CompilerSerialInterface.AppCodeEntry
                (
                    path: appCodeFile.Key,
                    content: CompilerSerialInterface.BytesJson.AsJson(appCodeFile.Value)
                ))
                .ToImmutableList();

            var dependenciesJson =
                dependencies
                .Select(dependency =>
                new
                {
                    key = dependency.key,
                    value = CompilerSerialInterface.BytesJson.AsJson(dependency.value),
                })
                .ToImmutableList();

            var argumentsJson = System.Text.Json.JsonSerializer.Serialize(
                new
                {
                    sourceFiles = sourceFilesJson,
                    compilationInterfaceElmModuleNamePrefixes = ElmAppInterfaceConvention.CompilationInterfaceModuleNamePrefixes,
                    dependencies = dependenciesJson,
                    compilationRootFilePath = compilationRootFilePath,
                    interfaceToHostRootModuleName = interfaceToHostRootModuleName,
                }
            );

            var argumentsJsonHash =
                CommonConversion.StringBase16(CommonConversion.HashSHA256(Encoding.UTF8.GetBytes(argumentsJson)));

            serializeStopwatch.Stop();

            System.Diagnostics.Stopwatch? prepareJavaScriptEngineStopwatch = null;
            System.Diagnostics.Stopwatch? inJavaScriptEngineStopwatch = null;
            System.Diagnostics.Stopwatch? deserializeStopwatch = null;

            CompilationIterationResult compileNew()
            {
                prepareJavaScriptEngineStopwatch = System.Diagnostics.Stopwatch.StartNew();

                var javaScriptEngine = CachedJavaScriptEngineToCompileFileTree(compilerElmProgramCodeFiles);

                prepareJavaScriptEngineStopwatch.Stop();

                inJavaScriptEngineStopwatch = System.Diagnostics.Stopwatch.StartNew();

                var responseJson =
                    javaScriptEngine.CallFunction("lowerSerialized", argumentsJson)
                    .ToString()!;

                inJavaScriptEngineStopwatch.Stop();

                deserializeStopwatch = System.Diagnostics.Stopwatch.StartNew();

                var compilationResponse =
                    System.Text.Json.JsonSerializer.Deserialize<
                        Result<
                            string,
                            Result<
                                IReadOnlyList<CompilerSerialInterface.LocatedCompilationError>,
                                CompilerSerialInterface.CompilationIterationSuccess>>>(responseJson)!;

                var compilationResponseOk =
                    compilationResponse
                    .Extract(error => throw new Exception("Protocol error: " + error));

                var mappedResult =
                    compilationResponseOk.Map(compileSuccess =>
                    new CompilationIterationSuccess(compiledFiles:
                        compileSuccess.compiledFiles.ToImmutableDictionary(
                            entry => entry.path,
                            entry => (ReadOnlyMemory<byte>)Convert.FromBase64String(entry.content.AsBase64),
                            keyComparer: EnumerableExtension.EqualityComparer<IReadOnlyList<string>>()),
                            rootModuleEntryPointKind: compileSuccess.rootModuleEntryPointKind));

                return mappedResult;
            }

            var result =
                ElmAppCompilationIterationCache.AddOrUpdate(
                    argumentsJsonHash,
                    _ => (compileNew(), cacheItemTimeSource.Elapsed),
                    (_, previousEntry) => (previousEntry.compilationResult, cacheItemTimeSource.Elapsed));

            ElmAppCompilationIterationCacheRemoveOlderItems(50_000_000);

            return
                (result.compilationResult,
                new CompilationIterationCompilationReport
                (
                    serializeTimeSpentMilli: (int)serializeStopwatch.ElapsedMilliseconds,
                    prepareJavaScriptEngineTimeSpentMilli: (int?)prepareJavaScriptEngineStopwatch?.ElapsedMilliseconds,
                    argumentsJsonHash: argumentsJsonHash,
                    argumentsToJavaScriptEngineSerializedLength: argumentsJson.Length,
                    inJavaScriptEngineTimeSpentMilli: (int?)inJavaScriptEngineStopwatch?.ElapsedMilliseconds,
                    deserializeTimeSpentMilli: (int?)deserializeStopwatch?.ElapsedMilliseconds,
                    totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds
                ));
        }

        public static ImmutableList<string> FilePathFromModuleName(IReadOnlyList<string> moduleName)
        {
            var fileName = moduleName.Last() + ".elm";
            var directoryNames = moduleName.Reverse().Skip(1).Reverse();

            return ["src", .. directoryNames, fileName];
        }

        public static IImmutableList<string> FilePathFromModuleName(string moduleName) =>
            FilePathFromModuleName(moduleName.Split('.'));

        public static string InterfaceToHostRootModuleName => "Backend.InterfaceToHost_Root";

        public static IJavaScriptEngine CachedJavaScriptEngineToCompileFileTree(
            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> compilerElmProgramCodeFiles)
        {
            var compilerId =
                CommonConversion.StringBase16(
                    PineValueHashTree.ComputeHashSorted(
                        PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(compilerElmProgramCodeFiles)));

            return FileTreeCompilerJavaScriptEngineCache.GetOrAdd(
                compilerId,
                _ => CreateJavaScriptEngineToCompileFileTree(compilerElmProgramCodeFiles));
        }

        private static readonly ConcurrentDictionary<string, IJavaScriptEngine> FileTreeCompilerJavaScriptEngineCache = new();

        public static IJavaScriptEngine CreateJavaScriptEngineToCompileFileTree(
            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> compilerElmProgramCodeFiles)
        {
            var javascript = BuildJavascriptToCompileFileTree(compilerElmProgramCodeFiles);

            var javascriptEngine = IJavaScriptEngine.BuildJavaScriptEngine();

            javascriptEngine.Evaluate(javascript);

            return javascriptEngine;
        }

        private static string BuildJavascriptToCompileFileTree(
            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> compilerElmProgramCodeFiles)
        {
            var elmMakeResult =
                Elm019Binaries.ElmMakeToJavascript(
                    compilerElmProgramCodeFiles,
                    workingDirectoryRelative: null,
                    ["src", "Main.elm"]);

            var javascriptFromElmMake =
                Encoding.UTF8.GetString(
                    elmMakeResult.Extract(err => throw new Exception("Failed elm make: " + err)).producedFile.Span);

            var javascriptMinusCrashes = ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

            var listFunctionToPublish =
                new[]
                {
                    (functionNameInElm: "Main.lowerSerialized",
                    publicName: "lowerSerialized",
                    arity: 1),
                };

            return
                ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                    javascriptMinusCrashes,
                    listFunctionToPublish);
        }

        public static readonly Lazy<Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>> CachedCompilerElmProgramCodeFilesForElmBackend =
            new(LoadCompilerElmProgramCodeFilesForElmBackend);

        public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadCompilerElmProgramCodeFilesForElmBackend() =>
            ElmInteractive.ElmInteractive.LoadCompileElmProgramCodeFiles();

        public static string CompileCompilationErrorsDisplayText(IReadOnlyList<LocatedCompilationError>? compilationErrors)
        {
            var errorsText =
                compilationErrors == null ? null :
                string.Join("\n\n", compilationErrors.Select(DescribeCompilationError));

            return "Compilation failed with " + compilationErrors?.Count + " error" + (compilationErrors?.Count == 1 ? "" : "s") + ":\n\n" + errorsText;
        }

        public static string DescribeCompilationError(LocatedCompilationError locatedCompilationError) =>
            (locatedCompilationError.location == null ? "without location information" :
            "in file " + string.Join('/', locatedCompilationError.location.filePath)) + ": " +
            DescribeCompilationError(locatedCompilationError.error);

        private static string DescribeCompilationError(CompilerSerialInterface.LocatedCompilationError locatedCompilationError) =>
            "in file " + string.Join('/', locatedCompilationError.location.filePath) + ": " +
            DescribeCompilationError(locatedCompilationError.error);

        private static string DescribeCompilationError(CompilationError compilationError)
        {
            if (compilationError.OtherError != null)
                return compilationError.OtherError;

            if (compilationError.DependencyError != null)
                return "Dependency Error: " + compilationError.DependencyError;

            throw new Exception("Unexpected shape of value " +
                System.Text.Json.JsonSerializer.Serialize(
                    compilationError,
                    new System.Text.Json.JsonSerializerOptions
                    {
                        DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull,
                        WriteIndented = true
                    }));
        }

        private static string DescribeCompilationError(CompilerSerialInterface.CompilationError compilationError)
        {
            var otherCompilationError =
                compilationError?.OtherCompilationError?.FirstOrDefault();

            if (otherCompilationError != null)
                return otherCompilationError;

            return
                System.Text.Json.JsonSerializer.Serialize(
                    compilationError,
                    new System.Text.Json.JsonSerializerOptions
                    {
                        DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull,
                        WriteIndented = true
                    });
        }

        private static long EstimateCacheItemSizeInMemory(
            CompilationIterationResult item) =>
            item.Unpack(
                fromErr: err => err.Sum(EstimateCacheItemSizeInMemory),
                fromOk: EstimateCacheItemSizeInMemory);

        private static long EstimateCacheItemSizeInMemory(
            CompilationIterationSuccess compilationIterationSuccess) =>
            EstimateCacheItemSizeInMemory(compilationIterationSuccess.compiledFiles);

        private static long EstimateCacheItemSizeInMemory(CompilerSerialInterface.LocatedCompilationError compilationError) =>
            100 + (compilationError.location?.filePath.Sum(e => e.Length) ?? 0) + EstimateCacheItemSizeInMemory(compilationError.error);

        private static long EstimateCacheItemSizeInMemory(CompilerSerialInterface.CompilationError compilationError) =>
            compilationError?.MissingDependencyError?.Sum(EstimateCacheItemSizeInMemory) ?? 0;

        private static long EstimateCacheItemSizeInMemory(CompilationResult item) =>
            item.Unpack(
                fromErr: errors => errors.Sum(EstimateCacheItemSizeInMemory),
                fromOk: EstimateCacheItemSizeInMemory);

        private static long EstimateCacheItemSizeInMemory(CompilationSuccess compilationSuccess) =>
            EstimateCacheItemSizeInMemory(compilationSuccess.result);

        private static long EstimateCacheItemSizeInMemory(IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> item) =>
            item?.Sum(file => file.Key.Sum(e => e.Length) + file.Value.Length) ?? 0;

        private static long EstimateCacheItemSizeInMemory(LocatedCompilationError compilationError) =>
            100 + (compilationError.location?.filePath.Sum(e => e.Length) ?? 0) + EstimateCacheItemSizeInMemory(compilationError.error);

        private static long EstimateCacheItemSizeInMemory(CompilationError compilationError) =>
            compilationError.DependencyError != null ? compilationError.DependencyError.Length * 2 : 0;

        private static long EstimateCacheItemSizeInMemory(CompilerSerialInterface.DependencyKey dependencyKey) =>
            dependencyKey.ElmMakeDependency?.Sum(EstimateCacheItemSizeInMemory) ?? 0;

        private static long EstimateCacheItemSizeInMemory(CompilerSerialInterface.ElmMakeRequestStructure elmMakeRequest) =>
            elmMakeRequest?.files?.Sum(file => file.content.AsBase64.Length) ?? 0;

        public record CompilationIterationReport(
            CompilationIterationCompilationReport compilation,
            IReadOnlyList<TimedReport<CompilationIterationDependencyReport>>? dependenciesReports,
            int? totalTimeSpentMilli);

        public record CompilationIterationCompilationReport(
            int serializeTimeSpentMilli,
            int? prepareJavaScriptEngineTimeSpentMilli,
            string? argumentsJsonHash,
            int? argumentsToJavaScriptEngineSerializedLength,
            int? inJavaScriptEngineTimeSpentMilli,
            int? deserializeTimeSpentMilli,
            int totalTimeSpentMilli);

        public record CompilationIterationDependencyReport(string dependencyKeySummary);

        public record TimedReport<T>(T report, int totalTimeSpentMilli);

        public record CompilationError(
            string? DependencyError = null,
            string? OtherError = null)
        {
            public static CompilationError AsCompilationError(CompilerSerialInterface.CompilationError compilationError)
            {
                var missingDependencyError = compilationError.MissingDependencyError?.FirstOrDefault();

                if (missingDependencyError != null)
                    return new CompilationError(DependencyError: "Missing dependency: " + DescribeCompilationError(compilationError));

                return new CompilationError(OtherError: compilationError.OtherCompilationError?.FirstOrDefault());
            }
        }

        public record LocatedCompilationError(CompilerSerialInterface.LocationInSourceFiles? location, CompilationError error);

        public static Result<string, IReadOnlyList<string>> ParseModuleNameFromElmModuleText(string elmModuleText)
        {
            var moduleDeclarationMatch = Regex.Match(elmModuleText, "^module\\s+([^\\s]+)");

            if (!moduleDeclarationMatch.Success)
                return Result<string, IReadOnlyList<string>>.err("Did not find module declaration");

            return
                Result<string, IReadOnlyList<string>>.ok(moduleDeclarationMatch.Groups[1].Value.Split('.'));
        }
    }

    namespace CompilerSerialInterface
    {
        public record CompilationIterationSuccess(
            IReadOnlyList<AppCodeEntry> compiledFiles,
            Result<string, ElmMakeEntryPointKind> rootModuleEntryPointKind);

        [System.Text.Json.Serialization.JsonConverter(typeof(JsonConverterForChoiceType))]
        public abstract record ElmMakeEntryPointKind
        {
            public record ClassicMakeEntryPoint(
                ElmMakeEntryPointStruct EntryPointStruct)
                : ElmMakeEntryPointKind;

            public record BlobMakeEntryPoint(
                ElmMakeEntryPointStruct EntryPointStruct)
                : ElmMakeEntryPointKind;
        }

        public record ElmMakeEntryPointStruct(string elmMakeJavaScriptFunctionName);

        public record CompilationError(
            IReadOnlyList<string>? OtherCompilationError = null,
            IReadOnlyList<DependencyKey>? MissingDependencyError = null);

        public record LocatedInSourceFilesRecord(LocationInSourceFiles location);

        public record LocatedCompilationError(LocationInSourceFiles location, CompilationError error);

        public record LocationInSourceFiles(IReadOnlyList<string> filePath);

        public record DependencyKey(IReadOnlyList<ElmMakeRequestStructure> ElmMakeDependency);

        public record ElmMakeRequestStructure(
            IReadOnlyList<AppCodeEntry> files,
            IReadOnlyList<string> entryPointFilePath,
            ElmMakeOutputType outputType,
            bool enableDebug);

        public record ElmMakeOutputType(
            IReadOnlyList<object>? ElmMakeOutputTypeHtml = null,
            IReadOnlyList<object>? ElmMakeOutputTypeJs = null);

        public record AppCodeEntry(IReadOnlyList<string> path, BytesJson content);

        public record BytesJson(string AsBase64)
        {
            public static BytesJson AsJson(ReadOnlyMemory<byte> bytes) =>
                new(AsBase64: Convert.ToBase64String(bytes.Span));
        }
    }
}