using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Pine;

namespace ElmFullstack
{
    public struct ElmAppInterfaceConfig
    {
        public string RootModuleName;

        static public ElmAppInterfaceConfig Default => new ElmAppInterfaceConfig
        {
            RootModuleName = "Backend.Main"
        };
    }

    public struct ElmAppInterfaceConvention
    {
        public const string InitialStateFunctionName = "interfaceToHost_initState";

        public const string ProcessSerializedEventFunctionName = "interfaceToHost_processEvent";

        public const string SerializeStateFunctionName = "interfaceToHost_serializeState";

        public const string DeserializeStateFunctionName = "interfaceToHost_deserializeState";

        static public IImmutableList<string> CompilationInterfaceModuleNamePrefixes => ImmutableList.Create("ElmFullstackCompilerInterface", "CompilationInterface");
    }

    public class ElmAppCompilation
    {
        static readonly System.Diagnostics.Stopwatch cacheItemTimeSource = System.Diagnostics.Stopwatch.StartNew();

        static readonly ConcurrentDictionary<string, (IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> compilationResult, TimeSpan lastUseTime)> ElmAppCompilationCache =
            new ConcurrentDictionary<string, (IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>, TimeSpan)>();

        static void ElmAppCompilationCacheRemoveOlderItems(long retainedSizeLimit) =>
            Cache.RemoveItemsToLimitRetainedSize(
                ElmAppCompilationCache,
                item => 1000 + EstimateCacheItemSizeInMemory(item.Value.compilationResult),
                item => item.Value.lastUseTime,
                retainedSizeLimit);

        static public (IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> compiledAppFiles, IImmutableList<CompilationIterationReport> iterationsReports) AsCompletelyLoweredElmApp(
            IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> sourceFiles,
            ElmAppInterfaceConfig interfaceConfig)
        {
            var sourceFilesHash =
                CommonConversion.StringBase16FromByteArray(Composition.GetHash(Composition.SortedTreeFromSetOfBlobsWithStringPath(sourceFiles)));

            var compilationHash =
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(Encoding.UTF8.GetBytes(
                    Newtonsoft.Json.JsonConvert.SerializeObject(new
                    {
                        sourceFilesHash,
                        interfaceConfig,
                    }))));

            IImmutableList<CompilationIterationReport> newCompilationIterationsReports = null;

            IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> compileNew()
            {
                var (compiledAppFiles, iterationsReports) =
                    AsCompletelyLoweredElmApp(
                        sourceFiles,
                        rootModuleName: interfaceConfig.RootModuleName.Split('.').ToImmutableList(),
                        interfaceToHostRootModuleName: InterfaceToHostRootModuleName.Split('.').ToImmutableList());

                newCompilationIterationsReports = iterationsReports;

                return compiledAppFiles;
            }

            var (compilationResult, _) =
                ElmAppCompilationCache.AddOrUpdate(
                    compilationHash,
                    _ => (compileNew(), cacheItemTimeSource.Elapsed),
                    (_, previousEntry) => (previousEntry.compilationResult, cacheItemTimeSource.Elapsed));

            ElmAppCompilationCacheRemoveOlderItems(50_000_000);

            return (compilationResult, newCompilationIterationsReports);
        }

        static (IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> compiledAppFiles, IImmutableList<CompilationIterationReport> iterationsReports) AsCompletelyLoweredElmApp(
            IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> sourceFiles,
            IImmutableList<string> rootModuleName,
            IImmutableList<string> interfaceToHostRootModuleName) =>
            AsCompletelyLoweredElmApp(
                sourceFiles,
                rootModuleName,
                interfaceToHostRootModuleName,
                ImmutableStack<(IImmutableList<(CompilerSerialInterface.DependencyKey key, IReadOnlyList<byte> value)> discoveredDependencies, CompilationIterationReport previousIterationsReports)>.Empty);

        static (IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>, IImmutableList<CompilationIterationReport>) AsCompletelyLoweredElmApp(
            IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> sourceFiles,
            IImmutableList<string> rootModuleName,
            IImmutableList<string> interfaceToHostRootModuleName,
            IImmutableStack<(IImmutableList<(CompilerSerialInterface.DependencyKey key, IReadOnlyList<byte> value)> discoveredDependencies, CompilationIterationReport iterationReport)> stack)
        {
            if (10 < stack.Count())
                throw new Exception("Iteration stack depth > 10");

            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var dependencies =
                stack.SelectMany(frame => frame.discoveredDependencies)
                .ToImmutableList();

            var (compilationResult, compilationReport) = CachedElmAppCompilationIteration(
                compilerElmProgramCodeFiles: CachedCompilerElmProgramCodeFilesForElmFullstackBackend.Value,
                sourceFiles: sourceFiles,
                rootModuleName: rootModuleName,
                interfaceToHostRootModuleName: interfaceToHostRootModuleName,
                dependencies: dependencies);

            var compilationSuccess = compilationResult.Ok?.FirstOrDefault();

            var currentIterationReport = new CompilationIterationReport
            {
                compilation = compilationReport,
            };

            if (compilationSuccess != null)
            {
                currentIterationReport.totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds;

                return (compilationSuccess, stack.Select(frame => frame.iterationReport).ToImmutableList().Add(currentIterationReport));
            }

            var compilationErrors =
                compilationResult.Err?.FirstOrDefault();

            if (compilationErrors == null)
                throw new Exception("Failed compilation: Protocol error: Missing error descriptions.");

            var compilationErrorsMissingElmMakeDependencies =
                compilationErrors
                .Where(error => error.error?.MissingDependencyError?.FirstOrDefault().ElmMakeDependency != null)
                .ToImmutableList();

            var otherErrors =
                compilationErrors.Except(compilationErrorsMissingElmMakeDependencies)
                .ToImmutableList();

            if (0 < otherErrors.Count)
            {
                var otherErrorsText =
                    string.Join("\n", otherErrors.Select(DescribeCompilationError));

                /*
                 * TODO: To clean up the appearance in the (CL) user interface, avoid burying the error message in a large stack trace:
                 * Propagate the errors further without runtime exception.
                 * New C# features might help simplify the syntax to implement this so that implementation might be easier after switching to .NET 6.
                 * Until then, add line breaks to the message to help spot this within a larger output.
                 * */

                throw new Exception("Failed compilation with " + compilationErrors.Count + " errors:\n" + otherErrorsText + "\n\n");
            }

            byte[] ElmMake(
                IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> elmCodeFiles,
                IImmutableList<string> pathToFileWithElmEntryPoint,
                bool makeJavascript,
                bool enableDebug)
            {
                var elmMakeCommandAppendix =
                    enableDebug ? "--debug" : null;

                var fileAsString = ProcessFromElm019Code.CompileElm(
                    elmCodeFiles,
                    pathToFileWithElmEntryPoint: pathToFileWithElmEntryPoint,
                    outputFileName: "output." + (makeJavascript ? "js" : "html"),
                    elmMakeCommandAppendix: elmMakeCommandAppendix);

                return Encoding.UTF8.GetBytes(fileAsString);
            }

            var newDependencies =
                compilationErrorsMissingElmMakeDependencies
                .Select(error =>
                {
                    var dependencyTotalStopwatch = System.Diagnostics.Stopwatch.StartNew();

                    var dependencyKey =
                        error.error.MissingDependencyError.FirstOrDefault();

                    var elmMakeRequest =
                        dependencyKey.ElmMakeDependency.FirstOrDefault();

                    var dependencyReport = new CompilationIterationDependencyReport
                    {
                    };

                    CompilationIterationDependencyReport completeDependencyReport()
                    {
                        dependencyReport.totalTimeSpentMilli = (int)dependencyTotalStopwatch.ElapsedMilliseconds;

                        return dependencyReport;
                    };

                    if (elmMakeRequest != null)
                    {
                        dependencyReport.dependencyKeySummary = "ElmMake";

                        var elmMakeRequestFiles =
                            elmMakeRequest.files
                            .ToImmutableDictionary(
                                entry => (IImmutableList<string>)entry.path.ToImmutableList(),
                                entry => (IReadOnlyList<byte>)Convert.FromBase64String(entry.content.AsBase64))
                            .WithComparers(EnumerableExtension.EqualityComparer<string>());

                        var value = ElmMake(
                            elmCodeFiles: elmMakeRequestFiles,
                            pathToFileWithElmEntryPoint: elmMakeRequest.entryPointFilePath.ToImmutableList(),
                            makeJavascript: elmMakeRequest.outputType.ElmMakeOutputTypeJs != null,
                            enableDebug: elmMakeRequest.enableDebug);

                        return (
                            (key: dependencyKey, (IReadOnlyList<byte>)value),
                            completeDependencyReport());
                    }

                    throw new Exception("Unknown type of dependency: " + DescribeCompilationError(error));
                })
                .ToImmutableList();

            currentIterationReport.dependenciesReports = newDependencies.Select(depAndReport => depAndReport.Item2).ToImmutableList();

            currentIterationReport.totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds;

            var newStackFrame =
                (newDependencies.Select(depAndReport => depAndReport.Item1).ToImmutableList(), currentIterationReport);

            return AsCompletelyLoweredElmApp(
                sourceFiles: sourceFiles,
                rootModuleName,
                interfaceToHostRootModuleName,
                stack: stack.Push(newStackFrame));
        }

        static readonly ConcurrentDictionary<string, (ElmValueCommonJson.Result<IReadOnlyList<CompilerSerialInterface.LocatedCompilationError>, ImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>> compilationResult, TimeSpan lastUseTime)> ElmAppCompilationIterationCache =
            new ConcurrentDictionary<string, (ElmValueCommonJson.Result<IReadOnlyList<CompilerSerialInterface.LocatedCompilationError>, ImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>>, TimeSpan)>();

        static void ElmAppCompilationIterationCacheRemoveOlderItems(long retainedSizeLimit) =>
            Cache.RemoveItemsToLimitRetainedSize(
                ElmAppCompilationIterationCache,
                item => 1000 + EstimateCacheItemSizeInMemory(item.Value.compilationResult),
                item => item.Value.lastUseTime,
                retainedSizeLimit);

        static (ElmValueCommonJson.Result<IReadOnlyList<CompilerSerialInterface.LocatedCompilationError>, ImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>>, CompilationIterationCompilationReport report) CachedElmAppCompilationIteration(
            IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> compilerElmProgramCodeFiles,
            IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> sourceFiles,
            IImmutableList<string> rootModuleName,
            IImmutableList<string> interfaceToHostRootModuleName,
            ImmutableList<(CompilerSerialInterface.DependencyKey key, IReadOnlyList<byte> value)> dependencies)
        {
            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();
            var serializeStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var sourceFilesJson =
                sourceFiles
                .Select(appCodeFile => new CompilerSerialInterface.AppCodeEntry
                {
                    path = appCodeFile.Key,
                    content = new CompilerSerialInterface.BytesJson { AsBase64 = Convert.ToBase64String(appCodeFile.Value.ToArray()) },
                })
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

            var argumentsJson = Newtonsoft.Json.JsonConvert.SerializeObject(
                new
                {
                    sourceFiles = sourceFilesJson,
                    compilationInterfaceElmModuleNamePrefixes = ElmAppInterfaceConvention.CompilationInterfaceModuleNamePrefixes,
                    dependencies = dependenciesJson,
                    rootModuleName = rootModuleName,
                    interfaceToHostRootModuleName = interfaceToHostRootModuleName,
                }
            );

            var argumentsJsonHash =
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(Encoding.UTF8.GetBytes(argumentsJson)));

            serializeStopwatch.Stop();

            System.Diagnostics.Stopwatch prepareJsEngineStopwatch = null;
            System.Diagnostics.Stopwatch inJsEngineStopwatch = null;
            System.Diagnostics.Stopwatch deserializeStopwatch = null;

            ElmValueCommonJson.Result<IReadOnlyList<CompilerSerialInterface.LocatedCompilationError>, ImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>> compileNew()
            {
                prepareJsEngineStopwatch = System.Diagnostics.Stopwatch.StartNew();

                var jsEngine = CachedJsEngineToCompileFileTree(compilerElmProgramCodeFiles);

                prepareJsEngineStopwatch.Stop();

                inJsEngineStopwatch = System.Diagnostics.Stopwatch.StartNew();

                var responseJson =
                    jsEngine.CallFunction("lowerSerialized", argumentsJson)
                    ?.ToString();

                inJsEngineStopwatch.Stop();

                deserializeStopwatch = System.Diagnostics.Stopwatch.StartNew();

                var compilationResponse =
                    Newtonsoft.Json.JsonConvert.DeserializeObject<
                        ElmValueCommonJson.Result<
                            string,
                            ElmValueCommonJson.Result<
                                IReadOnlyList<CompilerSerialInterface.LocatedCompilationError>,
                                IReadOnlyList<CompilerSerialInterface.AppCodeEntry>>>>(responseJson);

                if (compilationResponse.Ok?.FirstOrDefault() == null)
                    throw new Exception("Protocol error: " + compilationResponse.Err);

                var mappedResult =
                    compilationResponse.Ok?.FirstOrDefault().map(files =>
                        files.ToImmutableDictionary(
                            entry => (IImmutableList<string>)entry.path.ToImmutableList(),
                            entry => (IReadOnlyList<byte>)Convert.FromBase64String(entry.content.AsBase64))
                        .WithComparers(EnumerableExtension.EqualityComparer<string>()));

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
                {
                    serializeTimeSpentMilli = (int)serializeStopwatch.ElapsedMilliseconds,
                    prepareJsEngineTimeSpentMilli = (int?)prepareJsEngineStopwatch?.ElapsedMilliseconds,
                    inJsEngineTimeSpentMilli = (int?)inJsEngineStopwatch?.ElapsedMilliseconds,
                    deserializeTimeSpentMilli = (int?)deserializeStopwatch?.ElapsedMilliseconds,
                    totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds,
                });
        }

        static public IImmutableList<string> FilePathFromModuleName(IReadOnlyList<string> moduleName)
        {
            var fileName = moduleName.Last() + ".elm";
            var directoryNames = moduleName.Reverse().Skip(1).Reverse();

            return new[] { "src" }.Concat(directoryNames).Concat(new[] { fileName }).ToImmutableList();
        }

        static public IImmutableList<string> FilePathFromModuleName(string moduleName) =>
            FilePathFromModuleName(moduleName.Split(new[] { '.' }).ToImmutableList());

        static public string InterfaceToHostRootModuleName => "Backend.InterfaceToHost_Root";

        static public JavaScriptEngineSwitcher.Core.IJsEngine CachedJsEngineToCompileFileTree(
            IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> compilerElmProgramCodeFiles)
        {
            var compilerId =
                CommonConversion.StringBase16FromByteArray(
                    Composition.GetHash(
                        Composition.FromTreeWithStringPath(
                            Composition.SortedTreeFromSetOfBlobsWithStringPath(compilerElmProgramCodeFiles))));

            return FileTreeCompilerJsEngineCache.GetOrAdd(
                compilerId,
                _ => CreateJsEngineToCompileFileTree(compilerElmProgramCodeFiles));
        }

        static readonly ConcurrentDictionary<string, JavaScriptEngineSwitcher.Core.IJsEngine> FileTreeCompilerJsEngineCache =
            new ConcurrentDictionary<string, JavaScriptEngineSwitcher.Core.IJsEngine>();

        static public JavaScriptEngineSwitcher.Core.IJsEngine CreateJsEngineToCompileFileTree(
            IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> compilerElmProgramCodeFiles)
        {
            var javascript = BuildJavascriptToCompileFileTree(compilerElmProgramCodeFiles);

            var javascriptEngine = ProcessHostedWithV8.ConstructJsEngine();

            javascriptEngine.Evaluate(javascript);

            return javascriptEngine;
        }

        static string BuildJavascriptToCompileFileTree(IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> compilerElmProgramCodeFiles)
        {
            var javascriptFromElmMake =
                ProcessFromElm019Code.CompileElmToJavascript(
                    compilerElmProgramCodeFiles,
                    ImmutableList.Create("src", "Main.elm"));

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

        static public Lazy<IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>> CachedCompilerElmProgramCodeFilesForElmFullstackBackend =
            new Lazy<IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>>(LoadCompilerElmProgramCodeFilesForElmFullstackBackend);

        static public IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> LoadCompilerElmProgramCodeFilesForElmFullstackBackend() =>
            ImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>.Empty
            .WithComparers(EnumerableExtension.EqualityComparer<string>())
            .SetItem(ImmutableList.Create("elm.json"), GetManifestResourceStreamContent("elm_fullstack.ElmFullstack.compile_elm_program.elm.json"))
            .SetItem(ImmutableList.Create("src", "CompileFullstackApp.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmFullstack.compile_elm_program.src.CompileFullstackApp.elm"))
            .SetItem(ImmutableList.Create("src", "Main.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmFullstack.compile_elm_program.src.Main.elm"));

        static byte[] GetManifestResourceStreamContent(string name)
        {
            using var stream = typeof(ElmAppCompilation).Assembly.GetManifestResourceStream(name);
            using var memoryStream = new System.IO.MemoryStream();

            stream.CopyTo(memoryStream);

            return memoryStream.ToArray();
        }

        static string DescribeCompilationError(CompilerSerialInterface.LocatedCompilationError locatedCompilationError) =>
            "in file " + string.Join('/', locatedCompilationError.location.filePath) + ": " +
            DescribeCompilationError(locatedCompilationError.error);

        static string DescribeCompilationError(CompilerSerialInterface.CompilationError compilationError)
        {
            var otherCompilationError =
                compilationError?.OtherCompilationError?.FirstOrDefault();

            if (otherCompilationError != null)
                return otherCompilationError;

            return
                Newtonsoft.Json.JsonConvert.SerializeObject(
                    compilationError,
                    Newtonsoft.Json.Formatting.Indented,
                    new Newtonsoft.Json.JsonSerializerSettings
                    {
                        NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore,
                        DefaultValueHandling = Newtonsoft.Json.DefaultValueHandling.Include
                    });
        }

        static long EstimateCacheItemSizeInMemory(ElmValueCommonJson.Result<IReadOnlyList<CompilerSerialInterface.LocatedCompilationError>, ImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>> item) =>
            (item.Err?.Sum(err => err.Sum(EstimateCacheItemSizeInMemory)) ?? 0) +
            (item.Ok?.Sum(EstimateCacheItemSizeInMemory) ?? 0);

        static long EstimateCacheItemSizeInMemory(IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> item) =>
            (item?.Sum(file => file.Key.Sum(e => e.Length) + file.Value.Count)) ?? 0;

        static long EstimateCacheItemSizeInMemory(CompilerSerialInterface.LocatedCompilationError compilationError) =>
            100 + compilationError.location.filePath.Sum(e => e.Length) + EstimateCacheItemSizeInMemory(compilationError.error);

        static long EstimateCacheItemSizeInMemory(CompilerSerialInterface.CompilationError compilationError) =>
            compilationError?.MissingDependencyError?.Sum(EstimateCacheItemSizeInMemory) ?? 0;

        static long EstimateCacheItemSizeInMemory(CompilerSerialInterface.DependencyKey dependencyKey) =>
            dependencyKey.ElmMakeDependency?.Sum(EstimateCacheItemSizeInMemory) ?? 0;

        static long EstimateCacheItemSizeInMemory(CompilerSerialInterface.ElmMakeRequestStructure elmMakeRequest) =>
            elmMakeRequest?.files?.Sum(file => file.content.AsBase64.Length) ?? 0;

        public class CompilationIterationReport
        {
            public CompilationIterationCompilationReport compilation;

            public IReadOnlyList<CompilationIterationDependencyReport> dependenciesReports;

            public int totalTimeSpentMilli;
        }

        public class CompilationIterationCompilationReport
        {
            public int serializeTimeSpentMilli;

            public int? prepareJsEngineTimeSpentMilli;

            public int? inJsEngineTimeSpentMilli;

            public int? deserializeTimeSpentMilli;

            public int totalTimeSpentMilli;
        }

        public class CompilationIterationDependencyReport
        {
            public string dependencyKeySummary;

            public int totalTimeSpentMilli;
        }
    }

    namespace CompilerSerialInterface
    {
        class CompilationError
        {
            public IReadOnlyList<string> OtherCompilationError;

            public IReadOnlyList<DependencyKey> MissingDependencyError;
        }

        class LocatedCompilationError
        {
            public LocationInSourceFiles location;

            public CompilationError error;
        }

        class LocationInSourceFiles
        {
            public IReadOnlyList<string> filePath;
        }

        struct DependencyKey
        {
            public IReadOnlyList<ElmMakeRequestStructure> ElmMakeDependency;
        }

        class ElmMakeRequestStructure
        {
            public IReadOnlyList<AppCodeEntry> files;

            public IReadOnlyList<string> entryPointFilePath;

            public ElmMakeOutputType outputType;

            public bool enableDebug;
        }

        struct ElmMakeOutputType
        {
            public IReadOnlyList<object> ElmMakeOutputTypeHtml;

            public IReadOnlyList<object> ElmMakeOutputTypeJs;
        }

        struct AppCodeEntry
        {
            public IReadOnlyList<string> path;

            public BytesJson content;
        }

        struct BytesJson
        {
            public string AsBase64;

            static public BytesJson AsJson(IReadOnlyList<byte> bytes) =>
                new BytesJson { AsBase64 = Convert.ToBase64String(bytes as byte[] ?? bytes.ToArray()) };
        }
    }
}