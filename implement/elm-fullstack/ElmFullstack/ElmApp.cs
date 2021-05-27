using System;
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

    public class ElmApp
    {
        static public (IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> compiledAppFiles, IImmutableList<CompilationIterationReport> iterationsReports) AsCompletelyLoweredElmApp(
            IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> sourceFiles,
            ElmAppInterfaceConfig interfaceConfig) =>
                AsCompletelyLoweredElmApp(
                    sourceFiles,
                    rootModuleName: interfaceConfig.RootModuleName.Split('.').ToImmutableList(),
                    interfaceToHostRootModuleName: InterfaceToHostRootModuleName.Split('.').ToImmutableList());

        static (IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>, IImmutableList<CompilationIterationReport>) AsCompletelyLoweredElmApp(
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

            var (compilationResult, compilationReport) = ElmAppCompilation(
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
                .Where(error => error?.MissingDependencyError?.FirstOrDefault().ElmMakeDependency != null)
                .ToImmutableList();

            var otherErrors =
                compilationErrors.Except(compilationErrorsMissingElmMakeDependencies)
                .ToImmutableList();

            if (0 < otherErrors.Count)
            {
                var otherErrorsText =
                    string.Join("\n", otherErrors.Select(DescribeCompilationError));

                throw new Exception("Failed compilation with " + compilationErrors.Count + " errors:\n" + otherErrorsText);
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
                        error.MissingDependencyError.FirstOrDefault();

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

        static (ElmValueCommonJson.Result<IReadOnlyList<CompilerSerialInterface.CompilationError>, ImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>>, CompilationIterationCompilationReport report) ElmAppCompilation(
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

            serializeStopwatch.Stop();

            var prepareJsEngineStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var jsEngine = jsEngineToCompileElmApp.Value;

            prepareJsEngineStopwatch.Stop();

            var inJsEngineStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var responseJson =
                jsEngine.CallFunction("lowerSerialized", argumentsJson)
                ?.ToString();

            inJsEngineStopwatch.Stop();

            var deserializeStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var withFilesAsList =
                Newtonsoft.Json.JsonConvert.DeserializeObject<ElmValueCommonJson.Result<IReadOnlyList<CompilerSerialInterface.CompilationError>, IReadOnlyList<CompilerSerialInterface.AppCodeEntry>>>(responseJson);

            var mappedResult =
                withFilesAsList.map(files =>
                    files.ToImmutableDictionary(
                        entry => (IImmutableList<string>)entry.path.ToImmutableList(),
                        entry => (IReadOnlyList<byte>)Convert.FromBase64String(entry.content.AsBase64))
                    .WithComparers(EnumerableExtension.EqualityComparer<string>()));

            return
                (mappedResult,
                new CompilationIterationCompilationReport
                {
                    serializeTimeSpentMilli = (int)serializeStopwatch.ElapsedMilliseconds,
                    prepareJsEngineTimeSpentMilli = (int)prepareJsEngineStopwatch.ElapsedMilliseconds,
                    inJsEngineTimeSpentMilli = (int)inJsEngineStopwatch.ElapsedMilliseconds,
                    deserializeTimeSpentMilli = (int)deserializeStopwatch.ElapsedMilliseconds,
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

        static Lazy<JavaScriptEngineSwitcher.Core.IJsEngine> jsEngineToCompileElmApp = new Lazy<JavaScriptEngineSwitcher.Core.IJsEngine>(PrepareJsEngineToCompileElmApp);

        static public JavaScriptEngineSwitcher.Core.IJsEngine PrepareJsEngineToCompileElmApp()
        {
            var javascript = JavascriptToCompileElmApp.Value;

            var javascriptEngine = ProcessHostedWithV8.ConstructJsEngine();

            javascriptEngine.Evaluate(javascript);

            return javascriptEngine;
        }

        static readonly Lazy<string> JavascriptToCompileElmApp = new Lazy<string>(BuildJavascriptToCompileElmApp);

        static string BuildJavascriptToCompileElmApp()
        {
            var javascriptFromElmMake =
                ProcessFromElm019Code.CompileElmToJavascript(
                    CompileElmProgramAppCodeFiles(),
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

        static public IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> CompileElmProgramAppCodeFiles() =>
            ImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>.Empty
            .WithComparers(EnumerableExtension.EqualityComparer<string>())
            .SetItem(ImmutableList.Create("elm.json"), GetManifestResourceStreamContent("elm_fullstack.ElmFullstack.compile_elm_program.elm.json"))
            .SetItem(ImmutableList.Create("src", "CompileFullstackApp.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmFullstack.compile_elm_program.src.CompileFullstackApp.elm"))
            .SetItem(ImmutableList.Create("src", "Main.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmFullstack.compile_elm_program.src.Main.elm"));

        static byte[] GetManifestResourceStreamContent(string name)
        {
            using var stream = typeof(ElmApp).Assembly.GetManifestResourceStream(name);
            using var memoryStream = new System.IO.MemoryStream();

            stream.CopyTo(memoryStream);

            return memoryStream.ToArray();
        }

        static string DescribeCompilationError(CompilerSerialInterface.CompilationError compilationError) =>
            Newtonsoft.Json.JsonConvert.SerializeObject(compilationError, Newtonsoft.Json.Formatting.Indented);

        public class CompilationIterationReport
        {
            public CompilationIterationCompilationReport compilation;

            public IReadOnlyList<CompilationIterationDependencyReport> dependenciesReports;

            public int totalTimeSpentMilli;
        }

        public class CompilationIterationCompilationReport
        {
            public int serializeTimeSpentMilli;

            public int prepareJsEngineTimeSpentMilli;

            public int inJsEngineTimeSpentMilli;

            public int deserializeTimeSpentMilli;

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

        struct DependencyKey
        {
            public IReadOnlyList<ElmMakeRequestStructure> ElmMakeDependency;
        }

        class ElmMakeRequestStructure
        {
            public IReadOnlyList<CompilerSerialInterface.AppCodeEntry> files;

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