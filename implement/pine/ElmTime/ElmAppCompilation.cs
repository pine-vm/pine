using ElmTime.Elm019;
using Pine.Core;
using Pine.Json;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

using CompilationResult =
    Pine.Core.Result<
        System.Collections.Generic.IReadOnlyList<ElmTime.ElmAppCompilation.LocatedCompilationError>,
        ElmTime.ElmAppCompilation.CompilationSuccess>;

using CompilationIterationResult =
    Pine.Core.Result<
        System.Collections.Generic.IReadOnlyList<ElmTime.CompilerSerialInterface.LocatedCompilationError>,
        ElmTime.ElmAppCompilation.CompilationIterationSuccess>;

using ElmTime.ElmInteractive;
using Pine.Elm;
using Pine.PineVM;
using Pine.Core.Elm;


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
            var clock = System.Diagnostics.Stopwatch.StartNew();


            var elmCompilerFromBundle =
                BundledElmEnvironments.BundledElmCompilerCompiledEnvValue()
                ??
                throw new Exception("Failed to load Elm compiler from bundle.");

            var elmCompiler =
                ElmCompiler.ElmCompilerFromEnvValue(elmCompilerFromBundle)
                .Extract(err => throw new Exception(err));

            var pineVMCache = new PineVMCache();

            var pineVM =
                new PineVM(evalCache: pineVMCache.EvalCache);

            var parseCache = new PineVMParseCache();

            var elmCompilerCache = new ElmCompilerCache();


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
                    interfaceToHostRootModuleName: [.. InterfaceToHostRootModuleName.Split('.')],
                    elmCompiler,
                    parseCache,
                    pineVM,
                    elmCompilerCache);

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
            IReadOnlyList<string> interfaceToHostRootModuleName,
            ElmCompiler elmCompiler,
            PineVMParseCache parseCache,
            PineVM pineVM,
            ElmCompilerCache elmCompilerCache) =>
            AsCompletelyLoweredElmApp(
                sourceFiles,
                workingDirectoryRelative: workingDirectoryRelative,
                compilationRootFilePath: compilationRootFilePath,
                interfaceToHostRootModuleName,
                ImmutableStack<StackFrame>.Empty,
                elmCompiler,
                parseCache,
                pineVM,
                elmCompilerCache);

        private static CompilationResult AsCompletelyLoweredElmApp(
            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> sourceFiles,
            IReadOnlyList<string> workingDirectoryRelative,
            IReadOnlyList<string> compilationRootFilePath,
            IReadOnlyList<string> interfaceToHostRootModuleName,
            IImmutableStack<StackFrame> stack,
            ElmCompiler elmCompiler,
            PineVMParseCache parseCache,
            PineVM pineVM,
            ElmCompilerCache elmCompilerCache)
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

            var (compilationResult, compilationReport) =
                CachedElmAppCompilationIteration(
                    compilerElmProgramCodeFiles: compilerElmProgramCodeFiles,
                    sourceFiles: sourceFiles,
                    compilationRootFilePath: compilationRootFilePath,
                    interfaceToHostRootModuleName: interfaceToHostRootModuleName,
                    dependencies: dependencies,
                    elmCompiler: elmCompiler,
                    parseCache,
                    pineVM,
                    elmCompilerCache)
                .Extract(error => throw new Exception(error));

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
                            return
                                (CompilationResult)
                                otherErrors.Select(error =>
                                new LocatedCompilationError(
                                    error.location,
                                    error: CompilationError.AsCompilationError(error.error))).ToImmutableList();
                        }

                        Result<string, ReadOnlyMemory<byte>> ElmMake(
                            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> elmCodeFiles,
                            IImmutableList<string> pathToFileWithElmEntryPoint,
                            bool makeJavascript,
                            bool enableDebug,
                            bool enableOptimize)
                        {
                            var elmMakeCommandAppendix = new[]
                            {
                                enableDebug ? "--debug" : null,
                                enableOptimize ? "--optimize" : null
                            }.Where(flag => !string.IsNullOrEmpty(flag))
                            .Aggregate(
                                seed: (string?)null,
                                (current, next) => current + " " + next);

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
                                                    entry => [.. entry.path],
                                                    entry => (ReadOnlyMemory<byte>)Convert.FromBase64String(entry.content.AsBase64),
                                                    keyComparer: EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

                                            return
                                            ElmMake(
                                                elmCodeFiles: elmMakeRequestFiles,
                                                pathToFileWithElmEntryPoint: elmMakeRequest.entryPointFilePath.ToImmutableList(),
                                                makeJavascript: elmMakeRequest.outputType.ElmMakeOutputTypeJs != null,
                                                enableDebug: elmMakeRequest.enableDebug,
                                                enableOptimize: elmMakeRequest.enableOptimize);
                                        }
                                        catch (Exception e)
                                        {
                                            return "Failed with runtime exception: " + e;
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
                                dependenciesReports = [.. newDependencies.Select(depAndReport => depAndReport.Item2)],
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
                            return
                                (CompilationResult)
                                newDependenciesWithError.Select(dep => new LocatedCompilationError(
                                    location: null,
                                    error: new CompilationError(
                                        DependencyError:
                                        dep.Item2.report.dependencyKeySummary + " " +
                                        dep.Item1.result.Unpack(
                                            fromErr: error => error,
                                            fromOk: _ => throw new NotImplementedException()))))
                                .ToImmutableList();
                        }

                        var newStackFrame =
                            new StackFrame(
                                discoveredDependencies:
                                newDependencies.Select(depAndReport =>
                                (depAndReport.Item1.key,
                                depAndReport.Item1.result.Extract(error => throw new Exception(error)))).ToImmutableList(),
                                iterationReport: currentIterationReport);

                        return AsCompletelyLoweredElmApp(
                            sourceFiles: sourceFiles,
                            workingDirectoryRelative: workingDirectoryRelative,
                            compilationRootFilePath: compilationRootFilePath,
                            interfaceToHostRootModuleName: interfaceToHostRootModuleName,
                            stack: stack.Push(newStackFrame),
                            elmCompiler,
                            parseCache,
                            pineVM,
                            elmCompilerCache);
                    });
        }

        private static Result<string, (CompilationIterationResult, CompilationIterationCompilationReport report)>
            CachedElmAppCompilationIteration(
            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> compilerElmProgramCodeFiles,
            IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> sourceFiles,
            IReadOnlyList<string> compilationRootFilePath,
            IReadOnlyList<string> interfaceToHostRootModuleName,
            IReadOnlyList<(CompilerSerialInterface.DependencyKey key, ReadOnlyMemory<byte> value)> dependencies,
            ElmCompiler elmCompiler,
            PineVMParseCache parseCache,
            PineVM pineVM,
            ElmCompilerCache elmCompilerCache)
        {
            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();
            var serializeStopwatch = System.Diagnostics.Stopwatch.StartNew();

            /*
             * 
            module CompileElmAppMain exposing (..)

            import Bytes
            import CompileBackendApp
            import CompileElmApp
                exposing
                    ( CompilationIterationSuccess
                    , DependencyKey
                    , ElmMakeEntryPointKind
                    , EntryPointClass
                    , LocatedCompilationError
                    )
            import Dict


            type CompilationIterationSuccess
                = CompilationIterationSuccess (List ( List String, Bytes.Bytes )) (Result String ElmMakeEntryPointKind)


            asCompletelyLoweredElmApp :
                List ( List String, Bytes.Bytes )
                -> List ( DependencyKey, Bytes.Bytes )
                -> List String
                -> List String
                -> List String
                -> Result (List LocatedCompilationError) CompilationIterationSuccess
            asCompletelyLoweredElmApp sourceFiles dependencies compilationRootFilePath interfaceToHostRootModuleName interfaceElmModuleNamePrefixes =
                case
                    CompileElmApp.asCompletelyLoweredElmApp
                        defaultEntryPoints
                        { sourceFiles = Dict.fromList sourceFiles
                        , dependencies = dependencies
                        , compilationRootFilePath = compilationRootFilePath
                        , interfaceToHostRootModuleName = interfaceToHostRootModuleName
                        , compilationInterfaceElmModuleNamePrefixes = interfaceElmModuleNamePrefixes
                        }
                of
                    Err err ->
                        Err err

                    Ok success ->
                        Ok
                            (CompilationIterationSuccess
                                (Dict.toList success.compiledFiles)
                                success.rootModuleEntryPointKind
                            )


            defaultEntryPoints : List EntryPointClass
            defaultEntryPoints =
                List.concat
                    [ CompileElmApp.defaultEntryPoints
                    , CompileBackendApp.entryPoints
                    ]

            -- Imported from module CompileElmApp:


            type CompilationError
                = MissingDependencyError DependencyKey
                | OtherCompilationError String


            type alias LocationInSourceFiles =
                { filePath : List String
                , locationInModuleText : Elm.Syntax.Range.Range
                }


            type LocatedInSourceFiles a
                = LocatedInSourceFiles LocationInSourceFiles a


            type alias LocatedCompilationError =
                LocatedInSourceFiles CompilationError


            type DependencyKey
                = ElmMakeDependency ElmMakeRequestStructure


            type alias ElmMakeRequestStructure =
                { files : AppFiles
                , entryPointFilePath : List String
                , outputType : ElmMakeOutputType
                , enableDebug : Bool
                }

             * */

            var parseLowerElmAppResult =
                ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                    interactiveEnvironment: elmCompiler.CompilerEnvironment,
                    moduleName: "CompileElmAppMain",
                    declarationName: "asCompletelyLoweredElmApp",
                    parseCache);

            {
                if (parseLowerElmAppResult.IsErrOrNull() is { } err)
                {
                    return "Failed parsing function to lower Elm app: " + err;
                }
            }

            if (parseLowerElmAppResult.IsOkOrNullable() is not { } lowerElmAppDecl)
            {
                throw new NotImplementedException(
                    "Unexpected result type: " + parseLowerElmAppResult.GetType());
            }

            var sourceFilesValues =
                sourceFiles
                .Select(entry =>
                {
                    return
                    PineValue.List(
                        [PineListValueForElmListString(entry.Key)
                        ,ElmValueEncoding.AsElmBytesBytes(entry.Value)
                        ]);
                })
                .ToImmutableArray();

            var dependenciesValues =
                dependencies
                .Select(dependency =>
                {
                    return
                    PineValue.List(
                        [dependency.key.DependencyKeyValue
                        ?? throw new Exception("Protocol error: Missing dependency key value."),

                        ElmValueEncoding.AsElmBytesBytes(dependency.value)
                        ]);
                })
                .ToImmutableArray();

            var applyResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    pineVM: pineVM,
                    functionRecord: lowerElmAppDecl.functionRecord,
                    arguments:
                    [
                        // Source files
                        PineValue.List([..sourceFilesValues]),

                        // Dependencies
                        PineValue.List([..dependenciesValues]),

                        // Compilation root file path
                        PineListValueForElmListString(compilationRootFilePath),

                        // Interface to host root module name
                        PineListValueForElmListString([.. InterfaceToHostRootModuleName.Split('.')]),

                        // Interface Elm module name prefixes
                        PineValue.List(
                            [.. ElmAppInterfaceConvention.CompilationInterfaceModuleNamePrefixes.Select(
                                ElmValueEncoding.StringAsPineValue)
                            ]),
                        ]);

            {
                if (applyResult.IsErrOrNull() is { } err)
                {
                    return "Failed applying function to lower Elm app: " + err;
                }
            }

            if (applyResult.IsOkOrNull() is not { } applyResultValue)
            {
                throw new NotImplementedException(
                    "Unexpected result type: " + applyResult.GetType());
            }

            var compilationResult =
                ParseCompilationResult(
                    applyResultValue,
                    elmCompilerCache);

            return
                (compilationResult,
                new CompilationIterationCompilationReport
                (
                    serializeTimeSpentMilli: (int)serializeStopwatch.ElapsedMilliseconds,
                    prepareJavaScriptEngineTimeSpentMilli: null,
                    argumentsJsonHash: "",
                    argumentsToJavaScriptEngineSerializedLength: null,
                    inJavaScriptEngineTimeSpentMilli: null,
                    deserializeTimeSpentMilli: null,
                    totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds
                ));
        }


        private static CompilationIterationResult ParseCompilationResult(
            PineValue response,
            ElmCompilerCache elmCompilerCache)
        {
            return
                ElmValueInterop.ParseElmResultValue(
                    response,
                    err:
                    err =>
                    CompilationIterationResult.err(ParseLocatedCompilationErrors(err, elmCompilerCache)),

                    ok: loweringOk =>
                    (CompilationIterationResult)
                    ParseCompilationSuccess(loweringOk, elmCompilerCache),

                    invalid: err =>
                    throw new Exception("Invalid result shape: " + err));
        }

        private static IReadOnlyList<CompilerSerialInterface.LocatedCompilationError> ParseLocatedCompilationErrors(
            PineValue pineValue,
            ElmCompilerCache elmCompilerCache)
        {
            if (pineValue is not PineValue.ListValue list)
            {
                throw new Exception(
                    "Expected list value, got: " + pineValue);
            }

            var mapped = new CompilerSerialInterface.LocatedCompilationError[list.Elements.Length];

            for (var i = 0; i < list.Elements.Length; i++)
            {
                var locatedCompilationErrorValue = list.Elements.Span[i];

                var locatedCompilationError =
                    ParseLocatedCompilationError(
                        locatedCompilationErrorValue,
                        elmCompilerCache);

                mapped[i] = locatedCompilationError;
            }

            return mapped;
        }

        private static CompilerSerialInterface.LocatedCompilationError ParseLocatedCompilationError(
            PineValue pineValue,
            ElmCompilerCache elmCompilerCache)
        {
            /*

            type alias LocationInSourceFiles =
                { filePath : List String
                , locationInModuleText : Elm.Syntax.Range.Range
                }


            type LocatedInSourceFiles a
                = LocatedInSourceFiles LocationInSourceFiles a


            type alias LocatedCompilationError =
                LocatedInSourceFiles CompilationError

             * */

            if (pineValue is not PineValue.ListValue list)
            {
                throw new Exception(
                    "Expected list value, got: " + pineValue);
            }

            if (list.Elements.Length is not 2)
            {
                throw new Exception(
                    "Expected list with two elements, got: " + list.Elements.Length);
            }

            var tagNameValue = list.Elements.Span[0];

            if (tagNameValue != PineValueAsString.ValueFromString("LocatedInSourceFiles"))
            {
                throw new Exception(
                    "Expected first element to be 'LocatedInSourceFiles', got: " + tagNameValue);
            }

            var arguments = list.Elements.Span[1];

            if (arguments is not PineValue.ListValue argumentsList)
            {
                throw new Exception(
                    "Expected second element to be a list, got: " + arguments);
            }

            if (argumentsList.Elements.Length is not 2)
            {
                throw new Exception(
                    "Expected list with two elements in tag 'LocatedInSourceFiles', got: " +
                    argumentsList.Elements.Length);
            }

            var locationInSourceFilesValue = argumentsList.Elements.Span[0];

            var locationInSourceFiles =
                ParseLocationInSourceFiles(
                    locationInSourceFilesValue,
                    elmCompilerCache);

            var compilationErrorValue = argumentsList.Elements.Span[1];

            var compilationError =
                ParseCompilationError(compilationErrorValue, elmCompilerCache);

            return new CompilerSerialInterface.LocatedCompilationError(
                locationInSourceFiles, compilationError);
        }

        public static CompilerSerialInterface.LocationInSourceFiles ParseLocationInSourceFiles(
            PineValue pineValue,
            ElmCompilerCache elmCompilerCache)
        {
            var asElmValueResult =
                elmCompilerCache.PineValueDecodedAsElmValue(pineValue);

            if (asElmValueResult.IsErrOrNull() is { } err)
            {
                throw new Exception("Failed parsing as Elm value: " + err);
            }

            if (asElmValueResult.IsOkOrNull() is not { } asElmValue)
            {
                throw new Exception(
                    "Unexpected result type: " + asElmValueResult.GetType());
            }

            if (asElmValue is not ElmValue.ElmRecord asElmRecord)
            {
                throw new Exception("Expected Elm record value, got: " + asElmValue);
            }

            var filePathValue = asElmRecord["filePath"];

            if (filePathValue is not ElmValue.ElmList filePathList)
            {
                throw new Exception("Expected Elm list value, got: " + filePathValue);
            }

            var filePath =
                filePathList.Elements
                .Select(pathElement =>
                {
                    if (pathElement is not ElmValue.ElmString pathString)
                    {
                        throw new Exception("Expected Elm string value, got: " + pathElement);
                    }
                    return pathString.Value;
                })
                .ToImmutableList();

            var locationInModuleTextValue = asElmRecord["locationInModuleText"];

            return new CompilerSerialInterface.LocationInSourceFiles(filePath: filePath);
        }

        private static CompilerSerialInterface.CompilationError ParseCompilationError(
            PineValue pineValue,
            ElmCompilerCache elmCompilerCache)
        {
            /*

           type CompilationError
               = MissingDependencyError DependencyKey
               | OtherCompilationError String

           type DependencyKey
               = ElmMakeDependency ElmMakeRequestStructure

            * */

            if (pineValue is not PineValue.ListValue list)
            {
                throw new Exception(
                    "Expected list value, got: " + pineValue);
            }

            if (list.Elements.Length is not 2)
            {
                throw new Exception(
                    "Expected list with two elements, got: " + list.Elements.Length);
            }

            var tagNameValue = list.Elements.Span[0];

            var tagNameResult = PineValueAsString.StringFromValue(tagNameValue);

            {
                if (tagNameResult.IsErrOrNull() is { } err)
                {
                    throw new Exception("Failed parsing tag name: " + err);
                }
            }

            if (tagNameResult.IsOkOrNull() is not { } tagName)
            {
                throw new Exception(
                    "Unexpected result type: " + tagNameResult.GetType());
            }

            if (tagName is "MissingDependencyError")
            {
                var arguments = list.Elements.Span[1];

                if (arguments is not PineValue.ListValue argumentsList)
                {
                    throw new Exception(
                        "Expected second element to be a list, got: " + arguments);
                }

                if (argumentsList.Elements.Length is not 1)
                {
                    throw new Exception(
                        "Expected list with one element in tag 'MissingDependencyError', got: " +
                        argumentsList.Elements.Length);
                }

                var dependencyKeyValue = argumentsList.Elements.Span[0];

                var dependencyKey = ParseDependencyKey(dependencyKeyValue, elmCompilerCache);

                if (dependencyKey.ElmMakeDependency is null)
                {
                    throw new Exception(
                        "Expected Elm make dependency key, got: " + dependencyKey);
                }

                return
                    new CompilerSerialInterface.CompilationError(
                        MissingDependencyError:
                        [new CompilerSerialInterface.DependencyKey(
                            dependencyKey.ElmMakeDependency,
                            DependencyKeyValue: dependencyKeyValue)]);
            }

            if (tagName is "OtherCompilationError")
            {
                var arguments = list.Elements.Span[1];

                var argumentAsElmValueResult =
                    elmCompilerCache.PineValueDecodedAsElmValue(arguments);

                {
                    if (argumentAsElmValueResult.IsErrOrNull() is { } err)
                    {
                        throw new Exception("Failed parsing as Elm value: " + err);
                    }
                }

                if (argumentAsElmValueResult.IsOkOrNull() is not { } argumentAsElmValue)
                {
                    throw new Exception("Unexpected result type: " + argumentAsElmValueResult.GetType());
                }

                if (argumentAsElmValue is not ElmValue.ElmList asElmList)
                {
                    throw new Exception(
                        "Expected Elm list value, got: " + argumentAsElmValue);
                }

                if (asElmList.Elements.Count is not 1)
                {
                    throw new Exception(
                        "Expected list with one element, got: " + asElmList.Elements.Count);
                }


                if (asElmList.Elements[0] is not ElmValue.ElmString asElmString)
                {
                    throw new Exception(
                        "Expected Elm string value, got: " + asElmList.Elements[0]);
                }

                return new CompilerSerialInterface.CompilationError(
                    OtherCompilationError: [asElmString.Value]);
            }

            throw new Exception("Unexpected tag name: " + tagName);
        }

        private static CompilerSerialInterface.DependencyKey ParseDependencyKey(
            PineValue pineValue,
            ElmCompilerCache elmCompilerCache)
        {
            var asElmValueResult =
                elmCompilerCache.PineValueDecodedAsElmValue(pineValue);

            if (asElmValueResult.IsErrOrNull() is { } err)
            {
                throw new Exception("Failed parsing as Elm value: " + err);
            }

            if (asElmValueResult.IsOkOrNull() is not { } asElmValue)
            {
                throw new Exception("Unexpected result type: " + asElmValueResult.GetType());
            }

            if (asElmValue is not ElmValue.ElmTag asElmTag)
            {
                throw new Exception("Expected Elm tag value, got: " + asElmValue);
            }

            var tagName = asElmTag.TagName;

            if (tagName is "ElmMakeDependency")
            {
                var arguments = asElmTag.Arguments;

                if (arguments.Count is not 1)
                {
                    throw new Exception("Expected tag with one argument, got: " + arguments.Count);
                }

                var elmMakeRequestStructure =
                    ParseElmMakeRequestStructure(
                        arguments[0],
                        elmCompilerCache);

                return new CompilerSerialInterface.DependencyKey(ElmMakeDependency: [elmMakeRequestStructure]);
            }

            throw new Exception("Unexpected tag name: " + tagName);
        }

        private static CompilerSerialInterface.ElmMakeRequestStructure ParseElmMakeRequestStructure(
            ElmValue asElmValue,
            ElmCompilerCache elmCompilerCache)
        {
            if (asElmValue is not ElmValue.ElmRecord asElmRecord)
            {
                throw new Exception("Expected Elm record value, got: " + asElmValue);
            }

            var filesElmValue = asElmRecord["files"];

            if (filesElmValue is null)
            {
                throw new Exception("Expected field 'files' in Elm record.");
            }

            if (filesElmValue is not ElmValue.ElmList filesElmList)
            {
                throw new Exception("Expected Elm list value, got: " + filesElmValue);
            }

            IReadOnlyList<CompilerSerialInterface.AppCodeEntry> filesList =
                [.. filesElmList.Elements
                .Select(fileEntry =>
                {
                    if (fileEntry is not ElmValue.ElmList fileEntryList)
                    {
                        throw new Exception("Expected Elm list value, got: " + fileEntry);
                    }

                    if (fileEntryList.Elements.Count is not 2)
                    {
                        throw new Exception("Expected list with two elements, got: " + fileEntryList.Elements.Count);
                    }

                    var pathValue = fileEntryList.Elements[0];

                    if (pathValue is not ElmValue.ElmList pathList)
                    {
                        throw new Exception("Expected Elm list value, got: " + pathValue);
                    }

                    var path =
                        pathList.Elements
                        .Select(pathElement =>
                        {
                            if (pathElement is not ElmValue.ElmString pathString)
                            {
                                throw new Exception("Expected Elm string value, got: " + pathElement);
                            }
                            return pathString.Value;
                        })
                        .ToImmutableList();

                    var contentValue = fileEntryList.Elements[1];

                    if (contentValue is not ElmValue.ElmBytes contentBytes)
                    {
                        throw new Exception("Expected Elm bytes value, got: " + contentValue);
                    }

                    var contentBase64 = Convert.ToBase64String(contentBytes.Value.Span);

                    return
                    new CompilerSerialInterface.AppCodeEntry(
                        path: path,
                        content: new CompilerSerialInterface.BytesJson(contentBase64));
                })];

            var entryPointFilePathValue = asElmRecord["entryPointFilePath"];

            if (entryPointFilePathValue is not ElmValue.ElmList entryPointFilePathList)
            {
                throw new Exception("Expected Elm list value, got: " + entryPointFilePathValue);
            }

            var entryPointFilePath =
                entryPointFilePathList.Elements
                .Select(pathElement =>
                {
                    if (pathElement is not ElmValue.ElmString pathString)
                    {
                        throw new Exception("Expected Elm string value, got: " + pathElement);
                    }
                    return pathString.Value;
                })
                .ToImmutableList();

            var outputTypeValue = asElmRecord["outputType"];

            if (outputTypeValue is null)
            {
                throw new Exception("Expected field 'outputType' in Elm record.");
            }

            var outputType = ParseElmMakeOutputType(outputTypeValue);

            var enableDebugValue = asElmRecord["enableDebug"];

            if (enableDebugValue is null)
            {
                throw new Exception("Expected field 'enableDebug' in Elm record.");
            }

            if (enableDebugValue is not ElmValue.ElmTag enableDebugBoolTag)
            {
                throw new Exception("Expected Elm tag value, got: " + enableDebugValue);
            }

            var enableDebug =
                enableDebugBoolTag.TagName switch
                {
                    "True" => true,
                    "False" => false,

                    _ =>
                    throw new Exception("Unexpected tag name: " + enableDebugBoolTag.TagName)
                };

            return new CompilerSerialInterface.ElmMakeRequestStructure(
                files: filesList,
                entryPointFilePath: entryPointFilePath,
                outputType: outputType,
                enableDebug: enableDebug,
                enableOptimize: false);
        }

        private static CompilerSerialInterface.ElmMakeOutputType ParseElmMakeOutputType(ElmValue elmValue)
        {
            if (elmValue is not ElmValue.ElmTag elmTag)
            {
                throw new Exception("Expected tag value, got: " + elmValue);
            }

            if (elmTag.TagName is "ElmMakeOutputTypeJs")
            {
                return new CompilerSerialInterface.ElmMakeOutputType(ElmMakeOutputTypeJs: [new object()]);
            }

            if (elmTag.TagName is "ElmMakeOutputTypeHtml")
            {
                return new CompilerSerialInterface.ElmMakeOutputType(ElmMakeOutputTypeHtml: [new object()]);
            }

            throw new Exception("Unexpected tag name: " + elmTag.TagName);
        }

        private static CompilationIterationSuccess ParseCompilationSuccess(
            PineValue pineValue,
            ElmCompilerCache elmCompilerCache)
        {
            if (pineValue is not PineValue.ListValue list)
            {
                throw new Exception(
                    "Expected list value, got: " + pineValue);
            }

            if (list.Elements.Length is not 2)
            {
                throw new Exception(
                    "Expected list with two elements, got: " + list.Elements.Length);
            }

            var tagNameValue = list.Elements.Span[0];

            if (tagNameValue != PineValueAsString.ValueFromString("CompilationIterationSuccess"))
            {
                throw new Exception(
                    "Expected first element to be 'CompilationIterationSuccess', got: " + tagNameValue);
            }

            var arguments = list.Elements.Span[1];

            if (arguments is not PineValue.ListValue argumentsList)
            {
                throw new Exception(
                    "Expected second element to be a list, got: " + arguments);
            }

            if (argumentsList.Elements.Length is not 2)
            {
                throw new Exception(
                    "Expected list with two elements in tag 'CompilationIterationSuccess', got: " +
                    argumentsList.Elements.Length);
            }

            var compiledFilesValue = argumentsList.Elements.Span[0];

            if (compiledFilesValue is not PineValue.ListValue compiledFilesList)
            {
                throw new Exception(
                    "Expected first element to be a list, got: " + compiledFilesValue);
            }

            var compiledFilesResult =
                elmCompilerCache.PineValueDecodedAsElmValue(compiledFilesValue);

            if (compiledFilesResult.IsErrOrNull() is { } err)
            {
                throw new Exception(
                    "Failed parsing compiled files: " + err);
            }

            if (compiledFilesResult.IsOkOrNull() is not { } compiledFilesElmValue)
            {
                throw new Exception(
                    "Unexpected result type: " + compiledFilesResult.GetType());
            }

            if (compiledFilesElmValue is not ElmValue.ElmList compiledFilesElmList)
            {
                throw new Exception(
                    "Expected list value, got: " + compiledFilesElmValue);
            }

            var compiledFiles =
                compiledFilesElmList.Elements
                .Select(compiledFileElmValue =>
                {
                    if (compiledFileElmValue is not ElmValue.ElmList compiledFileElmList)
                    {
                        throw new Exception(
                            "Expected list value, got: " + compiledFileElmValue);
                    }

                    if (compiledFileElmList.Elements.Count is not 2)
                    {
                        throw new Exception(
                            "Expected list with two elements, got: " + compiledFileElmList.Elements.Count);
                    }

                    var pathElmValue = compiledFileElmList.Elements[0];

                    if (pathElmValue is not ElmValue.ElmList pathElmList)
                    {
                        throw new Exception(
                            "Expected string value, got: " + pathElmValue);
                    }

                    var path =
                        pathElmList.Elements
                        .Select(pathElement =>
                        {
                            if (pathElement is not ElmValue.ElmString pathString)
                            {
                                throw new Exception(
                                    "Expected string value, got: " + pathElement);
                            }
                            return pathString.Value;
                        })
                        .ToImmutableList();

                    var contentElmValue = compiledFileElmList.Elements[1];

                    if (contentElmValue is not ElmValue.ElmBytes contentElmBytes)
                    {
                        throw new Exception(
                            "Expected bytes value, got: " + contentElmValue);
                    }

                    return (path, contentElmBytes.Value);
                })
                .ToImmutableDictionary(
                    entry =>
                    (IReadOnlyList<string>)entry.path,
                    entry =>
                    entry.Value);

            var rootModuleEntryPointKindValue = argumentsList.Elements.Span[1];

            var rootModuleEntryPointKindResult =
                ParseElmMakeEntryPointKindResult(
                    rootModuleEntryPointKindValue,
                    elmCompilerCache);

            return
                new CompilationIterationSuccess(
                    compiledFiles: compiledFiles,
                    rootModuleEntryPointKindResult);
        }

        private static Result<string, CompilerSerialInterface.ElmMakeEntryPointKind> ParseElmMakeEntryPointKindResult(
            PineValue pineValue,
            ElmCompilerCache elmCompilerCache)
        {
            var asElmValueResult =
                elmCompilerCache.PineValueDecodedAsElmValue(pineValue);

            if (asElmValueResult.IsErrOrNull() is { } err)
            {
                throw new Exception("Failed parsing as Elm value: " + err);
            }

            if (asElmValueResult.IsOkOrNull() is not { } asElmValue)
            {
                throw new Exception("Unexpected result type: " + asElmValueResult.GetType());
            }

            if (asElmValue is not ElmValue.ElmTag asElmTag)
            {
                throw new Exception("Expected Elm tag value, got: " + asElmValue);
            }

            if (asElmTag.TagName is "Err")
            {
                var arguments = asElmTag.Arguments;

                if (arguments.Count is not 1)
                {
                    throw new Exception("Expected tag with one argument, got: " + arguments.Count);
                }

                var errValue = arguments[0];

                if (errValue is not ElmValue.ElmString errString)
                {
                    throw new Exception("Expected Elm string value, got: " + errValue);
                }

                return Result<string, CompilerSerialInterface.ElmMakeEntryPointKind>.err(errString.Value);
            }

            if (asElmTag.TagName is "Ok")
            {
                var arguments = asElmTag.Arguments;

                if (arguments.Count is not 1)
                {
                    throw new Exception("Expected tag with one argument, got: " + arguments.Count);
                }

                var okValue = arguments[0];

                var okResult = ParseElmMakeEntryPointKind(okValue);

                return Result<string, CompilerSerialInterface.ElmMakeEntryPointKind>.ok(okResult);
            }

            throw new Exception("Unexpected tag name: " + asElmTag.TagName);
        }

        private static CompilerSerialInterface.ElmMakeEntryPointKind ParseElmMakeEntryPointKind(
            ElmValue asElmValue)
        {
            /*
             * 

            type ElmMakeEntryPointKind
                = ClassicMakeEntryPoint ElmMakeEntryPointStruct
                | BlobMakeEntryPoint ElmMakeEntryPointStruct


            type alias ElmMakeEntryPointStruct =
                { elmMakeJavaScriptFunctionName : String
                }

             * */

            if (asElmValue is not ElmValue.ElmTag asElmTag)
            {
                throw new Exception("Expected Elm tag value, got: " + asElmValue);
            }

            var tagName = asElmTag.TagName;

            if (tagName is "ClassicMakeEntryPoint")
            {
                var arguments = asElmTag.Arguments;

                if (arguments.Count is not 1)
                {
                    throw new Exception("Expected tag with one argument, got: " + arguments.Count);
                }

                var elmMakeEntryPointStruct = ParseElmMakeEntryPointStruct(arguments[0]);

                return new CompilerSerialInterface.ElmMakeEntryPointKind.ClassicMakeEntryPoint(elmMakeEntryPointStruct);
            }

            if (tagName is "BlobMakeEntryPoint")
            {
                var arguments = asElmTag.Arguments;

                if (arguments.Count is not 1)
                {
                    throw new Exception("Expected tag with one argument, got: " + arguments.Count);
                }

                var elmMakeEntryPointStruct = ParseElmMakeEntryPointStruct(arguments[0]);

                return new CompilerSerialInterface.ElmMakeEntryPointKind.BlobMakeEntryPoint(elmMakeEntryPointStruct);
            }

            throw new Exception("Unexpected tag name: " + tagName);
        }

        private static CompilerSerialInterface.ElmMakeEntryPointStruct ParseElmMakeEntryPointStruct(
            ElmValue elmValue)
        {
            if (elmValue is not ElmValue.ElmRecord asElmRecord)
            {
                throw new Exception("Expected Elm record value, got: " + elmValue);
            }

            var elmMakeJavaScriptFunctionNameValue = asElmRecord["elmMakeJavaScriptFunctionName"];

            if (elmMakeJavaScriptFunctionNameValue is not ElmValue.ElmString elmMakeJavaScriptFunctionNameString)
            {
                throw new Exception("Expected Elm string value, got: " + elmMakeJavaScriptFunctionNameValue);
            }

            return new CompilerSerialInterface.ElmMakeEntryPointStruct(
                elmMakeJavaScriptFunctionName: elmMakeJavaScriptFunctionNameString.Value);
        }

        private static PineValue PineListValueForElmListString(IEnumerable<string> strings) =>
            PineValue.List([.. strings.Select(ElmValueEncoding.StringAsPineValue)]);


        public static ImmutableList<string> FilePathFromModuleName(IReadOnlyList<string> moduleName)
        {
            var fileName = moduleName.Last() + ".elm";
            var directoryNames = moduleName.Reverse().Skip(1).Reverse();

            return ["src", .. directoryNames, fileName];
        }

        public static IImmutableList<string> FilePathFromModuleName(string moduleName) =>
            FilePathFromModuleName(moduleName.Split('.'));

        public static string InterfaceToHostRootModuleName => "Backend.InterfaceToHost_Root";

        public static readonly Lazy<Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>> CachedCompilerElmProgramCodeFilesForElmBackend =
            new(LoadCompilerElmProgramCodeFilesForElmBackend);

        public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadCompilerElmProgramCodeFilesForElmBackend() =>
            ElmCompiler.LoadElmCompilerSourceCodeFiles();

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
                return "Did not find module declaration";

            return moduleDeclarationMatch.Groups[1].Value.Split('.');
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

        public record DependencyKey(
            IReadOnlyList<ElmMakeRequestStructure> ElmMakeDependency,
            PineValue? DependencyKeyValue = null);

        public record ElmMakeRequestStructure(
            IReadOnlyList<AppCodeEntry> files,
            IReadOnlyList<string> entryPointFilePath,
            ElmMakeOutputType outputType,
            bool enableDebug,
            bool enableOptimize);

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