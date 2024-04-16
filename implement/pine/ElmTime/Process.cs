using ElmTime.Elm019;
using ElmTime.JavaScript;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;

namespace ElmTime;

public interface IProcess<EventT, ResponseT>
{
    ResponseT ProcessEvent(EventT serializedEvent);
}

public interface IProcessWithStringInterface : IProcess<string, string>
{
}

public interface IDisposableProcessWithStringInterface : IProcessWithStringInterface, IDisposable
{
}

public class ProcessHostedWithJavaScriptEngine : IDisposableProcessWithStringInterface
{
    private readonly IJavaScriptEngine javascriptEngine;

    public ProcessHostedWithJavaScriptEngine(
        string javascriptPreparedToRun,
        IJavaScriptEngine javascriptEngine)
    {
        this.javascriptEngine = javascriptEngine;

        javascriptEngine.Evaluate(javascriptPreparedToRun);

        javascriptEngine.Evaluate(
            ProcessFromElm019Code.appStateJsVarName + " = " + ProcessFromElm019Code.initStateJsFunctionPublishedSymbol + ";");
    }

    public void Dispose()
    {
        javascriptEngine?.Dispose();
    }

    public string ProcessEvent(string serializedEvent)
    {
        /*
        Avoid high memory usage as described in exploration 2020-02-02:
        Use specialized implementation based on `CallFunction` instead of `Evaluate`.

        var eventExpression = AsJavascriptExpression(serializedEvent);

        var expressionJavascript = ProcessFromElm019Code.processEventSyncronousJsFunctionName + "(" + eventExpression + ")";

        return EvaluateInJsEngineAndReturnResultAsString(expressionJavascript);
        */

        var jsReturnValue = javascriptEngine.CallFunction(
            ProcessFromElm019Code.processEventSyncronousJsFunctionName, serializedEvent);

        return jsReturnValue.ToString()!;
    }
}

public class ProcessFromElm019Code
{
    public record PreparedProcess(
        BuildArtifacts buildArtifacts,
        Func<IDisposableProcessWithStringInterface> startProcess);

    public record BuildArtifacts(
        string javaScriptFromElmMake,
        string javaScriptPreparedToRun);

    public static PreparedProcess ProcessFromElmCodeFiles(
        IReadOnlyCollection<(IReadOnlyList<string>, ReadOnlyMemory<byte>)> elmCodeFiles,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null,
        Func<IJavaScriptEngine>? overrideJavaScriptEngineFactory = null) =>
        ProcessFromElmCodeFiles(
            PineValueComposition.ToFlatDictionaryWithPathComparer(elmCodeFiles),
            overrideElmAppInterfaceConfig,
            overrideJavaScriptEngineFactory);

    public static PreparedProcess ProcessFromElmCodeFiles(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> elmCodeFiles,
        ElmAppInterfaceConfig? overrideElmAppInterfaceConfig = null,
        Func<IJavaScriptEngine>? overrideJavaScriptEngineFactory = null)
    {
        var elmAppInterfaceConfig = overrideElmAppInterfaceConfig ?? ElmAppInterfaceConfig.Default;

        var elmMakeResult = Elm019Binaries.ElmMakeToJavascript(
            elmCodeFiles,
            workingDirectoryRelative: null,
            ElmAppCompilation.FilePathFromModuleName(ElmAppCompilation.InterfaceToHostRootModuleName));

        var javascriptFromElmMake =
            Encoding.UTF8.GetString(
                elmMakeResult.Extract(err => throw new Exception("Failed elm make: " + err)).producedFile.Span);

        var pathToFunctionCommonStart = ElmAppCompilation.InterfaceToHostRootModuleName + ".";

        var javascriptPreparedToRun =
            BuildAppJavascript(
                javascriptFromElmMake,
                pathToFunctionCommonStart + ElmAppInterfaceConvention.ProcessSerializedEventFunctionName,
                pathToFunctionCommonStart + ElmAppInterfaceConvention.InitialStateFunctionName);

        return
            new PreparedProcess(
                buildArtifacts: new BuildArtifacts(
                    javaScriptFromElmMake: javascriptFromElmMake,
                    javaScriptPreparedToRun: javascriptPreparedToRun),
                startProcess: () => new ProcessHostedWithJavaScriptEngine(
                    javascriptPreparedToRun,
                    javascriptEngine: overrideJavaScriptEngineFactory?.Invoke() ?? JavaScriptEngineJintOptimizedForElmApps.Create()));
    }

    public const string appStateJsVarName = "app_state";

    public const string initStateJsFunctionPublishedSymbol = "initState";

    public const string serializedEventFunctionPublishedSymbol = "serializedEvent";

    public const string processEventSyncronousJsFunctionName = "processEventAndUpdateState";

    public static string AsJavascriptExpression(string originalString) =>
        JsonSerializer.Serialize(originalString);

    /*
    Takes the javascript as emitted from Elm make 0.19 and inserts additional statements to
    prepare the script for usage in our application.
    This preparation includes:
    + Publish interfacing functions of app to the global scope.
    + Add a function which implements processing an event and storing the resulting process state and returns the response of the process.
    */
    private static string BuildAppJavascript(
        string javascriptFromElmMake,
        string pathToSerializedEventFunction,
        string pathToInitialStateFunction)
    {
        var javascriptMinusCrashes = JavascriptMinusCrashes(javascriptFromElmMake);

        var listFunctionToPublish =
            new[]
            {
                (functionNameInElm: pathToSerializedEventFunction,
                publicName: serializedEventFunctionPublishedSymbol,
                arity: 2),

                (functionNameInElm: pathToInitialStateFunction,
                publicName: initStateJsFunctionPublishedSymbol,
                arity: 0),
            };

        var JavaScriptFunctionsLines = new[]
        {
            "var " + processEventSyncronousJsFunctionName + " = function(eventSerial){",
            "var newStateAndResponse = " + serializedEventFunctionPublishedSymbol + "(eventSerial," + appStateJsVarName + ");",
            appStateJsVarName + " = newStateAndResponse.a;",
            "return newStateAndResponse.b;",
            "}",
        };

        var processEventAndUpdateStateFunctionJavascript =
            string.Join(Environment.NewLine, JavaScriptFunctionsLines);

        return
            PublishFunctionsFromJavascriptFromElmMake(
                javascriptMinusCrashes,
                listFunctionToPublish) +
            Environment.NewLine +
            processEventAndUpdateStateFunctionJavascript;
    }

    public static string PublishFunctionsFromJavascriptFromElmMake(
        string javascriptFromElmMake,
        IEnumerable<(string functionNameInElm, string publicName, int arity)> functions)
    {
        var invokeExportStatementMatch =
            Regex.Matches(javascriptFromElmMake, Regex.Escape("_Platform_export(")).OfType<Match>().Last();

        var listFunctionToPublish =
            functions
            .Select(
                functionToPublish =>
                new
                {
                    publicName = functionToPublish.publicName,
                    expression =
                        BuildElmFunctionPublicationExpression(
                            AppFunctionSymbolMap(functionToPublish.functionNameInElm), functionToPublish.arity)
                })
            .ToList();

        var publishStatements =
            listFunctionToPublish
            .Select(functionToPublish => "scope['" + functionToPublish.publicName + "'] = " + functionToPublish.expression + ";")
            .ToList();

        var publicationInsertLocation = invokeExportStatementMatch.Index;

        var publicationInsertString =
            string.Join(Environment.NewLine, ["", .. publishStatements, ""]);

        return
            javascriptFromElmMake.Insert(publicationInsertLocation, publicationInsertString);
    }

    /*
    Work around runtime exception with javascript from Elm make:
    > "Script threw an exception. 'console' is not defined"

    2018-12-02 Observed problematic statements in output from elm make, causing running the script to fail:
    console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');
    [...]
    console.log(tag + ': ' + _Debug_toString(value));

    For some applications collecting the arguments to those functions might be interesting,
    to implement this, have a look at https://github.com/Microsoft/ChakraCore/wiki/JavaScript-Runtime-(JSRT)-Overview
    */
    public static string JavascriptMinusCrashes(string javascriptFromElmMake) =>
        Regex.Replace(
            javascriptFromElmMake,
            "^\\s*console\\.\\w+\\(.+$", "",
            RegexOptions.Multiline);

    private static string BuildElmFunctionPublicationExpression(string functionToCallName, int arity)
    {
        if (arity < 2)
            return functionToCallName;

        var paramNameList = Enumerable.Range(0, arity).Select(paramIndex => "param_" + paramIndex).ToList();

        return
            "(" + string.Join(",", paramNameList) + ") => " + functionToCallName +
            string.Join("", paramNameList.Select(paramName => "(" + paramName + ")"));
    }

    private static string AppFunctionSymbolMap(string pathToFileWithElmEntryPoint) =>
        "$author$project$" + pathToFileWithElmEntryPoint.Replace(".", "$");
}
