using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Text.RegularExpressions;

namespace ElmTime;


public class ProcessFromElm019Code
{

    public const string appStateJsVarName = "app_state";

    public const string initStateJsFunctionPublishedSymbol = "initState";

    public const string serializedEventFunctionPublishedSymbol = "serializedEvent";

    public const string processEventSyncronousJsFunctionName = "processEventAndUpdateState";

    public static string AsJavascriptExpression(string originalString) =>
        JsonSerializer.Serialize(originalString);

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
