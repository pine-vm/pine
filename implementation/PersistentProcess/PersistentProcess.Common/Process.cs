using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Newtonsoft.Json;
using PuppeteerSharp;

namespace Kalmit
{
    public interface IProcess<EventT, ResponseT>
    {
        ResponseT ProcessEvent(EventT serializedEvent);

        string GetSerializedState();

        string SetSerializedState(string serializedState);
    }

    public interface IProcessWithCustomSerialization : IProcess<string, string>
    {
    }

    public interface IDisposableProcessWithCustomSerialization : IProcessWithCustomSerialization, IDisposable
    {
    }

    class ProcessWithCustomSerializationHostedWithPuppeteer : IDisposableProcessWithCustomSerialization
    {
        readonly Browser browser;

        readonly Page browserPage;

        public ProcessWithCustomSerializationHostedWithPuppeteer(string javascriptPreparedToRun)
        {
            new BrowserFetcher().DownloadAsync(BrowserFetcher.DefaultRevision).Wait();

            browser = Puppeteer.LaunchAsync(new LaunchOptions
            {
                Headless = true,
            }).Result;

            browserPage = browser.NewPageAsync().Result;

            var initAppResult = browserPage.EvaluateExpressionAsync(javascriptPreparedToRun).Result;

            var resetAppStateResult = browserPage.EvaluateExpressionAsync(
                ProcessFromElm019Code.appStateJsVarName + " = " + ProcessFromElm019Code.initStateJsFunctionPublishedSymbol + ";").Result;
        }

        static string AsJavascriptExpression(string originalString) =>
            JsonConvert.SerializeObject(originalString);

        public void Dispose()
        {
            browser?.Dispose();
        }

        public string ProcessEvent(string serializedEvent)
        {
            var eventExpression = AsJavascriptExpression(serializedEvent);

            var expressionJavascript = ProcessFromElm019Code.processEventSyncronousJsFunctionName + "(" + eventExpression + ")";

            var processEventtResult = browserPage.EvaluateExpressionAsync<string>(expressionJavascript).Result;

            return processEventtResult;
        }

        public string GetSerializedState()
        {
            var expressionJavascript =
                ProcessFromElm019Code.serializeStateJsFunctionPublishedSymbol +
                "(" + ProcessFromElm019Code.appStateJsVarName + ")";

            return browserPage.EvaluateExpressionAsync<string>(expressionJavascript).Result;
        }

        public string SetSerializedState(string serializedState)
        {
            var serializedStateExpression = AsJavascriptExpression(serializedState);

            var expressionJavascript =
                ProcessFromElm019Code.appStateJsVarName +
                " = " + ProcessFromElm019Code.deserializeStateJsFunctionPublishedSymbol +
                "(" + serializedStateExpression + ");";

            return browserPage.EvaluateExpressionAsync<string>(expressionJavascript).Result;
        }
    }

    public struct ElmAppEntryConfig
    {
        //  This provides information about entry points in the elm app relative to the files which make up the elm app.

        public ElmAppEntryConfigWithCustomSerialization? WithCustomSerialization;

        public struct ElmAppEntryConfigWithCustomSerialization
        {
            public string pathToFileWithElmEntryPoint;
            public string pathToInitialStateFunction;
            public string pathToSerializedEventFunction;
            public string pathToSerializeStateFunction;
            public string pathToDeserializeStateFunction;
        }
    }

    public class ProcessFromElm019Code
    {
        static public IDisposableProcessWithCustomSerialization WithCustomSerialization(
            IReadOnlyCollection<(string, byte[])> elmCodeFiles,
            ElmAppEntryConfig.ElmAppEntryConfigWithCustomSerialization entryConfig)
        {
            var javascriptFromElmMake = CompileElmToJavascript(elmCodeFiles, entryConfig.pathToFileWithElmEntryPoint);

            var javascriptPreparedToRun =
                BuildAppJavascript(
                    javascriptFromElmMake,
                    entryConfig.pathToSerializedEventFunction,
                    entryConfig.pathToInitialStateFunction,
                    entryConfig.pathToSerializeStateFunction,
                    entryConfig.pathToDeserializeStateFunction);

            return new ProcessWithCustomSerializationHostedWithPuppeteer(javascriptPreparedToRun);
        }

        static string CompileElmToJavascript(
            IReadOnlyCollection<(string, byte[])> elmCodeFiles,
            string pathToFileWithElmEntryPoint)
        {
            var outputFileName = "file-for-elm-make-output.js";

            var command = "make " + pathToFileWithElmEntryPoint + " --output=\"" + outputFileName + "\"";

            var commandResults = ExecutableFile.ExecuteFileWithArguments(elmCodeFiles, GetElmExecutableFile, command);

            var outputFileContent =
                commandResults.resultingFiles.FirstOrDefault(resultFile => resultFile.name == outputFileName).content;

            if (outputFileContent == null)
                throw new NotImplementedException(
                    "Output file not found. Maybe output from Elm make helps to find the cause:\nExit Code: " + commandResults.exitCode +
                    "\nStandard Output:\n" + commandResults.standardOutput);

            return Encoding.UTF8.GetString(outputFileContent);
        }

        /*
        Elm executable obtained on 2018-09-03 from
        https://github.com/elm/compiler/releases/download/0.19.0/installer-for-windows.exe
        */
        static byte[] GetElmExecutableFile => BlobLibrary.GetBlobWithSHA1(
            CommonConversion.ByteArrayFromStringBase16("80E414D3A261951BD2A732FD82B7D6994E017927"));

        public const string appStateJsVarName = "app_state";

        public const string initStateJsFunctionPublishedSymbol = "initState";

        public const string serializedEventFunctionPublishedSymbol = "serializedEvent";

        public const string serializeStateJsFunctionPublishedSymbol = "serializeState";

        public const string deserializeStateJsFunctionPublishedSymbol = "deserializeState";

        /*
        Takes the javascript as emitted from Elm make 0.19 and inserts additional statements to
        prepare the script for usage in our application.
        This preparation includes:
        + Publish interfacing functions of app to the global scope.
        + Add a function which implements processing an event and storing the resulting process state and returns the response of the process.
        */
        static string BuildAppJavascript(
            string javascriptFromElmMake,
            string pathToSerializedEventFunction,
            string pathToInitialStateFunction,
            string pathToSerializeStateFunction,
            string pathToDeserializeStateFunction)
        {
            var invokeExportStatementMatch =
                Regex.Matches(javascriptFromElmMake, Regex.Escape("_Platform_export(")).OfType<Match>().LastOrDefault();

            var listFunctionToPublish =
                new[]
                {
                    new
                    {
                        name = pathToSerializedEventFunction,
                        publicName = serializedEventFunctionPublishedSymbol,
                        arity = 2,
                    },
                    new
                    {
                        name = pathToInitialStateFunction,
                        publicName = initStateJsFunctionPublishedSymbol,
                        arity = 0,
                    },
                    new
                    {
                        name = pathToSerializeStateFunction,
                        publicName = serializeStateJsFunctionPublishedSymbol,
                        arity = 1,
                    },
                    new
                    {
                        name = pathToDeserializeStateFunction,
                        publicName = deserializeStateJsFunctionPublishedSymbol,
                        arity = 1,
                    },
                }
                .Select(
                    functionToPublish =>
                    new
                    {
                        publicName = functionToPublish.publicName,
                        expression =
                            BuildElmFunctionPublicationExpression(
                                appFunctionSymbolMap(functionToPublish.name), functionToPublish.arity)
                    })
                .ToList();

            var publishStatements =
                listFunctionToPublish
                .Select(functionToPublish => "scope['" + functionToPublish.publicName + "'] = " + functionToPublish.expression + ";")
                .ToList();

            var publicationInsertLocation = invokeExportStatementMatch.Index;

            var publicationInsertString =
                string.Join(Environment.NewLine, new[] { "" }.Concat(publishStatements).Concat(new[] { "" }));

            var processEventAndUpdateStateFunctionJavascriptLines = new[]
            {
                "var " + processEventSyncronousJsFunctionName + " = function(eventSerial){",
                "var newStateAndResponse = " + serializedEventFunctionPublishedSymbol + "(eventSerial," + appStateJsVarName + ");",
                appStateJsVarName + " = newStateAndResponse.a;",
                "return newStateAndResponse.b;",
                "}",
            };

            var processEventAndUpdateStateFunctionJavascript =
                String.Join(Environment.NewLine, processEventAndUpdateStateFunctionJavascriptLines);

            return
                javascriptFromElmMake.Insert(publicationInsertLocation, publicationInsertString) +
                Environment.NewLine +
                processEventAndUpdateStateFunctionJavascript;
        }

        public const string processEventSyncronousJsFunctionName = "processEvent";

        static string BuildElmFunctionPublicationExpression(string functionToCallName, int arity)
        {
            if (arity < 2)
                return functionToCallName;

            var paramNameList = Enumerable.Range(0, arity).Select(paramIndex => "param_" + paramIndex).ToList();

            return
                "(" + String.Join(",", paramNameList) + ") => " + functionToCallName +
                String.Join("", paramNameList.Select(paramName => "(" + paramName + ")"));
        }

        static string appFunctionSymbolMap(string pathToFileWithElmEntryPoint) =>
            "author$project$" + pathToFileWithElmEntryPoint.Replace(".", "$");

        static public bool FilePathMatchesPatternOfFilesInElmApp(string filePath) =>
            Regex.IsMatch(
                Path.GetFileName(filePath),
                "(^" + Regex.Escape("elm.json") + "|" + Regex.Escape(".elm") + ")$",
                RegexOptions.IgnoreCase);
    }
}
