using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using ElmFullstack;
using Pine;
using static Pine.Composition;

namespace elm_fullstack.ElmInteractive
{
    public class ElmInteractive
    {
        static public Result<string, SubmissionResponseValueStructure> EvaluateSubmissionAndGetResultingValue(
            TreeWithStringPath appCodeTree,
            string submission,
            IReadOnlyList<string> previousLocalSubmissions = null)
        {
            using var jsEngine = PrepareJsEngineToEvaluateElm();

            return EvaluateSubmissionAndGetResultingValue(
                jsEngine,
                appCodeTree: appCodeTree,
                submission: submission,
                previousLocalSubmissions: previousLocalSubmissions);
        }

        static public Result<string, SubmissionResponseValueStructure> EvaluateSubmissionAndGetResultingValue(
            JavaScriptEngineSwitcher.Core.IJsEngine evalElmPreparedJsEngine,
            TreeWithStringPath appCodeTree,
            string submission,
            IReadOnlyList<string> previousLocalSubmissions = null)
        {
            var modulesTexts =
                appCodeTree == null ? null
                :
                TreeToFlatDictionaryWithPathComparer(appCodeTree)
                .Select(appCodeFile => appCodeFile.Key.Last().EndsWith(".elm") ? Encoding.UTF8.GetString(appCodeFile.Value.ToArray()) : null)
                .WhereNotNull()
                .ToImmutableList();

            var argumentsJson = Newtonsoft.Json.JsonConvert.SerializeObject(
                new
                {
                    modulesTexts = modulesTexts ?? ImmutableList<string>.Empty,
                    submission = submission,
                    previousLocalSubmissions = previousLocalSubmissions ?? ImmutableList<string>.Empty,
                }
            );

            var responseJson =
                evalElmPreparedJsEngine.CallFunction("evaluateSubmissionInInteractive", argumentsJson)
                ?.ToString();

            var responseStructure =
                Newtonsoft.Json.JsonConvert.DeserializeObject<EvaluateSubmissionResponseStructure>(
                    responseJson);

            if (responseStructure.DecodedArguments == null)
                throw new Exception("Failed to decode arguments: " + responseStructure.FailedToDecodeArguments);

            if (responseStructure.DecodedArguments.Evaluated == null)
                return Result<string, SubmissionResponseValueStructure>.err(responseStructure.DecodedArguments.FailedToEvaluate);

            return Result<string, SubmissionResponseValueStructure>.ok(
                responseStructure.DecodedArguments.Evaluated.SubmissionResponseValue);
        }

        static public JavaScriptEngineSwitcher.Core.IJsEngine PrepareJsEngineToEvaluateElm()
        {
            var parseElmAppCodeFiles = ParseElmSyntaxAppCodeFiles();

            var javascriptFromElmMake =
                ProcessFromElm019Code.CompileElmToJavascript(
                    parseElmAppCodeFiles,
                    ImmutableList.Create("src", "Main.elm"));

            var javascriptMinusCrashes = ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

            var listFunctionToPublish =
                new[]
                {
                    (functionNameInElm: "Main.evaluateSubmissionInInteractive",
                    publicName: "evaluateSubmissionInInteractive",
                    arity: 1),
                };

            var javascriptPreparedToRun =
                ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                    javascriptMinusCrashes,
                    listFunctionToPublish);

            var javascriptEngine = ProcessHostedWithV8.ConstructJsEngine();

            javascriptEngine.Evaluate(javascriptPreparedToRun);

            return javascriptEngine;
        }

        static public IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> ParseElmSyntaxAppCodeFiles() =>
            ImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>.Empty
            .WithComparers(EnumerableExtension.EqualityComparer<string>())
            .SetItem(ImmutableList.Create("elm.json"), GetManifestResourceStreamContent("elm_fullstack.ElmInteractive.interpret_elm_program.elm.json"))
            .SetItem(ImmutableList.Create("src", "Pine.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmInteractive.interpret_elm_program.src.Pine.elm"))
            .SetItem(ImmutableList.Create("src", "ElmInteractive.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmInteractive.interpret_elm_program.src.ElmInteractive.elm"))
            .SetItem(ImmutableList.Create("src", "Main.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmInteractive.interpret_elm_program.src.Main.elm"));

        static byte[] GetManifestResourceStreamContent(string name)
        {
            using var stream = typeof(ElmInteractive).Assembly.GetManifestResourceStream(name);
            using var memoryStream = new MemoryStream();

            stream.CopyTo(memoryStream);

            return memoryStream.ToArray();
        }

        class EvaluateSubmissionResponseStructure
        {
            public string FailedToDecodeArguments = null;

            public DecodedArgumentsSctructure DecodedArguments = null;
        }

        class DecodedArgumentsSctructure
        {
            public string FailedToEvaluate = null;

            public EvaluatedSctructure Evaluated = null;
        }

        class EvaluatedSctructure
        {
            public object SubmissionResponseNoValue = null;

            public SubmissionResponseValueStructure SubmissionResponseValue = null;
        }

        public class SubmissionResponseValueStructure
        {
            public string valueAsElmExpressionText = null;

            public string valueAsJsonString = null;

            public string typeText = null;
        }
    }
}