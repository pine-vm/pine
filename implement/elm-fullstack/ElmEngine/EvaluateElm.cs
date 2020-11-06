using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using Kalmit;
using static Kalmit.Composition;

namespace elm_fullstack.ElmEngine
{
    public class EvaluateElm
    {
        static public Result<string, SubmissionResponseValueStructure> EvaluateSubmissionAndGetResultingValue(
            Composition.TreeComponent appCodeTree,
            string submission,
            IReadOnlyList<string> previousLocalSubmissions = null)
        {
            using (var jsEngine = PrepareJsEngineToEvaluateElm())
            {
                return EvaluateSubmissionAndGetResultingValue(
                    jsEngine,
                    appCodeTree: appCodeTree,
                    submission: submission,
                    previousLocalSubmissions: previousLocalSubmissions);
            }
        }

        static public Result<string, SubmissionResponseValueStructure> EvaluateSubmissionAndGetResultingValue(
            JavaScriptEngineSwitcher.Core.IJsEngine evalElmPreparedJsEngine,
            Composition.TreeComponent appCodeTree,
            string submission,
            IReadOnlyList<string> previousLocalSubmissions = null)
        {
            var modulesTexts =
                appCodeTree == null ? null
                :
                ElmApp.ToFlatDictionaryWithPathComparer(
                    appCodeTree.EnumerateBlobsTransitive()
                    .Select(file =>
                        (filePath: (IImmutableList<string>)file.path.Select(pathComponent => Encoding.UTF8.GetString(pathComponent.ToArray())).ToImmutableList(),
                        content: file.blobContent)))
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

            return Result<string, ElmEngine.EvaluateElm.SubmissionResponseValueStructure>.ok(
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

            var javascriptEngine = ProcessHostedWithChakraCore.ConstructJsEngine();

            var initAppResult = javascriptEngine.Evaluate(javascriptPreparedToRun);

            return javascriptEngine;
        }

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> ParseElmSyntaxAppCodeFiles() =>
            ImmutableDictionary<IImmutableList<string>, IImmutableList<byte>>.Empty
            .WithComparers(EnumerableExtension.EqualityComparer<string>())
            .SetItem(ImmutableList.Create("elm.json"), GetManifestResourceStreamContent("elm_fullstack.ElmEngine.evaluate_elm_program.elm.json").ToImmutableList())
            .SetItem(ImmutableList.Create("src", "Pine.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmEngine.evaluate_elm_program.src.Pine.elm").ToImmutableList())
            .SetItem(ImmutableList.Create("src", "ElmInteractive.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmEngine.evaluate_elm_program.src.ElmInteractive.elm").ToImmutableList())
            .SetItem(ImmutableList.Create("src", "Main.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmEngine.evaluate_elm_program.src.Main.elm").ToImmutableList());

        static byte[] GetManifestResourceStreamContent(string name)
        {
            using (var stream = typeof(EvaluateElm).Assembly.GetManifestResourceStream(name))
            {
                using (var memoryStream = new MemoryStream())
                {
                    stream.CopyTo(memoryStream);

                    return memoryStream.ToArray();
                }
            }
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