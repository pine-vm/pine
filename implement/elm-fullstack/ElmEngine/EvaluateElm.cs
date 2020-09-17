using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using Kalmit;

namespace elm_fullstack.ElmEngine
{
    public class EvaluateElm
    {
        static public string GetValueFromEntryPointAsJsonString(
            Composition.TreeComponent appCodeTree,
            string expression)
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

            var parseElmAppCodeFiles = ParseElmSyntaxAppCodeFiles();

            var argumentsJson = Newtonsoft.Json.JsonConvert.SerializeObject(
                new
                {
                    modulesTexts = modulesTexts ?? ImmutableList<string>.Empty,
                    expression = expression,
                }
            );

            var javascriptFromElmMake =
                ProcessFromElm019Code.CompileElmToJavascript(
                    parseElmAppCodeFiles,
                    ImmutableList.Create("src", "Main.elm"));

            var javascriptMinusCrashes = ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

            var listFunctionToPublish =
                new[]
                {
                    (functionNameInElm: "Main.evaluateExpressionInProject",
                    publicName: "evaluateExpressionInProject",
                    arity: 1),
                };

            var javascriptPreparedToRun =
                ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                    javascriptMinusCrashes,
                    listFunctionToPublish);

            using (var javascriptEngine = ProcessHostedWithChakraCore.ConstructJsEngine())
            {
                var initAppResult = javascriptEngine.Evaluate(javascriptPreparedToRun);

                var responseJson =
                    javascriptEngine.CallFunction("evaluateExpressionInProject", argumentsJson)
                    ?.ToString();

                var responseStructure =
                    Newtonsoft.Json.JsonConvert.DeserializeObject<EvaluateExpressionResponseStructure>(
                        responseJson);

                if (responseStructure.DecodedArguments == null)
                    throw new Exception("Failed to decode arguments: " + responseStructure.FailedToDecodeArguments);

                if (responseStructure.DecodedArguments.Evaluated == null)
                    throw new Exception("Failed to evaluate: " + responseStructure.DecodedArguments.FailedToEvaluate);

                return responseStructure.DecodedArguments.Evaluated.valueAsJsonString;
            }
        }

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> ParseElmSyntaxAppCodeFiles() =>
            ImmutableDictionary<IImmutableList<string>, IImmutableList<byte>>.Empty
            .WithComparers(EnumerableExtension.EqualityComparer<string>())
            .SetItem(ImmutableList.Create("elm.json"), GetManifestResourceStreamContent("elm_fullstack.ElmEngine.evaluate_elm_program.elm.json").ToImmutableList())
            .SetItem(ImmutableList.Create("src", "ElmEvaluation.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmEngine.evaluate_elm_program.src.ElmEvaluation.elm").ToImmutableList())
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

        class EvaluateExpressionResponseStructure
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
            public string valueAsJsonString = null;

            public string typeText = null;
        }
    }
}