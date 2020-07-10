using System;
using System.Collections.Generic;
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
            IImmutableList<string> evaluationRootFilePath,
            string evaluationRootDeclarationName)
        {
            var parsedModule = GetParsedModule(appCodeTree, evaluationRootFilePath);

            var entryPointDeclaration =
                parsedModule.declarations
                .FirstOrDefault(d => d?.value?.function?.declaration?.value?.name?.value == evaluationRootDeclarationName);

            if (entryPointDeclaration == null)
                throw new Exception("Did not find the declaration for the entry point '" + evaluationRootDeclarationName + "'.");

            var onlyLiteral = entryPointDeclaration?.value?.function?.declaration?.value?.expression?.value?.literal;

            if (onlyLiteral == null)
                throw new NotImplementedException("This case is not implemented yet.");

            if (!long.TryParse(onlyLiteral, out var _))
            {
                // It is not an integer, assume it is a string literal.
                return Newtonsoft.Json.JsonConvert.SerializeObject(onlyLiteral);
            }

            return onlyLiteral;
        }

        static ElmSyntaxJson.File GetParsedModule(
            Composition.TreeComponent appCodeTree,
            IImmutableList<string> filePath)
        {
            var appCodeFiles =
                ElmApp.ToFlatDictionaryWithPathComparer(
                    appCodeTree.EnumerateBlobsTransitive()
                    .Select(file =>
                        (filePath: (IImmutableList<string>)file.path.Select(pathComponent => Encoding.UTF8.GetString(pathComponent.ToArray())).ToImmutableList(),
                        content: file.blobContent)));

            var mainElmModuleFromAppCodeTree =
                appCodeFiles[filePath];

            var mainElmModuleText = Encoding.UTF8.GetString(mainElmModuleFromAppCodeTree.ToArray());

            var parseElmAppCodeFiles = elm_fullstack.ElmEngine.EvaluateElm.ParseElmSyntaxAppCodeFiles();

            var javascriptFromElmMake =
                ProcessFromElm019Code.CompileElmToJavascript(
                    parseElmAppCodeFiles,
                    ImmutableList.Create("src", "Main.elm"));

            var javascriptMinusCrashes = ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

            var listFunctionToPublish =
                new[]
                {
                        (functionNameInElm: "Main.parseElmModuleTextToJson",
                        publicName: "parseElmModuleTextToJson",
                        arity: 1),
                };

            var javascriptPreparedToRun =
                ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                    javascriptMinusCrashes,
                    listFunctionToPublish);

            using (var javascriptEngine = ProcessHostedWithChakraCore.ConstructJsEngine())
            {
                var initAppResult = javascriptEngine.Evaluate(javascriptPreparedToRun);

                var parseResultAsString =
                    javascriptEngine.CallFunction("parseElmModuleTextToJson", mainElmModuleText)
                    ?.ToString();

                var parseResult = Newtonsoft.Json.JsonConvert.DeserializeObject<ParseResultJson>(parseResultAsString);

                if (parseResult?.Ok == null)
                    throw new Exception("Failed to parse Main.elm module text: " + parseResult.Err);

                return parseResult.Ok;
            }
        }

        class ParseResultJson
        {
            public string Err;

            public ElmSyntaxJson.File Ok;
        }

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> ParseElmSyntaxAppCodeFiles() =>
            ImmutableDictionary<IImmutableList<string>, IImmutableList<byte>>.Empty
            .WithComparers(EnumerableExtension.EqualityComparer<string>())
            .SetItem(ImmutableList.Create("elm.json"), GetManifestResourceStreamContent("elm_fullstack.ElmEngine.parse_elm_syntax.elm.json").ToImmutableList())
            .SetItem(ImmutableList.Create("src", "Main.elm"), GetManifestResourceStreamContent("elm_fullstack.ElmEngine.parse_elm_syntax.src.Main.elm").ToImmutableList());

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
    }

    namespace ElmSyntaxJson
    {
        /*
        The types here are named based on the implementation on the Elm side: https://github.com/stil4m/elm-syntax/tree/783e85f051ac0259078dadf1cb948c8cc9a27413/src
        */

        public class File
        {
            public IReadOnlyList<Node<Declaration>> declarations;
        }

        // https://github.com/stil4m/elm-syntax/blob/783e85f051ac0259078dadf1cb948c8cc9a27413/src/Elm/Syntax/Declaration.elm#L62-L87
        public class Declaration
        {
            public string type;

            public DeclarationFunction function;
        }

        // https://github.com/stil4m/elm-syntax/blob/783e85f051ac0259078dadf1cb948c8cc9a27413/src/Elm/Syntax/Expression.elm#L364-L370
        public class DeclarationFunction
        {
            public Node<FunctionDeclaration> declaration;
        }

        public class FunctionDeclaration
        {
            public Node<string> name;

            public Node<Expression> expression;
        }

        public class Expression
        {
            public string type;

            public string literal;
        }

        // https://github.com/stil4m/elm-syntax/blob/783e85f051ac0259078dadf1cb948c8cc9a27413/src/Elm/Syntax/Node.elm#L69-L74
        public class Node<T>
        {
            public int[] range;

            public T value;
        }
    }
}