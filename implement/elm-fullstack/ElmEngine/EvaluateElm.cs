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

            var evaluationContext =
                EvaluationContextFromDeclarations(parsedModule.parsedModule.declarations);

            var entryPointDeclaration =
                FindFunctionDeclarationByName(evaluationContext, evaluationRootDeclarationName);

            if (entryPointDeclaration == null)
                throw new Exception("Did not find the declaration for the entry point '" + evaluationRootDeclarationName + "'.");

            return EvaluateExpression(
                evaluationContext,
                expression: entryPointDeclaration?.value?.expression?.value).AsJsonString();
        }

        static public ElmSyntaxJson.Node<ElmSyntaxJson.FunctionDeclaration> FindFunctionDeclarationByName(
            IImmutableDictionary<string, ElmSyntaxJson.Node<ElmSyntaxJson.FunctionDeclaration>> evaluationContext,
            string name) =>
            evaluationContext.GetValueOrDefault(name);

        static public ElmValue EvaluateExpression(
            IImmutableDictionary<string, ElmSyntaxJson.Node<ElmSyntaxJson.FunctionDeclaration>> evaluationContext,
            ElmSyntaxJson.Expression expression)
        {
            if (expression.literal != null)
            {
                if (long.TryParse(expression.literal, out var integer))
                {
                    return new ElmValue { IntegerValue = integer };
                }

                return new ElmValue { StringValue = expression.literal };
            }

            if (expression.operatorapplication != null)
                return EvaluateOperatorApplication(evaluationContext, expression.operatorapplication);

            if (expression.functionOrValue != null)
                return EvaluateFunctionOrValue(evaluationContext, expression.functionOrValue);

            if (expression.let != null)
                return EvaluateLet(evaluationContext, expression.let);

            throw new Exception("Unsupported expression type: " + expression.type);
        }

        static public ElmValue EvaluateOperatorApplication(
            IImmutableDictionary<string, ElmSyntaxJson.Node<ElmSyntaxJson.FunctionDeclaration>> evaluationContext,
            ElmSyntaxJson.OperatorApplicationExpression operatorApplication)
        {
            if (operatorApplication.@operator == "++")
            {
                return
                    ElmValue.OperationConcat(
                        left: EvaluateExpression(evaluationContext, operatorApplication.left.value),
                        right: EvaluateExpression(evaluationContext, operatorApplication.right.value));
            }

            throw new Exception("Unsupported operator: '" + operatorApplication.@operator + "'");
        }

        static public ElmValue EvaluateFunctionOrValue(
            IImmutableDictionary<string, ElmSyntaxJson.Node<ElmSyntaxJson.FunctionDeclaration>> evaluationContext,
            ElmSyntaxJson.FunctionOrValueExpression functionOrValue)
        {
            if (0 < functionOrValue.moduleName.Count)
                throw new NotImplementedException("Reaching in other modules is not implemented yet.");

            var declaration =
                FindFunctionDeclarationByName(evaluationContext, functionOrValue.name);

            if (declaration == null)
                throw new Exception("Did not find declaration for '" + functionOrValue.name + "'. This should not compile.");

            return EvaluateExpression(
                evaluationContext,
                declaration?.value?.expression?.value);
        }

        static public ElmValue EvaluateLet(
            IImmutableDictionary<string, ElmSyntaxJson.Node<ElmSyntaxJson.FunctionDeclaration>> evaluationContext,
            ElmSyntaxJson.LetExpression let)
        {
            return EvaluateExpression(
                WithDeclarationsAdded(evaluationContext, let.declarations),
                let.expression.value);
        }

        static IImmutableDictionary<string, ElmSyntaxJson.Node<ElmSyntaxJson.FunctionDeclaration>> EvaluationContextFromDeclarations(
            IReadOnlyList<ElmSyntaxJson.Node<ElmSyntaxJson.Declaration>> declarations) =>
            declarations
            .Select(declaration => declaration.value.function)
            .WhereNotNull()
            .ToImmutableDictionary(
                functionDeclaration => functionDeclaration.declaration.value.name.value,
                functionDeclaration => functionDeclaration.declaration);

        static IImmutableDictionary<string, ElmSyntaxJson.Node<ElmSyntaxJson.FunctionDeclaration>> WithDeclarationsAdded(
            IImmutableDictionary<string, ElmSyntaxJson.Node<ElmSyntaxJson.FunctionDeclaration>> evaluationContext,
            IReadOnlyList<ElmSyntaxJson.Node<ElmSyntaxJson.Declaration>> declarations) =>
            evaluationContext.SetItems(EvaluationContextFromDeclarations(declarations));

        static (string elmSyntaxJson, ElmSyntaxJson.File parsedModule) GetParsedModule(
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

                var elmSyntaxJson =
                    javascriptEngine.CallFunction("parseElmModuleTextToJson", mainElmModuleText)
                    ?.ToString();

                var parseResult = Newtonsoft.Json.JsonConvert.DeserializeObject<ParseResultJson>(elmSyntaxJson);

                if (parseResult?.Ok == null)
                    throw new Exception("Failed to parse Main.elm module text: " + parseResult.Err);

                return (elmSyntaxJson: elmSyntaxJson, parsedModule: parseResult.Ok);
            }
        }

        public class ElmValue
        {
            public string StringValue;

            public Int64? IntegerValue;

            static public ElmValue OperationConcat(ElmValue left, ElmValue right)
            {
                if (left.StringValue != null && right.StringValue != null)
                    return new ElmValue { StringValue = left.StringValue + right.StringValue };

                throw new Exception("Unsupported combination of types for concatenation");
            }

            public string AsJsonString()
            {
                if (StringValue != null)
                    return Newtonsoft.Json.JsonConvert.SerializeObject(StringValue);

                if (IntegerValue != null)
                    return Newtonsoft.Json.JsonConvert.SerializeObject(IntegerValue);

                throw new NotImplementedException();
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

            public IReadOnlyList<Node<FunctionArgument>> arguments;

            public Node<Expression> expression;
        }

        public class FunctionArgument
        {
            public string type;

            public Node<Expression> expression;
        }

        // https://github.com/stil4m/elm-syntax/blob/783e85f051ac0259078dadf1cb948c8cc9a27413/src/Elm/Syntax/Expression.elm#L226-L315
        public class Expression
        {
            public string type;

            public string literal;

            public OperatorApplicationExpression operatorapplication;

            public FunctionOrValueExpression functionOrValue;

            public LetExpression let;
        }

        // https://github.com/stil4m/elm-syntax/blob/551248d79d4b0b2ecbf5bb9b7bbad2f52cf01634/src/Elm/Syntax/Expression.elm#L313-L320
        public class OperatorApplicationExpression
        {
            public string @operator;

            public InfixDirection direction;

            public Node<Expression> left;

            public Node<Expression> right;
        }

        // https://github.com/stil4m/elm-syntax/blob/783e85f051ac0259078dadf1cb948c8cc9a27413/src/Elm/Syntax/Expression.elm#L240-L246
        public class FunctionOrValueExpression
        {
            public IReadOnlyList<string> moduleName;

            public string name;
        }

        // https://github.com/stil4m/elm-syntax/blob/783e85f051ac0259078dadf1cb948c8cc9a27413/src/Elm/Syntax/Expression.elm#L328-L333
        public class LetExpression
        {
            public IReadOnlyList<Node<Declaration>> declarations;

            public Node<Expression> expression;
        }

        // https://github.com/stil4m/elm-syntax/blob/551248d79d4b0b2ecbf5bb9b7bbad2f52cf01634/src/Elm/Syntax/Infix.elm#L54-L66
        public enum InfixDirection
        {
            left = 1, right = 2, non = 3
        }

        // https://github.com/stil4m/elm-syntax/blob/783e85f051ac0259078dadf1cb948c8cc9a27413/src/Elm/Syntax/Node.elm#L69-L74
        public class Node<T>
        {
            public int[] range;

            public T value;
        }
    }
}