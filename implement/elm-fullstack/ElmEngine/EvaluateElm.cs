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

            var entryFunctionDeclaration =
                evaluationContext.FindBoundSyntaxByName(evaluationRootDeclarationName)?.FunctionDeclaration;

            if (entryFunctionDeclaration == null)
                throw new Exception("Did not find the function declaration for the entry point '" + evaluationRootDeclarationName + "'.");

            return EvaluateExpression(
                evaluationContext,
                expression: entryFunctionDeclaration?.value?.expression?.value).AsJsonString();
        }

        static public ElmValue EvaluateExpression(
            EvaluationContext evaluationContext,
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

            if (expression.application != null)
                return EvaluateApplication(evaluationContext, expression.application);

            throw new Exception("Unsupported expression type: " + expression.type);
        }

        static public ElmValue EvaluateOperatorApplication(
            EvaluationContext evaluationContext,
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
            EvaluationContext evaluationContext,
            ElmSyntaxJson.FunctionOrValueExpression functionOrValue)
        {
            if (0 < functionOrValue.moduleName.Count)
                throw new NotImplementedException("Reaching in other modules is not implemented yet.");

            var boundSyntax =
                evaluationContext.FindBoundSyntaxByName(functionOrValue.name);

            if (boundSyntax == null)
                throw new NotImplementedException("Did not find bound syntax for '" + functionOrValue.name + "'. Import resolution is not implemented yet.");

            if (boundSyntax.Closure != null)
                return EvaluateExpression(
                    boundSyntax.Closure.evaluationContext,
                    boundSyntax.Closure.expression?.value);

            if (boundSyntax.FunctionDeclaration != null)
            {
                if (0 < boundSyntax.FunctionDeclaration?.value.arguments?.Count)
                    throw new NotImplementedException("Function arguments are not implemented yet.");

                return EvaluateExpression(
                    evaluationContext,
                    boundSyntax.FunctionDeclaration?.value?.expression?.value);
            }

            throw new Exception("Unexpected type of bound syntax.");
        }

        static public ElmValue EvaluateLet(
            EvaluationContext evaluationContext,
            ElmSyntaxJson.LetExpression let)
        {
            return EvaluateExpression(
                WithDeclarationsAdded(evaluationContext, let.declarations),
                let.expression.value);
        }

        static public ElmValue EvaluateApplication(
            EvaluationContext evaluationContext,
            IReadOnlyList<ElmSyntaxJson.Node<ElmSyntaxJson.Expression>> application)
        {
            var functionReference = application.First().value.functionOrValue;

            if (functionReference == null)
                throw new NotImplementedException(
                    "Unexpected type of first element in application: " + application.First().value.type);

            var functionDeclaration =
                evaluationContext.FindBoundSyntaxByName(functionReference.name)?.FunctionDeclaration;

            if (functionDeclaration == null)
                throw new NotImplementedException("Did not find function declaration for '" + functionReference.name + "'. Import resolution is not implemented yet.");

            var bindings =
                functionDeclaration.value.arguments
                .Select((argument, argumentIndex) =>
                {
                    if (argument.value.var != null)
                    {
                        var argumentExpression = application.ElementAtOrDefault(argumentIndex + 1);

                        if (argumentExpression == null)
                            throw new NotImplementedException("Partial application is not implemented yet.");

                        var closure = new Closure
                        {
                            evaluationContext = evaluationContext,
                            expression = argumentExpression,
                        };

                        return (name: argument.value.var.value, boundSyntax: new BoundSyntax { Closure = closure });
                    }

                    throw new NotImplementedException();
                })
                .ToImmutableDictionary(
                    argumentNameAndBoundSyntax => argumentNameAndBoundSyntax.name,
                    argumentNameAndBoundSyntax => argumentNameAndBoundSyntax.boundSyntax);

            return EvaluateExpression(
                evaluationContext.withBindings(bindings),
                functionDeclaration.value.expression.value);
        }

        static EvaluationContext EvaluationContextFromDeclarations(
            IReadOnlyList<ElmSyntaxJson.Node<ElmSyntaxJson.Declaration>> declarations) =>
            WithDeclarationsAdded(
                new EvaluationContext(),
                declarations
                .Select(declaration => declaration.value.function?.declaration)
                .WhereNotNull()
                .ToImmutableList());

        static EvaluationContext WithDeclarationsAdded(
            EvaluationContext parent,
            IReadOnlyList<ElmSyntaxJson.Node<ElmSyntaxJson.Declaration>> declarations) =>
            WithDeclarationsAdded(parent, declarations.Select(declaration =>
            {
                if (declaration.value.function != null)
                    return declaration.value.function.declaration;

                throw new NotImplementedException("Type of declaration not implemented: " + declaration.value.type);
            }).ToImmutableList());

        static EvaluationContext WithDeclarationsAdded(
            EvaluationContext parent,
            IReadOnlyList<ElmSyntaxJson.Node<ElmSyntaxJson.FunctionDeclaration>> declarations) =>
            parent.withBindings(declarations.ToImmutableDictionary(
                functionDeclaration => functionDeclaration.value.name.value,
                functionDeclaration => new BoundSyntax { FunctionDeclaration = functionDeclaration }));

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

        public class EvaluationContext
        {
            public EvaluationContext parent;

            IImmutableDictionary<string, BoundSyntax> bindings;

            public EvaluationContext withBinding(string name, BoundSyntax expression) =>
                withBindings(ImmutableDictionary<string, BoundSyntax>.Empty.SetItem(name, expression));

            public EvaluationContext withBindings(IImmutableDictionary<string, BoundSyntax> bindings)
            {
                return new EvaluationContext
                {
                    parent = this,
                    bindings = bindings,
                };
            }

            public BoundSyntax FindBoundSyntaxByName(string name)
            {
                if (bindings.TryGetValue(name, out var expression))
                    return expression;

                return parent?.FindBoundSyntaxByName(name);
            }
        }

        public class BoundSyntax
        {
            public ElmSyntaxJson.Node<ElmSyntaxJson.FunctionDeclaration> FunctionDeclaration;

            public Closure Closure;
        }

        public class Closure
        {
            public EvaluationContext evaluationContext;

            public ElmSyntaxJson.Node<ElmSyntaxJson.Expression> expression;
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
            .SetItem(ImmutableList.Create("elm.json"), GetManifestResourceStreamContent("elm_fullstack.ElmEngine.evaluate_elm_program.elm.json").ToImmutableList())
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

            public FunctionExpression function;
        }

        // https://github.com/stil4m/elm-syntax/blob/783e85f051ac0259078dadf1cb948c8cc9a27413/src/Elm/Syntax/Expression.elm#L364-L370
        public class FunctionExpression
        {
            public Node<FunctionDeclaration> declaration;
        }

        // https://github.com/stil4m/elm-syntax/blob/783e85f051ac0259078dadf1cb948c8cc9a27413/src/Elm/Syntax/Expression.elm#L373-L379
        public class FunctionDeclaration
        {
            public Node<string> name;

            public IReadOnlyList<Node<Pattern>> arguments;

            public Node<Expression> expression;
        }

        // https://github.com/stil4m/elm-syntax/blob/783e85f051ac0259078dadf1cb948c8cc9a27413/src/Elm/Syntax/Pattern.elm#L126-L230
        public class Pattern
        {
            public string type;

            public VarPattern var;
        }

        public class VarPattern
        {
            public string value;
        }

        // https://github.com/stil4m/elm-syntax/blob/783e85f051ac0259078dadf1cb948c8cc9a27413/src/Elm/Syntax/Expression.elm#L226-L315
        public class Expression
        {
            public string type;

            public string literal;

            public OperatorApplicationExpression operatorapplication;

            public FunctionOrValueExpression functionOrValue;

            public LetExpression let;

            public IReadOnlyList<Node<Expression>> application;
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