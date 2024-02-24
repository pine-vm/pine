using Jint;
using Jint.Native;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Runtime.CompilerServices;

namespace ElmTime.JavaScript;

public class JavaScriptEngineJint : IJavaScriptEngine
{
    public record FunctionDelegateIntoHost(
        string delegatedJavaScriptFunctionName,
        Func<string, Esprima.Ast.Node, Esprima.Ast.Expression> buildWrapperJavaScript,
        Func<Engine, JsValue, JsValue[], JsValue> hostFunc,
        int parameterCount);

    private readonly Engine engine = new(new Options { Json = new JsonOptions { MaxParseDepth = 1_000 } });

    private readonly IReadOnlyList<FunctionDelegateIntoHost> functionDelegatesIntoHost;

    private readonly IReadOnlyDictionary<string, Func<Esprima.Ast.Node, Esprima.Ast.Expression>> evalAstRewriterDeclarationReplacements;

    public JavaScriptEngineJint(IReadOnlyList<FunctionDelegateIntoHost>? functionDelegatesIntoHost)
    {
        this.functionDelegatesIntoHost = functionDelegatesIntoHost ?? [];

        foreach (var functionDelegate in this.functionDelegatesIntoHost)
        {
            var functionJint = BuildDelegatingFunctionInstance(
                engine,
                name: "delegating_" + functionDelegate.delegatedJavaScriptFunctionName + "_into_host",
                func: (jsThis, jsArguments) => functionDelegate.hostFunc(engine, jsThis, jsArguments),
                parameterCount: functionDelegate.parameterCount);

            engine.SetValue(functionDelegate.delegatedJavaScriptFunctionName + "_delegate_to_host", functionJint);
        }

        evalAstRewriterDeclarationReplacements =
            this.functionDelegatesIntoHost
            .ToImmutableDictionary(
                keySelector: functionDelegate => functionDelegate.delegatedJavaScriptFunctionName,
                elementSelector: functionDelegate =>
                new Func<Esprima.Ast.Node, Esprima.Ast.Expression>(originalExpression =>
                    functionDelegate.buildWrapperJavaScript(
                        functionDelegate.delegatedJavaScriptFunctionName + "_delegate_to_host",
                        originalExpression))
                );
    }

    public object CallFunction(string functionName, params object[] args)
    {
        return engine.Invoke(functionName, args);
    }

    public object Evaluate(string expression)
    {
        var parser = new Esprima.JavaScriptParser();

        var parsedExpression = parser.ParseScript(expression);

        var rewriter = new AstRewriter(evalAstRewriterDeclarationReplacements);

        var rewrittenAst = rewriter.VisitAndConvert(parsedExpression, allowNull: false);

        var jintValue = engine.Evaluate(rewrittenAst);

        var dotnetValue = CastToDotnetType(jintValue);

        return dotnetValue;
    }

    private static object CastToDotnetType(JsValue jintValue)
    {
        if (jintValue is JsString jsString)
            return jsString.AsString();

        if (jintValue is JsNumber jsNumber)
            return jsNumber.AsNumber();

        return jintValue;
    }

    public void Dispose()
    {
    }

    private class AstRewriter(
        IReadOnlyDictionary<string, Func<Esprima.Ast.Node, Esprima.Ast.Expression>> declarationReplacements)
        : Esprima.Utils.AstRewriter
    {
        public override T VisitAndConvert<T>(T node, bool allowNull = false, [CallerMemberName] string? callerName = null)
        {
            /*
             * Elm compiler emits code as follows:
             * var $danfishgold$base64_bytes$Decode$fromBytes = function (bytes) {
             * ...
             * */
            if (node is Esprima.Ast.VariableDeclarator variableDeclarator)
            {
                if (variableDeclarator.Id is Esprima.Ast.Identifier identifier)
                {
                    if (declarationReplacements.TryGetValue(identifier.Name, out var buildReplacement))
                    {
                        var replacement = buildReplacement(variableDeclarator.Init);

                        return variableDeclarator.UpdateWith(identifier, init: replacement) as T;
                    }
                }
            }

            if (node is Esprima.Ast.FunctionDeclaration functionDeclaration)
            {
                if (declarationReplacements.TryGetValue(functionDeclaration.Id.Name, out var buildReplacement))
                {
                    var replacement = buildReplacement(functionDeclaration);

                    return new Esprima.Ast.VariableDeclaration(
                        Esprima.Ast.NodeList.Create(
                            [new Esprima.Ast.VariableDeclarator(functionDeclaration.Id, replacement)]),
                        Esprima.Ast.VariableDeclarationKind.Var)
                        as T;
                }
            }

            return base.VisitAndConvert(node, allowNull, callerName);
        }
    }

    public static Jint.Native.Function.FunctionInstance BuildDelegatingFunctionInstance(
        Engine engine,
        string name,
        Func<JsValue, JsValue[], JsValue> func,
        int parameterCount) =>
        new Jint.Runtime.Interop.ClrFunctionInstance(engine, name, func, length: parameterCount);
}
