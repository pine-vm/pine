using Jint;
using Jint.Native;
using Jint.Runtime;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;

namespace ElmTime;

public class JsEngineJint : IJsEngine
{
    readonly Engine engine = new();

    readonly IReadOnlyList<FunctionDelegateIntoHost> functionDelegatesIntoHost;

    public JsEngineJint()
        :
        this(DefaultShortcutsIntoHost)
    {
    }

    public JsEngineJint(IReadOnlyList<FunctionDelegateIntoHost>? functionDelegatesIntoHost)
    {
        this.functionDelegatesIntoHost = functionDelegatesIntoHost ?? Array.Empty<FunctionDelegateIntoHost>();

        foreach (var functionDelegate in this.functionDelegatesIntoHost)
        {
            var functionJint = new DelegatingFunctionInstance(
                engine,
                engine.Realm,
                name: new JsString("delegating_" + functionDelegate.delegatedJavaScriptFunctionName + "_into_host"),
                func: functionDelegate.hostFunc);

            engine.SetValue(functionDelegate.delegatedJavaScriptFunctionName + "_host", functionJint);
        }
    }

    static public JsEngineJint Create() => new();

    public object CallFunction(string functionName, params object[] args)
    {
        return engine.Invoke(functionName, args);
    }

    public object Evaluate(string expression)
    {
        var expressionWithDelegateToHost = expression;

        foreach (var functionDelegateIntoHost in functionDelegatesIntoHost)
        {
            expressionWithDelegateToHost =
                PatchForFunctionDelegate(
                    functionDelegateIntoHost,
                    hostFunctionName: functionDelegateIntoHost.delegatedJavaScriptFunctionName + "_host",
                    originalExpression: expressionWithDelegateToHost);
        }

        var jintValue = engine.Evaluate(expressionWithDelegateToHost);

        var dotnetValue = CastToDotnetType(jintValue);

        return dotnetValue;
    }

    static string PatchForFunctionDelegate(
        FunctionDelegateIntoHost functionDelegateIntoHost,
        string hostFunctionName,
        string originalExpression)
    {
        var regexPattern =
            "var\\s+" + Regex.Escape(functionDelegateIntoHost.delegatedJavaScriptFunctionName) + "\\s*=\\s*(function.*?\n\\};)";

        var regexMatch = Regex.Match(originalExpression, regexPattern, RegexOptions.Singleline);

        if (!regexMatch.Success)
            return originalExpression;

        var beforeFunctionDeclaration = originalExpression[..regexMatch.Groups[1].Index];
        var afterFunctionDeclaration = originalExpression[(regexMatch.Groups[1].Index + regexMatch.Groups[1].Length)..];

        var replacement = functionDelegateIntoHost.wrapperJavaScript(hostFunctionName);

        var expression =
            beforeFunctionDeclaration +
            replacement +
            PatchForFunctionDelegate(
                functionDelegateIntoHost,
                hostFunctionName,
                afterFunctionDeclaration);

        return expression;
    }

    static object CastToDotnetType(JsValue jintValue)
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

    public record FunctionDelegateIntoHost(
        string delegatedJavaScriptFunctionName,
        Func<string, string> wrapperJavaScript,
        Func<JsValue, JsValue[], JsValue> hostFunc);

    static IReadOnlyList<FunctionDelegateIntoHost> DefaultShortcutsIntoHost = BuildDefaultShortcutsIntoHost().ToImmutableList();

    static IEnumerable<FunctionDelegateIntoHost> BuildDefaultShortcutsIntoHost()
    {
        yield return new FunctionDelegateIntoHost(
            // <https://github.com/danfishgold/base64-bytes/blob/ee966331d3819f56244145ed485ab13b0dc4f45a/src/Decode.elm#L8-L10>
            delegatedJavaScriptFunctionName: "$danfishgold$base64_bytes$Decode$fromBytes",
            wrapperJavaScript: hostFuncName => "function (dataView) { return $elm$core$Maybe$Just(" + hostFuncName + "(dataView.buffer)); }",
            hostFunc: (_, arguments) =>
            {
                var argument = arguments.Single();

                return new JsString(Base64_from_arrayBuffer(argument));
            });
    }

    static string Base64_from_arrayBuffer(object argumentFromJs)
    {
        var asJintObject = (Jint.Native.Object.ObjectInstance)argumentFromJs;

        var argumentType = asJintObject.GetType();

        var propertyArrayBufferData =
            argumentType.GetProperty(
                "ArrayBufferData",
                bindingAttr: BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);

        var bufferBytes = (byte[])propertyArrayBufferData.GetMethod.Invoke(argumentFromJs, null);

        return Convert.ToBase64String(bufferBytes);
    }

    class DelegatingFunctionInstance : Jint.Native.Function.FunctionInstance
    {
        readonly Func<JsValue, JsValue[], JsValue> func;

        public DelegatingFunctionInstance(
            Engine engine,
            Realm realm,
            JsString? name,
            Func<JsValue, JsValue[], JsValue> func)
            : base(engine, realm, name)
        {
            this.func = func;
        }

        protected override JsValue Call(JsValue thisObject, JsValue[] arguments)
        {
            return func(thisObject, arguments);
        }
    }
}
