using Jint;
using Jint.Native;
using Jint.Runtime;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using static ElmTime.JsEngineJint;

namespace ElmTime;

public class JsEngineJintOptimizedForElmApps
{
    static public JsEngineJint Create() =>
        new(DelegatesIntoHost);

    static readonly IReadOnlyList<FunctionDelegateIntoHost> DelegatesIntoHost = BuildDelegatesIntoHost().ToImmutableList();

    static IEnumerable<FunctionDelegateIntoHost> BuildDelegatesIntoHost()
    {
        yield return new FunctionDelegateIntoHost(
            // <https://github.com/danfishgold/base64-bytes/blob/ee966331d3819f56244145ed485ab13b0dc4f45a/src/Decode.elm#L8-L10>
            delegatedJavaScriptFunctionName: "$danfishgold$base64_bytes$Decode$fromBytes",
            buildWrapperJavaScript: (hostFuncName, originalExpression) =>
            {
                var dataViewParamName = "dataView";

                var getBufferExpression = new Esprima.Ast.StaticMemberExpression(
                    new Esprima.Ast.Identifier(dataViewParamName),
                    new Esprima.Ast.Identifier("buffer"),
                    optional: false);

                var callToHostExpression =
                new Esprima.Ast.CallExpression(
                    new Esprima.Ast.Identifier(hostFuncName),
                    Esprima.Ast.NodeList.Create<Esprima.Ast.Expression>(new[] { getBufferExpression }),
                    optional: false);

                var wrapInMaybeJustExpression =
                new Esprima.Ast.CallExpression(
                    new Esprima.Ast.Identifier("$elm$core$Maybe$Just"),
                    Esprima.Ast.NodeList.Create<Esprima.Ast.Expression>(new[] { callToHostExpression }),
                    optional: false);

                var wholeFunctionBody = new Esprima.Ast.BlockStatement(
                    Esprima.Ast.NodeList.Create<Esprima.Ast.Statement>(
                        new[] { new Esprima.Ast.ReturnStatement(wrapInMaybeJustExpression) }));

                var wholeFunctionExpression = new Esprima.Ast.FunctionExpression(
                    id: null,
                    parameters: Esprima.Ast.NodeList.Create<Esprima.Ast.Node>(new[] { new Esprima.Ast.Identifier(dataViewParamName) }),
                    body: wholeFunctionBody,
                    generator: false,
                    strict: true,
                    async: false);

                return wholeFunctionExpression;
            },
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
