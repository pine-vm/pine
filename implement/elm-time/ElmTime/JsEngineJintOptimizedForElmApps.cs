using Jint;
using Jint.Native;
using Jint.Native.Object;
using Jint.Runtime;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Text;
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
            // https://github.com/danfishgold/base64-bytes/blob/ee966331d3819f56244145ed485ab13b0dc4f45a/src/Decode.elm#L8-L10
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
            hostFunc: (_, _, arguments) =>
            {
                var argument = arguments.Single();

                return Convert.ToBase64String(Get_array_buffer_bytes(argument));
            });

        yield return new FunctionDelegateIntoHost(
            // https://github.com/danfishgold/base64-bytes/blob/ee966331d3819f56244145ed485ab13b0dc4f45a/src/Encode.elm#L8-L10
            delegatedJavaScriptFunctionName: "$danfishgold$base64_bytes$Encode$toBytes",
            // toBytes : String -> Maybe Bytes
            buildWrapperJavaScript: (hostFuncName, originalExpression) => new Esprima.Ast.Identifier(hostFuncName),
            hostFunc: (engine, _, arguments) =>
            {
                var argumentString = arguments.Single().AsString();

                var bytes = new byte[argumentString.Length];

                if (!Convert.TryFromBase64String(argumentString, bytes, out var bytesWritten))
                {
                    return ElmMaybeNothing(engine);
                }

                var arrayBuffer = ArrayBufferFromBytes(engine, bytes.AsSpan()[..bytesWritten].ToArray());

                return ElmMaybeJust(engine, DataViewFromArrayBuffer(engine, arrayBuffer));
            });

        yield return new FunctionDelegateIntoHost(
            // https://github.com/elm/bytes/blob/2bce2aeda4ef18c3dcccd84084647d22a7af36a6/src/Elm/Kernel/Bytes.js#L74-L85
            delegatedJavaScriptFunctionName: "_Bytes_getStringWidth",
            buildWrapperJavaScript: (hostFuncName, originalExpression) => new Esprima.Ast.Identifier(hostFuncName),
            hostFunc: (_, _, arguments) =>
            {
                var argument = arguments.Single();

                var argumentString = argument.AsString();

                return new JsNumber(Encoding.UTF8.GetByteCount(argumentString));
            });

        yield return new FunctionDelegateIntoHost(
            // https://github.com/elm/bytes/blob/2bce2aeda4ef18c3dcccd84084647d22a7af36a6/src/Elm/Kernel/Bytes.js#L87-L124
            delegatedJavaScriptFunctionName: "_Bytes_write_string",
            buildWrapperJavaScript: (hostFuncName, originalExpression) =>
            {
                if (originalExpression is not Esprima.Ast.CallExpression originalCallExpression)
                {
                    return (Esprima.Ast.Expression)originalExpression;
                }

                if (originalCallExpression.Callee is not Esprima.Ast.Identifier originalCallExpressionCallee)
                {
                    return originalCallExpression;
                }

                if (originalCallExpressionCallee.Name != "F3")
                    return originalCallExpression;

                return
                originalCallExpression
                .UpdateWith(
                    callee: originalCallExpression.Callee,
                    arguments: Esprima.Ast.NodeList.Create<Esprima.Ast.Expression>(
                        new[] { new Esprima.Ast.Identifier(hostFuncName) }));
            },
            hostFunc: (_, _, arguments) =>
            {
                var destDataView = (ObjectInstance)arguments.ElementAt(0);

                var offset = (int)arguments.ElementAt(1).AsNumber();

                var argumentString = arguments.ElementAt(2).AsString();

                var utf8String = Encoding.UTF8.GetBytes(argumentString);

                var destArrayBuffer = Get_DataView_array_buffer(destDataView);

                var destBufferBytes = Get_array_buffer_bytes(destArrayBuffer);

                Buffer.BlockCopy(utf8String, 0, destBufferBytes, offset, utf8String.Length);

                return new JsNumber(offset + utf8String.Length);
            });

        yield return new FunctionDelegateIntoHost(
            // https://github.com/elm/bytes/blob/2bce2aeda4ef18c3dcccd84084647d22a7af36a6/src/Elm/Kernel/Bytes.js#L152-L182
            delegatedJavaScriptFunctionName: "_Bytes_read_string",
            buildWrapperJavaScript: (hostFuncName, originalExpression) =>
            {
                if (originalExpression is not Esprima.Ast.CallExpression originalCallExpression)
                {
                    return (Esprima.Ast.Expression)originalExpression;
                }

                if (originalCallExpression.Callee is not Esprima.Ast.Identifier originalCallExpressionCallee)
                {
                    return originalCallExpression;
                }

                if (originalCallExpressionCallee.Name != "F3")
                    return originalCallExpression;

                return
                originalCallExpression
                .UpdateWith(
                    callee: originalCallExpression.Callee,
                    arguments: Esprima.Ast.NodeList.Create<Esprima.Ast.Expression>(
                        new[] { new Esprima.Ast.Identifier(hostFuncName) }));
            },
            hostFunc: (_, _, arguments) =>
            {
                var len = (int)arguments.ElementAt(0).AsNumber();

                var sourceDataView = (ObjectInstance)arguments.ElementAt(1);

                var offset = (int)arguments.ElementAt(2).AsNumber();

                var sourceArrayBuffer = Get_DataView_array_buffer(sourceDataView);

                var sourceBufferBytes = Get_array_buffer_bytes(sourceArrayBuffer);

                var readString = Encoding.UTF8.GetString(sourceBufferBytes, offset, len);

                var newOffset = offset + len;

                var elmTuple = new JsObject(sourceDataView.Engine);

                elmTuple.FastSetDataProperty("$", new JsString("#2"));
                elmTuple.FastSetDataProperty("a", newOffset);
                elmTuple.FastSetDataProperty("b", readString);

                return elmTuple;
            });
    }

    static JsObject ElmMaybeNothing(Engine engine)
    {
        // var $elm$core$Maybe$Nothing = {$: 'Nothing'};

        var jsObject = new JsObject(engine);

        jsObject.FastSetDataProperty("$", "Nothing");

        return jsObject;
    }

    static JsObject ElmMaybeJust(Engine engine, JsValue just)
    {
        /*
        var $elm$core$Maybe$Just = function (a) {
            return {$: 'Just', a: a};
        };
         * */

        var jsObject = new JsObject(engine);

        jsObject.FastSetDataProperty("$", "Just");
        jsObject.FastSetDataProperty("a", just);

        return jsObject;
    }

    static ObjectInstance DataViewFromArrayBuffer(Engine engine, ObjectInstance arrayBuffer)
    {
        return engine.Construct("DataView", arrayBuffer);
    }

    static ObjectInstance ArrayBufferFromBytes(Engine engine, ReadOnlyMemory<byte> bytes)
    {
        var arrayBuffer = ArrayBufferFromLength(engine, bytes.Length);

        var bufferBytes = Get_array_buffer_bytes(arrayBuffer);

        bytes.CopyTo(bufferBytes);

        return arrayBuffer;
    }

    static ObjectInstance ArrayBufferFromLength(Engine engine, int length)
    {
        return engine.Construct("ArrayBuffer", length);
    }

    static ObjectInstance Get_DataView_array_buffer(JsValue objectInstance)
    {
        var argumentType = objectInstance.GetType();

        var propertyArrayBuffer =
            argumentType.GetField(
                "_viewedArrayBuffer",
                bindingAttr: BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);

        return (ObjectInstance)propertyArrayBuffer.GetValue(objectInstance);
    }

    static byte[] Get_array_buffer_bytes(JsValue objectInstance)
    {
        var argumentType = objectInstance.GetType();

        var propertyArrayBufferData =
            argumentType.GetProperty(
                "ArrayBufferData",
                bindingAttr: BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);

        var bufferBytes = (byte[])propertyArrayBufferData.GetMethod.Invoke(objectInstance, null);

        return bufferBytes;
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
