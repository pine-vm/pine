using Jint;
using Jint.Native;
using Jint.Native.Object;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using static ElmTime.JavaScript.JavaScriptEngineJint;

namespace ElmTime.JavaScript;

public class JavaScriptEngineJintOptimizedForElmApps
{
    public static JavaScriptEngineJint Create() =>
        new(DelegatesIntoHost);

    private static readonly IReadOnlyList<FunctionDelegateIntoHost> DelegatesIntoHost = BuildDelegatesIntoHost().ToImmutableList();

    private static IEnumerable<FunctionDelegateIntoHost> BuildDelegatesIntoHost()
    {
        yield return new FunctionDelegateIntoHost(
            // https://github.com/danfishgold/base64-bytes/blob/ee966331d3819f56244145ed485ab13b0dc4f45a/src/Decode.elm#L8-L10
            delegatedJavaScriptFunctionName: "$danfishgold$base64_bytes$Decode$fromBytes",
            buildWrapperJavaScript: (hostFuncName, _) =>
            {
                var dataViewParamName = "dataView";

                var getBufferExpression = new Esprima.Ast.StaticMemberExpression(
                    new Esprima.Ast.Identifier(dataViewParamName),
                    new Esprima.Ast.Identifier("buffer"),
                    optional: false);

                var callToHostExpression =
                new Esprima.Ast.CallExpression(
                    new Esprima.Ast.Identifier(hostFuncName),
                    Esprima.Ast.NodeList.Create<Esprima.Ast.Expression>([getBufferExpression]),
                    optional: false);

                var wrapInMaybeJustExpression =
                new Esprima.Ast.CallExpression(
                    new Esprima.Ast.Identifier("$elm$core$Maybe$Just"),
                    Esprima.Ast.NodeList.Create<Esprima.Ast.Expression>([callToHostExpression]),
                    optional: false);

                var wholeFunctionBody = new Esprima.Ast.BlockStatement(
                    Esprima.Ast.NodeList.Create<Esprima.Ast.Statement>(
                        [new Esprima.Ast.ReturnStatement(wrapInMaybeJustExpression)]));

                var wholeFunctionExpression = new Esprima.Ast.FunctionExpression(
                    id: null,
                    parameters: Esprima.Ast.NodeList.Create<Esprima.Ast.Node>([new Esprima.Ast.Identifier(dataViewParamName)]),
                    body: wholeFunctionBody,
                    generator: false,
                    strict: true,
                    async: false);

                return wholeFunctionExpression;
            },
            hostFunc: (_, _, arguments) =>
            {
                var argument = arguments.Single();

                return Convert.ToBase64String(JintInterop.GetBytesOfArrayBuffer(argument));
            });

        yield return new FunctionDelegateIntoHost(
            // https://github.com/danfishgold/base64-bytes/blob/ee966331d3819f56244145ed485ab13b0dc4f45a/src/Encode.elm#L8-L10
            delegatedJavaScriptFunctionName: "$danfishgold$base64_bytes$Encode$toBytes",
            // toBytes : String -> Maybe Bytes
            buildWrapperJavaScript: (hostFuncName, _) => new Esprima.Ast.Identifier(hostFuncName),
            hostFunc: (engine, _, arguments) =>
            {
                var argumentString = arguments.Single().AsString();

                var bytes = new byte[argumentString.Length];

                if (!Convert.TryFromBase64String(argumentString, bytes, out var bytesWritten))
                {
                    return ElmInteropJint.ElmMaybeNothing(engine);
                }

                var arrayBuffer = JintInterop.NewArrayBufferFromBytes(engine, bytes.AsSpan()[..bytesWritten].ToArray());

                return ElmInteropJint.ElmMaybeJust(engine, JintInterop.NewDataViewFromArrayBuffer(engine, arrayBuffer));
            });

        yield return new FunctionDelegateIntoHost(
            // https://github.com/elm/bytes/blob/2bce2aeda4ef18c3dcccd84084647d22a7af36a6/src/Elm/Kernel/Bytes.js#L74-L85
            delegatedJavaScriptFunctionName: "_Bytes_getStringWidth",
            buildWrapperJavaScript: (hostFuncName, _) => new Esprima.Ast.Identifier(hostFuncName),
            hostFunc: (_, _, arguments) =>
            {
                var argument = arguments.Single();

                var argumentString = argument.AsString();

                return new JsNumber(Encoding.UTF8.GetByteCount(argumentString));
            });

        yield return new FunctionDelegateIntoHost(
            // https://github.com/elm/bytes/blob/2bce2aeda4ef18c3dcccd84084647d22a7af36a6/src/Elm/Kernel/Bytes.js#L58-L69
            delegatedJavaScriptFunctionName: "_Bytes_write_bytes",
            buildWrapperJavaScript: (hostFuncName, originalExpression) =>
            ElmInteropJint.AstDelegateInElmF3(originalExpression, hostFuncName)
            .WithDefaultBuilder(() => (Esprima.Ast.Expression)originalExpression),
            hostFunc: (_, _, arguments) =>
            {
                var destDataView = (ObjectInstance)arguments.ElementAt(0);

                var offset = (int)arguments.ElementAt(1).AsNumber();

                var sourceDataView = arguments.ElementAt(2);

                var sourceArrayBuffer = JintInterop.GetArrayBufferOfDataView(sourceDataView);

                var sourceBytes = JintInterop.GetBytesOfArrayBuffer(sourceArrayBuffer);

                var destArrayBuffer = JintInterop.GetArrayBufferOfDataView(destDataView);

                var destBufferBytes = JintInterop.GetBytesOfArrayBuffer(destArrayBuffer);

                Buffer.BlockCopy(sourceBytes, 0, destBufferBytes, offset, sourceBytes.Length);

                return new JsNumber(offset + sourceBytes.Length);
            });

        yield return new FunctionDelegateIntoHost(
            // https://github.com/elm/bytes/blob/2bce2aeda4ef18c3dcccd84084647d22a7af36a6/src/Elm/Kernel/Bytes.js#L87-L124
            delegatedJavaScriptFunctionName: "_Bytes_write_string",
            buildWrapperJavaScript: (hostFuncName, originalExpression) =>
            ElmInteropJint.AstDelegateInElmF3(originalExpression, hostFuncName)
            .WithDefaultBuilder(() => (Esprima.Ast.Expression)originalExpression),
            hostFunc: (_, _, arguments) =>
            {
                var destDataView = (ObjectInstance)arguments.ElementAt(0);

                var offset = (int)arguments.ElementAt(1).AsNumber();

                var argumentString = arguments.ElementAt(2).AsString();

                var utf8String = Encoding.UTF8.GetBytes(argumentString);

                var destArrayBuffer = JintInterop.GetArrayBufferOfDataView(destDataView);

                var destBufferBytes = JintInterop.GetBytesOfArrayBuffer(destArrayBuffer);

                Buffer.BlockCopy(utf8String, 0, destBufferBytes, offset, utf8String.Length);

                return new JsNumber(offset + utf8String.Length);
            });

        yield return new FunctionDelegateIntoHost(
            // https://github.com/elm/bytes/blob/2bce2aeda4ef18c3dcccd84084647d22a7af36a6/src/Elm/Kernel/Bytes.js#L152-L182
            delegatedJavaScriptFunctionName: "_Bytes_read_string",
            buildWrapperJavaScript: (hostFuncName, originalExpression) =>
            ElmInteropJint.AstDelegateInElmF3(originalExpression, hostFuncName)
            .WithDefaultBuilder(() => (Esprima.Ast.Expression)originalExpression),
            hostFunc: (_, _, arguments) =>
            {
                var len = (int)arguments.ElementAt(0).AsNumber();

                var sourceDataView = (ObjectInstance)arguments.ElementAt(1);

                var offset = (int)arguments.ElementAt(2).AsNumber();

                var sourceArrayBuffer = JintInterop.GetArrayBufferOfDataView(sourceDataView);

                var sourceBufferBytes = JintInterop.GetBytesOfArrayBuffer(sourceArrayBuffer);

                var readString = Encoding.UTF8.GetString(sourceBufferBytes, offset, len);

                var newOffset = offset + len;

                return ElmInteropJint.NewElmTuple2(sourceDataView.Engine, newOffset, readString);
            });

        yield return new FunctionDelegateIntoHost(
            // https://github.com/folkertdev/elm-sha2/blob/2a106ca6850c3f8197e02c7e6a3e3797f599e87a/src/SHA256.elm#L103-L105
            delegatedJavaScriptFunctionName: "$folkertdev$elm_sha2$SHA256$fromBytes",
            buildWrapperJavaScript: (hostFuncName, _) => new Esprima.Ast.Identifier(hostFuncName),
            hostFunc: (engine, _, arguments) =>
            {
                var argumentDataView = arguments[0];

                var buffer = JintInterop.GetArrayBufferOfDataView(argumentDataView);

                var bytes = JintInterop.GetBytesOfArrayBuffer(buffer);

                var sha256 = SHA256.HashData(bytes);

                var integersForElmTuple8 = new uint[8];

                Buffer.BlockCopy(sha256, 0, integersForElmTuple8, 0, sha256.Length);

                return
                ElmInteropJint.ElmChoiceTypeTag(
                    engine,
                    "Digest",
                    ElmInteropJint.ElmChoiceTypeTag(
                        engine,
                        "Tuple8",
                        integersForElmTuple8.Select(i => new JsNumber(i)).ToArray()));
            });

        yield return new FunctionDelegateIntoHost(
            // https://github.com/folkertdev/elm-sha2/blob/2a106ca6850c3f8197e02c7e6a3e3797f599e87a/src/SHA256.elm#L67-L69
            delegatedJavaScriptFunctionName: "$folkertdev$elm_sha2$SHA256$fromString",
            buildWrapperJavaScript: (hostFuncName, _) => new Esprima.Ast.Identifier(hostFuncName),
            hostFunc: (engine, _, arguments) =>
            {
                var argumentString = arguments[0].AsString();

                var utf8 = Encoding.UTF8.GetBytes(argumentString);

                var sha256 = SHA256.HashData(utf8);

                var integersForElmTuple8 = new uint[8];

                Buffer.BlockCopy(sha256, 0, integersForElmTuple8, 0, sha256.Length);

                return
                ElmInteropJint.ElmChoiceTypeTag(
                    engine,
                    "Digest",
                    ElmInteropJint.ElmChoiceTypeTag(
                        engine,
                        "Tuple8",
                        integersForElmTuple8.Select(i => new JsNumber(i)).ToArray()));
            });
    }
}
