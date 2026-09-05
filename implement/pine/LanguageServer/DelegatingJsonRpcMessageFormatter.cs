using StreamJsonRpc;
using StreamJsonRpc.Protocol;
using StreamJsonRpc.Reflection;
using System;
using System.Buffers;
using System.Text;

namespace Pine.LanguageServer;

public class DelegatingJsonRpcMessageFormatter(
    SystemTextJsonFormatter inner,
    Action<string>? logDelegate = null)
    : IJsonRpcMessageFormatter,
      IJsonRpcMessageTextFormatter,
      IJsonRpcFormatterState,
      IJsonRpcInstanceContainer,
      IJsonRpcMessageFactory,
      IJsonRpcFormatterTracingCallbacks,
      IDisposable
{
    public Encoding Encoding
    {
        get => inner.Encoding;
        set => inner.Encoding = value;
    }

    public RequestId SerializingMessageWithId =>
        ((IJsonRpcFormatterState)inner).SerializingMessageWithId;

    public RequestId DeserializingMessageWithId =>
        ((IJsonRpcFormatterState)inner).DeserializingMessageWithId;

    public bool SerializingRequest =>
        ((IJsonRpcFormatterState)inner).SerializingRequest;

    public JsonRpc? Rpc
    {
        set => ((IJsonRpcInstanceContainer)inner).Rpc = value;
    }

    public JsonRpcMessage Deserialize(ReadOnlySequence<byte> contentBuffer)
    {
        var message = inner.Deserialize(contentBuffer);
        LogReceived(message);
        return message;
    }

    public JsonRpcMessage Deserialize(ReadOnlySequence<byte> contentBuffer, Encoding encoding)
    {
        var message = inner.Deserialize(contentBuffer, encoding);
        LogReceived(message);
        return message;
    }

    private void LogReceived(JsonRpcMessage message)
    {
        if (message is JsonRpcRequest request)
        {
            var idDescription =
                request.RequestId is { } requestId
                ?
                "id " + requestId
                :
                "notification";

            logDelegate?.Invoke("RPC message received: " + request.Method + " (" + idDescription + ")");
        }
    }

    public object GetJsonText(JsonRpcMessage message) =>
        inner.GetJsonText(message);

    public void Serialize(IBufferWriter<byte> bufferWriter, JsonRpcMessage message) =>
        inner.Serialize(bufferWriter, message);

    public JsonRpcRequest CreateRequestMessage() =>
        ((IJsonRpcMessageFactory)inner).CreateRequestMessage();

    public JsonRpcError CreateErrorMessage() =>
        ((IJsonRpcMessageFactory)inner).CreateErrorMessage();

    public JsonRpcResult CreateResultMessage() =>
        ((IJsonRpcMessageFactory)inner).CreateResultMessage();

    public void OnSerializationComplete(JsonRpcMessage message, ReadOnlySequence<byte> buffer) =>
        ((IJsonRpcFormatterTracingCallbacks)inner).OnSerializationComplete(message, buffer);

    public void Dispose() =>
        inner.Dispose();
}
