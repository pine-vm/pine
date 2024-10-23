using StreamJsonRpc;
using StreamJsonRpc.Protocol;
using System;
using System.Buffers;

namespace Pine.LanguageServer;

public class DelegatingJsonRpcMessageFormatter(
    Func<ReadOnlySequence<byte>, JsonRpcMessage> deserialize,
    Func<JsonRpcMessage, object> getJsonText,
    Action<IBufferWriter<byte>, JsonRpcMessage> serialize)
    : IJsonRpcMessageFormatter
{
    public JsonRpcMessage Deserialize(ReadOnlySequence<byte> contentBuffer)
    {
        return deserialize(contentBuffer);
    }

    public object GetJsonText(JsonRpcMessage message)
    {
        return getJsonText(message);
    }

    public void Serialize(IBufferWriter<byte> bufferWriter, JsonRpcMessage message)
    {
        serialize(bufferWriter, message);
    }
}
