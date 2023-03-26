using Jint.Native.Object;
using Jint.Native;
using Jint;
using System;
using System.Reflection;

namespace ElmTime;

internal class JintInterop
{
    static public ObjectInstance NewDataViewFromArrayBuffer(Engine engine, ObjectInstance arrayBuffer)
    {
        return engine.Construct("DataView", arrayBuffer);
    }

    static public ObjectInstance NewArrayBufferFromBytes(Engine engine, ReadOnlyMemory<byte> bytes)
    {
        var arrayBuffer = NewArrayBufferFromLength(engine, bytes.Length);

        var bufferBytes = GetBytesOfArrayBuffer(arrayBuffer);

        bytes.CopyTo(bufferBytes);

        return arrayBuffer;
    }

    static public ObjectInstance NewArrayBufferFromLength(Engine engine, int length)
    {
        return engine.Construct("ArrayBuffer", length);
    }

    static public ObjectInstance GetArrayBufferOfDataView(JsValue objectInstance)
    {
        var argumentType = objectInstance.GetType();

        var propertyArrayBuffer =
            argumentType.GetField(
                "_viewedArrayBuffer",
                bindingAttr: BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);

        return (ObjectInstance)propertyArrayBuffer.GetValue(objectInstance);
    }

    static public byte[] GetBytesOfArrayBuffer(JsValue objectInstance)
    {
        var argumentType = objectInstance.GetType();

        var propertyArrayBufferData =
            argumentType.GetProperty(
                "ArrayBufferData",
                bindingAttr: BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);

        var bufferBytes = (byte[])propertyArrayBufferData.GetMethod.Invoke(objectInstance, null);

        return bufferBytes;
    }
}
