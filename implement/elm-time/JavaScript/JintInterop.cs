using Jint;
using Jint.Native;
using Jint.Native.Object;
using System;
using System.Reflection;

namespace ElmTime.JavaScript;

internal class JintInterop
{
    public static ObjectInstance NewDataViewFromArrayBuffer(Engine engine, ObjectInstance arrayBuffer)
    {
        return engine.Construct("DataView", arrayBuffer);
    }

    public static ObjectInstance NewArrayBufferFromBytes(Engine engine, ReadOnlyMemory<byte> bytes)
    {
        var arrayBuffer = NewArrayBufferFromLength(engine, bytes.Length);

        var bufferBytes = GetBytesOfArrayBuffer(arrayBuffer);

        bytes.CopyTo(bufferBytes);

        return arrayBuffer;
    }

    public static ObjectInstance NewArrayBufferFromLength(Engine engine, int length)
    {
        return engine.Construct("ArrayBuffer", length);
    }

    public static ObjectInstance GetArrayBufferOfDataView(JsValue objectInstance)
    {
        var argumentType = objectInstance.GetType();

        var propertyArrayBuffer =
            argumentType.GetField(
                "_viewedArrayBuffer",
                bindingAttr: BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);

        return (ObjectInstance)propertyArrayBuffer.GetValue(objectInstance);
    }

    public static byte[] GetBytesOfArrayBuffer(JsValue objectInstance)
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
