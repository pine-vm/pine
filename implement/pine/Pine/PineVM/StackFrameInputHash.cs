using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Security.Cryptography;

namespace Pine.PineVM;

public class StackFrameInputHash
{
    public record HashWithStats(
        ReadOnlyMemory<byte> HashBytes,
        long HashedBytesCount);

    private readonly Dictionary<PineValue, ReadOnlyMemory<byte>> _valueHashCache = [];

    private readonly Dictionary<StaticFunctionInterface, ReadOnlyMemory<byte>> _functionInterfaceHashCache = [];

    public HashWithStats
        ComposeHashBytes(
        StackFrameInput stackFrameInput)
    {
        var paramHashBytes =
            GetOrComputeHashForFunctionInterface(stackFrameInput.Parameters);

        var argumentsHashes =
            new ReadOnlyMemory<byte>[stackFrameInput.Arguments.Count];

        var aggregateBytesCount = paramHashBytes.Length;

        long aggregateHashedBytesCount = 0;

        for (var i = 0; i < stackFrameInput.Arguments.Count; i++)
        {
            var argValue =
                stackFrameInput.Arguments[i];

            var (argHashBytes, freshEncodingBytes) =
                GetOrComputeHashForValue(argValue.Evaluate());

            argumentsHashes[i] = argHashBytes;

            aggregateBytesCount += argHashBytes.Length;
            aggregateHashedBytesCount += freshEncodingBytes;
        }

        var hashSourceBytes = new byte[aggregateBytesCount];

        var offset = 0;

        paramHashBytes.Span.CopyTo(hashSourceBytes.AsSpan(offset));

        offset += paramHashBytes.Length;

        for (var i = 0; i < argumentsHashes.Length; i++)
        {
            var argumentHashBytes = argumentsHashes[i];

            argumentHashBytes.Span.CopyTo(hashSourceBytes.AsSpan(offset));

            offset += argumentHashBytes.Length;
        }

        aggregateHashedBytesCount += hashSourceBytes.Length;

        var finalHashBytes = SHA256.HashData(hashSourceBytes);

        return new HashWithStats(new ReadOnlyMemory<byte>(finalHashBytes), aggregateHashedBytesCount);
    }

    public HashWithStats
        GetOrComputeHashForValue(
        PineValue value)
    {
        if (_valueHashCache.TryGetValue(value, out var cachedHash))
        {
            return new HashWithStats(cachedHash, 0);
        }

        var (hashBytes, encodingBytesLength) =
            PineValueHashFlat.ComputeHashForValue(value);

        _valueHashCache[value] = hashBytes;

        return new HashWithStats(hashBytes, encodingBytesLength);
    }

    public ReadOnlyMemory<byte> GetOrComputeHashForFunctionInterface(
        StaticFunctionInterface functionInterface)
    {
        if (_functionInterfaceHashCache.TryGetValue(functionInterface, out var cachedHash))
        {
            return cachedHash;
        }

        var hashBytes =
            HashBytes(functionInterface);

        _functionInterfaceHashCache[functionInterface] = hashBytes;

        return hashBytes;
    }

    public static ReadOnlyMemory<byte> HashBytes(
        StaticFunctionInterface functionInterface)
    {
        var asString =
            StaticFunctionInterface.HashString(functionInterface);

        return System.Text.Encoding.UTF8.GetBytes(asString);
    }
}
