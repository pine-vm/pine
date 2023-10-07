using System;
using System.Collections.Concurrent;

namespace Pine.CompilePineToDotNet;

public class CompilerMutableCache
{
    readonly ConcurrentDictionary<PineValue, Result<string, PineVM.Expression>> decodeExpressionFromValueCache = new();

    readonly ConcurrentDictionary<PineValue, ReadOnlyMemory<byte>> valueHashCache = new();

    public Result<string, PineVM.Expression> DecodeExpressionFromValue(PineValue pineValue) =>
        decodeExpressionFromValueCache.GetOrAdd(
            pineValue,
            valueFactory: PineVM.PineVM.DecodeExpressionFromValueDefault);

    public ReadOnlyMemory<byte> ComputeHash(PineValue pineValue) =>
        valueHashCache
        .GetOrAdd(
            pineValue,
            valueFactory: PineValueHashTree.ComputeHash);
}
