using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;

namespace Pine.Core.Encodings;

/// <summary>Deterministic binary Pine trees, also usable through retained-model adapters.</summary>
public static partial class ValueEncodingBinaryDeterministic
{
    /// <summary>All tags are big-endian Int32 values.</summary>
    public const int TagSize = 4;

    /// <summary>A length-prefixed blob, padded to a multiple of four bytes.</summary>
    public const int TagBlob = 1;

    /// <summary>A counted list of encoded children.</summary>
    public const int TagList = 3;

    /// <summary>A backward, relative Int32 reference to a completed entry.</summary>
    public const int TagReferenceInteral = 4;

    /// <summary>A 256-bit reference resolved by the caller.</summary>
    public const int TagReferenceExternal256 = 5;

    /// <summary>An encoded expression followed by its environment.</summary>
    public const int TagEval = 6;

    /// <summary>External references contain exactly this many bytes.</summary>
    public const int ReferenceExternal256Length = 32;

    /// <summary>Encodes a Pine tree without closing the destination.</summary>
    public static void Encode(System.IO.Stream stream, PineValue composition) =>
        Encode(stream.Write, composition);

    /// <summary>Encodes a Pine tree iteratively, preserving the original byte layout.</summary>
    public static void Encode(Action<ReadOnlySpan<byte>> write, PineValue composition)
    {
        using var encoder = new TreeEncoder<PineValue>(write);
        var pending = new Stack<PineValue>();
        pending.Push(composition);

        while (pending.TryPop(out var current))
        {
            switch (current)
            {
                case PineValue.ListValue list:
                    if (encoder.TryReference(list))
                        break;

                    encoder.StartList(list.Items.Length);

                    for (var index = list.Items.Length - 1; index >= 0; --index)
                        pending.Push(list.Items.Span[index]);

                    break;

                case PineValue.BlobValue blob:
                    if (!encoder.TryBlobReference(blob))
                        encoder.Blob(blob.Bytes.Span);

                    break;

                default:
                    throw new NotImplementedException(
                        nameof(Encode) + " does not handle Pine value variant: " + current.GetType().Name);
            }
        }
    }

    /// <summary>
    /// Decodes one root. Each Eval invokes the callback once; internal references reuse completed
    /// results. Callbacks receive fully decoded arguments and use allocation-free ValueResult.
    /// Trailing bytes belong to the enclosing format.
    /// </summary>
    public static Result<DecodeError, PineValue> DecodeRoot(
        ReadOnlyMemory<byte> sourceBytes,
        IReadOnlyDictionary<int, FrozenSet<PineValue.BlobValue>>? blobInstancesToReuse = null,
        IReadOnlyDictionary<int, FrozenSet<PineValue.ListValue>>? listInstancesToReuse = null,
        Func<ReadOnlyMemory<byte>, PineValue?>? resolveExternalReference = null,
        Func<PineValue, PineValue, ValueResult<string, PineValue>>? evaluate = null)
    {
        using var decoder = new TreeDecoder<PineValue>(sourceBytes);

        try
        {
            return
                decoder.Read<PineValue, PineDecoding<PineValue>>(
                    new(blobInstancesToReuse, listInstancesToReuse, resolveExternalReference, evaluate));
        }
        catch (DecodeException error)
        {
            return error.Error;
        }
    }

    /// <summary>Pine factories reusable with a wider reference-table item type.</summary>
    public readonly struct PineDecoding<TItem>(
        IReadOnlyDictionary<int, FrozenSet<PineValue.BlobValue>>? blobs = null,
        IReadOnlyDictionary<int, FrozenSet<PineValue.ListValue>>? lists = null,
        Func<ReadOnlyMemory<byte>, PineValue?>? resolve = null,
        Func<PineValue, PineValue, ValueResult<string, PineValue>>? evaluate = null)
        : IDecoding<PineValue, TItem> where TItem : class
    {
        /// <inheritdoc/>
        public PineValue Blob(ReadOnlyMemory<byte> bytes)
        {
            var value = PineValue.Blob(bytes);

            if (blobs is not null && blobs.TryGetValue(bytes.Length, out var reusable) &&
                reusable.TryGetValue(value, out var reused))
            {
                return reused;
            }

            return value;
        }

        /// <inheritdoc/>
        public PineValue List(PineValue[] items)
        {
            var value = PineValue.List(items);

            if (lists is not null && lists.TryGetValue(items.Length, out var reusable) &&
                reusable.TryGetValue(value, out var reused))
            {
                return reused;
            }

            return value;
        }

        /// <inheritdoc/>
        public PineValue? ResolveExternal(ReadOnlyMemory<byte> reference) => resolve?.Invoke(reference);

        /// <inheritdoc/>
        public ValueResult<string, PineValue> Evaluate(PineValue expression, PineValue environment) =>
            evaluate is null
            ?
            ValueResult<string, PineValue>.err("No evaluation delegate was provided.")
            :
            evaluate(expression, environment);

        /// <inheritdoc/>
        public DecodeError Rejected(long offset, PineValue expression, PineValue environment, string reason) =>
            new DecodeError.EvalRejected(offset, expression, environment, reason);

        /// <inheritdoc/>
        public bool HasExternalEvaluation => false;

        /// <inheritdoc/>
        public int MaximumDepth => int.MaxValue;

        /// <inheritdoc/>
        public PineValue EvaluateExternal(ref TreeDecoder<TItem> decoder, ReadOnlyMemory<byte> reference) =>
            throw new InvalidOperationException("Pine evaluation requires a resolved expression.");
    }
}
