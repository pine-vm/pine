using Pine.Core.Functional;
using System;
using System.Buffers;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Pine.Core.Encodings;

public static partial class ValueEncodingBinaryDeterministic
{
    /// <summary>
    /// Supplies retained representations for the shared reader. Implement as a struct to avoid
    /// boxing. An external Eval specialization may consume a shallow environment directly.
    /// </summary>
    public interface IDecoding<TNode, TItem>
        where TNode : class
        where TItem : class
    {
        /// <summary>Creates a retained blob representation, possibly owning the immutable source slice.</summary>
        TNode Blob(ReadOnlyMemory<byte> bytes);

        /// <summary>Creates a retained list. Its child array is owned, never pooled.</summary>
        TNode List(TNode[] items);

        /// <summary>Resolves an ordinary external reference, or returns null when absent.</summary>
        TNode? ResolveExternal(ReadOnlyMemory<byte> reference);

        /// <summary>Evaluates fully decoded expression and environment nodes without a result-wrapper allocation.</summary>
        ValueResult<string, TNode> Evaluate(TNode expression, TNode environment);

        /// <summary>Preserves the adapter's structured evaluation rejection.</summary>
        DecodeError Rejected(long offset, TNode expression, TNode environment, string reason);

        /// <summary>Enables the restricted external-expression path instead of normal argument decoding.</summary>
        bool HasExternalEvaluation { get; }

        /// <summary>Maximum nesting of specialized reader calls; normal iterative traversal does not recurse.</summary>
        int MaximumDepth { get; }

        /// <summary>Dispatches a whitelisted expression hash and consumes its environment directly.</summary>
        TNode EvaluateExternal(ref TreeDecoder<TItem> decoder, ReadOnlyMemory<byte> reference);
    }

    /// <summary>A shared wire-format failure, with its original structured error.</summary>
    public sealed class DecodeException(DecodeError error) : Exception(error.ToString())
    {
        /// <summary>The original structured wire-format failure.</summary>
        public DecodeError Error { get; } = error;
    }

    [InlineArray(16)]
    private struct InlineEntries<T>
    {
        private T _first;
    }

    private struct ReferenceEntry<T>
    {
        public int _address;

        public T? _value;
    }

    private struct PooledStack<T>
    {
        private InlineEntries<T> _inline;

        private T[]? _items;

        public int Count { get; private set; }

        [UnscopedRef]
        public ref T Peek()
        {
            if (_items is { } items)
                return ref items[Count - 1];

            return ref _inline[Count - 1];
        }

        public void Push(T value)
        {
            if (Count == (_items?.Length ?? 16))
            {
                var next = ArrayPool<T>.Shared.Rent(checked(Count * 2));

                for (var index = 0; index < Count; ++index)
                    next[index] = _items is { } items ? items[index] : _inline[index];

                if (_items is { } previous)
                    ArrayPool<T>.Shared.Return(previous, clearArray: true);

                _inline = default;
                _items = next;
            }

            if (_items is { } array)
                array[Count++] = value;

            else
                _inline[Count++] = value;
        }

        public void Pop()
        {
            Peek() = default!;
            --Count;
        }

        public void Dispose()
        {
            if (_items is { } items)
                ArrayPool<T>.Shared.Return(items, clearArray: true);

            _items = null;
            _inline = default;
            Count = 0;
        }
    }

    /// <summary>
    /// A reusable-scope reader. References identify completed entries by byte address, not by
    /// the target type. Dispose returns temporary reference storage and clears retained objects.
    /// </summary>
    public ref struct TreeDecoder<TItem> where TItem : class
    {
        private readonly ReadOnlyMemory<byte> _source;

        private InlineEntries<ReferenceEntry<TItem>> _inline;

        private ReferenceEntry<TItem>[]? _entries;

        private int _count;

        private int _capacity;

        private int _depth;

        private bool _disposed;

        /// <summary>Starts a fresh scope, indexing reference targets without materializing values.</summary>
        public TreeDecoder(ReadOnlyMemory<byte> source)
        {
            _source = source;
            var initialized = false;

            try
            {
                ScanReferenceTargets();
                initialized = true;
            }
            finally
            {
                if (!initialized)
                    Dispose();
            }
        }

        /// <summary>Consumed extent, permitting embedding and multiple roots.</summary>
        public int BytesRead { readonly get; private set; }

        /// <summary>Bytes still available in the enclosing input.</summary>
        public readonly int Remaining => _source.Length - BytesRead;

        /// <summary>Inspects the next tag without advancing the cursor.</summary>
        public readonly int PeekTag()
        {
            if (_disposed)
                throw new ObjectDisposedException(nameof(TreeDecoder<>));

            if (Remaining < 4)
                throw new DecodeException(new DecodeError.UnexpectedEndOfInput(BytesRead, "tag"));

            return BinaryPrimitives.ReadInt32BigEndian(_source.Span.Slice(BytesRead, 4));
        }

        private int Int32(string expected)
        {
            if (_disposed)
                throw new ObjectDisposedException(nameof(TreeDecoder<>));

            if (Remaining < 4)
                throw new DecodeException(new DecodeError.UnexpectedEndOfInput(BytesRead, expected));

            var value = BinaryPrimitives.ReadInt32BigEndian(_source.Span.Slice(BytesRead, 4));
            BytesRead += 4;
            return value;
        }

        [UnscopedRef]
        private ref ReferenceEntry<TItem> Entry(int index)
        {
            if (_entries is { } entries)
                return ref entries[index];

            return ref _inline[index];
        }

        private static int Slot(int address, int capacity)
        {
            // Mix aligned byte offsets before power-of-two probing.
            var hash = (uint)address;
            hash = unchecked((hash ^ (hash >> 16)) * 0x7feb352d);
            hash = unchecked((hash ^ (hash >> 15)) * 0x846ca68b);
            return (int)((hash ^ (hash >> 16)) & (uint)(capacity - 1));
        }

        private int Find(int address)
        {
            if (_count is 0)
                return -1;

            var capacity = _entries is null ? 16 : _capacity;
            var index = Slot(address, capacity);

            while (Entry(index)._address is not 0)
            {
                if (Entry(index)._address == address + 1)
                    return index;

                index = (index + 1) & (capacity - 1);
            }

            return -1;
        }

        private void AddTarget(int address)
        {
            if (Find(address) >= 0)
                return;

            var capacity = _entries is null ? 16 : _capacity;

            if (_count == capacity * 3 / 4)
            {
                var nextCapacity = checked(capacity * 2);
                var next = ArrayPool<ReferenceEntry<TItem>>.Shared.Rent(nextCapacity);
                System.Array.Clear(next);

                for (var index = 0; index < capacity; ++index)
                {
                    var entry = Entry(index);

                    if (entry._address is 0)
                        continue;

                    var slot = Slot(entry._address - 1, nextCapacity);

                    while (next[slot]._address is not 0)
                        slot = (slot + 1) & (nextCapacity - 1);

                    next[slot] = entry;
                }

                var previous = _entries;
                _inline = default;
                _entries = next;
                _capacity = nextCapacity;

                if (previous is not null)
                    ArrayPool<ReferenceEntry<TItem>>.Shared.Return(previous, clearArray: true);

                capacity = nextCapacity;
            }

            var destination = Slot(address, capacity);

            while (Entry(destination)._address is not 0)
                destination = (destination + 1) & (capacity - 1);

            Entry(destination) = new() { _address = address + 1 };
            ++_count;
        }

        private void ScanReferenceTargets()
        {
            // Enclosing bytes may follow the valid codec prefix. Only actual reads validate
            // input and invoke callbacks; this scan merely identifies entries worth retaining.
            var source = _source.Span;
            var position = 0;

            while (source.Length - position >= 4)
            {
                var tag = BinaryPrimitives.ReadInt32BigEndian(source[position..]);

                if (tag is TagEval)
                {
                    position += 4;
                    continue;
                }

                if (tag is TagReferenceExternal256)
                {
                    if (source.Length - position < 4 + ReferenceExternal256Length)
                        return;

                    position += 4 + ReferenceExternal256Length;
                    continue;
                }

                if (source.Length - position < 8)
                    return;

                var argument = BinaryPrimitives.ReadInt32BigEndian(source[(position + 4)..]);

                if (tag is TagReferenceInteral)
                {
                    var target = (long)position + 4 + argument;

                    if (target >= 0 && target <= source.Length - 4)
                        AddTarget((int)target);

                    position += 8;
                }
                else if (tag is TagList)
                {
                    if (argument < 0 || argument > (source.Length - position - 8) / 8)
                        return;

                    position += 8;
                }
                else if (tag is TagBlob)
                {
                    var padded = ((long)argument + 3) & ~3L;

                    if (argument < 0 || padded > source.Length - position - 8)
                        return;

                    position += 8 + (int)padded;
                }
                else
                    return;
            }
        }

        private void Complete<TNode>(int index, TNode value) where TNode : class
        {
            if (value is not TItem item)
                throw new InvalidOperationException("Decoded node is not a reference-table item.");

            if (index >= 0)
                Entry(index)._value = item;
        }

        private TNode Reference<TNode>(int address) where TNode : class
        {
            var addressBase = BytesRead;
            var relative = Int32("relative address of reference");
            var target = (long)addressBase + relative;

            if (target >= 0 && target <= int.MaxValue)
            {
                var index = Find((int)target);

                if (index >= 0 && Entry(index)._value is TNode value)
                    return value;
            }

            throw new DecodeException(new DecodeError.InvalidInternalReference(address, target));
        }

        private int Length(int address, int tag, string expected)
        {
            var length = Int32(expected);

            if (length < 0)
                throw new DecodeException(new DecodeError.NegativeLength(address, tag, length));

            var needed = tag is TagBlob ? ((long)length + 3) & ~3L : (long)length * 8;

            if (needed > Remaining)
                throw new DecodeException(new DecodeError.LengthExceedsRemainingInput(address, tag, length, Remaining));

            return length;
        }

        /// <summary>
        /// Reads an unreferenceable inline constructor environment. Profiles using this API must
        /// neither emit nor admit references to the skipped environment or its raw scalar fields.
        /// </summary>
        public int ReadInlineListLength()
        {
            var address = BytesRead;

            if (Int32("tag") is not TagList)
                throw Invalid("Expected an inline list");

            return Length(address, TagList, "item count of list");
        }

        /// <summary>Reads a raw, unreferenceable blob slice without constructing a node.</summary>
        public ReadOnlyMemory<byte> ReadInlineBlob()
        {
            var address = BytesRead;

            if (Int32("tag") is not TagBlob)
                throw Invalid("Expected an inline blob");

            return BlobBytes(address);
        }

        private ReadOnlyMemory<byte> BlobBytes(int address)
        {
            var length = Length(address, TagBlob, "byte count of blob");
            var result = _source.Slice(BytesRead, length);
            BytesRead += (int)(((long)length + 3) & ~3L);
            return result;
        }

        /// <summary>Builds a profile-specific failure with the current byte offset.</summary>
        public readonly InvalidDataException Invalid(string reason) =>
            new(reason + " at offset " + BytesRead + ".");

        private ReadOnlyMemory<byte> ExternalReference()
        {
            if (Remaining < ReferenceExternal256Length)
            {
                throw new DecodeException(
                    new DecodeError.UnexpectedEndOfInput(BytesRead, "reference of external reference"));
            }

            var reference = _source.Slice(BytesRead, ReferenceExternal256Length);
            BytesRead += ReferenceExternal256Length;
            return reference;
        }

        private struct Pending<TNode> where TNode : class
        {
            public int _referenceIndex;

            public int _address;

            public int _tag;

            public TNode[]? _items;

            public int _next;

            public TNode? _expression;
        }

        /// <summary>
        /// Reads a node with a statically dispatched adapter. Normal lists/Evals are iterative;
        /// specialized external Evals may read their retained fields through this same cursor.
        /// </summary>
        public TNode Read<TNode, TAdapter>(TAdapter adapter)
            where TNode : class
            where TAdapter : struct, IDecoding<TNode, TItem>
        {
            ++_depth;
            var pending = new PooledStack<Pending<TNode>>();

            try
            {
                if (_depth > adapter.MaximumDepth)
                    throw Invalid("Specialized tree nesting exceeds " + adapter.MaximumDepth + " levels");

                while (true)
                {
                    var address = BytesRead;
                    var tag = Int32("tag");
                    TNode completed;

                    if (tag is TagReferenceInteral)
                        completed = Reference<TNode>(address);

                    else if (tag is TagBlob)
                    {
                        var index = Find(address);
                        completed = adapter.Blob(BlobBytes(address));
                        Complete(index, completed);
                    }
                    else if (tag is TagReferenceExternal256)
                    {
                        var index = Find(address);
                        var reference = ExternalReference();

                        completed =
                            adapter.ResolveExternal(reference) ??
                            throw new DecodeException(new DecodeError.ExternalReferenceNotFound(address, reference));

                        Complete(index, completed);
                    }
                    else if (tag is TagList)
                    {
                        var index = Find(address);
                        var length = Length(address, tag, "item count of list");

                        if (length is not 0)
                        {
                            pending.Push(
                                new()
                                {
                                    _referenceIndex = index,
                                    _address = address,
                                    _tag = tag,
                                    _items = new TNode[length]
                                });

                            continue;
                        }

                        completed = adapter.List([]);
                        Complete(index, completed);
                    }
                    else if (tag is TagEval)
                    {
                        var index = Find(address);

                        if (adapter.HasExternalEvaluation)
                        {
                            if (Int32("external expression tag") is not TagReferenceExternal256)
                                throw Invalid("Expected an external constructor expression");

                            var reference = ExternalReference();
                            completed = adapter.EvaluateExternal(ref this, reference);
                            Complete(index, completed);
                        }
                        else
                        {
                            pending.Push(
                                new()
                                {
                                    _referenceIndex = index,
                                    _address = address,
                                    _tag = tag
                                });

                            continue;
                        }
                    }
                    else
                        throw new DecodeException(new DecodeError.UnknownTag(address, tag));

                    while (true)
                    {
                        if (pending.Count is 0)
                            return completed;

                        var parent = pending.Peek();

                        if (parent._tag is TagList)
                        {
                            parent._items![parent._next++] = completed;

                            if (parent._next < parent._items.Length)
                            {
                                pending.Peek() = parent;
                                break;
                            }

                            completed = adapter.List(parent._items);
                        }
                        else if (parent._next is 0)
                        {
                            parent._expression = completed;
                            parent._next = 1;
                            pending.Peek() = parent;
                            break;
                        }
                        else
                        {
                            var evaluated = adapter.Evaluate(parent._expression!, completed);

                            if (evaluated.IsErr)
                            {
                                throw new DecodeException(
                                    adapter.Rejected(
                                        parent._address,
                                        parent._expression!,
                                        completed,
                                        evaluated.Error));
                            }

                            completed = evaluated.Value;
                        }

                        pending.Pop();
                        Complete(parent._referenceIndex, completed);
                    }
                }
            }
            finally
            {
                pending.Dispose();
                --_depth;
            }
        }

        /// <summary>Releases scratch arrays and clears retained references. The cursor cannot be reused.</summary>
        public void Dispose()
        {
            if (_entries is { } entries)
                ArrayPool<ReferenceEntry<TItem>>.Shared.Return(entries, clearArray: true);

            _entries = null;
            _inline = default;
            _count = 0;
            _capacity = 0;
            _disposed = true;
        }
    }

    private static class DictionaryPool<T> where T : notnull
    {
        private static Dictionary<T, long>? s_available;

        private static Dictionary<T, long>? s_second;

        public static Dictionary<T, long> Rent() =>
            Interlocked.Exchange(ref s_available, null) ??
            Interlocked.Exchange(ref s_second, null) ?? [];

        public static void Return(Dictionary<T, long> entries)
        {
            var count = entries.Count;
            entries.Clear();

            if (count <= 4096 && Interlocked.CompareExchange(ref s_available, entries, null) is not null)
                Interlocked.CompareExchange(ref s_second, entries, null);
        }
    }

    /// <summary>
    /// Shared streaming entry writer. No node descriptors or canonical values are required.
    /// Optional bounded dictionary reuse is intended for short-lived syntax sessions.
    /// </summary>
    /// <remarks>Starts a fresh scope; optional dictionary reuse retains at most two bounded tables.</remarks>
    public struct TreeEncoder<TItem>(Action<ReadOnlySpan<byte>> write, bool reuseReferences = false) : IDisposable where TItem : notnull
    {
        private readonly Action<ReadOnlySpan<byte>> _write = write;

        private readonly bool _reuseReferences = reuseReferences;

        private Dictionary<TItem, long>? _nodes;

        private Dictionary<TItem, long>? _blobs;

        private bool _disposed;

        /// <summary>Extent of bytes synchronously delivered to the destination.</summary>
        public long BytesWritten { readonly get; private set; }

        internal readonly void EnsureActive()
        {
            if (_disposed)
                throw new ObjectDisposedException(nameof(TreeEncoder<>));
        }

        private void Bytes(ReadOnlySpan<byte> bytes)
        {
            EnsureActive();

            _write(bytes);
            BytesWritten += bytes.Length;
        }

        private void Int32(int value)
        {
            Span<byte> bytes = stackalloc byte[4];
            BinaryPrimitives.WriteInt32BigEndian(bytes, value);
            Bytes(bytes);
        }

        private bool Reference(ref Dictionary<TItem, long>? entries, TItem key)
        {
            EnsureActive();

            entries ??= _reuseReferences ? DictionaryPool<TItem>.Rent() : [];

            if (entries.TryGetValue(key, out var address))
            {
                var relative = address - (BytesWritten + TagSize);

                if (relative < int.MinValue)
                    throw new InvalidOperationException("Relative address exceeds the range of a 32-bit integer: " + relative);

                Int32(TagReferenceInteral);
                Int32((int)relative);
                return true;
            }

            entries.Add(key, BytesWritten);
            return false;
        }

        /// <summary>Writes an earlier-node reference, or records the start of the next new node.</summary>
        public bool TryReference(TItem item) => Reference(ref _nodes, item);

        /// <summary>The same operation in the blob reference domain, preserving Pine's original dictionary layout.</summary>
        public bool TryBlobReference(TItem item) => Reference(ref _blobs, item);

        /// <summary>Writes a list header; the caller then emits exactly count children.</summary>
        public void StartList(int count)
        {
            ArgumentOutOfRangeException.ThrowIfNegative(count);

            Int32(TagList);
            Int32(count);
        }

        /// <summary>Writes an Eval, external expression hash, and inline environment header.</summary>
        public void StartExternalEval(ReadOnlySpan<byte> reference, int argumentCount)
        {
            if (reference.Length is not ReferenceExternal256Length)
                throw new ArgumentException("An external reference must contain exactly 32 bytes.", nameof(reference));

            Int32(TagEval);
            Int32(TagReferenceExternal256);
            Bytes(reference);
            StartList(argumentCount);
        }

        /// <summary>Streams a blob and its alignment padding without copying the payload.</summary>
        public void Blob(ReadOnlySpan<byte> bytes)
        {
            Int32(TagBlob);
            Int32(bytes.Length);
            Bytes(bytes);
            Padding(bytes.Length);
        }

        private void Padding(int length)
        {
            var count = (-length) & 3;

            if (count is 0)
                return;

            Span<byte> padding = stackalloc byte[3];
            padding.Clear();
            Bytes(padding[..count]);
        }

        /// <summary>Streams canonical Pine UTF-32 scalars through a bounded stack buffer.</summary>
        public void Utf32(string value)
        {
            var scalarCount = 0;

            for (var index = 0; index < value.Length; ++index)
            {
                _ = char.ConvertToUtf32(value, index);
                ++scalarCount;

                if (char.IsHighSurrogate(value[index]))
                    ++index;
            }

            var length = checked(scalarCount * 4);
            Int32(TagBlob);
            Int32(length);
            Span<byte> buffer = stackalloc byte[4096];
            var position = 0;

            for (var index = 0; index < value.Length; ++index)
            {
                if (position == buffer.Length)
                {
                    Bytes(buffer);
                    position = 0;
                }

                BinaryPrimitives.WriteInt32BigEndian(buffer[position..], char.ConvertToUtf32(value, index));
                position += 4;

                if (char.IsHighSurrogate(value[index]))
                    ++index;
            }

            if (position is not 0)
                Bytes(buffer[..position]);
        }

        /// <summary>Writes a Pine signed integer directly from its magnitude, without boxing or a Pine blob.</summary>
        public void Integer(BigInteger value)
        {
            var magnitude = BigInteger.Abs(value);
            var count = magnitude.GetByteCount(isUnsigned: true);
            Int32(TagBlob);
            Int32(checked(count + 1));
            Span<byte> sign = stackalloc byte[1];
            sign[0] = value.Sign < 0 ? (byte)2 : (byte)4;
            Bytes(sign);
            byte[]? rented = null;
            var buffer = count <= 256 ? stackalloc byte[256] : (rented = ArrayPool<byte>.Shared.Rent(count));

            try
            {
                if (!magnitude.TryWriteBytes(buffer, out var written, isUnsigned: true, isBigEndian: true))
                    throw new InvalidOperationException("Failed writing an integer.");

                Bytes(buffer[..written]);
                Padding(count + 1);
            }
            finally
            {
                if (rented is not null)
                    ArrayPool<byte>.Shared.Return(rented);
            }
        }

        /// <summary>Clears reference ownership, returning eligible dictionaries to the bounded pool.</summary>
        public void Dispose()
        {
            if (_reuseReferences)
            {
                if (_nodes is { } nodes)
                    DictionaryPool<TItem>.Return(nodes);

                if (_blobs is { } blobs)
                    DictionaryPool<TItem>.Return(blobs);
            }

            _nodes = null;
            _blobs = null;
            _disposed = true;
        }
    }
}
