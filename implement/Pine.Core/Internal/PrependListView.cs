using System;
using System.Collections;
using System.Collections.Generic;

namespace Pine.Core.Internal;

/// <summary>
/// Read-only list view supporting amortized constant-time prepending of a few items, used to represent
/// accumulators built with <c>concat [ [ item ], acc ]</c> (as in <c>List.map</c>) without copying the
/// accumulator on every step.
/// </summary>
/// <remarks>
/// <para>
/// Items are stored in reverse order in a growable array shared between views: a view covers the first
/// <see cref="Count"/> array entries, and its item at index <c>i</c> is stored at <c>Count - 1 - i</c>.
/// Array entries below the count of any existing view are never overwritten, so each view is an
/// immutable snapshot.
/// </para>
/// <para>
/// Prepending to the view that covers all used entries of its buffer appends to the buffer in place.
/// Prepending to any other view (for example an older accumulator reused by another computation) copies
/// the items into a new buffer once. Claiming the buffer for an in-place append is synchronized, so
/// views can be shared between threads.
/// </para>
/// </remarks>
internal sealed class PrependListView : IReadOnlyList<PineValueInProcess>
{
    private sealed class SharedBuffer(PineValueInProcess[] items, int usedCount)
    {
        public PineValueInProcess[] Items = items;

        public int UsedCount = usedCount;
    }

    private readonly SharedBuffer _buffer;

    private readonly PineValueInProcess[] _items;

    public int Count { get; }

    private PrependListView(SharedBuffer buffer, PineValueInProcess[] items, int count)
    {
        _buffer = buffer;
        _items = items;
        Count = count;
    }

    /// <summary>
    /// Returns a list containing the items of <paramref name="front"/> followed by the items of
    /// <paramref name="back"/>.
    /// </summary>
    public static PrependListView Prepend(
        IReadOnlyList<PineValueInProcess> front,
        IReadOnlyList<PineValueInProcess> back)
    {
        if (back is PrependListView backView)
        {
            return backView.Prepend(front);
        }

        var count = back.Count + front.Count;

        var items = new PineValueInProcess[Math.Max(16, count * 2)];

        for (var i = 0; i < back.Count; ++i)
        {
            items[i] = back[back.Count - 1 - i];
        }

        return CreateAfterWritingFront(new SharedBuffer(items, count), items, back.Count, front);
    }

    private PrependListView Prepend(IReadOnlyList<PineValueInProcess> front)
    {
        var newCount = Count + front.Count;

        lock (_buffer)
        {
            if (_buffer.UsedCount == Count)
            {
                var items = _buffer.Items;

                if (items.Length < newCount)
                {
                    var grown = new PineValueInProcess[newCount * 2];

                    Array.Copy(items, grown, Count);

                    items = grown;
                    _buffer.Items = grown;
                }

                _buffer.UsedCount = newCount;

                // The claimed entries at indices [Count, newCount) are not visible to any other view.
                return CreateAfterWritingFront(_buffer, items, Count, front);
            }
        }

        var copiedItems = new PineValueInProcess[newCount * 2];

        Array.Copy(_items, copiedItems, Count);

        return CreateAfterWritingFront(new SharedBuffer(copiedItems, newCount), copiedItems, Count, front);
    }

    private static PrependListView CreateAfterWritingFront(
        SharedBuffer buffer,
        PineValueInProcess[] items,
        int backCount,
        IReadOnlyList<PineValueInProcess> front)
    {
        var newCount = backCount + front.Count;

        for (var i = 0; i < front.Count; ++i)
        {
            items[newCount - 1 - i] = front[i];
        }

        return new PrependListView(buffer, items, newCount);
    }

    public PineValueInProcess this[int index]
    {
        get
        {
            if ((uint)index >= (uint)Count)
                throw new ArgumentOutOfRangeException(nameof(index));

            return _items[Count - 1 - index];
        }
    }

    public IEnumerator<PineValueInProcess> GetEnumerator()
    {
        for (var i = 0; i < Count; ++i)
        {
            yield return _items[Count - 1 - i];
        }
    }

    IEnumerator IEnumerable.GetEnumerator() =>
        GetEnumerator();
}
