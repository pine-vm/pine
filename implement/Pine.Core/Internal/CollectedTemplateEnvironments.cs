using System;
using System.Collections;
using System.Collections.Generic;

namespace Pine.Core.Internal;

/// <summary>
/// Persistent application history. Extending a history never copies its prefix;
/// indexed observation prepares one reusable linear-time snapshot on demand.
/// </summary>
internal sealed class CollectedTemplateEnvironments : IReadOnlyList<PineValueInProcess>
{
    public static readonly CollectedTemplateEnvironments Empty = new();

    private readonly CollectedTemplateEnvironments? _previous;

    private readonly PineValueInProcess? _last;

    private PineValueInProcess[]? _indexed;

    public int Count { get; }

    private CollectedTemplateEnvironments()
    {
    }

    private CollectedTemplateEnvironments(
        CollectedTemplateEnvironments previous,
        PineValueInProcess last)
    {
        _previous = previous;
        _last = last;
        Count = previous.Count + 1;
    }

    public CollectedTemplateEnvironments Append(PineValueInProcess environment) =>
        new(this, environment);

    public PineValueInProcess this[int index]
    {
        get
        {
            if ((uint)index >= (uint)Count)
                throw new ArgumentOutOfRangeException(nameof(index));

            if (index == Count - 1)
                return _last!;

            return Indexed()[index];
        }
    }

    private PineValueInProcess[] Indexed()
    {
        if (_indexed is { } cached)
            return cached;

        var items = new PineValueInProcess[Count];
        var current = this;

        for (var index = Count - 1; index >= 0; --index)
        {
            items[index] = current._last!;
            current = current._previous!;
        }

        return _indexed = items;
    }

    public IEnumerator<PineValueInProcess> GetEnumerator() =>
        ((IEnumerable<PineValueInProcess>)Indexed()).GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}
