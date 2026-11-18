using Pine.Core.Internal;
using System;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// A borrowed return-slot view, valid until the invocation's stack storage is reused.
/// Consumers retaining it must copy the slots or materialize the canonical value.
/// </summary>
public readonly struct InvocationResult
{
    private readonly PineValueInProcess? _canonical;

    private readonly ReadOnlyMemory<PineValueInProcess?> _slots;

    /// <summary>The statically established canonical reconstruction recipe.</summary>
    public ReturnValueLayout Layout { get; }

    /// <summary>Creates a borrowed view of flattened return components.</summary>
    public InvocationResult(ReturnValueLayout layout, ReadOnlyMemory<PineValueInProcess?> slots)
    {
        if (slots.Length != layout.SlotCount)
            throw new ArgumentException("Return slot count does not match the layout.", nameof(slots));

        Layout = layout;
        _slots = slots;
    }

    /// <summary>Creates an ordinary canonical result.</summary>
    public InvocationResult(PineValueInProcess value)
    {
        _canonical = value;
        Layout = ReturnValueLayout.Canonical;
    }

    /// <summary>Obtains a component without reconstructing the canonical value.</summary>
    public PineValueInProcess Component(int index) =>
        _canonical is not null && index == 0
        ?
        _canonical
        :
        _slots.Span[index] ?? throw new InvalidOperationException("Missing return component.");

    /// <summary>Constructs the canonical result only when requested.</summary>
    public PineValueInProcess Materialize() => _canonical ?? Layout.Materialize(_slots.Span);

    internal void CopyTo(ReturnValueLayout layout, Span<PineValueInProcess?> destination)
    {
        if (_canonical is null && Layout.Equals(layout))
            _slots.Span.CopyTo(destination);

        else
            layout.Deconstruct(Materialize(), destination);
    }

    /// <summary>Adapts an ordinary result to the return-consumer contract.</summary>
    public static implicit operator InvocationResult(PineValueInProcess value) => new(value);
}
