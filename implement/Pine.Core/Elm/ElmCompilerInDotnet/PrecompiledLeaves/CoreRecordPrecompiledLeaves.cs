using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

/// <summary>
/// Precompiled leaves (short-circuit .NET implementations) for the runtime record
/// access and record update functions emitted by the Elm compiler in .NET
/// (see <see cref="RecordRuntime"/>).
/// <para>
/// Each entry maps the Pine value encoding of the function dispatched via
/// <c>ParseAndEval</c> to a delegate that returns the result directly, bypassing the
/// intermediate VM's interpretation of the recursive field-walking expression tree.
/// </para>
/// <para>
/// These leaves only short-circuit the row-polymorphic runtime fallback used when the
/// record field layout is not known at compile time. When the layout is known, the
/// compiler emits direct index-based access/update instead, which is not routed through
/// <see cref="RecordRuntime"/> and therefore not affected by these leaves.
/// </para>
/// </summary>
public static class CoreRecordPrecompiledLeaves
{
    /// <summary>
    /// Pine value key under which the record-access leaf is registered in the
    /// precompiled-leaves dictionary; equal to
    /// <see cref="RecordRuntime.PineFunctionForRecordAccessAsValue"/> (the function the
    /// intermediate VM dispatches to via <c>ParseAndEval</c> for a runtime record access).
    /// </summary>
    public static PineValue RecordAccessLeafKey => RecordRuntime.PineFunctionForRecordAccessAsValue;

    /// <summary>
    /// Pine value key under which the record-update leaf is registered in the
    /// precompiled-leaves dictionary; equal to
    /// <see cref="RecordRuntime.PineFunctionForRecordUpdateAsValue"/> (the function the
    /// intermediate VM dispatches to via <c>ParseAndEval</c> for a runtime record update).
    /// </summary>
    public static PineValue RecordUpdateLeafKey => RecordRuntime.PineFunctionForRecordUpdateAsValue;

    /// <summary>
    /// Precompiled-leaf delegate for runtime record access; executes the field lookup
    /// directly in .NET and returns the resulting field value, or <c>null</c> if the
    /// environment does not match the expected shape (so the VM falls back to interpreting
    /// the recursive lookup).
    /// <para>
    /// Environment layout: <c>[record, fieldName]</c> where <c>record</c> is the flat
    /// record layout <c>[tag, name0, value0, name1, value1, ...]</c>.
    /// </para>
    /// </summary>
    public static PineValueInProcess? RecordAccessLeafDelegate(PineValueInProcess environment)
    {
        if (!environment.IsList() || environment.GetLength() < 2)
        {
            return null;
        }

        var record = environment.GetElementAt(0);
        var fieldName = environment.GetElementAt(1);

        if (!record.IsList())
        {
            return null;
        }

        // Flat layout: [tag, name0, value0, name1, value1, ...]; field pairs start at offset 1.
        for (var i = 1; i + 1 < record.GetLength(); i += 2)
        {
            if (PineValueInProcess.AreEqual(record.GetElementAt(i), fieldName))
            {
                return record.GetElementAt(i + 1);
            }
        }

        // Field not found: fall back to the VM (matches the runtime function's error branch).
        return null;
    }

    /// <summary>
    /// Precompiled-leaf delegate for runtime record update; executes the field merge
    /// directly in .NET and returns the reconstructed record, or <c>null</c> if the
    /// environment does not match the expected shape (so the VM falls back to interpreting
    /// the recursive update).
    /// <para>
    /// Environment layout: <c>[record, updates]</c> where <c>record</c> is the flat record
    /// layout <c>[tag, name0, value0, ...]</c> and <c>updates</c> is a list of
    /// <c>[fieldName, newValue]</c> pairs. Both the record fields and the updates must be
    /// sorted alphabetically by field name (the same precondition the runtime function relies on).
    /// </para>
    /// </summary>
    public static PineValueInProcess? RecordUpdateLeafDelegate(PineValueInProcess environment)
    {
        if (!environment.IsList() || environment.GetLength() < 2)
        {
            return null;
        }

        var record = environment.GetElementAt(0);
        var updates = environment.GetElementAt(1);

        if (!record.IsList())
        {
            return null;
        }

        if (!updates.IsList())
        {
            return null;
        }

        var recordLength = record.GetLength();

        if (recordLength < 1)
        {
            return null;
        }

        var resultItems = new PineValueInProcess[recordLength];

        // Tag stays in place at offset 0.
        resultItems[0] = record.GetElementAt(0);

        var updatesIndex = 0;

        // Single-pass merge of the (sorted) field stream and the (sorted) updates.
        for (var i = 1; i + 1 < recordLength; i += 2)
        {
            var fieldName = record.GetElementAt(i);
            var fieldValue = record.GetElementAt(i + 1);

            resultItems[i] = fieldName;

            if (updatesIndex < updates.GetLength() &&
                updates.GetElementAt(updatesIndex) is { } updatePair &&
                updatePair.IsList() &&
                updatePair.GetLength() >= 2 &&
                PineValueInProcess.AreEqual(updatePair.GetElementAt(0), fieldName))
            {
                resultItems[i + 1] = updatePair.GetElementAt(1);
                updatesIndex++;
            }
            else
            {
                resultItems[i + 1] = fieldValue;
            }
        }

        return PineValueInProcess.CreateList(resultItems);
    }

    /// <summary>
    /// Default precompiled-leaves dictionary contributed by the runtime record functions.
    /// Suitable for merging into the dictionary consumed by the intermediate VM.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, PrecompiledLeaf> DefaultLeaves =>
        s_defaultLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, PrecompiledLeaf>> s_defaultLeaves =
        new(
            () =>
            ImmutableDictionary<PineValue, PrecompiledLeaf>.Empty
            .Add(RecordAccessLeafKey, RecordAccessLeafDelegate)
            .Add(RecordUpdateLeafKey, RecordUpdateLeafDelegate));
}
