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
    public static PineValue? RecordAccessLeafDelegate(PineValue environment)
    {
        if (environment is not PineValue.ListValue envList || envList.Items.Length < 2)
        {
            return null;
        }

        var record = envList.Items.Span[0];
        var fieldName = envList.Items.Span[1];

        if (record is not PineValue.ListValue recordList)
        {
            return null;
        }

        var items = recordList.Items.Span;

        // Flat layout: [tag, name0, value0, name1, value1, ...]; field pairs start at offset 1.
        for (var i = 1; i + 1 < items.Length; i += 2)
        {
            if (items[i] == fieldName)
            {
                return items[i + 1];
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
    public static PineValue? RecordUpdateLeafDelegate(PineValue environment)
    {
        if (environment is not PineValue.ListValue envList || envList.Items.Length < 2)
        {
            return null;
        }

        var record = envList.Items.Span[0];
        var updates = envList.Items.Span[1];

        if (record is not PineValue.ListValue recordList)
        {
            return null;
        }

        if (updates is not PineValue.ListValue updatesList)
        {
            return null;
        }

        var recordItems = recordList.Items;

        if (recordItems.Length < 1)
        {
            return null;
        }

        var resultItems = new PineValue[recordItems.Length];

        // Tag stays in place at offset 0.
        resultItems[0] = recordItems.Span[0];

        var updatesItems = updatesList.Items;

        var updatesIndex = 0;

        // Single-pass merge of the (sorted) field stream and the (sorted) updates.
        for (var i = 1; i + 1 < recordItems.Length; i += 2)
        {
            var fieldName = recordItems.Span[i];
            var fieldValue = recordItems.Span[i + 1];

            resultItems[i] = fieldName;

            if (updatesIndex < updatesItems.Length &&
                updatesItems.Span[updatesIndex] is PineValue.ListValue updatePair &&
                updatePair.Items.Length >= 2 &&
                updatePair.Items.Span[0] == fieldName)
            {
                resultItems[i + 1] = updatePair.Items.Span[1];
                updatesIndex++;
            }
            else
            {
                resultItems[i + 1] = fieldValue;
            }
        }

        return PineValue.List(resultItems);
    }

    /// <summary>
    /// Default precompiled-leaves dictionary contributed by the runtime record functions.
    /// Suitable for merging into the dictionary consumed by the intermediate VM.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultLeaves =>
        s_defaultLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_defaultLeaves =
        new(
            () =>
            ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty
            .Add(RecordAccessLeafKey, RecordAccessLeafDelegate)
            .Add(RecordUpdateLeafKey, RecordUpdateLeafDelegate));
}
