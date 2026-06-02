using Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;
using System;
using System.Collections.Generic;

namespace Pine.Core.IntermediateVM;

/// <summary>
/// Setup helpers for the <see cref="PineVM"/> that depend only on material
/// available inside the <c>Pine.Core</c> project. This sits below the
/// counterpart in the <c>pine</c> project (<c>Pine.IntermediateVM.SetupVM</c>),
/// which extends the aggregate exposed here with additional, bundle-derived
/// precompiled-leaf entries.
/// </summary>
public static class SetupVM
{
    /// <summary>
    /// Precompiled-leaf entries contributed by <see cref="CoreBasicsPrecompiledLeaves"/>
    /// (currently the short-circuit implementation of <c>Basics.compare</c>). Exposed
    /// so that consumers building their own intermediate VM via
    /// <see cref="Interpreter.IntermediateVM.PineVM.CreateCustom"/> can pick the individual leaf packages they
    /// want to register without pulling in the full <see cref="DefaultPrecompiledLeaves"/>
    /// aggregate.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> BasicsComparePrecompiledLeaves =>
        CoreBasicsPrecompiledLeaves.DefaultLeaves;

    /// <summary>
    /// Precompiled-leaf entries contributed by <see cref="CoreDictPrecompiledLeaves"/>
    /// (currently the short-circuit implementation of <c>Dict.get</c>). Exposed so
    /// that consumers building their own intermediate VM via
    /// <see cref="Interpreter.IntermediateVM.PineVM.CreateCustom"/> can pick the individual leaf packages they
    /// want to register without pulling in the full <see cref="DefaultPrecompiledLeaves"/>
    /// aggregate.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DictGetPrecompiledLeaves =>
        CoreDictPrecompiledLeaves.DefaultLeaves;

    /// <summary>
    /// Precompiled-leaf entries contributed by <see cref="CoreRecordPrecompiledLeaves"/>
    /// (short-circuit implementations of the runtime record access and record update
    /// functions). Exposed so that consumers building their own intermediate VM via
    /// <see cref="Interpreter.IntermediateVM.PineVM.CreateCustom"/> can pick the individual leaf packages they
    /// want to register without pulling in the full <see cref="DefaultPrecompiledLeaves"/>
    /// aggregate.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> RecordAccessAndUpdatePrecompiledLeaves =>
        CoreRecordPrecompiledLeaves.DefaultLeaves;

    /// <summary>
    /// Precompiled-leaf entries contributed by <see cref="Base64PrecompiledLeaves"/>
    /// (short-circuit implementations of <c>Base64.Encode.toBytes</c> and
    /// <c>Base64.Decode.fromBytes</c>). Exposed so that consumers building their own
    /// intermediate VM via
    /// <see cref="Interpreter.IntermediateVM.PineVM.CreateCustom"/> can pick the individual leaf packages they
    /// want to register without pulling in the full <see cref="DefaultPrecompiledLeaves"/>
    /// aggregate.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> Base64ConversionPrecompiledLeaves =>
        Base64PrecompiledLeaves.DefaultLeaves;

    /// <summary>
    /// Aggregate of the per-area precompiled-leaf dictionaries
    /// (<see cref="BasicsComparePrecompiledLeaves"/>, <see cref="DictGetPrecompiledLeaves"/>,
    /// <see cref="RecordAccessAndUpdatePrecompiledLeaves"/>,
    /// <see cref="Base64ConversionPrecompiledLeaves"/>)
    /// available from inside the <c>Pine.Core</c> project. The counterpart in
    /// the <c>pine</c> project layers additional bundle-derived entries on top
    /// of this aggregate.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultPrecompiledLeaves =>
        s_defaultPrecompiledLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_defaultPrecompiledLeaves =
        new(BuildDefaultPrecompiledLeaves);

    private static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>
        BuildDefaultPrecompiledLeaves()
    {
        var merged = new Dictionary<PineValue, Func<PineValue, PineValue?>>();

        foreach (var entry in BasicsComparePrecompiledLeaves)
        {
            merged[entry.Key] = entry.Value;
        }

        // Entries already present (e.g. from earlier leaf packages) take
        // precedence over later contributions for the same key.
        foreach (var entry in DictGetPrecompiledLeaves)
        {
            if (!merged.ContainsKey(entry.Key))
            {
                merged[entry.Key] = entry.Value;
            }
        }

        foreach (var entry in RecordAccessAndUpdatePrecompiledLeaves)
        {
            if (!merged.ContainsKey(entry.Key))
            {
                merged[entry.Key] = entry.Value;
            }
        }

        foreach (var entry in Base64ConversionPrecompiledLeaves)
        {
            if (!merged.ContainsKey(entry.Key))
            {
                merged[entry.Key] = entry.Value;
            }
        }

        return merged;
    }
}
