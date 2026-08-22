using Pine.Core.Internal;
using System.Collections.Generic;

namespace Pine.Core.Elm;

/// <summary>
/// Helpers for constructing Elm values in the in-process representation.
/// </summary>
public static class ElmValueInProcess
{
    /// <summary>
    /// Creates a lazily materialized Elm choice value in the flat
    /// <c>[&lt;Choice_Type&gt;, tag, arg0, ...]</c> layout.
    /// </summary>
    public static PineValueInProcess CreateChoice(
        PineValueInProcess tag,
        IReadOnlyList<PineValueInProcess> tagArgs) =>
        PineValueInProcess.CreateList(
            [
            PineValueInProcess.Create(ElmValue.ElmChoiceTypeTagNameAsValue),
            tag,
            ..tagArgs
            ]);
}
