using Pine.Core.Encodings;
using Pine.Core.PineLanguage;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>Emits an explicit Pine evaluation failure carrying an Elm runtime diagnostic.</summary>
internal static class ElmRuntimeFailure
{
    // Pine has no throw instruction. Evaluating a diagnostic string as code fails parsing and
    // preserves that diagnostic in both direct and intermediate VM error reports.
    public static Expression Emit(string message) =>
        Emit(Expression.LitralInst(StringEncoding.ValueFromString(message)));

    public static Expression Emit(Expression message) =>
        new Expression.Eval(encoded: message, environment: Expression.EmptyList);
}
