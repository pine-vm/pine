using System;
using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;


/// <summary>
/// Configuring how to assign identifiers to parts of the program we encounter when parsing.
/// </summary>
/// <param name="IdentifyInstanceRequired">
/// The parser requires an identifier for every function call, since the produced syntax model does not allow for anonymous functions.
/// </param>
/// <param name="IdentifyInstanceOptional">
/// Optional identifiers, such as names for values corresponding to module-level declarations
/// </param>
/// <param name="IdentifyCrash">
/// How to build an identifier depending on origin of a crash.
/// </param>
/// <param name="IdentifyEncodedBodyOptional">
/// Optional lookup that recognizes a <see cref="PineValue"/> as a known callee's
/// <c>EncodedExpression</c> (the encoded body in the env-functions-at-index-0 layout, see
/// property 4 of <c>CompiledFunctionInfo</c> in
/// <c>explore/internal-analysis/2026-04-22-analysis-inline-non-recursive-callees.md</c>).
/// When non-null and a match is returned, the parser canonicalizes a
/// <c>ParseAndEval(Literal(v), List[Literal(envFuncs), arg0, ..., arg{n-1}])</c> call
/// site (Form A) to a saturated <c>FunctionApplication</c> with the same identifier
/// that a Form B call to the same callee would produce.
/// Defaults to a function that always returns <c>null</c> (Form A unsupported).
/// </param>
public record StaticProgramParserConfig<IdentifierT>(
    Func<IEnumerable<IdentifierT>, PineValue, StaticProgramParser.IdentifyResponse<IdentifierT>> IdentifyInstanceRequired,
    Func<IEnumerable<IdentifierT>, PineValue, StaticProgramParser.IdentifyResponse<IdentifierT>?> IdentifyInstanceOptional,
    Func<IEnumerable<IdentifierT>, StaticProgramParser.CrashOrigin, IdentifierT> IdentifyCrash,
    Func<IEnumerable<IdentifierT>, PineValue, StaticProgramParser.IdentifyResponse<IdentifierT>?>? IdentifyEncodedBodyOptional = null)
{
    public static StaticProgramParserConfig<IdentifierT> OptionalNullRequiredThrow()
        =>
        OptionalNullRequiredThrow(
            context => $"[{string.Join(" -> ", context)}]");

    public static StaticProgramParserConfig<IdentifierT> OptionalNullRequiredThrow(
        Func<IEnumerable<IdentifierT>, string> describeContext)
        =>
        new(
            IdentifyInstanceRequired:
            (context, value) =>
            {
                throw new InvalidOperationException(
                    $"No identifier provided for required instance in context {describeContext(context)} and value {value}.");
            },

            IdentifyInstanceOptional:
            (context, value) => null,

            IdentifyCrash:
            (context, origin) =>
            {
                throw new InvalidOperationException(
                    $"No identifier provided for crash in context {describeContext(context)}");
            },

            IdentifyEncodedBodyOptional:
            (context, value) => null);
}
