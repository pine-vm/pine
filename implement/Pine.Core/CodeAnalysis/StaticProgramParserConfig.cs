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
public record StaticProgramParserConfig<IdentifierT>(
    Func<IEnumerable<IdentifierT>, PineValue, StaticProgramParser.IdentifyResponse<IdentifierT>> IdentifyInstanceRequired,
    Func<IEnumerable<IdentifierT>, PineValue, StaticProgramParser.IdentifyResponse<IdentifierT>?> IdentifyInstanceOptional,
    Func<IEnumerable<IdentifierT>, StaticProgramParser.CrashOrigin, IdentifierT> IdentifyCrash)
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
                });
}
