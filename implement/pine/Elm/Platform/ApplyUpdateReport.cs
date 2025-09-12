using Pine.Core;
using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;

namespace Pine.Elm.Platform;

public record FunctionRecordValueAndParsed
{
    public PineValue Value { private init; get; }

    public ElmInteractiveEnvironment.FunctionRecord Parsed { private init; get; }

    private FunctionRecordValueAndParsed(
        PineValue value,
        ElmInteractiveEnvironment.FunctionRecord parsed)
    {
        Value = value;
        Parsed = parsed;
    }

    /// <inheritdoc/>
    public virtual bool Equals(FunctionRecordValueAndParsed? other)
    {
        if (other is null)
        {
            return false;
        }

        return Value == other.Value;
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        return Value.GetHashCode();
    }

    public static FunctionRecordValueAndParsed ParseOrThrow(
        PineValue value,
        PineVMParseCache parseCache)
    {
        var parseUpdateResult =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(
                value,
                parseCache);

        {
            if (parseUpdateResult.IsErrOrNull() is { } err)
            {
                throw new Exception("Failed to parse update: " + err);
            }
        }

        if (parseUpdateResult.IsOkOrNull() is not { } update)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + parseUpdateResult);
        }

        return
            new FunctionRecordValueAndParsed(
                value,
                update);
    }
}

public record ApplyFunctionInput(
    FunctionRecordValueAndParsed AppliedFunction,
    IReadOnlyList<PineValue> ArgsBeforeState);

public record ApplyUpdateReport<CommandT>(
    ApplyFunctionInput Input,
    PineValue ResponseState,
    IReadOnlyList<(PineValue cmdValue, CommandT cmdParsed)> ResponseCommands);
