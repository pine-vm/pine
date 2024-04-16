using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine;

public static class PineValueAsString
{
    /// <summary>
    /// Converts a .NET string to a Pine list value containing one element for each character in the input string.
    /// </summary>
    /// <returns>Pine list value containing one element for each character in the input string.</returns>
    public static PineValue ValueFromString(string str) =>
        PineValue.List(ListValueFromString(str));

    public static IImmutableList<PineValue> ListValueFromString(string str) =>
        ToCodePoints(str!)
        .Select(charAsInteger => new System.Numerics.BigInteger(charAsInteger))
        .Select(PineValueAsInteger.ValueFromUnsignedInteger)
        .Select(charResult => charResult.Extract(error => throw new Exception(error)))
        .ToImmutableList();

    public static Result<string, string> StringFromValue(PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue list)
            return Result<string, string>.err("Only a ListValue can represent a string.");

        var charsIntegersResults =
            list.Elements
            .Select(PineValueAsInteger.UnsignedIntegerFromValue)
            .ToImmutableList();

        return
            charsIntegersResults
            .Select(charIntegerResult => charIntegerResult.AndThen(charInteger =>
            // https://github.com/dotnet/runtime/blob/e05944dd74118db06d6987fe2724813950725737/src/libraries/System.Private.CoreLib/src/System/Char.cs#L1036
            UnicodeUtility.IsValidUnicodeScalar(charInteger) ?
            Result<string, string>.ok(char.ConvertFromUtf32((int)charInteger))
            :
            Result<string, string>.err("Out of range: " + charInteger)))
            .Select((result, index) => result.MapError(err => "at " + index + ": " + err))
            .ListCombine()
            .Map(chars => string.Join(null, chars));
    }

    // https://stackoverflow.com/questions/687359/how-would-you-get-an-array-of-unicode-code-points-from-a-net-string/28155130#28155130
    public static int[] ToCodePoints(string str)
    {
        var codePoints = new List<int>(str.Length);
        for (var i = 0; i < str.Length; i++)
        {
            codePoints.Add(char.ConvertToUtf32(str, i));
            if (char.IsHighSurrogate(str[i]))
                i += 1;
        }

        return [.. codePoints];
    }
}
