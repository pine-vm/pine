using System;
using System.Collections.Frozen;
using System.Collections.Generic;

namespace Pine.Core;

public static class PineValueAsString
{
    private static readonly FrozenDictionary<string, PineValue> ReusedInstances =
        PopularValues.PopularStrings
        .ToFrozenDictionary(
            keySelector: s => s,
            elementSelector: ValueFromString);

    private static readonly FrozenDictionary<PineValue.ListValue, string> CommonStringsDecoded =
        ReusedInstances
        .ToFrozenDictionary(
            keySelector: kvp => kvp.Value as PineValue.ListValue,
            elementSelector: kvp => kvp.Key);

    /// <summary>
    /// Converts a .NET string to a Pine list value containing one element for each character in the input string.
    /// </summary>
    /// <returns>Pine list value containing one element for each character in the input string.</returns>
    public static PineValue ValueFromString(string str)
    {
        if (str.Length is 0)
            return PineValue.EmptyList;

        if (ReusedInstances?.TryGetValue(str, out var reusedStringValue) ?? false && reusedStringValue is not null)
            return reusedStringValue;

        return PineValue.List(ListValueFromString(str));
    }

    /// <summary>
    /// Converts a .NET string to a list of Pine values containing one element for each character in the input string.
    /// </summary>
    public static ReadOnlyMemory<PineValue> ListValueFromString(string str)
    {
        var codePoints = ToCodePoints(str);

        var pineValues = new PineValue[codePoints.Count];

        for (var index = 0; index < codePoints.Count; index++)
        {
            var codePoint = codePoints[index];

            if (PineValueAsInteger.ReusedInstanceForUnsignedInteger(codePoint) is { } reused)
            {
                pineValues[index] = reused;
                continue;
            }

            var charAsInteger = new System.Numerics.BigInteger(codePoint);

            var charAsIntegerResult = PineValueAsInteger.ValueFromUnsignedInteger(charAsInteger);

            if (charAsIntegerResult is Result<string, PineValue>.Err error)
                throw new Exception(error.Value);

            if (charAsIntegerResult is Result<string, PineValue>.Ok ok)
            {
                pineValues[index] = ok.Value;
                continue;
            }

            throw new NotImplementedException(
                "Unexpected result type: " + charAsIntegerResult.GetType().FullName);
        }

        return pineValues;
    }

    public static Result<string, string> StringFromValue(PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue list)
            return Result<string, string>.err("Only a ListValue can represent a string.");

        return StringFromListValue(list);
    }

    private static readonly Result<string, string> emptyStringOk = Result<string, string>.ok("");

    public static Result<string, string> StringFromListValue(PineValue.ListValue list)
    {
        if (list.Elements.Length is 0)
            return emptyStringOk;

        if (CommonStringsDecoded.TryGetValue(list, out var commonString))
            return Result<string, string>.ok(commonString);

        var stringBuilder = new System.Text.StringBuilder(capacity: list.Elements.Length * 2);

        for (var index = 0; index < list.Elements.Length; index++)
        {
            if (list.Elements.Span[index] is not PineValue.BlobValue blobValue)
            {
                return Result<string, string>.err(
                    "failed decoding char as integer at [" + index + "]: Not a blob");
            }

            var charInteger = PineValueAsInteger.UnsignedIntegerFromBlobValue(blobValue.Bytes.Span);


            if (UnicodeUtility.IsValidUnicodeScalar(charInteger))
            {
                stringBuilder.Append(char.ConvertFromUtf32((int)charInteger));
                continue;
            }

            // https://github.com/dotnet/runtime/blob/e05944dd74118db06d6987fe2724813950725737/src/libraries/System.Private.CoreLib/src/System/Char.cs#L1036
            return Result<string, string>.err("Char code at [" + index + "] out of range: " + charInteger);
        }

        return Result<string, string>.ok(stringBuilder.ToString());
    }

    // https://stackoverflow.com/questions/687359/how-would-you-get-an-array-of-unicode-code-points-from-a-net-string/28155130#28155130
    public static IReadOnlyList<int> ToCodePoints(string str)
    {
        var codePoints = new List<int>(str.Length);
        for (var i = 0; i < str.Length; i++)
        {
            codePoints.Add(char.ConvertToUtf32(str, i));
            if (char.IsHighSurrogate(str[i]))
                i += 1;
        }

        return codePoints;
    }
}
