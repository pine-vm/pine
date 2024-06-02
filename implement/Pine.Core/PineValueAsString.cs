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

    public static IReadOnlyList<PineValue> ListValueFromString(string str)
    {
        var codePoints = ToCodePoints(str);

        var pineValues = new PineValue[codePoints.Length];

        for (var index = 0; index < codePoints.Length; index++)
        {
            var charAsInteger = new System.Numerics.BigInteger(codePoints[index]);

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

        if (list.Elements.Count is 0)
            return Result<string, string>.ok("");

        var stringBuilder = new System.Text.StringBuilder(capacity: list.Elements.Count * 2);

        for (var index = 0; index < list.Elements.Count; index++)
        {
            if (list.Elements[index] is not PineValue.BlobValue blobValue)
                return Result<string, string>.err("failed decoding char as integer at [" + index + "]: Not a blob");

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
