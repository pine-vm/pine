using System;
using System.Collections.Frozen;
using System.Collections.Generic;

namespace Pine.Core.PopularEncodings;

/// <summary>
/// Encoding of strings as Pine values and parsing Pine values as strings.
/// 
/// <para>
/// This is the same encoding used for the tags distinguishing Pine expression variants and kernel function names.
/// </para>
/// <para>
/// Frontend languages like Elm might use different representations of strings.
/// </para>
/// </summary>
public static class StringEncoding
{
    private static readonly FrozenDictionary<string, PineValue> ReusedInstances =
        PopularValues.PopularStrings
        .ToFrozenDictionary(
            keySelector: s => s,
            elementSelector: ValueFromString);

    private static readonly FrozenDictionary<string, PineValue> ReusedInstances_2024 =
        PopularValues.PopularStrings
        .ToFrozenDictionary(
            keySelector: s => s,
            elementSelector: ValueFromString_2024);

    private static readonly FrozenDictionary<PineValue.ListValue, string> CommonStringsDecodedAsList =
        ReusedInstances_2024
        .ToFrozenDictionary(
            keySelector: kvp => kvp.Value as PineValue.ListValue,
            elementSelector: kvp => kvp.Key);

    /// <summary>
    /// Converts a .NET string to a Pine blob value containing UTF-32 encoded characters.
    /// </summary>
    public static PineValue ValueFromString(string str)
    {
        if (ReusedInstances?.TryGetValue(str, out var reusedStringValue) ?? false && reusedStringValue is not null)
            return reusedStringValue;

        return BlobValueFromString(str);
    }

    /// <summary>
    /// Converts a .NET string to a Pine blob value containing UTF-32 encoded characters.
    /// </summary>
    public static PineValue.BlobValue BlobValueFromString(string str)
    {
        if (str.Length is 0)
            return PineValue.EmptyBlob;

        var codePoints = ToCodePoints(str);

        var bytes = new byte[codePoints.Count * 4];

        for (var index = 0; index < codePoints.Count; index++)
        {
            System.Buffers.Binary.BinaryPrimitives.WriteUInt32BigEndian(
                bytes.AsSpan(index * 4),
                (uint)codePoints[index]);
        }

        return (PineValue.BlobValue)PineValue.Blob(bytes);
    }

    /// <summary>
    /// Converts a .NET string to a Pine list value containing one element for each character in the input string.
    /// </summary>
    /// <returns>Pine list value containing one element for each character in the input string.</returns>
    public static PineValue ValueFromString_2024(string str)
    {
        if (str.Length is 0)
            return PineValue.EmptyList;

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

            if (IntegerEncoding.ReusedInstanceForUnsignedInteger(codePoint) is { } reused)
            {
                pineValues[index] = reused;
                continue;
            }

            var charAsInteger = new System.Numerics.BigInteger(codePoint);

            var charAsIntegerResult = IntegerEncoding.EncodeUnsignedInteger(charAsInteger);

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

    /// <summary>
    /// Try parse the given PineValue as a string.
    /// </summary>
    public static Result<string, string> StringFromValue(PineValue pineValue)
    {
        if (pineValue is PineValue.ListValue list)
            return StringFromListValue(list);

        if (pineValue is PineValue.BlobValue blob)
            return StringFromBlobValue(blob.Bytes);

        return
            Result<string, string>.err("PineValue is not a blob: " + pineValue.GetType().FullName);
    }

    private static readonly Result<string, string> emptyStringOk = Result<string, string>.ok("");

    private static Result<string, string> StringFromBlobValue(ReadOnlyMemory<byte> blobBytes)
    {
        if (blobBytes.Length is 0)
            return emptyStringOk;

        if (blobBytes.Length % 4 is not 0)
        {
            return Result<string, string>.err("Blob length is not a multiple of 4: " + blobBytes.Length);
        }

        var codePointsCount = blobBytes.Length / 4;

        var stringBuilder = new System.Text.StringBuilder(capacity: codePointsCount);

        for (var index = 0; index < codePointsCount; index++)
        {
            var codePoint =
                System.Buffers.Binary.BinaryPrimitives.ReadInt32BigEndian(blobBytes.Span[(index * 4)..]);

            if (!UnicodeUtility.IsValidUnicodeScalar(codePoint))
            {
                return Result<string, string>.err("Blob value at [" + index + "] out of range: " + codePoint);
            }

            stringBuilder.Append(char.ConvertFromUtf32(codePoint));
        }

        return Result<string, string>.ok(stringBuilder.ToString());
    }

    public static Result<string, string> StringFromListValue(PineValue.ListValue list)
    {
        if (list.Elements.Length is 0)
            return emptyStringOk;

        if (CommonStringsDecodedAsList.TryGetValue(list, out var commonString))
            return Result<string, string>.ok(commonString);

        var stringBuilder = new System.Text.StringBuilder(capacity: list.Elements.Length * 2);

        for (var index = 0; index < list.Elements.Length; index++)
        {
            if (list.Elements.Span[index] is not PineValue.BlobValue blobValue)
            {
                return Result<string, string>.err(
                    "failed decoding char as integer at [" + index + "]: Not a blob");
            }

            var charInteger = IntegerEncoding.ParseUnsignedInteger(blobValue.Bytes.Span);


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
