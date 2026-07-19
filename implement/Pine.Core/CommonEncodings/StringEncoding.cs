using System;
using System.Collections.Frozen;
using System.Collections.Generic;

namespace Pine.Core.CommonEncodings;

/// <summary>
/// Encoding of strings as Pine values and parsing Pine values as strings.
/// 
/// <para>
/// The string encoding uses four bytes (32 bits) per Unicode code point, in big-endian order.
/// This is the encoding used for file names and directory names in <see cref="FileTreeEncoding"/>.
/// </para>
/// <para>
/// Frontend languages like Elm might use different representations of strings.
/// </para>
/// </summary>
public static class StringEncoding
{
    private static readonly FrozenDictionary<string, PineValue> s_reusedInstances =
        PopularValues.PopularStrings
        .ToFrozenDictionary(
            keySelector: s => s,
            elementSelector: ValueFromString);

    private static readonly FrozenDictionary<string, PineValue> ReusedInstances_2024 =
        PopularValues.PopularStrings
        .ToFrozenDictionary(
            keySelector: s => s,
            elementSelector: ValueFromString_2024);

    private static readonly FrozenDictionary<PineValue, string> s_reusedStringFromValue =
        s_reusedInstances
        .ToFrozenDictionary(
            keySelector: kvp => kvp.Value,
            elementSelector: kvp => kvp.Key);

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
        if (s_reusedInstances?.TryGetValue(str, out var reusedStringValue) ?? false && reusedStringValue is not null)
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

        var codePointsCount = 0;

        for (var sourceIndex = 0; sourceIndex < str.Length; sourceIndex++)
        {
            _ = char.ConvertToUtf32(str, sourceIndex);

            codePointsCount++;

            if (char.IsHighSurrogate(str[sourceIndex]))
                sourceIndex++;
        }

        var bytes = new byte[codePointsCount * 4];
        var destinationOffset = 0;

        for (var sourceIndex = 0; sourceIndex < str.Length; sourceIndex++)
        {
            var codePoint = char.ConvertToUtf32(str, sourceIndex);

            System.Buffers.Binary.BinaryPrimitives.WriteUInt32BigEndian(
                bytes.AsSpan(destinationOffset, 4),
                (uint)codePoint);

            destinationOffset += 4;

            if (char.IsHighSurrogate(str[sourceIndex]))
                sourceIndex++;
        }

        return (PineValue.BlobValue)PineValue.Blob(bytes);
    }

    /// <summary>
    /// Converts a .NET string to a Pine list value containing one element for each character in the input string.
    /// <para>
    /// This was the original encoding of strings in Pine, used before 2025.
    /// </para>
    /// </summary>
    /// <returns>Pine list value containing one element for each character in the input string.</returns>
    public static PineValue ValueFromString_2024(string str)
    {
        if (str.Length is 0)
            return PineValue.EmptyList;

        if (ReusedInstances_2024?.TryGetValue(str, out var reusedInstance) ?? false && reusedInstance is not null)
        {
            return reusedInstance;
        }

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
    public static Result<string, string> StringFromValue(PineValue pineValue) =>
        StringFromValueWithoutResultAllocation(pineValue).ToPublicResult();

    internal static StringParseResult StringFromValueWithoutResultAllocation(PineValue pineValue)
    {
        if (s_reusedStringFromValue is { } dict &&
            dict.TryGetValue(pineValue, out var reusedInst) &&
            reusedInst is not null)
        {
            return StringParseResult.Ok(reusedInst);
        }

        if (pineValue is PineValue.ListValue list)
            return StringFromListValueWithoutResultAllocation(list);

        if (pineValue is PineValue.BlobValue blob)
            return StringFromBlobValueWithoutResultAllocation(blob.Bytes);

        return
            StringParseResult.Err(
                "PineValue is not a blob: " + pineValue.GetType().FullName);
    }

    private static readonly Result<string, string> s_emptyStringOk = Result<string, string>.ok("");

    /// <summary>
    /// Converts a binary blob containing UTF-32 encoded Unicode code points (in big-endian order) to a string.
    /// </summary>
    /// <remarks>If the input is empty, the method returns a successful result containing an empty string. The
    /// method validates that each 4-byte segment represents a valid Unicode scalar value.</remarks>
    /// <param name="blobBytes">A read-only memory region containing the UTF-32 encoded Unicode code points, with each code point represented as
    /// a 4-byte big-endian integer.</param>
    /// <returns>A result containing the decoded string if the conversion succeeds; otherwise, a result containing an error
    /// message if the blob length is not a multiple of 4 or if any code point is invalid.</returns>
    public static Result<string, string> StringFromBlobValue(ReadOnlyMemory<byte> blobBytes)
    {
        if (blobBytes.Length is 0)
            return s_emptyStringOk;

        return StringFromBlobValueWithoutResultAllocation(blobBytes).ToPublicResult();
    }

    private static StringParseResult StringFromBlobValueWithoutResultAllocation(
        ReadOnlyMemory<byte> blobBytes)
    {
        if (blobBytes.Length is 0)
            return StringParseResult.Ok("");

        if (blobBytes.Length % 4 is not 0)
        {
            return
                StringParseResult.Err(
                    "Blob length is not a multiple of 4: " + blobBytes.Length);
        }

        var codePointsCount = blobBytes.Length / 4;

        var stringBuilder = new System.Text.StringBuilder(capacity: codePointsCount);

        for (var index = 0; index < codePointsCount; index++)
        {
            var codePoint =
                System.Buffers.Binary.BinaryPrimitives.ReadInt32BigEndian(blobBytes.Span[(index * 4)..]);

            if (!UnicodeUtility.IsValidUnicodeScalar(codePoint))
            {
                return
                    StringParseResult.Err(
                        "Blob value at [" + index + "] out of range: " + codePoint);
            }

            stringBuilder.Append(char.ConvertFromUtf32(codePoint));
        }

        var asString =
            PopularValues.InternIfKnown(stringBuilder.ToString());

        return StringParseResult.Ok(asString);
    }

    public static Result<string, string> StringFromListValue(PineValue.ListValue list)
    {
        if (list.Items.Length is 0)
            return s_emptyStringOk;

        return StringFromListValueWithoutResultAllocation(list).ToPublicResult();
    }

    private static StringParseResult StringFromListValueWithoutResultAllocation(
        PineValue.ListValue list)
    {
        if (list.Items.Length is 0)
            return StringParseResult.Ok("");

        if (CommonStringsDecodedAsList.TryGetValue(list, out var commonString))
            return StringParseResult.Ok(commonString);

        var stringBuilder = new System.Text.StringBuilder(capacity: list.Items.Length * 2);

        for (var index = 0; index < list.Items.Length; index++)
        {
            if (list.Items.Span[index] is not PineValue.BlobValue blobValue)
            {
                return
                    StringParseResult.Err(
                        "failed decoding char as integer at [" + index + "]: Not a blob");
            }

            var charInteger = IntegerEncoding.ParseUnsignedInteger(blobValue.Bytes.Span);


            if (UnicodeUtility.IsValidUnicodeScalar(charInteger))
            {
                stringBuilder.Append(char.ConvertFromUtf32((int)charInteger));
                continue;
            }

            // https://github.com/dotnet/runtime/blob/e05944dd74118db06d6987fe2724813950725737/src/libraries/System.Private.CoreLib/src/System/Char.cs#L1036
            return
                StringParseResult.Err(
                    "Char code at [" + index + "] out of range: " + charInteger);
        }

        return StringParseResult.Ok(stringBuilder.ToString());
    }

    internal readonly struct StringParseResult
    {
        private StringParseResult(string? value, string? error)
        {
            Value = value;
            Error = error;
        }

        public string? Value { get; }

        public string? Error { get; }

        public bool IsOk => Value is not null;

        public static StringParseResult Ok(string value) =>
            new(value, error: null);

        public static StringParseResult Err(string error) =>
            new(value: null, error);

        public Result<string, string> ToPublicResult() =>
            Value is { } value
            ?
            Result<string, string>.ok(value)
            :
            Result<string, string>.err(
                Error ??
                throw new InvalidOperationException("String parse result contains neither a value nor an error"));
    }

    /// <summary>
    /// Converts the specified string to a list of Unicode code points.
    /// </summary>
    /// <remarks>Each code point in the returned list corresponds to a single Unicode character, including
    /// those represented by surrogate pairs. The method does not validate the input for invalid surrogate
    /// sequences.</remarks>
    /// <param name="str">The string to convert. May contain surrogate pairs representing supplementary Unicode characters.</param>
    public static IReadOnlyList<int> ToCodePoints(string str)
    {
        // https://stackoverflow.com/questions/687359/how-would-you-get-an-array-of-unicode-code-points-from-a-net-string/28155130#28155130

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
