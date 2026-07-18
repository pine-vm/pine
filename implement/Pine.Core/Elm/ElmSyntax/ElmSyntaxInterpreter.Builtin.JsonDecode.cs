using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Numerics;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Builtin implementation of <c>Json.Decode.parseValue</c> from
/// <c>elm-kernel-modules/Json/Decode.elm</c>, computed directly on the interpreter's value
/// model (<see cref="PineValueInProcess"/>) instead of by interpreting the recursive Elm
/// declarations.
/// <para>
/// The whole recursive-descent JSON parser (objects, arrays, numbers, strings with their
/// escape sequences and surrogate pairs) runs inside this C# port, so a single application of
/// <c>parseValue</c> short-circuits the entire Elm parser. The port mirrors the Elm code
/// function-by-function: the byte offsets it returns and the error messages it produces are
/// identical to the Elm implementation, so the result is observationally equal to running the
/// user-defined declarations.
/// </para>
/// <para>
/// Because the recursion happens entirely in C#, the interpreter only ever dispatches the
/// top-level application; the interpreter's performance counters therefore do not grow with the
/// length of the parsed string, the number of list items, or the magnitude of parsed integers.
/// </para>
/// </summary>
public partial class ElmSyntaxInterpreter
{
    // Tag-name encodings of the Json.Encode.Value constructors and the Result constructors,
    // matching the generic choice-type-tag encoding the interpreter uses
    // (a tagged value [ <constructor-name-as-UTF32-blob>, [ args... ] ]).
    private static readonly PineValueInProcess s_jsonNullValue =
        PineValueInProcess.CreateTagged(
            PineValueInProcess.Create(StringEncoding.ValueFromString("NullValue")),
            []);

    private static readonly PineValueInProcess s_jsonBoolValueTagName =
        PineValueInProcess.Create(StringEncoding.ValueFromString("BoolValue"));

    private static readonly PineValueInProcess s_jsonIntValueTagName =
        PineValueInProcess.Create(StringEncoding.ValueFromString("IntValue"));

    private static readonly PineValueInProcess s_jsonFloatValueTagName =
        PineValueInProcess.Create(StringEncoding.ValueFromString("FloatValue"));

    private static readonly PineValueInProcess s_jsonStringValueTagName =
        PineValueInProcess.Create(StringEncoding.ValueFromString("StringValue"));

    private static readonly PineValueInProcess s_jsonArrayValueTagName =
        PineValueInProcess.Create(StringEncoding.ValueFromString("ArrayValue"));

    private static readonly PineValueInProcess s_jsonObjectValueTagName =
        PineValueInProcess.Create(StringEncoding.ValueFromString("ObjectValue"));

    private static readonly PineValueInProcess s_resultOkTagName =
        PineValueInProcess.Create(StringEncoding.ValueFromString("Ok"));

    private static readonly PineValueInProcess s_resultErrTagName =
        PineValueInProcess.Create(StringEncoding.ValueFromString("Err"));

    private static readonly PineValueInProcess s_jsonBoolTrueValue =
        PineValueInProcess.CreateTagged(s_jsonBoolValueTagName, [PineValueInProcess.CreateBool(true)]);

    private static readonly PineValueInProcess s_jsonBoolFalseValue =
        PineValueInProcess.CreateTagged(s_jsonBoolValueTagName, [PineValueInProcess.CreateBool(false)]);

    /// <summary>
    /// Result of parsing a JSON value: exactly one of <see cref="OkValue"/> / <see cref="ErrorMessage"/>
    /// is non-null. <see cref="Offset"/> is the byte offset reached, mirroring the second component of
    /// the Elm <c>( Result String Value, Int )</c> tuple.
    /// </summary>
    private readonly record struct JsonValueParse(
        PineValueInProcess? OkValue,
        string? ErrorMessage,
        int Offset);

    private readonly record struct JsonIntParse(
        BigInteger? OkValue,
        string? ErrorMessage,
        int Offset);

    /// <summary>
    /// Builtin implementation of <c>Json.Decode.parseValue srcBytes offset</c>. The first argument is
    /// the raw UTF-32 characters blob of the string being parsed (the <c>Int</c> wrapped by the Elm
    /// <c>String</c> type), the second is the byte offset to start at. Returns the Elm tuple
    /// <c>( Result String Value, Int )</c>. Defers (returns null) when the arguments are not a
    /// blob / integer, so the user-defined implementation handles any unexpected shapes.
    /// </summary>
    private static PineValueInProcess? ResolveJsonDecodeParseValue(IReadOnlyList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var srcValue = arguments[0].Evaluate();

        System.ReadOnlyMemory<byte> src;

        if (srcValue is PineValue.BlobValue srcBlob)
        {
            src = srcBlob.Bytes;
        }
        else if (srcValue == PineValue.EmptyList)
        {
            src = System.ReadOnlyMemory<byte>.Empty;
        }
        else
        {
            // The source is not a plain characters blob: defer to the user-defined implementation.
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(arguments[1].Evaluate()).IsOkOrNullable() is not { } offsetBig ||
            offsetBig < int.MinValue || offsetBig > int.MaxValue)
        {
            // The offset is not a plain (in-range) integer: defer.
            return null;
        }

        var parsed = JsonParseValue(src, (int)offsetBig);

        var resultValue =
            parsed.ErrorMessage is { } errorMessage
            ?
            PineValueInProcess.CreateTagged(s_resultErrTagName, [MakeElmStringFromDotnet(errorMessage)])
            :
            PineValueInProcess.CreateTagged(s_resultOkTagName, [parsed.OkValue!]);

        return
            PineValueInProcess.CreateList(
                [resultValue, PineValueInProcess.CreateInteger(parsed.Offset)]);
    }

    internal static PineValue? JsonDecodeParseValue(PineValue source, PineValue offset) =>
        ResolveJsonDecodeParseValue(
            [PineValueInProcess.Create(source), PineValueInProcess.Create(offset)])
        ?.Evaluate();

    // ============================================================
    // Byte-level helpers mirroring Pine_kernel.take / skip on blobs.
    // ============================================================

    private static System.ReadOnlyMemory<byte> JsonSkip(int count, System.ReadOnlyMemory<byte> source)
    {
        if (count <= 0)
        {
            return source;
        }

        if (count >= source.Length)
        {
            return System.ReadOnlyMemory<byte>.Empty;
        }

        return source[count..];
    }

    private static System.ReadOnlyMemory<byte> JsonTake(int count, System.ReadOnlyMemory<byte> source)
    {
        if (count <= 0)
        {
            return System.ReadOnlyMemory<byte>.Empty;
        }

        if (count >= source.Length)
        {
            return source;
        }

        return source[..count];
    }

    /// <summary>Reads up to four bytes (one code point) at the given byte offset.</summary>
    private static System.ReadOnlyMemory<byte> JsonNextChar(System.ReadOnlyMemory<byte> src, int offset) =>
        JsonTake(4, JsonSkip(offset, src));

    /// <summary>Whether the four-byte slice equals the big-endian UTF-32 encoding of the code point.</summary>
    private static bool JsonCharIs(System.ReadOnlyMemory<byte> nextChar, int codePoint)
    {
        if (nextChar.Length is not 4)
        {
            return false;
        }

        var span = nextChar.Span;

        return
            span[0] == (byte)(codePoint >> 24) &&
            span[1] == (byte)(codePoint >> 16) &&
            span[2] == (byte)(codePoint >> 8) &&
            span[3] == (byte)codePoint;
    }

    /// <summary>Whether the slice equals the big-endian UTF-32 encoding of the ASCII string.</summary>
    private static bool JsonSliceEquals(System.ReadOnlyMemory<byte> slice, string asciiLiteral)
    {
        if (slice.Length != asciiLiteral.Length * 4)
        {
            return false;
        }

        var span = slice.Span;

        for (var i = 0; i < asciiLiteral.Length; ++i)
        {
            var c = asciiLiteral[i];

            if (span[i * 4] is not 0 ||
                span[(i * 4) + 1] is not 0 ||
                span[(i * 4) + 2] is not 0 ||
                span[(i * 4) + 3] != (byte)c)
            {
                return false;
            }
        }

        return true;
    }

    /// <summary>Decodes a single four-byte code point slice into a .NET string for error messages.</summary>
    private static string JsonCharToDotnetString(System.ReadOnlyMemory<byte> nextChar)
    {
        if (nextChar.Length is not 4)
        {
            return "";
        }

        var span = nextChar.Span;

        var codePoint =
            (span[0] << 24) | (span[1] << 16) | (span[2] << 8) | span[3];

        return char.ConvertFromUtf32(codePoint);
    }

    /// <summary>Decodes a UTF-32 big-endian bytes blob into a .NET string (used for object keys in errors).</summary>
    private static string JsonBytesToDotnetString(System.ReadOnlyMemory<byte> bytes)
    {
        var builder = new System.Text.StringBuilder(bytes.Length / 4);

        var span = bytes.Span;

        for (var i = 0; i + 4 <= bytes.Length; i += 4)
        {
            var codePoint =
                (span[i] << 24) | (span[i + 1] << 16) | (span[i + 2] << 8) | span[i + 3];

            builder.Append(char.ConvertFromUtf32(codePoint));
        }

        return builder.ToString();
    }

    /// <summary>Appends the big-endian UTF-32 encoding of a code point to the segment buffer.</summary>
    private static void JsonWriteCodePoint(System.IO.MemoryStream buffer, int codePoint)
    {
        buffer.WriteByte((byte)(codePoint >> 24));
        buffer.WriteByte((byte)(codePoint >> 16));
        buffer.WriteByte((byte)(codePoint >> 8));
        buffer.WriteByte((byte)codePoint);
    }

    /// <summary>Builds an Elm <c>String</c> value from a .NET string.</summary>
    private static PineValueInProcess MakeElmStringFromDotnet(string value) =>
        MakeElmString(StringEncoding.BlobValueFromString(value));

    private static bool JsonIsDigit(System.ReadOnlyMemory<byte> nextChar)
    {
        if (nextChar.Length is not 4)
        {
            return false;
        }

        var span = nextChar.Span;

        if (span[0] is not 0 || span[1] is not 0 || span[2] is not 0)
        {
            return false;
        }

        return span[3] is >= 0x30 and <= 0x39;
    }

    // ============================================================
    // Parser, ported function-by-function from Json/Decode.elm.
    // ============================================================

    private static int JsonSkipWhitespace(System.ReadOnlyMemory<byte> src, int offset)
    {
        while (true)
        {
            var nextChar = JsonNextChar(src, offset);

            if (JsonCharIs(nextChar, ' ') ||
                JsonCharIs(nextChar, '\t') ||
                JsonCharIs(nextChar, '\n') ||
                JsonCharIs(nextChar, '\r'))
            {
                offset += 4;
                continue;
            }

            return offset;
        }
    }

    private static JsonValueParse JsonParseValue(System.ReadOnlyMemory<byte> src, int offset0)
    {
        var offset1 = JsonSkipWhitespace(src, offset0);

        var nextChar = JsonNextChar(src, offset1);

        if (nextChar.Length is 0)
        {
            return new JsonValueParse(null, "Unexpected end of input while parsing value", offset1);
        }

        if (JsonCharIs(nextChar, 'n'))
        {
            return JsonParseNull(src, offset1);
        }

        if (JsonCharIs(nextChar, 't'))
        {
            return JsonParseTrue(src, offset1);
        }

        if (JsonCharIs(nextChar, 'f'))
        {
            return JsonParseFalse(src, offset1);
        }

        if (JsonCharIs(nextChar, '"'))
        {
            var (strBytes, strErr, offset2) = JsonParseStringLiteral(src, offset1 + 4);

            if (strErr is not null)
            {
                return new JsonValueParse(null, strErr, offset2);
            }

            return
                new JsonValueParse(
                    PineValueInProcess.CreateTagged(s_jsonStringValueTagName, [MakeElmString(strBytes!)]),
                    null,
                    offset2);
        }

        if (JsonCharIs(nextChar, '['))
        {
            var (items, arrErr, offset2) = JsonParseArray(src, offset1 + 4);

            if (arrErr is not null)
            {
                return new JsonValueParse(null, arrErr, offset2);
            }

            return
                new JsonValueParse(
                    PineValueInProcess.CreateTagged(
                        s_jsonArrayValueTagName,
                        [PineValueInProcess.CreateList(items!)]),
                    null,
                    offset2);
        }

        if (JsonCharIs(nextChar, '{'))
        {
            var (fields, objErr, offset2) = JsonParseObjectRec([], src, offset1 + 4);

            if (objErr is not null)
            {
                return new JsonValueParse(null, objErr, offset2);
            }

            return
                new JsonValueParse(
                    PineValueInProcess.CreateTagged(
                        s_jsonObjectValueTagName,
                        [PineValueInProcess.CreateList(fields!)]),
                    null,
                    offset2);
        }

        if (JsonCharIs(nextChar, '-'))
        {
            return JsonParseNumber(src, offset1);
        }

        if (JsonIsDigit(nextChar))
        {
            return JsonParseNumber(src, offset1);
        }

        return
            new JsonValueParse(
                null,
                "Unexpected character while parsing value: '" + JsonCharToDotnetString(nextChar) + "'",
                offset1);
    }

    private static JsonValueParse JsonParseNull(System.ReadOnlyMemory<byte> src, int offset0)
    {
        var nextChars = JsonTake(16, JsonSkip(offset0, src));

        if (JsonSliceEquals(nextChars, "null"))
        {
            return new JsonValueParse(s_jsonNullValue, null, offset0 + 16);
        }

        return new JsonValueParse(null, "Expecting 'null'", offset0);
    }

    private static JsonValueParse JsonParseTrue(System.ReadOnlyMemory<byte> src, int offset0)
    {
        var nextChars = JsonTake(16, JsonSkip(offset0, src));

        if (JsonSliceEquals(nextChars, "true"))
        {
            return new JsonValueParse(s_jsonBoolTrueValue, null, offset0 + 16);
        }

        return new JsonValueParse(null, "Expecting 'true'", offset0);
    }

    private static JsonValueParse JsonParseFalse(System.ReadOnlyMemory<byte> src, int offset0)
    {
        var nextChars = JsonTake(20, JsonSkip(offset0, src));

        if (JsonSliceEquals(nextChars, "false"))
        {
            return new JsonValueParse(s_jsonBoolFalseValue, null, offset0 + 20);
        }

        return new JsonValueParse(null, "Expecting 'false'", offset0);
    }

    private static JsonValueParse JsonParseNumber(System.ReadOnlyMemory<byte> src, int offset0)
    {
        var (intValue, intErr, offset1) = JsonParseInt(src, offset0);

        if (intErr is not null)
        {
            return new JsonValueParse(null, intErr, offset1);
        }

        var nextChar = JsonNextChar(src, offset1);

        if (JsonCharIs(nextChar, '.'))
        {
            var (denominator, denomErr, offset2) = JsonParseUnsignedInt(src, offset1 + 4);

            if (denomErr is not null)
            {
                return new JsonValueParse(null, denomErr, offset2);
            }

            // The float is kept as the raw source slice (matching the Elm implementation, which
            // takes the substring from the start of the number through the fractional part).
            var sliceLength = offset2 - offset0;

            var sliceBytes = JsonTake(sliceLength, JsonSkip(offset0, src));

            return
                new JsonValueParse(
                    PineValueInProcess.CreateTagged(s_jsonFloatValueTagName, [MakeElmString(sliceBytes)]),
                    null,
                    offset2);
        }

        return
            new JsonValueParse(
                PineValueInProcess.CreateTagged(
                    s_jsonIntValueTagName,
                    [PineValueInProcess.CreateInteger(intValue!.Value)]),
                null,
                offset1);
    }

    private static JsonIntParse JsonParseInt(System.ReadOnlyMemory<byte> src, int offset0)
    {
        var nextChar = JsonNextChar(src, offset0);

        if (JsonCharIs(nextChar, '-'))
        {
            var (unsigned, unsignedErr, offset1) = JsonParseUnsignedInt(src, offset0 + 4);

            if (unsignedErr is not null)
            {
                return new JsonIntParse(null, unsignedErr, offset1);
            }

            return new JsonIntParse(-unsigned!.Value, null, offset1);
        }

        return JsonParseUnsignedInt(src, offset0);
    }

    private static JsonIntParse JsonParseUnsignedInt(System.ReadOnlyMemory<byte> src, int offset0)
    {
        var nextChar = JsonNextChar(src, offset0);

        if (JsonTryDigit(nextChar, out var digit))
        {
            if (digit is 0)
            {
                return new JsonIntParse(BigInteger.Zero, null, offset0 + 4);
            }

            return JsonParseUnsignedIntRec(digit, src, offset0 + 4);
        }

        return new JsonIntParse(null, "Expecting a digit", offset0);
    }

    private static JsonIntParse JsonParseUnsignedIntRec(BigInteger upper, System.ReadOnlyMemory<byte> src, int offset0)
    {
        while (true)
        {
            var nextChar = JsonNextChar(src, offset0);

            if (JsonTryDigit(nextChar, out var digit))
            {
                upper = (upper * 10) + digit;
                offset0 += 4;
                continue;
            }

            return new JsonIntParse(upper, null, offset0);
        }
    }

    private static bool JsonTryDigit(System.ReadOnlyMemory<byte> nextChar, out int digit)
    {
        digit = 0;

        if (!JsonIsDigit(nextChar))
        {
            return false;
        }

        digit = nextChar.Span[3] - 0x30;

        return true;
    }

    private static (ImmutableList<PineValueInProcess>? Items, string? Error, int Offset)
        JsonParseArray(System.ReadOnlyMemory<byte> src, int offset0)
    {
        var offset1 = JsonSkipWhitespace(src, offset0);

        var nextChar = JsonNextChar(src, offset1);

        if (nextChar.Length is 0)
        {
            return (null, "Unexpected end of input while parsing array", offset1);
        }

        if (JsonCharIs(nextChar, ']'))
        {
            return (ImmutableList<PineValueInProcess>.Empty, null, offset1 + 4);
        }

        return JsonParseArrayItems(ImmutableList<PineValueInProcess>.Empty, src, offset1);
    }

    private static (ImmutableList<PineValueInProcess>? Items, string? Error, int Offset)
        JsonParseArrayItems(
        ImmutableList<PineValueInProcess> itemsBefore,
        System.ReadOnlyMemory<byte> src,
        int offset0)
    {
        while (true)
        {
            var valParse = JsonParseValue(src, offset0);

            if (valParse.ErrorMessage is { } valErr)
            {
                return (null, valErr, valParse.Offset);
            }

            var offset1 = JsonSkipWhitespace(src, valParse.Offset);

            var nextChar = JsonNextChar(src, offset1);

            var items = itemsBefore.Add(valParse.OkValue!);

            if (nextChar.Length is 0)
            {
                return (null, "Unclosed array, expected ',' or ']'", offset1);
            }

            if (JsonCharIs(nextChar, ','))
            {
                itemsBefore = items;
                offset0 = offset1 + 4;
                continue;
            }

            if (JsonCharIs(nextChar, ']'))
            {
                return (items, null, offset1 + 4);
            }

            return (null, "Expecting ',' or ']', got '" + JsonCharToDotnetString(nextChar) + "'", offset1);
        }
    }

    private static (ImmutableList<PineValueInProcess>? Fields, string? Error, int Offset)
        JsonParseObjectRec(
        ImmutableList<PineValueInProcess> fieldsBefore,
        System.ReadOnlyMemory<byte> src,
        int offset0)
    {
        while (true)
        {
            var offset1 = JsonSkipWhitespace(src, offset0);

            var nextChar = JsonNextChar(src, offset1);

            if (nextChar.Length is 0)
            {
                return (null, "Unexpected end of input while parsing object", offset1);
            }

            if (JsonCharIs(nextChar, '}'))
            {
                return (fieldsBefore, null, offset1 + 4);
            }

            if (!JsonCharIs(nextChar, '"'))
            {
                return
                    (null,
                    "Expecting '\"' to start object key, got '" + JsonCharToDotnetString(nextChar) + "'",
                    offset1);
            }

            var (keyBytes, keyErr, offsetAfterKey) = JsonParseStringLiteral(src, offset1 + 4);

            if (keyErr is not null)
            {
                return (null, keyErr, offsetAfterKey);
            }

            var offset2 = JsonSkipWhitespace(src, offsetAfterKey);

            var nextChar2 = JsonNextChar(src, offset2);

            if (!JsonCharIs(nextChar2, ':'))
            {
                return
                    (null,
                    "Expecting ':' after object key '" + JsonBytesToDotnetString(keyBytes!) + "'",
                    offset2);
            }

            var offset3 = JsonSkipWhitespace(src, offset2 + 4);

            var valParse = JsonParseValue(src, offset3);

            if (valParse.ErrorMessage is { } valErr)
            {
                return (null, "Error parsing object value: " + valErr, valParse.Offset);
            }

            var offset4 = JsonSkipWhitespace(src, valParse.Offset);

            var nextChar3 = JsonNextChar(src, offset4);

            var pair =
                PineValueInProcess.CreateList(
                    [MakeElmString(keyBytes!), valParse.OkValue!]);

            var fields = fieldsBefore.Add(pair);

            if (nextChar3.Length is 0)
            {
                return (null, "Unexpected end of input while reading JSON object", offset4);
            }

            if (JsonCharIs(nextChar3, ','))
            {
                fieldsBefore = fields;
                offset0 = offset4 + 4;
                continue;
            }

            if (JsonCharIs(nextChar3, '}'))
            {
                return (fields, null, offset4 + 4);
            }

            return (null, "Expecting ',' or '}', got '" + JsonCharToDotnetString(nextChar3) + "'", offset4);
        }
    }

    private static (byte[]? Bytes, string? Error, int Offset)
        JsonParseStringLiteral(System.ReadOnlyMemory<byte> src, int offset)
    {
        var segments = new System.IO.MemoryStream();

        while (true)
        {
            var simpleEnd = JsonParseStringSimpleChars(src, offset);

            var simpleSlice = JsonTake(simpleEnd - offset, JsonSkip(offset, src));

            segments.Write(simpleSlice.Span);

            var nextChar = JsonNextChar(src, simpleEnd);

            if (JsonCharIs(nextChar, '"'))
            {
                return (segments.ToArray(), null, simpleEnd + 4);
            }

            if (JsonCharIs(nextChar, '\\'))
            {
                var (escapedCode, escapeErr, newOffset) = JsonParseEscape(src, simpleEnd);

                if (escapeErr is not null)
                {
                    return (null, escapeErr, newOffset);
                }

                JsonWriteCodePoint(segments, escapedCode!.Value);

                offset = newOffset;
                continue;
            }

            return (null, "Unexpected end of input while reading JSON string", simpleEnd);
        }
    }

    private static int JsonParseStringSimpleChars(System.ReadOnlyMemory<byte> src, int offset)
    {
        while (true)
        {
            var nextChar = JsonNextChar(src, offset);

            if (nextChar.Length is 0)
            {
                return offset;
            }

            if (JsonCharIs(nextChar, '"') || JsonCharIs(nextChar, '\\'))
            {
                return offset;
            }

            offset += 4;
        }
    }

    private static (int? CodePoint, string? Error, int Offset)
        JsonParseEscape(System.ReadOnlyMemory<byte> src, int offset)
    {
        // We already know src[offset] == '\\'.
        var nextChar = JsonNextChar(src, offset + 4);

        if (JsonCharIs(nextChar, 'n'))
        {
            return (0x0A, null, offset + 8);
        }

        if (JsonCharIs(nextChar, 'r'))
        {
            return (0x0D, null, offset + 8);
        }

        if (JsonCharIs(nextChar, 't'))
        {
            return (0x09, null, offset + 8);
        }

        if (JsonCharIs(nextChar, '"'))
        {
            return (0x22, null, offset + 8);
        }

        if (JsonCharIs(nextChar, '\\'))
        {
            return (0x5C, null, offset + 8);
        }

        if (JsonCharIs(nextChar, '/'))
        {
            return (0x2F, null, offset + 8);
        }

        if (JsonCharIs(nextChar, 'b'))
        {
            return (8, null, offset + 8);
        }

        if (JsonCharIs(nextChar, 'f'))
        {
            return (12, null, offset + 8);
        }

        if (JsonCharIs(nextChar, 'u'))
        {
            return JsonParseUnicodeEscape(src, offset + 8);
        }

        return (null, "Unrecognized escape sequence: " + JsonCharToDotnetString(nextChar), offset + 8);
    }

    private static (int? CodePoint, string? Error, int Offset)
        JsonParseUnicodeEscape(System.ReadOnlyMemory<byte> src, int offset)
    {
        var fourHexChars = JsonTake(16, JsonSkip(offset, src));

        var (codeUnit, offsetAfterHex) = JsonConvert1OrMoreHexadecimal(0, fourHexChars);

        var hi = codeUnit;

        var followingTwoChars = JsonTake(8, JsonSkip(offset + 16, src));

        if (offsetAfterHex is not 16)
        {
            return (null, "Unexpected end of input in \\u escape (need 4 hex digits)", offset + 6);
        }

        if (hi is < 0xD800 or > 0xDBFF)
        {
            // Not a high surrogate: parse `\uXXXX` as a single character.
            return (hi, null, offset + 16);
        }

        if (!JsonSliceEquals(followingTwoChars, "\\u"))
        {
            // No second "\u" — decode `hi` as-is.
            return (hi, null, offset + 16);
        }

        var fourHexChars2 = JsonTake(16, JsonSkip(offset + 24, src));

        var (codeUnit2, offsetAfterHex2) = JsonConvert1OrMoreHexadecimal(0, fourHexChars2);

        var lo = codeUnit2;

        if (offsetAfterHex2 is not 16)
        {
            return (null, "Unexpected end of input in second \\u of a surrogate pair", offset + 16 + 8 + 24);
        }

        if (lo is < 0xDC00 or > 0xDFFF)
        {
            // The "\u" was not in the low surrogate range: treat `hi` as a normal char.
            return (hi, null, offset + 16);
        }

        var fullCodePoint = 0x00010000 + ((hi - 0xD800) * 0x0400) + lo - 0xDC00;

        return (fullCodePoint, null, offset + 16 + 8 + 16);
    }

    private static (int Value, int Offset)
        JsonConvert1OrMoreHexadecimal(int offset, System.ReadOnlyMemory<byte> src)
    {
        var nextChar = JsonNextChar(src, offset);

        if (JsonTryHexDigit(nextChar, out var digit))
        {
            return JsonConvert0OrMoreHexadecimal(digit, offset + 4, src);
        }

        return (0, -1);
    }

    private static (int Value, int Offset)
        JsonConvert0OrMoreHexadecimal(int soFar, int offset, System.ReadOnlyMemory<byte> src)
    {
        while (true)
        {
            var nextChar = JsonNextChar(src, offset);

            if (nextChar.Length is 0)
            {
                return (soFar, offset);
            }

            if (JsonTryHexDigit(nextChar, out var digit))
            {
                soFar = (soFar * 16) + digit;
                offset += 4;
                continue;
            }

            return (0, -1);
        }
    }

    private static bool JsonTryHexDigit(System.ReadOnlyMemory<byte> nextChar, out int digit)
    {
        digit = 0;

        if (nextChar.Length is not 4)
        {
            return false;
        }

        var span = nextChar.Span;

        if (span[0] is not 0 || span[1] is not 0 || span[2] is not 0)
        {
            return false;
        }

        var c = span[3];

        if (c is >= 0x30 and <= 0x39)
        {
            digit = c - 0x30;
            return true;
        }

        if (c is >= 0x61 and <= 0x66)
        {
            digit = (c - 0x61) + 10;
            return true;
        }

        if (c is >= 0x41 and <= 0x46)
        {
            digit = (c - 0x41) + 10;
            return true;
        }

        return false;
    }
}
