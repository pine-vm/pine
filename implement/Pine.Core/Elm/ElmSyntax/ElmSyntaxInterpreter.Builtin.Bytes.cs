using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using System.Collections.Generic;
using System.Numerics;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Builtins for the <c>Bytes.Encode</c>, <c>Bytes.Decode</c>, and base64
/// (<c>Base64.Encode</c> / <c>Base64.Decode</c>) kernel/core functions. Each one
/// short-circuits the recursive Elm-source implementation it mirrors while preserving the
/// exact observable result on the interpreter's value model.
/// <para>
/// The mirrored Elm sources are:
/// <list type="bullet">
///   <item><c>elm-kernel-modules/Bytes/Encode.elm</c> — <c>encodeCharsAsBlob</c>, <c>encodeBlob</c></item>
///   <item><c>elm-kernel-modules/Bytes/Decode.elm</c> — <c>decodeBlobAsCharsRec</c></item>
///   <item><c>src/Base64/Encode.elm</c> — <c>toBytes</c></item>
///   <item><c>src/Base64/Decode.elm</c> — <c>fromBytes</c></item>
/// </list>
/// </para>
/// </summary>
public partial class ElmSyntaxInterpreter
{
    // Tag-name encodings of the Bytes.Encode.Encoder constructors.
    private static readonly PineValue s_encoderI8TagNameValue = StringEncoding.ValueFromString("I8");

    private static readonly PineValue s_encoderI16TagNameValue = StringEncoding.ValueFromString("I16");

    private static readonly PineValue s_encoderI32TagNameValue = StringEncoding.ValueFromString("I32");

    private static readonly PineValue s_encoderU8TagNameValue = StringEncoding.ValueFromString("U8");

    private static readonly PineValue s_encoderU16TagNameValue = StringEncoding.ValueFromString("U16");

    private static readonly PineValue s_encoderU32TagNameValue = StringEncoding.ValueFromString("U32");

    private static readonly PineValue s_encoderSequenceTagNameValue = StringEncoding.ValueFromString("SequenceEncoder");

    private static readonly PineValue s_encoderBytesTagNameValue = StringEncoding.ValueFromString("BytesEncoder");

    // Tag-name encoding of the Bytes.Endianness.LE constructor (BE is anything that is not LE).
    private static readonly PineValue s_endiannessLittleEndianTagNameValue = StringEncoding.ValueFromString("LE");

    // Tag-name encoding of the Bytes.Bytes wrapper constructor (Elm_Bytes).
    private static readonly PineValueInProcess s_bytesElmBytesTagName =
        PineValueInProcess.Create(ElmValue.ElmBytesTypeTagNameAsValue);

    /// <summary>
    /// Returns the raw bytes of an in-process value that evaluates to a blob, treating the
    /// empty list as an empty blob (the kernel uses <c>Pine_kernel.take [ 0, 0 ]</c> to
    /// denote an empty byte sequence). Throws for any other shape.
    /// </summary>
    private static System.ReadOnlyMemory<byte> AsRawBlobBytes(PineValue value, string operationName)
    {
        if (value is PineValue.BlobValue blob)
            return blob.Bytes;

        if (value == PineValue.EmptyList)
            return System.ReadOnlyMemory<byte>.Empty;

        throw new System.InvalidOperationException(operationName + ": expected a blob.");
    }

    /// <summary>Builds a <c>Bytes.Bytes</c> value (<c>Elm_Bytes blob</c>) from a raw bytes blob.</summary>
    private static PineValueInProcess MakeElmBytes(System.ReadOnlyMemory<byte> bytes)
    {
        var blobValue = PineValue.Blob(bytes);

        return
            PineValueInProcess.CreateTagged(
                s_bytesElmBytesTagName,
                [PineValueInProcess.Create(blobValue)]);
    }

    /// <summary>
    /// Appends the UTF-8 encoding of the Unicode code point <paramref name="code"/> to
    /// <paramref name="output"/>, mirroring <c>encodeCharAsBlob</c> from
    /// <c>elm-kernel-modules/Bytes/Encode.elm</c> byte-for-byte (including its integer-division
    /// and bitwise arithmetic).
    /// </summary>
    private static void AppendCodePointUtf8(long code, System.IO.Stream output)
    {
        if (code <= 0x7F)
        {
            output.WriteByte((byte)(code & 0xFF));
        }
        else if (code <= 0x07FF)
        {
            output.WriteByte((byte)((0xC0 | (code / 64)) & 0xFF));
            output.WriteByte((byte)((0x80 | (code & 63)) & 0xFF));
        }
        else if (code <= 0xFFFF)
        {
            output.WriteByte((byte)((0xE0 | (code / 4096)) & 0xFF));
            output.WriteByte((byte)((0x80 | ((code / 64) & 63)) & 0xFF));
            output.WriteByte((byte)((0x80 | (code & 63)) & 0xFF));
        }
        else
        {
            output.WriteByte((byte)((0xF0 | (code / 262144)) & 0xFF));
            output.WriteByte((byte)((0x80 | ((code / 4096) & 63)) & 0xFF));
            output.WriteByte((byte)((0x80 | ((code / 64) & 63)) & 0xFF));
            output.WriteByte((byte)((0x80 | (code & 63)) & 0xFF));
        }
    }

    /// <summary>
    /// Builtin implementation of <c>Bytes.Encode.encodeCharsAsBlob</c> directly on the
    /// interpreter's value model, mirroring <c>elm-kernel-modules/Bytes/Encode.elm</c>: reads the
    /// UTF-32 (four-byte big-endian) code points of the supplied characters blob and produces the
    /// corresponding UTF-8 encoded blob.
    /// </summary>
    private static PineValueInProcess? ResolveBytesEncodeEncodeCharsAsBlob(IReadOnlyList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var charsBytes = AsRawBlobBytes(arguments[0].Evaluate(), "Bytes.Encode.encodeCharsAsBlob").Span;

        using var output = new System.IO.MemoryStream();

        for (var offset = 0; offset < charsBytes.Length;)
        {
            var chunkLength = System.Math.Min(4, charsBytes.Length - offset);

            long code = 0;

            for (var i = 0; i < chunkLength; ++i)
            {
                code = (code << 8) | charsBytes[offset + i];
            }

            AppendCodePointUtf8(code, output);

            offset += chunkLength;
        }

        return PineValueInProcess.Create(PineValue.Blob(output.ToArray()));
    }

    /// <summary>
    /// Builtin implementation of <c>Bytes.Encode.encodeBlob</c> directly on the interpreter's value
    /// model, mirroring <c>elm-kernel-modules/Bytes/Encode.elm</c>: recursively renders an
    /// <c>Encoder</c> value into the raw bytes blob it represents.
    /// </summary>
    private static PineValueInProcess? ResolveBytesEncodeEncodeBlob(IReadOnlyList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        using var output = new System.IO.MemoryStream();

        AppendEncoderBlob(arguments[0].Evaluate(), output);

        return PineValueInProcess.Create(PineValue.Blob(output.ToArray()));
    }

    private static void AppendEncoderBlob(PineValue encoder, System.IO.Stream output)
    {
        if (encoder is not PineValue.ListValue { Items.Length: 2 } encoderList)
        {
            throw new System.InvalidOperationException(
                "Bytes.Encode.encodeBlob: expected an Encoder tagged value.");
        }

        var tagName = encoderList.Items.Span[0];

        var tagArgs =
            encoderList.Items.Span[1] is PineValue.ListValue argsList
            ?
            argsList.Items
            :
            System.ReadOnlyMemory<PineValue>.Empty;

        if (tagName == s_encoderI8TagNameValue || tagName == s_encoderU8TagNameValue)
        {
            var n = ParseEncoderInteger(tagArgs.Span[0]);

            output.WriteByte((byte)(int)(n & 0xFF));

            return;
        }

        if (tagName == s_encoderI16TagNameValue || tagName == s_encoderU16TagNameValue)
        {
            var littleEndian = IsLittleEndian(tagArgs.Span[0]);
            var n = ParseEncoderInteger(tagArgs.Span[1]);

            var value = (int)(n & 0xFFFF);

            var high = (byte)((value >> 8) & 0xFF);
            var low = (byte)(value & 0xFF);

            if (littleEndian)
            {
                output.WriteByte(low);
                output.WriteByte(high);
            }
            else
            {
                output.WriteByte(high);
                output.WriteByte(low);
            }

            return;
        }

        if (tagName == s_encoderI32TagNameValue || tagName == s_encoderU32TagNameValue)
        {
            var littleEndian = IsLittleEndian(tagArgs.Span[0]);
            var n = ParseEncoderInteger(tagArgs.Span[1]);

            var value = (uint)(n & 0xFFFFFFFF);

            System.Span<byte> bigEndian =
                [
                (byte)((value >> 24) & 0xFF),
                (byte)((value >> 16) & 0xFF),
                (byte)((value >> 8) & 0xFF),
                (byte)(value & 0xFF),
                ];

            if (littleEndian)
            {
                for (var i = 3; i >= 0; --i)
                    output.WriteByte(bigEndian[i]);
            }
            else
            {
                for (var i = 0; i < 4; ++i)
                    output.WriteByte(bigEndian[i]);
            }

            return;
        }

        if (tagName == s_encoderSequenceTagNameValue)
        {
            if (tagArgs.Span[0] is PineValue.ListValue sequenceItems)
            {
                for (var i = 0; i < sequenceItems.Items.Length; ++i)
                {
                    AppendEncoderBlob(sequenceItems.Items.Span[i], output);
                }
            }

            return;
        }

        if (tagName == s_encoderBytesTagNameValue)
        {
            // BytesEncoder (Bytes.Elm_Bytes blob): emit the wrapped blob verbatim.
            var innerBlob = AsBytesValueBlob(tagArgs.Span[0], "Bytes.Encode.encodeBlob");

            output.Write(innerBlob.Span);

            return;
        }

        throw new System.InvalidOperationException(
            "Bytes.Encode.encodeBlob: unexpected Encoder constructor.");
    }

    /// <summary>
    /// Parses a (non-negative or negative) integer argument of an <c>Encoder</c> constructor.
    /// These are ordinary Elm integers (sign-prefixed blobs), in contrast with the raw byte blobs
    /// produced by the encoders.
    /// </summary>
    private static BigInteger ParseEncoderInteger(PineValue value)
    {
        if (IntegerEncoding.ParseSignedIntegerRelaxed(value).IsOkOrNullable() is not { } integer)
        {
            throw new System.InvalidOperationException(
                "Bytes.Encode.encodeBlob: expected an integer encoder argument.");
        }

        return integer;
    }

    /// <summary>
    /// Tests whether an <c>Endianness</c> value is <c>Bytes.LE</c>; any other value (i.e.
    /// <c>Bytes.BE</c>) is treated as big-endian, matching the Elm <c>if e == Bytes.LE</c> checks.
    /// </summary>
    private static bool IsLittleEndian(PineValue endianness) =>
        endianness is PineValue.ListValue { Items.Length: 2 } tagged &&
        tagged.Items.Span[0] == s_endiannessLittleEndianTagNameValue;

    /// <summary>
    /// Extracts the wrapped bytes blob from a <c>Bytes.Bytes</c> value (<c>Elm_Bytes blob</c>).
    /// </summary>
    private static System.ReadOnlyMemory<byte> AsBytesValueBlob(PineValue value, string operationName)
    {
        if (value is PineValue.ListValue { Items.Length: 2 } items &&
            items.Items.Span[0] == ElmValue.ElmBytesTypeTagNameAsValue &&
            items.Items.Span[1] is PineValue.ListValue { Items.Length: 1 } blobArg)
        {
            return AsRawBlobBytes(blobArg.Items.Span[0], operationName);
        }

        throw new System.InvalidOperationException(operationName + ": expected a Bytes value.");
    }

    /// <summary>
    /// Decodes the UTF-8 character starting at <paramref name="offset"/> in
    /// <paramref name="blob"/>, returning the decoded code point and the number of bytes consumed.
    /// Mirrors <c>decodeUtf8Char</c> from <c>elm-kernel-modules/Bytes/Decode.elm</c>, including its
    /// out-of-range reads (which the kernel reads as zero) and its replacement-character fallback
    /// for invalid leading bytes.
    /// </summary>
    private static (long code, int consumed) DecodeUtf8Char(System.ReadOnlySpan<byte> blob, int offset)
    {
        var firstByte = (offset < blob.Length) ? blob[offset] : 0;

        if (firstByte <= 0x7F)
        {
            return (firstByte, 1);
        }

        if ((firstByte & 0xE0) == 0xC0)
        {
            var byte2 = (offset + 1 < blob.Length) ? blob[offset + 1] : 0;

            var code = ((firstByte & 0x1F) * 64) + (byte2 & 0x3F);

            return (code, 2);
        }

        if ((firstByte & 0xF0) == 0xE0)
        {
            var byte2 = (offset + 1 < blob.Length) ? blob[offset + 1] : 0;
            var byte3 = (offset + 2 < blob.Length) ? blob[offset + 2] : 0;

            var code = ((firstByte & 0x0F) * 4096) + ((byte2 & 0x3F) * 64) + (byte3 & 0x3F);

            return (code, 3);
        }

        if ((firstByte & 0xF8) == 0xF0)
        {
            var byte2 = (offset + 1 < blob.Length) ? blob[offset + 1] : 0;
            var byte3 = (offset + 2 < blob.Length) ? blob[offset + 2] : 0;
            var byte4 = (offset + 3 < blob.Length) ? blob[offset + 3] : 0;

            var code =
                ((long)(firstByte & 0x07) * 262144) +
                ((byte2 & 0x3F) * 4096) +
                ((byte3 & 0x3F) * 64) +
                (byte4 & 0x3F);

            return (code, 4);
        }

        // Invalid UTF-8 sequence; use replacement character.
        return (0xFFFD, 1);
    }

    /// <summary>
    /// Builtin implementation of <c>Bytes.Decode.decodeBlobAsCharsRec</c> directly on the
    /// interpreter's value model, mirroring <c>elm-kernel-modules/Bytes/Decode.elm</c>: decodes the
    /// UTF-8 bytes of <c>blob</c> starting at <c>offset</c> to characters, prepending them onto the
    /// accumulator (<c>chars</c>) and finally producing <c>String.fromList (List.reverse chars)</c>.
    /// </summary>
    private static PineValueInProcess? ResolveBytesDecodeDecodeBlobAsCharsRec(
        IReadOnlyList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 3)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(arguments[0].Evaluate()).IsOkOrNullable() is not { } offsetBig)
        {
            throw new System.InvalidOperationException(
                "Bytes.Decode.decodeBlobAsCharsRec: expected an integer offset.");
        }

        var blob = AsRawBlobBytes(arguments[1].Evaluate(), "Bytes.Decode.decodeBlobAsCharsRec").Span;

        var initialChars = AsListItems(arguments[2]);

        if (initialChars is null)
        {
            throw new System.InvalidOperationException(
                "Bytes.Decode.decodeBlobAsCharsRec: expected a list of characters.");
        }

        using var output = new System.IO.MemoryStream();

        // The accumulator is reversed by the final 'List.reverse chars', so the resulting string
        // begins with the initial accumulator characters in reverse order.
        for (var i = initialChars.Count - 1; i >= 0; --i)
        {
            if (initialChars[i].Evaluate() is PineValue.BlobValue charBlob)
            {
                output.Write(charBlob.Bytes.Span);
            }
            else if (initialChars[i].Evaluate() != PineValue.EmptyList)
            {
                throw new System.InvalidOperationException(
                    "Bytes.Decode.decodeBlobAsCharsRec: expected each accumulator character to be a blob.");
            }
        }

        var offset = offsetBig < 0 ? 0 : (offsetBig > blob.Length ? blob.Length : (int)offsetBig);

        while (offset < blob.Length)
        {
            var (code, consumed) = DecodeUtf8Char(blob, offset);

            // Char.fromCode produces a four-byte big-endian code point.
            output.WriteByte((byte)((code >> 24) & 0xFF));
            output.WriteByte((byte)((code >> 16) & 0xFF));
            output.WriteByte((byte)((code >> 8) & 0xFF));
            output.WriteByte((byte)(code & 0xFF));

            offset += consumed;
        }

        return MakeElmString(output.ToArray());
    }

    // ============================================================
    // Base64
    // ============================================================

    private static bool Base64IsValidChar(long code) =>
        (code is >= 0x41 and <= 0x5A) ||
        (code is >= 0x61 and <= 0x7A) ||
        (code is >= 0x30 and <= 0x39) ||
        code is '+' or '/';

    private static int Base64ConvertChar(long code) =>
        code switch
        {
            >= 0x41 and <= 0x5A => (int)(code - 65),
            >= 0x61 and <= 0x7A => (int)(code - 97) + 26,
            >= 0x30 and <= 0x39 => (int)(code - 48) + 52,
            '+' => 62,
            '/' => 63,

            _ =>
            -1,
        };

    /// <summary>
    /// Appends the bytes for one base64 character quartet, mirroring <c>encodeCharacters</c> from
    /// <c>src/Base64/Encode.elm</c> (including its padding handling for <c>'='</c>). Returns
    /// <c>false</c> when the quartet is not valid base64 (i.e. the Elm function returns
    /// <c>Nothing</c>).
    /// </summary>
    private static bool Base64EncodeCharacters(long a, long b, long c, long d, System.IO.Stream output)
    {
        if (!(Base64IsValidChar(a) && Base64IsValidChar(b)))
            return false;

        var n1 = Base64ConvertChar(a);
        var n2 = Base64ConvertChar(b);

        if (d is '=')
        {
            if (c is '=')
            {
                var n = (n1 << 18) | (n2 << 12);
                var b1 = n >> 16;

                output.WriteByte((byte)(b1 & 0xFF));

                return true;
            }

            if (!Base64IsValidChar(c))
                return false;

            var n3 = Base64ConvertChar(c);
            var combinedBits = (n1 << 18) | (n2 << 12) | (n3 << 6);
            var combined = combinedBits >> 8;

            output.WriteByte((byte)((combined >> 8) & 0xFF));
            output.WriteByte((byte)(combined & 0xFF));

            return true;
        }

        if (!(Base64IsValidChar(c) && Base64IsValidChar(d)))
            return false;

        var nc = Base64ConvertChar(c);
        var nd = Base64ConvertChar(d);
        var bits = (n1 << 18) | (n2 << 12) | (nc << 6) | nd;
        var combinedFull = bits >> 8;

        output.WriteByte((byte)((combinedFull >> 8) & 0xFF));
        output.WriteByte((byte)(combinedFull & 0xFF));
        output.WriteByte((byte)(bits & 0xFF));

        return true;
    }

    /// <summary>
    /// Builtin implementation of <c>Base64.Encode.toBytes</c> directly on the interpreter's value
    /// model, mirroring <c>src/Base64/Encode.elm</c>: decodes a base64-encoded string into the raw
    /// <c>Bytes</c> it represents, yielding <c>Nothing</c> for malformed input.
    /// </summary>
    private static PineValueInProcess? ResolveBase64EncodeToBytes(IReadOnlyList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var charsBytes = AsStringCharsBytes(arguments[0].Evaluate(), "Base64.Encode.toBytes").Span;

        var charCount = charsBytes.Length / 4;

        using var output = new System.IO.MemoryStream();

        var index = 0;

        while (true)
        {
            var remaining = charCount - index;

            if (remaining is 0)
            {
                break;
            }

            if (remaining >= 4)
            {
                if (!Base64EncodeCharacters(
                    ReadCodePoint(charsBytes, index),
                    ReadCodePoint(charsBytes, index + 1),
                    ReadCodePoint(charsBytes, index + 2),
                    ReadCodePoint(charsBytes, index + 3),
                    output))
                {
                    return s_maybeNothingValue;
                }

                index += 4;

                continue;
            }

            if (remaining is 3)
            {
                if (!Base64EncodeCharacters(
                    ReadCodePoint(charsBytes, index),
                    ReadCodePoint(charsBytes, index + 1),
                    ReadCodePoint(charsBytes, index + 2),
                    '=',
                    output))
                {
                    return s_maybeNothingValue;
                }

                break;
            }

            if (remaining is 2)
            {
                if (!Base64EncodeCharacters(
                    ReadCodePoint(charsBytes, index),
                    ReadCodePoint(charsBytes, index + 1),
                    '=',
                    '=',
                    output))
                {
                    return s_maybeNothingValue;
                }

                break;
            }

            // remaining == 1: the Elm 'encodeChunks' falls through to its catch-all 'Nothing' case.
            return s_maybeNothingValue;
        }

        return
            PineValueInProcess.CreateTagged(
                s_maybeJustTagNameValue,
                [MakeElmBytes(output.ToArray())]);
    }

    private static char Base64UnsafeToChar(int n) =>
        n switch
        {
            <= 25 => (char)(65 + n),
            <= 51 => (char)(97 + (n - 26)),
            <= 61 => (char)(48 + (n - 52)),
            62 => '+',
            63 => '/',

            _ =>
            '\u0000',
        };

    private static void Base64AppendCharCode(char value, System.IO.Stream output)
    {
        // Each character is stored as a four-byte big-endian code point in the Elm String blob.
        output.WriteByte(0);
        output.WriteByte(0);
        output.WriteByte(0);
        output.WriteByte((byte)value);
    }

    private static long ReadCodePoint(System.ReadOnlySpan<byte> charsBytes, int charIndex) =>
        System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(charsBytes[(charIndex * 4)..]);

    /// <summary>
    /// Builtin implementation of <c>Base64.Decode.fromBytes</c> directly on the interpreter's value
    /// model, mirroring <c>src/Base64/Decode.elm</c>: encodes the raw <c>Bytes</c> argument into a
    /// padded base64 <c>String</c>. The Elm function always succeeds for in-range offsets, so this
    /// always yields <c>Just</c>.
    /// </summary>
    private static PineValueInProcess? ResolveBase64DecodeFromBytes(IReadOnlyList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var bytes = AsBytesValueBlob(arguments[0].Evaluate(), "Base64.Decode.fromBytes").Span;

        using var output = new System.IO.MemoryStream();

        var index = 0;

        for (; index + 3 <= bytes.Length; index += 3)
        {
            var combined = (bytes[index] << 16) | (bytes[index + 1] << 8) | bytes[index + 2];

            Base64AppendCharCode(Base64UnsafeToChar(combined >> 18), output);
            Base64AppendCharCode(Base64UnsafeToChar((combined >> 12) & 63), output);
            Base64AppendCharCode(Base64UnsafeToChar((combined >> 6) & 63), output);
            Base64AppendCharCode(Base64UnsafeToChar(combined & 63), output);
        }

        var rest = bytes.Length - index;

        if (rest is 2)
        {
            var combined = (bytes[index] << 16) | (bytes[index + 1] << 8);

            Base64AppendCharCode(Base64UnsafeToChar(combined >> 18), output);
            Base64AppendCharCode(Base64UnsafeToChar((combined >> 12) & 63), output);
            Base64AppendCharCode(Base64UnsafeToChar((combined >> 6) & 63), output);
            Base64AppendCharCode('=', output);
        }
        else if (rest is 1)
        {
            var combined = bytes[index] << 16;

            Base64AppendCharCode(Base64UnsafeToChar(combined >> 18), output);
            Base64AppendCharCode(Base64UnsafeToChar((combined >> 12) & 63), output);
            Base64AppendCharCode('=', output);
            Base64AppendCharCode('=', output);
        }

        return
            PineValueInProcess.CreateTagged(
                s_maybeJustTagNameValue,
                [MakeElmString(output.ToArray())]);
    }
}
