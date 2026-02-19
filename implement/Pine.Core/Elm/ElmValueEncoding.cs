using Pine.Core.CommonEncodings;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm;

/// <summary>
/// Encoding and decoding helpers between <see cref="PineValue"/> and the Elm-facing <see cref="ElmValue"/> model.
/// </summary>
/// <remarks>
/// Provides bidirectional conversions and utilities used by Elm interop, including fast paths for common Elm types
/// like String, Record, Bytes.Bytes, and Float. Most conversion APIs return a <see cref="Result{ErrT, OkT}"/> where the
/// error is a descriptive message (<see cref="string"/>) and the success contains the converted value.
/// </remarks>
public static class ElmValueEncoding
{
    /// <summary>
    /// Converts a <see cref="PineValue"/> into an <see cref="ElmValue"/>.
    /// </summary>
    /// <param name="pineValue">The source Pine value.</param>
    /// <param name="additionalReusableDecodings">Optional cache of precomputed decodings to reuse.</param>
    /// <param name="reportNewDecoding">Optional callback invoked when a new successful decoding is produced.</param>
    /// <returns>
    /// <see cref="Result{ErrT, OkT}"/> containing the decoded <see cref="ElmValue"/> on success, or an error message on failure.
    /// </returns>
    public static Result<string, ElmValue> PineValueAsElmValue(
        PineValue pineValue,
        IReadOnlyDictionary<PineValue, ElmValue>? additionalReusableDecodings,
        Action<PineValue, ElmValue>? reportNewDecoding)
    {
        ArgumentNullException.ThrowIfNull(pineValue);

        if (pineValue == PineKernelValues.TrueValue)
            return ElmValue.TrueValue;

        if (pineValue == PineKernelValues.FalseValue)
            return ElmValue.FalseValue;

        if (additionalReusableDecodings?.TryGetValue(pineValue, out var reused) ?? false)
            return reused;

        if (pineValue is PineValue.BlobValue blobValue)
        {
            return PineBlobValueAsElmValue(blobValue);
        }

        if (pineValue is PineValue.ListValue listValue)
        {
            return PineListValueAsElmValue(
                listValue,
                additionalReusableDecodings,
                reportNewDecoding);
        }

        throw new NotImplementedException(
            "Not implemented for value type: " + pineValue.GetType().FullName);
    }

    private static readonly ElmValue s_emptyBlob = new ElmValue.ElmInternal("empty-blob");

    private static readonly ElmValue s_emptyList = new ElmValue.ElmList([]);

    private static readonly IReadOnlyList<Result<string, ElmValue>> s_reusedBlobSingle =
        [..Enumerable.Range(0, 0x1_00)
        .Select(b => PineBlobValueAsElmValue((PineValue.BlobValue)PineValue.Blob([(byte)b])))];

    private static readonly IReadOnlyList<Result<string, ElmValue>> s_reusedBlobTuple =
        [..Enumerable.Range(0, 0x1_00_00)
        .Select(twoBytes =>
        PineBlobValueAsElmValue((PineValue.BlobValue)PineValue.Blob([(byte)(twoBytes >> 8), (byte)twoBytes])))];

    /// <summary>
    /// Converts a Pine blob into an <see cref="ElmValue"/>, recognizing small integers and characters when possible.
    /// </summary>
    /// <param name="blobValue">The blob value to convert.</param>
    /// <returns>
    /// <see cref="Result{ErrT, OkT}"/> with the resulting <see cref="ElmValue"/> on success; otherwise an error message.
    /// </returns>
    /// <remarks>
    /// Special cases:
    /// - Empty blob becomes an internal placeholder.
    /// - Length 1/2 blobs are interned for reuse.
    /// - Integer encodings (signed/unsigned) are detected.
    /// - 4-byte blobs may be mapped to a 'Char' value in Elm.
    /// Larger blobs are not expanded and yield a descriptive skip message.
    /// </remarks>
    public static Result<string, ElmValue> PineBlobValueAsElmValue(
        PineValue.BlobValue blobValue)
    {
        if (blobValue.Bytes.Length is 0)
            return s_emptyBlob;

        if (blobValue.Bytes.Length is 1 && s_reusedBlobSingle is { } internedBlobSingle)
        {
            return internedBlobSingle[blobValue.Bytes.Span[0]];
        }

        if (blobValue.Bytes.Length is 2 && s_reusedBlobTuple is { } internedBlobTuple)
        {
            return internedBlobTuple[blobValue.Bytes.Span[0] * 0x100 + blobValue.Bytes.Span[1]];
        }

        var firstByte = blobValue.Bytes.Span[0];

        if (firstByte is 2 or 4)
        {
            var asBigIntResult =
                IntegerEncoding.ParseSignedIntegerRelaxed(blobValue);

            if (asBigIntResult.IsOkOrNullable() is { } bigInt)
            {
                return ElmValue.Integer(bigInt);
            }

            if (asBigIntResult.IsErrOrNull() is { } err)
            {
                return "Failed to convert blob to integer: " + err;
            }

            throw new NotImplementedException(
                "Unexpected result type: " + asBigIntResult.GetType().FullName);
        }

        if (blobValue.Bytes.Length is 4)
        {
            var asInt = System.Buffers.Binary.BinaryPrimitives.ReadInt32BigEndian(blobValue.Bytes.Span);

            if (UnicodeUtility.IsValidUnicodeScalar(asInt))
            {
                return ElmValue.CharInstance(asInt);
            }
        }

        if (blobValue.Bytes.Length < 4)
        {
            if (IntegerEncoding.ParseUnsignedInteger(blobValue).IsOkOrNullable() is { } asBigInt)
            {
                var asInt = (int)asBigInt;

                if (asInt is 0 || UnicodeUtility.IsValidUnicodeScalar(asInt))
                {
                    return ElmValue.CharInstance((int)asBigInt);
                }
            }
        }

        return "skipped larger blob of length " + blobValue.Bytes.Length;
    }

    /// <summary>
    /// Converts a Pine list into an <see cref="ElmValue"/>, handling common Elm representations like String, Record, Bytes and Float.
    /// </summary>
    /// <param name="listValue">The list value to convert.</param>
    /// <param name="additionalReusableDecodings">Optional cache of precomputed decodings to reuse.</param>
    /// <param name="reportNewDecoding">Optional callback invoked when a new successful decoding is produced.</param>
    /// <returns>
    /// <see cref="Result{ErrT, OkT}"/> containing the converted <see cref="ElmValue"/> on success or an error message on failure.
    /// </returns>
    public static Result<string, ElmValue> PineListValueAsElmValue(
        PineValue.ListValue listValue,
        IReadOnlyDictionary<PineValue, ElmValue>? additionalReusableDecodings,
        Action<PineValue, ElmValue>? reportNewDecoding)
    {
        if (listValue.Items.Length is 0)
            return s_emptyList;

        if (ReusedInstances.Instance.ElmValueDecoding?.TryGetValue(listValue, out var reused) ?? false)
            return reused;

        Result<string, ElmValue> DecodeWithoutReport()
        {
            {
                if (listValue.Items.Length is 2 &&
                    listValue.Items.Span[1] is PineValue.ListValue tagArgumentsList)
                {
                    var tagCandidateValue = listValue.Items.Span[0];

                    {
                        // Optimize, especially for the case of an Elm String.

                        if (tagCandidateValue == ElmValue.ElmStringTypeTagNameAsValue)
                        {
                            if (tagArgumentsList.Items.Length is not 1)
                            {
                                return "Failed to convert value under String tag: Expected a list of tag arguments with one element";
                            }

                            var charsList = tagArgumentsList.Items.Span[0];

                            var mapToStringResult = StringEncoding.StringFromValue(charsList);

                            if (mapToStringResult.IsOkOrNull() is { } ok)
                                return ElmValue.StringInstance(ok);

                            if (mapToStringResult.IsErrOrNull() is { } err)
                                return "Failed to convert value under String tag: Failed mapping char " + err;

                            throw new NotImplementedException("Unexpected result type: " + mapToStringResult.GetType().FullName);
                        }
                    }

                    {
                        // Optimize, especially for the case of an Elm Record.

                        if (tagCandidateValue == ElmValue.ElmRecordTypeTagNameAsValue)
                        {
                            if (tagArgumentsList.Items.Length is not 1)
                            {
                                return "Failed to convert value under Record tag: Expected a list of tag arguments with one element";
                            }

                            var recordValue = tagArgumentsList.Items.Span[0];

                            var asRecordResult =
                                PineValueAsElmRecord(
                                    recordValue,
                                    additionalReusableDecodings,
                                    reportNewDecoding);

                            if (asRecordResult.IsOkOrNull() is { } ok)
                                return ok;

                            if (asRecordResult.IsErrOrNull() is { } err)
                                return "Failed to convert value under Record tag: " + err;

                            throw new NotImplementedException(
                                "Unexpected result type: " + asRecordResult.GetType().FullName);
                        }
                    }

                    {
                        // case of Bytes.Bytes

                        if (tagCandidateValue == ElmValue.ElmBytesTypeTagNameAsValue)
                        {
                            if (tagArgumentsList.Items.Length is not 1)
                            {
                                return "Failed to convert value under Bytes tag: Expected a list of tag arguments with single item";
                            }

                            if (tagArgumentsList.Items.Span[0] is not PineValue.BlobValue blobValue)
                                return "Failed to convert value under Bytes tag: Expected blob value in tag argument";

                            return new ElmValue.ElmBytes(blobValue.Bytes);
                        }
                    }

                    {
                        // Optimize for the case of an Elm Float.

                        if (tagCandidateValue == ElmValue.ElmFloatTypeTagNameAsValue)
                        {
                            if (tagArgumentsList.Items.Length is not 2)
                            {
                                return "Failed to convert value under Float tag: Expected a list of tag arguments with two elements";
                            }

                            var numeratorValue = tagArgumentsList.Items.Span[0];

                            var denominatorValue = tagArgumentsList.Items.Span[1];

                            if (IntegerEncoding.ParseSignedIntegerStrict(numeratorValue).IsOkOrNullable() is not { } numeratorOk)
                                return "Failed to parse numerator under Float tag";

                            if (IntegerEncoding.ParseSignedIntegerStrict(denominatorValue).IsOkOrNullable() is not { } denominatorOk)
                                return "Failed to parse denominator under Float tag";

                            return ElmValue.ElmFloat.Normalized(numeratorOk, denominatorOk);
                        }
                    }

                    {
                        if (tagCandidateValue is PineValue.BlobValue tagBlobValue &&
                            tagBlobValue.Bytes.Length is not 0)
                        {
                            // Optimize, especially for the case of an Elm Tag.

                            if (StringEncoding.StringFromValue(tagBlobValue).IsOkOrNull() is { } tagName)
                            {
                                if (StringIsValidTagName(tagName))
                                {
                                    var tagArgumentsListResults = new ElmValue[tagArgumentsList.Items.Length];

                                    var failedTagArguments = false;

                                    for (var argIndex = 0; argIndex < tagArgumentsList.Items.Length; argIndex++)
                                    {
                                        var tagArgument = tagArgumentsList.Items.Span[argIndex];

                                        var tagArgumentAsElmValueResult =
                                            PineValueAsElmValue(
                                                tagArgument,
                                                additionalReusableDecodings,
                                                reportNewDecoding);

                                        if (tagArgumentAsElmValueResult is Result<string, ElmValue>.Ok argOk)
                                        {
                                            tagArgumentsListResults[argIndex] = argOk.Value;
                                            continue;
                                        }

                                        failedTagArguments = true;
                                        break;
                                    }

                                    if (!failedTagArguments)
                                    {
                                        return ElmValue.TagInstance(tagName, tagArgumentsListResults);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            var itemsAsElmValues = new ElmValue[listValue.Items.Length];

            for (var i = 0; i < listValue.Items.Length; i++)
            {
                var item = listValue.Items.Span[i];

                var itemAsElmValueResult =
                    PineValueAsElmValue(
                        item,
                        additionalReusableDecodings,
                        reportNewDecoding);

                if (itemAsElmValueResult.IsOkOrNull() is { } ok)
                {
                    itemsAsElmValues[i] = ok;
                    continue;
                }

                if (itemAsElmValueResult.IsErrOrNull() is { } err)
                    return "Failed to convert list item [" + i + "]: " + err;

                throw new NotImplementedException(
                    "Unexpected result type: " + itemAsElmValueResult.GetType().FullName);
            }

            if (itemsAsElmValues.Length is 0)
                return s_emptyList;

            return new ElmValue.ElmList(itemsAsElmValues);
        }

        var decodeResult = DecodeWithoutReport();

        if (decodeResult.IsOkOrNull() is { } decodeOk)
        {
            reportNewDecoding?.Invoke(listValue, decodeOk);
        }

        return decodeResult;
    }

    /// <summary>
    /// Determines whether the specified string is a valid tag name according to naming rules.
    /// </summary>
    /// <remarks>A valid tag name must begin with an uppercase ASCII letter (A–Z). Subsequent characters may
    /// be letters, digits, or underscores. The method returns false for empty strings or if the first character is not
    /// an uppercase ASCII letter.</remarks>
    /// <param name="tagName">The string to validate as a tag name. The tag name must start with an uppercase ASCII letter and may contain
    /// only letters, digits, or underscores.</param>
    /// <returns>true if the specified string is a valid tag name; otherwise, false.</returns>
    public static bool StringIsValidTagName(string tagName)
    {
        if (tagName.Length is 0 || !char.IsAsciiLetterUpper(tagName[0]))
            return false;

        for (var charIndex = 1; charIndex < tagName.Length; charIndex++)
        {
            if (!char.IsLetterOrDigit(tagName[charIndex]) && tagName[charIndex] is not '_')
                return false;
        }

        return true;
    }

    /// <summary>
    /// Interprets a Pine list as a generic tag expression, returning the tag name and its arguments.
    /// </summary>
    /// <param name="pineValue">The Pine value expected to be a list of two elements: tag name and argument list.</param>
    /// <returns>
    /// <see cref="Result{ErrT, OkT}"/> with <c>(tagName, tagArguments)</c> on success; otherwise a descriptive error.
    /// </returns>
    public static Result<string, (string tagName, ReadOnlyMemory<PineValue> tagArguments)> ParseAsTag(
        PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue list)
            return "Value is not a list.";

        if (list.Items.Length is not 2)
            return "List does not have 2 elements.";

        var tagNameValue = list.Items.Span[0];

        var parseTagNameResult = StringEncoding.StringFromValue(tagNameValue);

        if (parseTagNameResult.IsErrOrNull() is { } tagNameErr)
            return "First element is not a string: " + tagNameErr;

        if (parseTagNameResult.IsOkOrNull() is not { } tagNameOk)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseTagNameResult.GetType().FullName);
        }

        var tagArgumentsValue = list.Items.Span[1];

        if (tagArgumentsValue is not PineValue.ListValue tagArgumentsList)
            return "Second element is not a list.";

        return (tagNameOk, tagArgumentsList.Items);
    }


    private static Result<string, ElmValue.ElmRecord> PineValueAsElmRecord(
        PineValue pineValue,
        IReadOnlyDictionary<PineValue, ElmValue>? additionalReusableDecodings,
        Action<PineValue, ElmValue>? reportNewDecoding)
    {
        if (pineValue is not PineValue.ListValue list)
            return "Value is not a list.";

        var recordFields = new (string fieldName, ElmValue fieldValue)[list.Items.Length];

        for (var i = 0; i < list.Items.Length; i++)
        {
            var element = list.Items.Span[i];

            if (element is not PineValue.ListValue fieldList)
                return "Field is not a list.";

            var fieldListElements = fieldList.Items.Span;

            if (fieldListElements.Length is not 2)
                return "Field list does not have 2 elements.";

            var fieldNameValue = fieldListElements[0];
            var fieldValue = fieldListElements[1];

            var fieldNameResult = StringEncoding.StringFromValue(fieldNameValue);

            if (fieldNameResult.IsOkOrNull() is { } fieldName)
            {
                var fieldValueResult =
                    PineValueAsElmValue(
                        fieldValue,
                        additionalReusableDecodings,
                        reportNewDecoding);

                if (fieldValueResult.IsOkOrNull() is { } fieldValueOk)
                {
                    recordFields[i] = (fieldName, fieldValueOk);
                    continue;
                }

                return "Failed decoding field value: " + fieldValueResult;
            }

            if (fieldNameResult.IsErrOrNull() is { } error)
                return "Failed decoding field name: " + error;

            throw new NotImplementedException(
                "Unexpected result type: " + fieldNameResult.GetType().FullName);
        }

        return new ElmValue.ElmRecord(recordFields);
    }

    /// <summary>
    /// Parses a Pine value expected to represent a record wrapped in the standard record tag.
    /// </summary>
    /// <param name="pineValue">The Pine value to parse. Expected structure: [Elm_Record, [ [ [fieldName, fieldValue], ... ] ]].</param>
    /// <returns>
    /// <see cref="Result{ErrT, OkT}"/> with the ordered list of fields on success; otherwise an error message.
    /// </returns>
    public static Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>> ParsePineValueAsRecordTagged(
        PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue taggedRecordList)
            return "Value is not a list.";

        var taggedRecordListElements = taggedRecordList.Items.Span;

        if (taggedRecordListElements.Length is not 2)
            return "List does not have 2 elements.";

        var tagNameValue = taggedRecordListElements[0];

        if (tagNameValue != ElmValue.ElmRecordTypeTagNameAsValue)
            return "First element is not the record tag name.";

        var recordFieldsListList = taggedRecordListElements[1];

        if (recordFieldsListList is not PineValue.ListValue recordFieldsList)
            return "Second element is not a list.";

        if (recordFieldsList.Items.Length is not 1)
            return "Record fields list does not have 1 element.";

        return ParsePineValueAsRecord(recordFieldsList.Items.Span[0]);
    }

    /// <summary>
    /// Parses a Pine list value representing record fields into a .NET list of (name, value) pairs.
    /// </summary>
    /// <param name="pineValue">The Pine value expected to be a list of two-element lists: [ [fieldName, fieldValue], ... ].</param>
    /// <returns>
    /// <see cref="Result{ErrT, OkT}"/> with the extracted field pairs on success; otherwise an error message.
    /// </returns>
    public static Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>> ParsePineValueAsRecord(
        PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue recordFieldsList)
            return "Value is not a list.";

        var recordFieldsListSpan = recordFieldsList.Items.Span;

        var recordFields = new (string fieldName, PineValue fieldValue)[recordFieldsListSpan.Length];

        for (var i = 0; i < recordFieldsListSpan.Length; i++)
        {
            var element = recordFieldsListSpan[i];

            if (element is not PineValue.ListValue fieldList)
                return "Field is not a list.";

            if (fieldList.Items.Length is not 2)
                return "Field list does not have 2 elements.";

            var fieldNameValue = fieldList.Items.Span[0];
            var fieldValue = fieldList.Items.Span[1];

            var fieldNameResult = StringEncoding.StringFromValue(fieldNameValue);

            if (fieldNameResult.IsOkOrNull() is { } fieldName)
            {
                recordFields[i] = (fieldName, fieldValue);
                continue;
            }

            if (fieldNameResult.IsErrOrNull() is { } error)
                return "Failed decoding field name: " + error;

            throw new NotImplementedException(
                "Unexpected result type: " + fieldNameResult.GetType().FullName);
        }

        return recordFields;
    }

    /// <summary>
    /// Converts an <see cref="ElmValue"/> to its <see cref="PineValue"/> representation using default options.
    /// </summary>
    /// <param name="elmValue">The Elm value to encode.</param>
    /// <returns>The encoded <see cref="PineValue"/>.</returns>
    public static PineValue ElmValueAsPineValue(
        ElmValue elmValue) =>
        ElmValueAsPineValue(
            elmValue,
            additionalReusableEncodings: null,
            reportNewEncoding: null);

    /// <summary>
    /// Converts an <see cref="ElmValue"/> to its <see cref="PineValue"/> representation.
    /// </summary>
    /// <param name="elmValue">The Elm value to encode.</param>
    /// <param name="additionalReusableEncodings">Optional cache of precomputed encodings to reuse.</param>
    /// <param name="reportNewEncoding">Optional callback invoked when a new encoding is produced.</param>
    /// <returns>The encoded <see cref="PineValue"/>.</returns>
    public static PineValue ElmValueAsPineValue(
        ElmValue elmValue,
        IReadOnlyDictionary<ElmValue, PineValue>? additionalReusableEncodings,
        Action<ElmValue, PineValue>? reportNewEncoding)
    {
        {
            if (additionalReusableEncodings?.TryGetValue(elmValue, out var reused) ?? false)
                return reused;
        }

        {
            if (ReusedInstances.Instance.ElmValueEncoding?.TryGetValue(elmValue, out var reused) ?? false)
                return reused;
        }

        var encoded =
            elmValue switch
            {
                ElmValue.ElmList elmList =>
                ElmListAsPineValue(elmList, additionalReusableEncodings, reportNewEncoding),

                ElmValue.ElmChar elmChar =>
                ElmCharAsPineValue(elmChar),

                ElmValue.ElmInteger elmInteger =>
                IntegerEncoding.EncodeSignedInteger(elmInteger.Value),

                ElmValue.ElmString elmString =>
                StringAsPineValue(elmString.Value),

                // Bool constructors are represented as Pine kernel booleans (blob [4] / blob [2]),
                // matching what the Elm compiler emits for True/False literals and what
                // kernel functions like int_is_sorted_asc return.
                ElmValue.ElmTag { TagName: "True", Arguments.Count: 0 } =>
                PineVM.PineKernelValues.TrueValue,

                ElmValue.ElmTag { TagName: "False", Arguments.Count: 0 } =>
                PineVM.PineKernelValues.FalseValue,

                ElmValue.ElmTag elmTag =>
                TagAsPineValue(
                    elmTag.TagName,
                    [..elmTag.Arguments
                    .Select(item => ElmValueAsPineValue(
                        item,
                        additionalReusableEncodings,
                        reportNewEncoding))]),

                ElmValue.ElmRecord elmRecord =>
                ElmRecordAsPineValue(
                    elmRecord.Fields,
                    additionalReusableEncodings,
                    reportNewEncoding),

                ElmValue.ElmBytes elmBytes =>
                PineValue.List(
                    [ElmValue.ElmBytesTypeTagNameAsValue,
                    PineValue.List([PineValue.Blob(elmBytes.Value)])]),

                ElmValue.ElmFloat elmFloat =>
                PineValue.List(
                    [ElmValue.ElmFloatTypeTagNameAsValue,
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(elmFloat.Numerator),
                        IntegerEncoding.EncodeSignedInteger(elmFloat.Denominator)
                        ])]),

                _ =>
                throw new NotImplementedException(
                    "Not implemented for value type: " + elmValue.GetType().FullName)
            };

        reportNewEncoding?.Invoke(elmValue, encoded);

        return encoded;
    }

    /// <summary>
    /// Encodes an Elm list value as a Pine list value.
    /// </summary>
    /// <param name="elmList">The Elm list to encode.</param>
    /// <param name="additionalReusableEncodings">Optional cache of precomputed encodings to reuse.</param>
    /// <param name="reportNewEncoding">Optional callback invoked when a new encoding is produced.</param>
    /// <returns>The encoded <see cref="PineValue"/> list.</returns>
    public static PineValue ElmListAsPineValue(
        ElmValue.ElmList elmList,
        IReadOnlyDictionary<ElmValue, PineValue>? additionalReusableEncodings,
        Action<ElmValue, PineValue>? reportNewEncoding)
    {
        if (elmList.Items.Count is 0)
        {
            return PineValue.EmptyList;
        }

        var items = elmList.Items;
        var encodedItems = new PineValue[items.Count];

        for (var i = 0; i < items.Count; i++)
        {
            encodedItems[i] = ElmValueAsPineValue(items[i], additionalReusableEncodings, reportNewEncoding);
        }

        return PineValue.List(encodedItems);
    }

    /// <summary>
    /// Encodes an <see cref="ElmValue.ElmChar"/> as a 4-byte big-endian <see cref="PineValue.BlobValue"/>.
    /// </summary>
    /// <param name="elmChar">The Elm character value (Unicode code point) to encode.</param>
    /// <returns>
    /// A <see cref="PineValue"/> blob containing the 4-byte big-endian representation of <paramref name="elmChar"/>.
    /// </returns>
    public static PineValue ElmCharAsPineValue(ElmValue.ElmChar elmChar)
    {
        return ElmCharAsPineValue(elmChar.Value);
    }

    /// <summary>
    /// Encodes an Elm char literal code as a 4-byte big-endian <see cref="PineValue.BlobValue"/>.
    /// </summary>
    public static PineValue ElmCharAsPineValue(int charCode)
    {
        var byte0 = (byte)(charCode >> 24);

        var byte1 = (byte)(charCode >> 16);

        var byte2 = (byte)(charCode >> 8);

        var byte3 = (byte)charCode;

        if (byte0 is 0 && byte1 is 0)
        {
            if (PineValue.ReusedBlobCharFourByte(third: byte2, fourth: byte3) is { } reused)
                return reused;
        }

        return PineValue.Blob([byte0, byte1, byte2, byte3]);
    }


    /// <summary>
    /// Encodes an Elm record as a <see cref="PineValue"/> using the standard record tag and field layout.
    /// </summary>
    /// <param name="fields">The record fields. Fields are ordered by name (ordinal) in the resulting value.</param>
    /// <param name="additionalReusableEncodings">Optional cache of precomputed encodings to reuse.</param>
    /// <param name="reportNewEncoding">Optional callback invoked when a new encoding is produced.</param>
    /// <returns>The encoded <see cref="PineValue"/> representing an Elm record.</returns>
    public static PineValue ElmRecordAsPineValue(
        IReadOnlyList<(string FieldName, ElmValue FieldValue)> fields,
        IReadOnlyDictionary<ElmValue, PineValue>? additionalReusableEncodings,
        Action<ElmValue, PineValue>? reportNewEncoding)
    {
        var orderedFields = fields.OrderBy(f => f.FieldName, StringComparer.Ordinal).ToArray();

        var fieldsValues = new PineValue[orderedFields.Length];

        for (var i = 0; i < orderedFields.Length; i++)
        {
            var field = orderedFields[i];

            fieldsValues[i] =
                PineValue.List(
                    [
                        StringEncoding.ValueFromString(field.FieldName),
                        ElmValueAsPineValue(
                            field.FieldValue,
                            additionalReusableEncodings,
                            reportNewEncoding)
                    ]);
        }

        return
            PineValue.List(
                [ElmValue.ElmRecordTypeTagNameAsValue,
                PineValue.List(
                    [PineValue.List(fieldsValues)])]);
    }

    /// <summary>
    /// Encodes an Elm record as a <see cref="PineValue"/> using the standard record tag and field layout.
    /// </summary>
    /// <param name="fields">The record fields. Fields are ordered by name (ordinal) in the resulting value.</param>
    /// <returns>The encoded <see cref="PineValue"/> representing an Elm record.</returns>
    public static PineValue ElmRecordAsPineValue(
        IReadOnlyList<(string FieldName, PineValue FieldValue)> fields)
    {
        var orderedFields = fields.OrderBy(f => f.FieldName, StringComparer.Ordinal).ToArray();

        var fieldsValues = new PineValue[orderedFields.Length];

        for (var i = 0; i < orderedFields.Length; i++)
        {
            var field = orderedFields[i];

            fieldsValues[i] =
                PineValue.List(
                    [
                        StringEncoding.ValueFromString(field.FieldName),
                        field.FieldValue
                    ]);
        }

        return
            PineValue.List(
                [ElmValue.ElmRecordTypeTagNameAsValue,
                PineValue.List(
                    [PineValue.List(fieldsValues)])]);
    }

    /// <summary>
    /// Encodes an Elm tag as a <see cref="PineValue"/> list of <c>[ tagName, arguments ]</c>.
    /// </summary>
    /// <param name="tagName">The tag name to encode.</param>
    /// <param name="tagArguments">The list of argument values.</param>
    /// <returns>The encoded <see cref="PineValue"/>.</returns>
    public static PineValue TagAsPineValue(string tagName, IReadOnlyList<PineValue> tagArguments) =>
        PineValue.List(
            [
                StringEncoding.ValueFromString(tagName),
                PineValue.List([..tagArguments])
            ]);

    /// <summary>
    /// Encodes an Elm string as a <see cref="PineValue"/> using the standard String tag representation.
    /// </summary>
    /// <param name="elmString">The string to encode.</param>
    /// <returns>The encoded <see cref="PineValue"/>.</returns>
    public static PineValue StringAsPineValue(
        string elmString) =>
        PineValue.List(
            [
            ElmValue.ElmStringTypeTagNameAsValue,
            PineValue.List([StringEncoding.ValueFromString(elmString)])
            ]);

    /// <summary>
    /// Converts a sequence of bytes from .NET to the corresponding
    /// Elm <see href="https://package.elm-lang.org/packages/elm/bytes/1.0.8/Bytes#Bytes">Bytes.Bytes</see> value.
    /// </summary>
    /// <param name="bytes">The raw bytes to wrap in an Elm Bytes value.</param>
    /// <returns>The encoded <see cref="PineValue"/> representing an Elm Bytes.Bytes instance.</returns>
    public static PineValue AsElmBytesBytes(ReadOnlyMemory<byte> bytes) =>
        PineValue.List(
            [
                ElmValue.ElmBytesTypeTagNameAsValue,
                PineValue.List([PineValue.Blob(bytes)])
            ]);
}
