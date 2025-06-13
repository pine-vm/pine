using Pine.Core.PopularEncodings;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm;

public static class ElmValueEncoding
{
    public static Result<string, ElmValue> PineValueAsElmValue(
        PineValue pineValue,
        IReadOnlyDictionary<PineValue, ElmValue>? additionalReusableDecodings,
        Action<PineValue, ElmValue>? reportNewDecoding)
    {
        ArgumentNullException.ThrowIfNull(pineValue);

        if (pineValue == PineVMValues.TrueValue)
            return ElmValue.TrueValue;

        if (pineValue == PineVMValues.FalseValue)
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

    private static readonly ElmValue EmptyBlob = new ElmValue.ElmInternal("empty-blob");

    private static readonly ElmValue EmptyList = new ElmValue.ElmList([]);

    private static readonly IReadOnlyList<Result<string, ElmValue>> ReusedBlobSingle =
        [..Enumerable.Range(0, 0x1_00)
        .Select(b => PineBlobValueAsElmValue((PineValue.BlobValue)PineValue.Blob([(byte)b])))];

    private static readonly IReadOnlyList<Result<string, ElmValue>> ReusedBlobTuple =
        [..Enumerable.Range(0, 0x1_00_00)
        .Select(twoBytes =>
        PineBlobValueAsElmValue((PineValue.BlobValue)PineValue.Blob([(byte)(twoBytes >> 8), (byte)twoBytes])))];

    public static Result<string, ElmValue> PineBlobValueAsElmValue(
        PineValue.BlobValue blobValue)
    {
        if (blobValue.Bytes.Length is 0)
            return EmptyBlob;

        if (blobValue.Bytes.Length is 1 && ReusedBlobSingle is { } internedBlobSingle)
        {
            return internedBlobSingle[blobValue.Bytes.Span[0]];
        }

        if (blobValue.Bytes.Length is 2 && ReusedBlobTuple is { } internedBlobTuple)
        {
            return internedBlobTuple[blobValue.Bytes.Span[0] * 0x100 + blobValue.Bytes.Span[1]];
        }

        var firstByte = blobValue.Bytes.Span[0];

        if (firstByte is 2 || firstByte is 4)
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
                int asInt = (int)asBigInt;

                if (asInt is 0 || UnicodeUtility.IsValidUnicodeScalar(asInt))
                {
                    return ElmValue.CharInstance((int)asBigInt);
                }
            }
        }

        return new ElmValue.ElmInternal("___error_skipped_large_blob___");
    }

    public static Result<string, ElmValue> PineListValueAsElmValue(
        PineValue.ListValue listValue,
        IReadOnlyDictionary<PineValue, ElmValue>? additionalReusableDecodings,
        Action<PineValue, ElmValue>? reportNewDecoding)
    {
        if (listValue.Elements.Length is 0)
            return EmptyList;

        if (ReusedInstances.Instance.ElmValueDecoding?.TryGetValue(listValue, out var reused) ?? false)
            return reused;

        Result<string, ElmValue> decodeWithoutReport()
        {
            {
                if (listValue.Elements.Length is 2 &&
                    listValue.Elements.Span[1] is PineValue.ListValue tagArgumentsList)
                {
                    var tagCandidateValue = listValue.Elements.Span[0];

                    {
                        // Optimize, especially for the case of an Elm String.

                        if (tagCandidateValue == ElmValue.ElmStringTypeTagNameAsValue ||
                            tagCandidateValue == ElmValue.ElmStringTypeTagNameAsValue_2024)
                        {
                            if (tagArgumentsList.Elements.Length is not 1)
                                return "Failed to convert value under String tag: Expected a list of tag arguments with one element";

                            var charsList = tagArgumentsList.Elements.Span[0];

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

                        if (tagCandidateValue == ElmValue.ElmRecordTypeTagNameAsValue ||
                            tagCandidateValue == ElmValue.ElmRecordTypeTagNameAsValue_2024)
                        {
                            if (tagArgumentsList.Elements.Length is not 1)
                                return "Failed to convert value under Record tag: Expected a list of tag arguments with one element";

                            var recordValue = tagArgumentsList.Elements.Span[0];

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

                        if (tagCandidateValue == ElmValue.ElmBytesTypeTagNameAsValue ||
                            tagCandidateValue == ElmValue.ElmBytesTypeTagNameAsValue_2024)
                        {
                            if (tagArgumentsList.Elements.Length is not 1)
                                return "Failed to convert value under Bytes tag: Expected a list of tag arguments with single item";

                            if (tagArgumentsList.Elements.Span[0] is not PineValue.BlobValue blobValue)
                                return "Failed to convert value under Bytes tag: Expected blob value in tag argument";

                            return new ElmValue.ElmBytes(blobValue.Bytes);
                        }
                    }

                    {
                        // Optimize for the case of an Elm Float.

                        if (tagCandidateValue == ElmValue.ElmFloatTypeTagNameAsValue ||
                            tagCandidateValue == ElmValue.ElmFloatTypeTagNameAsValue_2024)
                        {
                            if (tagArgumentsList.Elements.Length is not 2)
                                return "Failed to convert value under Float tag: Expected a list of tag arguments with two elements";

                            var numeratorValue = tagArgumentsList.Elements.Span[0];

                            var denominatorValue = tagArgumentsList.Elements.Span[1];

                            if (IntegerEncoding.ParseSignedIntegerStrict(numeratorValue).IsOkOrNullable() is not { } numeratorOk)
                                return "Failed to parse numerator under Float tag";

                            if (IntegerEncoding.ParseSignedIntegerStrict(denominatorValue).IsOkOrNullable() is not { } denominatorOk)
                                return "Failed to parse denominator under Float tag";

                            return ElmValue.ElmFloat.Normalized(numeratorOk, denominatorOk);
                        }
                    }

                    {
                        if ((tagCandidateValue is PineValue.BlobValue tagBlobValue &&
                            tagBlobValue.Bytes.Length is not 0) ||
                            (tagCandidateValue is PineValue.ListValue tagListValue &&
                            tagListValue.Elements.Length is not 0 &&
                            tagListValue.Elements.Span[0] is not PineValue.ListValue))
                        {
                            // Optimize, especially for the case of an Elm Tag.

                            if (StringEncoding.StringFromValue(tagCandidateValue).IsOkOrNull() is { } tagName)
                            {
                                if (tagName.Length is not 0 && char.IsAsciiLetterUpper(tagName[0]))
                                {
                                    bool tagNameContainsInvalidChar = false;

                                    for (var charIndex = 1; charIndex < tagName.Length; charIndex++)
                                    {
                                        if (!char.IsLetterOrDigit(tagName[charIndex]) && tagName[charIndex] is not '_')
                                        {
                                            tagNameContainsInvalidChar = true;
                                            break;
                                        }
                                    }

                                    if (!tagNameContainsInvalidChar)
                                    {
                                        var tagArgumentsListResults = new ElmValue[tagArgumentsList.Elements.Length];

                                        bool failedTagArguments = false;

                                        for (var argIndex = 0; argIndex < tagArgumentsList.Elements.Length; argIndex++)
                                        {
                                            var tagArgument = tagArgumentsList.Elements.Span[argIndex];

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
            }

            var itemsAsElmValues = new ElmValue[listValue.Elements.Length];

            for (var i = 0; i < listValue.Elements.Length; i++)
            {
                var item = listValue.Elements.Span[i];

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
                return EmptyList;

            return new ElmValue.ElmList(itemsAsElmValues);
        }

        var decodeResult = decodeWithoutReport();

        if (decodeResult.IsOkOrNull() is { } decodeOk)
        {
            reportNewDecoding?.Invoke(listValue, decodeOk);
        }

        return decodeResult;
    }

    public static Result<string, (string tagName, ReadOnlyMemory<PineValue> tagArguments)> ParseAsTag(
        PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue list)
            return "Value is not a list.";

        if (list.Elements.Length is not 2)
            return "List does not have 2 elements.";

        var tagNameValue = list.Elements.Span[0];

        var parseTagNameResult = StringEncoding.StringFromValue(tagNameValue);

        if (parseTagNameResult.IsErrOrNull() is { } tagNameErr)
            return "First element is not a string: " + tagNameErr;

        if (parseTagNameResult.IsOkOrNull() is not { } tagNameOk)
            throw new NotImplementedException(
                "Unexpected result type: " + parseTagNameResult.GetType().FullName);

        var tagArgumentsValue = list.Elements.Span[1];

        if (tagArgumentsValue is not PineValue.ListValue tagArgumentsList)
            return "Second element is not a list.";

        return (tagNameOk, tagArgumentsList.Elements);
    }


    private static Result<string, ElmValue.ElmRecord> PineValueAsElmRecord(
        PineValue pineValue,
        IReadOnlyDictionary<PineValue, ElmValue>? additionalReusableDecodings,
        Action<PineValue, ElmValue>? reportNewDecoding)
    {
        if (pineValue is not PineValue.ListValue list)
            return "Value is not a list.";

        var recordFields = new (string fieldName, ElmValue fieldValue)[list.Elements.Length];

        for (var i = 0; i < list.Elements.Length; i++)
        {
            var element = list.Elements.Span[i];

            if (element is not PineValue.ListValue fieldList)
                return "Field is not a list.";

            var fieldListElements = fieldList.Elements.Span;

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

    public static Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>> ParsePineValueAsRecordTagged(
        PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue taggedRecordList)
            return "Value is not a list.";

        var taggedRecordListElements = taggedRecordList.Elements.Span;

        if (taggedRecordListElements.Length is not 2)
            return "List does not have 2 elements.";

        var tagNameValue = taggedRecordListElements[0];

        if (tagNameValue != ElmValue.ElmRecordTypeTagNameAsValue &&
            tagNameValue != ElmValue.ElmRecordTypeTagNameAsValue_2024)
            return "First element is not the record tag name.";

        var recordFieldsListList = taggedRecordListElements[1];

        if (recordFieldsListList is not PineValue.ListValue recordFieldsList)
            return "Second element is not a list.";

        if (recordFieldsList.Elements.Length is not 1)
            return "Record fields list does not have 1 element.";

        return ParsePineValueAsRecord(recordFieldsList.Elements.Span[0]);
    }

    public static Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>> ParsePineValueAsRecord(
        PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue recordFieldsList)
            return "Value is not a list.";

        var recordFieldsListSpan = recordFieldsList.Elements.Span;

        var recordFields = new (string fieldName, PineValue fieldValue)[recordFieldsListSpan.Length];

        for (var i = 0; i < recordFieldsListSpan.Length; i++)
        {
            var element = recordFieldsListSpan[i];

            if (element is not PineValue.ListValue fieldList)
                return "Field is not a list.";

            if (fieldList.Elements.Length is not 2)
                return "Field list does not have 2 elements.";

            var fieldNameValue = fieldList.Elements.Span[0];
            var fieldValue = fieldList.Elements.Span[1];

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

    public static PineValue ElmValueAsPineValue(
        ElmValue elmValue) =>
        ElmValueAsPineValue(
            elmValue,
            additionalReusableEncodings: null,
            reportNewEncoding: null);

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
                PineValue.List(
                    [.. elmList.Elements
                    .Select(item => ElmValueAsPineValue(
                        item,
                        additionalReusableEncodings,
                        reportNewEncoding))]),

                ElmValue.ElmChar elmChar =>
                IntegerEncoding.EncodeUnsignedInteger(elmChar.Value)
                .Extract(err => throw new Exception(err)),

                ElmValue.ElmInteger elmInteger =>
                IntegerEncoding.EncodeSignedInteger(elmInteger.Value),

                ElmValue.ElmString elmString =>
                StringAsPineValue(elmString.Value),

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
                    [.. elmRecord.Fields
                    .OrderBy(field => field.FieldName)
                    .Select(field =>
                    (field.FieldName, ElmValueAsPineValue(
                        field.Value,
                        additionalReusableEncodings,
                        reportNewEncoding)))]),

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

    public static PineValue ElmRecordAsPineValue(
        IReadOnlyList<(string FieldName, PineValue FieldValue)> fields) =>
        PineValue.List(
            [ElmValue.ElmRecordTypeTagNameAsValue,
            PineValue.List(
                [PineValue.List(
                    [.. fields.Select(field =>
                    PineValue.List(
                        [
                            StringEncoding.ValueFromString(field.FieldName),
                            field.FieldValue
                        ]))])])]);

    public static PineValue TagAsPineValue(string tagName, IReadOnlyList<PineValue> tagArguments) =>
        PineValue.List(
            [
                StringEncoding.ValueFromString(tagName),
                PineValue.List([..tagArguments])
            ]);

    public static PineValue StringAsPineValue(
        string elmString) =>
        PineValue.List(
            [
            ElmValue.ElmStringTypeTagNameAsValue,
            PineValue.List([StringEncoding.ValueFromString(elmString)])
            ]);

    /// <summary>
    /// Converts a sequence of bytes from .NET to the corresponding
    /// Elm `<see href="https://package.elm-lang.org/packages/elm/bytes/1.0.8/Bytes#Bytes">Bytes.Bytes</see>` value.
    /// </summary>
    public static PineValue AsElmBytesBytes(ReadOnlyMemory<byte> bytes) =>
        PineValue.List(
            [
                ElmValue.ElmBytesTypeTagNameAsValue,
                PineValue.List([PineValue.Blob(bytes)])
            ]);
}
