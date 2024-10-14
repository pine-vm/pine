using Pine.Core;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.ElmInteractive;

public static class ElmValueEncoding
{
    public static Result<string, ElmValue> PineValueAsElmValue(
        PineValue pineValue)
    {
        ArgumentNullException.ThrowIfNull(pineValue);

        if (pineValue == PineVMValues.TrueValue)
            return ElmValue.TrueValue;

        if (pineValue == PineVMValues.FalseValue)
            return ElmValue.FalseValue;

        if (pineValue is PineValue.BlobValue blobValue)
        {
            return PineBlobValueAsElmValue(blobValue);
        }

        if (pineValue is PineValue.ListValue listValue)
        {
            return PineListValueAsElmValue(listValue);
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
        PineValue.BlobValue blobValue) =>
        blobValue.Bytes.Length is 0
        ?
        EmptyBlob
        :
        blobValue.Bytes.Length is 1 && ReusedBlobSingle is { } internedBlobSingle ?
        internedBlobSingle[blobValue.Bytes.Span[0]]
        :
        blobValue.Bytes.Length is 2 && ReusedBlobTuple is { } internedBlobTuple ?
        internedBlobTuple[blobValue.Bytes.Span[0] * 0x100 + blobValue.Bytes.Span[1]]
        :
        (blobValue.Bytes.Span[0] switch
        {
            4 or 2 =>
            PineValueAsInteger.SignedIntegerFromValueRelaxed(blobValue)
            .Map(bigInt => (ElmValue)new ElmValue.ElmInteger(bigInt)),

            _ =>
            blobValue.Bytes.Length > 10
            ?
            /*
            PineVM.ParseExpressionFromValueDefault(pineValue)
            .Map(_ => (ElmValue)new ElmValue.ElmInternal("expression"))
            .WithDefault(new ElmValue.ElmInternal("___error_skipped_large_blob___"))
            */
            new ElmValue.ElmInternal("___error_skipped_large_blob___")
            :
            PineValueAsInteger.UnsignedIntegerFromValue(blobValue)
            .Map(bigInt => ElmValue.CharInstance((int)bigInt))
        });

    public static Result<string, ElmValue> PineListValueAsElmValue(PineValue.ListValue listValue)
    {
        if (listValue.Elements.Count is 0)
            return EmptyList;

        if (ReusedInstances.Instance.ElmValueDecoding?.TryGetValue(listValue, out var reused) ?? false)
            return reused;

        {
            if (listValue.Elements.Count is 2)
            {
                {
                    // Optimize, especially for the case of an Elm String.

                    if (listValue.Elements[0] == ElmValue.ElmStringTypeTagNameAsValue)
                    {
                        var tagArgumentList = listValue.Elements[1];

                        if (tagArgumentList is not PineValue.ListValue tagArgumentsList)
                            return "Failed to convert value under String tag: Expected a list of tag arguments";

                        if (tagArgumentsList.Elements.Count is not 1)
                            return "Failed to convert value under String tag: Expected a list of tag arguments with one element";

                        var charsList = tagArgumentsList.Elements[0];

                        var mapToStringResult = PineValueAsString.StringFromValue(charsList);

                        if (mapToStringResult is Result<string, string>.Ok ok)
                            return ElmValue.StringInstance(ok.Value);

                        if (mapToStringResult is Result<string, string>.Err err)
                            return "Failed to convert value under String tag: Failed mapping char " + err.Value;

                        throw new NotImplementedException("Unexpected result type: " + mapToStringResult.GetType().FullName);
                    }
                }

                {
                    // Optimize, especially for the case of an Elm Record.

                    if (listValue.Elements[0] == ElmValue.ElmRecordTypeTagNameAsValue)
                    {
                        var tagArgumentList = listValue.Elements[1];

                        if (tagArgumentList is not PineValue.ListValue tagArgumentsList)
                            return "Failed to convert value under Record tag: Expected a list of tag arguments";

                        if (tagArgumentsList.Elements.Count is not 1)
                            return "Failed to convert value under Record tag: Expected a list of tag arguments with one element";

                        var recordValue = tagArgumentsList.Elements[0];

                        var asRecordResult = PineValueAsElmRecord(recordValue);

                        if (asRecordResult is Result<string, ElmValue.ElmRecord>.Ok ok)
                            return ok.Value;

                        if (asRecordResult is Result<string, ElmValue.ElmRecord>.Err err)
                            return "Failed to convert value under Record tag: " + err.Value;

                        throw new NotImplementedException("Unexpected result type: " + asRecordResult.GetType().FullName);
                    }
                }

                {
                    // case of Bytes.Bytes

                    if (listValue.Elements[0] == ElmValue.ElmBytesTypeTagNameAsValue)
                    {
                        var tagArgumentsValue = listValue.Elements[1];

                        if (tagArgumentsValue is not PineValue.ListValue tagArgumentsList)
                            return "Failed to convert value under Bytes tag: Expected a list of tag arguments";

                        if (tagArgumentsList.Elements.Count is not 1)
                            return "Failed to convert value under Bytes tag: Expected a list of tag arguments with single item";

                        if (tagArgumentsList.Elements[0] is not PineValue.BlobValue blobValue)
                            return "Failed to convert value under Bytes tag: Expected blob value in tag argument";

                        return new ElmValue.ElmBytes(blobValue.Bytes);
                    }
                }

                {
                    // Optimize for the case of an Elm Float.

                    if (listValue.Elements[0] == ElmValue.ElmFloatTypeTagNameAsValue)
                    {
                        var tagArgumentList = listValue.Elements[1];

                        if (tagArgumentList is not PineValue.ListValue tagArgumentsList)
                            return "Failed to convert value under Float tag: Expected a list of tag arguments";

                        if (tagArgumentsList.Elements.Count is not 2)
                            return "Failed to convert value under Float tag: Expected a list of tag arguments with two elements";

                        var numeratorValue = tagArgumentsList.Elements[0];

                        var denominatorValue = tagArgumentsList.Elements[1];

                        if (PineValueAsInteger.SignedIntegerFromValueStrict(numeratorValue) is not Result<string, System.Numerics.BigInteger>.Ok numeratorOk)
                            return "Failed to parse numerator under Float tag";

                        if (PineValueAsInteger.SignedIntegerFromValueStrict(denominatorValue) is not Result<string, System.Numerics.BigInteger>.Ok denominatorOk)
                            return "Failed to parse denominator under Float tag";

                        return ElmValue.ElmFloat.Normalized(numeratorOk.Value, denominatorOk.Value);
                    }
                }

                {
                    // Optimize, especially for the case of an Elm Tag.

                    if (listValue.Elements[0] is PineValue.ListValue tagNameValue &&
                        listValue.Elements[1] is PineValue.ListValue tagArgumentsList)
                    {
                        // Rule out any characters that might encode an integer.

                        bool tagNameItemMightBeInteger = false;

                        for (var i = 0; i < tagNameValue.Elements.Count; i++)
                        {
                            var tagNameChar = tagNameValue.Elements[i];

                            if (tagNameChar is not PineValue.BlobValue blobValue ||
                                (blobValue.Bytes.Length is > 1 &&
                                blobValue.Bytes.Span[0] is 2 or 4))
                            {
                                tagNameItemMightBeInteger = true;
                                break;
                            }
                        }

                        if (!tagNameItemMightBeInteger)
                        {
                            var asStringResult = PineValueAsString.StringFromListValue(tagNameValue);

                            if (asStringResult is Result<string, string>.Ok ok)
                            {
                                var tagName = ok.Value;

                                if (tagName.Length is not 0 && char.IsUpper(tagName[0]))
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
                                        var tagArgumentsListResults = new ElmValue[tagArgumentsList.Elements.Count];

                                        bool failedTagArguments = false;

                                        for (var argIndex = 0; argIndex < tagArgumentsList.Elements.Count; argIndex++)
                                        {
                                            var tagArgument = tagArgumentsList.Elements[argIndex];

                                            var tagArgumentAsElmValueResult = PineValueAsElmValue(tagArgument);

                                            if (tagArgumentAsElmValueResult is Result<string, ElmValue>.Ok argOk)
                                            {
                                                tagArgumentsListResults[argIndex] = argOk.Value;
                                                continue;
                                            }

                                            failedTagArguments = true;
                                            break;
                                        }

                                        if (!failedTagArguments)
                                            return ElmValue.TagInstance(tagName, tagArgumentsListResults);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        var itemsAsElmValues = new ElmValue[listValue.Elements.Count];

        for (var i = 0; i < listValue.Elements.Count; i++)
        {
            var item = listValue.Elements[i];
            var itemAsElmValueResult = PineValueAsElmValue(item);

            if (itemAsElmValueResult is Result<string, ElmValue>.Ok ok)
            {
                itemsAsElmValues[i] = ok.Value;
                continue;
            }

            if (itemAsElmValueResult is Result<string, ElmValue>.Err err)
                return "Failed to convert list item [" + i + "]: " + err.Value;

            throw new NotImplementedException(
                "Unexpected result type: " + itemAsElmValueResult.GetType().FullName);
        }

        if (itemsAsElmValues.Length is 0)
            return EmptyList;

        return new ElmValue.ElmList(itemsAsElmValues);
    }

    public static Result<string, (string tagName, IReadOnlyList<PineValue> tagArguments)> ParseAsTag(
        PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue list)
            return "Value is not a list.";

        if (list.Elements.Count is not 2)
            return "List does not have 2 elements.";

        var tagNameValue = list.Elements[0];

        var parseTagNameResult = PineValueAsString.StringFromValue(tagNameValue);

        if (parseTagNameResult is Result<string, string>.Err tagNameErr)
            return "First element is not a string: " + tagNameErr.Value;

        if (parseTagNameResult is not Result<string, string>.Ok tagNameOk)
            throw new NotImplementedException(
                "Unexpected result type: " + parseTagNameResult.GetType().FullName);

        var tagArgumentsValue = list.Elements[1];

        if (tagArgumentsValue is not PineValue.ListValue tagArgumentsList)
            return "Second element is not a list.";

        return (tagNameOk.Value, tagArgumentsList.Elements);
    }


    public static Result<string, ElmValue.ElmRecord> PineValueAsElmRecord(PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue list)
            return "Value is not a list.";

        var recordFields = new (string fieldName, ElmValue fieldValue)[list.Elements.Count];

        for (var i = 0; i < list.Elements.Count; i++)
        {
            var element = list.Elements[i];

            if (element is not PineValue.ListValue fieldList)
                return "Field is not a list.";

            if (fieldList.Elements.Count is not 2)
                return "Field list does not have 2 elements.";

            var fieldNameValue = fieldList.Elements[0];
            var fieldValue = fieldList.Elements[1];

            var fieldNameResult = PineValueAsString.StringFromValue(fieldNameValue);

            if (fieldNameResult is Result<string, string>.Ok fieldName)
            {
                var fieldValueResult = PineValueAsElmValue(fieldValue);

                if (fieldValueResult is Result<string, ElmValue>.Ok fieldValueOk)
                {
                    recordFields[i] = (fieldName.Value, fieldValueOk.Value);
                    continue;
                }

                return "Failed decoding field value: " + fieldValueResult;
            }

            if (fieldNameResult is Result<string, string>.Err error)
                return "Failed decoding field name: " + error.Value;

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

        if (taggedRecordList.Elements.Count is not 2)
            return "List does not have 2 elements.";

        var tagNameValue = taggedRecordList.Elements[0];

        if (tagNameValue != ElmValue.ElmRecordTypeTagNameAsValue)
            return "First element is not the record tag name.";

        var recordFieldsListList = taggedRecordList.Elements[1];

        if (recordFieldsListList is not PineValue.ListValue recordFieldsList)
            return "Second element is not a list.";

        if (recordFieldsList.Elements.Count is not 1)
            return "Record fields list does not have 1 element.";

        return ParsePineValueAsRecord(recordFieldsList.Elements[0]);
    }

    public static Result<string, IReadOnlyList<(string fieldName, PineValue fieldValue)>> ParsePineValueAsRecord(
        PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue recordFieldsList)
            return "Value is not a list.";

        var recordFields = new (string fieldName, PineValue fieldValue)[recordFieldsList.Elements.Count];

        for (var i = 0; i < recordFieldsList.Elements.Count; i++)
        {
            var element = recordFieldsList.Elements[i];

            if (element is not PineValue.ListValue fieldList)
                return "Field is not a list.";

            if (fieldList.Elements.Count is not 2)
                return "Field list does not have 2 elements.";

            var fieldNameValue = fieldList.Elements[0];
            var fieldValue = fieldList.Elements[1];

            var fieldNameResult = PineValueAsString.StringFromValue(fieldNameValue);

            if (fieldNameResult is Result<string, string>.Ok fieldName)
            {
                recordFields[i] = (fieldName.Value, fieldValue);
                continue;
            }

            if (fieldNameResult is Result<string, string>.Err error)
                return "Failed decoding field name: " + error.Value;

            throw new NotImplementedException(
                "Unexpected result type: " + fieldNameResult.GetType().FullName);
        }

        return recordFields;
    }

    public static PineValue ElmValueAsPineValue(ElmValue elmValue) =>
        ElmValueAsPineValue(
            elmValue,
            ReusedInstances.Instance.ElmValueEncoding);

    public static PineValue ElmValueAsPineValue(
        ElmValue elmValue,
        IReadOnlyDictionary<ElmValue, PineValue>? reusableEncoding)
    {
        if (reusableEncoding?.TryGetValue(elmValue, out var reused) ?? false)
            return reused;

        return
            elmValue switch
            {
                ElmValue.ElmList elmList =>
                PineValue.List(
                    [.. elmList.Elements.Select(item => ElmValueAsPineValue(item, reusableEncoding))]),

                ElmValue.ElmChar elmChar =>
                PineValueAsInteger.ValueFromUnsignedInteger(elmChar.Value)
                .Extract(err => throw new Exception(err)),

                ElmValue.ElmInteger elmInteger =>
                PineValueAsInteger.ValueFromSignedInteger((int)elmInteger.Value),

                ElmValue.ElmString elmString =>
                PineValue.List(
                    [ElmValue.ElmStringTypeTagNameAsValue,
                        PineValue.List([PineValueAsString.ValueFromString(elmString.Value)])]),

                ElmValue.ElmTag elmTag =>
                PineValue.List(
                    [PineValueAsString.ValueFromString(elmTag.TagName),
                        PineValue.List(
                            [..elmTag.Arguments.Select(item => ElmValueAsPineValue(item,reusableEncoding))])]),

                ElmValue.ElmRecord elmRecord =>
                ElmRecordAsPineValue(
                    [.. elmRecord.Fields.Select(field => (field.FieldName, ElmValueAsPineValue(field.Value, reusableEncoding)))]),

                ElmValue.ElmBytes elmBytes =>
                PineValue.List(
                    [ElmValue.ElmBytesTypeTagNameAsValue,
                    PineValue.List([PineValue.Blob(elmBytes.Value)])]),

                ElmValue.ElmFloat elmFloat =>
                PineValue.List(
                    [ElmValue.ElmFloatTypeTagNameAsValue,
                    PineValue.List(
                        [
                        PineValueAsInteger.ValueFromSignedInteger(elmFloat.Numerator),
                        PineValueAsInteger.ValueFromSignedInteger(elmFloat.Denominator)
                        ])]),

                _ =>
                throw new NotImplementedException(
                    "Not implemented for value type: " + elmValue.GetType().FullName)
            };
    }

    public static PineValue ElmRecordAsPineValue(IReadOnlyList<(string FieldName, PineValue FieldValue)> fields) =>
        PineValue.List(
            [ElmValue.ElmRecordTypeTagNameAsValue,
            PineValue.List(
                [PineValue.List(
                    [.. fields.Select(field =>
                    PineValue.List(
                        [PineValueAsString.ValueFromString(field.FieldName),
                        field.FieldValue]))])])]);
}
