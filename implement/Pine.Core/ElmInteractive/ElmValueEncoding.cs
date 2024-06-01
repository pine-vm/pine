using Pine;
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
        if (pineValue == PineVMValues.TrueValue)
            return ElmValue.TrueValue;

        if (pineValue == PineVMValues.FalseValue)
            return ElmValue.FalseValue;

        return pineValue switch
        {
            PineValue.BlobValue blobValue =>
            blobValue.Bytes.Length is 0
            ?
            new ElmValue.ElmInternal("empty-blob")
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
                .Map(bigInt => (ElmValue)new ElmValue.ElmChar((int)bigInt))
            }),

            PineValue.ListValue list =>
            list.Elements.Select(PineValueAsElmValue).ListCombine()
            .MapError(error => "Failed to combine list: " + error)
            .AndThen(listValues =>
            {
                Result<string, ElmValue> resultAsList() =>
                new ElmValue.ElmList(listValues);

                if (listValues.Count is 0)
                    return resultAsList();

                if (listValues.Count is 2)
                {
                    var tagNameChars = listValues[0];
                    var tagArguments = listValues[1];

                    if (tagNameChars is ElmValue.ElmList tagNameCharsList && tagArguments is ElmValue.ElmList tagArgumentsList)
                    {
                        var maybeTagName = ElmValue.TryMapElmValueToString(tagNameCharsList);

                        if (maybeTagName is Maybe<string>.Just tagName && 0 < tagName.Value.Length)
                        {
                            if (char.IsUpper(tagName.Value[0]))
                            {
                                if (tagName.Value is ElmValue.ElmRecordTypeTagName)
                                {
                                    if (tagArgumentsList.Elements.Count is 1)
                                    {
                                        var recordValue = tagArgumentsList.Elements[0];

                                        return
                                        ElmValue.ElmValueAsElmRecord(recordValue)
                                        .MapError(error => "Failed to extract value under record tag: " + error)
                                        .Map(r => (ElmValue)r);
                                    }

                                    return Result<string, ElmValue>.err(
                                        "Wrong number of tag arguments: " + tagArgumentsList.Elements.Count);
                                }

                                if (tagName.Value is "String")
                                {
                                    if (tagArgumentsList.Elements.Count is 1)
                                    {
                                        var charsList = tagArgumentsList.Elements[0];

                                        return
                                        (charsList switch
                                        {
                                            ElmValue.ElmList charsListAsList =>
                                            ElmValue.TryMapElmValueToString(charsListAsList)
                                            .Map(chars => Result<string, ElmValue>.ok(new ElmValue.ElmString(chars)))
                                            .WithDefault("Failed to map chars"),

                                            _ =>
                                            "Unexpected shape of tag arguments: " + charsList.GetType().Name
                                        })
                                        .MapError(error => "Failed to extract value under String tag: " + error);
                                    }

                                    return "Unexpected shape of tag arguments";
                                }

                                return new ElmValue.ElmTag(tagName.Value, tagArgumentsList.Elements);
                            }
                        }
                        else
                            return resultAsList();
                    }

                    return resultAsList();
                }


                return resultAsList();
            }),

            _ =>
            throw new NotImplementedException(
                "Not implemented for value type: " + pineValue.GetType().FullName)
        };
    }

    public static PineValue ElmValueAsPineValue(ElmValue elmValue)
    {
        return
            elmValue switch
            {
                ElmValue.ElmList elmList =>
                PineValue.List(elmList.Elements.Select(ElmValueAsPineValue).ToList()),

                ElmValue.ElmChar elmChar =>
                PineValueAsInteger.ValueFromUnsignedInteger(elmChar.Value)
                .Extract(err => throw new Exception(err)),

                ElmValue.ElmInteger elmInteger =>
                PineValueAsInteger.ValueFromSignedInteger((int)elmInteger.Value),

                ElmValue.ElmString elmString =>
                PineValue.List(
                    [PineValueAsString.ValueFromString("String"),
                        PineValue.List([PineValueAsString.ValueFromString(elmString.Value)])]),

                ElmValue.ElmTag elmTag =>
                PineValue.List(
                    [PineValueAsString.ValueFromString(elmTag.TagName),
                        PineValue.List(elmTag.Arguments.Select(ElmValueAsPineValue).ToList())]),

                ElmValue.ElmRecord elmRecord =>
                ElmRecordAsPineValue(
                    [.. elmRecord.Fields.Select(field => (field.FieldName, ElmValueAsPineValue(field.Value)))]),

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
