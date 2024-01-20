using Pine;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;

namespace ElmTime.ElmInteractive;

/// <summary>
/// Corresponding to the type ElmValue in ElmInteractive.elm
/// </summary>
public abstract record ElmValue
{
    /*

    type ElmValue
        = ElmList (List ElmValue)
        | ElmChar Char
        | ElmInteger BigInt.BigInt
        | ElmString String
        | ElmTag String (List ElmValue)
        | ElmRecord (List ( String, ElmValue ))
        | ElmInternal String

     * */

    public static readonly ElmValue TrueValue = new ElmTag("True", []);

    public static readonly ElmValue FalseValue = new ElmTag("False", []);

    public record ElmInteger(System.Numerics.BigInteger Value)
        : ElmValue;

    public record ElmTag(string TagName, IReadOnlyList<ElmValue> Arguments)
        : ElmValue;

    public record ElmList(IReadOnlyList<ElmValue> Elements)
        : ElmValue
    {
        public virtual bool Equals(ElmList? otherList)
        {
            if (otherList is null)
                return false;

            if (Elements.Count != otherList.Elements.Count)
                return false;

            for (int i = 0; i < Elements.Count; i++)
            {
                if (!Elements[i].Equals(otherList.Elements[i]))
                    return false;
            }

            return true;
        }

        public override int GetHashCode() =>
            Elements.Select(e => e.GetHashCode()).Aggregate((a, b) => a ^ b);
    }

    public record ElmString(string Value)
        : ElmValue;

    public record ElmChar(char Value)
        : ElmValue;

    public record ElmRecord(IReadOnlyList<(string FieldName, ElmValue Value)> Fields)
        : ElmValue
    {
        public virtual bool Equals(ElmRecord? otherRecord)
        {
            if (otherRecord is null)
                return false;

            if (Fields.Count != otherRecord.Fields.Count)
                return false;

            for (int i = 0; i < Fields.Count; i++)
            {
                if (Fields[i].FieldName != otherRecord.Fields[i].FieldName)
                    return false;

                if (!Fields[i].Value.Equals(otherRecord.Fields[i].Value))
                    return false;
            }

            return true;
        }

        public override int GetHashCode() =>
            Fields.Select(f => f.GetHashCode()).Aggregate((a, b) => a ^ b);
    }

    public record ElmInternal(string Value)
        : ElmValue;

    public static Result<string, ElmValue> PineValueAsElmValue(
        PineValue pineValue)
    {
        if (pineValue == PineVM.TrueValue)
            return Result<string, ElmValue>.ok(TrueValue);

        if (pineValue == PineVM.FalseValue)
            return Result<string, ElmValue>.ok(FalseValue);

        return pineValue switch
        {
            PineValue.BlobValue blobValue =>
            blobValue.Bytes.Length == 0
            ?
            Result<string, ElmValue>.ok(new ElmInternal("empty-blob"))
            :
            (blobValue.Bytes.Span[0] switch
            {
                4 or 2 =>
                PineValueAsInteger.SignedIntegerFromValue(blobValue)
                .Map(bigInt => (ElmValue)new ElmInteger(bigInt)),

                _ =>
                blobValue.Bytes.Length > 10
                ?
                Result<string, ElmValue>.ok(
                    PineVM.DecodeExpressionFromValueDefault(pineValue)
                    .Map(_ => (ElmValue)new ElmInternal("expression"))
                    .WithDefault(new ElmInternal("___error_skipped_large_blob___")))
                :
                PineValueAsInteger.UnsignedIntegerFromValue(blobValue)
                .Map(bigInt => (ElmValue)new ElmChar((char)bigInt))
            }),

            PineValue.ListValue list =>
            list.Elements.Select(PineValueAsElmValue).ListCombine()
            .MapError(error => "Failed to combine list: " + error)
            .AndThen(listValues =>
            {
                Result<string, ElmValue> resultAsList() =>
                Result<string, ElmValue>.ok(new ElmList(listValues));

                if (listValues.Count == 0)
                    return resultAsList();

                if (listValues.Count == 2)
                {
                    var tagNameChars = listValues[0];
                    var tagArguments = listValues[1];

                    if (tagNameChars is ElmList tagNameCharsList && tagArguments is ElmList tagArgumentsList)
                    {
                        var maybeTagName = TryMapElmValueToString(tagNameCharsList);

                        if (maybeTagName is Maybe<string>.Just tagName)
                        {
                            if (0 < tagName.Value.Length && char.IsUpper(tagName.Value[0]))
                            {
                                if (tagName.Value == "Record")
                                {
                                    if (tagArgumentsList.Elements.Count == 1)
                                    {
                                        var recordValue = tagArgumentsList.Elements[0];

                                        return
                                        ElmValueAsElmRecord(recordValue)
                                        .MapError(error => "Failed to extract value under record tag: " + error)
                                        .Map(r => (ElmValue)r);
                                    }

                                    return Result<string, ElmValue>.err(
                                        "Wrong number of tag arguments: " + tagArgumentsList.Elements.Count);
                                }

                                if (tagName.Value == "String")
                                {
                                    if (tagArgumentsList.Elements.Count == 1)
                                    {
                                        var charsList = tagArgumentsList.Elements[0];

                                        return
                                        (charsList switch
                                        {
                                            ElmList charsListAsList =>
                                            TryMapElmValueToString(charsListAsList)
                                            .Map(chars => Result<string, ElmValue>.ok(new ElmString(chars)))
                                            .WithDefault(Result<string, ElmValue>.err("Failed to map chars")),

                                            _ =>
                                            Result<string, ElmValue>.err(
                                                "Unexpected shape of tag arguments: " + charsList.GetType().Name)
                                        })
                                        .MapError(error => "Failed to extract value under String tag: " + error);
                                    }

                                    return Result<string, ElmValue>.err("Unexpected shape of tag arguments");
                                }

                                return Result<string, ElmValue>.ok(
                                    new ElmTag(tagName.Value, tagArgumentsList.Elements));
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
                ElmList elmList =>
                PineValue.List(elmList.Elements.Select(ElmValueAsPineValue).ToList()),

                ElmChar elmChar =>
                PineValueAsInteger.ValueFromUnsignedInteger((int)elmChar.Value)
                .Extract(err => throw new Exception(err)),

                ElmInteger elmInteger =>
                PineValueAsInteger.ValueFromSignedInteger((int)elmInteger.Value),

                ElmString elmString =>
                PineValue.List(
                    [PineValueAsString.ValueFromString("String"),
                        PineValue.List([PineValueAsString.ValueFromString(elmString.Value)])]),

                ElmTag elmTag =>
                PineValue.List(
                    [PineValueAsString.ValueFromString(elmTag.TagName),
                        PineValue.List(elmTag.Arguments.Select(ElmValueAsPineValue).ToList())]),

                ElmRecord elmRecord =>
                PineValue.List(
                    [PineValueAsString.ValueFromString("Record"),
                        PineValue.List(
                            [PineValue.List(
                            [.. elmRecord.Fields.Select(field =>
                            PineValue.List(
                                [PineValueAsString.ValueFromString(field.FieldName),
                                    ElmValueAsPineValue(field.Value)]))])])]),

                _ =>
                throw new NotImplementedException(
                    "Not implemented for value type: " + elmValue.GetType().FullName)
            };
    }

    public static Result<string, ElmRecord> ElmValueAsElmRecord(ElmValue elmValue)
    {
        static Result<string, (string, ElmValue)> tryMapToRecordField(ElmValue fieldElmValue)
        {
            if (fieldElmValue is ElmList fieldListItems)
            {
                if (fieldListItems.Elements.Count == 2)
                {
                    var fieldNameValue = fieldListItems.Elements[0];
                    var fieldValue = fieldListItems.Elements[1];

                    Result<string, (string, ElmValue)> continueWithFieldName(string fieldName) =>
                        (0 < fieldName.Length && !char.IsUpper(fieldName[0]))
                        ?
                        Result<string, (string, ElmValue)>.ok((fieldName, fieldValue))
                        :
                        Result<string, (string, ElmValue)>.err(
                            "Field name does start with uppercase: '" + fieldName + "'");

                    return fieldNameValue switch
                    {
                        ElmList fieldNameValueList =>
                        TryMapElmValueToString(fieldNameValueList)
                        .Map(continueWithFieldName)
                        .WithDefault(Result<string, (string, ElmValue)>.err(
                            "Failed parsing field name value.")),

                        ElmString fieldName =>
                        continueWithFieldName(fieldName.Value),

                        _ =>
                        Result<string, (string, ElmValue)>.err("Unexpected type in field name value.")
                    };
                }
                else
                    return Result<string, (string, ElmValue)>.err(
                        "Unexpected number of list items: " + fieldListItems.Elements.Count);
            }
            else
                return Result<string, (string, ElmValue)>.err("Not a list.");
        }

        return elmValue switch
        {
            ElmList recordFieldList =>
            recordFieldList.Elements.Select(tryMapToRecordField).ListCombine()
            .AndThen(recordFields =>
            {
                var recordFieldsNames = recordFields.Select(field => field.Item1).ToList();

                if (recordFieldsNames.OrderBy(name => name).SequenceEqual(recordFieldsNames))
                    return Result<string, ElmRecord>.ok(new ElmRecord(recordFields));
                else
                    return Result<string, ElmRecord>.err("Unexpected order of fields.");
            }),

            _ =>
            Result<string, ElmRecord>.err("Value is not a list.")
        };
    }

    public static Maybe<string> TryMapElmValueToString(ElmList elmValues) =>
        elmValues.Elements.Select(TryMapElmValueToChar).ListCombine()
        .Map(chars => new string([.. chars]));

    public static Maybe<char> TryMapElmValueToChar(ElmValue elmValue) =>
        elmValue switch
        {
            ElmChar elmChar =>
            Maybe<char>.just(elmChar.Value),

            _ =>
            Maybe<char>.nothing()
        };
}
