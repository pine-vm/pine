using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.PineVM;
using Pine.Core.PopularEncodings;
using System;
using System.Linq;

namespace Pine.Elm;

/// <summary>
/// Mirroring how the Elm kernel libraries encode JSON values:
/// https://github.com/pine-vm/pine/blob/4f08d38c56b59a27ee9f92c56fd25cb3be0dc1e5/implement/pine/Elm/elm-compiler/elm-kernel-modules/Json/Encode.elm#L6-L13
/// </summary>
public class ElmValueJsonValueEncoding
{
    public static PineValue EncodeAsJsonValuePineValue(ElmValue elmValue)
    {
        var (tagName, tagArg) = EncodeAsJsonValueTaggedPineValue(elmValue);

        return ElmValueEncoding.TagAsPineValue(tagName, [tagArg]);
    }

    public static (string tagName, PineValue tagArg) EncodeAsJsonValueTaggedPineValue(ElmValue elmValue)
    {
        /*
        type Value
            = NullValue
            | BoolValue Bool
            | IntValue Int
            | StringValue String
            | ArrayValue (List Value)
            | ObjectValue (List ( String, Value ))
            | FloatValue String
         * */

        if (elmValue is ElmValue.ElmString elmString)
        {
            return ("StringValue", ElmValueEncoding.StringAsPineValue(elmString.Value));
        }

        if (elmValue is ElmValue.ElmInteger elmInt)
        {
            return ("IntValue", IntegerEncoding.EncodeSignedInteger(elmInt.Value));
        }

        if (elmValue is ElmValue.ElmList elmList)
        {
            var listItems =
                elmList.Elements
                .Select(EncodeAsJsonValuePineValue).ToArray();

            return ("ArrayValue", PineValue.List(listItems));
        }

        if (elmValue is ElmValue.ElmTag elmTag)
        {
            if (elmTag.TagName is "True")
            {
                return ("BoolValue", PineKernelValues.TrueValue);
            }

            if (elmTag.TagName is "False")
            {
                return ("BoolValue", PineKernelValues.FalseValue);
            }

            /*
             * The automatic generation of JSON encoders for Elm types maps Elm tags to JSON objects,
             * creating a JSON object with a single key-value pair where the key is the tag name and
             * the value is an array of the tag's arguments.
             * */

            return
                ("ObjectValue",
                PineValue.List(
                    [
                        PineValue.List(
                            [
                            ElmValueEncoding.StringAsPineValue(elmTag.TagName),
                            EncodeAsJsonValuePineValue(ElmValue.ListInstance(elmTag.Arguments))
                            ])
                    ]));
        }

        if (elmValue is ElmValue.ElmRecord elmRecord)
        {
            var recordFields = new PineValue[elmRecord.Fields.Count];

            for (var i = 0; i < elmRecord.Fields.Count; i++)
            {
                var (fieldName, fieldValue) = elmRecord.Fields[i];

                recordFields[i] =
                    PineValue.List(
                        [
                        ElmValueEncoding.StringAsPineValue(fieldName),
                        EncodeAsJsonValuePineValue(fieldValue)
                        ]);
            }

            return
                ("ObjectValue",
                PineValue.List(recordFields));
        }

        if (elmValue is ElmValue.ElmBytes elmBytes)
        {
            var asBase64String = Convert.ToBase64String(elmBytes.Value.Span);

            return
                ("ObjectValue",
                PineValue.List(
                    [
                        PineValue.List(
                            [
                            ElmValueEncoding.StringAsPineValue("AsBase64"),
                            EncodeAsJsonValuePineValue(ElmValue.StringInstance(asBase64String))
                            ])
                    ]));
        }

        if (elmValue is ElmValue.ElmChar elmChar)
        {
            return
                ("StringValue",
                ElmValueEncoding.StringAsPineValue(new string((char)elmChar.Value, 1)));
        }

        if (elmValue is ElmValue.ElmFloat elmFloat)
        {
            var asDouble =
                (double)elmFloat.Numerator / (double)elmFloat.Denominator;

            return
                ("FloatValue",
                ElmValueEncoding.StringAsPineValue(System.Text.Json.JsonSerializer.Serialize(asDouble)));
        }

        throw new NotImplementedException(
            "JSON encoding not implemented for Elm value of type " + elmValue.GetType());
    }
}
