using Pine.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Json;

public class JsonConverterForPineValue : JsonConverter<PineValue>
{
    public override PineValue? Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType == JsonTokenType.Number)
        {
            var integerString = System.Text.Encoding.UTF8.GetString(reader.ValueSpan);

            if (System.Numerics.BigInteger.TryParse(integerString, out var integer))
            {
                reader.Read();

                return PineValueAsInteger.ValueFromSignedInteger(integer);
            }
        }

        if (reader.TokenType == JsonTokenType.StartArray)
        {
            reader.Read();

            var elements = new List<PineValue>();

            while (reader.TokenType != JsonTokenType.EndArray)
            {
                elements.Add(Read(ref reader, typeToConvert: typeToConvert, options: options)!);
            }

            reader.Read();

            return PineValue.List([.. elements]);
        }

        if (reader.TokenType == JsonTokenType.StartObject)
        {
            reader.Read();

            if (reader.TokenType is not JsonTokenType.PropertyName)
            {
                throw new NotSupportedException("Unexpected token type in object: " + reader.TokenType);
            }

            var propertyName = reader.GetString();

            reader.Read();

            if (propertyName is "AsBase64")
            {
                var pineValue = PineValue.Blob(reader.GetBytesFromBase64());

                reader.Read();

                if (reader.TokenType is not JsonTokenType.EndObject)
                    throw new NotSupportedException("Unexpected token type in object: " + reader.TokenType);

                reader.Read();

                return pineValue;
            }


            if (propertyName is "ListAsString")
            {
                var pineValue = PineValueAsString.ValueFromString(reader.GetString());

                reader.Read();

                if (reader.TokenType is not JsonTokenType.EndObject)
                    throw new NotSupportedException("Unexpected token type in object: " + reader.TokenType);

                reader.Read();

                return pineValue;
            }

            throw new NotSupportedException("Unsupported property name: " + propertyName);
        }

        throw new NotImplementedException("Unexpected token type: " + reader.TokenType);
    }

    public override void Write(Utf8JsonWriter writer, PineValue value, JsonSerializerOptions options)
    {
        if (PineValueAsInteger.SignedIntegerFromValueStrict(value) is Result<string, System.Numerics.BigInteger>.Ok asInt)
        {
            if (asInt.Value < long.MaxValue && long.MinValue < asInt.Value)
            {
                writer.WriteNumberValue((long)asInt.Value);

                return;
            }
        }

        if (value is PineValue.ListValue listValue)
        {
            if (0 < listValue.Elements.Length)
            {
                if (PineValueAsString.StringFromValue(value) is Result<string, string>.Ok asString && 0 < asString.Value.Length)
                {
                    if (!asString.Value.All(asChar => (asChar & 0xff00) == 0x400 || (asChar & 0xff00) == 0x200))
                    {
                        writer.WriteStartObject();

                        writer.WriteString("ListAsString", asString.Value);

                        writer.WriteEndObject();

                        return;
                    }
                }
            }
        }

        WriteDefaultRepresentation(writer, value, options);
    }

    private void WriteDefaultRepresentation(Utf8JsonWriter writer, PineValue value, JsonSerializerOptions options)
    {
        if (value is PineValue.ListValue list)
        {
            writer.WriteStartArray();

            for (int i = 0; i < list.Elements.Length; i++)
            {
                Write(writer, list.Elements.Span[i], options);
            }

            writer.WriteEndArray();

            return;
        }

        if (value is PineValue.BlobValue blob)
        {
            writer.WriteStartObject();

            writer.WriteBase64String("AsBase64", blob.Bytes.Span);

            writer.WriteEndObject();

            return;
        }

        throw new NotImplementedException();
    }
}
