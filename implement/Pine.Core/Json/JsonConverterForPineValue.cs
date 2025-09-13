using Pine.Core;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Json;

public class JsonConverterForPineValue : JsonConverter<PineValue>
{
    /// <inheritdoc/>
    public override PineValue? Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType == JsonTokenType.Number)
        {
            var integerString = System.Text.Encoding.UTF8.GetString(reader.ValueSpan);

            if (System.Numerics.BigInteger.TryParse(integerString, out var integer))
            {
                reader.Read();

                return IntegerEncoding.EncodeSignedInteger(integer);
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

            /*
             * Maintain backward compatibility: Previous versions used the label "ListAsString".
             * */
            if (propertyName is "BlobAsString" or "ListAsString")
            {
                var pineValue = StringEncoding.BlobValueFromString(reader.GetString());

                reader.Read();

                if (reader.TokenType is not JsonTokenType.EndObject)
                    throw new NotSupportedException("Unexpected token type in object: " + reader.TokenType);

                reader.Read();

                return pineValue;
            }

            if (propertyName is "ListAsString_2024")
            {
                var pineValue = StringEncoding.ValueFromString_2024(reader.GetString());

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

    /// <inheritdoc/>
    public override void Write(Utf8JsonWriter writer, PineValue value, JsonSerializerOptions options)
    {
        if (IntegerEncoding.ParseSignedIntegerStrict(value) is Result<string, System.Numerics.BigInteger>.Ok asInt)
        {
            if (asInt.Value < long.MaxValue && long.MinValue < asInt.Value)
            {
                writer.WriteNumberValue((long)asInt.Value);

                return;
            }
        }

        if (value is PineValue.BlobValue blobValue)
        {
            if (blobValue.Bytes.Length is not 0)
            {
                if (StringEncoding.StringFromValue(value).IsOkOrNull() is { } asString && 0 < asString.Length)
                {
                    if (!asString.All(asChar => (asChar & 0xff00) is 0x400 || (asChar & 0xff00) is 0x200))
                    {
                        writer.WriteStartObject();

                        writer.WriteString("BlobAsString", asString);

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

            for (var i = 0; i < list.Items.Length; i++)
            {
                Write(writer, list.Items.Span[i], options);
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
