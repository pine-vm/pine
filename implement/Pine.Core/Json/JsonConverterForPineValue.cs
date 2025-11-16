using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Core.Json;

/// <summary>
/// Custom <see cref="JsonConverter{T}"/> that converts between <see cref="PineValue"/> instances and their JSON representations.
/// </summary>
public class JsonConverterForPineValue : JsonConverter<PineValue>
{
    /// <inheritdoc/>
    public override PineValue? Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType is JsonTokenType.Number)
        {
            var integerString = System.Text.Encoding.UTF8.GetString(reader.ValueSpan);

            if (System.Numerics.BigInteger.TryParse(integerString, out var integer))
            {
                reader.Read();

                return IntegerEncoding.EncodeSignedInteger(integer);
            }
        }

        if (reader.TokenType is JsonTokenType.StartArray)
        {
            reader.Read();

            var elements = new List<PineValue>();

            while (reader.TokenType is not JsonTokenType.EndArray)
            {
                elements.Add(Read(ref reader, typeToConvert: typeToConvert, options: options)!);
            }

            reader.Read();

            return PineValue.List([.. elements]);
        }

        if (reader.TokenType is JsonTokenType.StartObject)
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
                var propertyValue =
                    reader.GetString() ??
                    throw new JsonException("Expected non-null string value for property " + propertyName);

                var pineValue = StringEncoding.ValueFromString(propertyValue);

                reader.Read();

                if (reader.TokenType is not JsonTokenType.EndObject)
                    throw new NotSupportedException("Unexpected token type in object: " + reader.TokenType);

                reader.Read();

                return pineValue;
            }

            if (propertyName is "ListAsString_2024")
            {
                var propertyValue =
                    reader.GetString() ??
                    throw new JsonException("Expected non-null string value for property " + propertyName);

                var pineValue = StringEncoding.ValueFromString_2024(propertyValue);

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

    /// <summary>
    /// Serialize a <see cref="PineValue"/> by emitting integers, arrays, or blob wrappers to maintain round-trippable JSON.
    /// </summary>
    /// <param name="writer">Writer receiving the JSON output.</param>
    /// <param name="value">The Pine value to encode.</param>
    /// <param name="options">Serializer options applied to nested conversions.</param>
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

    /// <summary>
    /// Falls back to the canonical JSON representation for lists and binary blobs when no specialized form applies.
    /// </summary>
    /// <param name="writer">Writer receiving the JSON output.</param>
    /// <param name="value">The Pine value to encode.</param>
    /// <param name="options">Serializer options applied to nested conversions.</param>
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
