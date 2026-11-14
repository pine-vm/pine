using Pine.Core.CodeAnalysis;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Core.Json;

/// <summary>
/// Contract-correct, lossless JSON converter for <see cref="PineValue"/> in model documents.
/// <para>
/// A <see cref="PineValue.ListValue"/> is encoded as a JSON array of its items; a
/// <see cref="PineValue.BlobValue"/> is encoded as a JSON object with a single <c>"Blob"</c> property holding
/// the bytes in base64. Unlike <see cref="JsonConverterForPineValue"/>, this converter leaves the
/// reader positioned on the last token of the value it read, as required by
/// <see cref="JsonSerializer"/> for value converters nested in object/collection converters.
/// </para>
/// </summary>
public class PineValueJsonConverter : JsonConverter<PineValue>
{
    /// <inheritdoc/>
    public override PineValue Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType is JsonTokenType.StartArray)
        {
            var items = new List<PineValue>();

            reader.Read();

            while (reader.TokenType is not JsonTokenType.EndArray)
            {
                items.Add(Read(ref reader, typeToConvert, options));

                reader.Read();
            }

            // Positioned on EndArray (the last token of this value).
            return PineValue.List([.. items]);
        }

        if (reader.TokenType is JsonTokenType.StartObject)
        {
            reader.Read();

            if (reader.TokenType is not JsonTokenType.PropertyName)
            {
                throw new JsonException("Expected property name in PineValue object.");
            }

            var propertyName = reader.GetString();

            if (propertyName is not "Blob")
            {
                throw new JsonException("Unexpected property name in PineValue object: " + propertyName);
            }

            reader.Read();

            var bytes = reader.GetBytesFromBase64();

            reader.Read();

            if (reader.TokenType is not JsonTokenType.EndObject)
            {
                throw new JsonException("Expected end of PineValue object.");
            }

            // Positioned on EndObject (the last token of this value).
            return PineValue.Blob(bytes);
        }

        throw new JsonException("Unexpected token type for PineValue: " + reader.TokenType);
    }

    /// <inheritdoc/>
    public override void Write(Utf8JsonWriter writer, PineValue value, JsonSerializerOptions options)
    {
        if (value is PineValue.BlobValue blobValue)
        {
            writer.WriteStartObject();

            writer.WriteBase64String("Blob", blobValue.Bytes.Span);

            writer.WriteEndObject();

            return;
        }

        if (value is PineValue.ListValue listValue)
        {
            writer.WriteStartArray();

            for (var i = 0; i < listValue.Items.Length; i++)
            {
                Write(writer, listValue.Items.Span[i], options);
            }

            writer.WriteEndArray();

            return;
        }

        throw new JsonException("Unexpected PineValue kind: " + value.GetType().FullName);
    }
}

/// <summary>
/// JSON converter for <see cref="System.Numerics.BigInteger"/>.
/// <para>
/// <see cref="JsonSerializer"/> has no built-in support for
/// <see cref="System.Numerics.BigInteger"/>. The value is encoded as its decimal
/// string representation to preserve arbitrary precision losslessly.
/// </para>
/// </summary>
public class BigIntegerJsonConverter : JsonConverter<System.Numerics.BigInteger>
{
    /// <inheritdoc/>
    public override System.Numerics.BigInteger Read(
        ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType is JsonTokenType.String)
        {
            var asString =
                reader.GetString()
                ?? throw new JsonException("Expected a non-null BigInteger string.");

            return System.Numerics.BigInteger.Parse(asString, System.Globalization.CultureInfo.InvariantCulture);
        }

        if (reader.TokenType is JsonTokenType.Number)
        {
            var asString = System.Text.Encoding.UTF8.GetString(reader.ValueSpan);

            return System.Numerics.BigInteger.Parse(asString, System.Globalization.CultureInfo.InvariantCulture);
        }

        throw new JsonException("Unexpected token type for BigInteger: " + reader.TokenType);
    }

    /// <inheritdoc/>
    public override void Write(
        Utf8JsonWriter writer, System.Numerics.BigInteger value, JsonSerializerOptions options) =>
        writer.WriteStringValue(value.ToString(System.Globalization.CultureInfo.InvariantCulture));
}

/// <summary>
/// JSON converter encoding a <see cref="DeclQualifiedName"/> as its
/// <see cref="DeclQualifiedName.FullName"/> string, both as a value and as a dictionary property name.
/// </summary>
public class DeclQualifiedNameJsonConverter : JsonConverter<DeclQualifiedName>
{
    /// <inheritdoc/>
    public override DeclQualifiedName Read(
        ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) =>
        DeclQualifiedName.FromString(
            reader.GetString()
            ?? throw new JsonException("Expected a non-null DeclQualifiedName string."));

    /// <inheritdoc/>
    public override void Write(
        Utf8JsonWriter writer, DeclQualifiedName value, JsonSerializerOptions options) =>
        writer.WriteStringValue(value.FullName);

    /// <inheritdoc/>
    public override DeclQualifiedName ReadAsPropertyName(
        ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) =>
        DeclQualifiedName.FromString(
            reader.GetString()
            ?? throw new JsonException("Expected a non-null DeclQualifiedName property name."));

    /// <inheritdoc/>
    public override void WriteAsPropertyName(
        Utf8JsonWriter writer, DeclQualifiedName value, JsonSerializerOptions options) =>
        writer.WritePropertyName(value.FullName);
}
