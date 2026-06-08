using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

/// <summary>
/// JSON serialization and deserialization for the abstract Elm syntax model rooted at <see cref="File"/>.
/// <para>
/// The choice types of the model (such as <see cref="Expression"/>, <see cref="Pattern"/>,
/// <see cref="Declaration"/> and <see cref="TypeAnnotation"/>) are annotated with
/// <c>[JsonConverter(typeof(JsonConverterForChoiceType))]</c>, which maps each variant to the form
/// <c>{ "VariantName": [ field0, field1, ... ] }</c>.
/// </para>
/// <para>
/// The precomputed <see cref="PineValue"/> instances carried by some nodes are encoded using
/// <see cref="PineValueJsonConverter"/>, so that a round-trip restores the exact same values.
/// The field-name list of <see cref="Pattern.RecordPattern"/> is encoded compactly as an array of
/// strings (see <see cref="RecordPatternFieldsJsonConverter"/>); the field-name
/// <see cref="PineValue"/> instances are recomputed on deserialization.
/// </para>
/// </summary>
public static class ElmSyntaxAbstractJson
{
    private static readonly JsonSerializerOptions s_jsonSerializerOptions = BuildJsonSerializerOptions();

    /// <summary>
    /// The shared <see cref="JsonSerializerOptions"/> used for the abstract Elm syntax model.
    /// </summary>
    public static JsonSerializerOptions JsonSerializerOptions => s_jsonSerializerOptions;

    /// <summary>
    /// Builds the <see cref="JsonSerializerOptions"/> used to serialize and deserialize the abstract Elm syntax model.
    /// </summary>
    public static JsonSerializerOptions BuildJsonSerializerOptions()
    {
        var options =
            new JsonSerializerOptions
            {
                MaxDepth = 1000,
                NewLine = "\n",
            };

        options.Converters.Add(new PineValueJsonConverter());
        options.Converters.Add(new BigIntegerJsonConverter());
        options.Converters.Add(new RecordPatternFieldsJsonConverter());

        return options;
    }

    /// <summary>
    /// Serializes the given abstract Elm syntax <see cref="File"/> to a JSON string.
    /// </summary>
    public static string FileToJsonString(File file) =>
        JsonSerializer.Serialize(file, s_jsonSerializerOptions);

    /// <summary>
    /// Deserializes an abstract Elm syntax <see cref="File"/> from the given JSON string.
    /// </summary>
    public static File FileFromJsonString(string json) =>
        JsonSerializer.Deserialize<File>(json, s_jsonSerializerOptions)
        ?? throw new JsonException("Decoded a null File from JSON.");

    /// <summary>
    /// Serializes any node of the abstract Elm syntax model to a JSON string.
    /// </summary>
    public static string ToJsonString<T>(T value) =>
        JsonSerializer.Serialize(value, s_jsonSerializerOptions);

    /// <summary>
    /// Deserializes a node of the abstract Elm syntax model from the given JSON string.
    /// </summary>
    public static T FromJsonString<T>(string json) =>
        JsonSerializer.Deserialize<T>(json, s_jsonSerializerOptions)
        ?? throw new JsonException("Decoded a null value from JSON for type " + typeof(T).FullName + ".");
}

/// <summary>
/// JSON converter for the field-name list of <see cref="Pattern.RecordPattern"/>.
/// <para>
/// Encodes the fields as a JSON array of the field names. The precomputed
/// <see cref="PineValue"/> string encodings are derived from the field names and therefore
/// recomputed on deserialization rather than stored.
/// </para>
/// </summary>
public class RecordPatternFieldsJsonConverter
    : JsonConverter<ImmutableArray<(string FieldName, PineValue FieldNameValue)>>
{
    /// <inheritdoc/>
    public override ImmutableArray<(string FieldName, PineValue FieldNameValue)> Read(
        ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType is not JsonTokenType.StartArray)
        {
            throw new JsonException("Expected start array for record pattern fields.");
        }

        reader.Read();

        var builder = ImmutableArray.CreateBuilder<(string FieldName, PineValue FieldNameValue)>();

        while (reader.TokenType is not JsonTokenType.EndArray)
        {
            var fieldName =
                reader.GetString()
                ?? throw new JsonException("Expected a non-null field name string.");

            builder.Add((fieldName, StringEncoding.ValueFromString(fieldName)));

            reader.Read();
        }

        return builder.ToImmutable();
    }

    /// <inheritdoc/>
    public override void Write(
        Utf8JsonWriter writer,
        ImmutableArray<(string FieldName, PineValue FieldNameValue)> value,
        JsonSerializerOptions options)
    {
        writer.WriteStartArray();

        foreach (var field in value)
        {
            writer.WriteStringValue(field.FieldName);
        }

        writer.WriteEndArray();
    }
}

/// <summary>
/// Contract-correct, lossless JSON converter for <see cref="PineValue"/> used by the abstract Elm syntax model.
/// <para>
/// A <see cref="PineValue.ListValue"/> is encoded as a JSON array of its items; a
/// <see cref="PineValue.BlobValue"/> is encoded as a JSON object with a single <c>"Blob"</c> property holding
/// the bytes in base64. Unlike <see cref="Pine.Core.Json.JsonConverterForPineValue"/>, this converter leaves the
/// reader positioned on the last token of the value it read, as required by
/// <see cref="System.Text.Json.JsonSerializer"/> for value converters nested in object/collection converters.
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
/// <see cref="System.Text.Json.JsonSerializer"/> has no built-in support for
/// <see cref="System.Numerics.BigInteger"/>, so the abstract Elm syntax model (whose integer and float
/// literals carry arbitrary-precision integers) needs this converter. The value is encoded as its decimal
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
