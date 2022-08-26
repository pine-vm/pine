using System;
using System.Diagnostics;
using System.Reflection;
using System.Text.Json.Serialization;
using System.Text.Json;

namespace Pine.Json;

public class JsonConverterForMaybe : JsonConverterFactory
{
    public override bool CanConvert(Type typeToConvert)
        => typeToConvert.IsGenericType
        && typeToConvert.GetGenericTypeDefinition() == typeof(Maybe<>);

    public override JsonConverter CreateConverter(
        Type typeToConvert, JsonSerializerOptions options)
    {
        Debug.Assert(typeToConvert.IsGenericType &&
            typeToConvert.GetGenericTypeDefinition() == typeof(Maybe<>));

        Type justType = typeToConvert.GetGenericArguments()[0];

        JsonConverter converter = (JsonConverter)Activator.CreateInstance(
            typeof(JsonConverterForMaybe<>).MakeGenericType(justType),
            BindingFlags.Instance | BindingFlags.Public,
            binder: null,
            args: null,
            culture: null)!;

        return converter;
    }
}

public class JsonConverterForMaybe<JustT> : JsonConverter<Maybe<JustT>>
{
    public override Maybe<JustT> Read(
        ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType != JsonTokenType.StartObject)
        {
            throw new JsonException("Expected start object");
        }
        reader.Read();

        if (reader.TokenType != JsonTokenType.PropertyName)
        {
            throw new JsonException("Expected property name (" + nameof(Maybe<JustT>.Nothing) + " or " + nameof(Maybe<JustT>.Just) + ")");
        }

        string? propertyName = reader.GetString();

        reader.Read();
        if (reader.TokenType != JsonTokenType.StartArray)
        {
            throw new JsonException("Expected start array");
        }

        reader.Read();

        var result = propertyName switch
        {
            nameof(Maybe<JustT>.Nothing) => Maybe<JustT>.Nothing.nothing(),
            nameof(Maybe<JustT>.Just) => ReadJust(ref reader, options),
            _ => throw new JsonException("Unexpected property name: " + propertyName)
        };

        if (reader.TokenType != JsonTokenType.EndArray)
        {
            throw new JsonException("Expected end array");
        }

        reader.Read();
        if (reader.TokenType != JsonTokenType.EndObject)
        {
            throw new JsonException("Expected end object");
        }

        return result;
    }

    static Maybe<JustT> ReadJust(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
        var value = Maybe<JustT>.just(JsonSerializer.Deserialize<JustT>(ref reader, options)!);

        reader.Read();

        return value;
    }

    public override void Write(
        Utf8JsonWriter writer, Maybe<JustT> value, JsonSerializerOptions options)
    {
        writer.WriteStartObject();

        writer.WritePropertyName(value.IsJust() ? nameof(Maybe<JustT>.Just) : nameof(Maybe<JustT>.Nothing));

        writer.WriteStartArray();

        if (value is Maybe<JustT>.Just just)
            JsonSerializer.Serialize(writer, just.Value, options);

        writer.WriteEndArray();

        writer.WriteEndObject();
    }
}
