using System;
using System.Diagnostics;
using System.Reflection;
using System.Text.Json.Serialization;
using System.Text.Json;

namespace Pine.Json;

public class JsonConverterForResult : JsonConverterFactory
{
    public override bool CanConvert(Type typeToConvert)
        => typeToConvert.IsGenericType
        && typeToConvert.GetGenericTypeDefinition() == typeof(Result<,>);

    public override JsonConverter CreateConverter(
        Type typeToConvert, JsonSerializerOptions options)
    {
        Debug.Assert(typeToConvert.IsGenericType &&
            typeToConvert.GetGenericTypeDefinition() == typeof(Result<,>));

        Type errType = typeToConvert.GetGenericArguments()[0];

        Type okType = typeToConvert.GetGenericArguments()[1];

        JsonConverter converter = (JsonConverter)Activator.CreateInstance(
            typeof(JsonConverterForResult<,>)
                .MakeGenericType(errType, okType),
            BindingFlags.Instance | BindingFlags.Public,
            binder: null,
            args: null,
            culture: null)!;

        return converter;
    }
}

public class JsonConverterForResult<ErrT, OkT> : JsonConverter<Result<ErrT, OkT>>
{
    public override Result<ErrT, OkT> Read(
        ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType != JsonTokenType.StartObject)
        {
            throw new JsonException("Expected start object");
        }
        reader.Read();

        if (reader.TokenType != JsonTokenType.PropertyName)
        {
            throw new JsonException("Expected property name (" + nameof(Result<ErrT, OkT>.Err) + " or " + nameof(Result<ErrT, OkT>.Ok) + ")");
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
            nameof(Result<ErrT, OkT>.Err) => Result<ErrT, OkT>.err(JsonSerializer.Deserialize<ErrT>(ref reader, options)!),
            nameof(Result<ErrT, OkT>.Ok) => Result<ErrT, OkT>.ok(JsonSerializer.Deserialize<OkT>(ref reader, options)!),
            _ => throw new JsonException("Unexpected property name: " + propertyName)
        };

        reader.Read();
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

    public override void Write(
        Utf8JsonWriter writer, Result<ErrT, OkT> value, JsonSerializerOptions options)
    {
        writer.WriteStartObject();

        writer.WritePropertyName(value.IsOk() ? nameof(Result<ErrT, OkT>.Ok) : nameof(Result<ErrT, OkT>.Err));

        writer.WriteStartArray();

        if (value is Result<ErrT, OkT>.Ok okResult)
            JsonSerializer.Serialize(writer, okResult.Value, options);

        if (value is Result<ErrT, OkT>.Err errResult)
            JsonSerializer.Serialize(writer, errResult.Value, options);

        writer.WriteEndArray();

        writer.WriteEndObject();
    }
}