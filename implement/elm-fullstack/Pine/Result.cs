using System;
using System.Diagnostics;
using System.Reflection;
using System.Text.Json.Serialization;
using System.Text.Json;

namespace Pine;

[JsonConverter(typeof(JsonConverterForResult))]
/// <summary>
/// Generic DU type to describe the outcome of an operation with an overall classification into either failure ('Err') or success ('Ok').
/// </summary>
public abstract record Result<ErrT, OkT>
{
    /// <summary>
    /// Constructor for the 'Err' case.
    /// </summary>
    static public Result<ErrT, OkT> err(ErrT err) => new Err(err);

    /// <summary>
    /// Constructor for the 'Ok' case.
    /// </summary>
    static public Result<ErrT, OkT> ok(OkT ok) => new Ok(ok);

    /// <summary>
    /// Returns whether this value represents an error case.
    /// </summary>
    public bool IsErr() =>
        this switch
        {
            Err _ => true,
            _ => false
        };

    /// <summary>
    /// Returns whether this value represents a success case.
    /// </summary>
    public bool IsOk() =>
        this switch
        {
            Ok _ => true,
            _ => false
        };

    /// <summary>
    /// A <see cref="Result{ErrT, OkT}"/> that is an error.
    /// </summary>
    public record Err(ErrT Error) : Result<ErrT, OkT>;

    /// <summary>
    /// A <see cref="Result{ErrT, OkT}"/> that is a success.
    /// </summary>
    public record Ok(OkT Success) : Result<ErrT, OkT>;

    /// <summary>
    /// Maps the value of the 'Ok' case.
    /// </summary>
    public Result<ErrT, MappedOkT> map<MappedOkT>(Func<OkT, MappedOkT> okMap) =>
        this switch
        {
            Ok ok => new Result<ErrT, MappedOkT>.Ok(okMap(ok.Success)),
            Err err => new Result<ErrT, MappedOkT>.Err(err.Error),
            _ => throw new NotImplementedException()
        };

    /// <summary>
    /// Maps the value of the 'Err' case.
    /// </summary>
    public Result<MappedErrT, OkT> mapError<MappedErrT>(Func<ErrT, MappedErrT> errMap) =>
        this switch
        {
            Ok ok => new Result<MappedErrT, OkT>.Ok(ok.Success),
            Err err => new Result<MappedErrT, OkT>.Err(errMap(err.Error)),
            _ => throw new NotImplementedException()
        };

    /// <summary>
    /// Map for the 'Ok' case that may fail.
    /// Used for chaining together a sequence of computations that may fail.
    /// </summary>
    public Result<ErrT, MappedOkT> andThen<MappedOkT>(Func<OkT, Result<ErrT, MappedOkT>> okMap) =>
        this switch
        {
            Ok ok => okMap(ok.Success),
            Err err => new Result<ErrT, MappedOkT>.Err(err.Error),
            _ => throw new NotImplementedException()
        };

    public OkT withDefault(Func<OkT> getDefault) =>
        unpack(fromErr: _ => getDefault(), fromOk: ok => ok);

    public OkT extract(Func<ErrT, OkT> fromErr) =>
        unpack(fromErr: fromErr, fromOk: ok => ok);

    public T unpack<T>(Func<ErrT, T> fromErr, Func<OkT, T> fromOk) =>
        this switch
        {
            Ok ok => fromOk(ok.Success),
            Err err => fromErr(err.Error),
            _ => throw new NotImplementedException()
        };
}

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
            JsonSerializer.Serialize(writer, okResult.Success, options);

        if (value is Result<ErrT, OkT>.Err errResult)
            JsonSerializer.Serialize(writer, errResult.Error, options);

        writer.WriteEndArray();

        writer.WriteEndObject();
    }
}