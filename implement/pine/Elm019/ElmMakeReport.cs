using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Elm019;


[JsonConverter(typeof(ElmMakeReportConverter))]
public abstract record ElmMakeReport
{
    public record ElmMakeReportCompileErrors(
        string Type,
        IReadOnlyList<ElmMakeReportCompileErrorsError> Errors)
        : ElmMakeReport;

    public record ElmMakeReportError(
        string Type,
        string? Path,
        string Title,
        [property: JsonConverter(typeof(MessageItemArrayConverter))]
        IReadOnlyList<MessageItem> Message)
        : ElmMakeReport;
}

public record ElmMakeReportCompileErrorsError(
    string Path,
    string Name,
    IReadOnlyList<ElmMakeReportCompileErrorsErrorProblem> Problems);

public record ElmMakeReportCompileErrorsErrorProblem(
    string Title,
    ElmMakeReportCompileErrorsErrorProblemRegion Region,
    [property: JsonConverter(typeof(MessageItemArrayConverter))]
    IReadOnlyList<MessageItem> Message);

public record ElmMakeReportCompileErrorsErrorProblemRegion(
    ElmMakeReportCompileErrorsErrorProblemRegionPosition Start,
    ElmMakeReportCompileErrorsErrorProblemRegionPosition End);

public record ElmMakeReportCompileErrorsErrorProblemRegionPosition(
    int Line,
    int Column);

[JsonConverter(typeof(MessageItemConverter))]
public abstract record MessageItem
{
    public record StringMessage(
        string Value)
        : MessageItem;

    public record StyledMessage(
        bool Bold,
        bool Underline,
        string? Color,
        string @String)
        : MessageItem;
}

public class ElmMakeReportConverter : JsonConverter<ElmMakeReport>
{
    public static readonly JsonSerializerOptions SerializerOptions =
        new()
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            Converters = { new ElmMakeReportConverter(), new MessageItemConverter(), new MessageItemArrayConverter() }
        };

    public static ElmMakeReport Deserialize(string json) =>
        JsonSerializer.Deserialize<ElmMakeReport>(json, SerializerOptions) ??
        throw new JsonException("Failed to deserialize ElmMakeReport.");

    public override ElmMakeReport? Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        using var doc = JsonDocument.ParseValue(ref reader);
        var root = doc.RootElement;

        if (!root.TryGetProperty("type", out var typeProperty))
        {
            throw new JsonException("No 'type' property found.");
        }

        var typeVal = typeProperty.GetString();

        if (typeVal is null)
        {
            throw new JsonException("The 'type' property is null.");
        }

        // Depending on the type, we deserialize into the appropriate record.
        return typeVal switch
        {
            "compile-errors" =>
            JsonSerializer.Deserialize<ElmMakeReport.ElmMakeReportCompileErrors>(
                root.GetRawText(), options) ??
                throw new JsonException("Failed to deserialize as compile-errors"),

            "error" =>
            JsonSerializer.Deserialize<ElmMakeReport.ElmMakeReportError>(
                root.GetRawText(), options) ??
                throw new JsonException("Failed to deserialize as error"),

            _ =>
            throw new JsonException($"Unexpected 'type' value: {typeVal}")
        };
    }

    public override void Write(Utf8JsonWriter writer, ElmMakeReport value, JsonSerializerOptions options)
    {
        switch (value)
        {
            case ElmMakeReport.ElmMakeReportCompileErrors compileErrors:
                JsonSerializer.Serialize(writer, compileErrors, options);
                break;
            case ElmMakeReport.ElmMakeReportError error:
                JsonSerializer.Serialize(writer, error, options);
                break;
            default:
                throw new JsonException("Unexpected ElmMakeReport subtype");
        }
    }
}

public class MessageItemArrayConverter : JsonConverter<IReadOnlyList<MessageItem>>
{
    public override IReadOnlyList<MessageItem> Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        var list = new List<MessageItem>();

        if (reader.TokenType != JsonTokenType.StartArray)
            throw new JsonException("Expected start of array.");

        while (reader.Read())
        {
            if (reader.TokenType is JsonTokenType.EndArray)
                break;

            list.Add(JsonSerializer.Deserialize<MessageItem>(ref reader, options)!);
        }

        return list;
    }

    public override void Write(Utf8JsonWriter writer, IReadOnlyList<MessageItem> value, JsonSerializerOptions options)
    {
        writer.WriteStartArray();
        foreach (var item in value)
        {
            JsonSerializer.Serialize(writer, item, options);
        }
        writer.WriteEndArray();
    }
}

public class MessageItemConverter : JsonConverter<MessageItem>
{
    public override MessageItem? Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType is JsonTokenType.String)
        {
            string str = reader.GetString()!;

            return new MessageItem.StringMessage(str);
        }

        if (reader.TokenType is JsonTokenType.StartObject)
        {
            // Deserialize into a StyledMessage using the existing options
            var styled =
                JsonSerializer.Deserialize<MessageItem.StyledMessage>(ref reader, options);

            if (styled is null)
            {
                throw new JsonException("Failed to deserialize object as StyledMessage.");
            }

            return styled;
        }

        throw new JsonException("Unexpected JSON token type for MessageItem.");
    }

    public override void Write(Utf8JsonWriter writer, MessageItem value, JsonSerializerOptions options)
    {
        throw new NotImplementedException();
    }
}
