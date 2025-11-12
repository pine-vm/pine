using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Core.Elm.Elm019;

/// <summary>
/// Discriminated union of reports produced by <c>elm make --report=json</c>.
/// Includes the <c>compile-errors</c> report and a generic <c>error</c> report shape.
/// Use <see cref="ElmMakeReportConverter"/> to deserialize from JSON.
/// </summary>
[JsonConverter(typeof(ElmMakeReportConverter))]
public abstract record ElmMakeReport
{
    /// <summary>
    /// Report containing compilation errors across one or more files.
    /// </summary>
    /// <param name="Type">Expected to be <c>compile-errors</c>.</param>
    /// <param name="Errors">The list of per-file error entries.</param>
    public record ElmMakeReportCompileErrors(
        string Type,
        IReadOnlyList<ElmMakeReportCompileErrorsError> Errors)
        : ElmMakeReport;

    /// <summary>
    /// Generic error report (not tied to the structured compile errors format).
    /// </summary>
    /// <param name="Type">Expected to be <c>error</c>.</param>
    /// <param name="Path">Optional file path related to the error.</param>
    /// <param name="Title">Short title describing the error.</param>
    /// <param name="Message">Detailed message items (string or styled fragments).</param>
    public record ElmMakeReportError(
        string Type,
        string? Path,
        string Title,
        [property: JsonConverter(typeof(MessageItemArrayConverter))]
        IReadOnlyList<MessageItem> Message)
        : ElmMakeReport;
}

/// <summary>
/// A per-file entry listing compile problems reported by the Elm compiler.
/// </summary>
/// <param name="Path">The file path where the errors occurred.</param>
/// <param name="Name">The module or file display name.</param>
/// <param name="Problems">The set of problems found in this file.</param>
public record ElmMakeReportCompileErrorsError(
    string Path,
    string Name,
    IReadOnlyList<ElmMakeReportCompileErrorsErrorProblem> Problems);

/// <summary>
/// A single compile-time problem, including a title, a text region and a message.
/// </summary>
/// <param name="Title">Short description of the problem.</param>
/// <param name="Region">The text region highlighting the problem location.</param>
/// <param name="Message">Detailed message items (string or styled fragments).</param>
public record ElmMakeReportCompileErrorsErrorProblem(
    string Title,
    ElmMakeReportCompileErrorsErrorProblemRegion Region,
    [property: JsonConverter(typeof(MessageItemArrayConverter))]
    IReadOnlyList<MessageItem> Message);

/// <summary>
/// A region in a source file, defined by start and end positions (1-based line/column).
/// </summary>
/// <param name="Start">Inclusive start position.</param>
/// <param name="End">Inclusive end position.</param>
public record ElmMakeReportCompileErrorsErrorProblemRegion(
    ElmMakeReportCompileErrorsErrorProblemRegionPosition Start,
    ElmMakeReportCompileErrorsErrorProblemRegionPosition End);

/// <summary>
/// A 1-based line/column position in a source file.
/// </summary>
/// <param name="Line">1-based line number.</param>
/// <param name="Column">1-based column number.</param>
public record ElmMakeReportCompileErrorsErrorProblemRegionPosition(
    int Line,
    int Column);

/// <summary>
/// Discriminated union over message items emitted by <c>elm make</c>: either a raw string or a styled object.
/// </summary>
[JsonConverter(typeof(MessageItemConverter))]
public abstract record MessageItem
{
    /// <summary>
    /// A simple string message item.
    /// </summary>
    /// <param name="Value">The message text.</param>
    public record StringMessage(
        string Value)
        : MessageItem;

    /// <summary>
    /// A message item with style metadata provided by the compiler.
    /// </summary>
    /// <param name="Bold">Whether the text should be rendered in bold.</param>
    /// <param name="Underline">Whether the text should be underlined.</param>
    /// <param name="Color">Suggested color name or code (optional).</param>
    /// <param name="String">The message text.</param>
    public record StyledMessage(
        bool Bold,
        bool Underline,
        string? Color,
        string @String)
        : MessageItem;
}

/// <summary>
/// Polymorphic JSON converter for <see cref="ElmMakeReport"/>, routing based on the <c>type</c> field.
/// </summary>
public class ElmMakeReportConverter : JsonConverter<ElmMakeReport>
{
    /// <summary>
    /// Pre-configured serializer options suitable for Elm make reports (camelCase, union converters registered).
    /// </summary>
    public static readonly JsonSerializerOptions SerializerOptions =
        new()
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            Converters = { new ElmMakeReportConverter(), new MessageItemConverter(), new MessageItemArrayConverter() }
        };

    /// <summary>
    /// Deserializes a JSON string produced by <c>elm make --report=json</c> into an <see cref="ElmMakeReport"/>.
    /// </summary>
    /// <param name="json">The JSON payload to deserialize.</param>
    /// <exception cref="JsonException">Thrown when deserialization fails.</exception>
    public static ElmMakeReport Deserialize(string json) =>
        JsonSerializer.Deserialize<ElmMakeReport>(json, SerializerOptions) ??
        throw new JsonException("Failed to deserialize ElmMakeReport.");

    /// <inheritdoc />
    public override ElmMakeReport? Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        using var doc = JsonDocument.ParseValue(ref reader);
        var root = doc.RootElement;

        if (!root.TryGetProperty("type", out var typeProperty))
        {
            throw new JsonException("No 'type' property found.");
        }

        var typeVal =
            typeProperty.GetString()
            ??
            throw new JsonException("The 'type' property is null.");

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

    /// <inheritdoc />
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

/// <summary>
/// JSON converter for arrays of <see cref="MessageItem"/> where elements can be either strings or objects.
/// </summary>
public class MessageItemArrayConverter : JsonConverter<IReadOnlyList<MessageItem>>
{
    /// <inheritdoc />
    public override IReadOnlyList<MessageItem> Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        var list = new List<MessageItem>();

        if (reader.TokenType is not JsonTokenType.StartArray)
            throw new JsonException("Expected start of array.");

        while (reader.Read())
        {
            if (reader.TokenType is JsonTokenType.EndArray)
                break;

            list.Add(JsonSerializer.Deserialize<MessageItem>(ref reader, options)!);
        }

        return list;
    }

    /// <inheritdoc />
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

/// <summary>
/// Polymorphic converter for <see cref="MessageItem"/> supporting string and styled object representations.
/// </summary>
public class MessageItemConverter : JsonConverter<MessageItem>
{
    /// <inheritdoc />
    public override MessageItem? Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType is JsonTokenType.String)
        {
            var str = reader.GetString()!;

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

    /// <summary>
    /// Writing is not implemented because serialization is not used in current workflows.
    /// </summary>
    /// <exception cref="NotImplementedException">Always thrown.</exception>
    public override void Write(Utf8JsonWriter writer, MessageItem value, JsonSerializerOptions options)
    {
        throw new NotImplementedException();
    }
}
