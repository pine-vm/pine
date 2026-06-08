using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using System;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// JSON serialization and deserialization for <see cref="ElmSyntaxInterpreter.Prepared"/>.
/// <para>
/// This is used to persist a prepared, canonicalized and lowered representation of the declarations
/// of an Elm app, so that the (expensive) preparation does not need to be repeated on every run.
/// </para>
/// <para>
/// The declaration values reuse the same encoding as <see cref="ElmSyntaxAbstractJson"/>. The
/// dictionary keys (<see cref="DeclQualifiedName"/>) are encoded as their
/// <see cref="DeclQualifiedName.FullName"/> string (see <see cref="DeclQualifiedNameJsonConverter"/>).
/// </para>
/// </summary>
public static class ElmSyntaxInterpreterPreparedJson
{
    private static readonly JsonSerializerOptions s_jsonSerializerOptions = BuildJsonSerializerOptions();

    /// <summary>
    /// The shared <see cref="JsonSerializerOptions"/> used for <see cref="ElmSyntaxInterpreter.Prepared"/>.
    /// </summary>
    public static JsonSerializerOptions JsonSerializerOptions => s_jsonSerializerOptions;

    /// <summary>
    /// Builds the <see cref="JsonSerializerOptions"/> used to serialize and deserialize
    /// <see cref="ElmSyntaxInterpreter.Prepared"/>.
    /// </summary>
    public static JsonSerializerOptions BuildJsonSerializerOptions()
    {
        var options = ElmSyntaxAbstractJson.BuildJsonSerializerOptions();

        options.Converters.Add(new DeclQualifiedNameJsonConverter());

        return options;
    }

    /// <summary>
    /// Serializes the given <see cref="ElmSyntaxInterpreter.Prepared"/> to a JSON string.
    /// </summary>
    public static string ToJsonString(ElmSyntaxInterpreter.Prepared prepared) =>
        JsonSerializer.Serialize(prepared, s_jsonSerializerOptions);

    /// <summary>
    /// Deserializes an <see cref="ElmSyntaxInterpreter.Prepared"/> from the given JSON string.
    /// </summary>
    public static ElmSyntaxInterpreter.Prepared FromJsonString(string json) =>
        JsonSerializer.Deserialize<ElmSyntaxInterpreter.Prepared>(json, s_jsonSerializerOptions)
        ?? throw new JsonException("Decoded a null Prepared from JSON.");
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
