using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using Pine.Core.Internal;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;

using AbstractDeclaration = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Declaration;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// JSON serialization and deserialization for <see cref="ElmSyntaxInterpreter.Prepared"/>.
/// <para>
/// This is used to persist a prepared, canonicalized and lowered representation of the declarations
/// of an Elm app, so that the (expensive) preparation does not need to be repeated on every run.
/// </para>
/// <para>
/// The current serialized form stores <see cref="ElmSyntaxInterpreter.PreparedDeclaration"/> values.
/// For backward compatibility, deserialization also accepts legacy payloads whose declaration values
/// are <see cref="Declaration"/> nodes and prepares them on load. The dictionary keys
/// (<see cref="DeclQualifiedName"/>) are encoded as their <see cref="DeclQualifiedName.FullName"/>
/// string (see <see cref="DeclQualifiedNameJsonConverter"/>).
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
        options.Converters.Add(new PineValueInProcessJsonConverter());

        return options;
    }

    /// <summary>
    /// Serializes the given <see cref="ElmSyntaxInterpreter.Prepared"/> to a JSON string.
    /// </summary>
    public static string ToJsonString(ElmSyntaxInterpreter.Prepared prepared) =>
        JsonSerializer.Serialize(prepared, s_jsonSerializerOptions);

    /// <summary>
    /// Deserializes an <see cref="ElmSyntaxInterpreter.Prepared"/> from the given JSON string,
    /// accepting both the current prepared format and the legacy abstract-declaration format.
    /// </summary>
    public static ElmSyntaxInterpreter.Prepared FromJsonString(string json)
    {
        try
        {
            return
                JsonSerializer.Deserialize<ElmSyntaxInterpreter.Prepared>(json, s_jsonSerializerOptions)
                ?? throw new JsonException("Decoded a null Prepared from JSON.");
        }
        catch (JsonException)
        {
            if (TryDeserializeLegacyPrepared(json) is { } legacyPrepared)
            {
                return legacyPrepared;
            }

            throw;
        }
    }

    private static ElmSyntaxInterpreter.Prepared? TryDeserializeLegacyPrepared(string json)
    {
        try
        {
            var legacyPrepared =
                JsonSerializer.Deserialize<LegacyPrepared>(json, s_jsonSerializerOptions);

            return legacyPrepared is null ? null : new ElmSyntaxInterpreter.Prepared(legacyPrepared.Declarations);
        }
        catch (JsonException)
        {
            return null;
        }
    }

    private sealed record LegacyPrepared(
        IReadOnlyDictionary<DeclQualifiedName, AbstractDeclaration> Declarations);
}

internal sealed class PineValueInProcessJsonConverter : JsonConverter<PineValueInProcess>
{
    public override PineValueInProcess Read(
        ref Utf8JsonReader reader,
        System.Type typeToConvert,
        JsonSerializerOptions options)
    {
        var pineValue =
            JsonSerializer.Deserialize<PineValue>(ref reader, options)
            ?? throw new JsonException("Decoded a null PineValue for PineValueInProcess.");

        return PineValueInProcess.CreateFullyRepresented(pineValue);
    }

    public override void Write(
        Utf8JsonWriter writer,
        PineValueInProcess value,
        JsonSerializerOptions options)
    {
        JsonSerializer.Serialize(writer, value.Evaluate(), options);
    }
}
