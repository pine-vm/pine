using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Json;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>Lossless JSON for resolved nodes and structural identities. Generated names are never used as encodings.</summary>
public static class CompilerModelJson
{
    /// <summary>Builds options for immutable model values and their source-name and Pine-value leaves.</summary>
    public static JsonSerializerOptions CreateOptions()
    {
        var options = ElmSyntaxAbstractJson.BuildJsonSerializerOptions();
        options.Converters.Add(new ClosedRecordConverter());
        options.Converters.Add(new StructuralTypeConverter());
        return options;
    }

    private sealed class StructuralTypeConverter : JsonConverter<StructuralType>
    {
        private readonly JsonConverterForChoiceType<StructuralType> _converter = new();

        public override StructuralType Read(
            ref Utf8JsonReader reader,
            Type typeToConvert,
            JsonSerializerOptions options) =>
            _converter.Read(ref reader, typeToConvert, options);

        public override void Write(Utf8JsonWriter writer, StructuralType value, JsonSerializerOptions options) =>
            _converter.Write(writer, value, options);
    }

    private sealed class ClosedRecordConverter : JsonConverter<StructuralType.ClosedRecord>
    {
        public override StructuralType.ClosedRecord Read(
            ref Utf8JsonReader reader,
            Type typeToConvert,
            JsonSerializerOptions options)
        {
            var result = JsonSerializer.Deserialize<StructuralType>(ref reader, options);

            return
                result is StructuralType.ClosedRecord record
                ?
                StructuralType.ClosedRecord.Create(record.Fields)
                :
                throw new JsonException("Expected a closed-record constraint.");
        }

        public override void Write(
            Utf8JsonWriter writer,
            StructuralType.ClosedRecord value,
            JsonSerializerOptions options) =>
            JsonSerializer.Serialize<StructuralType>(writer, value, options);
    }
}

/// <summary>Encodes program maps as entry sequences, preserving structural keys rather than stringifying identities.</summary>
public sealed class ProgramJsonConverter : JsonConverter<Program>
{
    private sealed record FunctionEntry(FunctionId Id, Function Function);

    private sealed record SourceEntry(CodeAnalysis.DeclQualifiedName Name, Declaration Declaration);

    private sealed record BindingEntry(BindingId Id, string Name);

    private sealed record Document(FunctionEntry[] Functions, SourceEntry[] SourceDeclarations, BindingEntry[] BindingNames);

    /// <inheritdoc/>
    public override Program Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        var document =
            JsonSerializer.Deserialize<Document>(ref reader, options) ??
            throw new JsonException("Expected a compiler program.");

        if (document.Functions is null || document.SourceDeclarations is null || document.BindingNames is null)
            throw new JsonException("A compiler program must provide all declaration and provenance collections.");

        return
            new(
                Entries(document.Functions.Select(entry => KeyValuePair.Create(entry.Id, entry.Function))),
                Entries(
                    document.SourceDeclarations.Select(entry => KeyValuePair.Create(entry.Name, entry.Declaration))))
            {
                BindingNames = Entries(document.BindingNames.Select(entry => KeyValuePair.Create(entry.Id, entry.Name)))
            };
    }

    private static ImmutableDictionary<TKey, TValue> Entries<TKey, TValue>(
        IEnumerable<KeyValuePair<TKey, TValue>> entries)
        where TKey : notnull
    {
        var builder = ImmutableDictionary.CreateBuilder<TKey, TValue>();

        foreach (var (key, value) in entries)
        {
            if (key is null || value is null || !builder.TryAdd(key, value))
                throw new JsonException("Program entries must have non-null, unique identities and non-null values.");
        }

        return builder.ToImmutable();
    }

    /// <inheritdoc/>
    public override void Write(Utf8JsonWriter writer, Program value, JsonSerializerOptions options) =>
        JsonSerializer.Serialize(
            writer,
            new Document(
                [
                .. value.Functions.OrderBy(entry => entry.Key, IdentityComparer.Instance).Select(
                    entry => new FunctionEntry(entry.Key, entry.Value))
                ],
                [
                .. value.SourceDeclarations.OrderBy(entry => entry.Key).Select(
                    entry => new SourceEntry(entry.Key, entry.Value))
                ],
                [
                .. value.BindingNames.OrderBy(entry => entry.Key, IdentityComparer.Instance).Select(
                    entry => new BindingEntry(entry.Key, entry.Value))
                ]),
            options);
}

/// <summary>Round-trip support for immutable, structurally compared compiler sequences.</summary>
public sealed class ValueListJsonConverterFactory : JsonConverterFactory
{
    /// <inheritdoc/>
    public override bool CanConvert(Type typeToConvert) =>
        typeToConvert.IsGenericType && typeToConvert.GetGenericTypeDefinition() == typeof(ValueList<>);

    /// <inheritdoc/>
    public override JsonConverter CreateConverter(Type typeToConvert, JsonSerializerOptions options) =>
        (JsonConverter)(Activator.CreateInstance(typeof(Converter<>).MakeGenericType(typeToConvert.GetGenericArguments()))
            ?? throw new InvalidOperationException("Unable to construct immutable-sequence JSON converter."));

    private sealed class Converter<T> : JsonConverter<ValueList<T>>
    {
        public override ValueList<T> Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) =>
            new(JsonSerializer.Deserialize<List<T>>(ref reader, options) ?? throw new JsonException("Expected a sequence."));

        public override void Write(Utf8JsonWriter writer, ValueList<T> value, JsonSerializerOptions options)
        {
            writer.WriteStartArray();

            foreach (var item in value)
                JsonSerializer.Serialize(writer, item, options);

            writer.WriteEndArray();
        }
    }
}
