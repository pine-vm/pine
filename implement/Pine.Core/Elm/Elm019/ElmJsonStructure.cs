using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace Pine.Core.Elm.Elm019;

/*
{
    "type": "application",
    "source-directories": [
        "src",
        "elm-syntax/src",
        "elm-syntax-encode-json/src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "cmditch/elm-bigint": "2.0.1",
            "danfishgold/base64-bytes": "1.1.0",
            "elm/bytes": "1.0.8",
            "elm/core": "1.0.5",
            "elm/json": "1.1.3",
            "elm/parser": "1.1.0",
            "elm-community/result-extra": "2.4.0",
            "folkertdev/elm-sha2": "1.0.0"
        },
        "indirect": {
            "elm/regex": "1.0.0",
            "elm-community/list-extra": "8.7.0",
            "elm-community/maybe-extra": "5.3.0",
            "rtfeldman/elm-hex": "1.0.0"
        }
    },
    "test-dependencies": {
        "direct": {
            "elm-explorations/test": "2.2.0"
        },
        "indirect": {
            "elm/html": "1.0.0",
            "elm/random": "1.0.0",
            "elm/time": "1.0.0",
            "elm/virtual-dom": "1.0.3"
        }
    }
}

 * */

/*
{
    "type": "package",
    "name": "stil4m/elm-syntax",
    "summary": "Elm Syntax in Elm: for parsing and writing Elm in Elm",
    "license": "MIT",
    "version": "7.3.8",
    "exposed-modules": [
        "Elm.Dependency",
        "Elm.Interface",
        "Elm.Parser",
        "Elm.Processing",
        "Elm.RawFile",
        "Elm.Writer",
        "Elm.Syntax.Comments",
        "Elm.Syntax.Declaration",
        "Elm.Syntax.Documentation",
        "Elm.Syntax.Exposing",
        "Elm.Syntax.Expression",
        "Elm.Syntax.File",
        "Elm.Syntax.Import",
        "Elm.Syntax.Infix",
        "Elm.Syntax.Module",
        "Elm.Syntax.ModuleName",
        "Elm.Syntax.Node",
        "Elm.Syntax.Pattern",
        "Elm.Syntax.Range",
        "Elm.Syntax.Signature",
        "Elm.Syntax.TypeAlias",
        "Elm.Syntax.TypeAnnotation",
        "Elm.Syntax.Type"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0",
        "elm/json": "1.0.0 <= v < 2.0.0",
        "elm/parser": "1.0.0 <= v < 2.0.0",
        "rtfeldman/elm-hex": "1.0.0 <= v < 2.0.0",
        "stil4m/structured-writer": "1.0.1 <= v < 2.0.0"
    },
    "test-dependencies": {
        "elm-explorations/test": "2.0.0 <= v < 3.0.0"
    }
}
 * */

/*
{
    "type": "package",
    "name": "elm/core",
    "summary": "Elm's standard libraries",
    "license": "BSD-3-Clause",
    "version": "1.0.5",
    "exposed-modules": {
        "Primitives": [
            "Basics",
            "String",
            "Char",
            "Bitwise",
            "Tuple"
        ],
        "Collections": [
            "List",
            "Dict",
            "Set",
            "Array"
        ],
        "Error Handling": [
            "Maybe",
            "Result"
        ],
        "Debug": [
            "Debug"
        ],
        "Effects": [
            "Platform.Cmd",
            "Platform.Sub",
            "Platform",
            "Process",
            "Task"
        ]
    },
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {},
    "test-dependencies": {}
}

 * */

/// <summary>
/// Represents the parsed contents of an <c>elm.json</c> file for Elm 0.19.x applications and packages.
/// Handles both application and package shapes and supports flexible dependency and exposed-module layouts.
/// </summary>
/// <param name="Type">The document type, e.g. <c>application</c> or <c>package</c>.</param>
/// <param name="Name">The package name (only for package type).</param>
/// <param name="Summary">A short description of the app or package.</param>
/// <param name="License">The SPDX license identifier.</param>
/// <param name="Version">The version string (only for package type).</param>
/// <param name="ExposedModules">List of exposed module names; accepts both array and sectioned object shapes.</param>
/// <param name="SourceDirectories">List of source directories (application type).</param>
/// <param name="ElmVersion">Elm version range or exact version required.</param>
/// <param name="Dependencies">Dependency table supporting direct/indirect or flat forms.</param>
public record ElmJsonStructure(
    [property: JsonPropertyName("type")]
    string Type,
    [property: JsonPropertyName("name")]
    string Name,
    [property: JsonPropertyName("summary")]
    string Summary,
    [property: JsonPropertyName("license")]
    string License,
    [property: JsonPropertyName("version")]
    string Version,
    [property: JsonPropertyName("exposed-modules")]
    [property: JsonConverter(typeof(ExposedModulesConverter))]
    IReadOnlyList<string> ExposedModules,
    [property: JsonPropertyName("source-directories")]
    IReadOnlyList<string> SourceDirectories,
    [property: JsonPropertyName("elm-version")]
    string ElmVersion,
    [property: JsonPropertyName("dependencies")]
    [property: JsonConverter(typeof(DependenciesConverter))]
    ElmJsonStructure.DependenciesStruct Dependencies)
{
    /// <summary>
    /// Enumerates source directories parsed into a relative-directory structure with parent traversal depth.
    /// </summary>
    public IEnumerable<RelativeDirectory> ParsedSourceDirectories =>
        SourceDirectories.Select(ParseSourceDirectory);

    /// <summary>
    /// Parses a source directory string from <c>elm.json</c> into a <see cref="RelativeDirectory"/>.
    /// Handles <c>./</c>, <c>..</c>, and path separators.
    /// </summary>
    /// <param name="sourceDirectory">A source directory path as written in <c>elm.json</c>.</param>
    /// <returns>A normalized relative-directory record.</returns>
    public static RelativeDirectory ParseSourceDirectory(string sourceDirectory)
    {
        var initialRecord = new RelativeDirectory(ParentLevel: 0, Subdirectories: []);

        sourceDirectory =
            sourceDirectory.Replace('\\', '/');

        sourceDirectory =
            sourceDirectory.StartsWith("./") ? sourceDirectory[2..] : sourceDirectory;

        var segmentsStrings =
            sourceDirectory
            .Split('/')
            .SkipWhile(segment => segment is "");

        return
            segmentsStrings
                .Aggregate(
                    seed: initialRecord,
                    func: (aggregate, nextSegment) =>
                        nextSegment switch
                        {
                            ".." =>
                                0 < aggregate.Subdirectories.Count ?
                                    aggregate with
                                    {
                                        Subdirectories = [.. aggregate.Subdirectories.SkipLast(1)]
                                    }
                                    :
                                    aggregate with
                                    {
                                        ParentLevel = aggregate.ParentLevel + 1
                                    },

                            "." =>
                            aggregate,

                            _ =>
                                aggregate with
                                {
                                    Subdirectories = [.. aggregate.Subdirectories, nextSegment]
                                }
                        });
    }

    /// <summary>
    /// A relative directory path expressed as the number of levels to traverse up from a base directory and
    /// the remaining subdirectory segments.
    /// </summary>
    /// <param name="ParentLevel">Number of <c>..</c> traversals above the base directory.</param>
    /// <param name="Subdirectories">Remaining subdirectory segments after applying <c>ParentLevel</c>.</param>
    public record RelativeDirectory(
        int ParentLevel,
        IReadOnlyList<string> Subdirectories);

    /// <summary>
    /// Represents dependencies declared in <c>elm.json</c>. Supports the application shape with
    /// <c>direct</c>/<c>indirect</c> sections and the package shape as a flat dictionary.
    /// </summary>
    /// <param name="Direct">Direct dependencies (application shape).</param>
    /// <param name="Indirect">Indirect dependencies (application shape).</param>
    /// <param name="Flat">Flat dependency dictionary (package shape, or additional keys).</param>
    public record DependenciesStruct(
        [property: JsonPropertyName("direct")]
        IReadOnlyDictionary<string, string>? Direct,
        [property: JsonPropertyName("indirect")]
        IReadOnlyDictionary<string, string>? Indirect,
        [property: JsonPropertyName("flat")]
        IReadOnlyDictionary<string, string>? Flat);
}

/// <summary>
/// JSON converter supporting both array and sectioned object shapes for <c>exposed-modules</c>.
/// </summary>
public class ExposedModulesConverter : JsonConverter<IReadOnlyList<string>>
{
    /// <summary>
    /// Reads the <c>exposed-modules</c> value as either an array of strings or a dictionary of sections.
    /// </summary>
    public override IReadOnlyList<string> Read(ref Utf8JsonReader reader, System.Type typeToConvert, JsonSerializerOptions options)
    {
        if (reader.TokenType == JsonTokenType.StartArray)
        {
            var list =
                JsonSerializer.Deserialize<List<string>>(ref reader, options);

            return list ?? [];
        }

        if (reader.TokenType == JsonTokenType.StartObject)
        {
            var sections =
                JsonSerializer.Deserialize<Dictionary<string, List<string>>>(ref reader, options);

            return sections?.Values.SelectMany(v => v).ToList() ?? [];
        }

        throw new JsonException("Unsupported shape for exposed-modules.");
    }

    /// <summary>
    /// Writes the list of exposed modules as a simple JSON array.
    /// </summary>
    public override void Write(Utf8JsonWriter writer, IReadOnlyList<string> value, JsonSerializerOptions options)
    {
        JsonSerializer.Serialize(writer, value, options);
    }
}

/// <summary>
/// JSON converter that accepts both the application dependency shape (with <c>direct</c>/<c>indirect</c>)
/// and the package shape (flat dictionary). Any extra keys become part of <c>Flat</c>.
/// </summary>
public class DependenciesConverter : JsonConverter<ElmJsonStructure.DependenciesStruct>
{
    /// <summary>
    /// Reads dependency tables, normalizing into <see cref="ElmJsonStructure.DependenciesStruct"/>.
    /// </summary>
    /// <exception cref="JsonException">Thrown when the JSON does not represent an object.</exception>
    public override ElmJsonStructure.DependenciesStruct Read(
        ref Utf8JsonReader reader,
        System.Type typeToConvert,
        JsonSerializerOptions options)
    {
        if (reader.TokenType != JsonTokenType.StartObject)
            throw new JsonException("Expected object for dependencies.");

        var direct = new Dictionary<string, string>();
        var indirect = new Dictionary<string, string>();
        var flat = new Dictionary<string, string>();

        using var jsonDoc = JsonDocument.ParseValue(ref reader);
        var root = jsonDoc.RootElement;

        // Check if shape has "direct"/"indirect" or is just a flat dictionary
        var hasDirect = root.TryGetProperty("direct", out var directElement);
        var hasIndirect = root.TryGetProperty("indirect", out var indirectElement);

        if (hasDirect || hasIndirect)
        {
            if (hasDirect)
                direct = JsonSerializer.Deserialize<Dictionary<string, string>>(directElement.GetRawText(), options) ?? [];

            if (hasIndirect)
                indirect = JsonSerializer.Deserialize<Dictionary<string, string>>(indirectElement.GetRawText(), options) ?? [];

            // Anything else becomes "flat"
            foreach (var property in root.EnumerateObject())
            {
                if (property.Name is not "direct" and not "indirect")
                    flat[property.Name] = property.Value.GetString() ?? "";
            }
        }
        else
        {
            // Entire object is "flat"
            flat = JsonSerializer.Deserialize<Dictionary<string, string>>(root.GetRawText(), options) ?? [];
        }

        return new ElmJsonStructure.DependenciesStruct(direct, indirect, flat);
    }

    /// <summary>
    /// Writes dependency tables, emitting <c>direct</c> and <c>indirect</c> when present and writing any <c>Flat</c>
    /// entries as top-level properties.
    /// </summary>
    public override void Write(
        Utf8JsonWriter writer,
        ElmJsonStructure.DependenciesStruct value,
        JsonSerializerOptions options)
    {
        writer.WriteStartObject();

        if (value.Direct is { } direct)
        {
            writer.WritePropertyName("direct");
            JsonSerializer.Serialize(writer, direct, options);
        }

        if (value.Indirect is { } indirect)
        {
            writer.WritePropertyName("indirect");
            JsonSerializer.Serialize(writer, indirect, options);
        }

        if (value.Flat is { } flat)
        {
            foreach (var kvp in flat)
            {
                writer.WritePropertyName(kvp.Key);
                writer.WriteStringValue(kvp.Value);
            }
        }

        writer.WriteEndObject();
    }
}

