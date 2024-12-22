using System.Collections.Generic;
using System.Linq;
using System.Text.Json.Serialization;

namespace Pine.Elm019;

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

public record ElmJsonStructure(
    [property: JsonPropertyName("type")]
    string Type,
    [property: JsonPropertyName("source-directories")]
    IReadOnlyList<string> SourceDirectories,
    [property: JsonPropertyName("elm-version")]
    string ElmVersion,
    [property: JsonPropertyName("dependencies")]
    ElmJsonStructure.DependenciesStruct Dependencies)
{
    public IEnumerable<RelativeDirectory> ParsedSourceDirectories =>
        SourceDirectories.Select(ParseSourceDirectory);

    public static RelativeDirectory ParseSourceDirectory(string sourceDirectory)
    {
        var initialRecord = new RelativeDirectory(ParentLevel: 0, Subdirectories: []);

        sourceDirectory = sourceDirectory.Replace('\\', '/');

        sourceDirectory = sourceDirectory.StartsWith("./") ? sourceDirectory[2..] : sourceDirectory;

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

    public record RelativeDirectory(
        int ParentLevel,
        IReadOnlyList<string> Subdirectories);

    public record DependenciesStruct(
        [property: JsonPropertyName("direct")]
        Dictionary<string, string> Direct,
        [property: JsonPropertyName("indirect")]
        Dictionary<string, string> Indirect);
}

