using System.Collections.Generic;
using System.Linq;
using System.Text.Json.Serialization;

namespace ElmTime.Elm019;

public record ElmJsonStructure(
    string type,
    [property: JsonPropertyName("source-directories")]
    IReadOnlyList<string> sourceDirectories)
{
    public IEnumerable<RelativeDirectory> ParsedSourceDirectories =>
        sourceDirectories.Select(ParseSourceDirectory);

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
}

