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

        if (sourceDirectory == ".")
            return initialRecord;

        sourceDirectory = sourceDirectory.Replace('\\', '/');

        sourceDirectory = sourceDirectory.StartsWith("./") ? sourceDirectory[2..] : sourceDirectory;

        var segmentsStrings = sourceDirectory.Split('/');

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

