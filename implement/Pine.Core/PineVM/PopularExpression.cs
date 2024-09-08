using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;

namespace Pine.PineVM;

public class PopularExpression
{
    static readonly JsonSerializerOptions jsonSerializerOptions = EncodePineExpressionAsJson.BuildJsonSerializerOptions();

    public static IImmutableDictionary<string, Expression> BuildPopularExpressionDictionary()
    {
        var exprFiles =
            LoadPopularExpressionFiles()
            .Extract(err => throw new Exception(err));

        return
            exprFiles
            .Aggregate(
                func: (aggregate, nextFile) =>
                {
                    if (nextFile.Key.Count is not 1)
                        return aggregate;

                    if (nextFile.Key[0].EndsWith(".json", StringComparison.OrdinalIgnoreCase))
                    {
                        var json = Encoding.UTF8.GetString(nextFile.Value.Span);

                        var expression = EncodePineExpressionAsJson.SingleFromJsonString(json);

                        var exprName = nextFile.Key[0][..^5];

                        return aggregate.SetItem(
                            exprName,
                            Expression.EnsureReuseInstanceGeneral(expression));
                    }

                    return aggregate;
                },
                seed: ImmutableDictionary<string, Expression>.Empty);
    }

    public static IImmutableDictionary<string, PineValue> BuildPopularValueDictionary()
    {
        var exprFiles =
            LoadPopularValueFiles()
            .Extract(err => throw new Exception(err));

        return
            exprFiles
            .Aggregate(
                func: (aggregate, nextFile) =>
                {
                    if (nextFile.Key.Count is not 1)
                        return aggregate;

                    if (nextFile.Key[0].EndsWith(".json", StringComparison.OrdinalIgnoreCase))
                    {
                        var json = Encoding.UTF8.GetString(nextFile.Value.Span);

                        var pineValue = JsonSerializer.Deserialize<PineValue>(json, jsonSerializerOptions)!;

                        var pineValueName = nextFile.Key[0][..^5];

                        return aggregate.SetItem(pineValueName, pineValue);
                    }

                    return aggregate;
                },
                seed: ImmutableDictionary<string, PineValue>.Empty);
    }

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadPopularExpressionFiles() =>
        DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ["PineVM", "PopularExpression"],
            assembly: typeof(PopularExpression).Assembly);

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadPopularValueFiles() =>
        DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ["PineVM", "PopularValue"],
            assembly: typeof(PopularExpression).Assembly);

}
