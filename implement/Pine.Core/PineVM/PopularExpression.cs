using Pine.Core;
using Pine.Core.DotNet;
using Pine.Core.Json;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;

namespace Pine.PineVM;

/// <summary>
/// Loads curated embedded expressions and values that are reused as popular instances.
/// </summary>
public class PopularExpression
{
    static readonly JsonSerializerOptions jsonSerializerOptions =
        EncodePineExpressionAsJson.BuildJsonSerializerOptions();

    /// <summary>
    /// Builds a name-to-expression dictionary from embedded JSON expression files, reusing canonical expression instances.
    /// </summary>
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

                        return
                            aggregate.SetItem(
                                exprName,
                                Expression.EnsureReuseInstanceGeneral(expression));
                    }

                    return aggregate;
                },
                seed: ImmutableDictionary<string, Expression>.Empty);
    }

    /// <summary>
    /// Builds a name-to-value dictionary from embedded JSON Pine value files.
    /// </summary>
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

    /// <summary>
    /// Loads the embedded files that define the popular expression set.
    /// </summary>
    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadPopularExpressionFiles() =>
        DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ["PineVM", "PopularExpression"],
            assembly: typeof(PopularExpression).Assembly);

    /// <summary>
    /// Loads the embedded files that define the popular Pine value set.
    /// </summary>
    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadPopularValueFiles() =>
        DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ["PineVM", "PopularValue"],
            assembly: typeof(PopularExpression).Assembly);

}
