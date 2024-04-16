using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;

namespace Pine;

public class DependenciesLoader
{
    public static Result<string, ReadOnlyMemory<byte>> ResolveHashReferenceWithoutCache(
    ConcurrentDictionary<string, IEnumerable<TreeNodeWithStringPath>> loadedTreesFromUrl,
    string hashBase16,
    IEnumerable<string> hintUrls,
    Func<byte[], byte[]?>? getFileFromHashSHA256)
    {
        var hash = CommonConversion.ByteArrayFromStringBase16(hashBase16);

        IReadOnlyDictionary<string, string>? errorFromHintUrl = null;

        var assemblyFromCacheOrLink =
            BlobLibrary.GetBlobWithSHA256Cached(
            hash,
            getIfNotCached: () =>
            {
                if (hintUrls is null)
                    return null;

                return
                GetBlobFromHashAndHintUrlsCached(loadedTreesFromUrl, hash, hintUrls)
                .Unpack(
                    fromErr: err =>
                    {
                        errorFromHintUrl = err;

                        return null;
                    },
                    fromOk: ok => ok);
            });

        Result<string, ReadOnlyMemory<byte>> returnError(string? error)
        {
            var errorFromDictionary =
                errorFromHintUrl is null ? null
                :
                "Failed loading from " + errorFromHintUrl.Count + " hint URL(s):\n" +
                string.Join("\n", errorFromHintUrl.Select(hintUrlAndError => hintUrlAndError.Key + ": " + hintUrlAndError.Value));

            return
                Result<string, ReadOnlyMemory<byte>>.err(
                    string.Join("\n", new[] { error, errorFromDictionary }.WhereNotNull()));
        }

        var assembly = assemblyFromCacheOrLink?.ToArray() ?? getFileFromHashSHA256?.Invoke(hash);

        if (assembly is null)
            return returnError("Did not find assembly image");

        if (!PineValueHashTree.ComputeHash(PineValue.Blob(assembly)).Span.SequenceEqual(hash) &&
            !CommonConversion.HashSHA256(assembly).Span.SequenceEqual(hash))
            return returnError("Selected assembly image hash does not match " + hashBase16);

        return
            Result<string, ReadOnlyMemory<byte>>.ok(assembly);
    }

    public static Result<IReadOnlyDictionary<string, string>, ReadOnlyMemory<byte>> GetBlobFromHashAndHintUrlsCached(
        ConcurrentDictionary<string, IEnumerable<TreeNodeWithStringPath>> loadedTreesFromUrl,
        byte[] hash, IEnumerable<string> hintUrls)
    {
        Result<string, ReadOnlyMemory<byte>> AttemptForUrl(string url)
        {
            if (!loadedTreesFromUrl.TryGetValue(url, out var treesFromUrl))
            {
                try
                {
                    treesFromUrl =
                    loadedTreesFromUrl.GetOrAdd(
                        url,
                        url => BlobLibrary.DownloadFromUrlAndExtractTrees(url).ToImmutableList());
                }
                catch (Exception e)
                {
                    Result<string, ReadOnlyMemory<byte>>.err("Loading from URL failed with runtime exception: " + e);
                }
            }

            if (treesFromUrl is null || !treesFromUrl.Any())
                return Result<string, ReadOnlyMemory<byte>>.err("Found no trees at that URL");

            var searchTreesResult =
                treesFromUrl
                .Aggregate(
                    seed:
                    Result<ImmutableList<TreeNodeWithStringPath>, ReadOnlyMemory<byte>>.err([]),
                    func:
                    (aggregate, tree) =>
                    {
                        return
                        aggregate
                        .Unpack(
                            fromErr:
                            err =>
                            {
                                var matchingBlob =
                                    tree.EnumerateBlobsTransitive()
                                    .Select(blobWithPath => blobWithPath.blobContent)
                                    .Where(BlobLibrary.BlobHasSHA256(hash))
                                    .Cast<ReadOnlyMemory<byte>?>()
                                    .FirstOrDefault();

                                if (matchingBlob != null)
                                {
                                    return Result<ImmutableList<TreeNodeWithStringPath>, ReadOnlyMemory<byte>>.ok(matchingBlob.Value);
                                }

                                return
                                Result<ImmutableList<TreeNodeWithStringPath>, ReadOnlyMemory<byte>>.err(err.Add(tree));
                            },
                            fromOk:
                            ok => Result<ImmutableList<TreeNodeWithStringPath>, ReadOnlyMemory<byte>>.ok(ok));
                    });

            return
                searchTreesResult
                .MapError(
                    searchedTrees =>
                    "Searched " + searchedTrees.Count + " tree nodes but none of those contained a matching blob:\n" +
                    string.Join("\n",
                    searchedTrees.Select((tree, treeIndex) => "Node " + treeIndex + " " +
                    CommonConversion.StringBase16(PineValueHashTree.ComputeHashNotSorted(tree)) + " " +
                    DescribeBlobOrTreeContentsForErrorMessage(tree))).Trim('\n'));
        }

        return
            hintUrls
            .Aggregate(
                seed: Result<ImmutableDictionary<string, string>, ReadOnlyMemory<byte>>.err(ImmutableDictionary<string, string>.Empty),
                func: (aggregate, hintUrl) =>
                {
                    return
                    aggregate.Unpack(
                        fromErr:
                        errorFromHintUrl =>
                        AttemptForUrl(hintUrl).MapError(err => errorFromHintUrl.SetItem(hintUrl, err)),

                        fromOk:
                        ok => Result<ImmutableDictionary<string, string>, ReadOnlyMemory<byte>>.ok(ok));
                })
            .MapError(dict => (IReadOnlyDictionary<string, string>)dict);
    }

    public static string DescribeBlobOrTreeContentsForErrorMessage(TreeNodeWithStringPath tree) =>
    tree switch
    {
        TreeNodeWithStringPath.BlobNode => "is a blob",

        _ => "is a tree:\n" + DescribeTreeContentsForErrorMessage(tree)
    };

    public static string DescribeTreeContentsForErrorMessage(TreeNodeWithStringPath tree) =>
        string.Join("\n",
            tree.EnumerateBlobsTransitive().Select(blobAtPath =>
            "Found " +
            CommonConversion.StringBase16(SHA256.HashData(blobAtPath.blobContent.Span)) +
            " at " + string.Join("/", blobAtPath.path)));
}
