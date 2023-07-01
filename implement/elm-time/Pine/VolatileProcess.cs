using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;

namespace Pine;

public class VolatileProcess
{
    public static Result<string, ReadOnlyMemory<byte>> LoadBlob(
        ConcurrentDictionary<string, IEnumerable<TreeNodeWithStringPath>> loadedTreesFromUrl,
        Func<byte[], byte[]?>? getFileFromHashSHA256,
        string hashSha256Base16,
        IEnumerable<string> hintUrls)
    {
        var hash = CommonConversion.ByteArrayFromStringBase16(hashSha256Base16);

        IReadOnlyDictionary<string, string>? errorFromHintUrl = null;

        var executableFileFromCacheOrLink =
            BlobLibrary.GetBlobWithSHA256Cached(
            hash,
            getIfNotCached: () =>
            {
                if (hintUrls is null)
                    return null;

                return
                DependenciesLoader.GetBlobFromHashAndHintUrlsCached(loadedTreesFromUrl, hash, hintUrls)
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

        var blob = executableFileFromCacheOrLink?.ToArray() ?? getFileFromHashSHA256?.Invoke(hash);

        if (blob is null)
            return returnError("Did not find blob");

        if (!PineValueHashTree.ComputeHash(PineValue.Blob(blob)).Span.SequenceEqual(hash) &&
            !CommonConversion.HashSHA256(blob).Span.SequenceEqual(hash))
            return returnError("Selected blob hash does not match " + hashSha256Base16);

        return
            Result<string, ReadOnlyMemory<byte>>.ok(blob);
    }
}
