using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Runtime.InteropServices;
using System.Threading.Tasks;

namespace Pine;

public class BlobLibrary
{
    static public Func<Func<ReadOnlyMemory<byte>, ReadOnlyMemory<byte>?>, Func<ReadOnlyMemory<byte>, ReadOnlyMemory<byte>?>>? OverrideGetBlobWithSHA256;

    static readonly string cacheDirectory = Path.Combine(Filesystem.CacheDirectory, "blob-library");

    static string ContainerUrl => "https://kalmit.blob.core.windows.net/blob-library";

    static public ReadOnlyMemory<byte>? LoadFileForCurrentOs(IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> dict)
    {
        var hashAndRemoteSource =
            dict.FirstOrDefault(c => RuntimeInformation.IsOSPlatform(c.Key)).Value;

        if (hashAndRemoteSource.hash == null)
            throw new Exception("Unknown OS: " + RuntimeInformation.OSDescription);

        var hash = CommonConversion.ByteArrayFromStringBase16(hashAndRemoteSource.hash);

        return GetBlobWithSHA256Cached(
            hash,
            getIfNotCached: () => DownloadFromUrlAndExtractBlobWithMatchingHash(hashAndRemoteSource.remoteSource, hash));
    }

    static public ReadOnlyMemory<byte>? GetBlobWithSHA256(ReadOnlyMemory<byte> sha256)
    {
        try
        {
            var getter = OverrideGetBlobWithSHA256?.Invoke(GetBlobWithSHA256Cached) ?? GetBlobWithSHA256Cached;

            var blobCandidate = getter(sha256);

            if (blobCandidate == null)
                return null;

            if (!(Composition.GetHash(Composition.Component.Blob(blobCandidate.Value)).Span.SequenceEqual(sha256.Span) ||
                System.Security.Cryptography.SHA256.HashData(blobCandidate.Value.Span).AsSpan().SequenceEqual(sha256.Span)))
                return null;

            return blobCandidate;
        }
        catch (Exception e)
        {
            throw new Exception(
                "Did not find blob with hash " + CommonConversion.StringBase16(sha256),
                innerException: e);
        }
    }

    static public ReadOnlyMemory<byte>? GetBlobWithSHA256Cached(ReadOnlyMemory<byte> sha256) =>
        GetBlobWithSHA256Cached(sha256, null);

    static public ReadOnlyMemory<byte>? GetBlobWithSHA256Cached(ReadOnlyMemory<byte> sha256, Func<ReadOnlyMemory<byte>?>? getIfNotCached)
    {
        var sha256DirectoryName = "by-sha256";

        var fileName = BitConverter.ToString(sha256.ToArray()).Replace("-", "").ToLowerInvariant();

        var cacheFilePath = Path.Combine(cacheDirectory, sha256DirectoryName, fileName);

        var blobHasExpectedSHA256 = BlobHasSHA256(sha256);

        try
        {
            var fromCache = File.ReadAllBytes(cacheFilePath);

            if (blobHasExpectedSHA256(fromCache))
                return fromCache;
        }
        catch
        { }

        string url(bool useUppercaseForHash) =>
            ContainerUrl + "/" + sha256DirectoryName + "/" +
            (useUppercaseForHash ? fileName.ToUpperInvariant() : fileName);

        if (getIfNotCached != null)
        {
            var fromExplicitSource = getIfNotCached();

            if (fromExplicitSource != null && blobHasExpectedSHA256(fromExplicitSource.Value))
                return tryUpdateCacheAndContinueFromBlob(fromExplicitSource.Value);
        }

        ReadOnlyMemory<byte> tryUpdateCacheAndContinueFromBlob(ReadOnlyMemory<byte> responseContent)
        {
            if (!blobHasExpectedSHA256(responseContent))
            {
                throw new NotImplementedException("Received unexpected blob for '" + fileName + "'.");
            }

            Directory.CreateDirectory(Path.GetDirectoryName(cacheFilePath)!);

            try
            {
                File.WriteAllBytes(cacheFilePath, responseContent.ToArray());
            }
            catch (IOException)
            {
                // This happens sometimes because another process fetched the same blob and is already writing it to the file.
            }

            return responseContent;
        }

        ReadOnlyMemory<byte>? tryUpdateCacheAndContinueFromHttpResponse(HttpResponseMessage httpResponse)
        {
            if (httpResponse.StatusCode == HttpStatusCode.NotFound)
                return null;

            if (!httpResponse.IsSuccessStatusCode)
            {
                throw new Exception(
                    "unexpected HTTP response status code in response from " + httpResponse.RequestMessage.RequestUri.ToString() +
                    ": " + (int)httpResponse.StatusCode + " (" +
                    httpResponse.StatusCode + ") ('" + httpResponse.Content.ReadAsStringAsync().Result + "')");
            }

            var responseContent = httpResponse.Content.ReadAsByteArrayAsync().Result;

            return tryUpdateCacheAndContinueFromBlob(responseContent);
        }

        var httpResponse = DownloadViaHttp(url(false));

        if (httpResponse.StatusCode == HttpStatusCode.NotFound)
        {
            //  2020-05-01 Maintain backward compatibility for now: Try for the file name using uppercase letters.

            try
            {
                return tryUpdateCacheAndContinueFromHttpResponse(DownloadViaHttp(url(true)));
            }
            catch { }
        }

        return tryUpdateCacheAndContinueFromHttpResponse(httpResponse);
    }

    static public Result<string, ReadOnlyMemory<byte>?> DownloadBlobViaHttpGetResponseBody(string sourceUrl) =>
        DownloadBlobViaHttpGetResponseBodyAsync(sourceUrl).Result;

    static public async Task<Result<string, ReadOnlyMemory<byte>?>> DownloadBlobViaHttpGetResponseBodyAsync(string sourceUrl)
    {
        try
        {
            var httpResponse = await DownloadViaHttpAsync(sourceUrl);

            if (!httpResponse.IsSuccessStatusCode)
            {
                return Result<string, ReadOnlyMemory<byte>?>.err(
                    "Unexpected HTTP response status code: " + (int)httpResponse.StatusCode + " (" + httpResponse.StatusCode + ")");
            }

            var responseContent = await httpResponse.Content.ReadAsByteArrayAsync();

            return Result<string, ReadOnlyMemory<byte>?>.ok(responseContent);
        }
        catch (Exception e)
        {
            return Result<string, ReadOnlyMemory<byte>?>.err("Runtime exception: " + e.ToString());
        }
    }

    static public HttpResponseMessage DownloadViaHttp(string url) =>
        DownloadViaHttpAsync(url).Result;

    static public async Task<HttpResponseMessage> DownloadViaHttpAsync(string url)
    {
        var handler = new HttpClientHandler
        {
            AutomaticDecompression = DecompressionMethods.All
        };

        using var httpClient = new HttpClient(handler);

        httpClient.Timeout = TimeSpan.FromMinutes(4);
        httpClient.DefaultRequestHeaders.AcceptEncoding.Add(new StringWithQualityHeaderValue("gzip"));
        httpClient.DefaultRequestHeaders.AcceptEncoding.Add(new StringWithQualityHeaderValue("deflate"));

        return await httpClient.GetAsync(url);
    }

    static public ReadOnlyMemory<byte>? DownloadFromUrlAndExtractBlobWithMatchingHash(
        string sourceUrl,
        ReadOnlyMemory<byte> sha256)
    {
        return
            DownloadFromUrlAndExtractBlobs(sourceUrl)
            .Where(BlobHasSHA256(sha256)).Cast<ReadOnlyMemory<byte>?>()
            .FirstOrDefault();
    }

    static public Func<ReadOnlyMemory<byte>, bool> BlobHasSHA256(ReadOnlyMemory<byte> sha256) =>
        blobCandidate =>
        Composition.GetHash(Composition.Component.Blob(blobCandidate)).Span.SequenceEqual(sha256.Span) ||
        System.Security.Cryptography.SHA256.HashData(blobCandidate.Span).AsSpan().SequenceEqual(sha256.Span);

    static public IEnumerable<ReadOnlyMemory<byte>> DownloadFromUrlAndExtractBlobs(string sourceUrl) =>
        DownloadFromUrlAndExtractTrees(sourceUrl)
        .SelectMany(tree => tree.EnumerateBlobsTransitive().Select(blob => blob.blobContent));

    static public IEnumerable<Composition.TreeWithStringPath> DownloadFromUrlAndExtractTrees(string sourceUrl)
    {
        var httpResponse = DownloadViaHttp(sourceUrl);

        if (!httpResponse.IsSuccessStatusCode)
        {
            yield break;
        }

        var responseContent = httpResponse.Content.ReadAsByteArrayAsync().Result;

        if (responseContent == null)
            yield break;

        yield return Composition.TreeWithStringPath.Blob((ReadOnlyMemory<byte>)responseContent);

        var blobName = sourceUrl.Split('/', '\\').Last();

        foreach (var extracted in ExtractTreesFromNamedBlob(blobName, responseContent))
            yield return extracted;
    }

    static public IEnumerable<Composition.TreeWithStringPath> ExtractTreesFromNamedBlob(string blobName, ReadOnlyMemory<byte> blobContent)
    {
        {
            Composition.TreeWithStringPath? fromZipArchive = null;

            try
            {
                fromZipArchive =
                    Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(blobContent));
            }
            catch { }

            if (fromZipArchive != null)
                yield return fromZipArchive;
        }

        if (blobName.EndsWith(".tar.gz", StringComparison.OrdinalIgnoreCase) || blobName.EndsWith(".tgz", StringComparison.OrdinalIgnoreCase))
        {
            Composition.TreeWithStringPath? fromTarArchive = null;

            try
            {
                fromTarArchive = TarArchive.TreeWithStringPathFromTarArchive(CommonConversion.DecompressGzip(blobContent));
            }
            catch { }

            if (fromTarArchive != null)
                yield return fromTarArchive;
        }
        else if (blobName.EndsWith(".gz"))
        {
            ReadOnlyMemory<byte>? fromGzip = null;

            try
            {
                fromGzip = CommonConversion.DecompressGzip(blobContent);
            }
            catch { }

            if (fromGzip != null)
                yield return Composition.TreeWithStringPath.Blob((ReadOnlyMemory<byte>)fromGzip);
        }
    }
}
