using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Runtime.InteropServices;

namespace Pine;

public class BlobLibrary
{
    static public Func<Func<byte[], byte[]?>, Func<byte[], byte[]?>>? OverrideGetBlobWithSHA256;

    static readonly string cacheDirectory = Path.Combine(Filesystem.CacheDirectory, "blob-library");

    static string ContainerUrl => "https://kalmit.blob.core.windows.net/blob-library";

    static public byte[]? LoadFileForCurrentOs(IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> dict)
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

    static public byte[]? GetBlobWithSHA256(byte[] sha256)
    {
        try
        {
            var getter = OverrideGetBlobWithSHA256?.Invoke(GetBlobWithSHA256Cached) ?? GetBlobWithSHA256Cached;

            var blobCandidate = getter(sha256);

            if (blobCandidate == null)
                return null;

            if (!(Enumerable.SequenceEqual(Composition.GetHash(Composition.Component.Blob(blobCandidate)), sha256) ||
                Enumerable.SequenceEqual(CommonConversion.HashSHA256(blobCandidate), sha256)))
                return null;

            return blobCandidate;
        }
        catch (Exception e)
        {
            throw new Exception(
                "Did not find blob with hash " + CommonConversion.StringBase16FromByteArray(sha256),
                innerException: e);
        }
    }

    static public byte[]? GetBlobWithSHA256Cached(byte[] sha256) =>
        GetBlobWithSHA256Cached(sha256, null);

    static public byte[]? GetBlobWithSHA256Cached(byte[] sha256, Func<byte[]?>? getIfNotCached)
    {
        var sha256DirectoryName = "by-sha256";

        var fileName = BitConverter.ToString(sha256).Replace("-", "").ToLowerInvariant();

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

            if (fromExplicitSource != null && blobHasExpectedSHA256(fromExplicitSource))
                return tryUpdateCacheAndContinueFromBlob(fromExplicitSource);
        }

        byte[] tryUpdateCacheAndContinueFromBlob(byte[] responseContent)
        {
            if (!blobHasExpectedSHA256(responseContent))
            {
                throw new NotImplementedException("Received unexpected blob for '" + fileName + "'.");
            }

            Directory.CreateDirectory(Path.GetDirectoryName(cacheFilePath)!);

            try
            {
                File.WriteAllBytes(cacheFilePath, responseContent);
            }
            catch (IOException)
            {
                // This happens sometimes because another process fetched the same blob and is already writing it to the file.
            }

            return responseContent;
        }

        byte[]? tryUpdateCacheAndContinueFromHttpResponse(HttpResponseMessage httpResponse)
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

    static public HttpResponseMessage DownloadViaHttp(string url)
    {
        var handler = new HttpClientHandler
        {
            AutomaticDecompression = DecompressionMethods.All
        };

        using var httpClient = new HttpClient(handler);

        httpClient.Timeout = TimeSpan.FromMinutes(4);
        httpClient.DefaultRequestHeaders.AcceptEncoding.Add(new StringWithQualityHeaderValue("gzip"));
        httpClient.DefaultRequestHeaders.AcceptEncoding.Add(new StringWithQualityHeaderValue("deflate"));

        return httpClient.GetAsync(url).Result;
    }

    static public byte[]? DownloadFromUrlAndExtractBlobWithMatchingHash(
        string sourceUrl,
        byte[] sha256)
    {
        return DownloadFromUrlAndExtractBlobs(sourceUrl).FirstOrDefault(BlobHasSHA256(sha256))?.ToArray();
    }

    static public Func<IReadOnlyList<byte>, bool> BlobHasSHA256(byte[] sha256) =>
        blobCandidate =>
        Enumerable.SequenceEqual(Composition.GetHash(Composition.Component.Blob(blobCandidate)), sha256) ||
        Enumerable.SequenceEqual(CommonConversion.HashSHA256(blobCandidate as byte[] ?? blobCandidate.ToArray()), sha256);

    static public IEnumerable<IReadOnlyList<byte>> DownloadFromUrlAndExtractBlobs(string sourceUrl) =>
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

        yield return Composition.TreeWithStringPath.Blob(responseContent);

        {
            Composition.TreeWithStringPath? fromZipArchive = null;

            try
            {
                fromZipArchive =
                    Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(responseContent));
            }
            catch { }

            if (fromZipArchive != null)
                yield return fromZipArchive;
        }

        if (sourceUrl.EndsWith(".tar.gz"))
        {
            Composition.TreeWithStringPath? fromTarArchive = null;

            try
            {
                fromTarArchive = TarArchive.TreeWithStringPathFromTarArchive(CommonConversion.DecompressGzip(responseContent));
            }
            catch { }

            if (fromTarArchive != null)
                yield return fromTarArchive;
        }
        else if (sourceUrl.EndsWith(".gz"))
        {
            byte[]? fromGzip = null;

            try
            {
                fromGzip = CommonConversion.DecompressGzip(responseContent);
            }
            catch { }

            if (fromGzip != null)
                yield return Composition.TreeWithStringPath.Blob(fromGzip);
        }
    }
}
