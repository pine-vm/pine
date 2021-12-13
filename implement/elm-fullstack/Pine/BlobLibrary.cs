using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;

namespace Pine;

public class BlobLibrary
{
    static public Func<Func<byte[], byte[]?>, Func<byte[], byte[]?>>? OverrideGetBlobWithSHA256;

    static readonly string cacheDirectory = Path.Combine(Filesystem.CacheDirectory, "blob-library");

    static string ContainerUrl => "https://kalmit.blob.core.windows.net/blob-library";

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

        bool blobHasExpectedSHA256(byte[] blobCandidate) =>
            Enumerable.SequenceEqual(Composition.GetHash(Composition.Component.Blob(blobCandidate)), sha256) ||
            Enumerable.SequenceEqual(CommonConversion.HashSHA256(blobCandidate), sha256);

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
        bool blobHasExpectedSHA256(byte[] blobCandidate) =>
            Enumerable.SequenceEqual(Composition.GetHash(Composition.Component.Blob(blobCandidate)), sha256) ||
            Enumerable.SequenceEqual(CommonConversion.HashSHA256(blobCandidate), sha256);

        return DownloadFromUrlAndExtractBlobs(sourceUrl).FirstOrDefault(blobHasExpectedSHA256);
    }

    static public IEnumerable<byte[]> DownloadFromUrlAndExtractBlobs(string sourceUrl)
    {
        var httpResponse = DownloadViaHttp(sourceUrl);

        if (!httpResponse.IsSuccessStatusCode)
        {
            yield break;
        }

        var responseContent = httpResponse.Content.ReadAsByteArrayAsync().Result;

        yield return responseContent;

        IEnumerator<byte[]>? enumerator = null;

        if (sourceUrl.EndsWith(".zip"))
        {
            try
            {
                enumerator = ZipArchive.EntriesFromZipArchive(responseContent).Select(c => c.content).GetEnumerator();
            }
            catch { }
        }

        if (sourceUrl.EndsWith(".tar.gz"))
        {
            try
            {
                enumerator =
                    SharpCompress.Archives.Tar.TarArchive.Open(new MemoryStream(CommonConversion.DecompressGzip(responseContent))).Entries
                    .Select(entry =>
                    {
                        using (var memoryStream = new MemoryStream())
                        {
                            using (var tarEntryStream = entry.OpenEntryStream())
                            {
                                tarEntryStream.CopyTo(memoryStream);
                                return memoryStream.ToArray();
                            }
                        }
                    }).GetEnumerator();
            }
            catch { }
        }

        if (enumerator != null)
        {
            try
            {
                if (!enumerator.MoveNext())
                    yield break;
            }
            catch { }

            yield return enumerator.Current;
        }
    }
}
