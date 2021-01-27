using System;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;

namespace Kalmit
{
    public class BlobLibrary
    {
        static public Func<Func<byte[], byte[]>, Func<byte[], byte[]>> OverrideGetBlobWithSHA256;

        static string cacheDirectory = Path.Combine(Filesystem.CacheDirectory, "blob-library");

        static string containerUrl => "https://kalmit.blob.core.windows.net/blob-library";

        static public byte[] GetBlobWithSHA256(byte[] sha256)
        {
            try
            {
                var getter = OverrideGetBlobWithSHA256?.Invoke(DefaultGetBlobWithSHA256) ?? DefaultGetBlobWithSHA256;

                var blobCandidate = getter(sha256);

                if (!Enumerable.SequenceEqual(CommonConversion.HashSHA256(blobCandidate), sha256))
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

        static public byte[] DefaultGetBlobWithSHA256(byte[] sha256)
        {
            var sha256DirectoryName = "by-sha256";

            var fileName = BitConverter.ToString(sha256).Replace("-", "").ToLowerInvariant();

            var cacheFilePath = Path.Combine(cacheDirectory, sha256DirectoryName, fileName);

            bool blobHasExpectedSHA256(byte[] blobCandidate) =>
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
                containerUrl + "/" + sha256DirectoryName + "/" +
                (useUppercaseForHash ? fileName.ToUpperInvariant() : fileName);

            var handler = new HttpClientHandler()
            {
                AutomaticDecompression = DecompressionMethods.Deflate | DecompressionMethods.GZip
            };

            byte[] continueForHttpResponse(HttpResponseMessage httpResponse)
            {
                if (!httpResponse.IsSuccessStatusCode)
                {
                    throw new Exception(
                        "unexpected HTTP response status code: " + (int)httpResponse.StatusCode + " (" +
                        httpResponse.StatusCode + ") ('" + httpResponse.Content.ReadAsStringAsync().Result + "')");
                }

                var responseContent = httpResponse.Content.ReadAsByteArrayAsync().Result;

                if (!blobHasExpectedSHA256(responseContent))
                {
                    throw new NotImplementedException("Received unexpected blob for '" + fileName + "'.");
                }

                Directory.CreateDirectory(Path.GetDirectoryName(cacheFilePath));

                File.WriteAllBytes(cacheFilePath, responseContent);

                return responseContent;
            }

            using (var httpClient = new HttpClient(handler))
            {
                httpClient.Timeout = TimeSpan.FromMinutes(4);
                httpClient.DefaultRequestHeaders.AcceptEncoding.Add(new StringWithQualityHeaderValue("gzip"));
                httpClient.DefaultRequestHeaders.AcceptEncoding.Add(new StringWithQualityHeaderValue("deflate"));

                var httpResponse = httpClient.GetAsync(url(false)).Result;

                if (httpResponse.StatusCode == HttpStatusCode.NotFound)
                {
                    //  2020-05-01 Maintain backward compatibility for now: Try for the file name using uppercase letters.

                    try
                    {
                        return continueForHttpResponse(httpClient.GetAsync(url(true)).Result);
                    }
                    catch { }
                }

                return continueForHttpResponse(httpResponse);
            }
        }
    }
}
