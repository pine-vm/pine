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
            var getter = OverrideGetBlobWithSHA256?.Invoke(DefaultGetBlobWithSHA256) ?? DefaultGetBlobWithSHA256;

            var blobCandidate = getter(sha256);

            if (!Enumerable.SequenceEqual(CommonConversion.HashSHA256(blobCandidate), sha256))
                return null;

            return blobCandidate;
        }

        static public byte[] DefaultGetBlobWithSHA256(byte[] sha256)
        {
            var sha256DirectoryName = "by-sha256";

            var fileName = BitConverter.ToString(sha256).Replace("-", "").ToUpperInvariant();

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

            var url = containerUrl + "/" + sha256DirectoryName + "/" + fileName;

            var handler = new HttpClientHandler()
            {
                AutomaticDecompression = DecompressionMethods.Deflate | DecompressionMethods.GZip
            };

            using (var httpClient = new HttpClient(handler))
            {
                httpClient.Timeout = TimeSpan.FromMinutes(4);
                httpClient.DefaultRequestHeaders.AcceptEncoding.Add(new StringWithQualityHeaderValue("gzip"));
                httpClient.DefaultRequestHeaders.AcceptEncoding.Add(new StringWithQualityHeaderValue("deflate"));

                var response = httpClient.GetAsync(url).Result;

                var responseContent = response.Content.ReadAsByteArrayAsync().Result;

                if (!blobHasExpectedSHA256(responseContent))
                    throw new NotImplementedException("Received unexpected blob.");

                Directory.CreateDirectory(Path.GetDirectoryName(cacheFilePath));

                File.WriteAllBytes(cacheFilePath, responseContent);

                return responseContent;
            }
        }
    }
}
