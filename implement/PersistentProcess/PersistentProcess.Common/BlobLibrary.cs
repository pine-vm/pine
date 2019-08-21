using System;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Security.Cryptography;

namespace Kalmit
{
    public class BlobLibrary
    {
        static string cacheDirectory = Path.Combine(Filesystem.CacheDirectory, "blob-library");

        static string containerUrl => "https://kalmit.blob.core.windows.net/blob-library";

        static public byte[] GetBlobWithSHA256(byte[] sha256)
        {
            var sha256DirectoryName = "by-sha256";

            var fileName = BitConverter.ToString(sha256).Replace("-", "").ToUpperInvariant();

            var cacheFilePath = Path.Combine(cacheDirectory, sha256DirectoryName, fileName);

            bool blobHasExpectedSHA256(byte[] blob) =>
                Enumerable.SequenceEqual(CommonConversion.HashSHA256(blob), sha256);

            try
            {
                var fromCache = File.ReadAllBytes(cacheFilePath);

                if (blobHasExpectedSHA256(fromCache))
                    return fromCache;
            }
            catch
            { }

            var url = containerUrl + "/" + sha256DirectoryName + "/" + fileName;

            var httpClient = new HttpClient();

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
