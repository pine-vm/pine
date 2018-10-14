using System;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Security.Cryptography;

namespace Kalmit
{
    public class BlobLibrary
    {
        static string cacheDirectory = Path.Combine(Directory.GetCurrentDirectory(), ".cache", "blob-library");

        static string containerUrl => "https://kalmit.blob.core.windows.net/blob-library";

        static public byte[] GetBlobWithSHA1(byte[] sha1)
        {
            var sha1DirectoryName = "by-sha1";

            var fileName = BitConverter.ToString(sha1).Replace("-", "").ToUpperInvariant();

            var cacheFilePath = Path.Combine(cacheDirectory, sha1DirectoryName, fileName);

            bool blobHasExpectedSHA1(byte[] blob) =>
                Enumerable.SequenceEqual(new SHA1Managed().ComputeHash(blob), sha1);

            try
            {
                var fromCache = File.ReadAllBytes(cacheFilePath);

                if (blobHasExpectedSHA1(fromCache))
                    return fromCache;
            }
            catch
            { }

            var url = containerUrl + "/" + sha1DirectoryName + "/" + fileName;

            var httpClient = new HttpClient();

            var response = httpClient.GetAsync(url).Result;

            var responseContent = response.Content.ReadAsByteArrayAsync().Result;

            if (!blobHasExpectedSHA1(responseContent))
                throw new NotImplementedException("Received unexpected blob.");

            Directory.CreateDirectory(Path.GetDirectoryName(cacheFilePath));

            File.WriteAllBytes(cacheFilePath, responseContent);

            return responseContent;
        }
    }
}
