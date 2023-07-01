using Pine;
using System;
using System.IO;
using System.Linq;
using System.Security.Cryptography;

namespace ElmTime.NativeDependency;

public class NativeDependencies
{
    public static void SetUpDependency(
        string cacheDirectory,
        DependencyFile dependency)
    {
        var hash = CommonConversion.ByteArrayFromStringBase16(dependency.HashBase16);

        var fileAbsolutePath = Path.Combine(cacheDirectory, dependency.ExpectedFileName);

        if (File.Exists(fileAbsolutePath))
        {
            var fileContent = File.ReadAllBytes(fileAbsolutePath);

            var fileContentHash = SHA256.HashData(fileContent);

            if (fileContentHash.SequenceEqual(hash))
            {
                return;
            }
        }

        var file = BlobLibrary.GetBlobWithSHA256Cached(
            sha256: hash,
            getIfNotCached:
            () =>
            BlobLibrary.DownloadFromUrlAndExtractBlobWithMatchingHashFromListOfRemoteSources(
                dependency.RemoteSources, hash))
            ?? throw new Exception(
                "Did not find dependency " + dependency.HashBase16 + " (" + dependency.ExpectedFileName + ") in any of the " +
                dependency.RemoteSources.Count + " remote sources");

        ExecutableFile.CreateAndWriteFileToPath(fileAbsolutePath, file, makeExecutable: true);
    }
}
