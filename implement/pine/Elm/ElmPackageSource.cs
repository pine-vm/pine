using Pine.Core;
using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Net.Http;
using System.Threading.Tasks;

namespace Pine.Elm;

/// <summary>
/// Provides functionality to load Elm packages by either retrieving a locally cached ZIP file
/// or downloading it from GitHub. The loaded package contents are returned as an in-memory
/// dictionary mapping path segments to file contents. If no valid local cache is found,
/// the package is fetched from GitHub and saved to the first cache directory for future use.
///
/// Usage:
/// <code>
/// var packageFiles = await ElmPackageSource.LoadElmPackageAsync("agu-z/elm-zip", "3.0.1");
/// // Access file contents by path segments from the returned dictionary
/// </code>
/// </summary>
public class ElmPackageSource
{
    /// <summary>
    /// Default local cache directories (example: ~/.cache/elm-package).
    /// You can add more directories if you want a search path.
    /// </summary>
    public static IReadOnlyList<string> LocalCacheDirectoriesDefault =>
        [
            Path.Combine(Filesystem.CacheDirectory, "elm-package"),
        ];

    /// <summary>
    /// Public entry point that uses the default cache directories.
    /// </summary>
    public static async Task<IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadElmPackageAsync(
        string packageName,
        string versionId)
    {
        return await LoadElmPackageAsync(packageName, versionId, LocalCacheDirectoriesDefault);
    }

    /// <summary>
    /// Tries to load from any of the given local cache directories; if not found, downloads from GitHub and saves to the first cache dir.
    /// </summary>
    public static async Task<IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadElmPackageAsync(
        string packageName,
        string versionId,
        IReadOnlyList<string> localCacheDirectories)
    {
        // 1) Try to load from each cache directory, in order:
        foreach (var cacheDirectory in localCacheDirectories)
        {
            // Construct the path for the cached file, for example "agu-z-elm-zip@3.0.1.zip".
            var localZipPath = GetLocalZipPath(cacheDirectory, packageName, versionId);

            if (File.Exists(localZipPath))
            {
                try
                {
                    // Attempt to load and parse the ZIP from disk.
                    var data = await File.ReadAllBytesAsync(localZipPath);

                    var fromCache = LoadElmPackageFromZipBytes(data);

                    return fromCache;
                }
                catch
                {
                    // If anything failed (e.g. file is corrupt), ignore and keep going.
                }
            }
        }

        // 2) No valid cache found — download from GitHub:
        var zipData = await DownloadPackageZipAsync(packageName, versionId);

        var fromGitHub = LoadElmPackageFromZipBytes(zipData);

        // 3) Write the ZIP to the *first* cache directory, if present (and possible).
        if (localCacheDirectories.Count > 0)
        {
            var firstCache = localCacheDirectories[0];

            try
            {
                Directory.CreateDirectory(firstCache); // Ensure the directory exists.

                var localZipPath = GetLocalZipPath(firstCache, packageName, versionId);

                await File.WriteAllBytesAsync(localZipPath, zipData);
            }
            catch
            {
                // Caching is non-critical. Swallow any exception here (e.g. no write permission).
            }
        }

        return fromGitHub;
    }

    /// <summary>
    /// Given the raw bytes of a ZIP file, extract all files and return them as a dictionary
    /// from path segments to the file contents.
    /// </summary>
    public static IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> LoadElmPackageFromZipBytes(
        byte[] zipBytes)
    {
        var result =
            new Dictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>(
                EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        using var memoryStream = new MemoryStream(zipBytes);

        using var archive = new System.IO.Compression.ZipArchive(memoryStream, ZipArchiveMode.Read);

        foreach (var entry in archive.Entries)
        {
            // If it's a directory entry, skip
            if (string.IsNullOrEmpty(entry.Name))
                continue;

            using var entryStream = entry.Open();
            using var entryMemoryStream = new MemoryStream();
            entryStream.CopyTo(entryMemoryStream);

            var fileContents = entryMemoryStream.ToArray();

            // Split the full name into segments
            var pathSegments = entry.FullName.Split(['/'], StringSplitOptions.RemoveEmptyEntries);

            result[pathSegments] = fileContents;
        }

        return UnpackElmPackageFilesDownloadedFromGitHub(result);
    }

    public static IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> UnpackElmPackageFilesDownloadedFromGitHub(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> rawDownload)
    {
        /*
         * GitHub at least sometimes puts the repository files not at the
         * root but into a directory with a name like 'core-1.0.5'
         * */

        if (!rawDownload.ContainsKey(["elm.json"]))
        {
            var directoryName =
                rawDownload
                .FirstOrDefault((filePath) => filePath.Key.Count is 2 && filePath.Key.Last() is "elm.json")
                .Key?[0];

            if (directoryName is not null)
            {
                var directoryFiles =
                    new Dictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>(
                        EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

                foreach (var (filePath, fileContent) in rawDownload)
                {
                    if (filePath.Count is 0)
                    {
                        continue;
                    }

                    if (filePath[0] == directoryName)
                    {
                        directoryFiles[[.. filePath.Skip(1)]] = fileContent;
                    }
                }

                return directoryFiles;
            }
        }

        return rawDownload;
    }


    /// <summary>
    /// Downloads the ZIP from GitHub as a byte array.
    /// </summary>
    private static async Task<byte[]> DownloadPackageZipAsync(string packageName, string versionId)
    {
        // Example: "https://github.com/agu-z/elm-zip/archive/refs/tags/3.0.1.zip"

        var downloadUrl = "https://github.com/" + packageName.Trim('/') + "/archive/refs/tags/" + versionId + ".zip";

        using var httpClient = new HttpClient();

        using var zipStream = await httpClient.GetStreamAsync(downloadUrl);

        // Copy to memory
        using var memoryStream = new MemoryStream();

        await zipStream.CopyToAsync(memoryStream);

        return memoryStream.ToArray();
    }

    /// <summary>
    /// Constructs a local ZIP file path: e.g. "agu-z-elm-zip@3.0.1.zip" in the given cache directory.
    /// Replaces slashes so the filename is filesystem-safe.
    /// </summary>
    private static string GetLocalZipPath(string cacheDirectory, string packageName, string versionId)
    {
        var safePkgName = packageName.Replace('/', '-');

        // Example: "agu-z-elm-zip@3.0.1.zip"
        var fileName = $"{safePkgName}@{versionId}.zip";

        return Path.Combine(cacheDirectory, fileName);
    }
}
