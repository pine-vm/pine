using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Formats.Tar;

namespace Pine.Core.Bundle;

/// <summary>
/// Helper class for creating and reading TAR archives compressed with GZip (.tar.gz)
/// </summary>
public static class TarGZipArchive
{
    /// <summary>
    /// Creates a TAR archive compressed with GZip from a dictionary of file paths and their contents.
    /// </summary>
    /// <param name="files">Dictionary where keys are file paths (list of path segments) and values are file contents</param>
    /// <returns>Compressed TAR archive as a byte array</returns>
    public static ReadOnlyMemory<byte> CreateArchive(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> files)
    {
        // Build TAR in memory using System.Formats.Tar
        using var tarStream = new MemoryStream();

        using (var tarWriter = new TarWriter(tarStream, TarEntryFormat.Pax, leaveOpen: true))
        {
            foreach (var file in files.OrderBy(kvp => string.Join("/", kvp.Key)))
            {
                var filePath = string.Join("/", file.Key);
                var fileContent = file.Value;

                // Create a regular file entry and attach content stream
                var entry = new PaxTarEntry(TarEntryType.RegularFile, filePath)
                {
                    DataStream = new MemoryStream(fileContent.ToArray(), writable: false)
                };

                tarWriter.WriteEntry(entry);
            }
        }

        tarStream.Position = 0;

        // Compress with GZip
        using var compressedStream = new MemoryStream();
        using (var gzip = new GZipStream(compressedStream, CompressionLevel.Optimal, leaveOpen: true))
        {
            tarStream.CopyTo(gzip);
        }

        return compressedStream.ToArray();
    }

    /// <summary>
    /// Extracts files from a TAR archive compressed with GZip.
    /// </summary>
    /// <param name="archiveBytes">The compressed TAR archive bytes</param>
    /// <returns>Dictionary where keys are file paths (list of path segments) and values are file contents</returns>
    public static IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> ExtractArchive(
        ReadOnlyMemory<byte> archiveBytes)
    {
        using var compressedStream = new MemoryStream(archiveBytes.ToArray());
        using var decompressedTarStream = new MemoryStream();

        // Decompress GZip to a TAR stream
        using (var gzip = new GZipStream(compressedStream, CompressionMode.Decompress))
        {
            gzip.CopyTo(decompressedTarStream);
        }

        decompressedTarStream.Position = 0;

        var files = new Dictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>();

        using (var tarReader = new TarReader(decompressedTarStream))
        {
            TarEntry? entry;
            while ((entry = tarReader.GetNextEntry()) != null)
            {
                // Only process regular file entries. Zero-length regular files may have null DataStream.
                if (entry.EntryType is not (TarEntryType.RegularFile or TarEntryType.V7RegularFile))
                    continue;

                var pathSegments = entry.Name.Replace('\\', '/').Split('/', StringSplitOptions.RemoveEmptyEntries).ToList();

                using var contentStream = new MemoryStream();
                if (entry.DataStream is not null)
                {
                    entry.DataStream.CopyTo(contentStream);
                }

                files[pathSegments] = contentStream.ToArray();
            }
        }

        return files;
    }
}
