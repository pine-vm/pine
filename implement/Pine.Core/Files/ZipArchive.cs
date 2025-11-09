using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Pine.Core.Files;

/// <summary>
/// Helpers for working with ZIP archives.
/// <see href="https://en.wikipedia.org/wiki/ZIP_(file_format)"></see>
/// </summary>
public static class ZipArchive
{
    /// <summary>
    /// https://github.com/dotnet/corefx/blob/a10890f4ffe0fadf090c922578ba0e606ebdd16c/src/System.IO.Compression/src/System/IO/Compression/ZipArchiveEntry.cs#L206-L234
    /// </summary>
    public static DateTimeOffset EntryLastWriteTimeDefault => new(1980, 1, 1, 0, 0, 0, TimeSpan.Zero);

    /// <summary>
    /// Creates a ZIP archive containing the specified file entries and returns its binary data as a byte array.
    /// </summary>
    /// <remarks>Each file entry is added to the archive using its provided name. The method does not preserve
    /// file metadata such as timestamps or permissions. The returned byte array can be saved to disk or transmitted as
    /// needed.</remarks>
    /// <param name="entries">A collection of file entries, each consisting of a file name and its content as a read-only memory buffer. Each
    /// entry will be added to the archive with the provided name and content.</param>
    /// <param name="compressionLevel">The compression level to apply to the archive. Defaults to <see
    /// cref="System.IO.Compression.CompressionLevel.Optimal"/> if not specified.</param>
    /// <returns>A byte array containing the ZIP archive data with all specified entries included.</returns>
    public static byte[] ZipArchiveFromFiles(
        IEnumerable<(string name, ReadOnlyMemory<byte> content)> entries,
        System.IO.Compression.CompressionLevel compressionLevel = System.IO.Compression.CompressionLevel.Optimal) =>
        ZipArchiveFromFiles(
            [.. entries.Select(entry => (entry.name, entry.content, EntryLastWriteTimeDefault))],
            compressionLevel);

    /// <summary>
    /// Creates a ZIP archive containing the specified files and returns its contents as a byte array.
    /// </summary>
    /// <remarks>Each file path is constructed by joining the path segments in the key with a forward slash
    /// ('/'). The method does not validate file names or contents; callers should ensure that paths and data are valid
    /// for their intended use.</remarks>
    /// <param name="entries">A dictionary mapping each file's path segments to its content. Each key represents the file path as a list of
    /// strings, and each value contains the file's data as a read-only memory buffer.</param>
    /// <param name="compressionLevel">The compression level to use when creating the ZIP archive. The default is CompressionLevel.Optimal.</param>
    /// <returns>A byte array containing the ZIP archive with all specified files. The array will be empty if no entries are
    /// provided.</returns>
    public static byte[] ZipArchiveFromFiles(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> entries,
        System.IO.Compression.CompressionLevel compressionLevel = System.IO.Compression.CompressionLevel.Optimal) =>
        ZipArchiveFromFiles(
            entries.Select(entry => (name: string.Join("/", entry.Key), content: entry.Value)),
            compressionLevel);

    /// <summary>
    /// Creates a ZIP archive containing the specified files and returns its contents as a byte array.
    /// </summary>
    /// <remarks>The returned ZIP archive is created in memory and is not written to disk. The method does not
    /// validate the uniqueness of file names; duplicate names may result in multiple entries with the same name in the
    /// archive. The last write time for each entry is set according to the provided value.</remarks>
    /// <param name="entries">A collection of file entries to include in the archive. Each entry specifies the file name, content as a
    /// read-only memory buffer, and the last write time to set for the file in the archive. The file name must be a
    /// valid ZIP entry name and cannot be null or empty.</param>
    /// <param name="compressionLevel">The compression level to use for each file in the archive. Defaults to <see
    /// cref="System.IO.Compression.CompressionLevel.Optimal"/> if not specified.</param>
    /// <returns>A byte array containing the complete ZIP archive. The array will be empty if no entries are provided.</returns>
    public static byte[] ZipArchiveFromFiles(
        IEnumerable<(string name, ReadOnlyMemory<byte> content, DateTimeOffset lastWriteTime)> entries,
        System.IO.Compression.CompressionLevel compressionLevel = System.IO.Compression.CompressionLevel.Optimal)
    {
        var stream = new MemoryStream();

        using (var fclZipArchive = new System.IO.Compression.ZipArchive(stream, System.IO.Compression.ZipArchiveMode.Create, true))
        {
            foreach (var (entryName, entryContent, lastWriteTime) in entries)
            {
                var entry = fclZipArchive.CreateEntry(entryName, compressionLevel);

                entry.LastWriteTime = lastWriteTime;

                using var entryStream = entry.Open();

                entryStream.Write(entryContent.Span);
            }
        }

        stream.Seek(0, SeekOrigin.Begin);

        var zipArchive = new byte[stream.Length];

        stream.Read(zipArchive, 0, (int)stream.Length);
        stream.Dispose();

        return zipArchive;
    }

    /// <summary>
    /// Enumerates the files contained in a ZIP archive, each represented by its (flat) path and content.
    /// </summary>
    /// <param name="zipArchive">A read-only memory buffer containing the ZIP archive data. Must be a valid ZIP file; otherwise, the behavior is
    /// undefined.</param>
    /// <returns>An enumerable collection of tuples, each containing the file name and its content as a read-only memory buffer.
    /// Only file entries are included; directory entries are excluded.</returns>
    public static IEnumerable<(string name, ReadOnlyMemory<byte> content)> FileEntriesFromZipArchive(
        ReadOnlyMemory<byte> zipArchive) =>
        EntriesFromZipArchive(
            zipArchive: zipArchive,
            includeEntry: entry => !entry.FullName.Replace('\\', '/').EndsWith('/'));

    /// <summary>
    /// Extracts all entries from the specified ZIP archive and returns their names and contents.
    /// </summary>
    /// <remarks>The method returns all entries in the archive, including files and directories.
    /// The order of entries matches their order in the archive.</remarks>
    /// <param name="zipArchive">A read-only memory buffer containing the ZIP archive data. Must represent a valid ZIP file format.</param>
    /// <returns>An enumerable collection of tuples, each containing the entry name and its content as a read-only memory buffer.
    /// The collection is empty if the archive contains no entries.</returns>
    public static IEnumerable<(string name, ReadOnlyMemory<byte> content)> EntriesFromZipArchive(ReadOnlyMemory<byte> zipArchive) =>
        EntriesFromZipArchive(zipArchive: zipArchive, includeEntry: _ => true);

    /// <summary>
    /// Enumerates entries from a ZIP archive and returns the name and content of each entry that matches the specified
    /// filter.
    /// </summary>
    /// <remarks>The method reads the entire content of each included entry into memory. Use caution when
    /// processing large archives or entries to avoid excessive memory usage.</remarks>
    /// <param name="zipArchive">A read-only memory buffer containing the binary data of the ZIP archive to be read.</param>
    /// <param name="includeEntry">A predicate used to determine whether a given ZIP archive entry should be included in the results. The function
    /// receives each entry and should return <see langword="true"/> to include the entry; otherwise, <see
    /// langword="false"/>.</param>
    /// <returns>An enumerable collection of tuples, each containing the entry name and its content as a read-only memory buffer.
    /// Only entries for which <paramref name="includeEntry"/> returns <see langword="true"/> are included.</returns>
    /// <exception cref="Exception">Thrown if the number of bytes read from an entry does not match the expected entry length.</exception>
    public static IEnumerable<(string name, ReadOnlyMemory<byte> content)> EntriesFromZipArchive(
        ReadOnlyMemory<byte> zipArchive,
        Func<System.IO.Compression.ZipArchiveEntry, bool> includeEntry)
    {
        using var fclZipArchive =
            new System.IO.Compression.ZipArchive(
                new MemoryStream(zipArchive.ToArray()),
                System.IO.Compression.ZipArchiveMode.Read);

        foreach (var entry in fclZipArchive.Entries)
        {
            if (!includeEntry(entry))
                continue;

            using var entryStream = entry.Open();
            using var memoryStream = new MemoryStream();

            entryStream.CopyTo(memoryStream);

            var entryContent = memoryStream.ToArray();

            if (entryContent.Length != entry.Length)
            {
                throw new Exception(
                    "Error trying to read entry '" + entry.FullName + "': got " +
                    entryContent.Length + " bytes from entry instead of " + entry.Length);
            }

            yield return (entry.FullName, entryContent);
        }
    }
}
