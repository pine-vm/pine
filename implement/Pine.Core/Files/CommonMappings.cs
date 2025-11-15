using System;
using System.Collections.Generic;

namespace Pine.Core.Files;

/// <summary>
/// Provides utility methods for extracting tree structures from named binary blobs, supporting common archive formats
/// such as ZIP, TAR.GZ, TGZ, and GZ.
/// </summary>
/// <remarks>Use the methods in this class to interpret and extract hierarchical data from blobs based on their
/// file name and format. The extraction logic supports several widely used archive types, enabling consistent access to
/// tree representations regardless of the underlying compression or packaging. This class is intended for scenarios
/// where blob content may represent an archive or compressed file and a tree structure is required for further
/// processing.</remarks>
public class CommonMappings
{
    /// <summary>
    /// Extracts one or more tree structures from the specified named blob, interpreting its content as a supported
    /// archive or compressed format.
    /// </summary>
    /// <remarks>The method attempts to extract trees from the blob by interpreting its content according to
    /// the file extension. Supported formats include ZIP, TAR.GZ, TGZ, and GZ. If the content cannot be parsed as a
    /// supported archive, no trees are returned. The method does not throw exceptions for invalid or unsupported
    /// formats; instead, it silently skips extraction for those cases.</remarks>
    /// <param name="blobName">The name of the blob, including its file extension, which determines how the content is interpreted (e.g.,
    /// ".zip", ".tar.gz", ".tgz", ".gz").</param>
    /// <param name="blobContent">The binary content of the blob to be processed. Must represent a valid archive or compressed file format
    /// supported by the method.</param>
    /// <returns>An enumerable collection of tree structures extracted from the blob content. The collection may contain zero or
    /// more trees, depending on the format and validity of the content.</returns>

    public static IEnumerable<FileTree> ExtractTreesFromNamedBlob(
        string blobName,
        ReadOnlyMemory<byte> blobContent)
    {
        {
            FileTree? fromZipArchive = null;

            try
            {
                fromZipArchive =
                    PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(blobContent));
            }
            catch { }

            if (fromZipArchive is not null)
                yield return fromZipArchive;
        }

        if (blobName.EndsWith(".tar.gz", StringComparison.OrdinalIgnoreCase) ||
            blobName.EndsWith(".tgz", StringComparison.OrdinalIgnoreCase))
        {
            FileTree? fromTarArchive = null;

            try
            {
                fromTarArchive =
                    TarArchive.TreeWithStringPathFromTarArchive(BytesConversions.DecompressGzip(blobContent));
            }
            catch { }

            if (fromTarArchive is not null)
                yield return fromTarArchive;
        }
        else
        {
            if (blobName.EndsWith(".gz", StringComparison.OrdinalIgnoreCase))
            {
                ReadOnlyMemory<byte>? fromGzip = null;

                try
                {
                    fromGzip = BytesConversions.DecompressGzip(blobContent);
                }
                catch { }

                if (fromGzip is not null)
                    yield return FileTree.File(fromGzip.Value);
            }
        }
    }
}
