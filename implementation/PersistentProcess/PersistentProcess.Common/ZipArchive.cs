using System;
using System.Collections.Generic;
using System.IO;

namespace Kalmit
{
    static public class ZipArchive
    {
        static public byte[] ZipArchiveFromEntries(
            IReadOnlyCollection<(string name, byte[] content)> entries,
            System.IO.Compression.CompressionLevel compressionLevel = System.IO.Compression.CompressionLevel.Optimal)
        {
            var stream = new MemoryStream();

            using (var fclZipArchive = new System.IO.Compression.ZipArchive(stream, System.IO.Compression.ZipArchiveMode.Create, true))
            {
                foreach (var (entryName, entryContent) in entries)
                {
                    var entry = fclZipArchive.CreateEntry(entryName, compressionLevel);
                    using (var entryStream = entry.Open())
                    {
                        entryStream.Write(entryContent, 0, entryContent.Length);
                    }
                }
            }

            stream.Seek(0, SeekOrigin.Begin);

            var zipArchive = new byte[stream.Length];
            stream.Read(zipArchive, 0, (int)stream.Length);
            stream.Dispose();
            return zipArchive;
        }

        static public IEnumerable<(string name, byte[] content)> EntriesFromZipArchive(byte[] zipArchive)
        {
            using (var fclZipArchive = new System.IO.Compression.ZipArchive(new MemoryStream(zipArchive), System.IO.Compression.ZipArchiveMode.Read))
            {
                foreach (var entry in fclZipArchive.Entries)
                {
                    var entryContent = new byte[entry.Length];

                    using (var entryStream = entry.Open())
                    {
                        if (entryStream.Read(entryContent, 0, entryContent.Length) != entryContent.Length)
                            throw new NotImplementedException();
                    }

                    yield return (entry.FullName, entryContent);
                }
            }
        }
    }
}
