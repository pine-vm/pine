using System;
using System.IO;
using System.Threading;

namespace Pine.Core.IO;

/// <summary>Publishes complete file contents using a temporary sibling and replacement.</summary>
public static class AtomicFile
{
    /// <summary>Replaces a file only after writing all bytes and checking cancellation.</summary>
    public static void WriteAllBytes(string destination, ReadOnlyMemory<byte> bytes, CancellationToken token = default)
    {
        destination = Path.GetFullPath(destination);
        var temporary = destination + "." + Guid.NewGuid().ToString("N") + ".tmp";

        try
        {
            token.ThrowIfCancellationRequested();
            File.WriteAllBytes(temporary, bytes.Span);
            token.ThrowIfCancellationRequested();
            File.Move(temporary, destination, overwrite: true);
        }
        finally
        {
            if (File.Exists(temporary))
                File.Delete(temporary);
        }
    }
}
