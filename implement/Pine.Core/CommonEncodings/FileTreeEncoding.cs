using Pine.Core.Files;
using System.Collections.Generic;

namespace Pine.Core.CommonEncodings;

/// <summary>
/// The standard encoding of file trees as Pine values.
/// Emits the flat 2026 format and accepts both the 2026 and legacy 2025 formats.
/// </summary>
public static class FileTreeEncoding
{
    /// <summary>
    /// Parses a file tree by trying the flat 2026 format first and falling back to the legacy 2025 format.
    /// </summary>
    public static Result<IReadOnlyList<(int index, string name)>, FileTree> Parse(
        PineValue composition)
    {
        var parsed2026 = FileTreeEncoding2026.Parse(composition);

        if (parsed2026.IsOkOrNull() is not null)
            return parsed2026;

        return FileTreeEncoding2025.Parse(composition);
    }

    /// <summary>
    /// Encodes a file tree using the flat 2026 format.
    /// </summary>
    public static PineValue Encode(FileTree node) =>
        FileTreeEncoding2026.Encode(node);
}
