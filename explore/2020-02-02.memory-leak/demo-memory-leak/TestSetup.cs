using System.Collections.Generic;
using System.Collections.Immutable;

namespace Kalmit.PersistentProcess.Test
{
    public class TestSetup
    {
        static public IReadOnlyCollection<(string filePath, byte[] fileContent)> GetElmAppFromFilePath(
            string filePath) =>
            Filesystem.GetAllFilesFromDirectory(filePath)
            .OrderBy(file => file.filePath)
            .ToImmutableList();
    }
}
