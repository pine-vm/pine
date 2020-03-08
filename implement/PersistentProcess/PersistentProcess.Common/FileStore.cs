using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Kalmit
{
    public interface IFileStore
    {
        void SetFileContent(IImmutableList<string> path, byte[] fileContent);

        void AppendFileContent(IImmutableList<string> path, byte[] fileContent);

        byte[] GetFileContent(IImmutableList<string> path);

        IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath);
    }

    public class FileStoreFromSystemIOFile : IFileStore
    {
        readonly string directoryPath;

        public FileStoreFromSystemIOFile(string directoryPath)
        {
            this.directoryPath = directoryPath;
        }

        string CombinePath(IImmutableList<string> path)
        {
            foreach (var pathComponent in path)
            {
                if (pathComponent.Contains('\\') || pathComponent.Contains('/'))
                    throw new System.ArgumentException("Invalid character in path component '" + pathComponent + "'.");
            }

            return Path.Combine(path.Insert(0, directoryPath).ToArray());
        }

        static void EnsureDirectoryExists(string directoryPath)
        {
            var parentDirectory = Path.GetDirectoryName(directoryPath);

            if (parentDirectory != null)
                EnsureDirectoryExists(parentDirectory);

            if (!Directory.Exists(directoryPath))
                Directory.CreateDirectory(directoryPath);
        }

        public void SetFileContent(IImmutableList<string> path, byte[] fileContent)
        {
            var filePath = CombinePath(path);

            var directoryPath = Path.GetDirectoryName(filePath);

            EnsureDirectoryExists(directoryPath);

            File.WriteAllBytes(filePath, fileContent);
        }

        public void AppendFileContent(IImmutableList<string> path, byte[] fileContent)
        {
            var filePath = CombinePath(path);

            var directoryPath = Path.GetDirectoryName(filePath);

            EnsureDirectoryExists(directoryPath);

            using (var fileStream = new FileStream(filePath, FileMode.Append, FileAccess.Write))
            {
                fileStream.Write(fileContent);
            }
        }

        public byte[] GetFileContent(IImmutableList<string> path)
        {
            var filePath = CombinePath(path);

            if (!File.Exists(filePath))
                return null;

            return File.ReadAllBytes(filePath);
        }

        public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath)
        {
            var fileSystemDirectoryPath = CombinePath(directoryPath);

            if (!Directory.Exists(fileSystemDirectoryPath))
                return ImmutableList<IImmutableList<string>>.Empty;

            return
                Directory.GetFiles(fileSystemDirectoryPath, "*", SearchOption.AllDirectories)
                .OrderBy(filePath => filePath)
                .Select(filePath => Path.GetRelativePath(fileSystemDirectoryPath, filePath).Split(Path.DirectorySeparatorChar).ToImmutableList());
        }
    }

    public class FileStoreWithMappedPath : IFileStore
    {
        readonly IFileStore originalFileStore;

        readonly System.Func<IImmutableList<string>, IImmutableList<string>> pathMap;

        public FileStoreWithMappedPath(IFileStore originalFileStore, System.Func<IImmutableList<string>, IImmutableList<string>> pathMap)
        {
            this.originalFileStore = originalFileStore;
            this.pathMap = pathMap;
        }

        public void AppendFileContent(IImmutableList<string> path, byte[] fileContent) =>
            originalFileStore.AppendFileContent(pathMap(path), fileContent);

        public byte[] GetFileContent(IImmutableList<string> path) =>
            originalFileStore.GetFileContent(pathMap(path));

        public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> path) =>
            originalFileStore.ListFilesInDirectory(pathMap(path));

        public void SetFileContent(IImmutableList<string> path, byte[] fileContent) =>
            originalFileStore.SetFileContent(pathMap(path), fileContent);
    }

    public class FileStoreFromSubdirectory : FileStoreWithMappedPath
    {
        public IFileStore fileStore;

        public FileStoreFromSubdirectory(IFileStore fileStore, string directoryName)
            :
            base(fileStore, originalPath => originalPath.Insert(0, directoryName))
        {
        }
    }
}
