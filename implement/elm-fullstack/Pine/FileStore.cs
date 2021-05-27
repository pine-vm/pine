using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Pine
{
    public interface IFileStoreReader
    {
        IReadOnlyList<byte> GetFileContent(IImmutableList<string> path);

        IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath);
    }

    public interface IFileStoreWriter
    {
        void SetFileContent(IImmutableList<string> path, IReadOnlyList<byte> fileContent);

        // TODO: Simplify IFileStoreWriter: Do we still need AppendFileContent there?
        void AppendFileContent(IImmutableList<string> path, IReadOnlyList<byte> fileContent);

        void DeleteFile(IImmutableList<string> path);
    }

    public class DelegatingFileStoreReader : IFileStoreReader
    {
        public Func<IImmutableList<string>, IReadOnlyList<byte>> GetFileContentDelegate;

        public Func<IImmutableList<string>, IEnumerable<IImmutableList<string>>> ListFilesInDirectoryDelegate;

        public IReadOnlyList<byte> GetFileContent(IImmutableList<string> path) => GetFileContentDelegate(path);

        public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath) =>
            ListFilesInDirectoryDelegate(directoryPath);
    }

    public class DelegatingFileStoreWriter : IFileStoreWriter
    {
        public Action<(IImmutableList<string> path, IReadOnlyList<byte> fileContent)> AppendFileContentDelegate;

        public Action<IImmutableList<string>> DeleteFileDelegate;

        public Action<(IImmutableList<string> path, IReadOnlyList<byte> fileContent)> SetFileContentDelegate;

        public void AppendFileContent(IImmutableList<string> path, IReadOnlyList<byte> fileContent) =>
            AppendFileContentDelegate((path, fileContent));

        public void DeleteFile(IImmutableList<string> path) =>
            DeleteFileDelegate(path);

        public void SetFileContent(IImmutableList<string> path, IReadOnlyList<byte> fileContent) =>
            SetFileContentDelegate((path, fileContent));
    }

    public interface IFileStore : IFileStoreReader, IFileStoreWriter
    {
    }

    public class FileStoreFromSystemIOFile : IFileStore
    {
        readonly string directoryPath;

        public FileStoreFromSystemIOFile(string directoryPath)
        {
            this.directoryPath = directoryPath ?? throw new ArgumentNullException(nameof(directoryPath));
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
            if (!(0 < directoryPath?.Length))
                return;

            var parentDirectory = Path.GetDirectoryName(directoryPath);

            if (parentDirectory != null)
                EnsureDirectoryExists(parentDirectory);

            if (!Directory.Exists(directoryPath))
                Directory.CreateDirectory(directoryPath);
        }

        public void SetFileContent(IImmutableList<string> path, IReadOnlyList<byte> fileContent)
        {
            var filePath = CombinePath(path);

            var directoryPath = Path.GetDirectoryName(filePath);

            EnsureDirectoryExists(directoryPath);

            File.WriteAllBytes(filePath, fileContent as byte[] ?? fileContent.ToArray());
        }

        public void AppendFileContent(IImmutableList<string> path, IReadOnlyList<byte> fileContent)
        {
            var filePath = CombinePath(path);

            var directoryPath = Path.GetDirectoryName(filePath);

            EnsureDirectoryExists(directoryPath);

            using var fileStream = new FileStream(filePath, FileMode.Append, FileAccess.Write);

            fileStream.Write(fileContent as byte[] ?? fileContent.ToArray());
        }

        public IReadOnlyList<byte> GetFileContent(IImmutableList<string> path)
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

        public void DeleteFile(IImmutableList<string> path)
        {
            var fileSystemPath = CombinePath(path);

            if (!File.Exists(fileSystemPath))
                return;

            File.Delete(fileSystemPath);
        }
    }

    public class FileStoreFromWriterAndReader : IFileStore
    {
        readonly IFileStoreWriter writer;

        readonly IFileStoreReader reader;

        public FileStoreFromWriterAndReader(IFileStoreWriter writer, IFileStoreReader reader)
        {
            this.writer = writer;
            this.reader = reader;
        }

        public void AppendFileContent(IImmutableList<string> path, IReadOnlyList<byte> fileContent) =>
            writer.AppendFileContent(path, fileContent);

        public void DeleteFile(IImmutableList<string> path) =>
            writer.DeleteFile(path);

        public void SetFileContent(IImmutableList<string> path, IReadOnlyList<byte> fileContent) =>
            writer.SetFileContent(path, fileContent);

        public IReadOnlyList<byte> GetFileContent(IImmutableList<string> path) =>
            reader.GetFileContent(path);

        public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath) =>
            reader.ListFilesInDirectory(directoryPath);
    }

    static public class FileStoreExtension
    {
        static public IFileStoreReader WithMappedPath(
            this IFileStoreReader originalFileStore, System.Func<IImmutableList<string>, IImmutableList<string>> pathMap) =>
            new DelegatingFileStoreReader
            {
                GetFileContentDelegate = originalPath => originalFileStore.GetFileContent(pathMap(originalPath)),
                ListFilesInDirectoryDelegate = originalPath => originalFileStore.ListFilesInDirectory(pathMap(originalPath)),
            };

        static public IFileStoreReader ForSubdirectory(
            this IFileStoreReader originalFileStore, string directoryName) =>
            WithMappedPath(originalFileStore, originalPath => originalPath.Insert(0, directoryName));

        static public IFileStoreWriter WithMappedPath(
            this IFileStoreWriter originalFileStore, System.Func<IImmutableList<string>, IImmutableList<string>> pathMap) =>
            new DelegatingFileStoreWriter
            {
                SetFileContentDelegate = pathAndFileContent => originalFileStore.SetFileContent(pathMap(pathAndFileContent.path), pathAndFileContent.fileContent),
                AppendFileContentDelegate = pathAndFileContent => originalFileStore.AppendFileContent(pathMap(pathAndFileContent.path), pathAndFileContent.fileContent),
                DeleteFileDelegate = originalPath => originalFileStore.DeleteFile(pathMap(originalPath)),
            };

        static public IFileStoreWriter ForSubdirectory(
            this IFileStoreWriter originalFileStore, string directoryName) =>
            WithMappedPath(originalFileStore, originalPath => originalPath.Insert(0, directoryName));
    }
}
