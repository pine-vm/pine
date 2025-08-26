using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Pine.Core.IO;


/// <summary>
/// <see cref="IFileStore"/> implementation backed by the local file system via <see cref="File"/> APIs.
/// </summary>
/// <param name="directoryPath">Root directory where this file store operates. All paths are relative to this root.</param>
/// <param name="retryOptions">Configuration for retrying transient I/O failures (e.g., sharing violations).</param>
public class FileStoreFromSystemIOFile(
    string directoryPath,
    FileStoreFromSystemIOFile.FileStoreRetryOptions retryOptions) : IFileStore
{
    /// <summary>
    /// Absolute or relative root directory for this file store instance.
    /// </summary>
    private readonly string _directoryPath =
        directoryPath
        ??
        throw new ArgumentNullException(nameof(directoryPath));

    /// <summary>
    /// Options controlling retry behavior for I/O operations.
    /// </summary>
    private readonly FileStoreRetryOptions _retryOptions =
        retryOptions
        ??
        throw new ArgumentNullException(nameof(retryOptions));

    /// <summary>
    /// Initializes a new instance operating under <paramref name="directoryPath"/> with retries disabled.
    /// </summary>
    /// <param name="directoryPath">Root directory where this file store operates. All paths are relative to this root.</param>
    public FileStoreFromSystemIOFile(string directoryPath) : this(directoryPath, FileStoreRetryOptions.NoRetry)
    {
    }

    /// <summary>
    /// Options that control retry behavior for file system operations.
    /// </summary>
    /// <param name="MaxRetryAttempts">Maximum number of retry attempts after the initial try.</param>
    /// <param name="InitialRetryDelay">Initial delay before the first retry attempt.</param>
    /// <param name="MaxRetryDelay">Upper bound for delay between retries (exponential backoff caps at this value).</param>
    public record FileStoreRetryOptions(
        int MaxRetryAttempts,
        TimeSpan InitialRetryDelay,
        TimeSpan MaxRetryDelay)
    {
        /// <summary>
        /// Gets a configuration that disables retries
        /// </summary>
        public static readonly FileStoreRetryOptions NoRetry =
            new(MaxRetryAttempts: 0, TimeSpan.Zero, TimeSpan.Zero);
    }

    /// <summary>
    /// Combines the root directory with the given componentized <paramref name="path"/>, validating that
    /// no component contains directory separator characters.
    /// </summary>
    /// <param name="path">Componentized path relative to the root directory.</param>
    /// <returns>The full file system path.</returns>
    /// <exception cref="ArgumentException">Thrown when any path component contains a directory separator.</exception>
    private string CombinePath(IImmutableList<string> path)
    {
        for (var i = 0; i < path.Count; i++)
        {
            var pathComponent = path[i];

            if (pathComponent.Contains('\\') || pathComponent.Contains('/'))
            {
                throw new ArgumentException(
                    "Invalid character in path component '" + pathComponent + "' at index " + i);
            }
        }

        return Path.Combine([_directoryPath, .. path]);
    }

    /// <inheritdoc />
    public void SetFileContent(
        IImmutableList<string> path,
        ReadOnlyMemory<byte> fileContent)
    {
        var filePath = CombinePath(path);

        var directoryPath = Path.GetDirectoryName(filePath);

        if (directoryPath is not null)
            Directory.CreateDirectory(directoryPath);

        ExecuteWithRetry(() =>
        {
            using var fileStream = new FileStream(filePath, FileMode.Create, FileAccess.Write);

            fileStream.Write(fileContent.Span);

            fileStream.Flush();
            return true;
        });
    }

    /// <inheritdoc />
    public void AppendFileContent(
        IImmutableList<string> path,
        ReadOnlyMemory<byte> fileContent)
    {
        var filePath = CombinePath(path);

        var directoryPath = Path.GetDirectoryName(filePath);

        if (directoryPath is not null)
            Directory.CreateDirectory(directoryPath);

        using var fileStream = new FileStream(filePath, FileMode.Append, FileAccess.Write);

        fileStream.Write(fileContent.Span);

        fileStream.Flush();
    }

    /// <inheritdoc />
    public ReadOnlyMemory<byte>? GetFileContent(
        IImmutableList<string> path)
    {
        var filePath = CombinePath(path);

        return ExecuteWithRetry<ReadOnlyMemory<byte>?>(() =>
        {
            try
            {
                return File.ReadAllBytes(filePath);
            }
            catch (FileNotFoundException)
            {
                return null;
            }
            catch (DirectoryNotFoundException)
            {
                return null;
            }
        });
    }

    /// <inheritdoc />
    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(
        IImmutableList<string> directoryPath)
    {
        var fileSystemDirectoryPath = CombinePath(directoryPath);

        return
            ExecuteWithRetry<IEnumerable<IImmutableList<string>>>(() =>
            {
                if (!Directory.Exists(fileSystemDirectoryPath))
                    return [];

                return
                    Directory.GetFiles(fileSystemDirectoryPath, "*", SearchOption.AllDirectories)
                    .Order()
                    .Select(filePath =>
                    Path.GetRelativePath(fileSystemDirectoryPath, filePath).Split(Path.DirectorySeparatorChar)
                    .ToImmutableList());
            });
    }

    /// <inheritdoc />
    public void DeleteFile(IImmutableList<string> path)
    {
        var fileSystemPath = CombinePath(path);

        ExecuteWithRetry(() =>
        {
            if (!File.Exists(fileSystemPath))
                return true;

            File.Delete(fileSystemPath);
            return true;
        });
    }

    /// <summary>
    /// Executes the provided operation while applying the retry policy configured for this instance.
    /// </summary>
    /// <typeparam name="T">The operation result type.</typeparam>
    /// <param name="operation">The operation to execute.</param>
    /// <returns>The result of <paramref name="operation"/>.</returns>
    private T ExecuteWithRetry<T>(Func<T> operation) =>
        ExecuteWithRetry(operation, _retryOptions);

    /// <summary>
    /// Executes the provided <paramref name="operation"/> with retry semantics for transient I/O errors
    /// (currently sharing violations), using the supplied <paramref name="retryOptions"/> to control backoff.
    /// </summary>
    /// <typeparam name="T">The operation result type.</typeparam>
    /// <param name="operation">The operation to execute.</param>
    /// <param name="retryOptions">Retry policy configuration.</param>
    /// <returns>The result of the successful operation.</returns>
    /// <exception cref="IOException">Re-thrown when the maximum number of retry attempts is exceeded.</exception>
    public static T ExecuteWithRetry<T>(
        Func<T> operation,
        FileStoreRetryOptions retryOptions)
    {
        // If retries are disabled, execute the operation directly
        if (retryOptions.MaxRetryAttempts <= 0)
        {
            return operation();
        }

        var attempts = 0;
        var delay = retryOptions.InitialRetryDelay;

        while (true)
        {
            try
            {
                attempts++;
                return operation();
            }
            catch (IOException ex)
            when (ex.HResult is unchecked((int)0x80070020)) // ERROR_SHARING_VIOLATION
            {
                if (!(attempts <= retryOptions.MaxRetryAttempts))
                    throw; // Rethrow if we've reached max attempts

                System.Threading.Thread.Sleep(delay);

                // Increase delay with exponential backoff (up to max)
                delay =
                    TimeSpan.FromMilliseconds(
                        Math.Min(
                            delay.TotalMilliseconds * 2,
                            retryOptions.MaxRetryDelay.TotalMilliseconds));
            }
        }
    }
}
