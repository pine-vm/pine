using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Pine.CLI;

internal sealed class BatchInputWatcher : IFileWatcher
{
    internal static StringComparer PathComparer { get; } =
        OperatingSystem.IsWindows() ? StringComparer.OrdinalIgnoreCase : StringComparer.Ordinal;

    internal static string ResolveDirectoryLinks(string path)
    {
        var fullPath = Path.GetFullPath(path);

        if (Directory.Exists(fullPath))
            return Resolve(new DirectoryInfo(fullPath));

        return Path.Combine(Resolve(new DirectoryInfo(Path.GetDirectoryName(fullPath)!)), Path.GetFileName(fullPath));

        static string Resolve(DirectoryInfo directory)
        {
            if (directory.Parent is not { } parent)
                return directory.FullName;

            var resolved = Path.Combine(Resolve(parent), directory.Name);
            return new DirectoryInfo(resolved).ResolveLinkTarget(returnFinalTarget: true)?.FullName ?? resolved;
        }
    }

    private readonly HashSet<string> _files;

    private readonly string? _sourceDirectory;

    private readonly string _outputFile;

    private readonly Action<string> _onChanged;

    private readonly HashSet<string> _sourceDirectories = new(PathComparer);

    private readonly List<FileSystemWatcher> _watchers = [];

    public event Action? Changed;

    public event Action<Exception>? Error;

    internal BatchInputWatcher(
        IReadOnlyList<string> files,
        string? sourceDirectory,
        string outputFile,
        Action<string> onChanged)
    {
        _files = new(files.Select(Path.GetFullPath), PathComparer);
        _sourceDirectory = sourceDirectory;
        _outputFile = ResolveDirectoryLinks(outputFile);
        _onChanged = onChanged;

        try
        {
            foreach (var directory in _files.Select(file => Path.GetDirectoryName(file)!).Distinct(PathComparer))
            {
                var watcher =
                    new FileSystemWatcher(directory)
                    {
                        NotifyFilter = NotifyFilters.FileName | NotifyFilters.LastWrite | NotifyFilters.Size,
                    };

                _watchers.Add(watcher);
                watcher.Changed += OnChanged;
                watcher.Created += OnChanged;
                watcher.Deleted += OnChanged;
                watcher.Renamed += OnRenamed;
                watcher.Error += (_, args) => Error?.Invoke(args.GetException());
            }

            if (sourceDirectory is not null)
            {
                RememberDirectories(sourceDirectory);

                var watcher =
                    new FileSystemWatcher(sourceDirectory)
                    {
                        IncludeSubdirectories = true,
                        NotifyFilter =
                        NotifyFilters.FileName | NotifyFilters.DirectoryName |
                        NotifyFilters.LastWrite | NotifyFilters.Size,
                    };

                _watchers.Add(watcher);
                watcher.Changed += OnSourceChanged;
                watcher.Created += OnSourceChanged;
                watcher.Deleted += OnSourceChanged;
                watcher.Renamed += OnSourceRenamed;
                watcher.Error += (_, args) => Error?.Invoke(args.GetException());
            }
        }
        catch
        {
            Dispose();
            throw;
        }
    }

    private void OnChanged(object sender, FileSystemEventArgs args)
    {
        if (_files.Contains(args.FullPath))
            ReportChange(args.FullPath);
    }

    private void OnRenamed(object sender, RenamedEventArgs args)
    {
        if (_files.Contains(args.FullPath))
            ReportChange(args.FullPath);

        if (_files.Contains(args.OldFullPath))
            ReportChange(args.OldFullPath);
    }

    private void ReportChange(string path)
    {
        _onChanged(path);
        Changed?.Invoke();
    }

    private bool IsSourcePath(string path) =>
        !PathComparer.Equals(path, _outputFile) &&
        !Path.GetRelativePath(_sourceDirectory!, path).Split(Path.DirectorySeparatorChar)
        .Any(part => part is ".git" or "elm-stuff" or "bin" or "obj" or "node_modules");

    private bool RememberDirectories(string directory)
    {
        if (!IsSourcePath(directory))
            return false;

        var containsSources =
            Directory.EnumerateFiles(directory).Any(
                file =>
                IsSourcePath(file) &&
                (Path.GetExtension(file) is ".elm" || Path.GetFileName(file) is "elm.json"));

        foreach (var child in Directory.EnumerateDirectories(directory))
            containsSources |= RememberDirectories(child);

        if (containsSources)
            _sourceDirectories.Add(directory);

        return containsSources;
    }

    private void OnSourceChanged(object sender, FileSystemEventArgs args) => SourceChanged(args.FullPath);

    private void OnSourceRenamed(object sender, RenamedEventArgs args)
    {
        SourceChanged(args.OldFullPath);
        SourceChanged(args.FullPath);
    }

    private void SourceChanged(string path)
    {
        if (!IsSourcePath(path))
            return;

        try
        {
            bool isDirectory;

            lock (_sourceDirectories)
            {
                isDirectory = _sourceDirectories.Contains(path);

                if (Directory.Exists(path))
                {
                    isDirectory |= RememberDirectories(path);
                }
                else if (isDirectory)
                {
                    _sourceDirectories.RemoveWhere(
                        directory =>
                        PathComparer.Equals(directory, path) ||
                        directory.StartsWith(
                            path + Path.DirectorySeparatorChar,
                            OperatingSystem.IsWindows() ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal));
                }
            }

            var isSourceFile = Path.GetExtension(path) is ".elm" || Path.GetFileName(path) is "elm.json";

            if (isSourceFile)
            {
                lock (_sourceDirectories)
                {
                    for (var parent = Path.GetDirectoryName(path);
                        parent is not null && IsSourcePath(parent);
                        parent = Path.GetDirectoryName(parent))
                    {
                        _sourceDirectories.Add(parent);

                        if (PathComparer.Equals(parent, _sourceDirectory))
                            break;
                    }
                }
            }

            if (isDirectory || isSourceFile)
                ReportChange(path);
        }
        catch (Exception exception) when (exception is IOException or UnauthorizedAccessException)
        {
            Error?.Invoke(exception);
        }
    }

    public void Start()
    {
        foreach (var watcher in _watchers)
            watcher.EnableRaisingEvents = true;
    }

    public void Stop()
    {
        foreach (var watcher in _watchers)
            watcher.EnableRaisingEvents = false;
    }

    public void Dispose()
    {
        foreach (var watcher in _watchers)
            watcher.Dispose();
    }
}
