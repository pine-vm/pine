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

    private readonly List<FileSystemWatcher> _watchers = [];

    public event Action? Changed;

    public event Action<Exception>? Error;

    internal BatchInputWatcher(IReadOnlyList<string> files)
    {
        _files = new(files.Select(Path.GetFullPath), PathComparer);

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
            Changed?.Invoke();
    }

    private void OnRenamed(object sender, RenamedEventArgs args)
    {
        if (_files.Contains(args.FullPath) || _files.Contains(args.OldFullPath))
            Changed?.Invoke();
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
