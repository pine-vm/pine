using System;
using System.IO;

namespace Pine.CLI;

internal interface IFileWatcher : IDisposable
{
    event Action? Changed;

    event Action<Exception>? Error;

    void Start();

    void Stop();
}

internal sealed class DirectoryFileWatcher : IFileWatcher
{
    private readonly FileSystemWatcher _watcher;

    public event Action? Changed;

    public event Action<Exception>? Error;

    internal DirectoryFileWatcher(string directory)
    {
        _watcher =
            new FileSystemWatcher(directory)
            {
                Filter = "*",
                IncludeSubdirectories = true,
                NotifyFilter =
                NotifyFilters.FileName | NotifyFilters.DirectoryName | NotifyFilters.LastWrite |
                NotifyFilters.Size | NotifyFilters.CreationTime | NotifyFilters.Attributes | NotifyFilters.Security,
            };

        _watcher.Changed += OnChanged;
        _watcher.Created += OnChanged;
        _watcher.Deleted += OnChanged;
        _watcher.Renamed += OnChanged;
        _watcher.Error += OnError;
    }

    private void OnChanged(object sender, FileSystemEventArgs args) => Changed?.Invoke();

    private void OnError(object sender, ErrorEventArgs args) => Error?.Invoke(args.GetException());

    public void Start() => _watcher.EnableRaisingEvents = true;

    public void Stop() => _watcher.EnableRaisingEvents = false;

    public void Dispose() => _watcher.Dispose();
}
