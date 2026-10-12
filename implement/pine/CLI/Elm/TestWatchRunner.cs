using System;
using System.IO;
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;

namespace Pine.CLI.Elm;

internal static class TestWatchRunner
{
    internal static async Task<int> RunAsync(
        string directory,
        Func<CancellationToken, int> run,
        Action onReady,
        Action<Exception> onError,
        CancellationToken cancellationToken,
        Func<string, ITestWatcher>? createWatcher = null)
    {
        var changes =
            Channel.CreateBounded<bool>(
                new BoundedChannelOptions(1)
                {
                    FullMode = BoundedChannelFullMode.DropWrite,
                    SingleReader = true,
                    SingleWriter = false,
                });

        Exception? watcherError = null;

        void Changed() => changes.Writer.TryWrite(true);

        void Failed(Exception exception)
        {
            Interlocked.Exchange(ref watcherError, exception);
            changes.Writer.TryWrite(true);
        }

        try
        {
            using var watcher =
                createWatcher?.Invoke(Path.GetFullPath(directory))
                ?? new FileSystemTestWatcher(Path.GetFullPath(directory));

            watcher.Changed += Changed;
            watcher.Error += Failed;

            try
            {
                watcher.Start();
                onReady();

                while (true)
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    if (Volatile.Read(ref watcherError) is { } error)
                    {
                        onError(error);
                        return 1;
                    }

                    // Leave events queued during the run so edits made while compiling or testing are not lost.
                    run(cancellationToken);
                    await changes.Reader.ReadAsync(cancellationToken);

                    do
                    {
                        await Task.Delay(TimeSpan.FromMilliseconds(150), cancellationToken);

                        if (Volatile.Read(ref watcherError) is { } debounceError)
                        {
                            onError(debounceError);
                            return 1;
                        }
                    }
                    while (changes.Reader.TryRead(out _));
                }
            }
            finally
            {
                watcher.Stop();
                watcher.Changed -= Changed;
                watcher.Error -= Failed;
            }
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return 130;
        }
        catch (Exception exception) when (
            exception is IOException or UnauthorizedAccessException or ArgumentException)
        {
            onError(exception);
            return 1;
        }
    }
}
