using System;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.WebBrowserTesting;

internal static class PlaywrightTaskExtensions
{
    public static async Task WaitForPlaywrightAsync(
        this Task operation,
        TimeSpan timeout,
        CancellationToken cancellationToken,
        Func<Task> abortOperation)
    {
        try
        {
            await operation.WaitAsync(timeout, cancellationToken).ConfigureAwait(false);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            await TryAbortAsync(abortOperation, timeout).ConfigureAwait(false);
            throw;
        }
        catch (TimeoutException)
        {
            await TryAbortAsync(abortOperation, timeout).ConfigureAwait(false);
            throw;
        }
    }

    public static async Task<T> WaitForPlaywrightAsync<T>(
        this Task<T> operation,
        TimeSpan timeout,
        CancellationToken cancellationToken,
        Func<Task> abortOperation)
    {
        try
        {
            return await operation.WaitAsync(timeout, cancellationToken).ConfigureAwait(false);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            await TryAbortAsync(abortOperation, timeout).ConfigureAwait(false);
            throw;
        }
        catch (TimeoutException)
        {
            await TryAbortAsync(abortOperation, timeout).ConfigureAwait(false);
            throw;
        }
    }

    private static async Task TryAbortAsync(Func<Task> abortOperation, TimeSpan timeout)
    {
        try
        {
            await abortOperation().WaitAsync(timeout).ConfigureAwait(false);
        }
        catch
        {
        }
    }
}
