using System;
using System.Diagnostics;

namespace Pine.Core.Elm.Testing;

/// <summary>
/// Measures the lifetime of an optional instrumentation scope and ends both together.
/// </summary>
internal sealed class TimedScope(Func<IDisposable?> enterScope) : IDisposable
{
    private readonly Stopwatch _stopwatch = Stopwatch.StartNew();

    private readonly IDisposable? _scope = enterScope();

    private bool _disposed;

    public TimeSpan Elapsed => _stopwatch.Elapsed;

    public void Dispose()
    {
        if (_disposed)
            return;

        _disposed = true;

        try
        {
            _scope?.Dispose();
        }
        finally
        {
            _stopwatch.Stop();
        }
    }
}
