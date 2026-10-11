using System;
using System.Diagnostics;

namespace Pine.Core.Elm.Testing;

/// <summary>
/// Measures the lifetime of an optional instrumentation scope and ends both together.
/// </summary>
internal sealed class TimedScope : IDisposable
{
    private readonly Stopwatch _stopwatch = Stopwatch.StartNew();

    private readonly IDisposable? _scope;

    private bool _disposed;

    public TimedScope(Func<IDisposable?> enterScope)
    {
        _scope = enterScope();
    }

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
