using Pine.Core.Addressing;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Linq;
using System.Text;
using System.Threading;

namespace Pine.Core.Elm.LanguageServer;

internal sealed class InvocationCacheStatisticsLog : IDisposable
{
    internal static readonly TimeSpan Interval = TimeSpan.FromMinutes(5);

    private readonly Lock _lock = new();

    private readonly Func<InvocationCacheStatisticsSnapshot?> _getStatistics;

    private readonly Action<string> _log;

    private readonly ITimer _timer;

    private bool _disposed;

    internal InvocationCacheStatisticsLog(
        Func<InvocationCacheStatisticsSnapshot?> getStatistics,
        Action<string> log,
        TimeProvider? timeProvider = null)
    {
        _getStatistics = getStatistics;
        _log = log;

        _timer =
            (timeProvider ?? TimeProvider.System).CreateTimer(
                _ => Report(),
                null,
                Interval,
                Interval);
    }

    private void Report()
    {
        lock (_lock)
        {
            if (_disposed)
                return;

            try
            {
                if (_getStatistics() is { } snapshot)
                    _log(Format(snapshot));
            }
            catch (Exception exception)
            {
                try
                {
                    _log("Invocation-cache statistics report failed: " + exception);
                }
                catch
                {
                    // A failed diagnostic sink must not terminate the server's timer thread.
                }
            }
        }
    }

    internal static string Format(InvocationCacheStatisticsSnapshot snapshot)
    {
        var text = new StringBuilder("Invocation-cache statistics (cumulative VM-facing operations)");
        text.AppendLine();
        text.Append("Totals: ").AppendLine(FormatCounters(snapshot.Totals));

        if (snapshot.SharedCacheGenerations is { } generations)
        {
            text.AppendLine(
                FormattableString.Invariant(
                    $"Shared storage: unique entries={generations.UniqueEntryCount}, physical entries={generations.PhysicalEntryCount}, promotions={generations.Promotions}, rotations={generations.Rotations}, retired slots={generations.RetiredEntries}"));
        }

        var expressions =
            snapshot.PerExpression.Select(
                entry =>
                (Hash: Convert.ToHexStringLower(PineValueHashFlat.ComputeHashForValue(entry.Key).hashBytes.Span),
                Counters: entry.Value))
            .OrderByDescending(entry => entry.Counters.HitCount)
            .ThenBy(entry => entry.Hash, StringComparer.Ordinal);

        foreach (var (hash, counters) in expressions)
            text.Append("Expression ").Append(hash).Append(": ").AppendLine(FormatCounters(counters));

        return text.ToString();
    }

    private static string FormatCounters(InvocationCacheExpressionStatistics counters) =>
        FormattableString.Invariant(
            $"admissions={counters.AdmissionCount}, admission attempts={counters.AdmissionAttemptCount}, rejected admissions={counters.RejectedAdmissionCount}, lookups={counters.LookupCount}, hits={counters.HitCount}, misses={counters.MissCount}, hit rate={counters.HitRate * 100:F2}%, expression checks={counters.ExpressionCheckCount}, skipped lookups={counters.SkippedLookupCount}");

    public void Dispose()
    {
        lock (_lock)
        {
            _disposed = true;
            _timer.Dispose();
        }
    }
}
