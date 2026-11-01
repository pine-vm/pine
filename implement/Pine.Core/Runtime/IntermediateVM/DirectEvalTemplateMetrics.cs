using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Opt-in measurements of simple direct-eval templates, separate from structural chain plans.
/// Retains expression identities for the lifetime of the owning work profile; timings and bytes
/// include observer overhead and must not be used as uninstrumented throughput measurements.
/// </summary>
public sealed class DirectEvalTemplateMetrics
{
    private sealed class Entry
    {
        public long Parses;

        public long ParseHits;

        public long Attempts;

        public long Applications;

        public long Instructions;

        public long Lists;

        public long ListItems;

        public long PhysicalEntries;

        public long Wrappers;

        public long Arrays;

        public long ArrayItems;

        public long MemoHits;

        public long Bytes;

        public long Ticks;
    }

    private readonly Dictionary<Expression, Entry> _entries = new(ReferenceEqualityComparer.Instance);

    private long _recognitionProbes;

    private long _rejectedEval;

    private long _rejectedBuiltin;

    private long _accepted;

    private long _parseProbes;

    private long _parseHits;

    private long _parseMisses;

    private long _parseFailures;

    private long _parseAddedEntries;

    private long _parseBytes;

    private long _parseTicks;

    internal void Recognize(Expression expression)
    {
        ++_recognitionProbes;

        if (expression.EvalCount > 0)
            ++_rejectedEval;

        else if (expression.BuiltinCount > 0)
            ++_rejectedBuiltin;

        else
            ++_accepted;
    }

    internal void Parsed(
        Expression? expression, bool hit, int addedEntries, long allocatedBytes, long ticks)
    {
        ++_parseProbes;

        if (hit)
            ++_parseHits;

        else
            ++_parseMisses;

        _parseAddedEntries += addedEntries;
        _parseBytes += allocatedBytes;
        _parseTicks += ticks;

        if (expression is null)
        {
            ++_parseFailures;
            return;
        }

        if (expression.EvalCount > 0 || expression.BuiltinCount > 0)
            return;

        var entry = GetEntry(expression);
        ++entry.Parses;

        if (hit)
            ++entry.ParseHits;
    }

    private Entry GetEntry(Expression expression)
    {
        if (_entries.TryGetValue(expression, out var entry))
            return entry;

        entry = new Entry();
        _entries.Add(expression, entry);
        return entry;
    }

    internal (Internal.PineValueInProcess value, EvalWorkCounters evalWorkCounters)? Apply(
        Expression expression, Internal.PineValueInProcess environment, PineVMWorkProfile profile)
    {
        var entry = GetEntry(expression);
        ++entry.Attempts;
        var entries = profile[PineVMWorkProfile.Metric.EvalDirectEntries];
        var wrappers = profile[PineVMWorkProfile.Metric.WrapperAllocations];
        var arrays = profile[PineVMWorkProfile.Metric.EvalDirectListArrays];
        var items = profile[PineVMWorkProfile.Metric.EvalDirectListItems];
        var memo = profile[PineVMWorkProfile.Metric.DirectConstructorMemoHits];
        var bytes = GC.GetAllocatedBytesForCurrentThread();
        var ticks = Stopwatch.GetTimestamp();

        try
        {
            var result = PineVM.EvalDirect(expression, environment);

            if (result is { } completed)
            {
                ++entry.Applications;
                entry.Instructions += completed.evalWorkCounters.InstructionCount;
                entry.Lists += completed.evalWorkCounters.BuildListCount;
                entry.ListItems += completed.evalWorkCounters.BuildListItemCount;
            }

            return result;
        }
        finally
        {
            entry.Ticks += Stopwatch.GetTimestamp() - ticks;
            entry.Bytes += GC.GetAllocatedBytesForCurrentThread() - bytes;
            entry.PhysicalEntries += profile[PineVMWorkProfile.Metric.EvalDirectEntries] - entries;
            entry.Wrappers += profile[PineVMWorkProfile.Metric.WrapperAllocations] - wrappers;
            entry.Arrays += profile[PineVMWorkProfile.Metric.EvalDirectListArrays] - arrays;
            entry.ArrayItems += profile[PineVMWorkProfile.Metric.EvalDirectListItems] - items;
            entry.MemoHits += profile[PineVMWorkProfile.Metric.DirectConstructorMemoHits] - memo;
        }
    }

    /// <summary>
    /// Exports numeric measurements. Parse probes cover all profiled parse-cache callers;
    /// per-template rows restrict these to shapes subsequently applied by the simple shortcut.
    /// Ids are report-local, not stable identifiers for comparing different runs.
    /// </summary>
    public object Snapshot()
    {
        var applied = _entries.Where(pair => pair.Value.Attempts > 0).ToArray();

        return
            new
            {
                RecognitionProbes = _recognitionProbes,
                RejectedEval = _rejectedEval,
                RejectedBuiltin = _rejectedBuiltin,
                AcceptedShapes = _accepted,
                ParseProbes = _parseProbes,
                ParseHits = _parseHits,
                ParseMisses = _parseMisses,
                ParseFailures = _parseFailures,
                ParseAddedEntries = _parseAddedEntries,
                ParseAllocatedBytes = _parseBytes,
                ParseWallMilliseconds = _parseTicks * 1000.0 / Stopwatch.Frequency,
                DistinctAppliedTemplates = applied.Length,
                ApplicationAttempts = applied.Sum(pair => pair.Value.Attempts),
                Applications = applied.Sum(pair => pair.Value.Applications),
                AppliedTemplateParseProbes = applied.Sum(pair => pair.Value.Parses),
                AppliedTemplateParseHits = applied.Sum(pair => pair.Value.ParseHits),
                LogicalInstructions = applied.Sum(pair => pair.Value.Instructions),
                LogicalLists = applied.Sum(pair => pair.Value.Lists),
                LogicalListItems = applied.Sum(pair => pair.Value.ListItems),
                PhysicalEntries = applied.Sum(pair => pair.Value.PhysicalEntries),
                FreshWrappers = applied.Sum(pair => pair.Value.Wrappers),
                ChildArrays = applied.Sum(pair => pair.Value.Arrays),
                ChildArrayItems = applied.Sum(pair => pair.Value.ArrayItems),
                InvocationLocalMemoHits = applied.Sum(pair => pair.Value.MemoHits),
                ApplicationAllocatedBytes = applied.Sum(pair => pair.Value.Bytes),
                ApplicationWallMilliseconds = applied.Sum(pair => pair.Value.Ticks) * 1000.0 / Stopwatch.Frequency,
                Templates =
                applied.Select(
                    (pair, index) => new
                    {
                        Id = index,
                        Nodes = pair.Key.SubexpressionCount + 1,
                        pair.Key.MaxDepth,
                        Kind = pair.Key.GetType().Name,
                        pair.Value.Parses,
                        pair.Value.ParseHits,
                        pair.Value.Attempts,
                        pair.Value.Applications,
                        pair.Value.Instructions,
                        pair.Value.Lists,
                        pair.Value.ListItems,
                        pair.Value.PhysicalEntries,
                        pair.Value.Wrappers,
                        pair.Value.Arrays,
                        pair.Value.ArrayItems,
                        pair.Value.MemoHits,
                        AllocatedBytes = pair.Value.Bytes,
                        WallMilliseconds = pair.Value.Ticks * 1000.0 / Stopwatch.Frequency
                    }).OrderByDescending(entry => entry.Applications).ToArray()
            };
    }
}
