using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// A histogram of invocation counts grouped by compiled expression, gathered from
/// the stack-frame entry events of one or more evaluation tasks.
///
/// <para>
/// Each entry in <see cref="InvocationCountPerExpression"/> maps a distinct
/// compiled expression (the <see cref="EnteredStackFrame.FrameExpression"/> value
/// reported by <see cref="ReportEnteredStackFrame"/>) to the number of times a
/// stack frame for that expression was entered.
/// </para>
///
/// <para>
/// The histogram shape (an <see cref="ImmutableDictionary{TKey, TValue}"/>) makes
/// it easy to combine histograms from multiple invocations via
/// <see cref="Aggregate(IEnumerable{InvocationCountReport})"/>: counts for the
/// same expression are summed, while expressions that appear in only one
/// histogram are added to the combined report unchanged.
/// </para>
/// </summary>
/// <param name="InvocationCountPerExpression">
/// Histogram of invocation counts keyed by the compiled expression that was
/// invoked. The number of entries is the number of distinct compiled
/// expressions invoked across the evaluation task(s) that produced the report.
/// </param>
public readonly record struct InvocationCountReport(
    ImmutableDictionary<Expression, long> InvocationCountPerExpression)
{
    /// <summary>
    /// An empty report with no invocations recorded.
    /// </summary>
    public static InvocationCountReport Empty { get; } =
        new(ImmutableDictionary<Expression, long>.Empty);

    /// <summary>
    /// The number of distinct compiled expressions for which at least one
    /// invocation was recorded in this report.
    /// </summary>
    public int CompiledExpressionCount =>
        InvocationCountPerExpression.Count;

    /// <summary>
    /// The total number of invocations recorded across all compiled expressions.
    /// </summary>
    public long InvocationCountTotal
    {
        get
        {
            long total = 0;

            foreach (var (_, count) in InvocationCountPerExpression)
            {
                total += count;
            }

            return total;
        }
    }

    /// <summary>
    /// The arithmetic mean of invocation counts across all compiled expressions
    /// in this report, rounded to the nearest integer. Returns zero when the
    /// report is empty.
    /// </summary>
    public long InvocationCountAverage
    {
        get
        {
            var compiledExpressionCount = CompiledExpressionCount;

            if (compiledExpressionCount is 0)
            {
                return 0;
            }

            var total = InvocationCountTotal;

            // Rounded division to the nearest integer (half away from zero for
            // non-negative totals, which is the only case that occurs here).
            return (total + (compiledExpressionCount / 2)) / compiledExpressionCount;
        }
    }

    /// <summary>
    /// The median of invocation counts across all compiled expressions in this
    /// report. For an even number of compiled expressions, returns the rounded
    /// arithmetic mean of the two middle values. Returns zero when the report
    /// is empty.
    /// </summary>
    public long InvocationCountMedian
    {
        get
        {
            var compiledExpressionCount = CompiledExpressionCount;

            if (compiledExpressionCount is 0)
            {
                return 0;
            }

            var sorted = SortedCounts();

            if ((compiledExpressionCount & 1) is 1)
            {
                return sorted[compiledExpressionCount / 2];
            }

            var lower = sorted[(compiledExpressionCount / 2) - 1];
            var upper = sorted[compiledExpressionCount / 2];

            // Rounded mean of the two middle values.
            return (lower + upper + 1) / 2;
        }
    }

    /// <summary>
    /// The 10th percentile of invocation counts across all compiled expressions
    /// in this report (nearest-rank by linear interpolation on sorted index).
    /// Returns zero when the report is empty.
    /// </summary>
    public long InvocationCountPercentile10 =>
        Percentile(0.10);

    /// <summary>
    /// The 90th percentile of invocation counts across all compiled expressions
    /// in this report (nearest-rank by linear interpolation on sorted index).
    /// Returns zero when the report is empty.
    /// </summary>
    public long InvocationCountPercentile90 =>
        Percentile(0.90);

    /// <summary>
    /// Returns the value at the given percentile <paramref name="fraction"/>
    /// (in [0, 1]) using linear interpolation on the 0-based sorted index
    /// (i.e. <c>index = round(fraction * (count - 1))</c>). Returns zero when
    /// the report is empty.
    /// </summary>
    private long Percentile(double fraction)
    {
        var compiledExpressionCount = CompiledExpressionCount;

        if (compiledExpressionCount is 0)
        {
            return 0;
        }

        var sorted = SortedCounts();

        var index = (int)System.Math.Round(fraction * (compiledExpressionCount - 1));

        if (index < 0)
        {
            index = 0;
        }
        else if (index >= compiledExpressionCount)
        {
            index = compiledExpressionCount - 1;
        }

        return sorted[index];
    }

    private long[] SortedCounts()
    {
        var sorted = new long[InvocationCountPerExpression.Count];

        var index = 0;

        foreach (var (_, count) in InvocationCountPerExpression)
        {
            sorted[index++] = count;
        }

        System.Array.Sort(sorted);

        return sorted;
    }

    /// <summary>
    /// Combines two reports into one by summing per-expression invocation counts.
    /// Expressions that appear in only one of the inputs are carried over unchanged.
    /// </summary>
    public static InvocationCountReport Add(
        InvocationCountReport a,
        InvocationCountReport b)
    {
        var builder = a.InvocationCountPerExpression.ToBuilder();

        foreach (var (expression, count) in b.InvocationCountPerExpression)
        {
            if (builder.TryGetValue(expression, out var existing))
            {
                builder[expression] = existing + count;
            }
            else
            {
                builder[expression] = count;
            }
        }

        return new InvocationCountReport(builder.ToImmutable());
    }

    /// <summary>
    /// Aggregates all <see cref="InvocationCountReport"/> instances in the given
    /// sequence into a single combined histogram. Returns an empty report if the
    /// sequence is empty.
    /// </summary>
    public static InvocationCountReport Aggregate(
        IEnumerable<InvocationCountReport> reports)
    {
        var builder = ImmutableDictionary.CreateBuilder<Expression, long>();

        foreach (var report in reports)
        {
            foreach (var (expression, count) in report.InvocationCountPerExpression)
            {
                if (builder.TryGetValue(expression, out var existing))
                {
                    builder[expression] = existing + count;
                }
                else
                {
                    builder[expression] = count;
                }
            }
        }

        return new InvocationCountReport(builder.ToImmutable());
    }
}

/// <summary>
/// Mutable accumulator that builds an <see cref="InvocationCountReport"/> by
/// observing each <see cref="EnteredStackFrame"/> event of one evaluation task.
///
/// <para>
/// Typical usage: create a builder, pass <see cref="Add"/> as the
/// <c>reportEnteredStackFrame</c> argument to
/// <see cref="PineVM.EvaluateExpressionOnCustomStack"/>, then call
/// <see cref="ToReport"/> after the evaluation finishes to obtain the histogram.
/// </para>
///
/// <para>
/// One builder per evaluation is the recommended pattern; combining histograms
/// from multiple evaluations is done after the fact via
/// <see cref="InvocationCountReport.Aggregate(System.Collections.Generic.IEnumerable{InvocationCountReport})"/>.
/// A single builder reused across multiple evaluations would silently merge their
/// counts, defeating the per-evaluation scoping motivation behind exposing the
/// callback at the eval entry point.
/// </para>
/// </summary>
public sealed class InvocationCountReportBuilder
{
    private readonly Dictionary<Expression, long> _counts = [];

    /// <summary>
    /// Records one invocation of <paramref name="enteredStackFrame"/>'s frame
    /// expression. Suitable as a <see cref="ReportEnteredStackFrame"/> callback.
    /// </summary>
    public void Add(in EnteredStackFrame enteredStackFrame)
    {
        var expression = enteredStackFrame.FrameExpression;

        if (_counts.TryGetValue(expression, out var existing))
        {
            _counts[expression] = existing + 1;
        }
        else
        {
            _counts[expression] = 1;
        }
    }

    /// <summary>
    /// Returns an immutable snapshot of the histogram accumulated so far.
    /// </summary>
    public InvocationCountReport ToReport() =>
        new(_counts.ToImmutableDictionary());
}
