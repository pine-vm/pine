using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using System;
using System.Diagnostics;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Internal;

/// <summary>
/// Synthetic scalability scenarios for the list operations that recursive list traversals in the
/// Elm syntax interpreter depend on: taking the tail of a list (<c>x :: xs</c>) and building an
/// accumulator by prepending (<c>concat [ [ item ], acc ]</c>, as in <c>List.map</c>).
/// </summary>
/// <remarks>
/// The input sizes are chosen so that a linear implementation completes within milliseconds while
/// an implementation copying the list on every step (quadratic in total) needs on the order of
/// 10^11 item copies, far beyond the time limits asserted here.
/// </remarks>
public class PineValueInProcessListScalabilityTests
{
    private const int ItemCount = 500_000;

    private static readonly TimeSpan s_timeLimit = TimeSpan.FromSeconds(10);

    private static PineValueInProcess IntegerItem(int value) =>
        PineValueInProcess.Create(IntegerEncoding.EncodeSignedInteger(value));

    private static PineValueInProcess InProcessListOfIntegers(int count) =>
        PineValueInProcess.CreateList(
            [.. Enumerable.Range(0, count).Select(IntegerItem)]);

    [Fact]
    public void Repeated_skip_one_over_in_process_list_scales_linearly()
    {
        var list = InProcessListOfIntegers(ItemCount);

        var stopwatch = Stopwatch.StartNew();

        var remaining = list;
        var visited = 0;

        while (remaining.GetLength() > 0)
        {
            remaining.GetElementAt(0).Should().BeSameAs(list.GetElementAt(visited));

            remaining = PineValueInProcess.Skip(1, remaining);
            ++visited;
        }

        stopwatch.Stop();

        visited.Should().Be(ItemCount);
        stopwatch.Elapsed.Should().BeLessThan(s_timeLimit);
    }

    [Fact]
    public void Repeated_skip_one_over_evaluated_list_scales_linearly()
    {
        var list =
            PineValueInProcess.Create(
                PineValue.List(
                    [.. Enumerable.Range(0, ItemCount).Select(i => IntegerEncoding.EncodeSignedInteger(i))]));

        var stopwatch = Stopwatch.StartNew();

        var remaining = list;
        var visited = 0;

        while (remaining.GetLength() > 0)
        {
            remaining = PineValueInProcess.Skip(1, remaining);
            ++visited;
        }

        stopwatch.Stop();

        visited.Should().Be(ItemCount);
        stopwatch.Elapsed.Should().BeLessThan(s_timeLimit);
    }

    [Fact]
    public void Slices_of_slices_select_the_expected_items()
    {
        var list = InProcessListOfIntegers(100);

        var slice =
            PineValueInProcess.Take(
                40,
                PineValueInProcess.Skip(7, PineValueInProcess.Skip(3, list)));

        var nested =
            PineValueInProcess.Skip(
                5,
                PineValueInProcess.Take(30, slice));

        slice.Evaluate().Should().Be(
            PineValue.List(
                [.. Enumerable.Range(10, 40).Select(i => IntegerEncoding.EncodeSignedInteger(i))]));

        nested.Evaluate().Should().Be(
            PineValue.List(
                [.. Enumerable.Range(15, 25).Select(i => IntegerEncoding.EncodeSignedInteger(i))]));
    }

    [Fact]
    public void Accumulating_by_prepending_single_items_scales_linearly()
    {
        var stopwatch = Stopwatch.StartNew();

        var accumulator = PineValueInProcess.EmptyList;

        for (var i = 0; i < ItemCount; ++i)
        {
            accumulator =
                PineValueInProcess.Concat(
                    PineValueInProcess.CreateList(
                        [PineValueInProcess.CreateList([IntegerItem(i)]), accumulator]));
        }

        var reversed = PineValueInProcess.Reverse(accumulator);

        stopwatch.Stop();

        stopwatch.Elapsed.Should().BeLessThan(s_timeLimit);

        reversed.GetLength().Should().Be(ItemCount);

        reversed.Evaluate().Should().Be(
            PineValue.List(
                [.. Enumerable.Range(0, ItemCount).Select(i => IntegerEncoding.EncodeSignedInteger(i))]));
    }

    [Fact]
    public void Prepending_to_an_older_accumulator_does_not_change_newer_accumulators()
    {
        static PineValueInProcess Prepend(int item, PineValueInProcess accumulator) =>
            PineValueInProcess.Concat(
                PineValueInProcess.CreateList(
                    [PineValueInProcess.CreateList([IntegerItem(item)]), accumulator]));

        static PineValue Expected(params int[] items) =>
            PineValue.List([.. items.Select(i => IntegerEncoding.EncodeSignedInteger(i))]);

        var baseItems = Enumerable.Range(0, 40).Reverse().ToArray();

        var common = PineValueInProcess.EmptyList;

        foreach (var item in Enumerable.Range(0, 40))
        {
            common = Prepend(item, common);
        }

        // Branch the accumulator: both continuations extend the same older list.
        var branchA = Prepend(100, common);
        var branchB = Prepend(200, common);
        var branchA2 = Prepend(101, branchA);
        var branchB2 = Prepend(201, branchB);

        common.Evaluate().Should().Be(Expected(baseItems));
        branchA.Evaluate().Should().Be(Expected([100, .. baseItems]));
        branchB.Evaluate().Should().Be(Expected([200, .. baseItems]));
        branchA2.Evaluate().Should().Be(Expected([101, 100, .. baseItems]));
        branchB2.Evaluate().Should().Be(Expected([201, 200, .. baseItems]));

        // Slices of accumulators observe the same items.
        PineValueInProcess.Skip(1, branchB2).Evaluate().Should().Be(Expected([200, .. baseItems]));
    }

    [Fact]
    public void Prepending_multiple_items_keeps_their_order()
    {
        var back = InProcessListOfIntegers(50);

        var front = PineValueInProcess.CreateList([IntegerItem(-3), IntegerItem(-2), IntegerItem(-1)]);

        var once =
            PineValueInProcess.Concat(PineValueInProcess.CreateList([front, back]));

        var twice =
            PineValueInProcess.Concat(PineValueInProcess.CreateList([front, once]));

        once.Evaluate().Should().Be(
            PineValue.List(
                [.. Enumerable.Range(-3, 53).Select(i => IntegerEncoding.EncodeSignedInteger(i))]));

        twice.Evaluate().Should().Be(
            PineValue.List(
                [
                .. Enumerable.Range(-3, 3).Select(i => IntegerEncoding.EncodeSignedInteger(i)),
                .. Enumerable.Range(-3, 53).Select(i => IntegerEncoding.EncodeSignedInteger(i)),
                ]));
    }
}
