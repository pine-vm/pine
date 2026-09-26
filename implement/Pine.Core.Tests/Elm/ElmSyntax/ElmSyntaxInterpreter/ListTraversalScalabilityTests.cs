using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using System;
using System.Diagnostics;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Synthetic scalability scenarios for recursive list traversals in the Elm syntax interpreter.
/// Uncons patterns (<c>x :: xs</c>) must take the tail without copying it, and accumulators built
/// with <c>concat [ [ item ], acc ]</c> (as in the kernel <c>List.map</c>, <c>List.range</c> and
/// <c>List.filter</c>) must not copy the accumulator on every step.
/// </summary>
/// <remarks>
/// With either operation copying the list, the scenarios below need on the order of 10^10 item
/// copies or more, taking many minutes; with linear behavior they complete within seconds.
/// </remarks>
public class ListTraversalScalabilityTests
{
    private const int ItemCount = 100_000;

    private static readonly TimeSpan s_timeLimit = TimeSpan.FromSeconds(30);

    private static readonly Lazy<ElmInterpreter.Prepared> s_prepared =
        new(
            () => InterpreterTestHelper.PrepareModulesFromSources(
                [
                InterpreterTestHelper.LoadKernelModuleSource("List.elm"),
                InterpreterTestHelper.LoadKernelModuleSource("Basics.elm"),
                InterpreterTestHelper.LoadKernelModuleSource("Maybe.elm"),
                InterpreterTestHelper.LoadKernelModuleSource("Char.elm"),
                """
                module ScalabilityTest exposing (..)


                sumByUncons : Int -> List Int -> Int
                sumByUncons acc list =
                    case list of
                        [] ->
                            acc

                        x :: xs ->
                            sumByUncons (acc + x) xs


                lengthByNestedUncons : Int -> List Int -> Int
                lengthByNestedUncons acc list =
                    case list of
                        _ :: _ :: rest ->
                            lengthByNestedUncons (acc + 2) rest

                        [ _ ] ->
                            acc + 1

                        [] ->
                            acc
                """
                ]));

    private static PineValue EvaluateWithinTimeLimit(string expression)
    {
        var prepared = s_prepared.Value;

        var stopwatch = Stopwatch.StartNew();

        var result = InterpreterTestHelper.EvaluateInModulesToPineValue(expression, prepared);

        stopwatch.Stop();

        stopwatch.Elapsed.Should().BeLessThan(s_timeLimit);

        return result;
    }

    private static readonly long s_sumOfRange = (long)ItemCount * (ItemCount + 1) / 2;

    [Fact]
    public void Uncons_pattern_traversal_scales_linearly()
    {
        var result =
            EvaluateWithinTimeLimit(
                "ScalabilityTest.sumByUncons 0 (List.range 1 " + ItemCount + ")");

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(s_sumOfRange));
    }

    [Fact]
    public void Nested_uncons_pattern_traversal_scales_linearly()
    {
        var result =
            EvaluateWithinTimeLimit(
                "ScalabilityTest.lengthByNestedUncons 0 (List.range 1 " + (ItemCount + 1) + ")");

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(ItemCount + 1));
    }

    [Fact]
    public void List_map_scales_linearly()
    {
        var result =
            EvaluateWithinTimeLimit(
                "ScalabilityTest.sumByUncons 0 (List.map (\\x -> x * 2) (List.range 1 " + ItemCount + "))");

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(2 * s_sumOfRange));
    }

    [Fact]
    public void List_foldl_over_mapped_list_scales_linearly()
    {
        var result =
            EvaluateWithinTimeLimit(
                "List.foldl (+) 0 (List.map (\\x -> x + 1) (List.range 0 " + (ItemCount - 1) + "))");

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(s_sumOfRange));
    }
}
