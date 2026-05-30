using AwesomeAssertions;
using Pine.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests;

/// <summary>
/// Investigation / regression tests for the change in commit
/// <c>c947c7c53215b058ec8c4f6c3cd3a908bd5c6d57</c> ("optimize via precompiled for
/// Elm core <c>Basics.compare</c> and <c>Dict.get</c>") that caused some integration
/// tests to run forever.
/// <para>
/// Root cause (see
/// <c>explore/2026-05-30.integration-tests-never-terminating-precompiled-leaves-regression.md</c>):
/// the central intermediate-VM factory <see cref="IntermediateVM.SetupVM.Create"/>
/// stopped sourcing its default precompiled-leaves dictionary from the bundled,
/// prebuilt dictionary (<see cref="Core.Bundle.BundledPineToDotnet"/>, embedded
/// <c>pine-default-leaves.dll</c>) and instead uses
/// <see cref="IntermediateVM.SetupVM.DefaultPrecompiledLeaves"/>, which only
/// contains the two newly added leaves. With ~144 hot-path .NET short-circuits gone,
/// the intermediate VM interprets the full recursive Pine expression graphs of those
/// functions, which makes the heavyweight integration tests never finish.
/// </para>
/// <para>
/// These tests guard the "union" fix: the default precompiled-leaves dictionary merges the
/// bundle with the new per-area leaves so that no bundled short-circuit is dropped.
/// </para>
/// </summary>
public class PrecompiledLeavesRegressionTests
{
    private static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> BundledLeaves()
    {
        var bundled =
            Core.Bundle.BundledPineToDotnet.LoadBundledTask.Result;

        bundled.Should().NotBeNull(
            because: "the integration tests rely on the bundled precompiled-leaves assembly being loadable");

        return bundled!.BuildDictionary();
    }

    /// <summary>
    /// Documents the size of the bundled precompiled-leaves dictionary that the
    /// integration tests historically ran with. This is the optimization surface that the
    /// subject commit removed from the default VM configuration.
    /// </summary>
    [Fact]
    public void Bundled_precompiled_leaves_dictionary_is_substantial()
    {
        var bundled = BundledLeaves();

        // The exact count is an implementation detail; the point is that it is a large
        // collection of hot-path short-circuits (146 on the checkout where this analysis
        // was written), not a handful.
        bundled.Count.Should().BeGreaterThan(
            100,
            because:
            "the integration tests depend on a large set of precompiled leaves to evaluate " +
            "large compiled Elm programs in finite time");
    }

    /// <summary>
    /// Guards the union fix: the dictionary that
    /// <see cref="IntermediateVM.SetupVM.Create"/> runs with by default now
    /// keeps (at least) all of the bundled leaves, in addition to the new per-area leaves.
    /// </summary>
    [Fact]
    public void Default_precompiled_leaves_retain_the_bundled_leaves()
    {
        var bundled = BundledLeaves();

        var defaultLeaves =
            IntermediateVM.SetupVM.DefaultPrecompiledLeaves;

        var bundledKeysLost =
            bundled.Keys.Count(k => !defaultLeaves.ContainsKey(k));

        // The default keeps every bundled leaf.
        bundledKeysLost.Should().Be(
            0,
            because:
            "the union fix merges the bundled leaves with the new per-area leaves, so no " +
            "bundled short-circuit is dropped from the default VM configuration");

        // The default is at least as large as the bundle (bundle plus the new per-area leaves).
        defaultLeaves.Count.Should().BeGreaterThanOrEqualTo(
            bundled.Count,
            because:
            "the default extends the bundle with the new per-area leaves instead of replacing it");
    }
}
