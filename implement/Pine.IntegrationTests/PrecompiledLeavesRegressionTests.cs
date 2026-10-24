using AwesomeAssertions;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests;

/// <summary>
/// Serializes tests that temporarily override the process-wide precompiled-leaves bundle.
/// </summary>
[CollectionDefinition(nameof(PrecompiledLeavesRegressionTests), DisableParallelization = true)]
public class PrecompiledLeavesRegressionTestsCollection;

/// <summary>
/// Guards the default per-area leaves and their union with an optional configured bundle.
/// The legacy embedded bundle is no longer produced by prebuild.
/// </summary>
[Collection(nameof(PrecompiledLeavesRegressionTests))]
public class PrecompiledLeavesRegressionTests
{
    [Fact]
    public void Default_precompiled_leaves_without_a_bundle_retain_the_core_leaves()
    {
        var previousBundle = IntermediateVM.SetupVM.PrecompiledLeavesDefault;

        try
        {
            IntermediateVM.SetupVM.PrecompiledLeavesDefault =
                new Dictionary<PineValue, PrecompiledLeaf>();

            var defaultLeaves = IntermediateVM.SetupVM.DefaultPrecompiledLeaves;
            var coreLeaves = Core.IntermediateVM.SetupVM.DefaultPrecompiledLeaves;

            coreLeaves.Should().NotBeEmpty();

            defaultLeaves.Should().BeSameAs(
                coreLeaves,
                because: "the per-area leaves remain available without the legacy bundle");
        }
        finally
        {
            IntermediateVM.SetupVM.PrecompiledLeavesDefault = previousBundle;
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Default_precompiled_leaves_merge_the_configured_bundle_with_core_precedence(
        bool includeCoreLeaf)
    {
        var coreLeaves = Core.IntermediateVM.SetupVM.DefaultPrecompiledLeaves;
        var bundledKey = IntegerEncoding.EncodeSignedInteger(1234567890);
        PrecompiledLeaf bundledLeaf = _ => null;

        coreLeaves.Should().NotContainKey(bundledKey);

        var bundled =
            new Dictionary<PineValue, PrecompiledLeaf>
            {
                [bundledKey] = bundledLeaf,
            };

        if (includeCoreLeaf)
        {
            bundled[coreLeaves.Keys.First()] = bundledLeaf;
        }

        var previousBundle = IntermediateVM.SetupVM.PrecompiledLeavesDefault;

        try
        {
            IntermediateVM.SetupVM.PrecompiledLeavesDefault = bundled;

            var defaultLeaves = IntermediateVM.SetupVM.DefaultPrecompiledLeaves;

            defaultLeaves.Count.Should().Be(coreLeaves.Count + 1);
            defaultLeaves[bundledKey].Should().BeSameAs(bundledLeaf);

            foreach (var entry in coreLeaves)
            {
                defaultLeaves[entry.Key].Should().BeSameAs(
                    entry.Value,
                    because: "per-area leaves take precedence over the optional bundle");
            }
        }
        finally
        {
            IntermediateVM.SetupVM.PrecompiledLeavesDefault = previousBundle;
        }
    }
}
