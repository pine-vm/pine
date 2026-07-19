using AwesomeAssertions;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

public class PrecompiledLeafValueBundleTests
{
    [Fact]
    public void Freshly_built_precompiled_leaf_values_equal_bundled_values()
    {
        var freshlyBuilt =
            IntermediateVM.SetupVM.BuildDefaultPrecompiledLeafValues();

        var bundled =
            IntermediateVM.SetupVM.LoadDefaultPrecompiledLeafValuesFromBundledResource();

        bundled.Should().BeEquivalentTo(freshlyBuilt);

        freshlyBuilt.Keys.Should().BeEquivalentTo(
            IntermediateVM.SetupVM.DefaultPrecompiledLeafFunctionsByName.Keys);

        var reusedListValues = ReusedInstances.Instance.ListValues!;

        freshlyBuilt.Values
            .OfType<PineValue.ListValue>()
            .Should()
            .OnlyContain(
                value =>
                reusedListValues.ContainsKey(
                    new PineValue.ListValue.ListValueStruct(value)));
    }
}
