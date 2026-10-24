using AwesomeAssertions;
using Pine.Core.Optimization;
using Pine.Core.PineLanguage;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Optimization;

public class ReusedInstancesTests
{
    [Fact]
    public void Ensure_reference_equality_between_mappings_between_reused_instances()
    {
        ReusedInstances.Instance.AssertReferenceEquality();
    }

    [Fact]
    public void Fallback_without_bundled_declarations_matches_expression_only_build()
    {
        Expression[] expressionRoots =
            [
            Expression.ListInst([Expression.EnvironmentInstance])
            ];

        var expected =
            ReusedInstances.BuildPineListValueReusedInstances(
                expressionRoots,
                additionalRoots: []);

        var actual =
            ReusedInstances.LoadPrecompiledFromEmbeddedOrDefault(
                () => expressionRoots,
                bundledDeclarations: null);

        AssertPineValueListDictsAreEquivalent(
            actual.PineValueLists,
            expected.PineValueLists);

        actual.ValuesExpectedInCompilerLists.SetEquals(expected.ValuesExpectedInCompilerLists).Should().BeTrue();
        actual.ValuesExpectedInCompilerBlobs.SetEquals(expected.ValuesExpectedInCompilerBlobs).Should().BeTrue();
    }

    public static void AssertPineValueListDictsAreEquivalent(
        IReadOnlyDictionary<PineValue.ListValue.ListValueStruct, PineValue.ListValue> a,
        IReadOnlyDictionary<PineValue.ListValue.ListValueStruct, PineValue.ListValue> b)
    {
        a.Count.Should().Be(b.Count, "Counts should be equal");

        foreach (var kv in a)
        {
            b.Should().Contain(kv, "dictionary should contain the key-value pair");
        }
    }
}
