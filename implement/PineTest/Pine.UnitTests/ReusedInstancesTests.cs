using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using System.Collections.Generic;

namespace Pine.UnitTests;

[TestClass]
public class ReusedInstancesTests
{
    [TestMethod]
    public void Ensure_reference_equality_between_mappings_between_reused_instances()
    {
        ReusedInstances.Instance.AssertReferenceEquality();
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
