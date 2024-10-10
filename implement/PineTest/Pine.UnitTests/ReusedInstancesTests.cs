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

    [TestMethod]
    public void Embedded_precompiled_pine_value_lists()
    {
        var fromFreshBuild =
            ReusedInstances.BuildPineListValueReusedInstances(
                ReusedInstances.ExpressionsSource());

        var file =
            ReusedInstances.BuildPrecompiledDictFile(fromFreshBuild);

        var parsedFile =
            ReusedInstances.LoadFromPrebuiltJson(file);

        AssertPineValueListDictsAreEquivalent(
            parsedFile.PineValueLists,
            fromFreshBuild.PineValueLists);

        AssertPineValueListDictsAreEquivalent(
            ReusedInstances.Instance.ListValues,
            fromFreshBuild.PineValueLists);
    }

    public static void AssertPineValueListDictsAreEquivalent(
        IReadOnlyDictionary<PineValue.ListValue.ListValueStruct, PineValue.ListValue> a,
        IReadOnlyDictionary<PineValue.ListValue.ListValueStruct, PineValue.ListValue> b)
    {
        if (a.Count != b.Count)
        {
            Assert.Fail("Counts are not equal: " + a.Count + " vs " + b.Count);
        }

        foreach (var kv in a)
        {
            Assert.IsTrue(b.ContainsKey(kv.Key), "contains key");
        }
    }
}