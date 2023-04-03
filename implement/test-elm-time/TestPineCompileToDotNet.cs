using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.PineVM;
using System.Collections.Immutable;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class TestPineCompileToDotNet
{
    static public readonly PineValue value_299b7decef = PineValueAsString.ValueFromString("List");

    static public readonly PineValue value_d597fb92e5 = PineValue.List(new[] { value_299b7decef, PineValue.EmptyList });

    [TestMethod]
    public void Test_sort_pine_value_by_containment()
    {
        Assert.IsTrue(value_d597fb92e5.ContainsInListTransitive(value_299b7decef));

        var listBeforeOrdering =
            new[]
            {
                value_d597fb92e5,
                value_299b7decef
            };

        var listWithHashes =
            listBeforeOrdering
            .Select(value => (value, hash: CommonConversion.StringBase16(PineValueHashTree.ComputeHash(value))))
            .ToImmutableList();

        var orderedValues = PineCompileToDotNet.OrderValuesByContainment(listBeforeOrdering).ToImmutableList();

        CollectionAssert.AreEqual(
            new[]
            {
                value_299b7decef,
                value_d597fb92e5
            },
            orderedValues);
    }
}
