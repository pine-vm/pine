using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.PineVM;
using System.Collections.Immutable;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class TestPineCompileToDotNet
{
    public static readonly PineValue value_299b7decef = PineValueAsString.ValueFromString("List");

    public static readonly PineValue value_d597fb92e5 = PineValue.List([value_299b7decef, PineValue.EmptyList]);

    [TestMethod]
    public void Test_sort_pine_value_for_declaration()
    {
        Assert.IsTrue(value_d597fb92e5.ContainsInListTransitive(value_299b7decef));

        var listBeforeOrdering =
            new[]
            {
                PineValueAsString.ValueFromString("Err"),
                value_d597fb92e5,
                PineValueAsString.ValueFromString("Ok"),
                value_299b7decef
            };

        var listWithHashes =
            listBeforeOrdering
            .Select(value => (value, hash: CommonConversion.StringBase16(PineValueHashTree.ComputeHash(value))))
            .ToImmutableList();

        var orderedValues = PineCompileToDotNet.OrderValuesForDeclaration(listBeforeOrdering).ToImmutableList();

        CollectionAssert.AreEqual(
            new[]
            {
                PineValueAsString.ValueFromString("Ok"),
                PineValueAsString.ValueFromString("Err"),
                value_299b7decef,
                value_d597fb92e5
            },
            orderedValues);
    }
}
