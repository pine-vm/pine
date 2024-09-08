using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;

namespace TestElmTime;

[TestClass]
public class PineValueTests
{
    [TestMethod]
    public void Pine_list_value_content_counts()
    {
        var testCases = new[]
        {
            new
            {
                testName = "empty list",

                listValue =
                PineValue.List([]),

                expectedNodeCount = 0,
                expectedByteCount = 0
            },

            new
            {
                testName = "list with one empty list",

                listValue =
                PineValue.List([PineValue.List([])]),

                expectedNodeCount = 1,
                expectedByteCount = 0
            },

            new
            {
                testName = "list with one list containing empty list",

                listValue =
                PineValue.List([PineValue.List([PineValue.List([])])]),

                expectedNodeCount = 2,
                expectedByteCount = 0
            },

            new
            {
                testName = "list (blob(1))",

                listValue =
                PineValue.List([PineValue.Blob([1])]),

                expectedNodeCount = 1,
                expectedByteCount = 1
            },

            new
            {
                testName = "list (blob(1),blob(3))",

                listValue =
                PineValue.List([PineValue.Blob([123]),PineValue.Blob([1,2,3])]),

                expectedNodeCount = 2,
                expectedByteCount = 4
            },
        };

        foreach (var testCase in testCases)
        {
            Assert.AreEqual(
                testCase.expectedNodeCount,
                testCase.listValue.NodesCount,
                testCase.testName + " - node count");

            Assert.AreEqual(
                testCase.expectedByteCount,
                testCase.listValue.BlobsBytesCount,
                testCase.testName + " - byte count");
        }
    }
}