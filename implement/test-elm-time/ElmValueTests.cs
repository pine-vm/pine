using ElmTime.ElmInteractive;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;

namespace TestElmTime;

[TestClass]
public class ElmValueTests
{
    [TestMethod]
    public void Test_ElmValue_as_pine_value_roundtrips()
    {
        var testCases = (IReadOnlyList<ElmValue>)[
            new ElmValue.ElmChar('a'),
            new ElmValue.ElmInteger(42),
            new ElmValue.ElmString("Hello, world!"),
            new ElmValue.ElmList([
                new ElmValue.ElmInteger(31),
                new ElmValue.ElmInteger(37),
                new ElmValue.ElmInteger(39)]),
            new ElmValue.ElmRecord([
                ("alfa", new ElmValue.ElmInteger(1)),
                ("beta", new ElmValue.ElmInteger(2)),
                ("gamma", new ElmValue.ElmInteger(3))
            ]),
        ];

        foreach (var testCase in testCases)
        {
            var pineValue = ElmValue.ElmValueAsPineValue(testCase);

            var roundtrip =
                ElmValue.PineValueAsElmValue(pineValue)
                .Extract(err => throw new System.Exception(err));

            Assert.AreEqual(testCase, roundtrip);
        }
    }
}
