using ElmTime.ElmInteractive;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.ElmInteractive;
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
                ("gamma", new ElmValue.ElmInteger(3)),
            ]),

            new ElmValue.ElmString("Hello, world 👋"),

            new ElmValue.ElmTag("True", []),
            new ElmValue.ElmTag("False", []),
        ];

        foreach (var testCase in testCases)
        {
            var pineValue = ElmValueEncoding.ElmValueAsPineValue(testCase);

            var roundtrip =
                ElmValueEncoding.PineValueAsElmValue(pineValue)
                .Extract(err => throw new System.Exception(err));

            Assert.AreEqual(testCase, roundtrip);
        }
    }

    [TestMethod]
    public void Test_ElmValueAsExpression()
    {
        var testCases =
            (IReadOnlyList<(ElmValue, string)>)
            [
                (new ElmValue.ElmInteger(42), "42"),

                (new ElmValue.ElmChar('a'), "'a'"),

                (new ElmValue.ElmString("Hello, world!"), "\"Hello, world!\""),

                (new ElmValue.ElmList([
                new ElmValue.ElmInteger(31),
                    new ElmValue.ElmInteger(37),
                    new ElmValue.ElmInteger(39)]),
                    "[31,37,39]"),

                (new ElmValue.ElmRecord([
                ("alfa", new ElmValue.ElmInteger(1)),
                    ("beta", new ElmValue.ElmInteger(2)),
                    ("gamma", new ElmValue.ElmInteger(3))]),
                "{ alfa = 1, beta = 2, gamma = 3 }"),

                (new ElmValue.ElmTag("Just", [new ElmValue.ElmInteger(43)]),
                "Just 43"),

                (new ElmValue.ElmTag("Nothing", []),
                "Nothing"),

                (new ElmValue.ElmTag("Just", [new ElmValue.ElmTag("Nothing", [])]),
                "Just Nothing"),

                (new ElmValue.ElmTag("Just", [new ElmValue.ElmTag("Just", [new ElmValue.ElmInteger(47)])]),
                "Just (Just 47)"),
            ];

        foreach (var (elmValue, expectedExpression) in testCases)
        {
            var (expressionString, needsParens) = ElmValue.ElmValueAsExpression(elmValue);

            Assert.AreEqual(expectedExpression, expressionString);
        }
    }
}
