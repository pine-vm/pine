using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
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
            ElmValue.Char('a'),
            ElmValue.Integer(42),
            ElmValue.String("Hello, world!"),
            new ElmValue.ElmList([
                ElmValue.Integer(31),
                ElmValue.Integer(37),
                ElmValue.Integer(39)]),
            new ElmValue.ElmRecord([
                ("alfa", ElmValue.Integer(1)),
                ("beta", ElmValue.Integer(2)),
                ("gamma", ElmValue.Integer(3)),
            ]),

            new ElmValue.ElmString("Hello, world 👋"),

            new ElmValue.ElmTag("True", []),
            new ElmValue.ElmTag("False", []),

            new ElmValue.ElmList([
                new ElmValue.ElmList([
                    ElmValue.Integer(7),
                    ElmValue.Integer(13)]),
                new ElmValue.ElmList([
                    ElmValue.Integer(41),
                    ElmValue.Integer(43),
                    ElmValue.Integer(47)]),
            ]),
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
                (ElmValue.Integer(42), "42"),

                (ElmValue.Char('a'), "'a'"),

                (ElmValue.String("Hello, world!"), "\"Hello, world!\""),

                (new ElmValue.ElmList([
                ElmValue.Integer(31),
                    ElmValue.Integer(37),
                    ElmValue.Integer(39)]),
                    "[31,37,39]"),

                (new ElmValue.ElmRecord([
                ("alfa", ElmValue.Integer(1)),
                    ("beta", ElmValue.Integer(2)),
                    ("gamma", ElmValue.Integer(3))]),
                "{ alfa = 1, beta = 2, gamma = 3 }"),

                (new ElmValue.ElmTag("Just", [ElmValue.Integer(43)]),
                "Just 43"),

                (new ElmValue.ElmTag("Nothing", []),
                "Nothing"),

                (new ElmValue.ElmTag("Just", [new ElmValue.ElmTag("Nothing", [])]),
                "Just Nothing"),

                (new ElmValue.ElmTag("Just", [new ElmValue.ElmTag("Just", [ElmValue.Integer(47)])]),
                "Just (Just 47)"),
            ];

        foreach (var (elmValue, expectedExpression) in testCases)
        {
            var (expressionString, needsParens) = ElmValue.ElmValueAsExpression(elmValue);

            Assert.AreEqual(expectedExpression, expressionString);
        }
    }
}
