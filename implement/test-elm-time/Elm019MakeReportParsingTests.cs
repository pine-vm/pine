using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Elm019;

namespace TestElmTime;

[TestClass]
public class Elm019MakeReportParsingTests
{
    [TestMethod]
    public void Elm_make_report_compile_errors()
    {
        var reportJson =
            """
            {
                "type": "compile-errors",
                "errors": [
                    {
                        "path": "C:\\Users\\Raetzel_M\\AppData\\Local\\Temp\\havjzbzg.vhc\\src\\Main.elm",
                        "name": "Main",
                        "problems": [
                            {
                                "title": "TYPE MISMATCH",
                                "region": {
                                    "start": {
                                        "line": 8,
                                        "column": 33
                                    },
                                    "end": {
                                        "line": 8,
                                        "column": 37
                                    }
                                },
                                "message": [
                                    "The 2nd element of this list does not match all the previous elements:\n\n8|     [ Html.text \"Hello World!\", 1234 ]\n                                   ",
                                    {
                                        "bold": false,
                                        "underline": false,
                                        "color": "RED",
                                        "string": "^^^^"
                                    },
                                    "\nThe 2nd element is a number of type:\n\n    ",
                                    {
                                        "bold": false,
                                        "underline": false,
                                        "color": "yellow",
                                        "string": "number"
                                    },
                                    "\n\nBut all the previous elements in the list are:\n\n    ",
                                    {
                                        "bold": false,
                                        "underline": false,
                                        "color": "yellow",
                                        "string": "Html.Html msg"
                                    },
                                    "\n\n",
                                    {
                                        "bold": false,
                                        "underline": true,
                                        "color": null,
                                        "string": "Hint"
                                    },
                                    ": Everything in a list must be the same type of value. This way, we never\nrun into unexpected values partway through a List.map, List.foldl, etc. Read\n<https://elm-lang.org/0.19.1/custom-types> to learn how to ÔÇ£mixÔÇØ types."
                                ]
                            }
                        ]
                    }
                ]
            }
            """;

        var parsedReport = ElmMakeReportConverter.Deserialize(reportJson);

        Assert.IsNotNull(parsedReport);

        if (parsedReport is not ElmMakeReport.ElmMakeReportCompileErrors compileErrors)
        {
            Assert.Fail("Unexpected type: " + parsedReport?.GetType());
            return;
        }

        Assert.AreEqual(1, compileErrors.Errors.Count);

        var error = compileErrors.Errors[0];

        Assert.AreEqual(
            "C:\\Users\\Raetzel_M\\AppData\\Local\\Temp\\havjzbzg.vhc\\src\\Main.elm",
            error.Path);

        Assert.AreEqual("Main", error.Name);

        Assert.AreEqual(1, error.Problems.Count);

        var problem = error.Problems[0];

        Assert.AreEqual("TYPE MISMATCH", problem.Title);

        Assert.AreEqual(8, problem.Region.Start.Line);
        Assert.AreEqual(33, problem.Region.Start.Column);
        Assert.AreEqual(8, problem.Region.End.Line);
        Assert.AreEqual(37, problem.Region.End.Column);

        Assert.AreEqual(
            new MessageItem.StringMessage(
            "The 2nd element of this list does not match all the previous elements:\n\n8|     [ Html.text \"Hello World!\", 1234 ]\n                                   "),
            problem.Message[0]);

        Assert.AreEqual(9, problem.Message.Count);

        Assert.AreEqual(
            new MessageItem.StyledMessage
            (
                Bold: false,
                Underline: false,
                Color: "RED",
                String: "^^^^"
            ),
            problem.Message[1]);
    }

    [TestMethod]
    public void Elm_make_report_error_no_elm_json()
    {
        var reportJson =
            """
            {
                "type": "error",
                "path": null,
                "title": "NO elm.json FILE",
                "message": [
                    "It looks like you are starting a new Elm project. Very exciting! Try running:\n\n    ",
                    {
                        "bold": false,
                        "underline": false,
                        "color": "GREEN",
                        "string": "elm init"
                    },
                    "\n\nIt will help you get set up. It is really simple!"
                ]
            }
            """;

        var parsedReport = ElmMakeReportConverter.Deserialize(reportJson);

        Assert.IsNotNull(parsedReport);

        if (parsedReport is not ElmMakeReport.ElmMakeReportError errorReport)
        {
            Assert.Fail("Unexpected type: " + parsedReport?.GetType());
            return;
        }

        Assert.IsNull(errorReport.Path);
        Assert.AreEqual("NO elm.json FILE", errorReport.Title);

        Assert.AreEqual(3, errorReport.Message.Count);

        Assert.AreEqual(
            new MessageItem.StringMessage(
            "It looks like you are starting a new Elm project. Very exciting! Try running:\n\n    "),
            errorReport.Message[0]);

        Assert.AreEqual(
            new MessageItem.StyledMessage
            (
                Bold: false,
                Underline: false,
                Color: "GREEN",
                String: "elm init"
            ),
            errorReport.Message[1]);

        Assert.AreEqual(
            new MessageItem.StringMessage(
            "\n\nIt will help you get set up. It is really simple!"),
            errorReport.Message[2]);
    }
}
