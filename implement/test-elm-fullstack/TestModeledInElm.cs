using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using ElmFullstack;
using JavaScriptEngineSwitcher.V8;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace TestElmFullstack;

[TestClass]
public class TestModeledInElm
{
    static IImmutableList<string> PathToDirectoryWithTestsModeledInElm =>
        ImmutableList.Create(".", "..", "..", "..", "modeled-in-elm");

    static string FilePathStringFromPath(IImmutableList<string> path) =>
        Path.Combine(path.ToArray());

    static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> GetLoweredElmAppFromDirectoryPath(
        IImmutableList<string> directoryPath) =>
        TestSetup.AsLoweredElmApp(
            TestSetup.GetElmAppFromDirectoryPath(directoryPath),
            compilationRootFilePath: ImmutableList.Create("src", "Main.elm"));

    /*
    Get the value from `tests` in the Elm module `Main`.
    */
    static string? GetTestsValueFromModuleMain(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> elmAppFiles)
    {
        var elmMakeResult = Elm019Binaries.ElmMakeToJavascript(
            elmAppFiles,
            ImmutableList.Create("src", "Main.elm"));

        var javascriptFromElmMake =
            Encoding.UTF8.GetString(
                elmMakeResult.Extract(err => throw new Exception("Failed elm make: " + err)).producedFile.Span);

        var javascriptPreparedToRun =
            ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake),
                new[]
                {(functionNameInElm: "Main.tests", publicName: "published_tests", arity: 0)});

        var javascriptEngine = new V8JsEngine(
            new V8Settings
            {
            }
        );

        javascriptEngine.Evaluate(javascriptPreparedToRun);

        return javascriptEngine.Evaluate("published_tests")?.ToString();
    }

    [TestMethod]
    public void Test_modeled_in_Elm()
    {
        var elmAppsDirectories =
            Directory.GetDirectories(FilePathStringFromPath(PathToDirectoryWithTestsModeledInElm));

        Assert.IsTrue(0 < elmAppsDirectories?.Length, "Found directories containing Elm apps.");

        foreach (var elmAppDirectory in elmAppsDirectories!)
        {
            var elmAppSubdirectory = Path.GetRelativePath(
                FilePathStringFromPath(PathToDirectoryWithTestsModeledInElm), elmAppDirectory);

            try
            {
                var elmCodeFiles = GetLoweredElmAppFromDirectoryPath(
                    PathToDirectoryWithTestsModeledInElm.Add(elmAppSubdirectory));

                var testsValue = GetTestsValueFromModuleMain(elmCodeFiles);

                Assert.IsNotNull(testsValue, "testsValue on interface is not null.");

                var testsResultEntries =
                    System.Text.Json.JsonSerializer.Deserialize<FromElmTestResultEntry[]>(testsValue.ToString())!;

                Assert.IsTrue(0 < testsResultEntries.Length, "Number of test result entries is greater than zero.");

                for (var i = 0; i < testsResultEntries.Length; ++i)
                {
                    var testResultEntry = testsResultEntries[i];

                    try
                    {
                        Assert.IsTrue(0 < testResultEntry.testName?.Length, "Test name is not empty.");

                        Assert.AreEqual(
                            testResultEntry.expected,
                            testResultEntry.derived);
                    }
                    catch (Exception e)
                    {
                        throw new Exception("Failed for test '" + testResultEntry.testName + "' (entry [" + i + "])", e);
                    }
                }
            }
            catch (Exception e)
            {
                throw new Exception("Failed in Elm app '" + elmAppSubdirectory + "'.", e);
            }
        }
    }

    record FromElmTestResultEntry(
        string testName,
        string expected,
        string derived);
}
