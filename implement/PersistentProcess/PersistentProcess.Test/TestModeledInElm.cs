using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using JavaScriptEngineSwitcher.ChakraCore;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Kalmit.PersistentProcess.Test
{
    [TestClass]
    public class TestModeledInElm
    {
        static IImmutableList<string> PathToDirectoryWithTestsModeledInElm =>
            ImmutableList.Create(".", "..", "..", "..", "..", "..", "test", "modeled-in-elm");

        static string FilePathStringFromPath(IImmutableList<string> path) =>
            Path.Combine(path.ToArray());

        static IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> GetElmAppFromDirectoryPath(
            IImmutableList<string> directoryPath) =>
            ElmApp.AsCompletelyLoweredElmApp(
                ElmApp.ToFlatDictionaryWithPathComparer(
                    ElmApp.FilesFilteredForElmApp(
                        Filesystem.GetAllFilesFromDirectory(FilePathStringFromPath(directoryPath)))
                    .Select(filePathAndContent => ((IImmutableList<string>)filePathAndContent.filePath.Split(new[] { '/', '\\' }).ToImmutableList(), filePathAndContent.fileContent))),
                    ElmAppInterfaceConfig.Default,
                    Console.WriteLine);

        /*
        Get the value from `tests` in the Elm module `Main`.
        */
        static string GetTestsValueFromModuleMain(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> elmAppFiles)
        {
            var javascriptFromElmMake = Kalmit.ProcessFromElm019Code.CompileElmToJavascript(
                elmAppFiles,
                ImmutableList.Create("src", "Main.elm"));

            var javascriptEngine = new ChakraCoreJsEngine(
                new ChakraCoreSettings
                {
                    DisableEval = true,
                    EnableExperimentalFeatures = true
                }
            );

            var javascriptPreparedToRun =
                Kalmit.ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                    Kalmit.ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake),
                    new[]
                    {(functionNameInElm: "Main.tests", publicName: "published_tests", arity: 0)});

            javascriptEngine.Evaluate(javascriptPreparedToRun);

            return javascriptEngine.Evaluate("published_tests")?.ToString();
        }

        [TestMethod]
        public void Test_modeled_in_Elm()
        {
            var elmAppsDirectories = Directory.GetDirectories(
                FilePathStringFromPath(PathToDirectoryWithTestsModeledInElm));

            Assert.IsTrue(0 < elmAppsDirectories?.Length, "Found directories containing Elm apps.");

            foreach (var elmAppDirectory in elmAppsDirectories)
            {
                var elmAppSubdirectory = Path.GetRelativePath(
                    FilePathStringFromPath(PathToDirectoryWithTestsModeledInElm), elmAppDirectory);

                try
                {
                    var elmCodeFiles = GetElmAppFromDirectoryPath(
                        PathToDirectoryWithTestsModeledInElm.Add(elmAppSubdirectory));

                    var testsValue = GetTestsValueFromModuleMain(elmCodeFiles);

                    Assert.IsNotNull(testsValue, "testsValue on interface is not null.");

                    var testsResultEntries =
                        System.Text.Json.JsonSerializer.Deserialize<FromElmTestResultEntry[]>(testsValue.ToString());

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

        class FromElmTestResultEntry
        {
            public string testName { set; get; } = null;

            public string expected { set; get; } = null;

            public string derived { set; get; } = null;
        }
    }
}
