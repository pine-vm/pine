using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Kalmit.PersistentProcess.Test
{
    [TestClass]
    public class TestElmEvaluation
    {
        static string pathToScenariosDirectory => @"./../../../../../test/elm-evaluation-scenarios";

        static string scenarioElmAppCodeEvaluationRootDeclarationName => "evaluation_root";

        [TestMethod]
        public void TestElmEvaluationScenarios()
        {
            foreach (var scenarioDirectory in Directory.EnumerateDirectories(pathToScenariosDirectory))
            {
                var scenarioName = Path.GetFileName(scenarioDirectory);

                if (Directory.EnumerateFiles(scenarioDirectory, "*").Take(1).Count() < 1)
                {
                    // Do not stumble over empty directory here. It could be a leftover after git checkout.
                    continue;
                }

                try
                {
                    var appCodeTree = LoadFromLocalFilesystem.LoadSortedTreeFromPath(Path.Combine(scenarioDirectory, "app-code"));

                    var expectedValueFile = File.ReadAllBytes(Path.Combine(scenarioDirectory, "expected-value.json"));

                    var expectedValueJson = System.Text.Encoding.UTF8.GetString(expectedValueFile);

                    var evaluatedJson =
                        elm_fullstack.ElmEngine.EvaluateElm.GetValueFromEntryPointAsJsonString(
                            appCodeTree: appCodeTree,
                            evaluationRootFilePath: ImmutableList.Create("src", "Main.elm"),
                            evaluationRootDeclarationName: scenarioElmAppCodeEvaluationRootDeclarationName);

                    Assert.AreEqual(
                        expectedValueJson,
                        evaluatedJson, "Value from evaluation matches expected value in scenario '" + scenarioName + "'");
                }
                catch (Exception e)
                {
                    throw new Exception("Failed for scenario '" + scenarioName + "'", e);
                }
            }
        }
    }
}
