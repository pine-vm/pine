using System.Collections.Immutable;
using ElmTime;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Linq;
using Pine;

namespace TestElmTime
{
    [TestClass]
    public class TestDemoBackendState
    {
        [TestMethod]
        public void Test_demo_backend_state()
        {
            var sourceFiles =
                TestSetup.GetElmAppFromDirectoryPath(
                    ImmutableList.Create(".", "..", "..", "..", "..", "example-apps", "demo-backend-state"));

            var webAppSource =
                TestSetup.AppConfigComponentFromFiles(sourceFiles);

            var compilationResult = ElmAppCompilation.AsCompletelyLoweredElmApp(
                sourceFiles: sourceFiles,
                interfaceConfig: ElmAppInterfaceConfig.Default);

            var compilationSuccess =
                compilationResult.Extract(err => throw new System.Exception(
                    "Failed compilation:\n" +
                    string.Join("\n", err.Select(singleErr => ElmAppCompilation.DescribeCompilationError(singleErr)))));

            using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppConfigAndInitElmState: webAppSource);
            using var server = testSetup.StartWebHost();
            using var publicAppClient = testSetup.BuildPublicAppHttpClient();
        }
    }
}