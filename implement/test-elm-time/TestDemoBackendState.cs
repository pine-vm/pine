using ElmTime;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class TestDemoBackendState
{
    [TestMethod]
    public void Test_demo_backend_state()
    {
        var sourceFiles =
            TestSetup.GetElmAppFromDirectoryPath(
                (IReadOnlyList<string>)[".", "..", "..", "..", "..", "example-apps", "demo-backend-state"]);

        var webAppSource =
            TestSetup.AppConfigComponentFromFiles(sourceFiles);

        var compilationResult = ElmAppCompilation.AsCompletelyLoweredElmApp(
            sourceFiles: sourceFiles,
            workingDirectoryRelative: [],
            interfaceConfig: ElmAppInterfaceConfig.Default);

        var compilationSuccess =
            compilationResult.Extract(err => throw new System.Exception(
                "Failed compilation:\n" +
                string.Join("\n", err.Select(singleErr => ElmAppCompilation.DescribeCompilationError(singleErr)))));

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: webAppSource);
        using var server = testSetup.StartWebHost();
        using var publicAppClient = testSetup.BuildPublicAppHttpClient();
    }
}