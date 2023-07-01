using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System.Net.Http;
using System.Threading.Tasks;

namespace TestElmTime;

[TestClass]
public class ElmWebServiceUsingVolatileProcessNativeTest
{
    public static PineValue ElmWebServiceVolatileProcessNative =>
        TestSetup.AppConfigComponentFromFiles(TestSetup.VolatileProcessNativeWebApp);

    [TestMethod]
    public async Task Volatile_process_native_echo_json()
    {
        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: ElmWebServiceVolatileProcessNative);

        using var server = testSetup.StartWebHost();

        using var client = testSetup.BuildPublicAppHttpClient();

        var httpResponse = await client.PostAsync(
            "/",
            new StringContent("[ 1,  3, 4 ]\n"));

        var httpResponseContent = await httpResponse.Content.ReadAsStringAsync();

        Assert.AreEqual("[1,3,4]", httpResponseContent.Trim());
    }
}
