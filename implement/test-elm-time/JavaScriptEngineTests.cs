using ElmTime.JavaScript;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace TestElmTime;

[TestClass]
public class JavaScriptEngineTests
{
    [TestMethod]
    public void Evaluate_in_JavaScriptEngine()
    {
        var javaScriptEngine = JavaScriptEngineJintOptimizedForElmApps.Create();

        Assert.AreEqual(
            4.ToString(),
            javaScriptEngine.Evaluate("3 + 1").ToString());
    }
}
