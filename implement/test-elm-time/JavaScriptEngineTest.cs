using ElmTime.JavaScript;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace TestElmTime;

[TestClass]
public class JavaScriptEngineTest
{
    [TestMethod]
    public void Evaluate_in_JavaScriptEngine()
    {
        var jsEngine = JsEngineFromJavaScriptEngineSwitcher.ConstructJsEngine();

        Assert.AreEqual(4, jsEngine.Evaluate("3 + 1"));
    }
}
