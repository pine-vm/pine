using ElmTime.JavaScript;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace TestElmTime;

[TestClass]
public class JavaScriptEngineTest
{
    [TestMethod]
    public void Evaluate_in_JavaScriptEngine()
    {
        var javaScriptEngine = JavaScriptEngineFromJavaScriptEngineSwitcher.ConstructJavaScriptEngine();

        Assert.AreEqual(4, javaScriptEngine.Evaluate("3 + 1"));
    }
}
