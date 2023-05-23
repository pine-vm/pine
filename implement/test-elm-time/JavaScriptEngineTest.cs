using ElmTime;
using Microsoft.ClearScript;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;

namespace TestElmTime;

[TestClass]
public class JavaScriptEngineTest
{
    [TestMethod]
    public void Evaluate_in_JavaScriptEngine()
    {
        try
        {
            var jsEngine = JsEngineFromJavaScriptEngineSwitcher.ConstructJsEngine();

            Assert.AreEqual(4, jsEngine.Evaluate("3 + 1"));
        }
        finally
        {
            Console.WriteLine("HostSettings.AuxiliarySearchPath: " + HostSettings.AuxiliarySearchPath);
        }
    }
}
