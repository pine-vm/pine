using JavaScriptEngineSwitcher.V8;
using System;

namespace ElmFullstack;

public interface IJsEngine : IDisposable
{
    object Evaluate(string expression);

    object CallFunction(string functionName, params object[] args);

    static public IJsEngine DefaultBuildJsEngine() =>
        JsEngineFromJavaScriptEngineSwitcher.ConstructJsEngine();

    static public Func<IJsEngine>? OverrideDefaultBuildJsEngine { set; get; }

    static public IJsEngine BuildJsEngine() =>
        OverrideDefaultBuildJsEngine?.Invoke() ?? DefaultBuildJsEngine();
}

public class JsEngineFromJavaScriptEngineSwitcher : IJsEngine
{
    readonly JavaScriptEngineSwitcher.Core.IJsEngine jsEngine;

    public JsEngineFromJavaScriptEngineSwitcher(JavaScriptEngineSwitcher.Core.IJsEngine jsEngine)
    {
        this.jsEngine = jsEngine;
    }

    object IJsEngine.CallFunction(string functionName, params object[] args) =>
        jsEngine.CallFunction(functionName, args);

    object IJsEngine.Evaluate(string expression) =>
        jsEngine.Evaluate(expression);

    void IDisposable.Dispose() =>
        jsEngine.Dispose();

    static public int? OverrideJsEngineSettingsMaxStackSize = null;

    static public IJsEngine ConstructJsEngine() =>
        new JsEngineFromJavaScriptEngineSwitcher(ConstructJavaScriptEngineSwitcherJsEngine());

    static public JavaScriptEngineSwitcher.Core.IJsEngine ConstructJavaScriptEngineSwitcherJsEngine()
    {
        return new V8JsEngine(
            new V8Settings
            {
                MaxStackUsage = (UIntPtr)(OverrideJsEngineSettingsMaxStackSize ?? 40_000_000),
            }
        );
    }

}
