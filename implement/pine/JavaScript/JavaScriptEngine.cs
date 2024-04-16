using JavaScriptEngineSwitcher.V8;
using System;

namespace ElmTime.JavaScript;

public interface IJavaScriptEngine : IDisposable
{
    object Evaluate(string expression);

    object CallFunction(string functionName, params object[] args);

    public static IJavaScriptEngine DefaultBuildJavaScriptEngine() =>
        JavaScriptEngineFromJavaScriptEngineSwitcher.ConstructJavaScriptEngine();

    public static Func<IJavaScriptEngine>? OverrideDefaultBuildJavaScriptEngine { set; get; }

    public static IJavaScriptEngine BuildJavaScriptEngine() =>
        OverrideDefaultBuildJavaScriptEngine?.Invoke() ?? DefaultBuildJavaScriptEngine();
}

public class JavaScriptEngineFromJavaScriptEngineSwitcher(
    JavaScriptEngineSwitcher.Core.IJsEngine javaScriptEngine)
    : IJavaScriptEngine
{
    private readonly JavaScriptEngineSwitcher.Core.IJsEngine javaScriptEngine = javaScriptEngine;

    object IJavaScriptEngine.CallFunction(string functionName, params object[] args) =>
        javaScriptEngine.CallFunction(functionName, args);

    object IJavaScriptEngine.Evaluate(string expression) =>
        javaScriptEngine.Evaluate(expression);

    void IDisposable.Dispose() =>
        javaScriptEngine.Dispose();

    public static int? OverrideJavaScriptEngineSettingsMaxStackSize = null;

    public static IJavaScriptEngine ConstructJavaScriptEngine() =>
        new JavaScriptEngineFromJavaScriptEngineSwitcher(ConstructClearScriptJavaScriptEngine());

    public static JavaScriptEngineSwitcher.Core.IJsEngine ConstructClearScriptJavaScriptEngine()
    {
        ClearScriptV8.SetupTask.Value.Wait();

        return new V8JsEngine(
            new V8Settings
            {
                MaxStackUsage = (nuint)(OverrideJavaScriptEngineSettingsMaxStackSize ?? 40_000_000),
            }
        );
    }
}
