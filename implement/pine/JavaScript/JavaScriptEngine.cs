using System;

namespace ElmTime.JavaScript;

public interface IJavaScriptEngine : IDisposable
{
    object Evaluate(string expression);

    object CallFunction(string functionName, params object[] args);

    public static IJavaScriptEngine DefaultBuildJavaScriptEngine() =>
        JavaScriptEngineJintOptimizedForElmApps.Create();

    public static Func<IJavaScriptEngine>? OverrideDefaultBuildJavaScriptEngine { set; get; }

    public static IJavaScriptEngine BuildJavaScriptEngine() =>
        OverrideDefaultBuildJavaScriptEngine?.Invoke() ?? DefaultBuildJavaScriptEngine();
}
