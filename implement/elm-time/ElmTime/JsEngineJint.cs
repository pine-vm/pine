using Jint;

namespace ElmTime;

public class JsEngineJint : IJsEngine
{
    readonly Engine engine = new();

    static public JsEngineJint Create() => new();

    public object CallFunction(string functionName, params object[] args)
    {
        return engine.Invoke(functionName, args);
    }

    public void Dispose()
    {
    }

    public object Evaluate(string expression)
    {
        var jintValue = engine.Evaluate(expression);

        var dotnetValue = CastToDotnetType(jintValue);

        return dotnetValue;
    }

    static object CastToDotnetType(Jint.Native.JsValue jintValue)
    {
        if (jintValue is Jint.Native.JsString jsString)
            return jsString.AsString();

        if (jintValue is Jint.Native.JsNumber jsNumber)
            return jsNumber.AsNumber();

        return jintValue;
    }
}
