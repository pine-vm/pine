using Jint;
using Jint.Native;
using Pine;

namespace ElmTime.JavaScript;

public class ElmInteropJint
{
    public static JsObject ElmMaybeNothing(Engine engine) =>
        ElmChoiceTypeTag(engine, "Nothing");

    public static JsObject ElmMaybeJust(Engine engine, JsValue just) =>
        ElmChoiceTypeTag(engine, "Just", just);

    public static JsObject NewElmTuple2(Engine engine, JsValue first, JsValue second)
    {
        // function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

        var elmTuple = new JsObject(engine);

        elmTuple.FastSetDataProperty("$", new JsString("#2"));
        elmTuple.FastSetDataProperty("a", first);
        elmTuple.FastSetDataProperty("b", second);

        return elmTuple;
    }

    public static JsObject ElmChoiceTypeTag(Engine engine, string tagName, params JsValue[] tagArguments)
    {
        /*
        * Example found in JavaScript emitted by Elm compiler:
        * 
        var $elm$core$Maybe$Just = function (a) {
            return {$: 'Just', a: a};
        };
        * */

        var jsObject = new JsObject(engine);

        jsObject.FastSetDataProperty("$", tagName);

        for (int tagArgumentIndex = 0; tagArgumentIndex < tagArguments.Length; tagArgumentIndex++)
        {
            jsObject.FastSetDataProperty(((char)(97 + tagArgumentIndex)).ToString(), tagArguments[tagArgumentIndex]);
        }

        return jsObject;
    }

    public static Result<string, Esprima.Ast.Expression> AstDelegateInElmF3(
        Esprima.Ast.Node originalExpression,
        string delegateIdentifier)
    {
        if (originalExpression is not Esprima.Ast.CallExpression originalCallExpression)
        {
            return Result<string, Esprima.Ast.Expression>.err("Not a call expression");
        }

        if (originalCallExpression.Callee is not Esprima.Ast.Identifier originalCallExpressionCallee)
        {
            return Result<string, Esprima.Ast.Expression>.err("Does not call F3");
        }

        if (originalCallExpressionCallee.Name != "F3")
            return Result<string, Esprima.Ast.Expression>.err("Does not call F3");

        return
            Result<string, Esprima.Ast.Expression>.ok(
                originalCallExpression
                .UpdateWith(
                    callee: originalCallExpression.Callee,
                    arguments: Esprima.Ast.NodeList.Create<Esprima.Ast.Expression>(
                        [new Esprima.Ast.Identifier(delegateIdentifier)])));
    }
}
