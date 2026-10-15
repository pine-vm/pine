using Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;

using SyntaxTypes = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Compiles Elm operator applications to Pine expressions.
/// </summary>
public class OperatorCompiler
{
    /// <summary>
    /// Compiles an operator application to a Pine expression.
    /// </summary>
    /// <param name="operatorApp">The operator application to compile.</param>
    /// <param name="context">The compilation context.</param>
    /// <returns>A result containing the compiled Pine expression or a compilation error.</returns>
    public static Result<CompilationError, Expression> Compile(
        SyntaxTypes.Expression.OperatorApplication operatorApp,
        ExpressionCompilationContext context)
    {
        var leftResult = ExpressionCompiler.Compile(operatorApp.Left, context);

        if (leftResult.IsErrOrNull() is { } leftErr)
        {
            return
                CompilationError.Scoped(
                    "compiling left side of operator " + operatorApp.Operator,
                    leftErr);
        }

        var rightResult = ExpressionCompiler.Compile(operatorApp.Right, context);

        if (rightResult.IsErrOrNull() is { } rightErr)
        {
            return
                CompilationError.Scoped(
                    "compiling right side of operator " + operatorApp.Operator,
                    rightErr);
        }

        if (leftResult.IsOkOrNull() is not { } leftCompiled)
        {
            throw new System.NotImplementedException(
                "Unexpected return type compiling left: " + leftResult.GetType());
        }

        if (rightResult.IsOkOrNull() is not { } rightCompiled)
        {
            throw new System.NotImplementedException(
                "Unexpected return type compiling right: " + rightResult.GetType());
        }

        if (operatorApp.Operator is "//")
        {
            return CoreBasics.Int_div(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "+")
        {
            return CoreBasics.Generic_Add(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "-")
        {
            return CoreBasics.Generic_Sub(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "*")
        {
            return CoreBasics.Generic_Mul(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "==")
        {
            return CoreBasics.Generic_Eq(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "/=")
        {
            return CoreBasics.Generic_Neq(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "<")
        {
            return CoreBasics.Generic_Lt(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is ">")
        {
            return CoreBasics.Generic_Gt(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "<=")
        {
            return CoreBasics.Generic_Le(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is ">=")
        {
            return CoreBasics.Generic_Ge(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "&&")
        {
            return CoreBasics.Generic_And(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "||")
        {
            return CoreBasics.Generic_Or(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "++")
        {
            return CoreBasics.Generic_Append(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is ">>")
        {
            return CoreBasics.Generic_ComposeR(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "<<")
        {
            return CoreBasics.Generic_ComposeL(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "<|")
        {
            return CoreBasics.Generic_ApL(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "|>")
        {
            return CoreBasics.Generic_ApR(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "::")
        {
            // Cons operator: prepend element to list
            // head :: tail  ==>  concat([[head], tail])
            var singletonList = Expression.ListInst([leftCompiled]);

            return
                BuiltinHelpers.ApplyBuiltinConcat(
                    [singletonList, rightCompiled]);
        }

        if (operatorApp.Operator is "/")
        {
            return CoreBasics.Float_div(leftCompiled, rightCompiled);
        }

        return CompilationError.UnsupportedOperator(operatorApp.Operator);
    }

}
