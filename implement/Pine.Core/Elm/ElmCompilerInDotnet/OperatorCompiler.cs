using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

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
        // Use type inference to determine the operation type
        var expressionType =
            TypeInference.InferExpressionType(
                operatorApp,
                context.ParameterNames,
                context.ParameterTypes,
                context.LocalBindingTypes,
                context.CurrentModuleName,
                context.FunctionReturnTypes);

        var leftResult = ExpressionCompiler.Compile(operatorApp.Left.Value, context);

        if (leftResult.IsErrOrNull() is { } leftErr)
        {
            return
                CompilationError.Scoped(
                    leftErr,
                    "compiling left side of operator " + operatorApp.Operator);
        }

        var rightResult = ExpressionCompiler.Compile(operatorApp.Right.Value, context);

        if (rightResult.IsErrOrNull() is { } rightErr)
        {
            return
                CompilationError.Scoped(
                    rightErr,
                    "compiling right side of operator " + operatorApp.Operator);
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

        if (expressionType is TypeInference.InferredType.IntType)
        {
            return
                CompileIntegerOperator(
                    operatorApp,
                    leftCompiled,
                    rightCompiled);
        }

        if (operatorApp.Operator is "//")
        {
            return BasicArithmetic.Int_div(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "+")
        {
            return BasicArithmetic.Generic_Add(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "-")
        {
            return BasicArithmetic.Generic_Sub(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "*")
        {
            return BasicArithmetic.Generic_Mul(leftCompiled, rightCompiled);
        }

        if (operatorApp.Operator is "/")
        {
            return CompilationError.UnsupportedOperator(operatorApp.Operator);
        }

        return CompilationError.UnsupportedOperator(operatorApp.Operator);
    }

    private static Result<CompilationError, Expression> CompileIntegerOperator(
        SyntaxTypes.Expression.OperatorApplication operatorApp,
        Expression leftCompiled,
        Expression rightCompiled)
    {

        return operatorApp.Operator switch
        {
            "+" =>
            BuiltinHelpers.ApplyBuiltinIntAdd([leftCompiled, rightCompiled]),

            "-" =>
            BasicArithmetic.Int_sub(leftCompiled, rightCompiled),

            "*" =>
            BuiltinHelpers.ApplyBuiltinIntMul([leftCompiled, rightCompiled]),

            "//" =>
            BasicArithmetic.Int_div(leftCompiled, rightCompiled),

            _ =>
            CompilationError.UnsupportedOperator(operatorApp.Operator)
        };
    }
}
