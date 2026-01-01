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
        var expressionType = TypeInference.InferExpressionType(
            operatorApp,
            context.ParameterNames,
            context.ParameterTypes);

        if (expressionType is TypeInference.InferredType.IntType)
        {
            return CompileIntegerOperator(operatorApp, context);
        }

        return new CompilationError.UnsupportedOperator(operatorApp.Operator);
    }

    private static Result<CompilationError, Expression> CompileIntegerOperator(
        SyntaxTypes.Expression.OperatorApplication operatorApp,
        ExpressionCompilationContext context)
    {
        var leftResult = ExpressionCompiler.Compile(operatorApp.Left.Value, context);
        if (leftResult.IsErrOrNull() is { } leftErr)
        {
            return leftErr;
        }

        var rightResult = ExpressionCompiler.Compile(operatorApp.Right.Value, context);
        if (rightResult.IsErrOrNull() is { } rightErr)
        {
            return rightErr;
        }

        var leftCompiled = leftResult.IsOkOrNull()!;
        var rightCompiled = rightResult.IsOkOrNull()!;

        return operatorApp.Operator switch
        {
            "+" => BuiltinHelpers.ApplyBuiltinIntAdd([leftCompiled, rightCompiled]),

            "-" =>
                // Subtraction: a - b = a + (-1 * b)
                BuiltinHelpers.ApplyBuiltinIntAdd(
                [
                    leftCompiled,
                    BuiltinHelpers.ApplyBuiltinIntMul(
                    [
                        Expression.LiteralInstance(ExpressionCompiler.EmitIntegerLiteral(-1)),
                        rightCompiled
                    ])
                ]),

            "*" => BuiltinHelpers.ApplyBuiltinIntMul([leftCompiled, rightCompiled]),

            _ => new CompilationError.UnsupportedOperator(operatorApp.Operator)
        };
    }
}
