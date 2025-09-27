using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Represents a Pine program in its static (analyzed) form.
/// A static program is a set of named functions
/// </summary>
/// <param name="NamedFunctions">
/// Map from function name to a tuple of
/// (<see cref="Expression"/> <c>origExpr</c>, <see cref="StaticExpression{TFunctionIdentifier}"/> <c>body</c>, <see cref="PineValueClass"/> <c>constraint</c>).
/// <c>origExpr</c> is the original expression before static lowering,
/// <c>body</c> is the static expression tree using the same function identifiers as in the program,
/// and <c>constraint</c> contains the value-class constraints inferred/assigned for the function.
/// </param>
public record StaticProgram(
    IReadOnlyDictionary<DeclQualifiedName, (Expression origExpr, StaticFunctionInterface interf, StaticExpression<DeclQualifiedName> body, PineValueClass constraint)> NamedFunctions)
{
    /// <summary>
    /// Returns a <see cref="StaticExpressionDisplay.FunctionApplicationRendering"/> describing how to render
    /// an application of the specified function in a static expression display.
    /// </summary>
    /// <param name="functionName">The name of the function to retrieve rendering metadata for.</param>
    /// <returns>A rendering descriptor containing the function name and its static interface.</returns>
    /// <exception cref="KeyNotFoundException">Thrown when the function name is not present in <see cref="NamedFunctions"/>.</exception>
    public StaticExpressionDisplay.FunctionApplicationRendering GetFunctionApplicationRendering(DeclQualifiedName functionName)
    {
        if (NamedFunctions.TryGetValue(functionName, out var funcInfo))
        {
            return
                new StaticExpressionDisplay.FunctionApplicationRendering
                (
                    FunctionName: functionName.FullName,
                    FunctionInterface: funcInfo.interf
                );
        }

        throw new KeyNotFoundException($"Function '{functionName}' not found in static program.");
    }
}




