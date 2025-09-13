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
/// <c>body</c> is the static expression tree using <see cref="string"/> as the function identifier type,
/// and <c>constraint</c> contains the value-class constraints inferred/assigned for the function.
/// </param>
public record StaticProgram<TFunctionIdentifier>(
    IReadOnlyDictionary<string, (Expression origExpr, StaticExpression<TFunctionIdentifier> body, PineValueClass constraint)> NamedFunctions);
