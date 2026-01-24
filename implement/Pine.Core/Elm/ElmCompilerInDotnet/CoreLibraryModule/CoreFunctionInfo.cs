using System;
using System.Collections.Generic;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;

/// <summary>
/// Information about a core module function, used for type inference and code generation.
/// </summary>
/// <param name="FunctionType">
/// The types in the function signature. For a binary function like <c>Int -> Int -> Int</c>,
/// this would be <c>[IntType, IntType, IntType]</c> where the last element is the return type.
/// </param>
/// <param name="CompileApplication">
/// A function that compiles applications of this function to a Pine expression.
/// Takes the compiled argument expressions and returns the compiled result.
/// </param>
public record CoreFunctionInfo(
    IReadOnlyList<TypeInference.InferredType> FunctionType,
    Func<IReadOnlyList<Expression>, Expression> CompileApplication);
