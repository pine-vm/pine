using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Represents a Pine program in its static (analyzed) form.
/// A static program consists of an optional entry-point expression and a set of named function bodies,
/// parameterized by the function identifier type.
/// </summary>
/// <typeparam name="TFuncId">
/// The type used to identify functions in the program.
/// For example, <see cref="DeclQualifiedName"/> for named programs or
/// <see cref="StaticFunctionIdentifier"/> for programs keyed by encoded expression + env class.
/// </typeparam>
/// <param name="EntryPoint">
/// The top-level parsed static expression, or <c>null</c> if the program has no single entry point
/// (e.g. when representing a collection of named module declarations).
/// </param>
/// <param name="NamedFunctions">
/// Map from function identifier to the function's parsed body expression.
/// </param>
public record StaticProgram<TFuncId>(
    StaticExpression<TFuncId>? EntryPoint,
    IReadOnlyDictionary<TFuncId, StaticExpression<TFuncId>> NamedFunctions)
    where TFuncId : notnull;

/// <summary>
/// Metadata about a function in a <see cref="StaticProgram{TFuncId}"/> that is derived during
/// static analysis but not stored in the program itself.
/// This metadata is computed as a post-processing step and passed to consumers that need it
/// (e.g., C# code generation, display rendering).
/// </summary>
/// <param name="OrigExpr">The original Pine expression before static lowering.</param>
/// <param name="Interface">The function's parameter structure derived from its body.</param>
/// <param name="Constraint">The value-class constraints for the function's environment.</param>
public record StaticProgramFunctionMetadata(
    Expression OrigExpr,
    StaticFunctionInterface Interface,
    PineValueClass Constraint);
