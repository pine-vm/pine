using System.Collections.Generic;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Represents errors that can occur during Elm compilation.
/// Provides structured, actionable error information instead of using plain strings.
/// </summary>
public abstract record CompilationError
{
    /// <summary>
    /// Describe the context of an error as a string.
    /// </summary>
    public static CompilationError Scoped(CompilationError innerError, string scopeDescription) =>
        new ScopedError(innerError, scopeDescription);

    /// <summary>
    /// The specified operator is not supported by the compiler.
    /// </summary>
    public static CompilationError UnsupportedOperator(string operatorSymbol) =>
        new UnsupportedOperatorError(operatorSymbol);

    /// <summary>
    /// Expression type is not supported by the compiler.
    /// </summary>
    public static CompilationError UnsupportedExpression(string expressionType) =>
        new UnsupportedExpressionError(expressionType);

    /// <summary>
    /// Describe the context of an error as a string.
    /// </summary>
    public sealed record ScopedError(CompilationError InnerError, string ScopeDescription) : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            $"In scope '{ScopeDescription}': {InnerError}";
    }

    /// <summary>
    /// Expression type is not supported by the compiler.
    /// </summary>
    public sealed record UnsupportedExpressionError(string ExpressionType) : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            $"Unsupported expression type: {ExpressionType}";
    }

    /// <summary>
    /// The specified operator is not supported by the compiler.
    /// </summary>
    public sealed record UnsupportedOperatorError(string Operator) : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            $"Unsupported operator: {Operator}";
    }

    /// <summary>
    /// A reference could not be resolved in the given module.
    /// </summary>
    public sealed record UnresolvedReference(string Name, string ModuleName) : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            $"Unresolved reference '{Name}' in module '{ModuleName}'";
    }

    /// <summary>
    /// A cyclic dependency was detected between functions.
    /// </summary>
    public sealed record CyclicDependency(IReadOnlyList<string> Cycle) : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            $"Cyclic dependency detected: {string.Join(" -> ", Cycle)}";
    }

    /// <summary>
    /// A pattern type is not supported by the compiler.
    /// </summary>
    public sealed record UnsupportedPattern(string PatternType) : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            $"Unsupported pattern type: {PatternType}";
    }

    /// <summary>
    /// A function was not found in the dependency layout.
    /// </summary>
    public sealed record FunctionNotInDependencyLayout(string FunctionName) : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            $"Function '{FunctionName}' not found in dependency layout";
    }

    /// <summary>
    /// Let functions with parameters are not yet supported.
    /// </summary>
    public sealed record UnsupportedLetFunctionWithParameters(string FunctionName) : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            $"Let function '{FunctionName}' with parameters is not yet supported";
    }

    /// <summary>
    /// Case expression has no patterns.
    /// </summary>
    public sealed record CaseExpressionNoPatterns() : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            "Case expression has no patterns - this should not happen with well-formed Elm code";
    }

    /// <summary>
    /// Application must have at least 2 arguments.
    /// </summary>
    public sealed record ApplicationTooFewArguments(int ArgumentCount) : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            $"Application must have at least 2 arguments (function and argument), got {ArgumentCount}";
    }

    /// <summary>
    /// Only Pine_kernel applications and function references are supported.
    /// </summary>
    public sealed record UnsupportedApplicationType() : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            "Only Pine_kernel applications and function references are supported";
    }

    /// <summary>
    /// Convert the error to a human-readable string.
    /// </summary>
    public abstract override string ToString();
}
