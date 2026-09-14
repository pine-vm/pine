using System.Collections.Generic;
using System.Linq;
using System.Text;

using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Represents errors that can occur during Elm compilation.
/// Provides structured, actionable error information instead of using plain strings.
/// </summary>
public abstract record CompilationError
{
    /// <summary>
    /// Identifies how one module became a dependency of another module.
    /// </summary>
    public enum ModuleDependencyKind
    {
        /// <summary>The importing module contains an import declaration.</summary>
        ExplicitImport,

        /// <summary>The Elm language makes the module available without a declaration.</summary>
        ImplicitImport,
    }

    /// <summary>
    /// Describes the import edge from the preceding module in a dependency chain.
    /// </summary>
    public sealed record ModuleDependencyOrigin(
        ModuleDependencyKind Kind,
        Range? Range,
        string? Alias,
        string? Exposing);

    /// <summary>
    /// One module in a root-to-error dependency chain. The root has no
    /// <see cref="Origin"/>; every subsequent item describes the import from its predecessor.
    /// </summary>
    public sealed record ModuleDependencyChainItem(
        string ModuleName,
        string FilePath,
        ModuleDependencyOrigin? Origin);

    /// <summary>
    /// One declaration in a root-to-failure dependency chain.
    /// </summary>
    public sealed record DeclarationDependencyChainItem(
        string DeclarationName,
        string? ReferencedBy,
        bool IsCompilationRoot);

    /// <summary>
    /// A canonicalization error together with the source file and complete chain that
    /// caused the module to participate in this compilation.
    /// </summary>
    public sealed record CanonicalizationDiagnostic(
        string ModuleName,
        string FilePath,
        CanonicalizationError Error,
        IReadOnlyList<ModuleDependencyChainItem> DependencyChain);

    /// <summary>
    /// One or more errors encountered while canonicalizing the reachable Elm modules.
    /// </summary>
    public sealed record CanonicalizationErrors(
        IReadOnlyList<CanonicalizationDiagnostic> Diagnostics)
        : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString()
        {
            var builder = new StringBuilder();

            builder.Append("Elm canonicalization failed with ");
            builder.Append(Diagnostics.Count);
            builder.Append(Diagnostics.Count is 1 ? " error." : " errors.");

            foreach (var diagnostic in Diagnostics)
            {
                builder.AppendLine();
                builder.AppendLine();
                AppendDiagnostic(builder, diagnostic);
            }

            return builder.ToString();
        }

        private static void AppendDiagnostic(
            StringBuilder builder,
            CanonicalizationDiagnostic diagnostic)
        {
            var range = diagnostic.Error.Range;

            builder.Append(ElmCompiler.RenderCanonicalizationError(diagnostic.Error));
            builder.Append(" at ");
            builder.Append(diagnostic.FilePath);
            builder.Append(':');
            builder.Append(range.Start.Row);
            builder.Append(':');
            builder.Append(range.Start.Column);
            builder.Append('-');
            builder.Append(range.End.Row);
            builder.Append(':');
            builder.Append(range.End.Column);
            builder.Append(" in module '");
            builder.Append(diagnostic.ModuleName);
            builder.AppendLine("'.");

            AppendDependencyChain(
                builder,
                "Dependency chain from a compilation root:",
                diagnostic.DependencyChain,
                static (chainBuilder, item, _) =>
                {
                    chainBuilder.Append(item.ModuleName);
                    chainBuilder.Append(" (");
                    chainBuilder.Append(item.FilePath);
                    chainBuilder.Append(')');

                    if (item.Origin is not { } origin)
                        return;

                    chainBuilder.Append(
                        origin.Kind is ModuleDependencyKind.ExplicitImport
                        ?
                        " — explicit import"
                        :
                        " — implicit Elm import");

                    if (origin.Range is { } importRange)
                    {
                        chainBuilder.Append(" at ");
                        chainBuilder.Append(importRange.Start.Row);
                        chainBuilder.Append(':');
                        chainBuilder.Append(importRange.Start.Column);
                    }

                    if (origin.Alias is { } alias)
                    {
                        chainBuilder.Append(" as ");
                        chainBuilder.Append(alias);
                    }

                    if (origin.Exposing is { } exposing)
                    {
                        chainBuilder.Append(" exposing ");
                        chainBuilder.Append(exposing);
                    }
                });

            if (diagnostic.Error is CanonicalizationError.UnresolvedReference unresolved &&
                diagnostic.DependencyChain.FirstOrDefault() is { } root)
            {
                builder.Append("The compiler searched for '");
                builder.Append(unresolved.Name);
                builder.Append("' while canonicalizing '");
                builder.Append(diagnostic.ModuleName);
                builder.Append("' because that module is reachable from root '");
                builder.Append(root.ModuleName);
                builder.Append("'.");
            }
        }

    }

    /// <summary>
    /// A declaration compilation error together with the SCC and the chain of
    /// declaration references that caused it to participate in the compilation.
    /// </summary>
    public sealed record DeclarationCompilationDiagnostic(
        string DeclarationName,
        IReadOnlyList<string> SccMembers,
        CompilationError Error,
        IReadOnlyList<DeclarationDependencyChainItem> DependencyChain)
        : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString()
        {
            var builder = new StringBuilder();

            builder.Append("Failed to compile declaration '");
            builder.Append(DeclarationName);
            builder.Append("' in SCC [");
            builder.Append(string.Join(", ", SccMembers));
            builder.AppendLine("].");
            builder.Append("Reason: ");
            builder.AppendLine(Error.ToString());

            if (DependencyChain.Count is not 0)
            {
                AppendDependencyChain(
                    builder,
                    "Declaration dependency chain from a compilation root:",
                    DependencyChain,
                    static (chainBuilder, item, _) =>
                    {
                        chainBuilder.Append(item.DeclarationName);

                        if (item.IsCompilationRoot)
                        {
                            chainBuilder.Append(" (compilation root)");
                        }
                        else if (item.ReferencedBy is { } referencedBy)
                        {
                            chainBuilder.Append(" — referenced by ");
                            chainBuilder.Append(referencedBy);
                        }
                    });
            }

            if (Error is FunctionNotInDependencyLayout missingFunction &&
                DependencyChain.FirstOrDefault() is { IsCompilationRoot: true } root)
            {
                builder.Append("The compiler searched for '");
                builder.Append(missingFunction.FunctionName);
                builder.Append("' while compiling '");
                builder.Append(DeclarationName);
                builder.Append("' because that declaration is reachable from compilation root '");
                builder.Append(root.DeclarationName);
                builder.Append("'.");
            }

            return builder.ToString();
        }
    }

    /// <summary>
    /// Identifies the declaration whose body produced an error.
    /// </summary>
    public sealed record InDeclaration(
        string DeclarationName,
        CompilationError InnerError)
        : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            $"Failed compiling declaration '{DeclarationName}': {InnerError}";
    }

    /// <summary>
    /// An error represented by an already formatted message.
    /// </summary>
    public sealed record Message(string Text) : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() => Text;
    }

    /// <summary>
    /// Describe the context of an error as a string.
    /// </summary>
    public static CompilationError Scoped(string scopeDescription, CompilationError innerError) =>
        new ScopedError(scopeDescription, innerError);

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
    public sealed record ScopedError(string ScopeDescription, CompilationError InnerError)
        : CompilationError
    {
        /// <inheritdoc/>
        public override string ToString() =>
            $"In scope '{ScopeDescription}': {InnerError}";
    }

    /// <summary>
    /// Describe the context of an error as a string.
    /// </summary>
    public CompilationError Scoped(string scopeDescription) =>
        new ScopedError(scopeDescription, this);

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

    private static void AppendDependencyChain<T>(
        StringBuilder builder,
        string heading,
        IReadOnlyList<T> items,
        System.Action<StringBuilder, T, int> appendItem)
    {
        builder.AppendLine(heading);

        for (var index = 0; index < items.Count; index++)
        {
            builder.Append("  ");
            builder.Append(index + 1);
            builder.Append(". ");
            appendItem(builder, items[index], index);
            builder.AppendLine();
        }
    }
}
