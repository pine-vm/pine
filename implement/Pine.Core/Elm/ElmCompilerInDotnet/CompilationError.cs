using System.Collections.Generic;
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
    /// Identifies whether a source import is explicit or supplied by the Elm language.
    /// </summary>
    public enum ImportKind
    {
        /// <summary>The importing module contains an import declaration.</summary>
        ExplicitImport,

        /// <summary>The Elm language makes the module available without a declaration.</summary>
        ImplicitImport,
    }

    /// <summary>
    /// Describes an import that makes a referenced declaration visible in a source scope.
    /// </summary>
    public sealed record ImportOrigin(
        ImportKind Kind,
        Range? Range,
        string? Alias,
        string? Exposing);

    /// <summary>
    /// One declaration in a root-to-failure dependency chain.
    /// </summary>
    public sealed record DeclarationDependencyChainItem(
        string DeclarationName,
        string? ReferencedBy,
        bool IsCompilationRoot)
    {
        /// <summary>Original source location, if this is a source declaration rather than a generated helper.</summary>
        public string? FilePath { get; init; }

        /// <summary>The complete source declaration range.</summary>
        public Range? DeclarationRange { get; init; }

        /// <summary>The reference in the preceding declaration that demands this declaration.</summary>
        public Range? ReferenceRange { get; init; }

        /// <summary>The actual referenced symbol, which can be a constructor rather than its owning type.</summary>
        public string? ReferencedName { get; init; }

        /// <summary>True for references in signatures and type definitions.</summary>
        public bool IsTypeReference { get; init; }

        /// <summary>Import metadata explains scope visibility; imports are not dependency edges.</summary>
        public ImportOrigin? Import { get; init; }
    }

    /// <summary>
    /// A canonicalization error together with the source file and complete chain that
    /// caused the declaration to be demanded by this compilation.
    /// </summary>
    public sealed record CanonicalizationDiagnostic(
        string DeclarationName,
        string FilePath,
        CanonicalizationError Error,
        IReadOnlyList<DeclarationDependencyChainItem> DependencyChain)
    {
        /// <summary>The supplied source scope searched for an unresolved member.</summary>
        public string? TargetSourcePath { get; init; }

        /// <summary>Declaration location for a private member, otherwise the target module header.</summary>
        public Range? TargetSourceRange { get; init; }

        /// <summary>The import relevant to the failing reference, not an additional demand edge.</summary>
        public ImportOrigin? ReferenceImport { get; init; }
    }

    /// <summary>
    /// One or more errors encountered while canonicalizing demanded declarations.
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

        internal static void AppendDiagnostic(
            StringBuilder builder,
            CanonicalizationDiagnostic diagnostic,
            bool includeDependencyChain = true)
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
            builder.Append(" in declaration '");
            builder.Append(diagnostic.DeclarationName);
            builder.AppendLine("'.");

            if (diagnostic.Error is CanonicalizationError.UnresolvedReference unresolved &&
                unresolved.ResolutionDetail is { } detail)
            {
                builder.Append("Resolution: ");
                builder.AppendLine(detail);
            }

            if (diagnostic.TargetSourcePath is { } targetPath)
            {
                builder.Append("Target source: ");
                AppendSourceLocation(builder, targetPath, diagnostic.TargetSourceRange);
                builder.AppendLine();
            }

            if (diagnostic.ReferenceImport is { } referenceImport)
            {
                builder.Append(
                    referenceImport.Kind is ImportKind.ExplicitImport
                    ?
                    "Reference scope: import"
                    :
                    "Reference scope: implicit Elm import");

                if (referenceImport.Range is { } importRange)
                {
                    builder.Append(" at ");
                    AppendSourceLocation(builder, diagnostic.FilePath, importRange);
                }

                if (referenceImport.Alias is { } alias)
                    builder.Append(" as ").Append(alias);

                if (referenceImport.Exposing is { } exposing)
                    builder.Append(" exposing ").Append(exposing);

                builder.AppendLine();
            }

            if (includeDependencyChain)
                AppendDeclarationDependencyChain(builder, diagnostic.DependencyChain);
        }

        /// <summary>Renders one complete declaration diagnostic without a compilation-wide summary.</summary>
        public static string RenderDiagnostic(
            CanonicalizationDiagnostic diagnostic,
            bool includeDependencyChain = true)
        {
            var builder = new StringBuilder();
            AppendDiagnostic(builder, diagnostic, includeDependencyChain);
            return builder.ToString().TrimEnd();
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
            builder.AppendLine("'.");

            if (SccMembers.Count > 1)
            {
                builder.Append("Recursive declaration group: ");
                builder.AppendLine(string.Join(", ", SccMembers));
            }

            builder.Append("Reason: ");
            builder.AppendLine(Error.ToString());

            if (DependencyChain.Count is not 0)
            {
                AppendDeclarationDependencyChain(
                    builder,
                    DependencyChain);
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
            $"Cannot compile reference '{FunctionName}': no executable implementation is available in the selected compiler environment.";
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

    private static void AppendDeclarationDependencyChain(
        StringBuilder builder,
        IReadOnlyList<DeclarationDependencyChainItem> items) =>
        AppendDependencyChain(
            builder,
            "Declaration dependency chain from a compilation root (references, not module imports):",
            items,
            (text, item, index) =>
            {
                text.Append(item.DeclarationName);

                if (item.IsCompilationRoot)
                    text.Append(" (compilation root)");

                else if (item.ReferencedBy is { } parent)
                {
                    text.Append(" — referenced by ");
                    text.Append(parent);
                }

                if (item.FilePath is { } path)
                {
                    text.AppendLine();

                    text.Append(
                        item.DeclarationRange is not null
                        ?
                        "     declaration at "
                        :
                        "     source namespace file ");

                    AppendSourceLocation(text, path, item.DeclarationRange);

                    if (item.DeclarationRange is null)
                        text.Append(" (original declaration location unavailable for generated or transformed code)");
                }

                if (item.ReferenceRange is { } reference && index > 0)
                {
                    text.AppendLine();
                    text.Append(item.IsTypeReference ? "     type reference" : "     value reference");

                    if (item.ReferencedName is { } symbol)
                    {
                        text.Append(" '");
                        text.Append(symbol);
                        text.Append('\'');
                    }

                    text.Append(" at ");
                    AppendSourceLocation(text, items[index - 1].FilePath, reference);

                    if (item.ReferencedName is { } referencedName && referencedName != item.DeclarationName)
                        text.Append(" (the referenced symbol belongs to this declaration)");
                }

                if (item.Import is { } import)
                {
                    text.AppendLine();

                    text.Append(
                        import.Kind is ImportKind.ExplicitImport
                        ?
                        "     scope provided by import"
                        :
                        "     scope provided by implicit Elm import");

                    if (import.Range is { } range && index > 0)
                    {
                        text.Append(" at ");
                        AppendSourceLocation(text, items[index - 1].FilePath, range);
                    }

                    if (import.Alias is { } alias)
                    {
                        text.Append(" as ");
                        text.Append(alias);
                    }

                    if (import.Exposing is { } exposing)
                    {
                        text.Append(" exposing ");
                        text.Append(exposing);
                    }
                }
            });

    internal static void AppendSourceLocation(StringBuilder builder, string? path, Range? range)
    {
        builder.Append(path ?? "(source path unavailable)");

        if (range is not { } location)
            return;

        builder.Append(':').Append(location.Start.Row).Append(':').Append(location.Start.Column);
        builder.Append('-').Append(location.End.Row).Append(':').Append(location.End.Column);
    }

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
