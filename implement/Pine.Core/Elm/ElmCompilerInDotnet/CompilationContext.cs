using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Information about a compiled function including its dependency layout.
/// </summary>
/// <param name="CompiledValue">The compiled Pine value for the function.</param>
/// <param name="DependencyLayout">
/// The list of qualified function names this function depends on (in order).
/// The first element is always the function itself, followed by its direct dependencies.
/// This ordering is used to build the runtime environment when calling the function.
/// </param>
public record CompiledFunctionInfo(
    PineValue CompiledValue,
    IReadOnlyList<string> DependencyLayout);

/// <summary>
/// Immutable context for module compilation.
/// Uses builder pattern for state changes instead of mutable cache.
/// This is the new immutable version of ModuleCompilationContext.
/// </summary>
/// <param name="AllFunctions">All available functions across all modules.</param>
/// <param name="CompiledFunctionsCache">Cache of already compiled functions with their dependency layouts.</param>
/// <param name="PineKernelModuleNames">Names of Pine kernel modules.</param>
/// <param name="FunctionDependencyLayouts">Pre-computed dependency layouts for all functions (populated before compilation).</param>
public record ModuleCompilationContext(
    IReadOnlyDictionary<string, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)> AllFunctions,
    ImmutableDictionary<string, CompiledFunctionInfo> CompiledFunctionsCache,
    FrozenSet<string> PineKernelModuleNames,
    IReadOnlyDictionary<string, IReadOnlyList<string>>? FunctionDependencyLayouts = null)
{
    /// <summary>
    /// Creates a new context with the specified function added to the cache.
    /// </summary>
    /// <param name="name">The qualified function name.</param>
    /// <param name="value">The compiled Pine value.</param>
    /// <param name="dependencyLayout">The function's dependency layout.</param>
    /// <returns>A new context with the updated cache.</returns>
    public ModuleCompilationContext WithCompiledFunction(string name, PineValue value, IReadOnlyList<string> dependencyLayout) =>
        this with { CompiledFunctionsCache = CompiledFunctionsCache.SetItem(name, new CompiledFunctionInfo(value, dependencyLayout)) };

    /// <summary>
    /// Creates a new context with the specified function added to the cache (without dependency layout for backward compatibility).
    /// </summary>
    /// <param name="name">The qualified function name.</param>
    /// <param name="value">The compiled Pine value.</param>
    /// <returns>A new context with the updated cache.</returns>
    public ModuleCompilationContext WithCompiledFunction(string name, PineValue value) =>
        WithCompiledFunction(name, value, []);

    /// <summary>
    /// Creates a new context with the specified dependency layouts.
    /// </summary>
    /// <param name="layouts">The dependency layouts dictionary.</param>
    /// <returns>A new context with the dependency layouts.</returns>
    public ModuleCompilationContext WithDependencyLayouts(IReadOnlyDictionary<string, IReadOnlyList<string>> layouts) =>
        this with { FunctionDependencyLayouts = layouts };

    /// <summary>
    /// Checks if a function is already in the cache.
    /// </summary>
    /// <param name="qualifiedName">The qualified function name.</param>
    /// <returns>True if the function is cached.</returns>
    public bool HasCompiledFunction(string qualifiedName) =>
        CompiledFunctionsCache.ContainsKey(qualifiedName);

    /// <summary>
    /// Gets a compiled function from the cache.
    /// </summary>
    /// <param name="qualifiedName">The qualified function name.</param>
    /// <param name="value">The compiled value if found.</param>
    /// <returns>True if the function was found.</returns>
    public bool TryGetCompiledFunction(string qualifiedName, out PineValue? value)
    {
        if (CompiledFunctionsCache.TryGetValue(qualifiedName, out var info))
        {
            value = info.CompiledValue;
            return true;
        }
        value = null;
        return false;
    }

    /// <summary>
    /// Gets compiled function info including dependency layout from the cache.
    /// </summary>
    /// <param name="qualifiedName">The qualified function name.</param>
    /// <param name="info">The compiled function info if found.</param>
    /// <returns>True if the function was found.</returns>
    public bool TryGetCompiledFunctionInfo(string qualifiedName, out CompiledFunctionInfo? info) =>
        CompiledFunctionsCache.TryGetValue(qualifiedName, out info);

    /// <summary>
    /// Gets the pre-computed dependency layout for a function.
    /// </summary>
    /// <param name="qualifiedName">The qualified function name.</param>
    /// <param name="layout">The dependency layout if found.</param>
    /// <returns>True if the layout was found.</returns>
    public bool TryGetDependencyLayout(string qualifiedName, out IReadOnlyList<string>? layout)
    {
        if (FunctionDependencyLayouts?.TryGetValue(qualifiedName, out var result) ?? false)
        {
            layout = result;
            return true;
        }
        layout = null;
        return false;
    }

    /// <summary>
    /// Gets function info from the all functions dictionary.
    /// </summary>
    /// <param name="qualifiedName">The qualified function name.</param>
    /// <param name="info">The function info if found.</param>
    /// <returns>True if the function was found.</returns>
    public bool TryGetFunctionInfo(
        string qualifiedName,
        out (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration) info)
    {
        if (AllFunctions.TryGetValue(qualifiedName, out var result))
        {
            info = result;
            return true;
        }
        info = default;
        return false;
    }

    /// <summary>
    /// Checks if a module name is a Pine kernel module.
    /// </summary>
    /// <param name="moduleName">The module name to check.</param>
    /// <returns>True if it's a kernel module.</returns>
    public bool IsPineKernelModule(string moduleName) =>
        PineKernelModuleNames.Contains(moduleName);
}

/// <summary>
/// Immutable context for expression compilation.
/// Contains all information needed to compile an expression.
/// This is the new immutable version of ExpressionCompilationContext.
/// </summary>
/// <param name="ParameterNames">Mapping from parameter names to their indices.</param>
/// <param name="ParameterTypes">Mapping from parameter names to their inferred types.</param>
/// <param name="CurrentModuleName">Name of the module being compiled.</param>
/// <param name="CurrentFunctionName">Name of the function being compiled (if any).</param>
/// <param name="LocalBindings">Local variable bindings from let expressions or patterns.</param>
/// <param name="DependencyLayout">Layout of function dependencies for the current function.</param>
/// <param name="ModuleCompilationContext">The parent module compilation context.</param>
public record ExpressionCompilationContext(
    IReadOnlyDictionary<string, int> ParameterNames,
    IReadOnlyDictionary<string, TypeInference.InferredType> ParameterTypes,
    string CurrentModuleName,
    string? CurrentFunctionName,
    IReadOnlyDictionary<string, Expression>? LocalBindings,
    IReadOnlyList<string> DependencyLayout,
    ModuleCompilationContext ModuleCompilationContext)
{
    /// <summary>
    /// Creates a new context with additional local bindings.
    /// </summary>
    /// <param name="bindings">The bindings to add.</param>
    /// <returns>A new context with the updated bindings.</returns>
    public ExpressionCompilationContext WithLocalBindings(IReadOnlyDictionary<string, Expression> bindings)
    {
        if (bindings.Count is 0)
            return this;

        var newBindings = new Dictionary<string, Expression>();

        if (LocalBindings is { } existing)
        {
            foreach (var kvp in existing)
            {
                newBindings[kvp.Key] = kvp.Value;
            }
        }

        foreach (var kvp in bindings)
        {
            newBindings[kvp.Key] = kvp.Value;
        }

        return this with { LocalBindings = newBindings };
    }

    /// <summary>
    /// Creates a new context with updated local bindings (replacing existing).
    /// </summary>
    /// <param name="bindings">The new bindings dictionary.</param>
    /// <returns>A new context with the new bindings.</returns>
    public ExpressionCompilationContext WithReplacedLocalBindings(IReadOnlyDictionary<string, Expression>? bindings) =>
        this with { LocalBindings = bindings };

    /// <summary>
    /// Tries to get a local binding by name.
    /// </summary>
    /// <param name="name">The binding name.</param>
    /// <param name="expression">The bound expression if found.</param>
    /// <returns>True if the binding was found.</returns>
    public bool TryGetLocalBinding(string name, out Expression? expression)
    {
        if (LocalBindings is { } bindings && bindings.TryGetValue(name, out var expr))
        {
            expression = expr;
            return true;
        }
        expression = null;
        return false;
    }

    /// <summary>
    /// Tries to get a parameter index by name.
    /// </summary>
    /// <param name="name">The parameter name.</param>
    /// <param name="index">The parameter index if found.</param>
    /// <returns>True if the parameter was found.</returns>
    public bool TryGetParameterIndex(string name, out int index) =>
        ParameterNames.TryGetValue(name, out index);

    /// <summary>
    /// Gets the index of a function in the dependency layout.
    /// </summary>
    /// <param name="qualifiedName">The qualified function name.</param>
    /// <returns>The index, or null if not found.</returns>
    public int? GetFunctionIndexInLayout(string qualifiedName)
    {
        for (var i = 0; i < DependencyLayout.Count; i++)
        {
            if (DependencyLayout[i] == qualifiedName)
            {
                return i;
            }
        }

        return null;
    }
}
