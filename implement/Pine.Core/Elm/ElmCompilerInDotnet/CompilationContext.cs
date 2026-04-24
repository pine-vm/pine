using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using SyntaxModelTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

internal static class QualifiedNameHelper
{
    public static string ToQualifiedNameString(IReadOnlyList<string> moduleName, string name) =>
        moduleName.Count is 0
        ?
        name
        :
        string.Join(".", moduleName) + "." + name;

    public static string ToQualifiedNameString(SyntaxModelTypes.QualifiedNameRef qualifiedName) =>
        ToQualifiedNameString(qualifiedName.ModuleName, qualifiedName.Name);

    public static SyntaxModelTypes.QualifiedNameRef ToQualifiedNameRef(IReadOnlyList<string> moduleName, string name) =>
        new(moduleName, name);

    public static SyntaxModelTypes.QualifiedNameRef FromQualifiedNameString(string qualifiedName) =>
        SyntaxModelTypes.QualifiedNameRef.FromFullName(qualifiedName);
}

/// <summary>
/// Information about a compiled function including its dependency layout.
/// <para>
/// Carries the four properties described in §2.1 of
/// <c>explore/internal-analysis/2026-04-22-analysis-inline-non-recursive-callees.md</c>:
/// <list type="bullet">
///   <item><see cref="CompiledValue"/> — the wrapper value (Form-B representation),</item>
///   <item><see cref="EncodedBody"/> — the encoded body (Form-A representation),</item>
///   <item><see cref="ParameterCount"/> — the function's declared arity,</item>
///   <item><see cref="EnvFunctions"/> — the env-functions list captured by the wrapper.</item>
/// </list>
/// </para>
/// <para>
/// As of §7.6a (analysis-doc snapshot 2026-04-23), <see cref="EnvFunctions"/> mirrors
/// the entire <see cref="DependencyLayout"/> (members ++ additional dependencies) so
/// the existing call-site emitter (which reads cross-SCC callees from <c>env[0][k]</c>
/// of the caller's layout) keeps working unchanged. The migration to a members-only
/// env-functions list and the corresponding emitter switch land jointly in §7.7.
/// </para>
/// </summary>
/// <param name="CompiledValue">The compiled Pine value for the function (the wrapper).</param>
/// <param name="EncodedBody">
/// The encoded inner expression (body) of the function.
/// This is used when building the env functions list for dependent functions
/// and is the artifact a saturated Form-A call site evaluates.
/// </param>
/// <param name="DependencyLayout">
/// The list of qualified function names this function depends on (in order).
/// For functions in an SCC, this is the shared layout for all SCC members.
/// For standalone functions, this is [self, dependencies...].
/// This ordering is used to build the runtime environment when calling the function.
/// </param>
/// <param name="ParameterCount">
/// The function's declared arity (number of formal parameters). Used by the
/// call-site emitter to pick between the saturated (Form A) and curried (Form B)
/// emission shapes.
/// </param>
/// <param name="EnvFunctions">
/// The env-functions list baked into <see cref="CompiledValue"/>. Per §7.6b this
/// equals the SCC member list for recursive members and is the empty list for
/// non-recursive single-member SCCs. Approach A1 (always emit WithEnvFunctions)
/// keeps the wrapper shape uniform: env[0] always holds the env-functions list,
/// even when that list is empty.
/// </param>
public record CompiledFunctionInfo(
    PineValue CompiledValue,
    PineValue EncodedBody,
    IReadOnlyList<string> DependencyLayout,
    int ParameterCount,
    IReadOnlyList<PineValue> EnvFunctions);

/// <summary>
/// Aggregates the type information known for a function.
/// </summary>
public record FunctionTypeInfo(
    TypeInference.InferredType ReturnType,
    IReadOnlyList<TypeInference.InferredType> ParameterTypes);

/// <summary>
/// Represents a Strongly Connected Component (SCC) of functions.
/// All functions in an SCC share the same dependency layout as per the spec:
/// "All functions in a group of mutually recursive functions use the same order."
///
/// <para>
/// As of §7.6b (analysis-doc 2026-04-23) the runtime env-functions list — and
/// therefore the per-member <see cref="GetLayout"/> — contains only the SCC
/// members. Cross-SCC dependencies are still tracked in
/// <see cref="AdditionalDependencies"/> so the compiler can order SCC compilation
/// (dependencies first), but they are no longer threaded through
/// <c>current_env[0]</c>: every cross-SCC callee is now reached at its call site
/// via a <c>Literal(callee.EncodedBody)</c> reference plus a
/// <c>Literal(List(callee.EnvFunctions))</c> env-functions literal sourced from
/// the cached <see cref="CompiledFunctionInfo"/>.
/// </para>
///
/// Note: Even single-function self-recursive functions form an SCC with one member.
/// Non-recursive standalone functions also form trivial single-member SCCs for consistency.
/// </summary>
/// <param name="Members">
/// The qualified names of all functions in this SCC. Must have at least one member.
/// </param>
/// <param name="AdditionalDependencies">
/// Cross-SCC dependencies (already compiled by the time this SCC is processed).
/// Used only for compilation ordering; not part of the runtime env-functions list.
/// </param>
/// <param name="IsRecursive">
/// True when this SCC contains real recursion: either it has more than one member
/// (mutual recursion among members), or its single member directly references
/// itself in its body. False only for single-member SCCs whose lone member does
/// not refer to itself; per §7.6b such callees have an empty runtime env-functions
/// list (no need for an env-functions slot), though Approach A1 keeps the wrapper
/// shape uniform — env[0] still holds the (empty) env-functions list.
/// </param>
public record FunctionScc(
    ImmutableList<string> Members,
    ImmutableList<string> AdditionalDependencies,
    bool IsRecursive)
{
    /// <summary>
    /// Gets the runtime dependency layout for this SCC.
    /// <para>
    /// For recursive SCCs (mutual recursion or self-recursion) the layout is
    /// the SCC members in alphabetical order — every member can reach every
    /// other through <c>current_env[0][k]</c>.
    /// </para>
    /// <para>
    /// For non-recursive single-member SCCs (§7.6b) the layout is empty: the
    /// member's body has no use for an env-functions slot. Approach A1 still
    /// emits the uniform WithEnvFunctions wrapper, with env[0] holding the
    /// empty env-functions list.
    /// </para>
    /// <para>
    /// <see cref="AdditionalDependencies"/> are deliberately excluded — see
    /// the type-level remarks for the §7.6b rationale.
    /// </para>
    /// </summary>
    public IReadOnlyList<string> GetLayout() => IsRecursive ? Members : [];
}

/// <summary>
/// Immutable context for module compilation.
/// Uses builder pattern for state changes instead of mutable cache.
/// </summary>
/// <param name="AllFunctions">All available functions across all modules.</param>
/// <param name="CompiledFunctionsCache">Cache of already compiled functions with their dependency layouts.</param>
/// <param name="PineKernelModuleNames">Names of Pine kernel modules.</param>
/// <param name="FunctionDependencyLayouts">Pre-computed dependency layouts for all functions (populated before compilation).</param>
/// <param name="FunctionTypes">Map of qualified function names to their return and parameter types.</param>
/// <param name="ChoiceTagArgumentTypes">Map of qualified choice type tag names to their argument types (for type inference from NamedPatterns).</param>
/// <param name="RecordTypeAliasConstructors">Map of qualified record type alias names to their field names in declaration order (for record constructors).</param>
public record ModuleCompilationContext(
    IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)> AllFunctions,
    ImmutableDictionary<SyntaxModelTypes.QualifiedNameRef, CompiledFunctionInfo> CompiledFunctionsCache,
    FrozenSet<string> PineKernelModuleNames,
    IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>>? FunctionDependencyLayouts = null,
    IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, FunctionTypeInfo>? FunctionTypes = null,
    IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>? ChoiceTagArgumentTypes = null,
    IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>>? RecordTypeAliasConstructors = null)
{
    /// <summary>
    /// Creates a new context with the specified function added to the cache.
    /// </summary>
    public ModuleCompilationContext WithCompiledFunction(
        string name,
        PineValue value,
        PineValue encodedBody,
        IReadOnlyList<string> dependencyLayout,
        int parameterCount,
        IReadOnlyList<PineValue> envFunctions) =>
        WithCompiledFunction(
            QualifiedNameHelper.FromQualifiedNameString(name),
            value,
            encodedBody,
            dependencyLayout,
            parameterCount,
            envFunctions);

    /// <summary>
    /// Creates a new context with the specified function added to the cache.
    /// </summary>
    public ModuleCompilationContext WithCompiledFunction(
        SyntaxModelTypes.QualifiedNameRef name,
        PineValue value,
        PineValue encodedBody,
        IReadOnlyList<string> dependencyLayout,
        int parameterCount,
        IReadOnlyList<PineValue> envFunctions) =>
        this with
        {
            CompiledFunctionsCache =
            CompiledFunctionsCache.SetItem(
                name,
                new CompiledFunctionInfo(
                    CompiledValue: value,
                    EncodedBody: encodedBody,
                    DependencyLayout: dependencyLayout,
                    ParameterCount: parameterCount,
                    EnvFunctions: envFunctions))
        };

    /// <summary>
    /// Creates a new context with the specified dependency layouts.
    /// </summary>
    public ModuleCompilationContext WithDependencyLayouts(IReadOnlyDictionary<string, IReadOnlyList<string>> layouts) =>
        WithDependencyLayouts(
            layouts.ToDictionary(
                kvp => QualifiedNameHelper.FromQualifiedNameString(kvp.Key),
                kvp => kvp.Value));

    /// <summary>
    /// Creates a new context with the specified dependency layouts.
    /// </summary>
    public ModuleCompilationContext WithDependencyLayouts(
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, IReadOnlyList<string>> layouts) =>
        this with { FunctionDependencyLayouts = layouts };

    /// <summary>
    /// Gets a compiled function from the cache.
    /// </summary>
    public PineValue? TryGetCompiledFunctionValue(string qualifiedName)
    {
        return TryGetCompiledFunctionValue(QualifiedNameHelper.FromQualifiedNameString(qualifiedName));
    }

    /// <summary>
    /// Gets a compiled function from the cache.
    /// </summary>
    public PineValue? TryGetCompiledFunctionValue(SyntaxModelTypes.QualifiedNameRef qualifiedName)
    {
        if (CompiledFunctionsCache.TryGetValue(qualifiedName, out var info))
        {
            return info.CompiledValue;
        }

        return null;
    }

    /// <summary>
    /// Gets compiled function info including dependency layout from the cache.
    /// </summary>
    public bool TryGetCompiledFunctionInfo(string qualifiedName, out CompiledFunctionInfo? info) =>
        TryGetCompiledFunctionInfo(QualifiedNameHelper.FromQualifiedNameString(qualifiedName), out info);

    /// <summary>
    /// Gets compiled function info including dependency layout from the cache.
    /// </summary>
    public bool TryGetCompiledFunctionInfo(
        SyntaxModelTypes.QualifiedNameRef qualifiedName,
        out CompiledFunctionInfo? info) =>
        CompiledFunctionsCache.TryGetValue(qualifiedName, out info);

    /// <summary>
    /// Gets the pre-computed dependency layout for a function.
    /// </summary>
    public IReadOnlyList<string>? TryGetDependencyLayout(string qualifiedName)
    {
        return TryGetDependencyLayout(QualifiedNameHelper.FromQualifiedNameString(qualifiedName));
    }

    /// <summary>
    /// Gets the pre-computed dependency layout for a function.
    /// </summary>
    public IReadOnlyList<string>? TryGetDependencyLayout(SyntaxModelTypes.QualifiedNameRef qualifiedName)
    {
        if (FunctionDependencyLayouts?.TryGetValue(qualifiedName, out var result) ?? false)
        {
            return result;
        }

        return null;
    }

    /// <summary>
    /// Gets function info from the all functions dictionary.
    /// </summary>
    public bool TryGetFunctionInfo(
        string qualifiedName,
        out (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration) info) =>
        TryGetFunctionInfo(QualifiedNameHelper.FromQualifiedNameString(qualifiedName), out info);

    /// <summary>
    /// Gets function info from the all functions dictionary.
    /// </summary>
    public bool TryGetFunctionInfo(
        SyntaxModelTypes.QualifiedNameRef qualifiedName,
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
    public bool IsPineKernelModule(string moduleName) =>
        PineKernelModuleNames.Contains(moduleName);

    /// <summary>
    /// Gets the recorded type information for a function.
    /// </summary>
    public FunctionTypeInfo? TryGetFunctionTypeInfo(string qualifiedName) =>
        TryGetFunctionTypeInfo(QualifiedNameHelper.FromQualifiedNameString(qualifiedName));

    /// <summary>
    /// Gets the recorded type information for a function.
    /// </summary>
    public FunctionTypeInfo? TryGetFunctionTypeInfo(SyntaxModelTypes.QualifiedNameRef qualifiedName)
    {
        if (FunctionTypes?.TryGetValue(qualifiedName, out var functionTypeInfo) ?? false)
        {
            return functionTypeInfo;
        }

        return null;
    }

    /// <summary>
    /// Tries to get the field names for a record type alias constructor.
    /// The field names are returned in the order they appear in the type alias declaration,
    /// which determines the order of constructor arguments.
    /// </summary>
    public IReadOnlyList<string>? TryGetRecordConstructorFieldNames(string qualifiedName)
    {
        return TryGetRecordConstructorFieldNames(QualifiedNameHelper.FromQualifiedNameString(qualifiedName));
    }

    /// <summary>
    /// Tries to get the field names for a record type alias constructor.
    /// </summary>
    public IReadOnlyList<string>? TryGetRecordConstructorFieldNames(SyntaxModelTypes.QualifiedNameRef qualifiedName)
    {
        if (RecordTypeAliasConstructors?.TryGetValue(qualifiedName, out var names) ?? false)
        {
            return names;
        }

        return null;
    }

    /// <summary>
    /// Gets the number of arguments expected by a choice type constructor.
    /// Returns null if the constructor is not found.
    /// </summary>
    public int? TryGetChoiceTypeConstructorArgumentCount(string qualifiedConstructorName)
    {
        return TryGetChoiceTypeConstructorArgumentCount(QualifiedNameHelper.FromQualifiedNameString(qualifiedConstructorName));
    }

    /// <summary>
    /// Gets the number of arguments expected by a choice type constructor.
    /// </summary>
    public int? TryGetChoiceTypeConstructorArgumentCount(SyntaxModelTypes.QualifiedNameRef qualifiedConstructorName)
    {
        if (ChoiceTagArgumentTypes?.TryGetValue(qualifiedConstructorName, out var argTypes) ?? false)
        {
            return argTypes.Count;
        }

        return null;
    }
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
/// <param name="LocalBindingTypes">Mapping from local binding names to their inferred types.</param>
/// <param name="DependencyLayout">Layout of function dependencies for the current function.</param>
/// <param name="ModuleCompilationContext">The parent module compilation context.</param>
/// <param name="FunctionTypes">Mapping from qualified function names to their return and parameter types.</param>
public record ExpressionCompilationContext(
    IReadOnlyDictionary<string, int> ParameterNames,
    IReadOnlyDictionary<string, TypeInference.InferredType> ParameterTypes,
    string CurrentModuleName,
    string? CurrentFunctionName,
    IReadOnlyDictionary<string, Expression>? LocalBindings,
    IReadOnlyDictionary<string, TypeInference.InferredType>? LocalBindingTypes,
    IReadOnlyList<string> DependencyLayout,
    ModuleCompilationContext ModuleCompilationContext,
    IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, FunctionTypeInfo>? FunctionTypes = null)
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
    /// Creates a new context with updated local bindings and their types (replacing existing).
    /// </summary>
    /// <param name="bindings">The new bindings dictionary.</param>
    /// <param name="bindingTypes">The new binding types dictionary.</param>
    /// <returns>A new context with the new bindings and types.</returns>
    public ExpressionCompilationContext WithReplacedLocalBindingsAndTypes(
        IReadOnlyDictionary<string, Expression>? bindings,
        IReadOnlyDictionary<string, TypeInference.InferredType>? bindingTypes) =>
        this with { LocalBindings = bindings, LocalBindingTypes = bindingTypes };

    /// <summary>
    /// Tries to get the type of a local binding by name.
    /// </summary>
    /// <param name="name">The binding name.</param>
    /// <param name="type">The type if found.</param>
    /// <returns>True if the binding type was found.</returns>
    public bool TryGetLocalBindingType(string name, out TypeInference.InferredType? type)
    {
        if (LocalBindingTypes is { } types && types.TryGetValue(name, out var t))
        {
            type = t;
            return true;
        }

        type = null;
        return false;
    }

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
