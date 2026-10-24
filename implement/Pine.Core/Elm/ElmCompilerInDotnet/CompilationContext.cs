using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;

using PineExpression = Pine.Core.PineLanguage.Expression;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

internal static class QualifiedNameHelper
{
    public static string ToQualifiedNameString(IReadOnlyList<string> moduleName, string name) =>
        moduleName.Count is 0 ? name : string.Join(".", moduleName) + "." + name;

    public static string ToQualifiedNameString(DeclQualifiedName name) => name.FullName;

    public static DeclQualifiedName ToDeclQualifiedName(IReadOnlyList<string> moduleName, string name) =>
        DeclQualifiedName.Create(moduleName, name);

    public static DeclQualifiedName FromQualifiedNameString(string name) => DeclQualifiedName.FromString(name);
}

/// <summary>Only runtime constructor and kernel metadata. No source bodies or alternate function-name lookup exists.</summary>
public sealed record ModuleCompilationContext(
    FrozenSet<string> PineKernelModuleNames,
    ImmutableDictionary<DeclQualifiedName, ValueList<string>> RecordTypeAliasConstructors,
    ImmutableDictionary<DeclQualifiedName, int> ChoiceConstructorArities)
{
    /// <summary>Whether the namespace is a native kernel.</summary>
    public bool IsPineKernelModule(string name) => PineKernelModuleNames.Contains(name);

    /// <summary>Record-constructor field order.</summary>
    public ValueList<string>? TryGetRecordConstructorFieldNames(DeclQualifiedName name) =>
        RecordTypeAliasConstructors.GetValueOrDefault(name);

    /// <summary>Choice-constructor arity, when declared.</summary>
    public int? TryGetChoiceTypeConstructorArgumentCount(DeclQualifiedName name) =>
        ChoiceConstructorArities.TryGetValue(name, out var arity) ? arity : null;
}

/// <summary>Resolved expression compilation. Every lexical lookup uses BindingId and every function lookup uses FunctionId.</summary>
public sealed record ExpressionCompilationContext(
    IReadOnlyDictionary<BindingId, int> ParameterNames,
    IReadOnlyDictionary<BindingId, PineExpression> LocalBindings,
    SccCompilationContext ResolvedFunctions,
    ModuleCompilationContext ModuleCompilationContext,
    string CurrentModuleName,
    PineVMParseCache ParseCache,
    IDictionary<(PineExpression, ReductionConfig), PineExpression>? ReducedExpressionCache)
{
    /// <summary>Adds lexical bindings without changing their identities.</summary>
    public ExpressionCompilationContext WithLocalBindings(IReadOnlyDictionary<BindingId, PineExpression> bindings)
    {
        if (bindings.Count is 0)
            return this;

        var combined = new Dictionary<BindingId, PineExpression>(LocalBindings);

        foreach (var (id, value) in bindings)
            combined[id] = value;

        return this with { LocalBindings = combined };
    }

    /// <summary>Replaces the lexical environment.</summary>
    public ExpressionCompilationContext WithReplacedLocalBindings(
        IReadOnlyDictionary<BindingId, PineExpression> bindings) =>
        this with { LocalBindings = bindings };

    /// <summary>Looks up one lexical identity.</summary>
    public PineExpression? TryGetLocalBinding(BindingId binding) => LocalBindings.GetValueOrDefault(binding);

    /// <summary>Looks up a parameter identity.</summary>
    public int? TryGetParameterIndex(BindingId binding) =>
        ParameterNames.TryGetValue(binding, out var index) ? index : null;
}
