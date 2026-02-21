using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

using ModuleName = ImmutableArray<string>;

/// <summary>
/// Represents an imported module, optionally with an alias.
/// </summary>
public sealed class ImportedModule : IEquatable<ImportedModule>
{
    /// <summary>
    /// Canonical name of the imported module (e.g. ["Platform", "Cmd"]).
    /// </summary>
    public ModuleName ModuleName { get; }

    /// <summary>
    /// An optional alias for the module (e.g. "Cmd" for "Platform.Cmd").
    /// Null when no alias is defined.
    /// </summary>
    public string? Alias { get; }

    private ImportedModule(ModuleName moduleName, string? alias)
    {
        ModuleName = moduleName;
        Alias = alias;
    }

    /// <summary>
    /// Creates an imported module instance without an alias for the specified module name.
    /// </summary>
    public static ImportedModule WithoutAlias(ModuleName moduleName) =>
        new(moduleName, alias: null);

    /// <summary>
    /// Creates a new imported module with the specified alias.
    /// </summary>
    public static ImportedModule WithAlias(ModuleName moduleName, string alias) =>
        new(moduleName, alias);

    /// <inheritdoc/>
    public bool Equals(ImportedModule? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return ModuleName.AsSpan().SequenceEqual(other.ModuleName.AsSpan())
            && string.Equals(Alias, other.Alias, StringComparison.Ordinal);
    }

    /// <inheritdoc/>
    public override bool Equals(object? obj) =>
        Equals(obj as ImportedModule);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hash = new HashCode();

        foreach (var part in ModuleName)
            hash.Add(part);

        hash.Add(Alias);

        return hash.ToHashCode();
    }
}

/// <summary>
/// Represents the configuration for implicit module imports and the specific members they expose within the compilation
/// context.
/// </summary>
public record ImplicitImportConfig(
    ImmutableHashSet<ImportedModule> ModuleImports,
    IImmutableDictionary<string, ModuleName> TypeImports,
    IImmutableDictionary<string, ModuleName> ValueImports,
    IImmutableDictionary<string, (ModuleName ModuleName, string FunctionName)> OperatorToFunction)
{
    /// <summary>
    /// Elm 0.19 defaults.
    /// </summary>
    public static readonly ImplicitImportConfig Default = BuildDefault();

    /// <summary>
    /// Elm 0.19 defaults.
    /// </summary>
    private static ImplicitImportConfig BuildDefault()
    {
        var modules =
            ImmutableHashSet<ImportedModule>.Empty
            .Add(ImportedModule.WithoutAlias(["Basics"]))
            .Add(ImportedModule.WithoutAlias(["List"]))
            .Add(ImportedModule.WithoutAlias(["Maybe"]))
            .Add(ImportedModule.WithoutAlias(["Result"]))
            .Add(ImportedModule.WithoutAlias(["Tuple"]))
            .Add(ImportedModule.WithoutAlias(["Char"]))
            .Add(ImportedModule.WithoutAlias(["String"]))
            .Add(ImportedModule.WithoutAlias(["Debug"]))
            .Add(ImportedModule.WithoutAlias(["Platform"]))
            .Add(ImportedModule.WithAlias(["Platform", "Cmd"], "Cmd"))
            .Add(ImportedModule.WithAlias(["Platform", "Sub"], "Sub"));

        var typeImports =
            ImmutableDictionary<string, ModuleName>.Empty
            .Add("Int", ["Basics"])
            .Add("Float", ["Basics"])
            .Add("Bool", ["Basics"])
            .Add("True", ["Basics"])
            .Add("False", ["Basics"])
            .Add("Never", ["Basics"])
            .Add("Order", ["Basics"])
            .Add("LT", ["Basics"])
            .Add("EQ", ["Basics"])
            .Add("GT", ["Basics"])
            .Add("Char", ["Char"])
            .Add("String", ["String"])
            .Add("List", ["List"])
            .Add("Maybe", ["Maybe"])
            .Add("Nothing", ["Maybe"])
            .Add("Just", ["Maybe"])
            .Add("Result", ["Result"])
            .Add("Ok", ["Result"])
            .Add("Err", ["Result"])
            .Add("Program", ["Platform"])
            .Add("Task", ["Task"])
            .Add("Cmd", ["Platform", "Cmd"])
            .Add("Sub", ["Platform", "Sub"]);

        var valueImports =
            ImmutableDictionary<string, ModuleName>.Empty
            .Add("compare", ["Basics"])
            .Add("not", ["Basics"])
            .Add("xor", ["Basics"])
            .Add("max", ["Basics"])
            .Add("min", ["Basics"])
            .Add("modBy", ["Basics"])
            .Add("remainderBy", ["Basics"])
            .Add("negate", ["Basics"])
            .Add("abs", ["Basics"])
            .Add("clamp", ["Basics"])
            .Add("sqrt", ["Basics"])
            .Add("logBase", ["Basics"])
            .Add("e", ["Basics"])
            .Add("pi", ["Basics"])
            .Add("cos", ["Basics"])
            .Add("sin", ["Basics"])
            .Add("tan", ["Basics"])
            .Add("acos", ["Basics"])
            .Add("asin", ["Basics"])
            .Add("atan", ["Basics"])
            .Add("atan2", ["Basics"])
            .Add("degrees", ["Basics"])
            .Add("radians", ["Basics"])
            .Add("turns", ["Basics"])
            .Add("toFloat", ["Basics"])
            .Add("round", ["Basics"])
            .Add("floor", ["Basics"])
            .Add("ceiling", ["Basics"])
            .Add("truncate", ["Basics"])
            .Add("toPolar", ["Basics"])
            .Add("fromPolar", ["Basics"])
            .Add("isNaN", ["Basics"])
            .Add("isInfinite", ["Basics"])
            .Add("identity", ["Basics"])
            .Add("always", ["Basics"])
            .Add("never", ["Basics"]);

        // Mapping from infix operators to their underlying function names and modules
        // Based on infix declarations in Basics.elm and List.elm
        var operatorToFunction =
            ImmutableDictionary<string, (ModuleName ModuleName, string FunctionName)>.Empty
            // Basics operators
            .Add("+", (["Basics"], "add"))
            .Add("-", (["Basics"], "sub"))
            .Add("*", (["Basics"], "mul"))
            .Add("//", (["Basics"], "idiv"))
            .Add("^", (["Basics"], "pow"))
            .Add("++", (["Basics"], "append"))
            .Add("==", (["Basics"], "eq"))
            .Add("/=", (["Basics"], "neq"))
            .Add("<", (["Basics"], "lt"))
            .Add(">", (["Basics"], "gt"))
            .Add("<=", (["Basics"], "le"))
            .Add(">=", (["Basics"], "ge"))
            .Add("&&", (["Basics"], "and"))
            .Add("||", (["Basics"], "or"))
            .Add("|>", (["Basics"], "apR"))
            .Add("<|", (["Basics"], "apL"))
            .Add("<<", (["Basics"], "composeL"))
            .Add(">>", (["Basics"], "composeR"))
            // List operators
            .Add("::", (["List"], "cons"));

        return new ImplicitImportConfig(modules, typeImports, valueImports, operatorToFunction);
    }
}
