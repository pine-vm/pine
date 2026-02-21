using System.Collections.Immutable;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

using ModuleName = ImmutableArray<string>;

/// <summary>
/// Represents the configuration for implicit module imports and the specific members they expose within the compilation
/// context.
/// </summary>
public record ImplicitImportConfig(
    ImmutableHashSet<ModuleName> ModuleImports,
    IImmutableDictionary<string, ModuleName> TypeImports,
    IImmutableDictionary<string, ModuleName> ValueImports,
    IImmutableDictionary<string, (ModuleName ModuleName, string FunctionName)> OperatorToFunction,
    IImmutableDictionary<string, ModuleName> ModuleAliases)
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
            ImmutableHashSet<ModuleName>.Empty
            .WithComparer(EnumerableExtensions.EqualityComparer<ModuleName>())
            .Add(["Basics"])
            .Add(["List"])
            .Add(["Maybe"])
            .Add(["Result"])
            .Add(["Tuple"])
            .Add(["Char"])
            .Add(["String"])
            .Add(["Debug"])
            .Add(["Platform"])
            .Add(["Platform", "Cmd"])
            .Add(["Platform", "Sub"]);

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

        // Module name aliases for implicit imports
        // Based on Elm 0.19 spec: Platform.Cmd is aliased as Cmd, Platform.Sub is aliased as Sub
        var moduleAliases =
            ImmutableDictionary<string, ModuleName>.Empty
            .Add("Cmd", ["Platform", "Cmd"])
            .Add("Sub", ["Platform", "Sub"]);

        return new ImplicitImportConfig(modules, typeImports, valueImports, operatorToFunction, moduleAliases);
    }
}
