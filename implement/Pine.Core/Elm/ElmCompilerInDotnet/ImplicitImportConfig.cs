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
    IImmutableDictionary<string, ModuleName> ValueImports)
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
            .Add(["Tuple"])
            .Add(["Char"])
            .Add(["String"])
            .Add(["Debug"]);

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
            .Add("Program", ["Platform"])
            .Add("Task", ["Task"])
            .Add("Cmd", ["Platform", "Cmd"])
            .Add("Sub", ["Platform", "Sub"]);

        var valueImports =
            ImmutableDictionary<string, ModuleName>.Empty
            .Add("compare", ["Basics"])
            .Add("not", ["Basics"])
            .Add("xor", ["Basics"])
            .Add("==", ["Basics"])
            .Add("/=", ["Basics"])
            .Add("<", ["Basics"])
            .Add("<=", ["Basics"])
            .Add(">", ["Basics"])
            .Add(">=", ["Basics"])
            .Add("&&", ["Basics"])
            .Add("||", ["Basics"])
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
            .Add("never", ["Basics"])
            .Add("|>", ["Basics"])
            .Add("<|", ["Basics"])
            .Add("<<", ["Basics"])
            .Add(">>", ["Basics"]);

        return new ImplicitImportConfig(modules, typeImports, valueImports);
    }
}
