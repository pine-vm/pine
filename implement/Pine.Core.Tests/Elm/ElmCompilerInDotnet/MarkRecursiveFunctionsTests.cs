using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Generic;
using System.Collections.Immutable;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using SyntaxModel = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Unit tests for <see cref="ElmSyntaxOptimization.MarkRecursiveFunctions"/>.
///
/// These exercise the call-graph recursion classification directly on
/// synthetically constructed function declarations, so they execute in
/// milliseconds and serve as a regression net for the SCC-based rewrite
/// of the previous quadratic <c>IsRecursiveFunction</c> implementation.
/// </summary>
public class MarkRecursiveFunctionsTests
{
    private static readonly SyntaxModel.Location s_zeroLoc = new(Row: 0, Column: 0);

    private static readonly SyntaxModel.Range s_zeroRange = new(Start: s_zeroLoc, End: s_zeroLoc);

    private static SyntaxModel.Node<T> N<T>(T value) => new(s_zeroRange, value);

    private static readonly IReadOnlyList<string> s_module = ["M"];

    private static SyntaxTypes.Expression FunctionOrValue(IReadOnlyList<string> moduleName, string name) =>
        new SyntaxTypes.Expression.FunctionOrValue(moduleName, name);

    private static SyntaxTypes.Expression.Application App(params SyntaxTypes.Expression[] parts)
    {
        var nodes = new List<SyntaxModel.Node<SyntaxTypes.Expression>>(parts.Length);

        foreach (var p in parts)
            nodes.Add(N(p));

        return new SyntaxTypes.Expression.Application(nodes);
    }

    private static SyntaxTypes.Declaration.FunctionDeclaration FuncDecl(
        string name,
        IReadOnlyList<string> argNames,
        SyntaxTypes.Expression body)
    {
        var argPatterns = new List<SyntaxModel.Node<SyntaxTypes.Pattern>>(argNames.Count);

        foreach (var argName in argNames)
            argPatterns.Add(N<SyntaxTypes.Pattern>(new SyntaxTypes.Pattern.VarPattern(argName)));

        var impl =
            new SyntaxTypes.FunctionImplementation(
                Name: N(name),
                Arguments: argPatterns,
                Expression: N(body));

        var func =
            new SyntaxTypes.FunctionStruct(
                Documentation: null,
                Signature: null,
                Declaration: N(impl));

        return new SyntaxTypes.Declaration.FunctionDeclaration(func);
    }

    private static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> Decls(
        params (string name, SyntaxTypes.Declaration decl)[] items)
    {
        var b = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (n, d) in items)
            b[DeclQualifiedName.Create(s_module, n)] = d;

        return b.ToImmutable();
    }

    private static bool IsMarkedRecursive(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> decls,
        string name)
    {
        var marked =
            ElmSyntaxOptimization.MarkRecursiveFunctions(
                ElmSyntaxOptimization.BuildFunctionDictionary(decls));

        return marked[DeclQualifiedName.Create(s_module, name)].IsRecursive;
    }

    [Fact]
    public void Plain_non_recursive_function_is_not_marked_recursive()
    {
        // f x = x
        var f = FuncDecl("f", ["x"], FunctionOrValue([], "x"));
        var decls = Decls(("f", f));

        IsMarkedRecursive(decls, "f").Should().BeFalse();
    }

    [Fact]
    public void Direct_self_call_is_marked_recursive()
    {
        // f x = f x
        var f = FuncDecl("f", ["x"], App(FunctionOrValue(s_module, "f"), FunctionOrValue([], "x")));
        var decls = Decls(("f", f));

        IsMarkedRecursive(decls, "f").Should().BeTrue();
    }

    [Fact]
    public void Indirect_two_step_cycle_marks_both_functions_recursive()
    {
        // a x = b x
        // b x = a x
        var a = FuncDecl("a", ["x"], App(FunctionOrValue(s_module, "b"), FunctionOrValue([], "x")));
        var b = FuncDecl("b", ["x"], App(FunctionOrValue(s_module, "a"), FunctionOrValue([], "x")));
        var decls = Decls(("a", a), ("b", b));

        IsMarkedRecursive(decls, "a").Should().BeTrue();
        IsMarkedRecursive(decls, "b").Should().BeTrue();
    }

    [Fact]
    public void Indirect_three_step_cycle_marks_all_three_recursive()
    {
        // a x = b x ; b x = c x ; c x = a x
        var a = FuncDecl("a", ["x"], App(FunctionOrValue(s_module, "b"), FunctionOrValue([], "x")));
        var b = FuncDecl("b", ["x"], App(FunctionOrValue(s_module, "c"), FunctionOrValue([], "x")));
        var c = FuncDecl("c", ["x"], App(FunctionOrValue(s_module, "a"), FunctionOrValue([], "x")));
        var decls = Decls(("a", a), ("b", b), ("c", c));

        IsMarkedRecursive(decls, "a").Should().BeTrue();
        IsMarkedRecursive(decls, "b").Should().BeTrue();
        IsMarkedRecursive(decls, "c").Should().BeTrue();
    }

    [Fact]
    public void Caller_of_recursive_function_is_not_itself_marked_recursive()
    {
        // r x = r x      -- recursive
        // c x = r x      -- only calls into the cycle, not in it
        var r = FuncDecl("r", ["x"], App(FunctionOrValue(s_module, "r"), FunctionOrValue([], "x")));
        var c = FuncDecl("c", ["x"], App(FunctionOrValue(s_module, "r"), FunctionOrValue([], "x")));
        var decls = Decls(("r", r), ("c", c));

        IsMarkedRecursive(decls, "r").Should().BeTrue();
        IsMarkedRecursive(decls, "c").Should().BeFalse();
    }

    [Fact]
    public void Long_acyclic_chain_marks_no_function_recursive()
    {
        // a x = b x ; b x = c x ; c x = d x ; d x = x
        var a = FuncDecl("a", ["x"], App(FunctionOrValue(s_module, "b"), FunctionOrValue([], "x")));
        var b = FuncDecl("b", ["x"], App(FunctionOrValue(s_module, "c"), FunctionOrValue([], "x")));
        var c = FuncDecl("c", ["x"], App(FunctionOrValue(s_module, "d"), FunctionOrValue([], "x")));
        var d = FuncDecl("d", ["x"], FunctionOrValue([], "x"));
        var decls = Decls(("a", a), ("b", b), ("c", c), ("d", d));

        IsMarkedRecursive(decls, "a").Should().BeFalse();
        IsMarkedRecursive(decls, "b").Should().BeFalse();
        IsMarkedRecursive(decls, "c").Should().BeFalse();
        IsMarkedRecursive(decls, "d").Should().BeFalse();
    }

    [Fact]
    public void Disjoint_cycle_and_independent_function_classified_independently()
    {
        // a x = b x ; b x = a x ; c x = x
        var a = FuncDecl("a", ["x"], App(FunctionOrValue(s_module, "b"), FunctionOrValue([], "x")));
        var b = FuncDecl("b", ["x"], App(FunctionOrValue(s_module, "a"), FunctionOrValue([], "x")));
        var c = FuncDecl("c", ["x"], FunctionOrValue([], "x"));
        var decls = Decls(("a", a), ("b", b), ("c", c));

        IsMarkedRecursive(decls, "a").Should().BeTrue();
        IsMarkedRecursive(decls, "b").Should().BeTrue();
        IsMarkedRecursive(decls, "c").Should().BeFalse();
    }
}
