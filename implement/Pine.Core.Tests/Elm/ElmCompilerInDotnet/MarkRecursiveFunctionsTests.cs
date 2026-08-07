using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Unit tests for <see cref="ElmSyntaxOptimization.MarkRecursiveFunctions"/>.
/// </summary>
public class MarkRecursiveFunctionsTests
{
    private static readonly IReadOnlyList<string> s_module = ["M"];

    private static SyntaxTypes.Expression Identifier(
        IReadOnlyList<string> moduleName,
        string name) =>
        SyntaxTypes.Expression.Identifier.Create(moduleName, name);

    private static SyntaxTypes.Expression.Application App(
        SyntaxTypes.Expression function,
        params SyntaxTypes.Expression[] arguments) =>
        new(function, arguments);

    private static SyntaxTypes.FunctionStruct Func(
        string name,
        IReadOnlyList<string> argumentNames,
        SyntaxTypes.Expression body) =>
        new(
            Signature: null,
            Declaration:
            new(
                Name: name,
                Arguments:
                [
                ..argumentNames.Select(
                    argumentName =>
                    new SyntaxTypes.Pattern.VarPattern(argumentName))
                ],
                Expression: body));

    private static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.FunctionStruct> Functions(
        params (string Name, SyntaxTypes.FunctionStruct Function)[] items)
    {
        var builder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.FunctionStruct>();

        foreach (var (name, function) in items)
            builder[DeclQualifiedName.Create(s_module, name)] = function;

        return builder.ToImmutable();
    }

    private static bool IsMarkedRecursive(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.FunctionStruct> functions,
        string name)
    {
        var marked = ElmSyntaxOptimization.MarkRecursiveFunctions(functions);

        return marked.Contains(DeclQualifiedName.Create(s_module, name));
    }

    [Fact]
    public void Plain_non_recursive_function_is_not_marked_recursive()
    {
        var f = Func("f", ["x"], Identifier([], "x"));
        var functions = Functions(("f", f));

        IsMarkedRecursive(functions, "f").Should().BeFalse();
    }

    [Fact]
    public void Direct_self_call_is_marked_recursive()
    {
        var f = Func("f", ["x"], App(Identifier(s_module, "f"), Identifier([], "x")));
        var functions = Functions(("f", f));

        IsMarkedRecursive(functions, "f").Should().BeTrue();
    }

    [Fact]
    public void Indirect_two_step_cycle_marks_both_functions_recursive()
    {
        var a = Func("a", ["x"], App(Identifier(s_module, "b"), Identifier([], "x")));
        var b = Func("b", ["x"], App(Identifier(s_module, "a"), Identifier([], "x")));
        var functions = Functions(("a", a), ("b", b));

        IsMarkedRecursive(functions, "a").Should().BeTrue();
        IsMarkedRecursive(functions, "b").Should().BeTrue();
    }

    [Fact]
    public void Indirect_three_step_cycle_marks_all_three_recursive()
    {
        var a = Func("a", ["x"], App(Identifier(s_module, "b"), Identifier([], "x")));
        var b = Func("b", ["x"], App(Identifier(s_module, "c"), Identifier([], "x")));
        var c = Func("c", ["x"], App(Identifier(s_module, "a"), Identifier([], "x")));
        var functions = Functions(("a", a), ("b", b), ("c", c));

        IsMarkedRecursive(functions, "a").Should().BeTrue();
        IsMarkedRecursive(functions, "b").Should().BeTrue();
        IsMarkedRecursive(functions, "c").Should().BeTrue();
    }

    [Fact]
    public void Caller_of_recursive_function_is_not_itself_marked_recursive()
    {
        var recursiveFunction =
            Func(
                "recursive",
                ["x"],
                App(Identifier(s_module, "recursive"), Identifier([], "x")));

        var caller =
            Func(
                "caller",
                ["x"],
                App(Identifier(s_module, "recursive"), Identifier([], "x")));

        var functions =
            Functions(
                ("recursive", recursiveFunction),
                ("caller", caller));

        IsMarkedRecursive(functions, "recursive").Should().BeTrue();
        IsMarkedRecursive(functions, "caller").Should().BeFalse();
    }

    [Fact]
    public void Long_acyclic_chain_marks_no_function_recursive()
    {
        var a = Func("a", ["x"], App(Identifier(s_module, "b"), Identifier([], "x")));
        var b = Func("b", ["x"], App(Identifier(s_module, "c"), Identifier([], "x")));
        var c = Func("c", ["x"], App(Identifier(s_module, "d"), Identifier([], "x")));
        var d = Func("d", ["x"], Identifier([], "x"));
        var functions = Functions(("a", a), ("b", b), ("c", c), ("d", d));

        IsMarkedRecursive(functions, "a").Should().BeFalse();
        IsMarkedRecursive(functions, "b").Should().BeFalse();
        IsMarkedRecursive(functions, "c").Should().BeFalse();
        IsMarkedRecursive(functions, "d").Should().BeFalse();
    }

    [Fact]
    public void Disjoint_cycle_and_independent_function_classified_independently()
    {
        var a = Func("a", ["x"], App(Identifier(s_module, "b"), Identifier([], "x")));
        var b = Func("b", ["x"], App(Identifier(s_module, "a"), Identifier([], "x")));
        var c = Func("c", ["x"], Identifier([], "x"));
        var functions = Functions(("a", a), ("b", b), ("c", c));

        IsMarkedRecursive(functions, "a").Should().BeTrue();
        IsMarkedRecursive(functions, "b").Should().BeTrue();
        IsMarkedRecursive(functions, "c").Should().BeFalse();
    }
}
