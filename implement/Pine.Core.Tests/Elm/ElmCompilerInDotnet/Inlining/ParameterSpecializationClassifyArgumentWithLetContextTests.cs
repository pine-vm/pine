using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using System.Collections.Immutable;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Core.Elm.ElmCompilerInDotnet;

using SyntaxTypes = Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using SyntaxModel = Core.Elm.ElmSyntax.SyntaxModel;
using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

/// <summary>
/// Unit tests for the
/// <see cref="ParameterSpecialization.ClassifyArgument(SyntaxTypes.Expression, ImmutableDictionary{string, SyntaxTypes.Expression})"/>
/// overload introduced as PR C of the plan in
/// <c>explore/internal-analysis/2026-05-19-monomorphizing-expressionAfterOpeningSquareBracket-lifted-lambda3.md</c>
/// (Steps 1 + 2 — peel one let-binding hop using <c>letRhsByName</c>).
/// </summary>
public class ParameterSpecializationClassifyArgumentWithLetContextTests
{
    private static readonly SyntaxModel.Range s_zeroRange =
        new(new SyntaxModel.Location(1, 1), new SyntaxModel.Location(1, 1));

    private static SyntaxModel.Node<T> Node<T>(T value) =>
        new(s_zeroRange, value);

    [Fact]
    public void Empty_letRhsByName_falls_through_to_unwrapping_overload()
    {
        ModuleName moduleName = ["App"];

        SyntaxTypes.Expression argument =
            new SyntaxTypes.Expression.FunctionOrValue(moduleName, "double");

        var classified =
            ParameterSpecialization.ClassifyArgument(
                argument,
                []);

        classified.Should().Be(
            new ParameterSpecialization.ConcreteFunctionValue(
                DeclQualifiedName.Create(moduleName, "double")));
    }

    [Fact]
    public void Bare_local_reference_in_letRhsByName_resolves_to_RHS_classification()
    {
        // Mirrors `let (Parser pA) = parseDouble in ...applyHO pA ...` where the
        // walker stored `pA -> parseDouble` and the call site references `pA`.
        ModuleName moduleName = ["App"];

        var rhs =
            (SyntaxTypes.Expression)
            new SyntaxTypes.Expression.FunctionOrValue(moduleName, "parseDouble");

        var letRhsByName =
            ImmutableDictionary<string, SyntaxTypes.Expression>.Empty
            .Add("pA", rhs);

        SyntaxTypes.Expression argument =
            new SyntaxTypes.Expression.FunctionOrValue([], "pA");

        var classified =
            ParameterSpecialization.ClassifyArgument(argument, letRhsByName);

        classified.Should().Be(
            new ParameterSpecialization.ConcreteFunctionValue(
                DeclQualifiedName.Create(moduleName, "parseDouble")));
    }

    [Fact]
    public void Bare_local_reference_not_in_letRhsByName_returns_null()
    {
        SyntaxTypes.Expression argument =
            new SyntaxTypes.Expression.FunctionOrValue([], "pZ");

        var classified =
            ParameterSpecialization.ClassifyArgument(
                argument,
                []);

        // No qualifying module, not a known-stable unqualified reference,
        // and no letRhsByName entry -> not classified.
        classified.Should().BeNull();
    }

    [Fact]
    public void Qualified_reference_classifies_normally_even_when_letRhsByName_has_unrelated_entries()
    {
        ModuleName moduleName = ["App"];

        var letRhsByName =
            ImmutableDictionary<string, SyntaxTypes.Expression>.Empty
            .Add(
                "pA",

                new SyntaxTypes.Expression.FunctionOrValue(moduleName, "parseDouble"));

        SyntaxTypes.Expression argument =
            new SyntaxTypes.Expression.FunctionOrValue(moduleName, "triple");

        var classified =
            ParameterSpecialization.ClassifyArgument(argument, letRhsByName);

        classified.Should().Be(
            new ParameterSpecialization.ConcreteFunctionValue(
                DeclQualifiedName.Create(moduleName, "triple")));
    }

    [Fact]
    public void Parenthesized_bare_local_reference_is_peeled_then_resolved()
    {
        ModuleName moduleName = ["App"];

        var letRhsByName =
            ImmutableDictionary<string, SyntaxTypes.Expression>.Empty
            .Add(
                "pA",

                new SyntaxTypes.Expression.FunctionOrValue(moduleName, "parseDouble"));

        SyntaxTypes.Expression argument =
            new SyntaxTypes.Expression.ParenthesizedExpression(
                Node<SyntaxTypes.Expression>(
                    new SyntaxTypes.Expression.FunctionOrValue([], "pA")));

        var classified =
            ParameterSpecialization.ClassifyArgument(argument, letRhsByName);

        classified.Should().Be(
            new ParameterSpecialization.ConcreteFunctionValue(
                DeclQualifiedName.Create(moduleName, "parseDouble")));
    }

    [Fact]
    public void Lambda_RHS_in_letRhsByName_resolves_to_ConcreteLambdaValue()
    {
        var lambdaStruct =
            new SyntaxTypes.LambdaStruct(
                Arguments:
                [
                Node<SyntaxTypes.Pattern>(new SyntaxTypes.Pattern.VarPattern("x")),
                ],
                Expression:
                Node<SyntaxTypes.Expression>(
                    new SyntaxTypes.Expression.FunctionOrValue([], "x")));

        var letRhsByName =
            ImmutableDictionary<string, SyntaxTypes.Expression>.Empty
            .Add(
                "f",

                new SyntaxTypes.Expression.LambdaExpression(lambdaStruct));

        SyntaxTypes.Expression argument =
            new SyntaxTypes.Expression.FunctionOrValue([], "f");

        var classified =
            ParameterSpecialization.ClassifyArgument(argument, letRhsByName);

        classified.Should().Be(new ParameterSpecialization.ConcreteLambdaValue(lambdaStruct));
    }
}
