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
/// Unit tests for the <see cref="ParameterSpecialization.TupleUnwrap"/>
/// variant introduced as PR A of the plan in
/// <c>explore/internal-analysis/2026-05-19-monomorphizing-expressionAfterOpeningSquareBracket-lifted-lambda3.md</c>.
/// <para>
/// Scope: pure data-type addition. These tests pin equality, hashing,
/// the deterministic sort key, the argument matcher (including the
/// one-layer let-newtype-wrap peel inside each tuple element), and the
/// boundary cases (empty, arity mismatch, nesting). No pipeline code
/// is exercised.
/// </para>
/// </summary>
public class ParameterSpecializationTupleUnwrapTests
{
    private static readonly SyntaxModel.Range s_zeroRange =
        new(new SyntaxModel.Location(1, 1), new SyntaxModel.Location(1, 1));

    private static SyntaxModel.Node<T> Node<T>(T value) =>
        new(s_zeroRange, value);

    [Fact]
    public void TupleUnwrap_value_equality_with_identical_concrete_function_value_elements()
    {
        ModuleName moduleNameA = ["App"];
        ModuleName moduleNameB = ["App"];

        ParameterSpecialization left =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleNameA, "double")),
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleNameA, "triple")),
                ]);

        ParameterSpecialization right =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleNameB, "double")),
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleNameB, "triple")),
                ]);

        left.Equals(right).Should().BeTrue();
        left.GetHashCode().Should().Be(right.GetHashCode());
    }

    [Fact]
    public void TupleUnwrap_value_inequality_with_mismatched_element_specialization()
    {
        ModuleName moduleName = ["App"];

        ParameterSpecialization left =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "double")),
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "triple")),
                ]);

        ParameterSpecialization right =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "double")),
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "quadruple")),
                ]);

        left.Equals(right).Should().BeFalse();
    }

    [Fact]
    public void TupleUnwrap_value_inequality_with_different_arity()
    {
        ModuleName moduleName = ["App"];

        ParameterSpecialization left =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "double")),
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "triple")),
                ]);

        ParameterSpecialization right =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "double")),
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "triple")),
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "identity")),
                ]);

        left.Equals(right).Should().BeFalse();
    }

    [Fact]
    public void TupleUnwrap_with_empty_element_array_equals_itself()
    {
        ParameterSpecialization left =
            new ParameterSpecialization.TupleUnwrap([]);

        ParameterSpecialization right =
            new ParameterSpecialization.TupleUnwrap([]);

        left.Equals(right).Should().BeTrue();
        left.GetHashCode().Should().Be(right.GetHashCode());
    }

    [Fact]
    public void TupleUnwrap_supports_nested_tuple_value_equality()
    {
        ModuleName moduleNameA = ["App"];
        ModuleName moduleNameB = ["App"];

        ParameterSpecialization left =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleNameA, "double")),
                new ParameterSpecialization.TupleUnwrap(
                    [
                    new ParameterSpecialization.ConcreteFunctionValue(
                        DeclQualifiedName.Create(moduleNameA, "triple")),
                    new ParameterSpecialization.ConcreteFunctionValue(
                        DeclQualifiedName.Create(moduleNameA, "identity")),
                    ]),
                ]);

        ParameterSpecialization right =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleNameB, "double")),
                new ParameterSpecialization.TupleUnwrap(
                    [
                    new ParameterSpecialization.ConcreteFunctionValue(
                        DeclQualifiedName.Create(moduleNameB, "triple")),
                    new ParameterSpecialization.ConcreteFunctionValue(
                        DeclQualifiedName.Create(moduleNameB, "identity")),
                    ]),
                ]);

        left.Equals(right).Should().BeTrue();
        left.GetHashCode().Should().Be(right.GetHashCode());
    }

    [Fact]
    public void ArgumentMatchesSpecialization_returns_true_for_TupledExpression_of_matching_concrete_function_value_references()
    {
        ModuleName moduleName = ["App"];

        var spec =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "double")),
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "triple")),
                ]);

        SyntaxTypes.Expression argument =
            new SyntaxTypes.Expression.TupledExpression(
                [
                Node<SyntaxTypes.Expression>(new SyntaxTypes.Expression.FunctionOrValue(moduleName, "double")),
                Node<SyntaxTypes.Expression>(new SyntaxTypes.Expression.FunctionOrValue(moduleName, "triple")),
                ]);

        ParameterSpecialization.ArgumentMatchesSpecialization(argument, spec).Should().BeTrue();
    }

    [Fact]
    public void ArgumentMatchesSpecialization_returns_false_for_TupledExpression_with_wrong_arity()
    {
        ModuleName moduleName = ["App"];

        var spec =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "double")),
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "triple")),
                ]);

        SyntaxTypes.Expression argument =
            new SyntaxTypes.Expression.TupledExpression(
                [
                Node<SyntaxTypes.Expression>(new SyntaxTypes.Expression.FunctionOrValue(moduleName, "double")),
                ]);

        ParameterSpecialization.ArgumentMatchesSpecialization(argument, spec).Should().BeFalse();
    }

    [Fact]
    public void ArgumentMatchesSpecialization_returns_false_for_non_tupled_expression()
    {
        ModuleName moduleName = ["App"];

        var spec =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "double")),
                ]);

        SyntaxTypes.Expression argument =
            new SyntaxTypes.Expression.FunctionOrValue(moduleName, "double");

        ParameterSpecialization.ArgumentMatchesSpecialization(argument, spec).Should().BeFalse();
    }

    [Fact]
    public void ArgumentMatchesSpecialization_peels_parens_around_TupledExpression()
    {
        ModuleName moduleName = ["App"];

        var spec =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "double")),
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "triple")),
                ]);

        var inner =
            new SyntaxTypes.Expression.TupledExpression(
                [
                Node<SyntaxTypes.Expression>(new SyntaxTypes.Expression.FunctionOrValue(moduleName, "double")),
                Node<SyntaxTypes.Expression>(new SyntaxTypes.Expression.FunctionOrValue(moduleName, "triple")),
                ]);

        SyntaxTypes.Expression argument =
            new SyntaxTypes.Expression.ParenthesizedExpression(
                Node<SyntaxTypes.Expression>(inner));

        ParameterSpecialization.ArgumentMatchesSpecialization(argument, spec).Should().BeTrue();
    }

    [Fact]
    public void ArgumentMatchesSpecialization_peels_one_layer_of_let_newtype_wrap_inside_tuple_element()
    {
        // Mirrors the §16 production shape:
        //   let (Parser pA) = parseDouble in pA
        // wrapped inside a tuple element.
        ModuleName moduleName = ["App"];
        ModuleName parserModule = ["App"];

        var spec =
            new ParameterSpecialization.TupleUnwrap(
                [
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "parseDouble")),
                new ParameterSpecialization.ConcreteFunctionValue(
                    DeclQualifiedName.Create(moduleName, "parseTriple")),
                ]);

        SyntaxTypes.Expression BuildWrappedRef(string declName, string bindName)
        {
            var pattern =
                new SyntaxTypes.Pattern.NamedPattern(
                    new SyntaxTypes.QualifiedNameRef(parserModule, "Parser"),
                    [Node<SyntaxTypes.Pattern>(new SyntaxTypes.Pattern.VarPattern(bindName))]);

            var rhs =
                Node<SyntaxTypes.Expression>(new SyntaxTypes.Expression.FunctionOrValue(moduleName, declName));

            var letDecl =
                Node<SyntaxTypes.Expression.LetDeclaration>(
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Node<SyntaxTypes.Pattern>(pattern),
                        rhs));

            var letBody =
                Node<SyntaxTypes.Expression>(new SyntaxTypes.Expression.FunctionOrValue([], bindName));

            return
                new SyntaxTypes.Expression.LetExpression(
                    new SyntaxTypes.Expression.LetBlock([letDecl], letBody));
        }

        SyntaxTypes.Expression argument =
            new SyntaxTypes.Expression.TupledExpression(
                [
                Node(BuildWrappedRef("parseDouble", "pA")),
                Node(BuildWrappedRef("parseTriple", "pB")),
                ]);

        ParameterSpecialization.ArgumentMatchesSpecialization(argument, spec).Should().BeTrue();
    }

    [Fact]
    public void FunctionSpecialization_DeterministicSortKey_renders_TupleUnwrap_with_nested_recursion()
    {
        ModuleName moduleName = ["App"];

        var nestedSpec =
            new FunctionSpecialization(
                ImmutableDictionary<int, ParameterSpecialization>.Empty.Add(
                    0,
                    new ParameterSpecialization.TupleUnwrap(
                        [
                        new ParameterSpecialization.ConcreteFunctionValue(
                            DeclQualifiedName.Create(moduleName, "double")),
                        new ParameterSpecialization.TupleUnwrap(
                            [
                            new ParameterSpecialization.ConcreteFunctionValue(
                                DeclQualifiedName.Create(moduleName, "triple")),
                            new ParameterSpecialization.ConcreteFunctionValue(
                                DeclQualifiedName.Create(moduleName, "identity")),
                            ]),
                        ])));

        // Deterministic + structurally informative: the key encodes the
        // tuple element list verbatim and recurses into nested tuples,
        // which is required for stable __specialized__N counter naming
        // (per the FunctionSpecialization.ParameterSpecializationSortKey
        // memory).
        nestedSpec.DeterministicSortKey.Should().Be("0:U:[F:App.double,U:[F:App.triple,F:App.identity]]#I");
    }
}
