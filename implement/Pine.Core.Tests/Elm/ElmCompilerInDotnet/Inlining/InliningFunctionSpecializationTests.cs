using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using System.Collections.Immutable;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Core.Elm.ElmCompilerInDotnet;

using SyntaxTypes = Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

public class InliningFunctionSpecializationTests
{
    [Fact]
    public void ParameterSpecialization_ConcreteFunctionValue_value_equality()
    {
        ModuleName moduleNameA = ["App"];
        ModuleName moduleNameB = ["App"];

        ParameterSpecialization left =
            new ParameterSpecialization.ConcreteFunctionValue(
                new DeclQualifiedName(moduleNameA, "increment"));

        ParameterSpecialization right =
            new ParameterSpecialization.ConcreteFunctionValue(
                new DeclQualifiedName(moduleNameB, "increment"));

        left.Equals(right).Should().BeTrue();
        left.GetHashCode().Should().Be(right.GetHashCode());
    }

    [Fact]
    public void ParameterSpecialization_ConcreteLambdaValue_value_equality()
    {
        var range =
            new Core.Elm.ElmSyntax.SyntaxModel.Range(
                new Core.Elm.ElmSyntax.SyntaxModel.Location(1, 1),
                new Core.Elm.ElmSyntax.SyntaxModel.Location(1, 10));

        ParameterSpecialization left =
            new ParameterSpecialization.ConcreteLambdaValue(
                new SyntaxTypes.LambdaStruct(
                    [
                    new Core.Elm.ElmSyntax.SyntaxModel.Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x"))
                    ],
                    new Core.Elm.ElmSyntax.SyntaxModel.Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))));

        ParameterSpecialization right =
            new ParameterSpecialization.ConcreteLambdaValue(
                new SyntaxTypes.LambdaStruct(
                    [
                    new Core.Elm.ElmSyntax.SyntaxModel.Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x"))
                    ],
                    new Core.Elm.ElmSyntax.SyntaxModel.Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))));

        left.Equals(right).Should().BeTrue();
        left.GetHashCode().Should().Be(right.GetHashCode());
    }

    [Fact]
    public void ParameterSpecialization_SingleChoiceTagUnwrap_supports_nested_value_equality()
    {
        ModuleName moduleNameA = ["App"];
        ModuleName moduleNameB = ["App"];

        ParameterSpecialization left =
            new ParameterSpecialization.SingleChoiceTagUnwrap(
                new DeclQualifiedName(moduleNameA, "Outer"),
                ImmutableDictionary<int, ParameterSpecialization>.Empty.Add(
                    0,
                    new ParameterSpecialization.SingleChoiceTagUnwrap(
                        new DeclQualifiedName(moduleNameA, "Inner"),
                        [])));

        ParameterSpecialization right =
            new ParameterSpecialization.SingleChoiceTagUnwrap(
                new DeclQualifiedName(moduleNameB, "Outer"),
                ImmutableDictionary<int, ParameterSpecialization>.Empty.Add(
                    0,
                    new ParameterSpecialization.SingleChoiceTagUnwrap(
                        new DeclQualifiedName(moduleNameB, "Inner"),
                        [])));

        left.Equals(right).Should().BeTrue();
        left.GetHashCode().Should().Be(right.GetHashCode());
    }

    [Fact]
    public void FunctionSpecialization_value_equality_uses_parameter_specialization_value_equality()
    {
        ModuleName moduleNameA = ["App"];
        ModuleName moduleNameB = ["App"];

        var left =
            new FunctionSpecialization(
                ImmutableDictionary<int, ParameterSpecialization>.Empty.Add(
                    1,
                    new ParameterSpecialization.SingleChoiceTagUnwrap(
                        new DeclQualifiedName(moduleNameA, "Outer"),
                        ImmutableDictionary<int, ParameterSpecialization>.Empty.Add(
                            0,
                            new ParameterSpecialization.SingleChoiceTagUnwrap(
                                new DeclQualifiedName(moduleNameA, "Inner"),
                                [])))));

        var right =
            new FunctionSpecialization(
                ImmutableDictionary<int, ParameterSpecialization>.Empty.Add(
                    1,
                    new ParameterSpecialization.SingleChoiceTagUnwrap(
                        new DeclQualifiedName(moduleNameB, "Outer"),
                        ImmutableDictionary<int, ParameterSpecialization>.Empty.Add(
                            0,
                            new ParameterSpecialization.SingleChoiceTagUnwrap(
                                new DeclQualifiedName(moduleNameB, "Inner"),
                                [])))));

        left.Equals(right).Should().BeTrue();
        left.GetHashCode().Should().Be(right.GetHashCode());
    }
}
