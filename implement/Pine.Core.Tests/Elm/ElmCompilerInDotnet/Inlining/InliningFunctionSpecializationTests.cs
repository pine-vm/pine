using AwesomeAssertions;
using System.Collections.Immutable;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using InliningFunctionSpecialization = Core.Elm.ElmCompilerInDotnet.InliningFunctionSpecialization;
using SyntaxTypes = Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

public class InliningFunctionSpecializationTests
{
    [Fact]
    public void ParameterSpecialization_ConcreteFunctionValue_value_equality()
    {
        ModuleName moduleNameA = ["App"];
        ModuleName moduleNameB = ["App"];

        InliningFunctionSpecialization.ParameterSpecialization left =
            new InliningFunctionSpecialization.ParameterSpecialization.ConcreteFunctionValue(
                moduleNameA,
                "increment");

        InliningFunctionSpecialization.ParameterSpecialization right =
            new InliningFunctionSpecialization.ParameterSpecialization.ConcreteFunctionValue(
                moduleNameB,
                "increment");

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

        InliningFunctionSpecialization.ParameterSpecialization left =
            new InliningFunctionSpecialization.ParameterSpecialization.ConcreteLambdaValue(
                new SyntaxTypes.LambdaStruct(
                    [
                    new Core.Elm.ElmSyntax.SyntaxModel.Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x"))
                    ],
                    new Core.Elm.ElmSyntax.SyntaxModel.Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))));

        InliningFunctionSpecialization.ParameterSpecialization right =
            new InliningFunctionSpecialization.ParameterSpecialization.ConcreteLambdaValue(
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

        InliningFunctionSpecialization.ParameterSpecialization left =
            new InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap(
                new SyntaxTypes.QualifiedNameRef(moduleNameA, "Outer"),
                ImmutableDictionary<int, InliningFunctionSpecialization.ParameterSpecialization>.Empty.Add(
                    0,
                    new InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap(
                        new SyntaxTypes.QualifiedNameRef(moduleNameA, "Inner"),
                        ImmutableDictionary<int, InliningFunctionSpecialization.ParameterSpecialization>.Empty)));

        InliningFunctionSpecialization.ParameterSpecialization right =
            new InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap(
                new SyntaxTypes.QualifiedNameRef(moduleNameB, "Outer"),
                ImmutableDictionary<int, InliningFunctionSpecialization.ParameterSpecialization>.Empty.Add(
                    0,
                    new InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap(
                        new SyntaxTypes.QualifiedNameRef(moduleNameB, "Inner"),
                        ImmutableDictionary<int, InliningFunctionSpecialization.ParameterSpecialization>.Empty)));

        left.Equals(right).Should().BeTrue();
        left.GetHashCode().Should().Be(right.GetHashCode());
    }

    [Fact]
    public void FunctionSpecialization_value_equality_uses_parameter_specialization_value_equality()
    {
        ModuleName moduleNameA = ["App"];
        ModuleName moduleNameB = ["App"];

        var left =
            new InliningFunctionSpecialization.FunctionSpecialization(
                "alfa",
                moduleNameA,
                ImmutableDictionary<int, InliningFunctionSpecialization.ParameterSpecialization>.Empty.Add(
                    1,
                    new InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap(
                        new SyntaxTypes.QualifiedNameRef(moduleNameA, "Outer"),
                        ImmutableDictionary<int, InliningFunctionSpecialization.ParameterSpecialization>.Empty.Add(
                            0,
                            new InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap(
                                new SyntaxTypes.QualifiedNameRef(moduleNameA, "Inner"),
                                ImmutableDictionary<int, InliningFunctionSpecialization.ParameterSpecialization>.Empty)))));

        var right =
            new InliningFunctionSpecialization.FunctionSpecialization(
                "alfa",
                moduleNameB,
                ImmutableDictionary<int, InliningFunctionSpecialization.ParameterSpecialization>.Empty.Add(
                    1,
                    new InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap(
                        new SyntaxTypes.QualifiedNameRef(moduleNameB, "Outer"),
                        ImmutableDictionary<int, InliningFunctionSpecialization.ParameterSpecialization>.Empty.Add(
                            0,
                            new InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap(
                                new SyntaxTypes.QualifiedNameRef(moduleNameB, "Inner"),
                                ImmutableDictionary<int, InliningFunctionSpecialization.ParameterSpecialization>.Empty)))));

        left.Equals(right).Should().BeTrue();
        left.GetHashCode().Should().Be(right.GetHashCode());
    }
}
