using AwesomeAssertions;
using Xunit;

using PineCodeAnalysis = Pine.Core.CodeAnalysis.CodeAnalysis;

namespace Pine.Core.Tests.CodeAnalysis;

/// <summary>
/// Focused tests for <see cref="PineCodeAnalysis.ParseGenericFunctionApplication(Expression)"/>
/// and the matching constructor <see cref="PineCodeAnalysis.BuildGenericFunctionApplication(Expression, System.Collections.Generic.IReadOnlyList{Expression})"/>.
/// </summary>
public class ParseGenericFunctionApplicationTests
{
    [Fact]
    public void Returns_null_for_non_parse_and_eval_expressions()
    {
        // A bare literal: not a generic application chain.
        PineCodeAnalysis.ParseGenericFunctionApplication(
            Expression.LiteralInstance(PineValue.EmptyList))
            .Should().BeNull();

        // Environment node alone: not a generic application chain.
        PineCodeAnalysis.ParseGenericFunctionApplication(
            Expression.EnvironmentInstance)
            .Should().BeNull();

        // Plain list expression: not a ParseAndEval at the root.
        PineCodeAnalysis.ParseGenericFunctionApplication(
            Expression.ListInstance(
                [
                Expression.LiteralInstance(PineValue.EmptyList),
                Expression.EnvironmentInstance
                ]))
            .Should().BeNull();
    }

    [Fact]
    public void Parses_single_argument_chain()
    {
        var func = Expression.LiteralInstance(PineValue.Blob([1, 2, 3]));
        var arg0 = Expression.EnvironmentInstance;

        var built =
            PineCodeAnalysis.BuildGenericFunctionApplication(func, [arg0]);

        // The built chain must be a single ParseAndEval.
        built.Should().BeOfType<Expression.ParseAndEval>();

        var parsed = PineCodeAnalysis.ParseGenericFunctionApplication(built);

        parsed.HasValue.Should().BeTrue();
        parsed!.Value.functionExpr.Should().BeSameAs(func);
        parsed.Value.arguments.Should().Equal([arg0]);
    }

    [Fact]
    public void Parses_multi_argument_chain_in_application_order()
    {
        var func = Expression.LiteralInstance(PineValue.Blob([0xAA]));

        var arg0 = Expression.LiteralInstance(PineValue.Blob([0x10]));
        var arg1 = Expression.LiteralInstance(PineValue.Blob([0x20]));
        var arg2 = Expression.LiteralInstance(PineValue.Blob([0x30]));
        var arg3 = Expression.LiteralInstance(PineValue.Blob([0x40]));

        var built =
            PineCodeAnalysis.BuildGenericFunctionApplication(
                func,
                [arg0, arg1, arg2, arg3]);

        // Outer expression is ParseAndEval whose environment is the LAST argument.
        built.Should().BeOfType<Expression.ParseAndEval>();

        var outer = (Expression.ParseAndEval)built;
        outer.Environment.Should().BeSameAs(arg3);

        var parsed = PineCodeAnalysis.ParseGenericFunctionApplication(built);

        parsed.HasValue.Should().BeTrue();
        parsed!.Value.functionExpr.Should().BeSameAs(func);
        parsed.Value.arguments.Should().Equal([arg0, arg1, arg2, arg3]);
    }

    [Fact]
    public void Build_with_no_arguments_returns_function_expression_unchanged()
    {
        var func = Expression.LiteralInstance(PineValue.Blob([7]));

        var built =
            PineCodeAnalysis.BuildGenericFunctionApplication(func, []);

        built.Should().BeSameAs(func);

        // The result is no longer a ParseAndEval, so parsing should fail.
        PineCodeAnalysis.ParseGenericFunctionApplication(built).Should().BeNull();
    }

    [Fact]
    public void Function_expression_can_be_any_pine_expression_including_paths_into_environment()
    {
        // The inner function value may not be a literal: it can be a path into the environment
        // (for example when the function comes from a parameter).
        var funcExpr =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                Expression.EnvironmentInstance);

        var arg0 = Expression.LiteralInstance(PineValue.Blob([1]));
        var arg1 = Expression.LiteralInstance(PineValue.Blob([2]));

        var built =
            PineCodeAnalysis.BuildGenericFunctionApplication(funcExpr, [arg0, arg1]);

        var parsed = PineCodeAnalysis.ParseGenericFunctionApplication(built);

        parsed.HasValue.Should().BeTrue();
        parsed!.Value.functionExpr.Should().BeSameAs(funcExpr);
        parsed.Value.arguments.Should().Equal([arg0, arg1]);
    }

    [Fact]
    public void Parses_chain_with_arguments_referencing_the_environment()
    {
        var func = Expression.LiteralInstance(PineValue.Blob([0x55]));

        // Arguments may also be expressions referencing the environment.
        var arg0 = Expression.EnvironmentInstance;
        var arg1 =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                Expression.EnvironmentInstance);
        var arg2 = Expression.LiteralInstance(PineValue.Blob([0xCC]));

        var built =
            PineCodeAnalysis.BuildGenericFunctionApplication(
                func,
                [arg0, arg1, arg2]);

        var parsed = PineCodeAnalysis.ParseGenericFunctionApplication(built);

        parsed.HasValue.Should().BeTrue();
        parsed!.Value.functionExpr.Should().BeSameAs(func);
        parsed.Value.arguments.Should().HaveCount(3);
        parsed.Value.arguments[0].Should().BeSameAs(arg0);
        parsed.Value.arguments[1].Should().BeSameAs(arg1);
        parsed.Value.arguments[2].Should().BeSameAs(arg2);
    }

    [Fact]
    public void Parse_round_trips_through_build_for_chains_of_varying_length()
    {
        var func = Expression.LiteralInstance(PineValue.Blob([0x01]));

        for (var argCount = 1; argCount <= 6; ++argCount)
        {
            var arguments = new Expression[argCount];

            for (var i = 0; i < argCount; ++i)
            {
                arguments[i] = Expression.LiteralInstance(PineValue.Blob([(byte)(0x80 + i)]));
            }

            var built = PineCodeAnalysis.BuildGenericFunctionApplication(func, arguments);

            var parsed = PineCodeAnalysis.ParseGenericFunctionApplication(built);

            parsed.HasValue.Should().BeTrue();
            parsed!.Value.functionExpr.Should().BeSameAs(func);
            parsed.Value.arguments.Should().Equal(arguments);
        }
    }
}
