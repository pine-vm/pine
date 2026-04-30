using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter;
using System.Collections.Generic;
using Xunit;

using PineCodeAnalysis = Pine.Core.CodeAnalysis.CodeAnalysis;

namespace Pine.Core.Tests.CodeAnalysis;

/// <summary>
/// Focused tests for the optimization that consolidates the generic form of partial
/// application emitted by frontend compilers
/// (see <see cref="ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(Expression.ParseAndEval, PineVMParseCache)"/>).
/// The chain is constructed using the same helper used by the Elm compiler:
/// <see cref="PineCodeAnalysis.BuildGenericFunctionApplication(Expression, IReadOnlyList{Expression})"/>.
/// </summary>
public class ReducePineExpressionGenericApplicationTests
{
    private static readonly PineVMParseCache s_parseCache = new();

    /// <summary>
    /// Builds a function value (in the "wrapper" form emitted by FunctionValueBuilder)
    /// for an N-parameter function whose body simply returns the environment so the test
    /// can observe each captured argument by inspecting the produced list.
    /// </summary>
    private static PineValue BuildIdentityEnvironmentFunctionValue(int parameterCount)
    {
        // The body of the function receives env = [envFunctions, arg0, arg1, ..., arg_{N-1}].
        // Returning the Environment lets us observe each captured argument in the result.
        var body = Expression.EnvironmentInstance;

        return
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression: body,
                parameterCount: parameterCount,
                envFunctions: []);
    }

    /// <summary>
    /// Counts the number of <see cref="Expression.ParseAndEval"/> nodes in an expression tree.
    /// </summary>
    private static int CountParseAndEval(Expression expression)
    {
        var count = 0;

        foreach (var sub in Expression.EnumerateSelfAndDescendants(expression))
        {
            if (sub is Expression.ParseAndEval)
            {
                ++count;
            }
        }

        return count;
    }

    private static PineValue Evaluate(Expression expression)
    {
        var interpreter = new DirectInterpreter(s_parseCache, evalCache: null);

        return interpreter.EvaluateExpressionDefault(expression, PineValue.EmptyList);
    }

    [Fact]
    public void Returns_null_for_chain_with_one_argument_only()
    {
        // Build a chain of just one ParseAndEval; no consolidation should occur.
        var functionValue = BuildIdentityEnvironmentFunctionValue(parameterCount: 3);

        var chain =
            PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(functionValue),
                [Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13))]);

        chain.Should().BeOfType<Expression.ParseAndEval>();

        ReducePineExpression
            .TryConsolidateGenericFunctionApplicationChain((Expression.ParseAndEval)chain, s_parseCache)
            .Should().BeNull();
    }

    [Fact]
    public void Returns_null_when_inner_function_expression_is_not_a_literal()
    {
        // If the function expression is not a literal (for example, it is the Environment),
        // the optimization must not fire even when there are several arguments.
        var nonLiteralFunc = Expression.EnvironmentInstance;

        var chain =
            PineCodeAnalysis.BuildGenericFunctionApplication(
                nonLiteralFunc,
                [
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13)),
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17)),
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(19))
                ]);

        ReducePineExpression
            .TryConsolidateGenericFunctionApplicationChain((Expression.ParseAndEval)chain, s_parseCache)
            .Should().BeNull();
    }

    [Fact]
    public void Consolidates_two_arg_chain_into_a_single_parse_and_eval_for_3_param_function()
    {
        // Three-parameter function; partially apply two arguments via the generic form.
        var functionValue = BuildIdentityEnvironmentFunctionValue(parameterCount: 3);

        var arg0 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11));
        var arg1 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13));

        var chain =
            (Expression.ParseAndEval)PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(functionValue),
                [arg0, arg1]);

        // The chain has two nested ParseAndEval expressions.
        CountParseAndEval(chain).Should().Be(2);

        var consolidated =
            ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(chain, s_parseCache);

        consolidated.Should().NotBeNull();

        // The consolidated form must contain at most one ParseAndEval (the runtime-needed one).
        CountParseAndEval(consolidated!).Should().BeLessThanOrEqualTo(1);

        // Both forms must produce the same result when applying the third argument and
        // evaluating end-to-end.
        var arg2 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17));

        var beforeFull = (Expression)new Expression.ParseAndEval(chain, arg2);
        var afterFull = (Expression)new Expression.ParseAndEval(consolidated!, arg2);

        Evaluate(afterFull).Should().Be(Evaluate(beforeFull));
    }

    [Fact]
    public void Consolidates_three_arg_chain_into_a_single_parse_and_eval_for_4_param_function()
    {
        // Four-parameter function; partially apply three arguments via the generic form.
        var functionValue = BuildIdentityEnvironmentFunctionValue(parameterCount: 4);

        var arg0 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11));
        var arg1 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13));
        var arg2 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17));

        var chain =
            (Expression.ParseAndEval)PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(functionValue),
                [arg0, arg1, arg2]);

        CountParseAndEval(chain).Should().Be(3);

        var consolidated =
            ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(chain, s_parseCache);

        consolidated.Should().NotBeNull();

        CountParseAndEval(consolidated!).Should().BeLessThanOrEqualTo(1);

        // End-to-end equivalence: apply the final fourth argument and evaluate both forms.
        var arg3 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(19));

        var beforeFull = (Expression)new Expression.ParseAndEval(chain, arg3);
        var afterFull = (Expression)new Expression.ParseAndEval(consolidated!, arg3);

        Evaluate(afterFull).Should().Be(Evaluate(beforeFull));
    }

    [Fact]
    public void Bottom_up_reduction_consolidates_chain_for_two_argument_partial_application()
    {
        // The optimization must also be triggered from the standard bottom-up reducer.
        var functionValue = BuildIdentityEnvironmentFunctionValue(parameterCount: 3);

        var arg0 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11));
        var arg1 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13));

        var chain =
            PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(functionValue),
                [arg0, arg1]);

        var reduced =
            ReducePineExpression.ReduceExpressionBottomUp(
                chain,
                s_parseCache,
                reducedExpressionCache: null);

        // The reduced expression must contain at most one ParseAndEval node.
        CountParseAndEval(reduced).Should().BeLessThanOrEqualTo(1);
    }

    [Fact]
    public void Saturated_application_via_generic_form_is_consolidated_to_a_single_parse_and_eval()
    {
        // Apply all three arguments via the generic form to a 3-parameter function, then
        // confirm the resulting expression contains only one ParseAndEval.
        var functionValue = BuildIdentityEnvironmentFunctionValue(parameterCount: 3);

        var arg0 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11));
        var arg1 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(13));
        var arg2 = Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(17));

        var chain =
            (Expression.ParseAndEval)PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(functionValue),
                [arg0, arg1, arg2]);

        CountParseAndEval(chain).Should().Be(3);

        var consolidated =
            ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(chain, s_parseCache);

        consolidated.Should().NotBeNull();
        CountParseAndEval(consolidated!).Should().BeLessThanOrEqualTo(1);

        // Saturation produces the same final result as evaluation of the original chain.
        Evaluate(consolidated!).Should().Be(Evaluate(chain));
    }
}
