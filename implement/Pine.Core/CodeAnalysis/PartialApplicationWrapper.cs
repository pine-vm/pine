using Pine.Core.CommonEncodings;
using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Creates nested wrappers for partial application of Elm functions.
/// 
/// This approach emits actual nested expressions where each wrapper:
/// - Takes one argument at a time from the environment
/// - Either produces the final result (when all arguments are collected)
/// - Or produces another expression (wrapper) that can accept the next argument
/// 
/// The wrapper structure uses a recursive pattern where each level:
/// - Receives the next argument as its environment
/// - Produces an encoded expression for the next level (with the argument captured as a literal)
/// - The innermost level invokes the actual function with all collected arguments
/// </summary>
public static class PartialApplicationWrapper
{
    private static readonly Expression s_parseAndEvalTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("ParseAndEval"));

    private static readonly Expression s_literalTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("Literal"));

    private static readonly Expression s_listTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("List"));

    private static readonly Expression s_environmentTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("Environment"));

    /// <summary>
    /// Composes a function value in a form that supports incremental argument application via
    /// <see cref="Expression.ParseAndEval"/> expressions.
    /// </summary>
    /// <param name="innerExpression">The inner function expression body.</param>
    /// <param name="parameterCount">Total number of parameters expected by the function.</param>
    /// <param name="envFunctions">List of environment functions needed by the inner expression.</param>
    /// <returns>
    /// A <see cref="PineValue"/> representing the nested wrapper expression encoded as a value.
    /// When evaluated with the first argument, it produces the next wrapper (or final result).
    /// </returns>
    public static PineValue EmitFunctionValue(
        Expression innerExpression,
        int parameterCount,
        IReadOnlyList<PineValue> envFunctions)
    {
        if (parameterCount <= 0)
        {
            // Zero parameters: direct invocation
            return EmitZeroParameterWrapper(innerExpression, envFunctions);
        }

        if (parameterCount is 1)
        {
            // Single parameter: simple wrapper that takes env as the argument
            return EmitSingleParameterWrapper(innerExpression, envFunctions);
        }

        // Multiple parameters: build recursive wrapper structure
        return EmitMultiParameterWrapper(innerExpression, parameterCount, envFunctions);
    }

    /// <summary>
    /// Creates a wrapper for a zero-parameter function - immediate invocation.
    /// </summary>
    private static PineValue EmitZeroParameterWrapper(
        Expression innerExpression,
        IReadOnlyList<PineValue> envFunctions)
    {
        // ParseAndEval(innerExpr, [envFunctions, []])
        var envFunctionsExpr = CreateLiteralListExpression(envFunctions);
        var invocationEnv = Expression.ListInstance([envFunctionsExpr, Expression.EmptyList]);

        var invocationExpr =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(ExpressionEncoding.EncodeExpressionAsValue(innerExpression)),
                environment: invocationEnv);

        return ExpressionEncoding.EncodeExpressionAsValue(invocationExpr);
    }

    /// <summary>
    /// Creates a wrapper for a single-parameter function.
    /// When evaluated with arg as env, invokes innerExpr with [envFuncs, [arg]].
    /// </summary>
    private static PineValue EmitSingleParameterWrapper(
        Expression innerExpression,
        IReadOnlyList<PineValue> envFunctions)
    {
        // Expression: ParseAndEval(innerExpr, [envFunctions, [env]])
        // Where env is the single argument
        var envFunctionsExpr = CreateLiteralListExpression(envFunctions);
        var argsListExpr = Expression.ListInstance([Expression.EnvironmentInstance]);
        var invocationEnv = Expression.ListInstance([envFunctionsExpr, argsListExpr]);

        var invocationExpr =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(ExpressionEncoding.EncodeExpressionAsValue(innerExpression)),
                environment: invocationEnv);

        return ExpressionEncoding.EncodeExpressionAsValue(invocationExpr);
    }

    /// <summary>
    /// Creates a nested wrapper for a multi-parameter function (N >= 2).
    /// 
    /// The structure is built from outside in:
    /// - Level 0 (outermost): receives arg0, returns encoded Level 1 with arg0 embedded
    /// - Level 1: receives arg1, returns encoded Level 2 with [arg0, arg1] embedded
    /// - ...
    /// - Level N-1 (innermost): receives argN-1, invokes inner function with [arg0..argN-1]
    /// </summary>
    private static PineValue EmitMultiParameterWrapper(
        Expression innerExpression,
        int parameterCount,
        IReadOnlyList<PineValue> envFunctions)
    {
        // Build from innermost to outermost
        // Innermost level: takes last arg, invokes function with all args
        // Each outer level: takes an arg, returns encoded inner level with arg captured

        // Start with the innermost expression (level N-1)
        // This level receives: [[arg0..argN-2], argN-1] as environment
        // and invokes: ParseAndEval(innerExpr, [envFuncs, concat(env[0], [env[1]])])
        var innermostExpr = BuildInnermostExpression(innerExpression, envFunctions);

        // Build outer levels from N-2 down to 0
        PineValue currentEncoded = ExpressionEncoding.EncodeExpressionAsValue(innermostExpr);

        for (var level = parameterCount - 2; level >= 0; level--)
        {
            currentEncoded = BuildIntermediateLevel(currentEncoded, level);
        }

        return currentEncoded;
    }

    /// <summary>
    /// Builds the innermost expression that invokes the inner function.
    /// 
    /// Environment structure: [[arg0, arg1, ..., argN-2], argN-1]
    /// Invocation: ParseAndEval(innerExpr, [envFuncs, concat(env[0], [env[1]])])
    /// </summary>
    private static Expression BuildInnermostExpression(
        Expression innerExpression,
        IReadOnlyList<PineValue> envFunctions)
    {
        // env[0] = list of previously collected args
        // env[1] = last argument
        var capturedArgsExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance);

        var lastArgExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance);

        // Full args = concat(captured, [lastArg])
        var fullArgsExpr =
            BuiltinAppConcatBinary(
                capturedArgsExpr,
                Expression.ListInstance([lastArgExpr]));

        // Build invocation environment: [envFuncs, fullArgs]
        var envFuncsExpr = CreateLiteralListExpression(envFunctions);
        var invocationEnv = Expression.ListInstance([envFuncsExpr, fullArgsExpr]);

        // ParseAndEval(innerExpr, invocationEnv)
        return
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(ExpressionEncoding.EncodeExpressionAsValue(innerExpression)),
                environment: invocationEnv);
    }

    /// <summary>
    /// Builds an intermediate level wrapper.
    /// 
    /// For level 0: env = arg0, returns encoded next level with [[arg0]] as captured
    /// For level > 0: env = [[captured_so_far], current_arg], returns encoded next level with updated captured
    /// 
    /// The returned expression, when evaluated, produces the next level's encoded expression
    /// with the current argument captured.
    /// </summary>
    private static PineValue BuildIntermediateLevel(PineValue nextLevelEncoded, int level)
    {
        Expression wrapperExpr;

        if (level is 0)
        {
            // Level 0: env = arg0 directly
            // Returns an expression that encodes ParseAndEval(nextLevel, [[arg0], <future_env>])
            // Where <future_env> will be filled when the returned expression is evaluated

            // We need to construct a value that IS an encoded expression
            // That encoded expression expects env = arg1
            // And uses [[arg0]] as its captured args (arg0 embedded from this level's env)

            // The expression we build returns:
            // ParseAndEval(nextLevel, [[Literal([env])], ["Environment", []]]]
            wrapperExpr = BuildLevel0Expression(nextLevelEncoded);
        }
        else
        {
            // Level > 0: env = [[captured_so_far], current_arg]
            // Returns encoded ParseAndEval(nextLevel, [concat(captured, [current_arg]), <future_env>])
            wrapperExpr = BuildLevelNExpression(nextLevelEncoded);
        }

        return ExpressionEncoding.EncodeExpressionAsValue(wrapperExpr);
    }

    /// <summary>
    /// Builds the level 0 expression.
    /// 
    /// This expression receives arg0 as its environment and produces an encoded expression
    /// that expects arg1 and has [[arg0]] as captured arguments.
    /// </summary>
    private static Expression BuildLevel0Expression(PineValue nextLevelEncoded)
    {
        // We need to return an encoded expression (a value).
        // That value encodes: ParseAndEval(nextLevel, [[[arg0]], env_of_returned_expr])
        // 
        // At this level: env = arg0
        // The returned expr will be evaluated later with env = arg1
        // The returned expr needs to have [[arg0]] baked in (as literal)
        //
        // Build a List expression that constructs the encoding:
        // ["ParseAndEval", [Literal(nextLevel), [[Literal([[env]])], ["Environment", []]]]]
        //
        // Using list operations to construct the Pine expression encoding dynamically

        // Encode the next level as a literal: ["Literal", [nextLevelValue]]
        var nextLevelLiteralEncoded =
            Expression.ListInstance(
                [
                s_literalTag,
                Expression.ListInstance([Expression.LiteralInstance(nextLevelEncoded)])
                ]);

        // Encode Environment expression: ["Environment", []]
        var environmentEncoded =
            Expression.ListInstance(
                [
                s_environmentTag,
                Expression.EmptyList
                ]);

        // Encode captured args [arg0]:
        // At this level, env = arg0 directly
        // We need to build Literal([env]) which when evaluated produces [arg0]
        // capturedArgsEncoded = ["Literal", [[env]]]
        var capturedArgsEncoded =
            Expression.ListInstance(
                [
                s_literalTag,
                Expression.ListInstance([Expression.ListInstance([Expression.EnvironmentInstance])])
                ]);

        // Encode the full env structure: ["List", [[capturedArgsEncoded, environmentEncoded]]]
        // This produces [[captured_args], next_env] when evaluated
        var envStructureEncoded =
            Expression.ListInstance(
                [
                s_listTag,
                Expression.ListInstance([Expression.ListInstance([capturedArgsEncoded, environmentEncoded])])
                ]);

        // Final ParseAndEval encoding: ["ParseAndEval", [nextLevelLiteralEncoded, envStructureEncoded]]
        return
            Expression.ListInstance(
                [
                s_parseAndEvalTag,
                Expression.ListInstance([nextLevelLiteralEncoded, envStructureEncoded])
                ]);
    }

    /// <summary>
    /// Builds a level N (N > 0) expression.
    /// 
    /// This expression receives [[captured_so_far], current_arg] as env and produces
    /// an encoded expression for the next level with updated captured arguments.
    /// </summary>
    private static Expression BuildLevelNExpression(PineValue nextLevelEncoded)
    {
        // Environment at this level: [[captured_so_far], current_arg]
        // env[0] = [captured_so_far], env[1] = current_arg
        // new_captured = concat(captured_so_far, [current_arg])
        //
        // Return encoded: ParseAndEval(nextLevel, [[new_captured], env_of_returned_expr])

        // Get captured_so_far and current_arg from environment
        var capturedSoFar =
            ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance);

        var currentArg =
            ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance);

        // new_captured = concat(captured_so_far, [current_arg])
        var newCapturedExpr =
            BuiltinAppConcatBinary(
                capturedSoFar,
                Expression.ListInstance([currentArg]));

        // Encode nextLevel as literal
        var nextLevelLiteralEncoded =
            Expression.ListInstance(
                [
                s_literalTag,
                Expression.ListInstance([Expression.LiteralInstance(nextLevelEncoded)])
                ]);

        // Encode Environment for the returned expression
        var environmentEncoded = Expression.ListInstance(
            [s_environmentTag, Expression.EmptyList]);

        // Encode captured as literal: ["Literal", [newCaptured]]
        // Note: newCapturedExpr is evaluated at THIS level's eval time
        // This produces [new_captured] when evaluated
        var capturedLiteralEncoded = Expression.ListInstance(
            [
                s_literalTag,
                Expression.ListInstance([newCapturedExpr])
            ]);

        // Encode env structure: ["List", [[capturedLiteralEncoded, environmentEncoded]]]
        // This produces [[new_captured], next_env] when evaluated
        var envStructureEncoded = Expression.ListInstance(
            [
                s_listTag,
                Expression.ListInstance([Expression.ListInstance([capturedLiteralEncoded, environmentEncoded])])
            ]);

        // Final: ["ParseAndEval", [nextLevelLiteralEncoded, envStructureEncoded]]
        return Expression.ListInstance(
            [
                s_parseAndEvalTag,
                Expression.ListInstance([nextLevelLiteralEncoded, envStructureEncoded])
            ]);
    }

    /// <summary>
    /// Creates a literal List expression containing the given values.
    /// </summary>
    private static Expression CreateLiteralListExpression(IReadOnlyList<PineValue> values)
    {
        return ExpressionBuilder.CreateLiteralList(values);
    }

    private static Expression.KernelApplication BuiltinAppConcatBinary(Expression left, Expression right) =>
        Expression.KernelApplicationInstance(
            function: nameof(KernelFunction.concat),
            input: Expression.ListInstance([left, right]));
}
