using Pine.Core.CommonEncodings;
using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Creates nested wrappers for generic and partial application of Elm functions.
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
/// 
/// <para>
/// For background on function values and generic function application, see
/// <see href="https://github.com/pine-vm/pine/blob/fa0af408c25311d1fd3b5f6ba68d12197fcd4f8b/implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md"></see>
/// </para>
/// </summary>
public static class FunctionValueBuilder
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
    /// Composes a function expression that, when evaluated, produces a function value
    /// supporting incremental argument application via <see cref="Expression.ParseAndEval"/> expressions.
    /// Unlike <see cref="EmitFunctionValue"/>, this method takes expressions for env functions
    /// that will be evaluated to get the actual values at expression evaluation time.
    /// </summary>
    /// <param name="innerExpression">The inner function expression body.</param>
    /// <param name="parameterCount">Total number of parameters expected by the function.</param>
    /// <param name="envFunctionsExprs">Expressions that evaluate to environment function values.</param>
    /// <returns>
    /// An <see cref="Expression"/> that, when evaluated, produces the nested wrapper expression
    /// encoded as a <see cref="PineValue"/>.
    /// </returns>
    public static Expression EmitFunctionExpression(
        Expression innerExpression,
        int parameterCount,
        IReadOnlyList<Expression> envFunctionsExprs)
    {
        if (parameterCount <= 0)
        {
            // Zero parameters: direct invocation
            return EmitZeroParameterWrapperExpression(innerExpression, envFunctionsExprs);
        }

        if (parameterCount is 1)
        {
            // Single parameter: simple wrapper that takes env as the argument
            return EmitSingleParameterWrapperExpression(innerExpression, envFunctionsExprs);
        }

        // Multiple parameters: build recursive wrapper structure
        return EmitMultiParameterWrapperExpression(innerExpression, parameterCount, envFunctionsExprs);
    }

    /// <summary>
    /// Creates an expression that produces the encoding for a zero-parameter function wrapper.
    /// </summary>
    private static Expression EmitZeroParameterWrapperExpression(
        Expression innerExpression,
        IReadOnlyList<Expression> envFunctionsExprs)
    {
        // Build the encoding: ["ParseAndEval", [Literal(innerExpr), [envFuncs, []]]]
        // The env functions need to be evaluated dynamically

        var innerExprEncoded = ExpressionEncoding.EncodeExpressionAsValue(innerExpression);

        // Build: ["Literal", [innerExpr]]
        var encodedExprLiteralEncoding = Expression.ListInstance(
            [
            s_literalTag,
            Expression.ListInstance([Expression.LiteralInstance(innerExprEncoded)])
            ]);

        // Build env functions list encoding: ["List", [[envFunc0, envFunc1, ...]]]
        // Each envFunc is wrapped in ["Literal", [envFuncExpr]] where envFuncExpr is evaluated
        var envFuncEncodings = new Expression[envFunctionsExprs.Count];
        for (var i = 0; i < envFunctionsExprs.Count; i++)
        {
            envFuncEncodings[i] = Expression.ListInstance(
                [
                s_literalTag,
                Expression.ListInstance([envFunctionsExprs[i]])
                ]);
        }

        var envFuncsListEncoding = Expression.ListInstance(
            [
            s_listTag,
            Expression.ListInstance([Expression.ListInstance(envFuncEncodings)])
            ]);

        // Build empty list encoding: ["List", [[]]]
        var emptyListEncoding = Expression.ListInstance(
            [
            s_listTag,
            Expression.ListInstance([Expression.EmptyList])
            ]);

        // Build invocation env: ["List", [[envFuncsListEncoding, emptyListEncoding]]]
        var invocationEnvEncoding = Expression.ListInstance(
            [
            s_listTag,
            Expression.ListInstance([Expression.ListInstance([envFuncsListEncoding, emptyListEncoding])])
            ]);

        // Final: ["ParseAndEval", [encodedExprLiteralEncoding, invocationEnvEncoding]]
        return Expression.ListInstance(
            [
            s_parseAndEvalTag,
            Expression.ListInstance([encodedExprLiteralEncoding, invocationEnvEncoding])
            ]);
    }

    /// <summary>
    /// Creates an expression that produces the encoding for a single-parameter function wrapper.
    /// </summary>
    private static Expression EmitSingleParameterWrapperExpression(
        Expression innerExpression,
        IReadOnlyList<Expression> envFunctionsExprs)
    {
        // Build the encoding: ["ParseAndEval", [Literal(innerExpr), [envFuncs, [Environment]]]]

        var innerExprEncoded = ExpressionEncoding.EncodeExpressionAsValue(innerExpression);

        // Build: ["Literal", [innerExpr]]
        var encodedExprLiteralEncoding = Expression.ListInstance(
            [
            s_literalTag,
            Expression.ListInstance([Expression.LiteralInstance(innerExprEncoded)])
            ]);

        // Build env functions list encoding
        var envFuncEncodings = new Expression[envFunctionsExprs.Count];
        for (var i = 0; i < envFunctionsExprs.Count; i++)
        {
            envFuncEncodings[i] = Expression.ListInstance(
                [
                s_literalTag,
                Expression.ListInstance([envFunctionsExprs[i]])
                ]);
        }

        var envFuncsListEncoding = Expression.ListInstance(
            [
            s_listTag,
            Expression.ListInstance([Expression.ListInstance(envFuncEncodings)])
            ]);

        // Build [Environment] encoding: ["List", [["Environment", []]]]
        var environmentExprEncoding = Expression.ListInstance(
            [
            s_environmentTag,
            Expression.EmptyList
            ]);

        var argsListEncoding = Expression.ListInstance(
            [
            s_listTag,
            Expression.ListInstance([Expression.ListInstance([environmentExprEncoding])])
            ]);

        // Build invocation env: ["List", [[envFuncsListEncoding, argsListEncoding]]]
        var invocationEnvEncoding = Expression.ListInstance(
            [
            s_listTag,
            Expression.ListInstance([Expression.ListInstance([envFuncsListEncoding, argsListEncoding])])
            ]);

        // Final: ["ParseAndEval", [encodedExprLiteralEncoding, invocationEnvEncoding]]
        return Expression.ListInstance(
            [
            s_parseAndEvalTag,
            Expression.ListInstance([encodedExprLiteralEncoding, invocationEnvEncoding])
            ]);
    }

    /// <summary>
    /// Creates an expression that produces the encoding for a multi-parameter function wrapper.
    /// The returned expression, when evaluated, produces the same value as 
    /// <see cref="EmitMultiParameterWrapper"/> would with the same env function values.
    /// 
    /// Strategy: Build the complete nested encoding as a single expression that, when evaluated,
    /// produces the final PineValue. The env functions are embedded in the innermost level,
    /// which is then wrapped by static outer levels.
    /// 
    /// The key insight: EmitFunctionValue returns EncodeExpressionAsValue(level0WrapperExpr),
    /// where level0WrapperExpr is a List expression that produces a ParseAndEval encoding.
    /// So we need to produce an expression that evaluates to that same encoded value.
    /// </summary>
    private static Expression EmitMultiParameterWrapperExpression(
        Expression innerExpression,
        int parameterCount,
        IReadOnlyList<Expression> envFunctionsExprs)
    {
        // Build the innermost expression (as an Expression, not encoded yet)
        var innermostExpr = BuildInnermostExpressionWithEnvExprs(innerExpression, envFunctionsExprs);

        // Encode the innermost expression - produces a value when evaluated
        var innermostEncodingExpr = EncodeExpressionAsListExpression(innermostExpr);

        // Build outer wrapper levels from N-2 down to 0
        // Each level takes the previous encoding and wraps it
        Expression currentEncodingExpr = innermostEncodingExpr;

        for (var level = parameterCount - 2; level >= 0; level--)
        {
            // Build the wrapper expression for this level
            // This is a List expression that produces a ParseAndEval encoding when evaluated
            var wrapperExpr = BuildWrapperExpressionForLevel(currentEncodingExpr, level);
            
            // Encode this wrapper expression - now we have an expression that produces
            // the encoding of the wrapper (which is what EmitFunctionValue returns)
            currentEncodingExpr = EncodeExpressionAsListExpression(wrapperExpr);
        }

        return currentEncodingExpr;
    }

    /// <summary>
    /// Builds the wrapper expression for a given level.
    /// This is a List expression that, when evaluated, produces a ParseAndEval encoding.
    /// Similar to BuildLevel0Expression/BuildLevelNExpression in EmitFunctionValue.
    /// </summary>
    private static Expression BuildWrapperExpressionForLevel(Expression nextLevelEncodingExpr, int level)
    {
        if (level is 0)
        {
            // Level 0: env = arg0 directly
            // Produces: ["ParseAndEval", [["Literal", [nextLevel]], ["List", [[["Literal", [[env]]], ["Environment", []]]]]]]
            return BuildLevel0WrapperExpression(nextLevelEncodingExpr);
        }
        else
        {
            // Level N > 0: env = [[captured], currentArg]
            return BuildLevelNWrapperExpression(nextLevelEncodingExpr);
        }
    }

    /// <summary>
    /// Builds the Level 0 wrapper expression (a List expression that produces ParseAndEval encoding).
    /// When evaluated, this produces: ["ParseAndEval", [["Literal", [nextLevel]], envStructure]]
    /// </summary>
    private static Expression BuildLevel0WrapperExpression(Expression nextLevelEncodingExpr)
    {
        // The wrapper is a List expression that, when evaluated, produces:
        // ["ParseAndEval", [["Literal", [nextLevelValue]], ["List", [[capturedArgs], ["Environment", []]]]]]
        // 
        // Where nextLevelValue comes from evaluating nextLevelEncodingExpr

        // ["Literal", [nextLevelEncodingExpr]] - when evaluated: ["Literal", [nextLevelValue]]
        var nextLevelLiteralEncoded = Expression.ListInstance(
            [
            s_literalTag,
            Expression.ListInstance([nextLevelEncodingExpr])
            ]);

        // ["Environment", []] - static
        var environmentEncoded = Expression.ListInstance(
            [s_environmentTag, Expression.EmptyList]);

        // ["Literal", [[env]]] - captures [[arg0]] when level 0 runs with env=arg0
        var capturedArgsEncoded = Expression.ListInstance(
            [
            s_literalTag,
            Expression.ListInstance([Expression.ListInstance([Expression.EnvironmentInstance])])
            ]);

        // ["List", [[capturedArgsEncoded, environmentEncoded]]]
        var envStructureEncoded = Expression.ListInstance(
            [
            s_listTag,
            Expression.ListInstance([Expression.ListInstance([capturedArgsEncoded, environmentEncoded])])
            ]);

        // ["ParseAndEval", [nextLevelLiteralEncoded, envStructureEncoded]]
        return Expression.ListInstance(
            [
            s_parseAndEvalTag,
            Expression.ListInstance([nextLevelLiteralEncoded, envStructureEncoded])
            ]);
    }

    /// <summary>
    /// Builds the Level N (N > 0) wrapper expression.
    /// When evaluated with env = [[captured], currentArg], produces encoding for next level.
    /// </summary>
    private static Expression BuildLevelNWrapperExpression(Expression nextLevelEncodingExpr)
    {
        // Similar to BuildLevelNExpression but nextLevel comes from expression
        
        var nextLevelLiteralEncoded = Expression.ListInstance(
            [
            s_literalTag,
            Expression.ListInstance([nextLevelEncodingExpr])
            ]);

        var environmentEncoded = Expression.ListInstance(
            [s_environmentTag, Expression.EmptyList]);

        // new_captured = concat(head(env), [head(skip(1, env))])
        var capturedSoFarExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance);
        var currentArgExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance);
        var newCapturedExpr =
            BuiltinAppConcatBinary(capturedSoFarExpr, Expression.ListInstance([currentArgExpr]));

        // ["Literal", [newCapturedExpr]] - evaluated at runtime
        var capturedArgsEncoded = Expression.ListInstance(
            [
            s_literalTag,
            Expression.ListInstance([newCapturedExpr])
            ]);

        var envStructureEncoded = Expression.ListInstance(
            [
            s_listTag,
            Expression.ListInstance([Expression.ListInstance([capturedArgsEncoded, environmentEncoded])])
            ]);

        return Expression.ListInstance(
            [
            s_parseAndEvalTag,
            Expression.ListInstance([nextLevelLiteralEncoded, envStructureEncoded])
            ]);
    }

    /// <summary>
    /// Builds the innermost expression with env functions from expressions.
    /// Similar to <see cref="BuildInnermostExpression"/> but takes expressions instead of values.
    /// </summary>
    private static Expression BuildInnermostExpressionWithEnvExprs(
        Expression innerExpression,
        IReadOnlyList<Expression> envFunctionsExprs)
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

        // Build env functions list - a List expression containing the env function expressions
        // When evaluated, this produces a list of the actual env function values
        var envFuncsExpr = Expression.ListInstance([.. envFunctionsExprs]);

        // Build invocation environment: [envFuncs, fullArgs]
        var invocationEnv = Expression.ListInstance([envFuncsExpr, fullArgsExpr]);

        // ParseAndEval(innerExpr, invocationEnv)
        return
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(ExpressionEncoding.EncodeExpressionAsValue(innerExpression)),
                environment: invocationEnv);
    }

    /// <summary>
    /// Builds a List expression that, when evaluated, produces the encoding of the given expression.
    /// This is like ExpressionEncoding.EncodeExpressionAsValue, but returns an Expression
    /// that produces the encoding when evaluated (allowing dynamic values from sub-expressions).
    /// </summary>
    private static Expression EncodeExpressionAsListExpression(Expression expression)
    {
        return expression switch
        {
            Expression.Literal literal =>
                // ["Literal", [value]]
                Expression.ListInstance(
                    [
                    s_literalTag,
                    Expression.ListInstance([Expression.LiteralInstance(literal.Value)])
                    ]),

            Expression.Environment =>
                // ["Environment", []]
                Expression.ListInstance([s_environmentTag, Expression.EmptyList]),

            Expression.List list =>
                // ["List", [[item0_encoded, item1_encoded, ...]]]
                EncodeListExpressionAsListExpression(list),

            Expression.ParseAndEval parseAndEval =>
                // ["ParseAndEval", [encoded_encoded, environment_encoded]]
                Expression.ListInstance(
                    [
                    s_parseAndEvalTag,
                    Expression.ListInstance(
                        [
                        EncodeExpressionAsListExpression(parseAndEval.Encoded),
                        EncodeExpressionAsListExpression(parseAndEval.Environment)
                        ])
                    ]),

            Expression.KernelApplication kernelApp =>
                // ["KernelApplication", [function_name, input_encoded]]
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(StringEncoding.ValueFromString("KernelApplication")),
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(StringEncoding.ValueFromString(kernelApp.Function)),
                        EncodeExpressionAsListExpression(kernelApp.Input)
                        ])
                    ]),

            _ =>
                throw new System.NotSupportedException(
                    $"EncodeExpressionAsListExpression does not support {expression.GetType().Name}")
        };
    }

    /// <summary>
    /// Encodes a List expression as a List expression that produces the encoding.
    /// </summary>
    private static Expression EncodeListExpressionAsListExpression(Expression.List list)
    {
        // ["List", [[item0_encoded, item1_encoded, ...]]]
        var encodedItems = new Expression[list.Items.Count];

        for (var i = 0; i < list.Items.Count; i++)
        {
            encodedItems[i] = EncodeExpressionAsListExpression(list.Items[i]);
        }

        return Expression.ListInstance(
            [
            s_listTag,
            Expression.ListInstance([Expression.ListInstance(encodedItems)])
            ]);
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
