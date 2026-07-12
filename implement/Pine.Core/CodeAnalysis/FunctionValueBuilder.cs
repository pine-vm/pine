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
/// For background on function values and generic function application, see 'elm-compiler-implementation-guide.md'
/// </para>
/// </summary>
public static class FunctionValueBuilder
{
    private static readonly Expression s_evalTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("Eval"));

    private static readonly Expression s_litralTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("Litral"));

    private static readonly Expression s_listTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("List"));

    private static readonly Expression s_environmentTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("Environment"));

    private static readonly Expression s_builtinTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("Builtin"));

    private static readonly Expression s_conditionTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("Condition"));

    private static readonly Expression s_labelTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("Label"));

    private static readonly Expression s_concatTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("concat"));

    private static readonly Expression s_level0EnvStructureEncodingStatic =
        BuildLevel0EnvStructureEncodingStatic();

    private static readonly Expression s_levelNEnvStructureEncodingStatic =
        BuildLevelNEnvStructureEncodingStatic();

    /// <summary>
    /// Composes a function value in a form that supports incremental argument application via
    /// <see cref="Expression.ParseAndEval"/> expressions.
    /// The inner expression will receive environment as [envFunctions, arg0, arg1, ...].
    /// </summary>
    /// <param name="innerExpression">The inner function expression body.</param>
    /// <param name="parameterCount">Total number of parameters expected by the function.</param>
    /// <param name="envFunctions">List of environment functions needed by the inner expression.</param>
    /// <param name="encodeExprCache">Optional cache for encoding expressions to avoid redundant work.</param>
    /// <returns>
    /// A <see cref="PineValue"/> representing the nested wrapper expression encoded as a value.
    /// When evaluated with the first argument, it produces the next wrapper (or final result).
    /// </returns>
    public static PineValue EmitFunctionValueWithEnvFunctions(
        Expression innerExpression,
        int parameterCount,
        IReadOnlyList<PineValue> envFunctions,
        PineExpressionEncodingCache? encodeExprCache = null)
    {
        if (parameterCount <= 0)
        {
            // Zero parameters: direct invocation
            return EmitZeroParameterWrapper(innerExpression, envFunctions, encodeExprCache);
        }

        if (parameterCount is 1)
        {
            // Single parameter: simple wrapper that takes env as the argument
            return EmitSingleParameterWrapper(innerExpression, envFunctions, encodeExprCache);
        }

        // Multiple parameters: build recursive wrapper structure
        return EmitMultiParameterWrapper(innerExpression, parameterCount, envFunctions, encodeExprCache);
    }

    /// <summary>
    /// Composes a function expression that, when evaluated, produces a function value
    /// supporting incremental argument application via <see cref="Expression.ParseAndEval"/> expressions.
    /// Unlike <see cref="EmitFunctionValueWithEnvFunctions"/>, this method takes expressions for env functions
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
        var innerExprEncoded = ExpressionEncoding2026.EncodeExpressionAsValue(innerExpression);

        return
            EmitFunctionExpressionFromEncodedBody(
                Expression.LiteralInstance(innerExprEncoded),
                parameterCount,
                envFunctionsExprs);
    }

    /// <summary>
    /// Composes a function expression that, when evaluated, produces a function value
    /// supporting incremental argument application via <see cref="Expression.ParseAndEval"/> expressions.
    /// This overload takes an expression that evaluates to the already-encoded function body,
    /// which is useful when the encoded body is stored in the environment (e.g., for same-SCC functions).
    /// </summary>
    /// <param name="encodedBodyExpr">An expression that evaluates to the already-encoded function body.</param>
    /// <param name="parameterCount">Total number of parameters expected by the function.</param>
    /// <param name="envFunctionsExprs">Expressions that evaluate to environment function values.</param>
    /// <returns>
    /// An <see cref="Expression"/> that, when evaluated, produces the nested wrapper expression
    /// encoded as a <see cref="PineValue"/>.
    /// </returns>
    public static Expression EmitFunctionExpressionFromEncodedBody(
        Expression encodedBodyExpr,
        int parameterCount,
        IReadOnlyList<Expression> envFunctionsExprs)
    {
        if (parameterCount <= 0)
        {
            // Zero parameters: direct invocation
            return EmitZeroParameterWrapperExpressionFromEncodedBody(encodedBodyExpr, envFunctionsExprs);
        }

        if (parameterCount is 1)
        {
            // Single parameter: simple wrapper that takes env as the argument
            return EmitSingleParameterWrapperExpressionFromEncodedBody(encodedBodyExpr, envFunctionsExprs);
        }

        // Multiple parameters: build recursive wrapper structure
        return EmitMultiParameterWrapperExpressionFromEncodedBody(encodedBodyExpr, parameterCount, envFunctionsExprs);
    }

    /// <summary>
    /// Builds the compact encoding of a wrapper level, embedding the expression that produces
    /// the next level's encoded value.
    /// </summary>
    private static Expression BuildWrapperLevelEncodingDirect(Expression nextLevelEncodingExpr, int level)
    {
        var envStructureEncoding =
            level is 0
            ?
            s_level0EnvStructureEncodingStatic
            :
            s_levelNEnvStructureEncodingStatic;

        var nextLevelLiteralEncoding =
            EncodeList(
                [
                EncodeLitral(s_litralTag),
                EncodeLitral(nextLevelEncodingExpr)
                ]);

        return EncodeList(
            [
            EncodeLitral(s_evalTag),
            nextLevelLiteralEncoding,
            EncodeExpressionAsListExpression(envStructureEncoding)
            ]);
    }

    /// <summary>
    /// Builds the static encoding of level 0 env structure.
    /// At level 0 runtime, env = arg0, so the environment contains the captured argument
    /// and the environment of the next wrapper.
    /// </summary>
    private static Expression BuildLevel0EnvStructureEncodingStatic()
    {
        return EncodeList(
            [
            EncodeLitral(Expression.ListInstance([Expression.EnvironmentInstance])),
            EncodeEnvironment()
            ]);
    }

    /// <summary>
    /// Builds the static encoding of level N > 0 env structure.
    /// At level N runtime, env = [[captured], arg], so envStructure captures newCaptured = concat(env[0], [env[1]]).
    /// </summary>
    private static Expression BuildLevelNEnvStructureEncodingStatic()
    {
        var capturedSoFarExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance);

        var currentArgExpr = ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance);
        var newCapturedExpr = BuiltinAppConcatBinary(capturedSoFarExpr, Expression.ListInstance([currentArgExpr]));

        return EncodeList(
            [
            EncodeLitral(newCapturedExpr),
            EncodeEnvironment()
            ]);
    }

    /// <summary>
    /// Builds a List expression that, when evaluated, produces the encoding of the given expression.
    /// This is like ExpressionEncoding2026.EncodeExpressionAsValue, but returns an Expression
    /// that produces the encoding when evaluated (allowing dynamic values from sub-expressions).
    /// </summary>
    private static Expression EncodeExpressionAsListExpression(Expression expression)
    {
        return expression switch
        {
            Expression.Literal literal =>
            EncodeLitral(Expression.LiteralInstance(literal.Value)),

            Expression.Environment =>
            EncodeEnvironment(),

            Expression.List list =>
            EncodeListExpressionAsListExpression(list),

            Expression.ParseAndEval parseAndEval =>
            EncodeEval(
                EncodeExpressionAsListExpression(parseAndEval.Encoded),
                EncodeExpressionAsListExpression(parseAndEval.Environment)),

            Expression.KernelApplication kernelApp =>
            EncodeBuiltin(
                Expression.LiteralInstance(StringEncoding.ValueFromString(kernelApp.Function)),
                EncodeExpressionAsListExpression(kernelApp.Input)),

            Expression.Conditional conditional =>
            EncodeCondition(
                EncodeExpressionAsListExpression(conditional.Condition),
                EncodeExpressionAsListExpression(conditional.FalseBranch),
                EncodeExpressionAsListExpression(conditional.TrueBranch)),

            Expression.StringTag stringTag =>
            EncodeLabel(
                Expression.LiteralInstance(stringTag.LabelValue),
                EncodeExpressionAsListExpression(stringTag.Tagged)),

            _ =>
            throw new System.NotImplementedException(
                "EncodeExpressionAsListExpression does not handle expression variant: " +
                expression.GetType().Name)
        };
    }

    /// <summary>
    /// Encodes a List expression as a List expression that produces the encoding.
    /// </summary>
    private static Expression EncodeListExpressionAsListExpression(Expression.List list)
    {
        var encodedItems = new Expression[list.Items.Count];

        for (var i = 0; i < list.Items.Count; i++)
        {
            encodedItems[i] = EncodeExpressionAsListExpression(list.Items[i]);
        }

        return EncodeList(encodedItems);
    }

    private static Expression EncodeLitral(Expression value) =>
        Expression.ListInstance([s_litralTag, value]);

    private static Expression EncodeList(IReadOnlyList<Expression> encodedItems)
    {
        var items = new Expression[encodedItems.Count + 1];

        items[0] = s_listTag;

        for (var i = 0; i < encodedItems.Count; ++i)
            items[i + 1] = encodedItems[i];

        return Expression.ListInstance(items);
    }

    private static Expression EncodeBuiltin(Expression functionName, Expression encodedInput) =>
        Expression.ListInstance([s_builtinTag, functionName, encodedInput]);

    private static Expression EncodeCondition(
        Expression encodedCondition,
        Expression encodedFalseBranch,
        Expression encodedTrueBranch) =>
        Expression.ListInstance(
            [s_conditionTag, encodedCondition, encodedFalseBranch, encodedTrueBranch]);

    private static Expression EncodeEnvironment() =>
        Expression.ListInstance([s_environmentTag]);

    private static Expression EncodeEval(
        Expression encodedExpression,
        Expression encodedEnvironment) =>
        Expression.ListInstance([s_evalTag, encodedExpression, encodedEnvironment]);

    private static Expression EncodeLabel(Expression label, Expression encodedLabeled) =>
        Expression.ListInstance([s_labelTag, label, encodedLabeled]);

    /// <summary>
    /// Creates a wrapper for a zero-parameter function - immediate invocation.
    /// </summary>
    private static PineValue EmitZeroParameterWrapper(
        Expression innerExpression,
        IReadOnlyList<PineValue> envFunctions,
        PineExpressionEncodingCache? encodeExprCache)
    {
        // ParseAndEval(innerExpr, [envFunctions])
        var envFunctionsExpr = CreateLiteralListExpression(envFunctions);
        var invocationEnv = Expression.ListInstance([envFunctionsExpr]);

        var invocationExpr =
            new Expression.ParseAndEval(
                encoded:
                Expression.LiteralInstance(EncodeExpressionAsValue2026(innerExpression, encodeExprCache)),
                environment: invocationEnv);

        return EncodeExpressionAsValue2026(invocationExpr, encodeExprCache);
    }

    /// <summary>
    /// Creates a wrapper for a single-parameter function.
    /// When evaluated with arg as env, invokes innerExpr with [envFuncs, arg].
    /// </summary>
    private static PineValue EmitSingleParameterWrapper(
        Expression innerExpression,
        IReadOnlyList<PineValue> envFunctions,
        PineExpressionEncodingCache? encodeExprCache)
    {
        // Expression: ParseAndEval(innerExpr, [envFunctions, env])
        // Where env is the single argument
        var envFunctionsExpr = CreateLiteralListExpression(envFunctions);
        var invocationEnv = Expression.ListInstance([envFunctionsExpr, Expression.EnvironmentInstance]);

        var invocationExpr =
            new Expression.ParseAndEval(
                encoded:
                Expression.LiteralInstance(EncodeExpressionAsValue2026(innerExpression, encodeExprCache)),
                environment: invocationEnv);

        return EncodeExpressionAsValue2026(invocationExpr, encodeExprCache);
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
        IReadOnlyList<PineValue> envFunctions,
        PineExpressionEncodingCache? encodeExprCache)
    {
        // Build from innermost to outermost
        // Innermost level: takes last arg, invokes function with all args
        // Each outer level: takes an arg, returns encoded inner level with arg captured

        // Start with the innermost expression (level N-1)
        // This level receives: [[arg0..argN-2], argN-1] as environment
        // and invokes: ParseAndEval(innerExpr, concat([envFuncs], captured, [env[1]]))
        var innermostExpr = BuildInnermostExpression(innerExpression, envFunctions, encodeExprCache);

        // Build outer levels from N-2 down to 0
        PineValue currentEncoded = EncodeExpressionAsValue2026(innermostExpr, encodeExprCache);

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
    /// Invocation: ParseAndEval(innerExpr, concat([envFuncs], env[0], [env[1]]))
    /// </summary>
    private static Expression BuildInnermostExpression(
        Expression innerExpression,
        IReadOnlyList<PineValue> envFunctions,
        PineExpressionEncodingCache? encodeExprCache)
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

        // Build invocation environment: concat([envFuncs], fullArgs)
        var envFuncsExpr = CreateLiteralListExpression(envFunctions);

        var invocationEnv =
            BuiltinAppConcatBinary(
                Expression.ListInstance([envFuncsExpr]),
                fullArgsExpr);

        // ParseAndEval(innerExpr, invocationEnv)
        return
            new Expression.ParseAndEval(
                encoded:
                Expression.LiteralInstance(EncodeExpressionAsValue2026(innerExpression, encodeExprCache)),
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
            // ParseAndEval(nextLevel, [Literal([env]), Environment])
            wrapperExpr = BuildLevel0Expression(nextLevelEncoded);
        }
        else
        {
            // Level > 0: env = [[captured_so_far], current_arg]
            // Returns encoded ParseAndEval(nextLevel, [concat(captured, [current_arg]), <future_env>])
            wrapperExpr = BuildLevelNExpression(nextLevelEncoded);
        }

        return ExpressionEncoding2026.EncodeExpressionAsValue(wrapperExpr);
    }

    /// <summary>
    /// Builds the level 0 expression.
    /// 
    /// This expression receives arg0 as its environment and produces an encoded expression
    /// that expects arg1 and has [[arg0]] as captured arguments.
    /// </summary>
    private static Expression BuildLevel0Expression(PineValue nextLevelEncoded)
    {
        var nextLevelLiteralEncoded =
            EncodeLitral(Expression.LiteralInstance(nextLevelEncoded));

        var capturedArgsEncoded =
            EncodeLitral(Expression.ListInstance([Expression.EnvironmentInstance]));

        var envStructureEncoded =
            EncodeList([capturedArgsEncoded, EncodeEnvironment()]);

        return EncodeEval(nextLevelLiteralEncoded, envStructureEncoded);
    }

    /// <summary>
    /// Builds a level N (N > 0) expression.
    /// 
    /// This expression receives [[captured_so_far], current_arg] as env and produces
    /// an encoded expression for the next level with updated captured arguments.
    /// </summary>
    private static Expression BuildLevelNExpression(PineValue nextLevelEncoded)
    {
        var capturedSoFar =
            ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance);

        var currentArg =
            ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance);

        var newCapturedExpr =
            BuiltinAppConcatBinary(
                capturedSoFar,
                Expression.ListInstance([currentArg]));

        var nextLevelLiteralEncoded =
            EncodeLitral(Expression.LiteralInstance(nextLevelEncoded));

        var capturedLiteralEncoded =
            EncodeLitral(newCapturedExpr);

        var envStructureEncoded =
            EncodeList([capturedLiteralEncoded, EncodeEnvironment()]);

        return EncodeEval(nextLevelLiteralEncoded, envStructureEncoded);
    }

    private static PineValue.ListValue EncodeExpressionAsValue2026(
        Expression expression,
        PineExpressionEncodingCache? cache) =>
        cache is null
        ?
        ExpressionEncoding2026.EncodeExpressionAsValue(expression)
        :
        cache.EncodeExpressionAsValue(expression);

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

    #region FromEncodedBody variants

    /// <summary>
    /// Creates an expression that produces the encoding for a zero-parameter function wrapper,
    /// using an expression that evaluates to the already-encoded function body.
    /// </summary>
    private static Expression EmitZeroParameterWrapperExpressionFromEncodedBody(
        Expression encodedBodyExpr,
        IReadOnlyList<Expression> envFunctionsExprs)
    {
        var encodedExprLiteralEncoding =
            EncodeLitral(encodedBodyExpr);

        var envFuncEncodings = EncodeLitrals(envFunctionsExprs);
        var envFuncsListEncoding =
            EncodeList(envFuncEncodings);

        var invocationEnvEncoding =
            EncodeList([envFuncsListEncoding]);

        return EncodeEval(encodedExprLiteralEncoding, invocationEnvEncoding);
    }

    /// <summary>
    /// Creates an expression that produces the encoding for a single-parameter function wrapper,
    /// using an expression that evaluates to the already-encoded function body.
    /// </summary>
    private static Expression EmitSingleParameterWrapperExpressionFromEncodedBody(
        Expression encodedBodyExpr,
        IReadOnlyList<Expression> envFunctionsExprs)
    {
        var encodedExprLiteralEncoding =
            EncodeLitral(encodedBodyExpr);

        var envFuncEncodings = EncodeLitrals(envFunctionsExprs);
        var envFuncsListEncoding =
            EncodeList(envFuncEncodings);

        var invocationEnvEncoding =
            EncodeList([envFuncsListEncoding, EncodeEnvironment()]);

        return EncodeEval(encodedExprLiteralEncoding, invocationEnvEncoding);
    }

    /// <summary>
    /// Creates an expression that produces the encoding for a multi-parameter function wrapper,
    /// using an expression that evaluates to the already-encoded function body.
    /// </summary>
    private static Expression EmitMultiParameterWrapperExpressionFromEncodedBody(
        Expression encodedBodyExpr,
        int parameterCount,
        IReadOnlyList<Expression> envFunctionsExprs)
    {
        // Build the innermost expression encoding directly with env funcs wrapped in Literal pattern
        var innermostEncodingExpr =
            BuildInnermostEncodingWithLiteralEnvFuncsFromEncodedBody(encodedBodyExpr, envFunctionsExprs);

        // Build outer wrapper levels from N-2 down to 0
        var currentEncodingExpr = innermostEncodingExpr;

        for (var level = parameterCount - 2; level >= 0; level--)
        {
            // Build wrapper encoding directly (not using EncodeExpressionAsListExpression)
            currentEncodingExpr = BuildWrapperLevelEncodingDirect(currentEncodingExpr, level);
        }

        return currentEncodingExpr;
    }

    /// <summary>
    /// Builds the innermost expression encoding using an expression that evaluates to the already-encoded body.
    /// </summary>
    private static Expression BuildInnermostEncodingWithLiteralEnvFuncsFromEncodedBody(
        Expression encodedBodyExpr,
        IReadOnlyList<Expression> envFunctionsExprs)
    {
        var innerExprLiteralEncoding =
            EncodeLitral(encodedBodyExpr);

        var envFuncEncodings = EncodeLitrals(envFunctionsExprs);
        var envFuncsListEncoding =
            EncodeList(envFuncEncodings);

        var capturedArgsExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance);

        var lastArgExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance);

        var fullArgsExpr =
            BuiltinAppConcatBinary(
                capturedArgsExpr,
                Expression.ListInstance([lastArgExpr]));

        var fullArgsEncoding = EncodeExpressionAsListExpression(fullArgsExpr);

        var envFuncsWrappedEncoding =
            EncodeList([envFuncsListEncoding]);

        var concatInputEncoding =
            EncodeList([envFuncsWrappedEncoding, fullArgsEncoding]);

        var invocationEnvEncoding =
            EncodeBuiltin(s_concatTag, concatInputEncoding);

        return EncodeEval(innerExprLiteralEncoding, invocationEnvEncoding);
    }

    private static Expression[] EncodeLitrals(IReadOnlyList<Expression> valueExpressions)
    {
        var encoded = new Expression[valueExpressions.Count];

        for (var i = 0; i < valueExpressions.Count; ++i)
            encoded[i] = EncodeLitral(valueExpressions[i]);

        return encoded;
    }

    #endregion
}
