using Pine.Core.CommonEncodings;
using Pine.Core.PineVM;
using System.Collections.Generic;

namespace Pine.Core.CodeGen;

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


    /// <summary>
    /// Marker used to identify placeholder slots for arguments while composing curried function templates
    /// in <see cref="TryBuildCurriedFunctionValueAsTemplate"/>. Placeholders are encoded as
    /// <c>List([marker, index])</c> literal values that are unlikely to collide with any real value.
    /// </summary>
    private static readonly PineValue s_templateArgPlaceholderMarker =
        StringEncoding.ValueFromString("__pine_curry_template_arg_placeholder__");

    /// <summary>
    /// Placeholder sentinel marking the slot that holds the encoded function body when the body is not
    /// available as a static value but is produced at runtime by an expression (see
    /// <see cref="EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBodyExpression"/>). It is distinct
    /// from <see cref="s_templateArgPlaceholderMarker"/> based argument sentinels so the two never collide.
    /// </summary>
    private static readonly PineValue s_templateEncodedBodyPlaceholderSentinel =
        PineValue.List(
            [StringEncoding.ValueFromString("__pine_curry_template_encoded_body_placeholder__")]);

    /// <summary>
    /// Placeholder sentinel marking the slot (environment index 0) that holds the env-functions list when
    /// the list is not available as a static value but is produced at runtime by an expression (see
    /// <see cref="EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBodyExpression"/>).
    /// </summary>
    private static readonly PineValue s_templateEnvFunctionsPlaceholderSentinel =
        PineValue.List(
            [StringEncoding.ValueFromString("__pine_curry_template_env_functions_placeholder__")]);

    private static readonly Expression s_level0EnvStructureEncodingStatic =
        BuildLevel0EnvStructureEncodingStatic();

    private static readonly Expression s_levelNEnvStructureEncodingStatic =
        BuildLevelNEnvStructureEncodingStatic();

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
    /// Builds a function value as a simple template that can be evaluated directly via
    /// <see cref="Interpreter.IntermediateVM.PineVM.DirectEvalIfSimpleTemplate(Expression, Internal.PineValueInProcess)"/>
    /// meaning the intermediate states reached via partial application contain no
    /// builtin/kernel expressions and no eval expressions.
    /// </summary>
    /// <param name="innerExpression">The inner function expression body.</param>
    /// <param name="parameterCount">Total number of parameters expected by the function.</param>
    /// <param name="envFunctions">
    /// Optional list of environment functions needed by the inner expression. When provided, the inner
    /// expression is evaluated with an environment of <c>[envFunctions, arg_0, ..., arg_{n-1}]</c>, i.e. the
    /// list of env functions is placed at index 0. When omitted or empty, index 0 holds an empty list.
    /// </param>
    /// <param name="encodeExprCache">Optional cache for encoding expressions to avoid redundant work.</param>
    public static PineValue? TryBuildCurriedFunctionValueAsTemplate(
        Expression innerExpression,
        int parameterCount,
        IReadOnlyList<PineValue>? envFunctions = null,
        PineExpressionEncodingCache? encodeExprCache = null)
    {
        if (parameterCount < 1)
        {
            return null;
        }

        var innerExprEncoded =
            ExpressionEncoding.EncodeExpressionAsValue(innerExpression, encodeExprCache);

        return
            TryBuildCurriedFunctionValueAsTemplateFromEncodedBody(
                innerExprEncoded,
                parameterCount,
                envFunctions,
                encodeExprCache);
    }

    /// <summary>
    /// Same as <see cref="TryBuildCurriedFunctionValueAsTemplate"/>, but takes the inner function
    /// body already encoded as a <see cref="PineValue"/> instead of an <see cref="Expression"/>.
    /// This is useful when the encoded body is already available (for example cached on a compiled
    /// function), avoiding the need to decode and re-encode it.
    /// </summary>
    /// <param name="innerExprEncoded">The inner function expression body, already encoded as a value.</param>
    /// <param name="parameterCount">Total number of parameters expected by the function.</param>
    /// <param name="envFunctions">
    /// Optional list of environment functions needed by the inner expression. When provided, the inner
    /// expression is evaluated with an environment of <c>[envFunctions, arg_0, ..., arg_{n-1}]</c>, i.e. the
    /// list of env functions is placed at index 0. When omitted or empty, index 0 holds an empty list.
    /// </param>
    /// <param name="encodeExprCache">Optional cache for encoding expressions to avoid redundant work.</param>
    public static PineValue? TryBuildCurriedFunctionValueAsTemplateFromEncodedBody(
        PineValue innerExprEncoded,
        int parameterCount,
        IReadOnlyList<PineValue>? envFunctions = null,
        PineExpressionEncodingCache? encodeExprCache = null)
    {
        if (parameterCount < 1)
        {
            return null;
        }

        var envFunctionsValue =
            PineValue.List(envFunctions is null ? [] : [.. envFunctions]);

        var outerExpression =
            BuildCurriedTemplateExpression(
                innerExprEncoded,
                parameterCount,
                capturedArgumentCount: 0,
                envFunctionsValue);

        return ExpressionEncoding.EncodeExpressionAsValue(outerExpression, encodeExprCache);
    }

    /// <summary>
    /// Emits an expression that, when evaluated, produces the template-form function value for a
    /// <em>partial application</em> in which the first <paramref name="leadingArgExpressions"/>
    /// arguments are already available as expressions at emission time.
    ///
    /// <para>
    /// The produced value is identical to the intermediate value obtained by applying the same leading
    /// arguments, one at a time, to the plain (zero-arg) function value produced by
    /// <see cref="TryBuildCurriedFunctionValueAsTemplate"/> for the same inner expression and parameter
    /// count. Instead of emitting the full curried template plus the leading applications, this emits only
    /// the nesting levels for the <em>remaining</em> parameters and embeds the already available arguments
    /// directly, yielding a compact emission. The embedded argument expressions are evaluated at the point
    /// where the emitted expression is evaluated (the partial application site).
    /// </para>
    /// </summary>
    /// <param name="innerExpression">The inner function body, expressed for the full parameter layout.</param>
    /// <param name="parameterCount">Total number of parameters expected by the function.</param>
    /// <param name="leadingArgExpressions">
    /// Expressions for the leading arguments already provided at the partial application site. Their count
    /// must be at least zero and strictly less than <paramref name="parameterCount"/> (at least one parameter
    /// must remain).
    /// </param>
    /// <param name="envFunctions">
    /// Optional list of environment functions needed by the inner expression, placed at environment index 0
    /// (mirrors <see cref="TryBuildCurriedFunctionValueAsTemplate"/>).
    /// </param>
    /// <param name="encodeExprCache">Optional cache for encoding expressions to avoid redundant work.</param>
    public static Expression EmitCurriedFunctionTemplateWithLeadingArgs(
        Expression innerExpression,
        int parameterCount,
        IReadOnlyList<Expression> leadingArgExpressions,
        IReadOnlyList<PineValue>? envFunctions = null,
        PineExpressionEncodingCache? encodeExprCache = null)
    {
        var innerExprEncoded = ExpressionEncoding2026.EncodeExpressionAsValue(innerExpression);

        return
            EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBody(
                innerExprEncoded,
                parameterCount,
                leadingArgExpressions,
                envFunctions);
    }

    /// <summary>
    /// Same as <see cref="EmitCurriedFunctionTemplateWithLeadingArgs"/>, but takes the inner function body
    /// already encoded as a <see cref="PineValue"/>.
    /// </summary>
    /// <param name="innerExprEncoded">The inner function body, already encoded as a value.</param>
    /// <param name="parameterCount">Total number of parameters expected by the function.</param>
    /// <param name="leadingArgExpressions">Expressions for the leading arguments already provided.</param>
    /// <param name="envFunctions">Optional list of environment functions placed at environment index 0.</param>
    public static Expression EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBody(
        PineValue innerExprEncoded,
        int parameterCount,
        IReadOnlyList<Expression> leadingArgExpressions,
        IReadOnlyList<PineValue>? envFunctions = null)
    {
        var envFunctionsValue =
            PineValue.List(envFunctions is null ? [] : [.. envFunctions]);

        return
            EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBodyExpression(
                Expression.LiteralInstance(innerExprEncoded),
                parameterCount,
                leadingArgExpressions,
                Expression.LiteralInstance(envFunctionsValue));
    }

    /// <summary>
    /// Same as <see cref="EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBody"/>, but takes the inner
    /// function body and the env-functions list as <em>expressions</em> (evaluated at the partial-application
    /// site) instead of static values. This supports the same-SCC case, where the encoded body and the
    /// env-functions list are not available as compile-time constants but are read from the caller's runtime
    /// environment (e.g. <c>env[0, functionIndex]</c> for the body and <c>env[0]</c> for the env-functions
    /// list). The produced function value is identical in shape to the static variant: both carry the
    /// encoded body and the env-functions list as <c>Literal</c> nodes.
    /// </summary>
    /// <param name="innerExprEncodedExpression">
    /// Expression that, evaluated at the partial-application site, produces the encoded inner function body.
    /// </param>
    /// <param name="parameterCount">Total number of parameters expected by the function.</param>
    /// <param name="leadingArgExpressions">Expressions for the leading arguments already provided.</param>
    /// <param name="envFunctionsExpression">
    /// Expression that, evaluated at the partial-application site, produces the env-functions list placed at
    /// environment index 0.
    /// </param>
    public static Expression EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBodyExpression(
        Expression innerExprEncodedExpression,
        int parameterCount,
        IReadOnlyList<Expression> leadingArgExpressions,
        Expression envFunctionsExpression)
    {
        if (parameterCount < 1)
        {
            throw new System.ArgumentOutOfRangeException(
                nameof(parameterCount),
                parameterCount,
                "Parameter count must be positive.");
        }

        if (leadingArgExpressions.Count >= parameterCount)
        {
            throw new System.ArgumentException(
                "At least one parameter must remain unapplied.",
                nameof(leadingArgExpressions));
        }

        var templateExpression =
            BuildCurriedTemplateExpression(
                s_templateEncodedBodyPlaceholderSentinel,
                parameterCount,
                leadingArgExpressions.Count,
                s_templateEnvFunctionsPlaceholderSentinel);

        return
            EncodeExpressionAsTemplateProducerViaPostOrder(
                templateExpression,
                replaceLiteral:
                literal =>
                {
                    if (literal.Value == s_templateEncodedBodyPlaceholderSentinel)
                        return EncodeLitral(innerExprEncodedExpression);

                    if (literal.Value == s_templateEnvFunctionsPlaceholderSentinel)
                        return EncodeLitral(envFunctionsExpression);

                    for (var argumentIndex = 0;
                        argumentIndex < leadingArgExpressions.Count;
                        ++argumentIndex)
                    {
                        if (literal.Value == BuildTemplateArgumentPlaceholder(argumentIndex))
                            return EncodeLitral(leadingArgExpressions[argumentIndex]);
                    }

                    return null;
                },
                evalIndependentToLiteral: false);
    }

    private static Expression BuildCurriedTemplateExpression(
        PineValue innerExprEncoded,
        int parameterCount,
        int capturedArgumentCount,
        PineValue envFunctionsValue)
    {
        var invocationEnvironmentItems = new Expression[parameterCount + 1];

        invocationEnvironmentItems[0] =
            Expression.LiteralInstance(envFunctionsValue);

        for (var argumentIndex = 0; argumentIndex < parameterCount - 1; ++argumentIndex)
        {
            invocationEnvironmentItems[argumentIndex + 1] =
                Expression.LiteralInstance(BuildTemplateArgumentPlaceholder(argumentIndex));
        }

        invocationEnvironmentItems[parameterCount] =
            Expression.EnvironmentInstance;

        Expression current =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(innerExprEncoded),
                environment: Expression.ListInstance(invocationEnvironmentItems));

        for (var argumentIndex = parameterCount - 2;
            argumentIndex >= capturedArgumentCount;
            --argumentIndex)
        {
            var placeholder = BuildTemplateArgumentPlaceholder(argumentIndex);

            current =
                EncodeExpressionAsTemplateProducerViaPostOrder(
                    current,
                    replaceLiteral:
                    literal =>
                    literal.Value == placeholder
                    ? EncodeLitral(Expression.EnvironmentInstance)
                    : null,
                    evalIndependentToLiteral: false);
        }

        return current;
    }

    private static PineValue BuildTemplateArgumentPlaceholder(int argumentIndex) =>
        PineValue.List(
            [
            s_templateArgPlaceholderMarker,
            IntegerEncoding.EncodeSignedInteger(argumentIndex)
            ]);

    /// <summary>
    /// Builds a template-producer expression using an explicit work stack instead of recursion, so that
    /// arbitrarily deep expression trees do not overflow the call stack.
    /// <para>
    /// Subexpressions are encoded in post-order (children before parents) into a local store that also
    /// deduplicates shared subtrees, so each distinct subexpression is encoded at most once. Each
    /// <see cref="Expression.Literal"/> is offered to <paramref name="replaceLiteral"/>; a non-null result
    /// is substituted in place of quoting the literal as a constant.
    /// </para>
    /// </summary>
    private static Expression EncodeExpressionAsTemplateProducerViaPostOrder(
        Expression rootExpression,
        System.Func<Expression.Literal, Expression?> replaceLiteral,
        bool evalIndependentToLiteral)
    {
        var store = new Dictionary<Expression, Expression>();

        var stack = new Stack<Expression>();

        stack.Push(rootExpression);

        var expanded = new HashSet<Expression>();

        while (stack.Count is not 0)
        {
            var current = stack.Peek();

            if (store.ContainsKey(current))
            {
                stack.Pop();
                continue;
            }

            if (expanded.Add(current))
            {
                // First visit: schedule not-yet-encoded direct children to be processed first.
                PushUnencodedTemplateChildren(current, store, stack);

                continue;
            }

            // Second visit: all children are encoded and present in 'store'.
            stack.Pop();

            var beforeReduce =
                EncodeTemplateNode(
                    current,
                    replaceLiteral,
                    child => store[child],
                    evalIndependentToLiteral);

            var reduced = beforeReduce;

            /*
            if (evalIndependentToLiteral)
            {
                reduced =
                    ReducePineExpression.ReduceExpressionBottomUp(beforeReduce, parseCache);
            }
            */

            store[current] = reduced;
        }

        return store[rootExpression];
    }

    private static void PushUnencodedTemplateChildren(
        Expression expression,
        Dictionary<Expression, Expression> store,
        Stack<Expression> stack)
    {
        switch (expression)
        {
            case Expression.Literal:
            case Expression.Environment:
                break;

            case Expression.List list:
                for (var i = 0; i < list.Items.Count; i++)
                {
                    if (!store.ContainsKey(list.Items[i]))
                        stack.Push(list.Items[i]);
                }

                break;

            case Expression.ParseAndEval parseAndEval:
                if (!store.ContainsKey(parseAndEval.Encoded))
                    stack.Push(parseAndEval.Encoded);

                if (!store.ContainsKey(parseAndEval.Environment))
                    stack.Push(parseAndEval.Environment);

                break;

            default:
                throw new System.NotSupportedException(
                    "EncodeExpressionAsTemplateProducer does not support " + expression.GetType().Name);
        }
    }

    /// <summary>
    /// Produces the template-producer encoding for a single <paramref name="expression"/> node, assuming
    /// every direct child has already been encoded and is retrievable via <paramref name="encodedChild"/>.
    /// </summary>
    private static Expression EncodeTemplateNode(
        Expression expression,
        System.Func<Expression.Literal, Expression?> replaceLiteral,
        System.Func<Expression, Expression> encodedChild,
        bool evalIndependentToLiteral)
    {
        if (evalIndependentToLiteral &&
            !expression.ReferencesEnvironment &&
            expression is not Expression.Literal)
        {
            if (TryEvaluateIndependentExpression(
                expression,
                abortForNode: node => node is Expression.Literal literal && replaceLiteral(literal) is not null) is { } evaluated)
            {
                return Expression.LiteralInstance(evaluated);
            }
        }

        switch (expression)
        {
            case Expression.Literal literal:

                if (replaceLiteral(literal) is { } replacement)
                {
                    return replacement;
                }

                // ["Litral", value]
                return
                    EncodeLitral(Expression.LiteralInstance(literal.Value));

            case Expression.Environment:

                // ["Environment"]
                return EncodeEnvironment();

            case Expression.List list:
                {
                    var encodedItems = new Expression[list.Items.Count];

                    for (var i = 0; i < list.Items.Count; i++)
                    {
                        encodedItems[i] = encodedChild(list.Items[i]);
                    }

                    // ["List", item0_encoded, item1_encoded, ...]
                    return EncodeList(encodedItems);
                }

            case Expression.ParseAndEval parseAndEval:

                // ["Eval", encoded_encoded, environment_encoded]
                return
                    EncodeEval(
                        encodedChild(parseAndEval.Encoded),
                        encodedChild(parseAndEval.Environment));

            default:
                throw new System.NotSupportedException(
                    "EncodeExpressionAsTemplateProducer does not support " + expression.GetType().Name);
        }
    }

    private static PineValue? TryEvaluateIndependentExpression(
        Expression expression,
        System.Func<Expression, bool> abortForNode)
    {
        return null;

        if (expression.ReferencesEnvironment)
            throw new System.ArgumentException("Expression is not independent: " + expression);

        if (expression.EvalCount > 0)
            return null;

        if (abortForNode(expression))
            return null;

        if (expression is Expression.Literal literal)
        {
            return literal.Value;
        }

        if (expression is Expression.List list)
        {
            var evaluatedItems = new PineValue[list.Items.Count];

            for (var i = 0; i < list.Items.Count; i++)
            {
                var itemValue = TryEvaluateIndependentExpression(list.Items[i], abortForNode);

                if (itemValue is null)
                    return null;

                evaluatedItems[i] = itemValue;
            }

            return PineValue.List(evaluatedItems);
        }

        if (expression is Expression.Conditional conditional)
        {
            var conditionValue =
                TryEvaluateIndependentExpression(conditional.Condition, abortForNode);

            if (conditionValue is null)
                return null;

            if (conditionValue == PineKernelValues.TrueValue)
                return TryEvaluateIndependentExpression(conditional.TrueBranch, abortForNode);

            return TryEvaluateIndependentExpression(conditional.FalseBranch, abortForNode);
        }

        if (expression.ReferencesEnvironment)
        {
            throw new System.ArgumentException(
                "Expression is not independent: " + expression);
        }

        return null;
    }

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
            function: nameof(BuiltinFunction.concat),
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
