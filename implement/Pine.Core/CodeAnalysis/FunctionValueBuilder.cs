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
    private static readonly PineValue s_parseAndEvalTagValue =
        StringEncoding.ValueFromString("ParseAndEval");

    private static readonly Expression s_parseAndEvalTagExpr =
        Expression.LiteralInstance(s_parseAndEvalTagValue);

    private static readonly PineValue s_literalTagValue =
        StringEncoding.ValueFromString("Literal");

    private static readonly Expression s_literalTagExpr =
        Expression.LiteralInstance(s_literalTagValue);

    private static readonly PineValue s_listTagValue =
        StringEncoding.ValueFromString("List");

    private static readonly Expression s_listTagExpr =
        Expression.LiteralInstance(s_listTagValue);

    private static readonly PineValue s_environmentTagValue =
        StringEncoding.ValueFromString("Environment");

    private static readonly Expression s_environmentTagExpr =
        Expression.LiteralInstance(s_environmentTagValue);

    private static readonly Expression s_kernelApplicationTag =
        Expression.LiteralInstance(StringEncoding.ValueFromString("KernelApplication"));

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

        // Build the innermost (final) application expression:
        //   ParseAndEval(Literal(innerExprEncoded), List([slot_0, ..., slot_n]))
        //
        // The inner expression is evaluated with an environment of [envFunctions, arg_0, ..., arg_{n-1}],
        // i.e. the argument values start at index 1 (index 0 holds the list of env functions).
        // The final application's environment is the last argument, so slot_n is Environment,
        // while slot_{1+i} for i < n-1 is a placeholder that the producer at stage i fills with the
        // captured value of arg_i.
        Expression current =
            BuildFinalApplicationWithPlaceholders(
                Expression.LiteralInstance(innerExprEncoded),
                envFunctions is null || envFunctions.Count is 0
                ?
                Expression.EmptyList
                :
                Expression.LiteralInstance(PineValue.List([.. envFunctions])),
                parameterCount,
                out var placeholderSentinels);

        // Fold from the innermost stage outward. At stage k (from n-2 down to 0), wrap `current` into
        // a producer that, when evaluated with the current argument, yields the encoding of the next
        // stage's expression. The placeholder for arg_k is replaced by a dynamic producer that reads
        // the current argument from the environment and wraps it in a single "Literal" encoding layer.
        // When arg_k is applied (at stage k), this producer is evaluated and its result is substituted
        // directly into the next stage's expression as Literal(arg_k); all later stages then carry that
        // Literal(arg_k) along as a constant. Every other part is quoted as a constant and carried along.
        for (var k = parameterCount - 2; k >= 0; k--)
        {
            var dynamicArgProducer =
                BuildWrappedLiteralProducer(layerCount: 1);

            current =
                EncodeExpressionAsTemplateProducer(
                    current,
                    placeholderSentinels[k],
                    dynamicArgProducer);
        }

        return ExpressionEncoding.EncodeExpressionAsValue(current, encodeExprCache);
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
        var innerExprEncoded =
            ExpressionEncoding.EncodeExpressionAsValue(innerExpression, encodeExprCache);

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
        // The encoded body and env-functions list are available as static values, so embed them directly:
        // the encoded body becomes the literal in the final application's "encoded" slot, and the
        // env-functions list (or an empty list when none) becomes the literal at environment index 0.
        var encodedBodySlot = Expression.LiteralInstance(innerExprEncoded);

        Expression envFunctionsSlot =
            envFunctions is null || envFunctions.Count is 0
            ?
            Expression.EmptyList
            :
            Expression.LiteralInstance(PineValue.List([.. envFunctions]));

        return
            BuildCurriedFunctionTemplateWithLeadingArgs(
                encodedBodySlot,
                envFunctionsSlot,
                parameterCount,
                leadingArgExpressions,
                additionalPlaceholderReplacements: null);
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
        // The encoded body and env-functions list are not static values; they are produced at runtime by the
        // given expressions. Mark their slots with dedicated placeholder sentinels and wrap each runtime
        // value in a single "Literal" encoding layer so that, in the produced function value, the encoded
        // body appears in the final application's "encoded" slot and the env-functions list appears at
        // environment index 0 - both as Literal nodes, identical in shape to the static variant.
        var encodedBodySlot =
            Expression.LiteralInstance(s_templateEncodedBodyPlaceholderSentinel);

        var envFunctionsSlot =
            Expression.LiteralInstance(s_templateEnvFunctionsPlaceholderSentinel);

        var additionalPlaceholderReplacements =
            new Dictionary<PineValue, Expression>(2)
            {
                [s_templateEncodedBodyPlaceholderSentinel] =
                    WrapInSingleLiteralLayer(innerExprEncodedExpression),

                [s_templateEnvFunctionsPlaceholderSentinel] =
                    WrapInSingleLiteralLayer(envFunctionsExpression),
            };

        return
            BuildCurriedFunctionTemplateWithLeadingArgs(
                encodedBodySlot,
                envFunctionsSlot,
                parameterCount,
                leadingArgExpressions,
                additionalPlaceholderReplacements);
    }

    /// <summary>
    /// Shared implementation behind <see cref="EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBody"/>
    /// and <see cref="EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBodyExpression"/>. Builds the
    /// compact curried template that nests only the remaining parameters and embeds the already-provided
    /// (leading) arguments directly.
    /// </summary>
    /// <param name="encodedBodySlot">
    /// Expression placed in the final application's "encoded" slot. Either a literal of the static encoded
    /// body, or a placeholder sentinel replaced via <paramref name="additionalPlaceholderReplacements"/>.
    /// </param>
    /// <param name="envFunctionsSlot">
    /// Expression placed at environment index 0. Either a literal (or empty list) of the static
    /// env-functions list, or a placeholder sentinel replaced via
    /// <paramref name="additionalPlaceholderReplacements"/>.
    /// </param>
    /// <param name="parameterCount">Total number of parameters expected by the function.</param>
    /// <param name="leadingArgExpressions">Expressions for the leading arguments already provided.</param>
    /// <param name="additionalPlaceholderReplacements">
    /// Optional replacements for the body/env-functions placeholder sentinels (used by the expression-based
    /// variant); <see langword="null"/> when the body and env-functions are embedded as static literals.
    /// </param>
    private static Expression BuildCurriedFunctionTemplateWithLeadingArgs(
        Expression encodedBodySlot,
        Expression envFunctionsSlot,
        int parameterCount,
        IReadOnlyList<Expression> leadingArgExpressions,
        IReadOnlyDictionary<PineValue, Expression>? additionalPlaceholderReplacements)
    {
        var leadingCount = leadingArgExpressions.Count;

        if (parameterCount < 1)
        {
            throw new System.ArgumentException(
                "parameterCount must be at least 1", nameof(parameterCount));
        }

        if (leadingCount < 0 || leadingCount >= parameterCount)
        {
            throw new System.ArgumentException(
                "leadingArgExpressions count must be in range [0, parameterCount - 1]",
                nameof(leadingArgExpressions));
        }

        // Build the final application's slots exactly like the plain template builder does, so that the
        // structure of the value we emit matches the structure obtained by applying the leading arguments
        // to the plain template. The leading argument slots keep their placeholder sentinels here and are
        // substituted with the embedded argument expressions at the end.
        Expression current =
            BuildFinalApplicationWithPlaceholders(
                encodedBodySlot,
                envFunctionsSlot,
                parameterCount,
                out var placeholderSentinels);

        // Fold only the stages for the REMAINING parameters (indices [leadingCount, parameterCount - 2]),
        // leaving the leading parameters' placeholders in place. This is exactly the structure that remains
        // after applying the leading arguments to the plain template.
        for (var k = parameterCount - 2; k >= leadingCount; k--)
        {
            var dynamicArgProducer =
                BuildWrappedLiteralProducer(layerCount: 1);

            current =
                EncodeExpressionAsTemplateProducer(
                    current,
                    placeholderSentinels[k],
                    dynamicArgProducer);
        }

        // Encode the remaining-stage structure into the function value, substituting each leading
        // placeholder with its already available argument expression wrapped in a single "Literal" encoding
        // layer. The wrapped form ensures that, in the produced value, the leading argument value appears as
        // a Literal in the final application's environment - identical to what applying the argument to the
        // plain template would have produced. Any body/env-functions placeholders are substituted at the
        // same time via the additional replacements.
        var replacements =
            new Dictionary<PineValue, Expression>(
                leadingCount + (additionalPlaceholderReplacements?.Count ?? 0));

        for (var j = 0; j < leadingCount; j++)
        {
            replacements[placeholderSentinels[j]] =
                WrapInSingleLiteralLayer(leadingArgExpressions[j]);
        }

        if (additionalPlaceholderReplacements is not null)
        {
            foreach (var replacement in additionalPlaceholderReplacements)
            {
                replacements[replacement.Key] = replacement.Value;
            }
        }

        return EncodeExpressionAsTemplateProducer(current, replacements);
    }

    /// <summary>
    /// Wraps <paramref name="valueProducer"/> in a single <c>Literal</c> encoding layer, producing the
    /// expression <c>["Literal", [valueProducer]]</c>. In a template producer this makes the runtime value
    /// of <paramref name="valueProducer"/> appear as a <see cref="Expression.Literal"/> node in the produced
    /// (encoded) expression value.
    /// </summary>
    private static Expression WrapInSingleLiteralLayer(Expression valueProducer) =>
        Expression.ListInstance(
            [
            s_literalTagExpr,
            Expression.ListInstance([valueProducer])
            ]);

    /// <summary>
    /// Builds the innermost (final) application expression shared by the template builders:
    /// <c>ParseAndEval(encodedBodySlot, List([envFunctionsSlot, slot_0, ..., slot_{n-2}, Environment]))</c>.
    /// The inner expression is evaluated with an environment of <c>[envFunctions, arg_0, ..., arg_{n-1}]</c>;
    /// index 0 holds <paramref name="envFunctionsSlot"/> (the env functions list, empty when none), the
    /// final slot is the environment (the last argument), and each intermediate slot is a unique placeholder
    /// sentinel that callers replace with the captured/embedded value of the corresponding argument. The
    /// sentinels (one per non-final parameter) are returned via <paramref name="placeholderSentinels"/>.
    /// </summary>
    private static Expression BuildFinalApplicationWithPlaceholders(
        Expression encodedBodySlot,
        Expression envFunctionsSlot,
        int parameterCount,
        out PineValue[] placeholderSentinels)
    {
        placeholderSentinels = new PineValue[parameterCount - 1];

        var finalSlots = new Expression[parameterCount + 1];

        // Index 0 holds the list of env functions (an empty list when none are provided).
        finalSlots[0] = envFunctionsSlot;

        for (var i = 0; i < parameterCount - 1; i++)
        {
            var sentinel =
                PineValue.List(
                    [
                    s_templateArgPlaceholderMarker,
                    IntegerEncoding.EncodeSignedInteger(i)
                    ]);

            placeholderSentinels[i] = sentinel;
            finalSlots[1 + i] = Expression.LiteralInstance(sentinel);
        }

        finalSlots[parameterCount] = Expression.EnvironmentInstance;

        return
            new Expression.ParseAndEval(
                encoded: encodedBodySlot,
                environment: Expression.ListInstance(finalSlots));
    }

    /// <summary>
    /// Builds an expression that produces the value <c>["Literal", [ ... ["Literal", [Environment]] ... ]]</c>
    /// with <paramref name="layerCount"/> nested <c>Literal</c> encoding layers around the current
    /// environment value. With <paramref name="layerCount"/> equal to zero this is just the environment.
    /// </summary>
    private static Expression BuildWrappedLiteralProducer(int layerCount)
    {
        Expression result = Expression.EnvironmentInstance;

        for (var i = 0; i < layerCount; i++)
        {
            // Produce ["Literal", [<result>]]
            result =
                Expression.ListInstance(
                    [
                    s_literalTagExpr,
                    Expression.ListInstance([result])
                    ]);
        }

        return result;
    }

    /// <summary>
    /// Builds an expression that, when evaluated, produces the encoding of <paramref name="expression"/>.
    /// Occurrences of a <see cref="Expression.Literal"/> whose value equals
    /// <paramref name="placeholderSentinel"/> are replaced by <paramref name="placeholderReplacement"/>
    /// instead of being quoted as a constant. The encoding is built structurally (descending into all
    /// subexpressions) so that any other placeholder literals remain reachable as leaves for later folds.
    /// </summary>
    private static Expression EncodeExpressionAsTemplateProducer(
        Expression expression,
        PineValue placeholderSentinel,
        Expression placeholderReplacement)
    {
        return
            EncodeExpressionAsTemplateProducerViaPostOrder(
                expression,
                literal =>
                literal.Value.Equals(placeholderSentinel) ? placeholderReplacement : null);
    }

    /// <summary>
    /// Multi-placeholder variant of
    /// <see cref="EncodeExpressionAsTemplateProducer(Expression, PineValue, Expression)"/>.
    /// Builds an expression that, when evaluated, produces the encoding of <paramref name="expression"/>,
    /// replacing each <see cref="Expression.Literal"/> whose value is a key in
    /// <paramref name="placeholderReplacements"/> with the corresponding replacement expression instead of
    /// quoting it as a constant. When <paramref name="placeholderReplacements"/> is empty this is a plain
    /// structural encoder producing the same value as
    /// <see cref="ExpressionEncoding.EncodeExpressionAsValue(Expression, PineExpressionEncodingCache)"/>.
    /// </summary>
    private static Expression EncodeExpressionAsTemplateProducer(
        Expression expression,
        IReadOnlyDictionary<PineValue, Expression> placeholderReplacements)
    {
        return
            EncodeExpressionAsTemplateProducerViaPostOrder(
                expression,
                literal =>
                placeholderReplacements.TryGetValue(literal.Value, out var replacement) ? replacement : null);
    }

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
        System.Func<Expression.Literal, Expression?> replaceLiteral)
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

            store[current] =
                EncodeTemplateNode(current, replaceLiteral, child => store[child]);
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
        System.Func<Expression, Expression> encodedChild)
    {
        switch (expression)
        {
            case Expression.Literal literal:

                if (replaceLiteral(literal) is { } replacement)
                {
                    return replacement;
                }

                // ["Literal", [value]]
                return
                    Expression.ListInstance(
                        [
                        s_literalTagExpr,
                        Expression.ListInstance([Expression.LiteralInstance(literal.Value)])
                        ]);

            case Expression.Environment:

                // ["Environment", []]
                return Expression.ListInstance([s_environmentTagExpr, Expression.EmptyList]);

            case Expression.List list:
                {
                    var encodedItems = new Expression[list.Items.Count];

                    for (var i = 0; i < list.Items.Count; i++)
                    {
                        encodedItems[i] = encodedChild(list.Items[i]);
                    }

                    // ["List", [[item0_encoded, item1_encoded, ...]]]
                    return
                        Expression.ListInstance(
                            [
                            s_listTagExpr,
                            Expression.ListInstance([Expression.ListInstance(encodedItems)])
                            ]);
                }

            case Expression.ParseAndEval parseAndEval:

                // ["ParseAndEval", [encoded_encoded, environment_encoded]]
                return
                    Expression.ListInstance(
                        [
                        s_parseAndEvalTagExpr,
                        Expression.ListInstance(
                            [
                            encodedChild(parseAndEval.Encoded),
                            encodedChild(parseAndEval.Environment)
                            ])
                        ]);

            default:
                throw new System.NotSupportedException(
                    "EncodeExpressionAsTemplateProducer does not support " + expression.GetType().Name);
        }
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
    /// Builds the wrapper level encoding directly. This produces the same result as
    /// EncodeExpressionAsListExpression(BuildWrapperExpressionForLevel(...)) but without
    /// encoding the nextLevelEncodingExpr - it's embedded directly.
    /// </summary>
    private static Expression BuildWrapperLevelEncodingDirect(Expression nextLevelEncodingExpr, int level)
    {
        // The wrapper expression structure is:
        // List([parseAndEvalTag, List([nextLevelLiteral, envStructure])])
        //
        // When encoded, this becomes:
        // ["List", [[["Literal", ["ParseAndEval"]], ["List", [[nextLevelLiteralEnc, envStructureEnc]]]]]]
        //
        // nextLevelLiteralEnc = ["List", [[["Literal", ["Literal"]], ["List", [[nextLevelEncodingExpr]]]]]]
        // Note: nextLevelEncodingExpr is wrapped in ["List", [[...]]] not ["Literal", [...]] because
        // it's already an expression that produces an encoding value, not a value to be embedded.

        // Build nextLevelLiteral encoding
        // Original: List([literalTag, List([Literal(nextLevelEncoded)])])
        // What we want: the encoding of this, where Literal(nextLevelEncoded) becomes the result of nextLevelEncodingExpr
        //
        // encode(List([literalTag, List([Literal(nextLevelEncoded)])])) =
        //   ["List", [[encode(literalTag), encode(List([Literal(nextLevelEncoded)]))]]]
        //   = ["List", [[["Literal", ["Literal"]], ["List", [[encode(Literal(nextLevelEncoded))]]]]]]
        //   = ["List", [[["Literal", ["Literal"]], ["List", [[["Literal", [nextLevelEncoded]]]]]]]
        //
        // To make nextLevelEncoded dynamic, we need to wrap it so nextLevelEncodingExpr produces the value:
        //   ["List", [[["Literal", ["Literal"]], ["List", [[["Literal", [EVAL(nextLevelEncodingExpr)]]]]]]]
        //
        // The ["Literal", [EVAL(nextLevelEncodingExpr)]] is achieved by: ["Literal", [nextLevelEncodingExpr]]
        // which evaluates nextLevelEncodingExpr at overall expr time.

        // ["Literal", ["Literal"]]
        var literalTagEncoding =
            Expression.ListInstance(
                [
                s_literalTagExpr,
                Expression.ListInstance([s_literalTagExpr])
                ]);

        // ["Literal", [nextLevelEncodingExpr]] - evaluates to ["Literal", [nextLevelValue]]
        var nextLevelValueLiteralEncoding =
            Expression.ListInstance(
                [
                s_literalTagExpr,
                Expression.ListInstance([nextLevelEncodingExpr])
                ]);

        // ["List", [[nextLevelValueLiteralEncoding]]]
        var nextLevelValueListEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([nextLevelValueLiteralEncoding])])
                ]);

        // ["List", [[literalTagEncoding, nextLevelValueListEncoding]]]
        var nextLevelLiteralEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([literalTagEncoding, nextLevelValueListEncoding])])
                ]);

        // Build envStructure encoding based on level
        Expression envStructureEncoding;

        if (level is 0)
        {
            envStructureEncoding = s_level0EnvStructureEncodingStatic;
        }
        else
        {
            envStructureEncoding = s_levelNEnvStructureEncodingStatic;
        }

        // ["Literal", ["ParseAndEval"]]
        var parseAndEvalTagEncoding =
            Expression.ListInstance(
                [
                s_literalTagExpr,
                Expression.ListInstance([s_parseAndEvalTagExpr])
                ]);

        // ["List", [[nextLevelLiteralEncoding, envStructureEncoding]]]
        var innerListEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([nextLevelLiteralEncoding, envStructureEncoding])])
                ]);

        // ["List", [[parseAndEvalTagEncoding, innerListEncoding]]]
        return
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([parseAndEvalTagEncoding, innerListEncoding])])
                ]);
    }

    /// <summary>
    /// Builds the static encoding of level 0 env structure.
    /// At level 0 runtime, env = arg0, so envStructure = ["List", [[["Literal", [[arg0]]], ["Environment", []]]]]
    /// </summary>
    private static Expression BuildLevel0EnvStructureEncodingStatic()
    {
        // envStructure expression: List([listTag, List([List([capturedLiteral, envLiteral])])])
        // where capturedLiteral = List([literalTag, List([List([env])])])
        //       envLiteral = List([envTag, List([])])
        //
        // We encode this statically since it doesn't depend on envFuncExprs

        // Build capturedLiteral encoding
        // encode(List([literalTag, List([List([env])])])) = 
        //   ["List", [[["Literal", ["Literal"]], ["List", [[["List", [[["Environment", []]]]]]]]]]

        var literalTagEncoding =
            Expression.ListInstance(
                [
                s_literalTagExpr,
                Expression.ListInstance([s_literalTagExpr])
                ]);

        // encode(env) = ["Environment", []]
        var environmentEncoding =
            Expression.ListInstance(
                [
                s_environmentTagExpr,
                Expression.EmptyList
                ]);

        // encode(List([env])) = ["List", [[["Environment", []]]]]
        var listOfEnvEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([environmentEncoding])])
                ]);

        // encode(List([List([env])])) = ["List", [[listOfEnvEncoding]]]
        var listOfListOfEnvEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([listOfEnvEncoding])])
                ]);

        // encode(capturedLiteral) = ["List", [[literalTagEncoding, listOfListOfEnvEncoding]]]
        var capturedLiteralEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([literalTagEncoding, listOfListOfEnvEncoding])])
                ]);

        // Build envLiteral encoding
        // encode(List([envTag, List([])])) = ["List", [[["Literal", ["Environment"]], ["List", [[]]]]]]

        var envTagEncoding =
            Expression.ListInstance(
                [
                s_literalTagExpr,
                Expression.ListInstance([s_environmentTagExpr])
                ]);

        var emptyListEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.EmptyList])
                ]);

        var envLiteralEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([envTagEncoding, emptyListEncoding])])
                ]);

        // Build innerList encoding
        // encode(List([List([capturedLiteral, envLiteral])])) = ["List", [[["List", [[capturedEnc, envEnc]]]]]]

        var innerPairEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([capturedLiteralEncoding, envLiteralEncoding])])
                ]);

        var innerListEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([innerPairEncoding])])
                ]);

        // Build listTag encoding
        var listTagEncoding =
            Expression.ListInstance(
                [
                s_literalTagExpr,
                Expression.ListInstance([s_listTagExpr])
                ]);

        // encode(envStructure) = ["List", [[listTagEncoding, innerListEncoding]]]
        return
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([listTagEncoding, innerListEncoding])])
                ]);
    }

    /// <summary>
    /// Builds the static encoding of level N > 0 env structure.
    /// At level N runtime, env = [[captured], arg], so envStructure captures newCaptured = concat(env[0], [env[1]]).
    /// </summary>
    private static Expression BuildLevelNEnvStructureEncodingStatic()
    {
        // newCaptured = concat(env[0], [env[1]])
        var capturedSoFarExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance);

        var currentArgExpr = ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance);
        var newCapturedExpr = BuiltinAppConcatBinary(capturedSoFarExpr, Expression.ListInstance([currentArgExpr]));

        // Use standard encoding for newCapturedExpr - it doesn't contain envFuncExprs, only references
        // to the level N environment (captured so far and current arg), which are correctly evaluated
        // when level N runs.
        var newCapturedEncoding = EncodeExpressionAsListExpression(newCapturedExpr);

        // capturedLiteral = List([literalTag, List([newCaptured])])
        // encode: ["List", [[["Literal", ["Literal"]], ["List", [[newCapturedEncoding]]]]]]

        var literalTagEncoding =
            Expression.ListInstance(
                [
                s_literalTagExpr,
                Expression.ListInstance([s_literalTagExpr])
                ]);

        var newCapturedListEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([newCapturedEncoding])])
                ]);

        var capturedLiteralEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([literalTagEncoding, newCapturedListEncoding])])
                ]);

        // envLiteral encoding (same as level 0)
        var envTagEncoding =
            Expression.ListInstance(
                [
                s_literalTagExpr,
                Expression.ListInstance([s_environmentTagExpr])
                ]);

        var emptyListEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.EmptyList])
                ]);

        var envLiteralEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([envTagEncoding, emptyListEncoding])])
                ]);

        // innerList encoding
        var innerPairEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([capturedLiteralEncoding, envLiteralEncoding])])
                ]);

        var innerListEncoding =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([innerPairEncoding])])
                ]);

        // listTag encoding
        var listTagEncoding =
            Expression.ListInstance(
                [
                s_literalTagExpr,
                Expression.ListInstance([s_listTagExpr])
                ]);

        // envStructure encoding
        return
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([listTagEncoding, innerListEncoding])])
                ]);
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
                s_literalTagExpr,
                Expression.ListInstance([Expression.LiteralInstance(literal.Value)])
                ]),

            Expression.Environment =>
            // ["Environment", []]
            Expression.ListInstance([s_environmentTagExpr, Expression.EmptyList]),

            Expression.List list =>
            // ["List", [[item0_encoded, item1_encoded, ...]]]
            EncodeListExpressionAsListExpression(list),

            Expression.ParseAndEval parseAndEval =>
            // ["ParseAndEval", [encoded_encoded, environment_encoded]]
            Expression.ListInstance(
                [
                s_parseAndEvalTagExpr,
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
                s_kernelApplicationTag,
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

        return
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance(encodedItems)])
                ]);
    }

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
                Expression.LiteralInstance(ExpressionEncoding.EncodeExpressionAsValue(innerExpression, encodeExprCache)),
                environment: invocationEnv);

        return ExpressionEncoding.EncodeExpressionAsValue(invocationExpr, encodeExprCache);
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
                Expression.LiteralInstance(ExpressionEncoding.EncodeExpressionAsValue(innerExpression, encodeExprCache)),
                environment: invocationEnv);

        return ExpressionEncoding.EncodeExpressionAsValue(invocationExpr, encodeExprCache);
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
        PineValue currentEncoded = ExpressionEncoding.EncodeExpressionAsValue(innermostExpr, encodeExprCache);

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
                Expression.LiteralInstance(ExpressionEncoding.EncodeExpressionAsValue(innerExpression, encodeExprCache)),
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
                s_literalTagExpr,
                Expression.ListInstance([Expression.LiteralInstance(nextLevelEncoded)])
                ]);

        // Encode Environment expression: ["Environment", []]
        var environmentEncoded =
            Expression.ListInstance(
                [
                s_environmentTagExpr,
                Expression.EmptyList
                ]);

        // Encode captured args [arg0]:
        // At this level, env = arg0 directly
        // We need to build Literal([env]) which when evaluated produces [arg0]
        // capturedArgsEncoded = ["Literal", [[env]]]
        var capturedArgsEncoded =
            Expression.ListInstance(
                [
                s_literalTagExpr,
                Expression.ListInstance([Expression.ListInstance([Expression.EnvironmentInstance])])
                ]);

        // Encode the full env structure: ["List", [[capturedArgsEncoded, environmentEncoded]]]
        // This produces [[captured_args], next_env] when evaluated
        var envStructureEncoded =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([capturedArgsEncoded, environmentEncoded])])
                ]);

        // Final ParseAndEval encoding: ["ParseAndEval", [nextLevelLiteralEncoded, envStructureEncoded]]
        return
            Expression.ListInstance(
                [
                s_parseAndEvalTagExpr,
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
                s_literalTagExpr,
                Expression.ListInstance([Expression.LiteralInstance(nextLevelEncoded)])
                ]);

        // Encode Environment for the returned expression
        var environmentEncoded =
            Expression.ListInstance(
                [s_environmentTagExpr, Expression.EmptyList]);

        // Encode captured as literal: ["Literal", [newCaptured]]
        // Note: newCapturedExpr is evaluated at THIS level's eval time
        // This produces [new_captured] when evaluated
        var capturedLiteralEncoded =
            Expression.ListInstance(
                [
                s_literalTagExpr,
                Expression.ListInstance([newCapturedExpr])
                ]);

        // Encode env structure: ["List", [[capturedLiteralEncoded, environmentEncoded]]]
        // This produces [[new_captured], next_env] when evaluated
        var envStructureEncoded =
            Expression.ListInstance(
                [
                s_listTagExpr,
                Expression.ListInstance([Expression.ListInstance([capturedLiteralEncoded, environmentEncoded])])
                ]);

        // Final: ["ParseAndEval", [nextLevelLiteralEncoded, envStructureEncoded]]
        return
            Expression.ListInstance(
                [
                s_parseAndEvalTagExpr,
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
