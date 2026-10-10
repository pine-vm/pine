using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;

namespace Pine.Core.Interpreter;


/// <summary>
/// A minimal, direct interpreter for Pine <see cref="Expression"/> trees.
/// Evaluates expressions by recursive traversal without an intermediate representation or compilation step.
/// <para>
/// Optionally caches results of <see cref="Expression.Eval"/> evaluations keyed by
/// (<see cref="EvalCacheEntryKey.ExprValue"/>, <see cref="EvalCacheEntryKey.EnvValue"/>) pairs.
/// </para>
/// </summary>
public class DirectInterpreter : IPineVM
{
    private readonly DirectInterpreterCounters _counters;

    /// <summary>Work performed by this interpreter since construction, including recursive evaluations.</summary>
    public IntermediateVM.PerformanceCounters Counters => _counters.Snapshot();

    private readonly PineVMParseCache _parseCache;

    private readonly IDictionary<EvalCacheEntryKey, PineValue>? _evalCache;

    private DirectInterpreter(
        PineVMParseCache parseCache,
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache,
        DirectInterpreterCounters? counters = null)
    {
        _counters = counters ?? new DirectInterpreterCounters();
        _parseCache = parseCache;
        _evalCache = evalCache;
    }

    /// <summary>
    /// Creates an interpreter with an evaluation cache owned by this interpreter instance.
    /// </summary>
    public static DirectInterpreter WithLocalEvalCache(PineVMParseCache parseCache) =>
        new(parseCache, new Dictionary<EvalCacheEntryKey, PineValue>());

    /// <summary>
    /// Creates an interpreter that uses the supplied evaluation cache.
    /// The caller must synchronize access when sharing the cache across concurrent evaluations.
    /// </summary>
    public static DirectInterpreter WithSharedEvalCache(
        PineVMParseCache parseCache,
        IDictionary<EvalCacheEntryKey, PineValue> evalCache) =>
        new(parseCache, evalCache);

    /// <summary>
    /// Creates an interpreter without an evaluation cache.
    /// </summary>
    public static DirectInterpreter WithoutEvalCaching(PineVMParseCache parseCache) =>
        new(parseCache, evalCache: null);

    internal static DirectInterpreter WithLocalEvalCache(
        PineVMParseCache parseCache,
        DirectInterpreterCounters counters) =>
        new(parseCache, new Dictionary<EvalCacheEntryKey, PineValue>(), counters);

    internal static DirectInterpreter WithoutEvalCaching(
        PineVMParseCache parseCache,
        DirectInterpreterCounters counters) =>
        new(parseCache, evalCache: null, counters);

    /// <summary>
    /// Key type for the evaluation cache, combining the encoded expression value and the environment value.
    /// </summary>
    public record struct EvalCacheEntryKey(
        PineValue ExprValue,
        PineValue EnvValue);

    /// <summary>Enters direct evaluation, counting one invocation independently of recursive tree depth.</summary>
    public PineValue EvaluateExpressionDefault(Expression expression, PineValue environment)
    {
        _counters.InvocationCount++;
        return EvaluateExpressionDefaultCore(expression, environment);
    }

    /// <summary>Enters direct evaluation, counting one invocation independently of recursive tree depth.</summary>
    public PineValue EvaluateListExpression(Expression.List listExpression, PineValue environment)
    {
        _counters.InvocationCount++;
        return EvaluateListExpressionCore(listExpression, environment);
    }

    /// <summary>Enters direct evaluation, counting one invocation independently of recursive tree depth.</summary>
    public PineValue EvaluateParseAndEvalExpression(Expression.Eval parseAndEval, PineValue environment)
    {
        _counters.InvocationCount++;
        return EvaluateParseAndEvalExpressionCore(parseAndEval, environment);
    }

    /// <summary>Enters direct evaluation, counting one invocation independently of recursive tree depth.</summary>
    public PineValue EvaluateBuiltinExpression(PineValue environment, Expression.Builtin application)
    {
        _counters.InvocationCount++;
        return EvaluateBuiltinExpressionCore(environment, application);
    }

    /// <summary>Enters direct evaluation, counting one invocation independently of recursive tree depth.</summary>
    public PineValue EvaluateBuiltinExpressionGeneric(PineValue environment, Expression.Builtin application)
    {
        _counters.InvocationCount++;
        _counters.ExpressionCount++;
        _counters.BuiltinCount++;
        return EvaluateBuiltinExpressionGenericCore(environment, application);
    }

    /// <summary>Enters direct evaluation, counting one invocation independently of recursive tree depth.</summary>
    public PineValue EvaluateConditionalExpression(PineValue environment, Expression.Conditional conditional)
    {
        _counters.InvocationCount++;
        return EvaluateConditionalExpressionCore(environment, conditional);
    }

    /// <summary>
    /// Evaluates a Pine <see cref="Expression"/> in the given environment, returning the resulting <see cref="PineValue"/>.
    /// Dispatches to specialized methods based on the expression type.
    /// </summary>
    private PineValue EvaluateExpressionDefaultCore(
        Expression expression,
        PineValue environment)
    {
        if (expression is Expression.Litral literalExpression)
        {
            _counters.ExpressionCount++;
            _counters.LiteralCount++;
            return literalExpression.Value;
        }

        if (expression is Expression.List listExpression)
        {
            return EvaluateListExpressionCore(listExpression, environment);
        }

        if (expression is Expression.Eval applicationExpression)
        {
            return
                EvaluateParseAndEvalExpressionCore(
                    applicationExpression,
                    environment);
        }

        if (expression is Expression.Builtin builtinExpression)
        {
            return
                EvaluateBuiltinExpressionCore(
                    environment,
                    builtinExpression);
        }

        if (expression is Expression.Conditional conditionalExpression)
        {
            return
                EvaluateConditionalExpressionCore(
                    environment,
                    conditionalExpression);
        }

        if (expression is Expression.Environment)
        {
            _counters.ExpressionCount++;
            _counters.EnvironmentCount++;
            return environment;
        }

        if (expression is Expression.Label stringTagExpression)
        {
            _counters.ExpressionCount++;

            return
                EvaluateExpressionDefaultCore(
                    stringTagExpression.Tagged,
                    environment);
        }

        throw new NotImplementedException(
            "Unexpected shape of expression: " + expression.GetType().FullName);
    }

    /// <summary>
    /// Evaluates a <see cref="Expression.List"/> expression by evaluating each item
    /// and collecting the results into a <see cref="PineValue.ListValue"/>.
    /// </summary>
    private PineValue EvaluateListExpressionCore(
        Expression.List listExpression,
        PineValue environment)
    {
        _counters.ExpressionCount++;
        _counters.ListCount++;

        _counters.BuildListItemCount += listExpression.Items.Count;
        var listItems = new PineValue[listExpression.Items.Count];

        for (var i = 0; i < listExpression.Items.Count; i++)
        {
            var item = listExpression.Items[i];

            var itemResult =
                EvaluateExpressionDefaultCore(
                    item,
                    environment);

            listItems[i] = itemResult;
        }

        return PineValue.List(listItems);
    }

    /// <summary>
    /// Evaluates a <see cref="Expression.Eval"/> expression:
    /// first evaluates the encoded expression and environment sub-expressions, then parses the
    /// encoded value into an <see cref="Expression"/> and evaluates it in the computed environment.
    /// Results may be cached when <c>evalCache</c> is provided.
    /// </summary>
    /// <exception cref="ParseExpressionException">Thrown when the encoded value cannot be parsed as a valid expression.</exception>
    private PineValue EvaluateParseAndEvalExpressionCore(
        Expression.Eval parseAndEval,
        PineValue environment)
    {
        _counters.ExpressionCount++;
        _counters.EvalCount++;

        var environmentValue =
            EvaluateExpressionDefaultCore(
                parseAndEval.Environment,
                environment);

        var expressionValue =
            EvaluateExpressionDefaultCore(
                parseAndEval.Encoded,
                environment);

        if (_evalCache is not null)
        {
            var cacheKey = new EvalCacheEntryKey(ExprValue: expressionValue, EnvValue: environmentValue);

            if (_evalCache.TryGetValue(cacheKey, out var fromCache))
            {
                return fromCache;
            }
        }

        var parseResult = _parseCache.ParseExpression(expressionValue);

        if (parseResult is Result<string, Expression>.Err parseErr)
        {
            var message =
                "Failed to parse expression from value: " + parseErr.Value +
                " - expressionValue is " + DescribeValueForErrorMessage(expressionValue) +
                " - environmentValue is " + DescribeValueForErrorMessage(environmentValue);

            throw new ParseExpressionException(message);
        }

        if (parseResult is not Result<string, Expression>.Ok parseOk)
        {
            throw new NotImplementedException("Unexpected result type: " + parseResult.GetType().FullName);
        }

        var result =
            EvaluateExpressionDefaultCore(
                environment: environmentValue,
                expression: parseOk.Value);

        if (_evalCache is not null)
        {
            var cacheKey = new EvalCacheEntryKey(ExprValue: expressionValue, EnvValue: environmentValue);

            _evalCache[cacheKey] = result;
        }

        return result;
    }

    /// <summary>
    /// Returns a short human-readable description of a <see cref="PineValue"/> for use in error messages.
    /// Attempts to decode the value as a string; falls back to "not a string" if decoding fails.
    /// </summary>
    public static string DescribeValueForErrorMessage(PineValue pineValue) =>
        StringEncoding.StringFromValue(pineValue)
        .Unpack(
            fromErr: _ => "not a string",
            fromOk: asString => "string \'" + asString + "\'");

    /// <summary>
    /// Evaluates a <see cref="Expression.Builtin"/> expression.
    /// Includes an optimized fast path for the common <c>head(skip(...))</c> pattern used for
    /// environment path access, falling back to the generic builtin function application.
    /// </summary>
    private PineValue EvaluateBuiltinExpressionCore(
        PineValue environment,
        Expression.Builtin application)
    {
        _counters.ExpressionCount++;
        _counters.BuiltinCount++;

        if (application.Function is nameof(BuiltinFunction.head) &&
            application.Input is Expression.Builtin innerBuiltinExpression)
        {
            if (innerBuiltinExpression.Function is nameof(BuiltinFunction.skip) &&
                innerBuiltinExpression.Input is Expression.List skipListExpr &&
                skipListExpr.Items.Count is 2)
            {
                var skipValue =
                    EvaluateExpressionDefaultCore(
                        skipListExpr.Items[0],
                        environment);

                if (BuiltinFunction.SignedIntegerFromValueRelaxed(skipValue) is { } skipCount)
                {
                    if (EvaluateExpressionDefaultCore(
                        skipListExpr.Items[1],
                        environment) is PineValue.ListValue list)
                    {
                        if (list.Items.Length < 1 || list.Items.Length <= skipCount)
                        {
                            return PineValue.EmptyList;
                        }

                        return list.Items.Span[skipCount < 0 ? 0 : (int)skipCount];
                    }
                    else
                    {
                        return PineValue.EmptyList;
                    }
                }
            }
        }

        return EvaluateBuiltinExpressionGenericCore(environment, application);
    }

    /// <summary>
    /// Evaluates a <see cref="Expression.Builtin"/> using the generic builtin function dispatch.
    /// Evaluates the input expression first, then applies the named builtin function.
    /// </summary>
    private PineValue EvaluateBuiltinExpressionGenericCore(
        PineValue environment,
        Expression.Builtin application)
    {
        var inputValue =
            EvaluateExpressionDefaultCore(application.Input, environment);

        return
            BuiltinFunction.ApplyFunctionGeneric(
                function: application.Function,
                inputValue: inputValue);
    }

    /// <summary>
    /// Evaluates a <see cref="Expression.Conditional"/> expression.
    /// Evaluates the condition first; if it equals <see cref="PineKernelValues.TrueValue"/>,
    /// evaluates and returns the true branch; otherwise evaluates and returns the false branch.
    /// </summary>
    private PineValue EvaluateConditionalExpressionCore(
        PineValue environment,
        Expression.Conditional conditional)
    {
        _counters.ExpressionCount++;
        _counters.ConditionalCount++;

        var conditionValue =
            EvaluateExpressionDefaultCore(
                conditional.Condition,
                environment);

        if (conditionValue == PineKernelValues.TrueValue)
        {
            return
                EvaluateExpressionDefaultCore(
                    conditional.TrueBranch,
                    environment);
        }

        return
            EvaluateExpressionDefaultCore(
                conditional.FalseBranch,
                environment);
    }

    /// <summary>
    /// Implements <see cref="IPineVM.EvaluateExpression"/> by delegating to <see cref="EvaluateExpressionDefault"/>.
    /// Returns <see cref="Result{TErr,TOk}.Ok"/> wrapping the evaluated value.
    /// Exceptions from <see cref="EvaluateExpressionDefault"/> (e.g., <see cref="ParseExpressionException"/>)
    /// propagate to the caller.
    /// </summary>
    public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment)
    {
        return
            EvaluateExpressionDefault(
                expression,
                environment);
    }
}
