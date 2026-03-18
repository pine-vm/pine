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
/// Optionally caches results of <see cref="Expression.ParseAndEval"/> evaluations keyed by
/// (<see cref="EvalCacheEntryKey.ExprValue"/>, <see cref="EvalCacheEntryKey.EnvValue"/>) pairs.
/// </para>
/// </summary>
/// <param name="parseCache">Cache for parsing encoded expression values.</param>
/// <param name="evalCache">Optional cache for memoizing ParseAndEval results. Pass <c>null</c> to disable caching.</param>
public class DirectInterpreter(
    PineVMParseCache parseCache,
    IDictionary<DirectInterpreter.EvalCacheEntryKey, PineValue>? evalCache) : IPineVM
{
    /// <summary>
    /// Key type for the evaluation cache, combining the encoded expression value and the environment value.
    /// </summary>
    public record struct EvalCacheEntryKey(
        PineValue ExprValue,
        PineValue EnvValue);

    /// <summary>
    /// Evaluates a Pine <see cref="Expression"/> in the given environment, returning the resulting <see cref="PineValue"/>.
    /// Dispatches to specialized methods based on the expression type.
    /// </summary>
    public PineValue EvaluateExpressionDefault(
        Expression expression,
        PineValue environment)
    {
        if (expression is Expression.Literal literalExpression)
            return literalExpression.Value;

        if (expression is Expression.List listExpression)
        {
            return EvaluateListExpression(listExpression, environment);
        }

        if (expression is Expression.ParseAndEval applicationExpression)
        {
            return
                EvaluateParseAndEvalExpression(
                    applicationExpression,
                    environment);
        }

        if (expression is Expression.KernelApplication kernelApplicationExpression)
        {
            return
                EvaluateKernelApplicationExpression(
                    environment,
                    kernelApplicationExpression);
        }

        if (expression is Expression.Conditional conditionalExpression)
        {
            return
                EvaluateConditionalExpression(
                    environment,
                    conditionalExpression);
        }

        if (expression is Expression.Environment)
        {
            return environment;
        }

        if (expression is Expression.StringTag stringTagExpression)
        {
            return
                EvaluateExpressionDefault(
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
    public PineValue EvaluateListExpression(
        Expression.List listExpression,
        PineValue environment)
    {
        var listItems = new PineValue[listExpression.Items.Count];

        for (var i = 0; i < listExpression.Items.Count; i++)
        {
            var item = listExpression.Items[i];

            var itemResult =
                EvaluateExpressionDefault(
                    item,
                    environment);

            listItems[i] = itemResult;
        }

        return PineValue.List(listItems);
    }

    /// <summary>
    /// Evaluates a <see cref="Expression.ParseAndEval"/> expression:
    /// first evaluates the encoded expression and environment sub-expressions, then parses the
    /// encoded value into an <see cref="Expression"/> and evaluates it in the computed environment.
    /// Results may be cached when <c>evalCache</c> is provided.
    /// </summary>
    /// <exception cref="ParseExpressionException">Thrown when the encoded value cannot be parsed as a valid expression.</exception>
    public PineValue EvaluateParseAndEvalExpression(
        Expression.ParseAndEval parseAndEval,
        PineValue environment)
    {
        var environmentValue =
            EvaluateExpressionDefault(
                parseAndEval.Environment,
                environment);

        var expressionValue =
            EvaluateExpressionDefault(
                parseAndEval.Encoded,
                environment);

        if (evalCache is not null)
        {
            var cacheKey = new EvalCacheEntryKey(ExprValue: expressionValue, EnvValue: environmentValue);

            if (evalCache.TryGetValue(cacheKey, out var fromCache))
            {
                return fromCache;
            }
        }

        var parseResult = parseCache.ParseExpression(expressionValue);

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

        return
            EvaluateExpressionDefault(
                environment: environmentValue,
                expression: parseOk.Value);
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
    /// Evaluates a <see cref="Expression.KernelApplication"/> expression.
    /// Includes an optimized fast path for the common <c>head(skip(...))</c> pattern used for
    /// environment path access, falling back to the generic kernel function application.
    /// </summary>
    public PineValue EvaluateKernelApplicationExpression(
        PineValue environment,
        Expression.KernelApplication application)
    {
        if (application.Function is nameof(KernelFunction.head) &&
            application.Input is Expression.KernelApplication innerKernelApplication)
        {
            if (innerKernelApplication.Function is nameof(KernelFunction.skip) &&
                innerKernelApplication.Input is Expression.List skipListExpr &&
                skipListExpr.Items.Count is 2)
            {
                var skipValue =
                    EvaluateExpressionDefault(
                        skipListExpr.Items[0],
                        environment);

                if (KernelFunction.SignedIntegerFromValueRelaxed(skipValue) is { } skipCount)
                {
                    if (EvaluateExpressionDefault(
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

        return EvaluateKernelApplicationExpressionGeneric(environment, application);
    }

    /// <summary>
    /// Evaluates a <see cref="Expression.KernelApplication"/> using the generic kernel function dispatch.
    /// Evaluates the input expression first, then applies the named kernel function.
    /// </summary>
    public PineValue EvaluateKernelApplicationExpressionGeneric(
        PineValue environment,
        Expression.KernelApplication application)
    {
        var inputValue =
            EvaluateExpressionDefault(application.Input, environment);

        return
            KernelFunction.ApplyKernelFunctionGeneric(
                function: application.Function,
                inputValue: inputValue);
    }

    /// <summary>
    /// Evaluates a <see cref="Expression.Conditional"/> expression.
    /// Evaluates the condition first; if it equals <see cref="PineKernelValues.TrueValue"/>,
    /// evaluates and returns the true branch; otherwise evaluates and returns the false branch.
    /// </summary>
    public PineValue EvaluateConditionalExpression(
        PineValue environment,
        Expression.Conditional conditional)
    {
        var conditionValue =
            EvaluateExpressionDefault(
                conditional.Condition,
                environment);

        if (conditionValue == PineKernelValues.TrueValue)
        {
            return
                EvaluateExpressionDefault(
                    conditional.TrueBranch,
                    environment);
        }

        return
            EvaluateExpressionDefault(
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
