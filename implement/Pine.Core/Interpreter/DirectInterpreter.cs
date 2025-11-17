using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;

namespace Pine.Core.Interpreter;


public class DirectInterpreter(
    PineVMParseCache parseCache,
    IDictionary<DirectInterpreter.EvalCacheEntryKey, PineValue>? evalCache) : IPineVM
{
    public record struct EvalCacheEntryKey(
        PineValue ExprValue,
        PineValue EnvValue);

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
            return EvaluateConditionalExpression(
                environment,
                conditionalExpression);
        }

        if (expression is Expression.Environment)
        {
            return environment;
        }

        if (expression is Expression.StringTag stringTagExpression)
        {
            return EvaluateExpressionDefault(
                stringTagExpression.Tagged,
                environment);
        }

        throw new NotImplementedException(
            "Unexpected shape of expression: " + expression.GetType().FullName);
    }

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

    public static string DescribeValueForErrorMessage(PineValue pineValue) =>
        StringEncoding.StringFromValue(pineValue)
        .Unpack(fromErr: _ => "not a string", fromOk: asString => "string \'" + asString + "\'");

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
            return EvaluateExpressionDefault(
                conditional.TrueBranch,
                environment);
        }

        return EvaluateExpressionDefault(
            conditional.FalseBranch,
            environment);
    }

    public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment)
    {
        return
            EvaluateExpressionDefault(
                expression,
                environment);
    }
}
