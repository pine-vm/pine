using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.PineVM;

public interface IPineVM
{
    Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment);
}

public class PineVM : IPineVM
{
    public long EvaluateExpressionCount { private set; get; }

    public long FunctionApplicationMaxEnvSize { private set; get; }

    private readonly ParseExprDelegate parseExpressionDelegate;

    private readonly EvalExprDelegate evalExprDelegate;

    public static PineVM Construct(
        IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, Result<string, PineValue>>>? parseExpressionOverrides = null,
        OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        var parseExpressionOverridesDict =
            parseExpressionOverrides
            ?.ToFrozenDictionary(
                keySelector: encodedExprAndDelegate => encodedExprAndDelegate.Key,
                elementSelector: encodedExprAndDelegate => new Expression.DelegatingExpression(encodedExprAndDelegate.Value));

        return new PineVM(
            overrideParseExpression:
            parseExpressionOverridesDict switch
            {
                null =>
                originalHandler => originalHandler,

                not null =>
                _ => value => ExpressionEncoding.ParseExpressionFromValue(value, parseExpressionOverridesDict)
            },
            overrideEvaluateExpression);
    }

    public PineVM(
        OverrideParseExprDelegate? overrideParseExpression = null,
        OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        parseExpressionDelegate =
            overrideParseExpression
            ?.Invoke(ExpressionEncoding.ParseExpressionFromValueDefault) ??
            ExpressionEncoding.ParseExpressionFromValueDefault;

        evalExprDelegate =
            overrideEvaluateExpression?.Invoke(EvaluateExpressionDefault) ?? EvaluateExpressionDefault;
    }

    public Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment) => evalExprDelegate(expression, environment);

    public Result<string, PineValue> EvaluateExpressionDefault(
        Expression expression,
        PineValue environment)
    {
        if (expression is Expression.LiteralExpression literalExpression)
            return literalExpression.Value;

        if (expression is Expression.ListExpression listExpression)
        {
            return EvaluateListExpression(listExpression, environment);
        }

        if (expression is Expression.ParseAndEvalExpression applicationExpression)
        {
            return
                EvaluateParseAndEvalExpression(applicationExpression, environment) switch
                {
                    Result<string, PineValue>.Err err =>
                    "Failed to evaluate parse and evaluate: " + err,

                    var other =>
                    other
                };
        }

        if (expression is Expression.KernelApplicationExpression kernelApplicationExpression)
        {
            return
                EvaluateKernelApplicationExpression(environment, kernelApplicationExpression) switch
                {
                    Result<string, PineValue>.Err err =>
                    "Failed to evaluate kernel function application: " + err,

                    var other =>
                    other
                };
        }

        if (expression is Expression.ConditionalExpression conditionalExpression)
        {
            return EvaluateConditionalExpression(environment, conditionalExpression);
        }

        if (expression is Expression.EnvironmentExpression)
        {
            return environment;
        }

        if (expression is Expression.StringTagExpression stringTagExpression)
        {
            return EvaluateExpression(stringTagExpression.tagged, environment);
        }

        if (expression is Expression.DelegatingExpression delegatingExpr)
        {
            return delegatingExpr.Delegate.Invoke(evalExprDelegate, environment);
        }

        throw new NotImplementedException("Unexpected shape of expression: " + expression.GetType().FullName);
    }

    public Result<string, PineValue> EvaluateListExpression(
        Expression.ListExpression listExpression,
        PineValue environment)
    {
        var listItems = new List<PineValue>(listExpression.List.Count);

        for (var i = 0; i < listExpression.List.Count; i++)
        {
            var item = listExpression.List[i];

            var itemResult = EvaluateExpression(item, environment);

            if (itemResult is Result<string, PineValue>.Err itemErr)
                return "Failed to evaluate list element [" + i + "]: " + itemErr.Value;

            if (itemResult is Result<string, PineValue>.Ok itemOk)
            {
                listItems.Add(itemOk.Value);
                continue;
            }

            throw new NotImplementedException("Unexpected result type: " + itemResult.GetType().FullName);
        }

        return PineValue.List(listItems);
    }

    private static readonly System.Threading.AsyncLocal<int> evalDepth = new();

    public Result<string, PineValue> EvaluateParseAndEvalExpression(
        Expression.ParseAndEvalExpression parseAndEval,
        PineValue environment)
    {
        Result<string, PineValue> continueWithEnvValueAndFunction(
            PineValue environmentValue,
            Expression functionExpression)
        {
            if (environmentValue is PineValue.ListValue list)
            {
                FunctionApplicationMaxEnvSize =
                FunctionApplicationMaxEnvSize < list.Elements.Count ? list.Elements.Count : FunctionApplicationMaxEnvSize;
            }

            /*
             * 2024-05-19 work around for stack overflow:
             * After seeing some apps crash with stack overflow, reduce stack sizes by offloading to another thread.
             * 

            var evalResult = EvaluateExpression(environment: environmentValue, expression: functionExpression);

            return evalResult;
            */

            var evalDelegate =
                new Func<Result<string, PineValue>>(
                    () => EvaluateExpression(environment: environmentValue, expression: functionExpression));

            evalDepth.Value++;

            try
            {
                if (evalDepth.Value < 16)
                {
                    /*
                     * As long as we don't risk the stack becoming too large, reuse the same thread/stack.
                     * */
                    return evalDelegate();
                }

                /*
                 * Specify a cancellation token to prevent the Task framework from attempting synchronous execution.
                 * From the documentation on Task:
                 * > We will attempt inline execution only if an infinite wait was requested
                 * > [...]
                 * Source: https://github.com/dotnet/runtime/blob/087e15321bb712ef6fe8b0ba6f8bd12facf92629/src/libraries/System.Private.CoreLib/src/System/Threading/Tasks/Task.cs#L2997-L3008
                 * */

                var cancellationTokenSource = new System.Threading.CancellationTokenSource();

                var task =
                    System.Threading.Tasks.Task.Run(
                        function: evalDelegate,
                        cancellationToken: cancellationTokenSource.Token);

                task.Wait(cancellationToken: cancellationTokenSource.Token);

                return task.Result;
            }
            finally
            {
                evalDepth.Value--;
            }
        }

        return
            EvaluateExpression(parseAndEval.environment, environment) switch
            {
                Result<string, PineValue>.Err envErr =>
                "Failed to evaluate argument: " + envErr.Value,

                Result<string, PineValue>.Ok environmentValue =>
                EvaluateExpression(parseAndEval.expression, environment) switch
                {
                    Result<string, PineValue>.Err exprErr =>
                    "Failed to evaluate expression: " + exprErr.Value,

                    Result<string, PineValue>.Ok expressionValue =>
                    parseExpressionDelegate(expressionValue.Value) switch
                    {
                        Result<string, Expression>.Err parseErr =>
                        "Failed to parse expression from value: " + parseErr.Value +
                        " - expressionValue is " + DescribeValueForErrorMessage(expressionValue.Value) +
                        " - environmentValue is " + DescribeValueForErrorMessage(expressionValue.Value),

                        Result<string, Expression>.Ok functionExpression =>
                        continueWithEnvValueAndFunction(environmentValue.Value, functionExpression.Value),

                        var otherResult =>
                        throw new NotImplementedException("Unexpected result type for parse: " + otherResult.GetType().FullName)
                    },

                    var otherResult =>
                    throw new NotImplementedException("Unexpected result type for expr: " + otherResult.GetType().FullName)
                },

                var otherResult =>
                throw new NotImplementedException("Unexpected result type for env: " + otherResult.GetType().FullName)
            };
    }

    public static string DescribeValueForErrorMessage(PineValue pineValue) =>
        PineValueAsString.StringFromValue(pineValue)
        .Unpack(fromErr: _ => "not a string", fromOk: asString => "string \'" + asString + "\'");


    public Result<string, PineValue> EvaluateKernelApplicationExpression(
        PineValue environment,
        Expression.KernelApplicationExpression application) =>
        EvaluateExpression(application.argument, environment) switch
        {
            Result<string, PineValue>.Ok argument =>
            application.function(argument.Value),

            Result<string, PineValue>.Err error =>
            "Failed to evaluate argument: " + error,

            var otherResult =>
            throw new NotImplementedException("Unexpected result type: " + otherResult.GetType().FullName)
        };


    public Result<string, PineValue> EvaluateConditionalExpression(
        PineValue environment,
        Expression.ConditionalExpression conditional) =>
        EvaluateExpression(conditional.condition, environment) switch
        {
            Result<string, PineValue>.Ok conditionValue =>
            conditionValue == PineVMValues.TrueValue
            ?
            EvaluateExpression(conditional.ifTrue, environment)
            :
            conditionValue == PineVMValues.FalseValue
            ?
            EvaluateExpression(conditional.ifFalse, environment)
            :
            PineValue.EmptyList,

            Result<string, PineValue>.Err error =>
            "Failed to evaluate condition: " + error,

            var otherResult =>
            throw new NotImplementedException("Unexpected result type: " + otherResult.GetType().FullName)
        };
}
