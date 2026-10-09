using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Parsed runtime information for a canonical incrementally applicable function value.
/// </summary>
internal sealed record CurriedFunctionPlan(
    PineValue FunctionValue,
    PineValue EncodedBody,
    Expression Body,
    int ParameterCount,
    ReadOnlyMemory<PineValue> EnvFunctions,
    ReadOnlyMemory<PineValue> InitialArguments,
    bool UsesNestedArgFormat,
    PineVMParseCache ParseCache)
{
    /// <summary>
    /// Parses only canonical curried-template forms, without probing arbitrary Eval values.
    /// </summary>
    public static FunctionRecord? TryParseFunctionRecord(
        PineValue functionValue,
        PineVMParseCache parseCache)
    {
        if (parseCache.ParseExpression(functionValue).IsOkOrNull() is not { } expression)
            return null;

        var isTemplateProducer =
            expression is Expression.List &&
            expression.EvalCount is 0 &&
            expression.BuiltinCount is 0 &&
            expression.ConditionCount is 0;

        var isTerminalTemplate =
            expression is Expression.Eval
            {
                Encoded: Expression.Litral,
                Environment: Expression.List environment
            } &&
            environment.Items.Count >= 2 &&
            environment.Items[0] is Expression.Litral
            {
                Value: PineValue.ListValue
            } &&
            environment.Items[^1] is Expression.Environment;

        if (!isTemplateProducer && !isTerminalTemplate)
            return null;

        try
        {
            var function =
                FunctionRecord.ParseCurriedTemplateForm(
                    functionValue,
                    parseCache)
                .IsOkOrNull();

            if (function is null ||
                function.ParameterCount <= function.ArgumentsAlreadyCollected.Length)
                return null;

            var canonicalValue =
                FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                    function.InnerFunction,
                    function.ParameterCount,
                    function.EnvFunctions.ToArray())!;

            var interpreter = DirectInterpreter.WithoutEvalCaching(parseCache);

            foreach (var argument in function.ArgumentsAlreadyCollected.Span)
            {
                canonicalValue =
                    interpreter.EvaluateExpressionDefault(
                        new Expression.Eval(
                            Expression.LitralInst(canonicalValue),
                            Expression.LitralInst(argument)),
                        PineValue.EmptyList);
            }

            var canonicalExpression = parseCache.ParseExpression(canonicalValue).IsOkOrNull()!;

            // Probing identifies a candidate, but only a canonical template can bypass evaluation.
            return MatchesTemplate(expression, canonicalExpression, interpreter) ? function : null;
        }
        catch (ParseExpressionException)
        {
            return null;
        }
    }

    private static bool MatchesTemplate(
        Expression expression,
        Expression canonical,
        DirectInterpreter interpreter)
    {
        var pending = new Stack<(Expression Actual, Expression Expected)>();
        pending.Push((expression, canonical));

        while (pending.TryPop(out var pair))
        {
            if (pair.Actual.Equals(pair.Expected))
                continue;

            if (!pair.Actual.ReferencesEnvironment && !pair.Expected.ReferencesEnvironment &&
                pair.Actual.EvalCount is 0 && pair.Actual.BuiltinCount is 0 && pair.Actual.ConditionCount is 0 &&
                pair.Expected.EvalCount is 0 && pair.Expected.BuiltinCount is 0 && pair.Expected.ConditionCount is 0)
            {
                if (interpreter.EvaluateExpressionDefault(pair.Actual, PineValue.EmptyList) !=
                    interpreter.EvaluateExpressionDefault(pair.Expected, PineValue.EmptyList))
                    return false;

                continue;
            }

            if (pair.Actual is Expression.Eval actualEval && pair.Expected is Expression.Eval expectedEval)
            {
                pending.Push((actualEval.Encoded, expectedEval.Encoded));
                pending.Push((actualEval.Environment, expectedEval.Environment));
                continue;
            }

            if (pair.Actual is not Expression.List actualList ||
                pair.Expected is not Expression.List expectedList ||
                actualList.Items.Count != expectedList.Items.Count)
                return false;

            for (var index = 0; index < actualList.Items.Count; ++index)
                pending.Push((actualList.Items[index], expectedList.Items[index]));
        }

        return true;
    }

    /// <summary>
    /// Number of arguments not present in the original function value.
    /// </summary>
    public int RemainingArity => ParameterCount - InitialArguments.Length;

    /// <summary>
    /// Materializes the canonical value produced by applying the supplied arguments incrementally.
    /// </summary>
    public PineValue Materialize(IReadOnlyList<PineValueInProcess> additionalArguments)
    {
        var interpreter = DirectInterpreter.WithoutEvalCaching(ParseCache);
        var currentValue = FunctionValue;

        for (var i = 0; i < additionalArguments.Count; ++i)
        {
            currentValue =
                interpreter.EvaluateExpressionDefault(
                    new Expression.Eval(
                        encoded: Expression.LitralInst(currentValue),
                        environment:
                        Expression.LitralInst(
                            additionalArguments[i].Evaluate())),
                    PineValue.EmptyList);
        }

        return currentValue;
    }

    /// <summary>
    /// Builds the body environment without materializing the supplied arguments.
    /// </summary>
    public PineValueInProcess BuildBodyEnvironment(
        IReadOnlyList<PineValueInProcess> additionalArguments)
    {
        var allArguments =
            new PineValueInProcess[InitialArguments.Length + additionalArguments.Count];

        for (var i = 0; i < InitialArguments.Length; ++i)
        {
            allArguments[i] = PineValueInProcess.Create(InitialArguments.Span[i]);
        }

        for (var i = 0; i < additionalArguments.Count; ++i)
        {
            allArguments[InitialArguments.Length + i] = additionalArguments[i];
        }

        var envFunctions =
            PineValueInProcess.Create(
                PineValue.List(EnvFunctions.ToArray()));

        if (UsesNestedArgFormat)
        {
            return
                PineValueInProcess.CreateList(
                    [
                    envFunctions,
                    PineValueInProcess.CreateList(allArguments)
                    ]);
        }

        var environmentItems = new PineValueInProcess[allArguments.Length + 1];
        environmentItems[0] = envFunctions;
        allArguments.CopyTo(environmentItems, 1);

        return PineValueInProcess.CreateList(environmentItems);
    }
}
