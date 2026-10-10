using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Recognized encoded-expression template chain and a plan for invoking its terminal expression.
/// </summary>
internal sealed record ExpressionTemplatePlan(
    PineValue TemplateValue,
    PineValue EncodedTerminalExpression,
    Expression TerminalExpression,
    int EnvironmentCount,
    ReadOnlyMemory<PineValue> CapturedValues,
    ReadOnlyMemory<PineValue> InitialEnvironments,
    bool UsesNestedEnvironmentFormat,
    PineVMParseCache ParseCache)
{
    /// <summary>
    /// Recognizes canonical expression-template chains without probing arbitrary Eval values.
    /// </summary>
    public static FunctionRecord? TryParseTemplate(
        PineValue templateValue,
        PineVMParseCache parseCache,
        DirectInterpreterCounters? counters = null)
    {
        counters ??= new DirectInterpreterCounters();

        if (parseCache.ParseExpression(templateValue).IsOkOrNull() is not { } expression)
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
            var templateDescription =
                FunctionRecord.ParseCurriedTemplateForm(
                    templateValue,
                    parseCache,
                    counters)
                .IsOkOrNull();

            if (templateDescription is null ||
                templateDescription.ParameterCount <= templateDescription.ArgumentsAlreadyCollected.Length)
            {
                return null;
            }

            var canonicalValue =
                FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                    templateDescription.InnerFunction,
                    templateDescription.ParameterCount,
                    templateDescription.EnvFunctions.ToArray())!;

            var interpreter = DirectInterpreter.WithoutEvalCaching(parseCache, counters);

            foreach (var initialEnvironment in templateDescription.ArgumentsAlreadyCollected.Span)
            {
                canonicalValue =
                    interpreter.EvaluateExpressionDefault(
                        new Expression.Eval(
                            Expression.LitralInst(canonicalValue),
                            Expression.LitralInst(initialEnvironment)),
                        PineValue.EmptyList);
            }

            var canonicalExpression = parseCache.ParseExpression(canonicalValue).IsOkOrNull()!;

            // Probing identifies a candidate, but only a canonical template can bypass evaluation.
            return MatchesTemplate(expression, canonicalExpression, interpreter) ? templateDescription : null;
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
                {
                    return false;
                }

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
            {
                return false;
            }

            for (var index = 0; index < actualList.Items.Count; ++index)
                pending.Push((actualList.Items[index], expectedList.Items[index]));
        }

        return true;
    }

    /// <summary>
    /// Number of successive environments still required to reach the terminal expression.
    /// </summary>
    public int RemainingEnvironmentCount => EnvironmentCount - InitialEnvironments.Length;

    /// <summary>
    /// Reconstructs the intermediate encoded-expression value by evaluating the template with successive environments.
    /// </summary>
    public PineValue Materialize(
        IReadOnlyList<PineValueInProcess> additionalEnvironments,
        DirectInterpreterCounters counters)
    {
        var interpreter = DirectInterpreter.WithoutEvalCaching(ParseCache, counters);
        var currentValue = TemplateValue;

        for (var i = 0; i < additionalEnvironments.Count; ++i)
        {
            currentValue =
                interpreter.EvaluateExpressionDefault(
                    new Expression.Eval(
                        encoded: Expression.LitralInst(currentValue),
                        environment:
                        Expression.LitralInst(
                            additionalEnvironments[i].Evaluate())),
                    PineValue.EmptyList);
        }

        return currentValue;
    }

    /// <summary>
    /// Builds the terminal expression's environment without materializing the supplied environment values.
    /// </summary>
    public PineValueInProcess BuildTerminalEnvironment(
        IReadOnlyList<PineValueInProcess> additionalEnvironments)
    {
        var allEnvironments =
            new PineValueInProcess[InitialEnvironments.Length + additionalEnvironments.Count];

        for (var i = 0; i < InitialEnvironments.Length; ++i)
        {
            allEnvironments[i] = PineValueInProcess.Create(InitialEnvironments.Span[i]);
        }

        for (var i = 0; i < additionalEnvironments.Count; ++i)
        {
            allEnvironments[InitialEnvironments.Length + i] = additionalEnvironments[i];
        }

        var capturedValues =
            PineValueInProcess.Create(
                PineValue.List(CapturedValues.ToArray()));

        if (UsesNestedEnvironmentFormat)
        {
            return
                PineValueInProcess.CreateList(
                    [
                    capturedValues,
                    PineValueInProcess.CreateList(allEnvironments)
                    ]);
        }

        var environmentItems = new PineValueInProcess[allEnvironments.Length + 1];
        environmentItems[0] = capturedValues;
        allEnvironments.CopyTo(environmentItems, 1);

        return PineValueInProcess.CreateList(environmentItems);
    }
}
