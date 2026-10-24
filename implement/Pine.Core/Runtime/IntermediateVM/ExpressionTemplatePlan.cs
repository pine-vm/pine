using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// A fixed-code chain proved by symbolic constructor evaluation, independent of supplied environments.
/// </summary>
internal sealed record ExpressionTemplatePlan(
    PineValue TemplateValue,
    PineValue EncodedTerminalExpression,
    Expression TerminalExpression,
    int EnvironmentCount,
    IReadOnlyList<TemplateValueRecipe> IntermediateRecipes,
    TemplateValueRecipe TerminalEnvironmentRecipe,
    bool ElidesTerminalEval = false)
{
    public int RemainingEnvironmentCount => EnvironmentCount;

    /// <summary>
    /// Recognition never runs arbitrary Pine code. Exhausted budgets and uncertain code shapes
    /// are ordinary misses, including values which would fail only at a later Eval.
    /// </summary>
    public static ExpressionTemplatePlan? TryParseTemplate(
        PineValue templateValue,
        PineVMParseCache parseCache,
        DirectInterpreterCounters? counters = null)
    {
        var budget = new RecognitionBudget();
        TemplateValueRecipe current = new TemplateValueRecipe.Constant(templateValue);
        var intermediates = new List<TemplateValueRecipe>();
        (PineValue Code, int Stage)? lastInvariantConstructor = null;

        for (var stage = 0; stage < 256; ++stage)
        {
            PineVMWorkProfile.Current?.Count(PineVMWorkProfile.Metric.RecognizerStages);
            var environment = TemplateValueRecipe.EnvironmentSlot(stage);
            var constructedNodes = new Dictionary<TemplateValueRecipe, TemplateValueRecipe?>();
            var constructed = TryConstruct(current, environment, budget, 0, constructedNodes);

            if (constructed is not null)
            {
                PineVMWorkProfile.Current?.Count(PineVMWorkProfile.Metric.RecognizerConstructorStages);

                if (stage > 0 && current.TryGetConstant() is { } invariantCode)
                    lastInvariantConstructor = (invariantCode, stage);

                intermediates.Add(constructed);
                current = constructed;
                continue;
            }

            if (budget.Exhausted)
                return null;

            if (TryGetEvalArguments(current, out var encoded, out var input))
            {
                var terminalCode = TryConstruct(encoded, environment, budget, 0, constructedNodes)?.TryGetConstant();
                var terminalInput = TryConstruct(input, environment, budget, 0, constructedNodes);

                if (terminalCode is not null &&
                    terminalInput is not null &&
                    ParseBounded(terminalCode, parseCache, budget) is { } terminal)
                {
                    return
                        new(
                            templateValue,
                            terminalCode,
                            terminal,
                            stage + 1,
                            [.. intermediates],
                            terminalInput,
                            ElidesTerminalEval: true);
                }
            }

            // An invariant arbitrary computation can end a constructor chain directly.
            // A literal containing a slot is not invariant code, even though its encoding tag is fixed.
            if (intermediates.Count > 0 &&
                current.TryGetConstant() is { } constantCode &&
                ParseBounded(constantCode, parseCache, budget) is { } body)
            {
                return
                    new(
                        templateValue,
                        constantCode,
                        body,
                        stage + 1,
                        [.. intermediates],
                        environment);
            }

            // A fixed constructor may itself be the terminal computation: for example,
            // a literal returning a blob or Environment returning arbitrary user data.
            // Do not mistake an unfamiliar result for malformed code to execute now.
            if (lastInvariantConstructor is { } boundary &&
                ParseBounded(boundary.Code, parseCache, budget) is { } constructorBody)
            {
                return
                    new(
                        templateValue,
                        boundary.Code,
                        constructorBody,
                        boundary.Stage + 1,
                        [.. intermediates.GetRange(0, boundary.Stage)],
                        TemplateValueRecipe.EnvironmentSlot(boundary.Stage));
            }

            return null;
        }

        PineVMWorkProfile.Current?.Count(PineVMWorkProfile.Metric.RecognizerBudgetFailures);
        return null;
    }

    internal static Expression? ParseBounded(
        PineValue value,
        PineVMParseCache parseCache,
        RecognitionBudget budget)
    {
        if (!budget.CheckValue(value))
            return null;

        var parsed = parseCache.ParseExpression(value).IsOkOrNull();

        if (parsed is null)
            PineVMWorkProfile.Current?.Count(PineVMWorkProfile.Metric.RecognizerTerminalParseFailures);

        return parsed;
    }

    private static TemplateValueRecipe? TryConstruct(
        TemplateValueRecipe encoded,
        TemplateValueRecipe environment,
        RecognitionBudget budget,
        int depth,
        Dictionary<TemplateValueRecipe, TemplateValueRecipe?> constructedNodes)
    {
        if (constructedNodes.TryGetValue(encoded, out var cached))
            return cached;

        var result = TryConstructUncached(encoded, environment, budget, depth, constructedNodes);
        constructedNodes[encoded] = result;
        return result;
    }

    private static TemplateValueRecipe? TryConstructUncached(
        TemplateValueRecipe encoded,
        TemplateValueRecipe environment,
        RecognitionBudget budget,
        int depth,
        Dictionary<TemplateValueRecipe, TemplateValueRecipe?> constructedNodes)
    {
        if (!budget.Visit(depth) ||
            encoded.GetListItems() is not { Count: > 0 } items ||
            items[0].TryGetConstant() is not { } tagValue)
        {
            return null;
        }

        // Compare against fixed tags, rather than decoding arbitrary user values as strings.
        if (tagValue == s_literalTag && items.Count is 2)
            return items[1];

        if (tagValue == s_environmentTag && items.Count is 1)
            return environment;

        if (tagValue != s_listTag)
            return null;

        if (!budget.AllowItems(items.Count - 1))
            return null;

        var resultItems = new TemplateValueRecipe[items.Count - 1];
        PineVMWorkProfile.Current?.Count(PineVMWorkProfile.Metric.RecognizerRecipeChildArrays);
        PineVMWorkProfile.Current?.Count(PineVMWorkProfile.Metric.RecognizerRecipeChildItems, resultItems.Length);

        for (var index = 1; index < items.Count; ++index)
        {
            if (TryConstruct(items[index], environment, budget, depth + 1, constructedNodes) is not { } child)
                return null;

            resultItems[index - 1] = child;
        }

        return TemplateValueRecipe.BuildList(resultItems);
    }

    private static bool TryGetEvalArguments(
        TemplateValueRecipe encoded,
        out TemplateValueRecipe code,
        out TemplateValueRecipe environment)
    {
        code = environment = TemplateValueRecipe.EmptyList;

        if (encoded.GetListItems() is not { } items)
            return false;

        if (items.Count is 3 && items[0].TryGetConstant() == s_evalTag)
        {
            code = items[1];
            environment = items[2];
            return true;
        }

        return false;
    }

    /// <summary>Reconstructs the exact escaped value, preserving its original encoding.</summary>
    public PineValue Materialize(
        IReadOnlyList<PineValueInProcess> environments,
        DirectInterpreterCounters counters)
    {
        if (environments.Count is 0)
            return TemplateValue;

        if (environments.Count >= EnvironmentCount)
            throw new ArgumentOutOfRangeException(nameof(environments), "The template is already saturated.");

        return IntermediateRecipes[environments.Count - 1].Instantiate(environments).Evaluate();
    }

    public PineValueInProcess BuildTerminalEnvironment(
        IReadOnlyList<PineValueInProcess> environments) =>
        TerminalEnvironmentRecipe.Instantiate(environments);

    private static readonly PineValue s_listTag = StringEncoding.ValueFromString("List");

    private static readonly PineValue s_literalTag = StringEncoding.ValueFromString("Litral");

    private static readonly PineValue s_environmentTag = StringEncoding.ValueFromString("Environment");

    private static readonly PineValue s_evalTag = StringEncoding.ValueFromString("Eval");

    internal sealed class RecognitionBudget
    {
        private int _remaining = 16_384;

        public bool Exhausted { get; private set; }

        public bool Visit(int depth)
        {
            var profile = PineVMWorkProfile.Current;
            profile?.Count(PineVMWorkProfile.Metric.RecognizerVisits);
            profile?.Checkpoint();

            if (Exhausted || depth > 128 || --_remaining < 0)
            {
                if (!Exhausted)
                    profile?.Count(PineVMWorkProfile.Metric.RecognizerBudgetFailures);

                Exhausted = true;
                return false;
            }

            return true;
        }

        public bool AllowItems(int count)
        {
            if (count > _remaining)
            {
                if (!Exhausted)
                    PineVMWorkProfile.Current?.Count(PineVMWorkProfile.Metric.RecognizerBudgetFailures);

                Exhausted = true;
                return false;
            }

            return !Exhausted;
        }

        public bool CheckValue(PineValue value)
        {
            var pending = new Stack<(PineValue Value, int Depth)>();
            pending.Push((value, 0));

            while (pending.TryPop(out var item))
            {
                if (!Visit(item.Depth))
                    return false;

                switch (item.Value)
                {
                    case PineValue.BlobValue:
                        break;

                    case PineValue.ListValue list:
                        if (list.Items.Length > _remaining)
                        {
                            if (!Exhausted)
                                PineVMWorkProfile.Current?.Count(PineVMWorkProfile.Metric.RecognizerBudgetFailures);

                            Exhausted = true;
                            return false;
                        }

                        foreach (var child in list.Items.Span)
                            pending.Push((child, item.Depth + 1));

                        break;

                    default:
                        throw new NotImplementedException(
                            "CheckValue does not handle value variant: " + item.Value.GetType().Name);
                }
            }

            return true;
        }
    }
}
