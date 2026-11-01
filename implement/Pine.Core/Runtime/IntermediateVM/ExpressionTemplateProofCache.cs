using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Parse-cache-scoped fixed-code proofs. Constructor holes are opaque input data, never code
/// assumptions. Exact code values retain their own bounded proof, including budget negatives.
/// </summary>
internal sealed class ExpressionTemplateProofCache
{
    private sealed record ConstructorProof(ExpressionTemplatePlan? Plan);

    private sealed record ValueProof(int? Nodes);

    private readonly ConcurrentDictionary<PineValue, ConstructorProof> _constructors = [];

    private readonly ConcurrentDictionary<PineValue, ValueProof> _values = [];

    internal int ConstructorCount => _constructors.Count;

    internal int BoundedValueCount => _values.Count;

    internal ExpressionTemplatePlan? TryResolve(PineValue value, PineVMParseCache parseCache)
    {
        var remaining = ExpressionTemplatePlan.RecognitionBudget.MaxNodes;

        if (BuildShape(value, 0, ref remaining) is not { } shape)
            return null;

        var profile = PineVMWorkProfile.Current;
        profile?.Count(PineVMWorkProfile.Metric.TemplateShapeProbes);

        if (_constructors.TryGetValue(shape, out var proof))
            profile?.Count(PineVMWorkProfile.Metric.TemplateShapeHits);

        else
        {
            proof =
                _constructors.GetOrAdd(
                    shape,
                    key =>
                    {
                        profile?.Count(PineVMWorkProfile.Metric.TemplateShapeFactories);

                        return
                            new ConstructorProof(
                                ExpressionTemplatePlan.TryParseTemplateUncached(
                                    key,
                                    Parameterize(key, []),
                                    parseCache,
                                    environmentOffset: 1));
                    });
        }

        if (proof.Plan is not { } plan)
        {
            profile?.Count(PineVMWorkProfile.Metric.TemplateShapeFallbacks);
            return null;
        }

        var bound = new Dictionary<TemplateValueRecipe, TemplateValueRecipe>(ReferenceEqualityComparer.Instance);
        var root = new TemplateValueRecipe.Constant(value);
        var intermediates = new TemplateValueRecipe[plan.IntermediateRecipes.Count];

        for (var index = 0; index < intermediates.Length; ++index)
            intermediates[index] = Bind(plan.IntermediateRecipes[index], root, bound);

        profile?.Count(PineVMWorkProfile.Metric.TemplateShapeApplications);

        return
            plan with
            {
                TemplateValue = value,
                IntermediateRecipes = intermediates,
                TerminalEnvironmentRecipe = Bind(plan.TerminalEnvironmentRecipe, root, bound)
            };
    }

    internal bool CheckValue(PineValue value, ExpressionTemplatePlan.RecognitionBudget budget)
    {
        var profile = PineVMWorkProfile.Current;
        profile?.Count(PineVMWorkProfile.Metric.FixedCodeProofProbes);

        if (_values.TryGetValue(value, out var proof))
            profile?.Count(PineVMWorkProfile.Metric.FixedCodeProofHits);

        else
            proof = _values.GetOrAdd(value, static key => new ValueProof(CountBoundedValue(key)));

        return budget.ApplyValueProof(proof.Nodes);
    }

    private static int? CountBoundedValue(PineValue value)
    {
        PineVMWorkProfile.Current?.Count(PineVMWorkProfile.Metric.FixedCodeProofFactories);
        var budget = new ExpressionTemplatePlan.RecognitionBudget(reportFailures: false);
        return budget.CheckValue(value) ? budget.Consumed : null;
    }

    private static PineValue? BuildShape(PineValue value, int depth, ref int remaining)
    {
        var profile = PineVMWorkProfile.Current;
        profile?.Count(PineVMWorkProfile.Metric.TemplateShapeVisits);
        profile?.Checkpoint();

        if (--remaining < 0 || depth > ExpressionTemplatePlan.RecognitionBudget.MaxDepth ||
            value is not PineValue.ListValue list || list.Items.Length is 0)
        {
            return null;
        }

        var items = list.Items;
        var tag = items.Span[0];

        if (tag == s_literal && items.Length is 2)
        {
            var payload = items.Span[1];

            if (IsLiteralEncoding(payload))
            {
                var nested = BuildShape(payload, depth + 1, ref remaining);
                return nested is null ? null : PineValue.List([s_literal, nested]);
            }

            return IsFixedPayload(payload) ? value : PineValue.List([s_literal, PineValue.EmptyBlob]);
        }

        if (tag == s_environment && items.Length is 1)
            return value;

        if ((tag != s_list && tag != s_eval) || (tag == s_eval && items.Length is not 3) ||
            items.Length - 1 > remaining)
        {
            return null;
        }

        var children = new PineValue[items.Length];
        profile?.Count(PineVMWorkProfile.Metric.TemplateShapeKeyArrays);
        profile?.Count(PineVMWorkProfile.Metric.TemplateShapeKeyItems, children.Length);
        children[0] = tag;

        for (var index = 1; index < children.Length; ++index)
        {
            if (BuildShape(items.Span[index], depth + 1, ref remaining) is not { } child)
                return null;

            children[index] = child;
        }

        return PineValue.List(children);
    }

    // Retaining code-looking data is conservative: it reduces reuse but cannot freeze changing code.
    private static bool IsFixedPayload(PineValue value) =>
        s_tags.Contains(value) ||
        (value is PineValue.ListValue list && list.Items.Length > 0 && s_tags.Contains(list.Items.Span[0]));

    private static bool IsLiteralEncoding(PineValue value) =>
        value is PineValue.ListValue list && list.Items.Length is 2 && list.Items.Span[0] == s_literal;

    private static TemplateValueRecipe Parameterize(PineValue shape, int[] path)
    {
        if (shape is not PineValue.ListValue list)
            throw new InvalidOperationException("A constructor shape must be a list.");

        var items = new TemplateValueRecipe[list.Items.Length];
        items[0] = new TemplateValueRecipe.Constant(list.Items.Span[0]);

        for (var index = 1; index < items.Length; ++index)
        {
            var childPath = new int[path.Length + 1];
            path.CopyTo(childPath, 0);
            childPath[^1] = index;

            if (list.Items.Span[0] == s_literal)
            {
                items[index] =
                    list.Items.Span[index] == PineValue.EmptyBlob
                    ?
                    new TemplateValueRecipe.ProjectedSlot(0, childPath)
                    :
                    IsLiteralEncoding(list.Items.Span[index])
                    ?
                    Parameterize(list.Items.Span[index], childPath)
                    :
                    new TemplateValueRecipe.Constant(list.Items.Span[index]);
            }
            else
                items[index] = Parameterize(list.Items.Span[index], childPath);
        }

        return TemplateValueRecipe.BuildList(items);
    }

    private static TemplateValueRecipe Bind(
        TemplateValueRecipe recipe,
        TemplateValueRecipe.Constant root,
        Dictionary<TemplateValueRecipe, TemplateValueRecipe> bound)
    {
        if (bound.TryGetValue(recipe, out var cached))
            return cached;

        var profile = PineVMWorkProfile.Current;
        profile?.Count(PineVMWorkProfile.Metric.TemplateShapeBindingVisits);
        TemplateValueRecipe result;

        switch (recipe)
        {
            case TemplateValueRecipe.Constant:
                result = recipe;
                break;

            case TemplateValueRecipe.Slot slot:
                result = slot.Index is 0 ? root : TemplateValueRecipe.EnvironmentSlot(slot.Index - 1);
                break;

            case TemplateValueRecipe.ProjectedSlot slot:
                result =
                    slot.Index is 0
                    ?
                    root.Project(slot.Path)
                    :
                    new TemplateValueRecipe.ProjectedSlot(slot.Index - 1, slot.Path);

                break;

            case TemplateValueRecipe.List list:
                profile?.Count(PineVMWorkProfile.Metric.TemplateShapeBindingArrays);
                profile?.Count(PineVMWorkProfile.Metric.TemplateShapeBindingItems, list.Items.Count);
                var items = new TemplateValueRecipe[list.Items.Count];

                for (var index = 0; index < items.Length; ++index)
                    items[index] = Bind(list.Items[index], root, bound);

                result = TemplateValueRecipe.BuildList(items);
                break;

            default:
                throw new NotImplementedException(
                    "Bind does not handle recipe variant: " + recipe.GetType().Name);
        }

        bound.Add(recipe, result);
        return result;
    }

    private static readonly PineValue s_list = StringEncoding.ValueFromString("List");

    private static readonly PineValue s_literal = StringEncoding.ValueFromString("Litral");

    private static readonly PineValue s_environment = StringEncoding.ValueFromString("Environment");

    private static readonly PineValue s_eval = StringEncoding.ValueFromString("Eval");

    private static readonly HashSet<PineValue> s_tags =
        [
        s_list, s_literal, s_environment, s_eval,
        StringEncoding.ValueFromString("Builtin"), StringEncoding.ValueFromString("Conditional"),
        StringEncoding.ValueFromString("Condition"), StringEncoding.ValueFromString("Label")
        ];
}
