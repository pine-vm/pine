using Pine.Core.Internal;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Shared constructor DAG. Slots are process-wide references, not suspended computations.
/// </summary>
internal abstract record TemplateValueRecipe
{
    internal sealed record Constant(PineValue Value) : TemplateValueRecipe;

    internal sealed record Slot(int Index) : TemplateValueRecipe;

    internal sealed record List(IReadOnlyList<TemplateValueRecipe> Items) : TemplateValueRecipe;

    internal sealed record ProjectedSlot(int Index, IReadOnlyList<int> Path) : TemplateValueRecipe;

    private static readonly ConcurrentDictionary<int, Slot> s_slots = [];

    public static readonly TemplateValueRecipe EmptyList = new Constant(PineValue.EmptyList);

    public static Slot EnvironmentSlot(int index) =>
        s_slots.GetOrAdd(index, static index => new Slot(index));

    public PineValue? TryGetConstant() =>
        this switch
        {
            Constant constant => constant.Value,
            Slot => null,
            List => null,
            ProjectedSlot => null,

            _ =>
            throw new NotImplementedException(
                "TryGetConstant does not handle recipe variant: " + GetType().Name)
        };

    public IReadOnlyList<TemplateValueRecipe>? GetListItems() =>
        this switch
        {
            Constant { Value: PineValue.ListValue list } => new ConstantItems(list),
            Constant => null,
            List list => list.Items,
            Slot => null,
            ProjectedSlot => null,

            _ =>
            throw new NotImplementedException(
                "GetListItems does not handle recipe variant: " + GetType().Name)
        };

    public static TemplateValueRecipe BuildList(IReadOnlyList<TemplateValueRecipe> items)
    {
        var allConstant = true;

        for (var index = 0; index < items.Count; ++index)
            allConstant &= items[index] is Constant;

        if (!allConstant)
            return new List(items);

        if (items.Count is 0)
            return EmptyList;

        var values = new PineValue[items.Count];
        PineVMWorkProfile.Current?.Count(PineVMWorkProfile.Metric.RecipeConstantChildArrays);

        for (var index = 0; index < items.Count; ++index)
            values[index] = ((Constant)items[index]).Value;

        return new Constant(PineValue.List(values));
    }

    /// <summary>
    /// Simplifies projections before runtime so enclosing constructed lists are never needed.
    /// Missing fields and projections into blobs yield the empty list, as in Pine paths.
    /// </summary>
    public TemplateValueRecipe Project(IReadOnlyList<int> path)
    {
        TemplateValueRecipe current = this;

        for (var offset = 0; offset < path.Count; ++offset)
        {
            switch (current)
            {
                case Constant constant:
                    var constantValue = constant.Value;

                    for (var index = offset; index < path.Count; ++index)
                    {
                        var itemIndex = Math.Max(0, path[index]);

                        if (constantValue is not PineValue.ListValue list ||
                            itemIndex >= list.Items.Length)
                        {
                            return EmptyList;
                        }

                        constantValue = list.Items.Span[itemIndex];
                    }

                    return new Constant(constantValue);

                case List list:
                    var listIndex = Math.Max(0, path[offset]);

                    if (listIndex >= list.Items.Count)
                        return EmptyList;

                    current = list.Items[listIndex];
                    break;

                case Slot slot:
                    return new ProjectedSlot(slot.Index, CopyPath(path, offset, []));

                case ProjectedSlot slot:
                    return new ProjectedSlot(slot.Index, CopyPath(path, offset, slot.Path));

                default:
                    throw new NotImplementedException(
                        "Project does not handle recipe variant: " + current.GetType().Name);
            }
        }

        return current;
    }

    private static int[] CopyPath(IReadOnlyList<int> path, int offset, IReadOnlyList<int> prefix)
    {
        var result = new int[prefix.Count + path.Count - offset];

        for (var index = 0; index < prefix.Count; ++index)
            result[index] = prefix[index];

        for (var index = offset; index < path.Count; ++index)
            result[prefix.Count + index - offset] = path[index];

        return result;
    }

    public PineValueInProcess Instantiate(IReadOnlyList<PineValueInProcess> environments)
    {
        PineVMWorkProfile.Current?.Count(PineVMWorkProfile.Metric.RecipeInstantiateCalls);

        switch (this)
        {
            case Constant constant:
                return PineValueInProcess.Create(constant.Value);

            case Slot slot:
                return environments[slot.Index];

            case ProjectedSlot slot:
                return
                    PineValueInProcess.ValueFromPathOrNull(environments[slot.Index], slot.Path)
                    ?? PineValueInProcess.EmptyList;

            case List list:
                return InstantiateShared(list, environments, new(ReferenceEqualityComparer.Instance));

            default:
                throw new NotImplementedException(
                    "Instantiate does not handle recipe variant: " + GetType().Name);
        }
    }

    private static PineValueInProcess InstantiateShared(
        TemplateValueRecipe recipe,
        IReadOnlyList<PineValueInProcess> environments,
        Dictionary<TemplateValueRecipe, PineValueInProcess> instantiated)
    {
        var profile = PineVMWorkProfile.Current;
        profile?.Count(PineVMWorkProfile.Metric.RecipeInstantiateSharedCalls);

        if (instantiated.TryGetValue(recipe, out var cached))
        {
            profile?.Count(PineVMWorkProfile.Metric.RecipeInstantiateSharedMemoHits);
            return cached;
        }

        PineValueInProcess value;

        switch (recipe)
        {
            case Constant:
            case Slot:
            case ProjectedSlot:
                value = recipe.Instantiate(environments);
                break;

            case List list:
                var items = new PineValueInProcess[list.Items.Count];
                profile?.Count(PineVMWorkProfile.Metric.RecipeInstantiateSharedChildArrays);
                profile?.Count(PineVMWorkProfile.Metric.RecipeInstantiateSharedChildItems, items.Length);
                profile?.ChildArray(items.Length);

                for (var index = 0; index < items.Length; ++index)
                    items[index] = InstantiateShared(list.Items[index], environments, instantiated);

                value = PineValueInProcess.CreateList(items);
                break;

            default:
                throw new NotImplementedException(
                    "InstantiateShared does not handle recipe variant: " + recipe.GetType().Name);
        }

        instantiated[recipe] = value;
        return value;
    }

    private sealed class ConstantItems(PineValue.ListValue list) : IReadOnlyList<TemplateValueRecipe>
    {
        public int Count => list.Items.Length;

        public TemplateValueRecipe this[int index] => new Constant(list.Items.Span[index]);

        public IEnumerator<TemplateValueRecipe> GetEnumerator()
        {
            for (var index = 0; index < Count; ++index)
                yield return this[index];
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() => GetEnumerator();
    }
}
