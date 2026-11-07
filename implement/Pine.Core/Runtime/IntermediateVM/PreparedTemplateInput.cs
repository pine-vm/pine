using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Runtime.CompilerServices;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Immutable parameter projections for one recipe and compilation. Only metadata is shared;
/// argument values and constructor memoization remain invocation-owned.
/// </summary>
internal sealed class PreparedTemplateInput
{
    private static readonly ConditionalWeakTable<ExpressionCompilation, ProjectionCache> s_compilations = [];

    private readonly ImmutableArray<TemplateValueRecipe> _arguments;

    private readonly bool _requiresMemo;

    public StaticFunctionInterface Parameters { get; }

    private PreparedTemplateInput(TemplateValueRecipe recipe, StaticFunctionInterface parameters)
    {
        Parameters = parameters;
        var arguments = ImmutableArray.CreateBuilder<TemplateValueRecipe>(parameters.ParamsPaths.Count);

        foreach (var path in parameters.ParamsPaths)
            arguments.Add(recipe.Project(path));

        _arguments = arguments.MoveToImmutable();
        var seen = new HashSet<TemplateValueRecipe>(ReferenceEqualityComparer.Instance);
        var containsList = false;
        var repeated = false;

        foreach (var argument in _arguments)
            Visit(argument);

        _requiresMemo = containsList && repeated;

        void Visit(TemplateValueRecipe item)
        {
            if (!seen.Add(item))
            {
                repeated = true;
                return;
            }

            switch (item)
            {
                case TemplateValueRecipe.List list:
                    containsList = true;

                    foreach (var child in list.Items)
                        Visit(child);

                    break;

                case TemplateValueRecipe.Constant:
                case TemplateValueRecipe.Slot:
                case TemplateValueRecipe.ProjectedSlot:
                    break;

                default:
                    throw new System.NotImplementedException(
                        "PreparedTemplateInput does not handle recipe variant: " + item.GetType().Name);
            }
        }
    }

    public static PreparedTemplateInput Get(ExpressionCompilation compilation, TemplateValueRecipe recipe) =>
        s_compilations.GetValue(
            compilation,
            static compilation => new ProjectionCache(compilation.Generic.Parameters)).Get(recipe);

    public StackFrameInput Instantiate(IReadOnlyList<PineValueInProcess> environments)
    {
        var arguments =
            _arguments.IsEmpty ? [] : new PineValueInProcess[_arguments.Length];

        Dictionary<TemplateValueRecipe, PineValueInProcess>? memo = null;

        for (var index = 0; index < arguments.Length; ++index)
            arguments[index] = _arguments[index].Instantiate(environments, ref memo, _requiresMemo);

        return StackFrameInput.FromArguments(Parameters, arguments);
    }

    private sealed class ProjectionCache
    {
        private readonly ConditionalWeakTable<TemplateValueRecipe, PreparedTemplateInput> _recipes = [];

        private readonly ConditionalWeakTable<TemplateValueRecipe, PreparedTemplateInput>.CreateValueCallback _prepare;

        public ProjectionCache(StaticFunctionInterface parameters)
        {
            _prepare = recipe => new PreparedTemplateInput(recipe, parameters);
        }

        public PreparedTemplateInput Get(TemplateValueRecipe recipe) => _recipes.GetValue(recipe, _prepare);
    }
}
