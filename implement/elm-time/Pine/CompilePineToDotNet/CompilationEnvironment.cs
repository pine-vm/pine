using Pine.PineVM;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public record CompilationEnvironment(
    string ArgumentEnvironmentName,
    string ArgumentEvalGenericName,
    IReadOnlyDictionary<Expression, LetBinding> LetBindings,
    CompilationEnvironment? ParentEnvironment)
{
    public IEnumerable<CompilationEnvironment> EnumerateAncestors()
    {
        var current = this;

        while (current.ParentEnvironment is { } parentEnv)
        {
            yield return parentEnv;

            current = parentEnv;
        }
    }

    public IEnumerable<CompilationEnvironment> EnumerateSelfAndAncestors() =>
        EnumerateAncestors().Prepend(this);

    public ImmutableDictionary<Expression, LetBinding> EnumerateSelfAndAncestorsLetBindingsTransitive() =>
        EnumerateSelfAndAncestors()
        .SelectMany(env => env.EnumerateSelfLetBindingsTransitive())
        .ToImmutableDictionary();

    public ImmutableDictionary<Expression, LetBinding> EnumerateSelfLetBindingsTransitive() =>
        CompiledExpression.Union(
            LetBindings
            .Select(binding =>
            binding.Value.Expression.EnumerateLetBindingsTransitive()
            .SetItem(binding.Key, binding.Value)));
}
