using Pine.PineVM;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public record FunctionCompilationEnvironment(
    string ArgumentEnvironmentName,
    string ArgumentEvalGenericName);

public record ExpressionCompilationEnvironment(
    FunctionCompilationEnvironment FunctionEnvironment,
    IReadOnlyDictionary<Expression, LetBinding> LetBindings,
    ExpressionCompilationEnvironment? ParentEnvironment)
{
    public IEnumerable<ExpressionCompilationEnvironment> EnumerateAncestors()
    {
        var current = this;

        while (current.ParentEnvironment is { } parentEnv)
        {
            yield return parentEnv;

            current = parentEnv;
        }
    }

    public IEnumerable<ExpressionCompilationEnvironment> EnumerateSelfAndAncestors() =>
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
