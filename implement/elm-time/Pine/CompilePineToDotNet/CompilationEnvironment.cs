using Pine.PineVM;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public record FunctionCompilationEnvironment(
    string ArgumentEnvironmentName,
    string ArgumentEvalGenericName,
    CompilationUnitEnv CompilationUnit);

public record CompilationUnitEnv(
    IReadOnlyDictionary<Expression, ImmutableHashSet<EnvConstraintId>> AvailableSpecialized)
{
    public static CompilationUnitEnv Empty { get; } = new(ImmutableDictionary<Expression, ImmutableHashSet<EnvConstraintId>>.Empty);

    public virtual bool Equals(CompilationUnitEnv? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            AvailableSpecialized.Count == other.AvailableSpecialized.Count &&
            AvailableSpecialized.All(pair =>
            other.AvailableSpecialized.TryGetValue(pair.Key, out var otherValue) &&
            pair.Value.SetEquals(otherValue));
    }

    override public int GetHashCode()
    {
        var hashCode = new System.HashCode();

        foreach (var pair in AvailableSpecialized)
        {
            hashCode.Add(pair.Key);
            hashCode.Add(pair.Value);
        }

        return hashCode.ToHashCode();
    }
}

public record ExpressionCompilationEnvironment(
    FunctionCompilationEnvironment FunctionEnvironment,
    IReadOnlyDictionary<Expression, LetBinding> LetBindings,
    ExpressionCompilationEnvironment? ParentEnvironment,
    EnvConstraintId? EnvConstraint)
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
