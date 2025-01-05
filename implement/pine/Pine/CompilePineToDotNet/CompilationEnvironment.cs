using Pine.Core;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public record FunctionCompilationEnv(
    ExprFunctionCompilationInterface SelfInterface,
    CompilationUnitEnv CompilationUnit)
{
    public virtual bool Equals(FunctionCompilationEnv? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            SelfInterface.Equals(other.SelfInterface) &&
            CompilationUnit.Equals(other.CompilationUnit);
    }

    override public int GetHashCode() =>
        HashCode.Combine(SelfInterface, CompilationUnit);

    public static IReadOnlyList<IReadOnlyList<int>> CompileEnvItemsPathsForExprFunction(
        Expression expression,
        EnvConstraintId? envConstraint)
    {
        var allSubexpressions =
            Expression.EnumerateSelfAndDescendants(expression)
            .ToImmutableArray();

        var allPaths = SubsetOfExpressionsWithPaths(allSubexpressions);

        var subExpressionsExceptUnderPaths =
            Expression.EnumerateSelfAndDescendants(
                expression,
                skipConditionalBranches: false,
                skipDescendants: subExpr => allPaths.Any(path => path.subExpr == subExpr))
            .ToImmutableArray();

        var pathsLessRedundant = SubsetOfExpressionsWithPaths(subExpressionsExceptUnderPaths);

        var pathsCommonPrefixes =
            GetPathsCommonPrefixes([.. pathsLessRedundant.Select(exprAndPath => exprAndPath.path)]);

        IReadOnlyList<IReadOnlyList<int>> pathsBeforeSorting =
            pathsCommonPrefixes
            .Where(path => envConstraint?.TryGetValue(path) is null)
            .ToImmutableArray();

        return
            [.. pathsBeforeSorting.Order(IntPathComparer.Instance)];
    }

    static IReadOnlyList<(Expression subExpr, IReadOnlyList<int> path)> SubsetOfExpressionsWithPaths(
        IReadOnlyList<Expression> expressions)
    {
        return
            [..expressions
                .SelectMany<Expression, (Expression subExpr, IReadOnlyList<int> path)>(subExpr =>
                CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(subExpr) is ExprMappedToParentEnv.PathInParentEnv subExprPath
                ?
                [(subExpr, subExprPath.Path)]
                :
                [])];
    }

    static IReadOnlyList<IReadOnlyList<int>> GetPathsCommonPrefixes(IReadOnlyList<IReadOnlyList<int>> paths)
    {
        bool keepPath(IReadOnlyList<int> path) =>
            !paths.Any(otherPath =>
            otherPath.Count < path.Count && otherPath.SequenceEqual(path.Take(otherPath.Count)));

        return
            [..paths
            .Where(keepPath)
            .Distinct(IntPathEqualityComparer.Instance)];
    }
}


public abstract record ExprResolvedInFunction
{
    public record ExprResolvedToFunctionParam(
        string ParameterName,
        IReadOnlyList<int> PathFromParam)
        : ExprResolvedInFunction;

    public record ExprResolvedToLiteral(PineValue Value)
        : ExprResolvedInFunction;
}

public record ExprFunctionCompilationInterface(
    IReadOnlyList<(IReadOnlyList<int> path, string paramName)> EnvItemsParamNames,
    string ArgumentEvalGenericName)
{
    public (string paramName, IReadOnlyList<int> pathFromParam)? GetParamForEnvItemPath(IReadOnlyList<int> path)
    {
        foreach (var (paramPath, paramName) in EnvItemsParamNames.OrderByDescending(param => param.path.Count))
        {
            if (paramPath.SequenceEqual(path.Take(paramPath.Count)))
            {
                return (paramName, [.. path.Skip(paramPath.Count)]);
            }
        }

        return null;
    }

    public virtual bool Equals(ExprFunctionCompilationInterface? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            EnvItemsParamNames.Count == other.EnvItemsParamNames.Count &&
            Enumerable.Zip(EnvItemsParamNames, other.EnvItemsParamNames, EnvItemsEqual).All(x => x) &&
            ArgumentEvalGenericName == other.ArgumentEvalGenericName;
    }

    override public int GetHashCode()
    {
        var hashCode = new HashCode();

        foreach (var (_, paramName) in EnvItemsParamNames)
        {
            hashCode.Add(paramName);
        }

        hashCode.Add(ArgumentEvalGenericName);

        return hashCode.ToHashCode();
    }

    static bool EnvItemsEqual(
        (IReadOnlyList<int> path, string paramName) itemA,
        (IReadOnlyList<int> path, string paramName) itemB) =>
        itemA.path.SequenceEqual(itemB.path) && itemA.paramName == itemB.paramName;

    public static IReadOnlyList<IReadOnlyList<int>> CompileEnvItemsPathsForExprFunction(
        Expression expression,
        EnvConstraintId? envConstraint)
    {
        var allSubexpressions =
            Expression.EnumerateSelfAndDescendants(expression)
            .ToImmutableArray();

        var allPaths = SubsetOfExpressionsWithPaths(allSubexpressions);

        var subExpressionsExceptUnderPaths =
            Expression.EnumerateSelfAndDescendants(
                expression,
                skipConditionalBranches: false,
                skipDescendants: subExpr => allPaths.Any(path => path.subExpr == subExpr))
            .ToImmutableArray();

        var pathsLessRedundant = SubsetOfExpressionsWithPaths(subExpressionsExceptUnderPaths);

        var pathsCommonPrefixes =
            GetPathsCommonPrefixes([.. pathsLessRedundant.Select(exprAndPath => exprAndPath.path)]);

        IReadOnlyList<IReadOnlyList<int>> pathsBeforeSorting =
            pathsCommonPrefixes
            .Where(path => envConstraint?.TryGetValue(path) is null)
            .ToImmutableArray();

        return
            [.. pathsBeforeSorting.Order(IntPathComparer.Instance)];
    }

    static IReadOnlyList<(Expression subExpr, IReadOnlyList<int> path)> SubsetOfExpressionsWithPaths(
        IReadOnlyList<Expression> expressions)
    {
        return
            [..expressions
                .SelectMany<Expression, (Expression subExpr, IReadOnlyList<int> path)>(subExpr =>
                CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(subExpr) is ExprMappedToParentEnv.PathInParentEnv subExprPath
                ?
                [(subExpr, subExprPath.Path)]
                :
                [])];
    }

    static IReadOnlyList<IReadOnlyList<int>> GetPathsCommonPrefixes(IReadOnlyList<IReadOnlyList<int>> paths)
    {
        bool keepPath(IReadOnlyList<int> path) =>
            !paths.Any(otherPath =>
            otherPath.Count < path.Count && otherPath.SequenceEqual(path.Take(otherPath.Count)));

        return
            [..paths
            .Where(keepPath)
            .Distinct(IntPathEqualityComparer.Instance)];
    }


    public static IReadOnlyList<(IReadOnlyList<int> path, string paramName)> EnvItemsParamNamesFromPaths(
        IReadOnlyList<IReadOnlyList<int>> paths,
        string commonPrefix) =>
        [.. paths.Select(path => (path, string.Join('_', [commonPrefix, .. path.Select(index => index.ToString())])))];

    public IReadOnlyList<Expression> ComposeArgumentsExpressionsForInvocation(
        Expression parentEnvExpr) =>
        [..EnvItemsParamNames
            .Select(pathAndParamName =>
            /*
             * Instead of using the generic expression reduction here, we could also specialize the building
             * of the expression for the given path, to apply the reduction earlier.
             * */
            ReducePineExpression.SearchForExpressionReductionRecursive(
                maxDepth: 5,
                PineCSharpSyntaxFactory.BuildPineExpressionToGetItemFromPath(
                    compositionExpr: parentEnvExpr,
                    path: pathAndParamName.path)))
            ];

    public static ExprResolvedInFunction? TryResolveExpressionInFunction(
        ExprFunctionCompilationInterface functionInterface,
        Expression expression,
        EnvConstraintId? envConstraint)
    {
        if (CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expression) is not { } exprMappedToParent)
            return null;

        if (exprMappedToParent is ExprMappedToParentEnv.LiteralInParentEnv asLiteral)
        {
            return new ExprResolvedInFunction.ExprResolvedToLiteral(asLiteral.Value);
        }

        if (exprMappedToParent is ExprMappedToParentEnv.PathInParentEnv pathInParentEnv)
        {
            if (envConstraint?.TryGetValue(pathInParentEnv.Path) is { } fromEnvConstraint)
            {
                return new ExprResolvedInFunction.ExprResolvedToLiteral(fromEnvConstraint);
            }

            /*
            var paramName =
                functionInterface.EnvItemsParamNames
                .FirstOrDefault(item => item.path.SequenceEqual(pathInParentEnv.Path)).paramName;

            if (paramName is not null)
                return new ExprResolvedInFunction.ExprResolvedToFunctionParam(paramName);
            */

            if (functionInterface.GetParamForEnvItemPath(pathInParentEnv.Path) is not { } match)
                return null;

            return
                new ExprResolvedInFunction.ExprResolvedToFunctionParam(
                    ParameterName: match.paramName,
                    PathFromParam: match.pathFromParam);
        }

        return null;
    }
}

public record CompilationUnitEnv(
    IReadOnlyDictionary<Expression, CompilationUnitEnvExprEntry> AvailableExpr,

    /*
     * The current implementation of the compiler starts emission before all dependencies are known.
     * Therefore, we define a default interface to be used for these later-discovered dependencies.
     * An alternative would be rearranging compilation to derive all dependencies in an earlier pass.
     * */
    ExprFunctionCompilationInterface DefaultInterface)
{
    public virtual bool Equals(CompilationUnitEnv? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            DefaultInterface.Equals(other.DefaultInterface) &&
            AvailableExpr.Count == other.AvailableExpr.Count &&
            AvailableExpr.All(pair =>
            other.AvailableExpr.TryGetValue(pair.Key, out var otherValue) &&
            pair.Value.Equals(otherValue));
    }

    override public int GetHashCode()
    {
        var hashCode = new HashCode();

        hashCode.Add(DefaultInterface);

        foreach (var pair in AvailableExpr)
        {
            hashCode.Add(pair.Key);
            hashCode.Add(pair.Value.GetHashCode());
        }

        return hashCode.ToHashCode();
    }

    public ExprFunctionCompilationInterface GetInterfaceForExprUsage(
        Expression expression,
        EnvConstraintId? envConstraint)
    {
        if (AvailableExpr.TryGetValue(expression, out var exprEntry))
        {
            if (envConstraint is null)
                return exprEntry.GenericReprInterface;

            if (exprEntry.AvailableSpecialized.TryGetValue(envConstraint, out var specializedInterface))
                return specializedInterface;
        }

        return DefaultInterface;
    }
}

public record CompilationUnitEnvExprEntry(
    ExprFunctionCompilationInterface GenericReprInterface,
    ImmutableDictionary<EnvConstraintId, ExprFunctionCompilationInterface> AvailableSpecialized)
{
    public virtual bool Equals(CompilationUnitEnvExprEntry? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            GenericReprInterface.Equals(other.GenericReprInterface) &&
            AvailableSpecialized.Count == other.AvailableSpecialized.Count &&
            AvailableSpecialized.All(pair =>
            other.AvailableSpecialized.TryGetValue(pair.Key, out var otherValue) &&
            pair.Value.Equals(otherValue));
    }

    override public int GetHashCode()
    {
        var hashCode = new HashCode();

        hashCode.Add(GenericReprInterface);

        foreach (var pair in AvailableSpecialized)
        {
            hashCode.Add(pair.Key);
            hashCode.Add(pair.Value.GetHashCode());
        }

        return hashCode.ToHashCode();
    }
}

public record ExpressionCompilationEnvironment(
    FunctionCompilationEnv FunctionEnvironment,
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
