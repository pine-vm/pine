using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Interface to call a function.
/// </summary>
public record StaticFunctionInterface
{
    /// <summary>
    /// Gets the collection of parameter paths represented as sequences of indexes.
    /// </summary>
    public IReadOnlyList<IReadOnlyList<int>> ParamsPaths { get; }

    private readonly string _hashString;

    private readonly int _hashCode;

    /// <summary>
    /// Only a single parameter containing the whole environment.
    /// </summary>
    public static readonly StaticFunctionInterface Generic = new([[]]);

    /// <summary>
    /// Instance with zero parameters.
    /// </summary>
    public static readonly StaticFunctionInterface ZeroParameters = new([]);

    private StaticFunctionInterface(
        IReadOnlyList<IReadOnlyList<int>> paramsPaths)
    {
        ParamsPaths = paramsPaths;

        _hashString = ComputeHashString(ParamsPaths);

        _hashCode = _hashString.GetHashCode();
    }

    /// <summary>
    /// Creates a <see cref="StaticFunctionInterface"/> instance from an arbitrary enumeration of
    /// parameter paths, sorting them deterministically using <c>IntPathComparer</c>.
    /// </summary>
    /// <param name="paramsPaths">Enumeration of parameter reference paths (unsorted).</param>
    /// <returns>A <see cref="StaticFunctionInterface"/> instance with paths sorted.</returns>
    public static StaticFunctionInterface FromPathsSorted(
        IReadOnlyList<IReadOnlyList<int>> paramsPaths)
    {
        if (paramsPaths.Count is 0 && ZeroParameters is not null)
        {
            return ZeroParameters;
        }

        if (paramsPaths.Count is 1 && paramsPaths[0].Count is 0 && Generic is not null)
        {
            return Generic;
        }

        return
            new StaticFunctionInterface([.. paramsPaths.Order(IntPathComparer.Instance)]);
    }

    /// <summary>
    /// Builds a <see cref="StaticFunctionInterface"/> based on the parameters referenced within the supplied expression.
    /// </summary>
    /// <param name="expression">Expression used to infer referenced environment parameter paths.</param>
    /// <returns>A <see cref="StaticFunctionInterface"/> representing the inferred parameter set.</returns>
    public static StaticFunctionInterface FromExpression(
        Expression expression)
    {
        var envPaths =
            UnpackedParamsFiltered(expression)
            .Select(m => (IReadOnlyList<int>)[.. m.Span]);

        return FromPathsSorted([.. envPaths]);
    }

    /// <summary>
    /// Enumerates the distinct environment paths referenced by the expression while removing entries that are redundant because of broader paths.
    /// </summary>
    /// <param name="rootExpression">Root expression to inspect.</param>
    /// <returns>Filtered collection of environment reference paths.</returns>
    public static IEnumerable<ReadOnlyMemory<int>> UnpackedParamsFiltered(
        Expression rootExpression)
    {
        IReadOnlyList<ReadOnlyMemory<int>> unfiltered =
            [.. EnumerateUnpackedParamsDistinct(rootExpression)];

        bool IsCoveredByOther(ReadOnlyMemory<int> path)
        {
            foreach (var otherPath in unfiltered)
            {
                if (path.Length <= otherPath.Length)
                    continue;

                if (path.Span[..otherPath.Length].SequenceEqual(path.Span))
                    return true;
            }

            return false;
        }

        foreach (var path in unfiltered)
        {
            if (IsCoveredByOther(path))
                continue;

            yield return path;
        }
    }

    private static IEnumerable<ReadOnlyMemory<int>> EnumerateUnpackedParamsDistinct(
        Expression rootExpression)
    {
        var alreadySeen = new HashSet<Expression>();

        var stack = new Stack<Expression>([rootExpression]);

        while (stack.TryPop(out var expression))
        {
            if (!expression.ReferencesEnvironment)
                continue;

            if (alreadySeen.Contains(expression))
                continue;

            alreadySeen.Add(expression);

            if (CodeAnalysis.TryParseExprAsPathInEnv(expression) is { } path)
            {
                yield return new ReadOnlyMemory<int>([.. path]);

                continue;
            }

            switch (expression)
            {
                case Expression.Environment:
                    yield return ReadOnlyMemory<int>.Empty;
                    break;

                case Expression.Literal:
                    break;

                case Expression.List list:
                    for (var i = 0; i < list.Items.Count; i++)
                    {
                        stack.Push(list.Items[i]);
                    }
                    break;

                case Expression.ParseAndEval parseAndEvaluate:

                    stack.Push(parseAndEvaluate.Encoded);
                    stack.Push(parseAndEvaluate.Environment);

                    break;

                case Expression.KernelApplication kernelApplication:

                    stack.Push(kernelApplication.Input);

                    break;

                case Expression.Conditional conditional:

                    stack.Push(conditional.Condition);
                    stack.Push(conditional.FalseBranch);
                    stack.Push(conditional.TrueBranch);

                    break;

                case Expression.StringTag stringTag:

                    stack.Push(stringTag.Tagged);

                    break;

                default:
                    throw new NotImplementedException(
                        "Unknown expression type: " + expression.GetType().FullName);
            }
        }
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        return _hashCode;
    }

    /// <inheritdoc/>
    public virtual bool Equals(StaticFunctionInterface? other)
    {
        return
            other is not null &&
            _hashCode == other._hashCode &&
            _hashString == other._hashString;
    }

    /// <summary>
    /// Builds a hash string representation for a collection of parameter paths.
    /// The string is distinct for distinct collections of paths, to guarantee there are no hash collisions.
    /// </summary>
    /// <param name="paramsPaths">Sorted list of paths.</param>
    private static string ComputeHashString(IReadOnlyList<IReadOnlyList<int>> paramsPaths)
    {
        var builder = new System.Text.StringBuilder(capacity: 100);

        builder.Append(paramsPaths.Count);

        for (var i = 0; i < paramsPaths.Count; i++)
        {
            if (i > 0)
            {
                builder.Append('|');
            }

            var path = paramsPaths[i];

            builder.Append(path.Count);

            for (var j = 0; j < path.Count; j++)
            {
                builder.Append(',');

                builder.Append(path[j]);
            }
        }

        return builder.ToString();
    }
}
