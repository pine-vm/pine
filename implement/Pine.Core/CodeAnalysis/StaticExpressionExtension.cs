using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Helper extensions for working with <see cref="StaticExpression{TFunctionName}"/> trees.
/// Provides utilities for:
/// - discovering implicit parameters (environment paths),
/// - generic depth-first traversal,
/// - safe tree transformations with selective node replacement, and
/// - parsing/constructing paths encoded via <see cref="KernelFunction.head"/>/<see cref="KernelFunction.skip"/>.
/// </summary>
public static class StaticExpressionExtension
{
    /// <summary>
    /// Derives the implicit parameter list for a function from its body.
    /// </summary>
    /// <param name="functionBody">The static expression representing the function body.</param>
    /// <returns>
    /// A list of distinct paths to the environment expression used anywhere in <paramref name="functionBody"/>.
    /// </returns>
    /// <remarks>
    /// This helper walks the expression tree, collects all parameter reference nodes, removes duplicates while preserving
    /// structural equality, and produces a stable ordering by comparing the integer path components using
    /// <see cref="IntPathEqualityComparer"/> and <see cref="IntPathComparer"/>.
    /// The resulting sequence is suitable for generating a canonical function header such as
    /// <c>f param_1_0 param_1_2 = ...</c>, ensuring deterministic naming and ordering across builds and platforms.
    /// </remarks>
    public static IReadOnlyList<IReadOnlyList<int>> ImplicitFunctionParameterList<TFunctionName>(
        this StaticExpression<TFunctionName> functionBody,
        PineValueClass ignoreDeterminedByEnv)
    {
        var collectedPaths = new HashSet<IReadOnlyList<int>>(IntPathEqualityComparer.Instance);

        bool ShouldSkipDescendants(StaticExpression<TFunctionName> expr)
        {
            if (TryParseAsPathToExpression(
                expr,
                pathEndExpression: StaticExpression<TFunctionName>.EnvironmentInstance) is { } path)
            {
                if (ignoreDeterminedByEnv.TryGetValue(path) is not null)
                {
                    // This path is already determined by the environment; ignore it.
                    return true;
                }

                collectedPaths.Add(path);

                // We found a concrete path endpoint; no need to traverse below this node.
                return true;
            }

            return false;
        }

        foreach (var _ in EnumerateAllDescendants(functionBody, skipDescendants: ShouldSkipDescendants))
        {
            // Intentionally empty loop body; we only care about the side effect of collecting paths.
        }

        return
            [.. collectedPaths.Order(IntPathComparer.Instance)];
    }

    /// <summary>
    /// Enumerates the direct child expressions of the given static expression.
    /// </summary>
    /// <typeparam name="TFunctionName">Type of user-defined function identifiers.</typeparam>
    /// <param name="expression">Expression whose direct children to enumerate.</param>
    /// <returns>Direct child expressions in a stable order per expression kind.</returns>
    public static IEnumerable<StaticExpression<TFunctionName>> EnumerateDirectChildren<TFunctionName>(
        this StaticExpression<TFunctionName> expression)
    {
        switch (expression)
        {
            case StaticExpression<TFunctionName>.Literal:
            case StaticExpression<TFunctionName>.Environment:
                yield break;

            case StaticExpression<TFunctionName>.List list:
                for (var i = 0; i < list.Items.Count; i++)
                {
                    yield return list.Items[i];
                }
                yield break;

            case StaticExpression<TFunctionName>.Conditional conditional:
                yield return conditional.Condition;
                yield return conditional.FalseBranch;
                yield return conditional.TrueBranch;
                yield break;

            case StaticExpression<TFunctionName>.KernelApplication kernelApp:
                yield return kernelApp.Input;
                yield break;

            case StaticExpression<TFunctionName>.FunctionApplication functionApp:
                yield return functionApp.Arguments;
                yield break;

            case StaticExpression<TFunctionName>.CrashingParseAndEval crashing:
                yield return crashing.Encoded;
                yield return crashing.EnvironmentExpr;
                yield break;

            default:
                throw new NotSupportedException($"Unknown static expression type: {expression.GetType()}");
        }
    }

    /// <summary>
    /// Enumerates the expression and all of its descendants in depth-first (pre-order) sequence.
    /// The sequence starts with <paramref name="expression"/> itself.
    /// </summary>
    /// <param name="expression">Root expression to traverse.</param>
    /// <param name="skipDescendants">
    /// Optional predicate to determine whether to skip traversing the descendants of a given node.
    /// If the function returns <see langword="true"/> for a node, its descendants are not enumerated.
    /// </param>
    /// <returns>A depth-first traversal sequence over the expression tree.</returns>
    public static IEnumerable<StaticExpression<TFunctionName>> EnumerateAllDescendants<TFunctionName>(
        this StaticExpression<TFunctionName> expression,
        Func<StaticExpression<TFunctionName>, bool>? skipDescendants)
    {
        var stack = new Stack<StaticExpression<TFunctionName>>();

        stack.Push(expression);

        while (stack.Count > 0)
        {
            var current = stack.Pop();

            yield return current;

            if (skipDescendants?.Invoke(current) is true)
            {
                continue;
            }

            switch (current)
            {
                case StaticExpression<TFunctionName>.Literal:
                    break;

                case StaticExpression<TFunctionName>.Environment:
                    break;

                case StaticExpression<TFunctionName>.List list:

                    for (var i = 0; i < list.Items.Count; i++)
                    {
                        stack.Push(list.Items[i]);
                    }

                    break;

                case StaticExpression<TFunctionName>.Conditional conditional:
                    stack.Push(conditional.Condition);
                    stack.Push(conditional.FalseBranch);
                    stack.Push(conditional.TrueBranch);
                    break;

                case StaticExpression<TFunctionName>.KernelApplication kernelApp:
                    stack.Push(kernelApp.Input);
                    break;

                case StaticExpression<TFunctionName>.FunctionApplication functionApp:
                    stack.Push(functionApp.Arguments);
                    break;

                case StaticExpression<TFunctionName>.CrashingParseAndEval:
                    break;

                default:
                    throw new NotSupportedException($"Unknown static expression type: {current.GetType()}");
            }
        }
    }

    /// <summary>
    /// Transforms a static expression tree by attempting to replace nodes using a provided function.
    /// Returns the transformed expression along with a flag indicating whether the original
    /// environment is still referenced by any node in the transformed tree.
    /// </summary>
    /// <typeparam name="TFunctionName">The user-defined function identifier type used in the static tree.</typeparam>
    /// <param name="findReplacement">Function that returns a replacement expression for a node, or <c>null</c> to keep it.</param>
    /// <param name="expression">The root expression to transform.</param>
    /// <returns>The transformed expression and whether it references the original environment.</returns>
    /// <remarks>
    /// Replacement short-circuits traversal: If <paramref name="findReplacement"/> returns a non-null value for a node,
    /// that replacement node is used, and the original node's descendants are not visited.
    /// The returned <c>referencesOriginalEnv</c> flag only accounts for <see cref="StaticExpression{TFunctionName}.Environment"/>
    /// nodes that originated from the input tree and were not replaced; any <see cref="StaticExpression{TFunctionName}.Environment"/>
    /// nodes introduced by replacements are intentionally not counted as references to the original environment.
    /// </remarks>
    public static (StaticExpression<TFunctionName> expr, bool referencesOriginalEnv)
        TransformStaticExpressionWithOptionalReplacement<TFunctionName>(
            Func<StaticExpression<TFunctionName>, StaticExpression<TFunctionName>?> findReplacement,
            StaticExpression<TFunctionName> expression)
    {
        if (findReplacement(expression) is { } fromReplacement)
            return (fromReplacement, false);

        switch (expression)
        {
            case StaticExpression<TFunctionName>.Literal:
                return (expression, false);

            case StaticExpression<TFunctionName>.Environment:
                return (expression, true);

            case StaticExpression<TFunctionName>.List list:
                {
                    var referencesOriginalEnv = false;

                    var mappedItems = new StaticExpression<TFunctionName>[list.Items.Count];

                    for (var i = 0; i < list.Items.Count; i++)
                    {
                        var (mappedItem, itemReferencesOriginalEnv) =
                            TransformStaticExpressionWithOptionalReplacement(
                                findReplacement: findReplacement,
                                expression: list.Items[i]);

                        mappedItems[i] = mappedItem;
                        referencesOriginalEnv = referencesOriginalEnv || itemReferencesOriginalEnv;
                    }

                    return (StaticExpression<TFunctionName>.ListInstance(mappedItems), referencesOriginalEnv);
                }

            case StaticExpression<TFunctionName>.Conditional conditional:
                {
                    var conditionTransform =
                        TransformStaticExpressionWithOptionalReplacement(
                            findReplacement,
                            conditional.Condition);

                    var trueBranchTransform =
                        TransformStaticExpressionWithOptionalReplacement(
                            findReplacement,
                            conditional.TrueBranch);

                    var falseBranchTransform =
                        TransformStaticExpressionWithOptionalReplacement(
                            findReplacement,
                            conditional.FalseBranch);

                    return (
                        StaticExpression<TFunctionName>.ConditionalInstance(
                            conditionTransform.expr,
                            falseBranchTransform.expr,
                            trueBranchTransform.expr
                        ),
                        conditionTransform.referencesOriginalEnv ||
                        falseBranchTransform.referencesOriginalEnv ||
                        trueBranchTransform.referencesOriginalEnv);
                }

            case StaticExpression<TFunctionName>.KernelApplication kernelApp:
                {
                    var argumentTransform =
                        TransformStaticExpressionWithOptionalReplacement(
                            findReplacement,
                            kernelApp.Input);

                    return (
                        StaticExpression<TFunctionName>.KernelApplicationInstance(
                            kernelApp.Function,
                            argumentTransform.expr
                        ),
                        argumentTransform.referencesOriginalEnv);
                }

            case StaticExpression<TFunctionName>.FunctionApplication functionApp:
                {
                    var argumentsTransform =
                        TransformStaticExpressionWithOptionalReplacement(
                            findReplacement,
                            functionApp.Arguments);

                    return (
                        StaticExpression<TFunctionName>.FunctionApplicationInstance(
                            functionApp.FunctionName,
                            argumentsTransform.expr
                        ),
                        argumentsTransform.referencesOriginalEnv);
                }

            case StaticExpression<TFunctionName>.CrashingParseAndEval crashing:
                {
                    var encodedTransform =
                        TransformStaticExpressionWithOptionalReplacement(
                            findReplacement,
                            crashing.Encoded);

                    var envTransform =
                        TransformStaticExpressionWithOptionalReplacement(
                            findReplacement,
                            crashing.EnvironmentExpr);

                    return (
                        new StaticExpression<TFunctionName>.CrashingParseAndEval(
                            encodedTransform.expr,
                            envTransform.expr
                        ),
                        encodedTransform.referencesOriginalEnv || envTransform.referencesOriginalEnv);
                }
        }

        throw new NotSupportedException(
            $"Unknown static expression type: {expression.GetType().FullName}");
    }


    /// <summary>
    /// Attempts to parse a chain of nested <see cref="KernelFunction.head"/> (and optional <see cref="KernelFunction.skip"/>) kernel applications
    /// representing a path from an arbitrary starting expression back to a designated <paramref name="pathEndExpression"/>.
    /// </summary>
    /// <param name="expression">The root expression that potentially encodes a path.</param>
    /// <param name="pathEndExpression">The terminal expression that marks the end (origin) of the path (typically <see cref="StaticExpression{TFunctionName}.EnvironmentInstance"/>).</param>
    /// <returns>
    /// A list of integer offsets describing the path if <paramref name="expression"/> is a valid encoding; otherwise <c>null</c>.
    /// Offsets are ordered from the outermost application (closest to <paramref name="expression"/>) to the innermost
    /// (closest to <paramref name="pathEndExpression"/>). Zero denotes a direct <c>head</c> without an intervening <c>skip</c>.
    /// </returns>
    /// <remarks>
    /// The recognized shape is a (possibly empty) sequence of kernel applications with the outermost node being
    /// a <see cref="KernelFunction.head"/>. A non-zero offset step is represented as <c>head (skip N NEXT)</c>, while a zero offset is
    /// simply <c>head NEXT</c>. Parsing stops successfully when <paramref name="pathEndExpression"/> is reached.
    /// Any deviation from this pattern causes the method to return <c>null</c>.
    /// </remarks>
    public static IReadOnlyList<int>? TryParseAsPathToExpression<TFunctionName>(
        StaticExpression<TFunctionName> expression,
        StaticExpression<TFunctionName> pathEndExpression)
    {
        var path = new List<int>();

        var current = expression;

        while (true)
        {
            if (current == pathEndExpression)
            {
                path.Reverse();

                return path;
            }

            if (current is not StaticExpression<TFunctionName>.KernelApplication outerKernelApp ||
                outerKernelApp.Function is not nameof(KernelFunction.head))
            {
                return null;
            }

            var next = outerKernelApp.Input;
            var offset = 0;

            if (next is StaticExpression<TFunctionName>.KernelApplication innerKernelApp &&
                innerKernelApp.Function is nameof(KernelFunction.skip) &&
                innerKernelApp.Input is StaticExpression<TFunctionName>.List skipList &&
                skipList.Items.Count is 2 &&
                skipList.Items[0] is StaticExpression<TFunctionName>.Literal skipLiteral &&
                KernelFunction.SignedIntegerFromValueRelaxed(skipLiteral.Value) is { } skipInteger)
            {
                offset = (int)skipInteger;
                next = skipList.Items[1];
            }

            path.Add(offset);
            current = next;
        }
    }

    /// <summary>
    /// Attempts to partially resolve a path (as used by <see cref="BuildPathToExpression"/>) against an already materialized
    /// static expression tree. Leading offsets that index directly into existing <see cref="StaticExpression{TFunctionName}.List"/> nodes are consumed and
    /// the traversal advances into the corresponding child. Once the traversal can no longer continue (because the current
    /// node is not a <see cref="StaticExpression{TFunctionName}.List"/> or an offset is out of range), the remaining suffix of the path is re-encoded as
    /// kernel applications (using <see cref="KernelFunction.head"/> / <see cref="KernelFunction.skip"/>) starting from the last resolved expression.
    /// </summary>
    /// <param name="path">Full sequence of integer offsets describing a path toward <paramref name="pathEndExpression"/>.</param>
    /// <param name="pathEndExpression">The starting (deepest) expression the path ultimately points to; usually the function environment.</param>
    /// <returns>
    /// A static expression equivalent to one built from <paramref name="path"/> by <see cref="BuildPathToExpression"/>, but
    /// with an initial segment already resolved into existing list nodes where possible. This shortens the resulting encoded path
    /// and avoids redundant kernel applications.
    /// </returns>
    /// <remarks>
    /// Example: If <paramref name="pathEndExpression"/> is already a list <c>[A, B, C]</c> and <paramref name="path"/> is <c>[2, 0, 1]</c>,
    /// the function first advances to item index 2 (<c>C</c>) and then encodes the remaining <c>[0,1]</c> relative to <c>C</c>.
    /// </remarks>
    public static StaticExpression<TFunctionName> BuildReducedPathToExpression<TFunctionName>(
        IReadOnlyList<int> path,
        StaticExpression<TFunctionName> pathEndExpression)
    {
        var current = pathEndExpression;

        var reducedPathIndex = 0;

        while (reducedPathIndex < path.Count)
        {
            var nextOffset = path[reducedPathIndex];

            if (current is not StaticExpression<TFunctionName>.List listExpr)
            {
                break;
            }

            if (nextOffset < 0 || nextOffset >= listExpr.Items.Count)
            {
                break;
            }

            current = listExpr.Items[nextOffset];
            ++reducedPathIndex;
        }

        return
            BuildPathToExpression([.. path.Skip(reducedPathIndex)], current);
    }

    /// <summary>
    /// Builds an expression that encodes a path as a chain of <see cref="KernelFunction.head"/> (and optional <see cref="KernelFunction.skip"/>) kernel applications
    /// terminating at <paramref name="pathEndExpression"/>.
    /// </summary>
    /// <param name="path">Sequence of integer offsets describing steps from the start toward the end expression. Order must match that returned by <see cref="TryParseAsPathToExpression"/>.</param>
    /// <param name="pathEndExpression">The terminal expression that the constructed path should reference (typically the environment instance).</param>
    /// <returns>An expression representing the encoded path.</returns>
    /// <remarks>
    /// Each offset <c>n</c> produces either <c>head CURRENT</c> (when <c>n == 0</c>) or
    /// <c>head (skip n CURRENT)</c> (when <c>n != 0</c>). Offsets are applied sequentially in the order they appear
    /// in <paramref name="path"/>. Passing the resulting expression to <see cref="TryParseAsPathToExpression"/>
    /// with the same <paramref name="pathEndExpression"/> will yield the original <paramref name="path"/>.
    /// </remarks>
    public static StaticExpression<TFunctionName> BuildPathToExpression<TFunctionName>(
        IReadOnlyList<int> path,
        StaticExpression<TFunctionName> pathEndExpression)
    {
        var current = pathEndExpression;

        for (var i = 0; i < path.Count; ++i)
        {
            var offset = path[i];

            if (offset is 0)
            {
                current =
                    StaticExpression<TFunctionName>.KernelApplicationInstance(
                        nameof(KernelFunction.head),
                        current);
            }
            else
            {
                current =
                    StaticExpression<TFunctionName>.KernelApplicationInstance(
                        nameof(KernelFunction.head),
                        StaticExpression<TFunctionName>.KernelApplicationInstance(
                            nameof(KernelFunction.skip),
                            StaticExpression<TFunctionName>.ListInstance(
                                [StaticExpression<TFunctionName>.LiteralInstance(IntegerEncoding.EncodeSignedInteger(offset)),
                                 current])));
            }
        }

        return current;
    }

    /// <summary>
    /// Collects all paths to the environment expression that are reachable without traversing into
    /// user-defined function applications.
    /// </summary>
    /// <typeparam name="TFuncId">Type of user-defined function identifiers.</typeparam>
    /// <param name="expr">Root expression to analyze.</param>
    /// <returns>
    /// A sequence of paths (lists of integer offsets) pointing to occurrences of
    /// <see cref="StaticExpression{TFuncId}.Environment"/> encoded via
    /// <see cref="KernelFunction.head"/>/<see cref="KernelFunction.skip"/>. Only occurrences that are
    /// outside any <see cref="StaticExpression{TFuncId}.FunctionApplication"/> are returned.
    /// </returns>
    /// <remarks>
    /// This method performs a breadth-first traversal. When an encoded environment path is found at a node,
    /// that subtree is not traversed further. Subtrees rooted at <see cref="StaticExpression{TFuncId}.FunctionApplication"/>
    /// are skipped entirely.
    /// </remarks>
    public static IEnumerable<IReadOnlyList<int>> CollectEnvPathsOutsideFunctionApplications<TFuncId>(
        StaticExpression<TFuncId> expr)
    {
        var queue = new Queue<StaticExpression<TFuncId>>([expr]);

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();

            if (TryParseAsPathToExpression(current, StaticExpression<TFuncId>.EnvironmentInstance) is { } path)
            {
                yield return path;
                continue;
            }

            if (current is StaticExpression<TFuncId>.FunctionApplication)
            {
                // Do not recurse into function applications
                continue;
            }

            foreach (var child in EnumerateDirectChildren(current))
            {
                queue.Enqueue(child);
            }
        }
    }

    /// <summary>
    /// Traverses the expression following the given list-index path as far as possible and
    /// returns the deepest subexpression reached together with the remaining, unresolved path.
    /// </summary>
    /// <typeparam name="TFunctionName">Type of user-defined function identifiers.</typeparam>
    /// <param name="expr">The expression to traverse from.</param>
    /// <param name="path">A sequence of integer offsets to follow through nested <see cref="StaticExpression{TFunctionName}.List"/> nodes.</param>
    /// <returns>
    /// A tuple containing the reached subexpression (<c>subexpr</c>) and the suffix of <paramref name="path"/>
    /// that could not be consumed (<c>pathRemaining</c>) either because a non-list node was encountered or an index was out of range.
    /// </returns>
    /// <remarks>
    /// The traversal logic mirrors the resolving step used by <see cref="BuildReducedPathToExpression"/>,
    /// but instead of re-encoding the remainder as kernel applications, it returns the unresolved suffix as integers.
    /// </remarks>
    public static (StaticExpression<TFunctionName> subexpr, IReadOnlyList<int> pathRemaining)
        GetSubexpressionAtPath<TFunctionName>(
        this StaticExpression<TFunctionName> expr,
        IReadOnlyList<int> path)
    {
        var current = expr;
        var currentIndex = 0;

        while (currentIndex < path.Count)
        {
            var nextOffset = path[currentIndex];

            if (current is not StaticExpression<TFunctionName>.List listExpr)
            {
                break;
            }

            if (nextOffset < 0 || nextOffset >= listExpr.Items.Count)
            {
                break;
            }

            current = listExpr.Items[nextOffset];
            ++currentIndex;
        }

        return (current, [.. path.Skip(currentIndex)]);
    }

    /// <summary>
    /// If the argument at <paramref name="paramPath"/> within the given function application is a
    /// concatenation of the same parameter path with additional items, return those appended items.
    /// </summary>
    /// <param name="funcAppExpr">The function application expression whose arguments are inspected.</param>
    /// <param name="paramPath">The path (list indices) to locate within <paramref name="funcAppExpr"/> arguments.</param>
    /// <returns>
    /// The sequence of expressions appended after the recursive reference to <paramref name="paramPath"/>,
    /// or <c>null</c> if the shape does not match <c>concat [ path-to-env(paramPath), ...items ]</c> exactly.
    /// </returns>
    /// <remarks>
    /// This recognizes an expression of the form <c>concat [ P, x1, x2, ... ]</c> where <c>P</c> is an encoded path to the
    /// environment that is equal to <paramref name="paramPath"/>. If the argument located at <paramref name="paramPath"/>
    /// cannot be fully resolved within the arguments list, or does not have this <see cref="KernelFunction.concat"/> shape,
    /// the method returns <c>null</c>.
    /// </remarks>
    public static IReadOnlyList<StaticExpression<DeclQualifiedName>>?
        ParseParamPathAsAppendItemsInFunctionApplication(
        StaticExpression<DeclQualifiedName>.FunctionApplication funcAppExpr,
        IReadOnlyList<int> paramPath)
    {
        var (subexpr, pathRemaining) =
            GetSubexpressionAtPath(funcAppExpr.Arguments, paramPath);

        if (pathRemaining.Count is not 0)
            return null;

        if (subexpr is not StaticExpression<DeclQualifiedName>.KernelApplication kernelApp)
        {
            return null;
        }

        if (kernelApp.Function is not nameof(KernelFunction.concat))
        {
            return null;
        }

        if (kernelApp.Input is not StaticExpression<DeclQualifiedName>.List concatInputList)
        {
            return null;
        }

        if (concatInputList.Items.Count is 0)
        {
            return null;
        }

        var firstItem = concatInputList.Items[0];

        // First item must be a recursive reference to the same parameter path

        if (TryParseAsPathToExpression(
            firstItem,
            StaticExpression<DeclQualifiedName>.EnvironmentInstance) is not { } firstItemPath)
        {
            return null;
        }

        if (!IntPathEqualityComparer.Instance.Equals(firstItemPath, paramPath))
        {
            return null;
        }

        return [.. concatInputList.Items.Skip(1)];
    }

    /// <summary>
    /// Determines whether the expression contains any occurrence of the given environment path.
    /// </summary>
    /// <param name="expr">The expression to search.</param>
    /// <param name="path">The environment path to look for.</param>
    /// <returns><see langword="true"/> if the path is mentioned anywhere within <paramref name="expr"/>; otherwise <see langword="false"/>.</returns>
    public static bool MentionsEnvPath(
        StaticExpression<DeclQualifiedName> expr,
        IReadOnlyList<int> path)
    {
        if (TryParseAsPathToExpression(
            expr,
            StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } currentEnvPath)
        {
            if (currentEnvPath.Count < path.Count)
                return false;

            for (var i = 0; i < path.Count; i++)
            {
                if (currentEnvPath[i] != path[i])
                    return false;
            }

            return true;
        }

        switch (expr)
        {
            case StaticExpression<DeclQualifiedName>.Literal:
            case StaticExpression<DeclQualifiedName>.Environment:
                return false;

            case StaticExpression<DeclQualifiedName>.List list:

                foreach (var item in list.Items)
                {
                    if (MentionsEnvPath(item, path))
                        return true;
                }

                return false;

            case StaticExpression<DeclQualifiedName>.Conditional cond:
                return MentionsEnvPath(cond.Condition, path)
                    || MentionsEnvPath(cond.TrueBranch, path)
                    || MentionsEnvPath(cond.FalseBranch, path);

            case StaticExpression<DeclQualifiedName>.KernelApplication k:
                return MentionsEnvPath(k.Input, path);

            case StaticExpression<DeclQualifiedName>.FunctionApplication f:
                return MentionsEnvPath(f.Arguments, path);

            case StaticExpression<DeclQualifiedName>.CrashingParseAndEval parseAndEval:
                return
                    MentionsEnvPath(parseAndEval.Encoded, path) ||
                    MentionsEnvPath(parseAndEval.EnvironmentExpr, path);

            default:
                throw new NotImplementedException(
                    "Internal error: Unknown expression type in MentionsEnvPath: " +
                    expr.GetType().FullName);
        }
    }

    /// <summary>
    /// Iteratively peels a chain of <see cref="KernelFunction.head"/> (and optional <see cref="KernelFunction.skip"/>)
    /// applications and yields the progressively decoded subexpression together with the path built so far.
    /// </summary>
    /// <typeparam name="TFunctionName">Type of user-defined function identifiers.</typeparam>
    /// <param name="expression">The expression to interpret as a path.</param>
    /// <returns>
    /// A sequence of pairs where <c>pathInSubexpr</c> is the path collected so far and <c>subexpr</c> is the remaining
    /// inner expression after removing the corresponding outer applications. The path is in the reverse order compared to
    /// <see cref="TryParseAsPathToExpression{TFunctionName}(StaticExpression{TFunctionName}, StaticExpression{TFunctionName})"/>.
    /// </returns>
    public static IEnumerable<(IReadOnlyList<int> pathInSubexpr, StaticExpression<TFunctionName> subexpr)>
        InterpretAsPathReversed<TFunctionName>(StaticExpression<TFunctionName> expression)
    {
        var pathSegments = new List<int>();

        var currentExpr = expression;

        while (currentExpr is StaticExpression<TFunctionName>.KernelApplication currentKernelApp)
        {
            if (currentKernelApp.Function is not nameof(KernelFunction.head))
                break;

            var currentOffset = 0;

            currentExpr = currentKernelApp.Input;

            if (currentExpr is StaticExpression<TFunctionName>.KernelApplication inputKernelApp &&
                inputKernelApp.Function is nameof(KernelFunction.skip) &&
                inputKernelApp.Input is StaticExpression<TFunctionName>.List skipInputList &&
                skipInputList.Items.Count is 2)
            {
                if (skipInputList.Items[0] is StaticExpression<TFunctionName>.Literal skipCountLiteral &&
                    KernelFunction.SignedIntegerFromValueRelaxed(skipCountLiteral.Value) is { } skipCount)
                {
                    currentOffset = (int)skipCount;

                    currentExpr = skipInputList.Items[1];
                }
            }

            pathSegments.Insert(0, currentOffset);

            yield return (pathSegments.ToArray(), currentExpr);
        }
    }

    /// <summary>
    /// Enumerates function applications that appear in tail position of the given expression.
    /// Includes the root itself when it is a function application and both branches of conditionals.
    /// </summary>
    /// <typeparam name="TFunctionName">Type of user-defined function identifiers.</typeparam>
    /// <param name="expression">The expression to analyze for tail calls.</param>
    /// <returns>An enumeration of function applications in tail position.</returns>
    public static IEnumerable<StaticExpression<TFunctionName>.FunctionApplication> EnumerateTailCalls<TFunctionName>(
        this StaticExpression<TFunctionName> expression)
    {
        if (expression is StaticExpression<TFunctionName>.FunctionApplication funcApp)
        {
            yield return funcApp;
        }

        if (expression is StaticExpression<TFunctionName>.Conditional conditional)
        {
            foreach (var fromTrue in EnumerateTailCalls(conditional.TrueBranch))
            {
                yield return fromTrue;
            }

            foreach (var fromFalse in EnumerateTailCalls(conditional.FalseBranch))
            {
                yield return fromFalse;
            }
        }
    }

    /// <summary>
    /// Transforms all tail-position expressions within the expression using the provided mapping function.
    /// Non-tail nodes are left unchanged.
    /// </summary>
    /// <typeparam name="TFunctionName">Type of user-defined function identifiers.</typeparam>
    /// <param name="expression">The root expression whose tail calls should be transformed.</param>
    /// <param name="mapTail">A mapping function applied to each tail-position <see cref="StaticExpression{TFunctionName}.FunctionApplication"/>.</param>
    /// <returns>The expression with tail calls transformed by <paramref name="mapTail"/>.</returns>
    public static StaticExpression<TFunctionName> TransformTails<TFunctionName>(
        this StaticExpression<TFunctionName> expression,
        Func<StaticExpression<TFunctionName>, StaticExpression<TFunctionName>> mapTail)
    {
        if (expression is StaticExpression<TFunctionName>.Conditional conditional)
        {
            return
                StaticExpression<TFunctionName>.ConditionalInstance(
                    condition: conditional.Condition,
                    trueBranch: TransformTails(conditional.TrueBranch, mapTail),
                    falseBranch: TransformTails(conditional.FalseBranch, mapTail));
        }

        return mapTail(expression);
    }
}
