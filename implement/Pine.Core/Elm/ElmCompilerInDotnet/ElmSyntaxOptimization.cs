using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Transformations and analyses of Elm syntax used by optimization stages.
/// </summary>
public partial class ElmSyntaxOptimization
{
    /// <summary>
    /// Applies the wrap/unwrap cancellation rewrite when enabled.
    /// </summary>
    public static OptimizedElmSyntaxDeclarations ApplyWrapUnwrapCancellation(
        OptimizedElmSyntaxDeclarations declarations,
        Config config)
    {
        if (!config.WrapUnwrapCancellationEnabled)
            return declarations;

        return WrapUnwrapCancellation.RewriteDeclarationDictionary(declarations);
    }

    /// <summary>
    /// Applies the sibling-aware wrap/unwrap cancellation rewrite when enabled.
    /// </summary>
    internal static OptimizedElmSyntaxDeclarations ApplyWrapUnwrapCancellation(
        OptimizedElmSyntaxDeclarations declarations,
        Config config,
        ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl> siblingsByOriginal)
    {
        if (!config.WrapUnwrapCancellationEnabled)
            return declarations;

        return WrapUnwrapCancellation.RewriteDeclarationDictionary(declarations, siblingsByOriginal);
    }

    /// <summary>
    /// Returns the functions that participate in a recursive call cycle.
    /// </summary>
    internal static ImmutableHashSet<DeclQualifiedName> MarkRecursiveFunctions(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.FunctionStruct> functions)
    {
        var directReferences =
            new Dictionary<DeclQualifiedName, HashSet<DeclQualifiedName>>(functions.Count);

        foreach (var (functionName, function) in functions)
        {
            var references = new HashSet<DeclQualifiedName>();

            foreach (var referencedName in CollectFunctionReferences(function.Declaration.Expression))
            {
                if (functions.ContainsKey(referencedName))
                    references.Add(referencedName);
            }

            directReferences[functionName] = references;
        }

        return ComputeRecursiveNodes(directReferences).ToImmutableHashSet();
    }

    /// <summary>
    /// Enumerates all identifier references in an expression.
    /// </summary>
    internal static IEnumerable<DeclQualifiedName> CollectFunctionReferences(
        SyntaxTypes.Expression expression)
    {
        var references = new List<DeclQualifiedName>();

        SyntaxTypes.SyntaxAnalysis.WalkExpressionsWithScope(
            expression,
            [],
            (node, _) =>
            {
                if (node is SyntaxTypes.Expression.Identifier identifier)
                    references.Add(identifier.QualifiedName);
            });

        return references;
    }

    /// <summary>
    /// Returns all nodes in a directed graph that can reach themselves.
    /// </summary>
    private static HashSet<DeclQualifiedName> ComputeRecursiveNodes(
        Dictionary<DeclQualifiedName, HashSet<DeclQualifiedName>> graph)
    {
        var index = 0;
        var indices = new Dictionary<DeclQualifiedName, int>(graph.Count);
        var lowlink = new Dictionary<DeclQualifiedName, int>(graph.Count);
        var onStack = new HashSet<DeclQualifiedName>();
        var stronglyConnectedComponentStack = new Stack<DeclQualifiedName>();
        var recursive = new HashSet<DeclQualifiedName>();

        foreach (var (node, adjacentNodes) in graph)
        {
            if (adjacentNodes.Contains(node))
                recursive.Add(node);
        }

        var work =
            new Stack<(
                DeclQualifiedName Node,
                IEnumerator<DeclQualifiedName> AdjacentNodes)>();

        foreach (var startNode in graph.Keys)
        {
            if (indices.ContainsKey(startNode))
                continue;

            indices[startNode] = index;
            lowlink[startNode] = index;
            index++;
            stronglyConnectedComponentStack.Push(startNode);
            onStack.Add(startNode);
            work.Push((startNode, graph[startNode].GetEnumerator()));

            while (work.Count > 0)
            {
                var frame = work.Peek();
                var node = frame.Node;

                if (frame.AdjacentNodes.MoveNext())
                {
                    var adjacentNode = frame.AdjacentNodes.Current;

                    if (!graph.ContainsKey(adjacentNode))
                        continue;

                    if (!indices.ContainsKey(adjacentNode))
                    {
                        indices[adjacentNode] = index;
                        lowlink[adjacentNode] = index;
                        index++;
                        stronglyConnectedComponentStack.Push(adjacentNode);
                        onStack.Add(adjacentNode);
                        work.Push((adjacentNode, graph[adjacentNode].GetEnumerator()));
                    }
                    else if (onStack.Contains(adjacentNode) &&
                        indices[adjacentNode] < lowlink[node])
                    {
                        lowlink[node] = indices[adjacentNode];
                    }
                }
                else
                {
                    work.Pop();

                    if (lowlink[node] == indices[node])
                    {
                        var component = new List<DeclQualifiedName>();
                        DeclQualifiedName componentNode;

                        do
                        {
                            componentNode = stronglyConnectedComponentStack.Pop();
                            onStack.Remove(componentNode);
                            component.Add(componentNode);
                        }
                        while (!componentNode.Equals(node));

                        if (component.Count > 1)
                        {
                            foreach (var recursiveNode in component)
                                recursive.Add(recursiveNode);
                        }
                    }

                    if (work.Count > 0)
                    {
                        var parent = work.Peek().Node;

                        if (lowlink[node] < lowlink[parent])
                            lowlink[parent] = lowlink[node];
                    }
                }
            }
        }

        return recursive;
    }
}
