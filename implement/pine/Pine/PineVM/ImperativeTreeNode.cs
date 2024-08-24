using System.Collections.Generic;

namespace Pine.PineVM;

public abstract record ImperativeNode
{
    public record LeafNode(Expression Expression)
        : ImperativeNode;

    public record ConditionalNode(
        Expression.Conditional Origin,
        ImperativeNode Condition,
        ImperativeNode FalseBranch,
        ImperativeNode TrueBranch,
        ImperativeNode Continuation)
        : ImperativeNode;

    public static IEnumerable<ImperativeNode> EnumerateSelfAndDescendants(
        ImperativeNode rootNode,
        bool skipBranches)
    {
        var stack = new Stack<ImperativeNode>([rootNode]);

        while (stack.Count > 0)
        {
            var node = stack.Pop();

            yield return node;

            if (node is ConditionalNode conditional)
            {
                stack.Push(conditional.Condition);

                if (!skipBranches)
                {
                    stack.Push(conditional.FalseBranch);
                    stack.Push(conditional.TrueBranch);
                }

                stack.Push(conditional.Continuation);
            }
        }
    }
}
