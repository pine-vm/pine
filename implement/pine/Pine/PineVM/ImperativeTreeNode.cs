namespace Pine.PineVM;

public abstract record ImperativeNode
{
    public record LeafNode(Expression Expression)
        : ImperativeNode;

    public record ConditionalNode(
        Expression.ConditionalExpression Origin,
        ImperativeNode Condition,
        ImperativeNode FalseBranch,
        ImperativeNode TrueBranch,
        ImperativeNode Continuation)
        : ImperativeNode;
}
