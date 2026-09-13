using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Frontend;

/// <summary>
/// The ownership boundary for language expressions. Legacy lists and literal memories can retain
/// caller arrays, so compiler requests cannot retain the legacy AST itself.
/// </summary>
public abstract record OwnedExpression
{
    private OwnedExpression() { }

    /// <summary>An owned literal.</summary>
    public sealed record Literal(LiteralValue Value) : OwnedExpression;

    /// <summary>An ordered list expression.</summary>
    public sealed record List(ImmutableList<OwnedExpression> Items) : OwnedExpression
    {
        /// <inheritdoc/>
        public bool Equals(List? other) => other is not null && Items.SequenceEqual(other.Items);

        /// <inheritdoc/>
        public override int GetHashCode() => ModelEquality.SequenceHash(Items);
    }

    /// <summary>A builtin with its canonical input.</summary>
    public sealed record Builtin(string Name, OwnedExpression Input) : OwnedExpression;

    /// <summary>A conditional whose false arm includes noncanonical conditions.</summary>
    public sealed record Conditional(
        OwnedExpression Condition, OwnedExpression FalseBranch, OwnedExpression TrueBranch) : OwnedExpression;

    /// <summary>The current environment.</summary>
    public sealed record Environment : OwnedExpression;

    /// <summary>An invocation; runtime evaluates Environment before Encoded.</summary>
    public sealed record Eval(OwnedExpression Encoded, OwnedExpression InvocationEnvironment) : OwnedExpression;

    /// <summary>A retained source label, not an executable operation.</summary>
    public sealed record Label(LiteralValue Value, string Tag, OwnedExpression Tagged) : OwnedExpression;

    /// <summary>Copies every mutable boundary; no input collection is retained.</summary>
    public static OwnedExpression Capture(Expression expression) =>
        expression switch
        {
            Expression.Litral literal => new Literal(CaptureValue(literal.Value)),
            Expression.List list => new List([.. list.Items.Select(Capture)]),
            Expression.Builtin builtin => new Builtin(builtin.Function, Capture(builtin.Input)),
            Expression.Conditional conditional => new Conditional(
                Capture(conditional.Condition), Capture(conditional.FalseBranch), Capture(conditional.TrueBranch)),
            Expression.Environment => new Environment(),
            Expression.Eval eval => new Eval(Capture(eval.Encoded), Capture(eval.Environment)),
            Expression.Label label => new Label(CaptureValue(label.LabelValue), label.Tag, Capture(label.Tagged)),
            _ => throw new NotImplementedException("Unknown expression variant: " + expression.GetType().Name),
        };

    /// <summary>Creates a detached language AST for interoperability with existing analyses.</summary>
    public Expression ToExpression() =>
        this switch
        {
            Literal literal => new Expression.Litral(ToValue(literal.Value)),
            List list => new Expression.List(list.Items.Select(item => item.ToExpression()).ToArray()),
            Builtin builtin => new Expression.Builtin(builtin.Name, builtin.Input.ToExpression()),
            Conditional conditional => new Expression.Conditional(
                conditional.Condition.ToExpression(),
                conditional.FalseBranch.ToExpression(),
                conditional.TrueBranch.ToExpression()),
            Environment => Expression.EnvironmentInstance,
            Eval eval => new Expression.Eval(eval.Encoded.ToExpression(), eval.InvocationEnvironment.ToExpression()),
            Label label => new Expression.Label(ToValue(label.Value), label.Tagged.ToExpression()),
            _ => throw new NotImplementedException("Unknown owned expression variant: " + GetType().Name),
        };

    /// <summary>Copies all nested Pine payloads.</summary>
    public static LiteralValue CaptureValue(PineValue value) =>
        value switch
        {
            PineValue.BlobValue blob => new LiteralValue.Blob(blob.Bytes.ToArray().ToImmutableList()),
            PineValue.ListValue list => new LiteralValue.List([.. list.Items.ToArray().Select(CaptureValue)]),
            _ => throw new NotImplementedException("Unknown value variant: " + value.GetType().Name),
        };

    /// <summary>Creates fresh legacy payloads, never exposing immutable compiler storage.</summary>
    public static PineValue ToValue(LiteralValue value) =>
        value switch
        {
            LiteralValue.Blob blob => new PineValue.BlobValue(blob.Bytes.ToArray()),
            LiteralValue.List list => new PineValue.ListValue(list.Items.Select(ToValue).ToArray()),
            _ => throw new NotImplementedException("Unknown literal variant: " + value.GetType().Name),
        };
}
