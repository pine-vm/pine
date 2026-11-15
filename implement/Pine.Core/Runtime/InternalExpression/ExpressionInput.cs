using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections;
using System.Collections.Generic;

namespace Pine.Core.Runtime.InternalExpression;

internal readonly struct ExpressionInput : IReadOnlyList<Expression>
{
    private readonly Expression.BuiltinBinary? _binary;

    internal Expression? GeneralExpression { get; }

    private ExpressionInput(Expression? expression, Expression.BuiltinBinary? binary)
    {
        GeneralExpression = expression;
        _binary = binary;
    }

    public static implicit operator ExpressionInput(Expression expression) => new(expression, null);

    internal static ExpressionInput FromBinary(Expression.BuiltinBinary binary) => new(null, binary);

    internal bool IsList => _binary is not null || GeneralExpression is Expression.List;

    public int Count => Length;

    public int Length =>
        _binary is not null
        ?
        2
        :
        GeneralExpression is Expression.List list
        ?
        list.Items.Length
        :
        throw new InvalidOperationException("Builtin input is not a static expression list.");

    public Expression this[int index] => _binary is { } binary
        ? index switch
        {
            0 => binary.Left,
            1 => binary.Right,
            _ => throw new ArgumentOutOfRangeException(nameof(index))
        }
        : GeneralExpression is Expression.List list ? list.Items[index] :
        throw new InvalidOperationException("Builtin input is not a static expression list.");

    internal ExpressionInput Items => this;

    internal Expression ToExpression() => GeneralExpression ?? Expression.ListInst(this);

    internal PineValue EncodeList() =>
        GeneralExpression is { } expression
        ?
        ExpressionValueEncoding.EncodeExpressionAsValue(expression)
        :
        PineValue.List(
            [
            StringEncoding.ValueFromString("List"),
            ExpressionValueEncoding.EncodeExpressionAsValue(this[0]),
            ExpressionValueEncoding.EncodeExpressionAsValue(this[1])
            ]);

    public Enumerator GetEnumerator() => new(this);

    IEnumerator<Expression> IEnumerable<Expression>.GetEnumerator() => GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

    public struct Enumerator(ExpressionInput input) : IEnumerator<Expression>
    {
        private int _index = -1;

        public readonly Expression Current => input[_index];

        readonly object IEnumerator.Current => Current;

        public bool MoveNext() => ++_index < input.Length;

        public void Reset() => _index = -1;

        public readonly void Dispose() { }
    }
}
