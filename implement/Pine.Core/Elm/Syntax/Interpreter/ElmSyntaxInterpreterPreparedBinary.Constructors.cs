using Pine.Core.Addressing;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Linq;

namespace Pine.Core.Elm.Syntax.Interpreter;

public static partial class ElmSyntaxInterpreterPreparedBinary
{
    private static class ConstructorExpressions
    {
        internal static readonly PineValue[] Instances = BuildConstructors();
    }

    private static ReadOnlySpan<byte> ConstructorHash(Constructor constructor) =>
        ConstructorHashBytes.Slice((int)constructor * 32, 32);

    /// <summary>
    /// Resolves prepared constructors together with the shared abstract-syntax/string constructors.
    /// Returned expressions can be evaluated by an ordinary Pine interpreter.
    /// </summary>
    public static PineValue? ResolveConstructorExpression(ReadOnlyMemory<byte> hash)
    {
        for (var index = 0; index < ConstructorHashBytes.Length / 32; ++index)
        {
            if (ConstructorHash((Constructor)index).SequenceEqual(hash.Span))
                return ConstructorExpressions.Instances[index];
        }

        return Abstract.ElmSyntaxAbstractBinary.ResolveConstructorExpression(hash);
    }

    private static PineValue[] BuildConstructors()
    {
        var constructors = Enum.GetValues<Constructor>();
        var result = new PineValue[constructors.Length];

        foreach (var constructor in constructors)
        {
            var expression = ExpressionEncoding.EncodeExpressionAsValue(CanonicalConstructor(constructor));
            var hash = PineValueHashTree.ComputeHash(expression);

            if (!hash.Span.SequenceEqual(ConstructorHash(constructor)))
                throw new InvalidOperationException("Prepared constructor hash needs regeneration: " + constructor);

            if (result.Take((int)constructor).Any(
                previous => PineValueHashTree.ComputeHash(previous).Span.SequenceEqual(hash.Span)) ||
                Abstract.ElmSyntaxAbstractBinary.ResolveConstructorExpression(hash) is not null)
            {
                throw new InvalidOperationException("Ambiguous prepared constructor expression: " + constructor);
            }

            result[(int)constructor] = expression;
        }

        return result;
    }

    private static Expression CanonicalConstructor(Constructor constructor) =>
        Expression.ListInst(
            [
            Expression.LitralInst(StringEncoding.ValueFromString(ElmValue.ElmChoiceTypeTagName)),
            Expression.LitralInst(StringEncoding.ValueFromString("Prepared" + constructor)),
            Expression.EnvironmentInstance
            ]);
}
