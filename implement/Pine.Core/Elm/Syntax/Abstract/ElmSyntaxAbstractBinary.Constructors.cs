using Pine.Core.Addressing;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Linq;

using Expr = Pine.Core.PineLanguage.Expression;

namespace Pine.Core.Elm.Syntax.Abstract;

public static partial class ElmSyntaxAbstractBinary
{
    private sealed record ConstructorExpression(ReadOnlyMemory<byte> Hash, PineValue Expression);

    private static class ConstructorExpressions
    {
        internal static readonly ConstructorExpression[] Instances = BuildConstructors();
    }

    private static ReadOnlySpan<byte> ConstructorHash(Constructor constructor) =>
        ConstructorHashBytes.Slice((int)constructor * 32, 32);

    /// <summary>
    /// Resolves exactly the constructor expressions admitted by this syntax profile.
    /// A general Pine decoder can evaluate these expressions with an ordinary interpreter.
    /// </summary>
    public static PineValue? ResolveConstructorExpression(ReadOnlyMemory<byte> hash)
    {
        for (var index = 0; index < ConstructorHashBytes.Length / 32; ++index)
        {
            if (ConstructorHash((Constructor)index).SequenceEqual(hash.Span))
                return ConstructorExpressions.Instances[index].Expression;
        }

        return null;
    }

    private static ConstructorExpression[] BuildConstructors()
    {
        var constructors = Enum.GetValues<Constructor>();
        var result = new ConstructorExpression[constructors.Length];

        foreach (var constructor in constructors)
        {
            var expression = ExpressionEncoding.EncodeExpressionAsValue(CanonicalConstructor(constructor));
            var hash = PineValueHashTree.ComputeHash(expression);

            if (!hash.Span.SequenceEqual(ConstructorHash(constructor)))
                throw new InvalidOperationException("Syntax constructor hash needs regeneration: " + constructor);

            if (result.Take((int)constructor).Any(previous => previous.Hash.Span.SequenceEqual(hash.Span)))
                throw new InvalidOperationException("Ambiguous syntax constructor expression: " + constructor);

            result[(int)constructor] = new(hash, expression);
        }

        return result;
    }

    private static Expr Literal(PineValue value) => Expr.LitralInst(value);

    private static Expr Number(int value) => Literal(IntegerEncoding.EncodeSignedInteger(value));

    private static Expr List(params Expr[] values) => Expr.ListInst(values);

    private static Expr Builtin(string name, params Expr[] values) => Expr.BuiltinInst(name, List(values));

    private static Expr Skip(Expr count) => Builtin("skip", count, Expr.EnvironmentInstance);

    private static Expr Tail(int count) => Skip(Number(count));

    private static Expr At(int index) => Expr.BuiltinInst("head", Tail(index));

    private static Expr At(Expr index) => Expr.BuiltinInst("head", Skip(index));

    private static Expr Add(Expr a, int b) => Builtin("int_add", a, Number(b));

    private static Expr Slice(Expr start, Expr length) => Builtin("take", length, Skip(start));

    private static Expr Tag(string name, params Expr[] arguments) =>
        Expr.ListInst(
            [
                Literal(StringEncoding.ValueFromString(ElmValue.ElmChoiceTypeTagName)),
                Literal(StringEncoding.ValueFromString(name)),
                .. arguments,
            ]);

    private static Expr Record(params (string Name, Expr Value)[] fields) =>
        Expr.ListInst(
            [
                Literal(ElmValue.ElmRecordTypeTagNameAsValue),
                .. fields.OrderBy(field => field.Name, StringComparer.Ordinal)
                .SelectMany(field => new[] { Literal(StringEncoding.ValueFromString(field.Name)), field.Value }),
            ]);

    private static Expr Direction(Expr value) =>
        Expr.ConditionalInst(
            Builtin("equal", value, Number(0)),
            Expr.ConditionalInst(Builtin("equal", value, Number(1)), Tag("Non"), Tag("Right")),
            Tag("Left"));

    private static Expr CanonicalConstructor(Constructor constructor)
    {
        var environment = Expr.EnvironmentInstance;
        var signature = Record(("name", At(0)), ("typeAnnotation", At(1)));
        var qualified = Record(("moduleName", Tail(1)), ("name", At(0)));

        return constructor switch
        {
            Constructor.String => Tag(ElmValue.ElmStringTypeTagName, At(0)),

            Constructor.QualifiedDeclaration =>
            Expr.ConditionalInst(
                Literal(PineKernelValues.FalseValue),
                qualified,
                Literal(StringEncoding.ValueFromString("DeclQualifiedName"))),

            Constructor.QualifiedPattern => qualified,
            Constructor.None => Tag("Nothing"),
            Constructor.Some => Tag("Just", At(0)),
            Constructor.Function => Record(("signature", At(0)), ("declaration", At(1))),
            Constructor.Implementation => Record(("name", At(0)), ("expression", At(1)), ("arguments", Tail(2))),
            Constructor.Signature => signature,

            Constructor.Choice =>
            Record(
                ("name", At(0)),
                ("generics", Slice(Number(2), At(1))),
                ("constructors", Skip(Add(At(1), 2)))),

            Constructor.Alias =>
            Record(
                ("name", At(0)),
                ("generics", Slice(Number(2), At(1))),
                ("typeAnnotation", At(Add(At(1), 2)))),

            Constructor.Infix =>
            Record(
                ("direction", Direction(At(0))),
                ("precedence", At(1)),
                ("operator", At(2)),
                ("functionName", At(3))),

            Constructor.ValueConstructor => Record(("name", At(0)), ("arguments", Tail(2))),
            Constructor.RecordDefinition => environment,
            Constructor.RecordField => Record(("fieldName", At(0)), ("fieldType", At(2))),
            Constructor.RecordSetter => Record(("fieldName", At(0)), ("value", At(2))),
            Constructor.Case => Record(("pattern", At(0)), ("expression", At(1))),
            Constructor.LetFunction => Tag("LetFunction", At(0)),
            Constructor.LetDestructuring => Tag("LetDestructuring", At(0), At(1)),
            Constructor.FunctionDeclaration => Tag("FunctionDeclaration", At(0)),
            Constructor.ChoiceTypeDeclaration => Tag("ChoiceTypeDeclaration", At(0)),
            Constructor.AliasDeclaration => Tag("AliasDeclaration", At(0)),
            Constructor.PortDeclaration => Tag("PortDeclaration", At(0)),
            Constructor.InfixDeclaration => Tag("InfixDeclaration", At(0)),
            Constructor.GenericType => Tag("GenericType", At(0)),
            Constructor.Typed => Tag("Typed", Slice(Number(1), At(0)), At(Add(At(0), 1)), Skip(Add(At(0), 2))),
            Constructor.Unit => Tag("Unit"),
            Constructor.Tupled => Tag("Tupled", environment),
            Constructor.Record => Tag("Record", At(0)),
            Constructor.GenericRecord => Tag("GenericRecord", At(0), At(1)),
            Constructor.FunctionTypeAnnotation => Tag("FunctionTypeAnnotation", At(0), At(1)),
            Constructor.AllPattern => Tag("AllPattern"),
            Constructor.VarPattern => Tag("VarPattern", At(0)),
            Constructor.UnitPattern => Tag("UnitPattern"),
            Constructor.CharPattern => Tag("CharPattern", At(0)),
            Constructor.StringPattern => Tag("StringPattern", At(0)),
            Constructor.IntPattern => Tag("IntPattern", At(0)),
            Constructor.FloatPattern => Tag("FloatPattern", Tag(ElmValue.ElmFloatTypeTagName, At(0), At(1))),
            Constructor.TuplePattern => Tag("TuplePattern", environment),
            Constructor.RecordPattern => Tag("RecordPattern", Slice(Number(1), At(0))),
            Constructor.UnConsPattern => Tag("UnConsPattern", At(0), At(1)),
            Constructor.ListPattern => Tag("ListPattern", environment),
            Constructor.NamedPattern => Tag("NamedPattern", At(0), Tail(2)),
            Constructor.AsPattern => Tag("AsPattern", At(0), At(1)),
            Constructor.UnitExpr => Tag("UnitExpr"),
            Constructor.StringLiteral => Tag("StringLiteral", At(0)),
            Constructor.CharLiteral => Tag("CharLiteral", At(0)),
            Constructor.IntegerLiteral => Tag("IntegerLiteral", At(0)),
            Constructor.FloatLiteral => Tag("FloatLiteral", Tag(ElmValue.ElmFloatTypeTagName, At(0), At(1))),
            Constructor.Negation => Tag("Negation", At(0)),
            Constructor.ListExpr => Tag("ListExpr", environment),

            Constructor.Identifier =>
            Tag(
                "Identifier",
                Expr.BuiltinInst("head", Builtin("skip", Number(2), At(0))),
                Expr.BuiltinInst("head", Builtin("skip", Number(4), At(0)))),

            Constructor.IfBlock => Tag("IfBlock", At(0), At(1), At(2)),
            Constructor.PrefixOperator => Tag("PrefixOperator", At(0)),
            Constructor.Application => Tag("Application", At(0), Tail(1)),
            Constructor.OperatorApplication => Tag("OperatorApplication", At(0), Direction(At(1)), At(2), At(3)),
            Constructor.TupledExpression => Tag("TupledExpression", environment),
            Constructor.LambdaExpression => Tag("LambdaExpression", Tail(1), At(0)),
            Constructor.CaseExpression => Tag("CaseExpression", At(0), Tail(1)),
            Constructor.LetExpression => Tag("LetExpression", Tail(1), At(0)),
            Constructor.RecordExpr => Tag("RecordExpr", environment),
            Constructor.RecordAccess => Tag("RecordAccess", At(0), At(1)),
            Constructor.RecordAccessFunction => Tag("RecordAccessFunction", At(0)),
            Constructor.RecordUpdateExpression => Tag("RecordUpdateExpression", At(0), Tail(1)),
            Constructor.GLSLExpression => Tag("GLSLExpression", At(0)),

            _ =>
            throw new NotImplementedException("Unknown syntax constructor: " + constructor)
        };
    }
}
