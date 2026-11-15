using Pine.Core.Addressing;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;

using Canonical = Pine.Core.PineLanguage.Expression;

namespace Pine.Core.Runtime.InternalExpression;

public static partial class ExpressionBinary
{
    private enum Constructor
    {
        Literal,
        List,
        Eval,
        Builtin,
        Conditional,
        Environment,
        Label,
        BuiltinBinary,
        Length_Equal_Binary,
        HeadSkip
    }

    private static class ConstructorExpressions
    {
        internal static readonly IReadOnlyDictionary<string, PineValue> Values =
            BuildValues();

        private static IReadOnlyDictionary<string, PineValue> BuildValues()
        {
            var values = new Dictionary<string, PineValue>(StringComparer.Ordinal);

            foreach (var kind in Enum.GetValues<Constructor>())
            {
                var value = ExpressionEncoding.EncodeExpressionAsValue(BuildConstructor(kind));
                var hash = PineValueHashTree.ComputeHash(value);

                if (!hash.Span.SequenceEqual(ConstructorHash(kind)))
                {
                    throw new InvalidOperationException(
                        "Internal constructor hash needs regeneration: " + kind + " " +
                        Convert.ToHexStringLower(hash.Span));
                }

                values.Add(kind.ToString(), value);
            }

            return values.ToFrozenDictionary(StringComparer.Ordinal);
        }
    }

    private static readonly byte[] s_constructorHashes =
        Convert.FromHexString(
            "1bc49725adcb42160f215ba8be507f9222e084984e9e067e09822a4aad2185a5" +
            "ae7fdffadc2c7288c849f9059ca9e7b0f309105ecf92f13f04b59642d0cc7b57" +
            "36f1ad73ee3902f179cb64831866c61c2fa9728eea7ba2dd929609c5d4c8bd5f" +
            "fc224679734b951781eb08bbbbb91f41e8508667ea996ce5855a785de9568e05" +
            "5235ffe6f91469b33205b41944b1a6f8c28a5eeb99e0039087e1d25c4fbab361" +
            "8de96843187da81f001af05d563609093b806eb0fdc59d633312d046b61cb296" +
            "1448655241228ac4f2cc635eeb89a3857a9b881c8ca9fbe917c6ed060870cab9" +
            "68554a13960f8add4c7761d7a78f720b44d563142b120da0d829693c137490d1" +
            "7577b17aa5ba759567b72843ac379167a409a9e2115ea47421fab31ad147a479" +
            "b9937055783274d3a36bf45b4cf2a56c3be282325678742baf38769ef1d22f09");

    private static ReadOnlySpan<byte> ConstructorHash(Constructor constructor) =>
        s_constructorHashes.AsSpan((int)constructor * 32, 32);

    /// <summary>
    /// Constructor programs for regeneration and independent Pine decoding. Access initializes
    /// canonical programs; ordinary retained decoding/encoding does not need their objects.
    /// </summary>
    public static IReadOnlyDictionary<string, PineValue> GetConstructorExpressions() => ConstructorExpressions.Values;

    /// <summary>Resolves only the constructor programs admitted by this retained-model profile.</summary>
    public static PineValue? ResolveConstructorExpression(ReadOnlyMemory<byte> hash)
    {
        foreach (var kind in Enum.GetValues<Constructor>())
        {
            if (ConstructorHash(kind).SequenceEqual(hash.Span))
                return ConstructorExpressions.Values[kind.ToString()];
        }

        return null;
    }

    private static Canonical Literal(PineValue value) => Canonical.LitralInst(value);

    private static Canonical Number(int value) => Literal(IntegerEncoding.EncodeSignedInteger(value));

    private static Canonical List(params Canonical[] items) => Canonical.ListInst(items);

    private static Canonical Builtin(string name, params Canonical[] items) => Canonical.BuiltinInst(name, List(items));

    private static Canonical At(int index) =>
        Canonical.BuiltinInst("head", Builtin("skip", Number(index), Canonical.EnvironmentInstance));

    private static Canonical Tag(string name, params Canonical[] fields) =>
        List([Literal(StringEncoding.ValueFromString(name)), .. fields]);

    private static Canonical Call(string name, Canonical input) =>
        Tag("Builtin", Literal(StringEncoding.ValueFromString(name)), input);

    private static Canonical BuildConstructor(Constructor constructor) =>
        constructor switch
        {
            Constructor.Literal => Tag("Litral", At(0)),

            Constructor.List =>
            Builtin(
                "concat",
                List(Literal(StringEncoding.ValueFromString("List"))),
                Canonical.EnvironmentInstance),

            Constructor.Eval => Tag("Eval", At(0), At(1)),

            Constructor.Builtin =>
            Tag("Builtin", BuiltinName(At(0)), At(1)),

            Constructor.Conditional => Tag("Conditional", At(0), At(1), At(2)),
            Constructor.Environment => Tag("Environment"),
            Constructor.Label => Tag("Label", At(0), At(1)),

            Constructor.BuiltinBinary =>
            Tag("Builtin", BuiltinName(At(0)), Tag("List", At(1), At(2))),

            Constructor.Length_Equal_Binary =>
            Canonical.ConditionalInst(
                Builtin("equal", At(2), Number(1)),
                Call("equal", Tag("List", At(1), Call("length", At(0)))),
                Call("equal", Tag("List", Call("length", At(0)), At(1)))),

            Constructor.HeadSkip => Call("head", Call("skip", Tag("List", Tag("Litral", At(0)), At(1)))),

            _ =>
            throw new NotImplementedException(
                nameof(BuildConstructor) + " does not handle constructor: " + constructor)
        };

    private static Canonical BuiltinName(Canonical identifier) =>
        Canonical.BuiltinInst(
            "head",
            Builtin(
                "skip",
                identifier,
                Literal(
                    PineValue.List(
                        [
                            .. Enum.GetValues<BuiltinFunction>().Select(
                                function => StringEncoding.ValueFromString(BuiltinFunctions.ToName(function)))
                        ]))));
}
