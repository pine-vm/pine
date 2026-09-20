using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using Pine.Core.Internal;
using Pine.Core.PineVM;
using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

/// <summary>
/// Precompiled leaves (short-circuit .NET implementations) for inner bodies of function
/// values defined in <see cref="CoreBasics"/>.
/// <para>
/// Each entry maps the Pine value encoding of a function's innermost
/// <c>ParseAndEval</c>-target expression to a delegate that returns the result directly,
/// bypassing the intermediate VM's interpretation of the recursive expression tree.
/// </para>
/// </summary>
public static class CoreBasicsPrecompiledLeaves
{
    /// <summary>
    /// Env value class identifying invocation targets eligible for the
    /// <c>Basics.compare</c> precompiled leaf.
    /// <para>
    /// The inner body of <see cref="CoreBasics.Compare_FunctionValue"/> is invoked with
    /// environment of shape <c>[envFunctions, arg0, arg1]</c> with
    /// <c>envFunctions = []</c> (the empty list); the constraint pins
    /// <c>env[0]</c> to <see cref="PineValue.EmptyList"/>.
    /// </para>
    /// </summary>
    public static PineValueClass CompareLeafEnvClass { get; } =
        PineValueClass.Create(
            [new KeyValuePair<IReadOnlyList<int>, PineValue>([0], PineValue.EmptyList)]);

    /// <summary>
    /// Pine value key under which the <c>Basics.compare</c> leaf is registered in the
    /// precompiled-leaves dictionary; equal to
    /// <see cref="CoreBasics.Compare_InnerBodyEncodedValue"/>.
    /// </summary>
    public static PineValue CompareLeafKey => CoreBasics.Compare_InnerBodyEncodedValue;

    /// <summary>
    /// Precompiled-leaf delegate for <c>Basics.compare</c>; executes the comparison
    /// directly in .NET and returns the resulting <c>Order</c> tag, or <c>null</c> if the
    /// environment does not match the expected shape.
    /// </summary>
    public static PineValueInProcess? CompareLeafDelegate(PineValueInProcess environment)
    {
        if (!CompareLeafEnvClass.SatisfiedByValue(environment))
        {
            return null;
        }

        var argA = environment.ValueInProcessFromPathOrEmptyList([1]);
        var argB = environment.ValueInProcessFromPathOrEmptyList([2]);

        return BasicsCompare(argA, argB);
    }

    /// <summary>
    /// Env value class identifying invocation targets eligible for the
    /// <c>Basics.eq</c> precompiled leaf.
    /// <para>
    /// The recursive <c>eqDeep</c> helper (see <see cref="CoreBasics.Eq_InnerBodyEncodedValue"/>)
    /// is invoked with environment of shape <c>[[selfEncoded], [a, b]]</c>; the constraint pins
    /// <c>env[0]</c> to a single-element list containing exactly the leaf key (the self-reference
    /// used for recursion).
    /// </para>
    /// </summary>
    public static PineValueClass EqLeafEnvClass { get; } =
        PineValueClass.Create(
            [new KeyValuePair<IReadOnlyList<int>, PineValue>([0], PineValue.List([EqLeafKey]))]);

    /// <summary>
    /// Pine value key under which the <c>Basics.eq</c> leaf is registered in the
    /// precompiled-leaves dictionary; equal to
    /// <see cref="CoreBasics.Eq_InnerBodyEncodedValue"/>.
    /// </summary>
    public static PineValue EqLeafKey => CoreBasics.Eq_InnerBodyEncodedValue;

    /// <summary>
    /// Precompiled-leaf delegate for <c>Basics.eq</c> (deep structural equality); executes the
    /// comparison directly in .NET and returns the resulting <c>Bool</c> tag, or <c>null</c> if
    /// the environment does not match the expected shape.
    /// </summary>
    public static PineValueInProcess? EqLeafDelegate(PineValueInProcess environment)
    {
        if (!EqLeafEnvClass.SatisfiedByValue(environment))
        {
            return null;
        }

        var argA = environment.ValueInProcessFromPathOrEmptyList([1, 0]);
        var argB = environment.ValueInProcessFromPathOrEmptyList([1, 1]);

        return BasicsEq(argA, argB);
    }

    /// <summary>
    /// Env value class identifying invocation targets eligible for the
    /// <c>Basics.idiv</c> precompiled leaf.
    /// <para>
    /// The recursive <c>idivHelper</c> helper (see
    /// <see cref="CoreBasics.IdivHelper_InnerBodyEncodedValue"/>) is invoked with environment of
    /// shape <c>[[selfEncoded], [dividend, divisor, quotient]]</c> where <c>dividend</c> and
    /// <c>divisor</c> are both non-negative; the constraint pins <c>env[0]</c> to a
    /// single-element list containing exactly the leaf key.
    /// </para>
    /// </summary>
    public static PineValueClass IdivLeafEnvClass { get; } =
        PineValueClass.Create(
            [new KeyValuePair<IReadOnlyList<int>, PineValue>([0], PineValue.List([IdivLeafKey]))]);

    /// <summary>
    /// Pine value key under which the <c>Basics.idiv</c> helper leaf is registered in the
    /// precompiled-leaves dictionary; equal to
    /// <see cref="CoreBasics.IdivHelper_InnerBodyEncodedValue"/>.
    /// </summary>
    public static PineValue IdivLeafKey => CoreBasics.IdivHelper_InnerBodyEncodedValue;

    /// <summary>
    /// Precompiled-leaf delegate for the recursive <c>idivHelper</c> used by
    /// <c>Basics.idiv</c> (the <c>//</c> operator); computes
    /// <c>quotient + dividend / divisor</c> directly in .NET (both operands are non-negative by
    /// construction), or returns <c>null</c> if the environment does not match the expected
    /// shape.
    /// </summary>
    public static PineValueInProcess? IdivLeafDelegate(PineValueInProcess environment)
    {
        if (!IdivLeafEnvClass.SatisfiedByValue(environment))
        {
            return null;
        }

        var dividendValue = environment.ValueInProcessFromPathOrEmptyList([1, 0]);
        var divisorValue = environment.ValueInProcessFromPathOrEmptyList([1, 1]);
        var quotientValue = environment.ValueInProcessFromPathOrEmptyList([1, 2]);

        if (!dividendValue.IsBlob() ||
            !divisorValue.IsBlob() ||
            !quotientValue.IsBlob() ||
            dividendValue.AsInteger() is not { } dividend ||
            divisorValue.AsInteger() is not { } divisor ||
            quotientValue.AsInteger() is not { } quotient)
        {
            return null;
        }

        if (divisor == System.Numerics.BigInteger.Zero)
        {
            return null;
        }

        return PineValueInProcess.CreateInteger(quotient + (dividend / divisor));
    }

    /// <summary>
    /// Env value class identifying invocation targets eligible for the
    /// <c>Basics.gcd</c> precompiled leaf.
    /// <para>
    /// The recursive <c>gcd</c> function (see <see cref="CoreBasics.Gcd_InnerBodyEncodedValue"/>)
    /// is invoked with environment of shape <c>[[selfEncoded], [a, b]]</c>; the constraint pins
    /// <c>env[0]</c> to a single-element list containing exactly the leaf key.
    /// </para>
    /// </summary>
    public static PineValueClass GcdLeafEnvClass { get; } =
        PineValueClass.Create(
            [new KeyValuePair<IReadOnlyList<int>, PineValue>([0], PineValue.List([GcdLeafKey]))]);

    /// <summary>
    /// Pine value key under which the <c>Basics.gcd</c> leaf is registered in the
    /// precompiled-leaves dictionary; equal to
    /// <see cref="CoreBasics.Gcd_InnerBodyEncodedValue"/>.
    /// </summary>
    public static PineValue GcdLeafKey => CoreBasics.Gcd_InnerBodyEncodedValue;

    /// <summary>
    /// Precompiled-leaf delegate for <c>Basics.gcd</c>; executes the Euclidean algorithm
    /// directly in .NET, or returns <c>null</c> if the environment does not match the expected
    /// shape.
    /// </summary>
    public static PineValueInProcess? GcdLeafDelegate(PineValueInProcess environment)
    {
        if (!GcdLeafEnvClass.SatisfiedByValue(environment))
        {
            return null;
        }

        var argA = environment.ValueInProcessFromPathOrEmptyList([1, 0]);
        var argB = environment.ValueInProcessFromPathOrEmptyList([1, 1]);

        if (!argA.IsBlob() ||
            !argB.IsBlob() ||
            argA.AsInteger() is not { } a ||
            argB.AsInteger() is not { } b)
        {
            return null;
        }

        while (b != System.Numerics.BigInteger.Zero)
        {
            (a, b) = (b, ElmModBy(b, a));
        }

        return PineValueInProcess.CreateInteger(a);
    }

    /// <summary>
    /// Replicates the semantics of Elm's <c>modBy</c> for <see cref="System.Numerics.BigInteger"/>
    /// operands: the result always has the same sign as <paramref name="modulus"/> (or is zero).
    /// </summary>
    private static System.Numerics.BigInteger ElmModBy(
        System.Numerics.BigInteger modulus,
        System.Numerics.BigInteger value)
    {
        var remainder = value % modulus;

        if ((remainder > System.Numerics.BigInteger.Zero && modulus < System.Numerics.BigInteger.Zero) ||
            (remainder < System.Numerics.BigInteger.Zero && modulus > System.Numerics.BigInteger.Zero))
        {
            return remainder + modulus;
        }

        return remainder;
    }

    /// <summary>
    /// Default precompiled-leaves dictionary contributed by <see cref="CoreBasics"/>.
    /// Suitable for merging into the dictionary consumed by the intermediate VM.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, PrecompiledLeaf> DefaultLeaves { get; } =
        ImmutableDictionary<PineValue, PrecompiledLeaf>.Empty
        .Add(CompareLeafKey, CompareLeafDelegate)
        .Add(EqLeafKey, EqLeafDelegate)
        .Add(IdivLeafKey, IdivLeafDelegate)
        .Add(GcdLeafKey, GcdLeafDelegate);

    // ========== .NET implementations of Basics.compare ==========
    // The logic below mirrors the legacy Pine.PineVM.Precompiled.BasicsCompare entry point,
    // restated here to avoid a project dependency from Pine.Core onto the pine project.

    private static readonly PineValueInProcess s_tag_EQ =
        PineValueInProcess.CreateFullyRepresented(
            ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("EQ", [])));

    private static readonly PineValueInProcess s_tag_LT =
        PineValueInProcess.CreateFullyRepresented(
            ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("LT", [])));

    private static readonly PineValueInProcess s_tag_GT =
        PineValueInProcess.CreateFullyRepresented(
            ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("GT", [])));

    internal static PineValueInProcess BasicsCompare(
        PineValueInProcess a,
        PineValueInProcess b)
    {
        if (PineValueInProcess.AreEqual(a, b))
        {
            return s_tag_EQ;
        }

        if (a.IntegerOrNull is { } cachedA && b.IntegerOrNull is { } cachedB)
        {
            return CompareIntegers(cachedA, cachedB);
        }

        var aTag = a.ValueInProcessFromPathOrEmptyList([1]);
        var bTag = b.ValueInProcessFromPathOrEmptyList([1]);

        var aIsString =
            PineValueInProcess.AreEqual(aTag, ElmValue.ElmStringTypeTagNameAsValue);

        var bIsString =
            PineValueInProcess.AreEqual(bTag, ElmValue.ElmStringTypeTagNameAsValue);

        if (aIsString && bIsString)
        {
            return
                CompareStrings(
                    a.ValueInProcessFromPathOrEmptyList([2]),
                    b.ValueInProcessFromPathOrEmptyList([2]));
        }

        var aIsFloat =
            PineValueInProcess.AreEqual(aTag, ElmValue.ElmFloatTypeTagNameAsValue);

        var bIsFloat =
            PineValueInProcess.AreEqual(bTag, ElmValue.ElmFloatTypeTagNameAsValue);

        if (aIsFloat && bIsFloat)
        {
            var leftProduct =
                MultiplyIntegers(
                    a.ValueInProcessFromPathOrEmptyList([2]),
                    b.ValueInProcessFromPathOrEmptyList([3]));

            var rightProduct =
                MultiplyIntegers(
                    b.ValueInProcessFromPathOrEmptyList([2]),
                    a.ValueInProcessFromPathOrEmptyList([3]));

            return CompareNonListValues(leftProduct, rightProduct);
        }

        if (aIsFloat)
        {
            var rightProduct =
                MultiplyIntegers(
                    a.ValueInProcessFromPathOrEmptyList([3]),
                    b);

            return
                CompareNonListValues(
                    a.ValueInProcessFromPathOrEmptyList([2]),
                    rightProduct);
        }

        if (bIsFloat)
        {
            var leftProduct =
                MultiplyIntegers(
                    a,
                    b.ValueInProcessFromPathOrEmptyList([3]));

            return
                CompareNonListValues(
                    leftProduct,
                    b.ValueInProcessFromPathOrEmptyList([2]));
        }

        if (a.IsList())
        {
            return CompareLists(a, b);
        }

        return CompareNonListValues(a, b);
    }

    private static PineValueInProcess CompareLists(
        PineValueInProcess a,
        PineValueInProcess b)
    {
        var lengthA = a.GetLength();

        if (lengthA is 0)
        {
            return b.IsList() && b.GetLength() is 0 ? s_tag_EQ : s_tag_LT;
        }

        if (!b.IsList())
        {
            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        var lengthB = b.GetLength();

        if (lengthB is 0)
        {
            return s_tag_GT;
        }

        var commonLength = Math.Min(lengthA, lengthB);

        for (var i = 0; i < commonLength; ++i)
        {
            var itemOrder = BasicsCompare(a.GetElementAt(i), b.GetElementAt(i));

            if (!ReferenceEquals(itemOrder, s_tag_EQ))
            {
                return itemOrder;
            }
        }

        return
            lengthA < lengthB
            ?
            s_tag_LT
            :
            lengthA > lengthB
            ?
            s_tag_GT
            :
            s_tag_EQ;
    }

    private static PineValueInProcess CompareStrings(
        PineValueInProcess stringA,
        PineValueInProcess stringB)
    {
        if (PineValueInProcess.AreEqual(stringA, stringB))
        {
            return s_tag_EQ;
        }

        if (stringA.Evaluate() is PineValue.BlobValue blobA &&
            stringB.Evaluate() is PineValue.BlobValue blobB)
        {
            var commonLength = Math.Min(blobA.Bytes.Length, blobB.Bytes.Length);
            var commonLengthChars = commonLength / 4;

            for (var i = 0; i < commonLengthChars; ++i)
            {
                var offset = i * 4;

                var charA = BinaryPrimitives.ReadInt32BigEndian(blobA.Bytes.Span[offset..]);
                var charB = BinaryPrimitives.ReadInt32BigEndian(blobB.Bytes.Span[offset..]);

                if (charA != charB)
                {
                    return charA < charB ? s_tag_LT : s_tag_GT;
                }
            }

            return
                blobA.Bytes.Length < blobB.Bytes.Length
                ?
                s_tag_LT
                :
                blobA.Bytes.Length > blobB.Bytes.Length
                ?
                s_tag_GT
                :
                s_tag_EQ;
        }

        return CompareNonListValues(stringA, stringB);
    }

    private static PineValueInProcess CompareNonListValues(
        PineValueInProcess a,
        PineValueInProcess b)
    {
        if (PineValueInProcess.AreEqual(a, b))
        {
            return s_tag_EQ;
        }

        if (a.IsBlob() &&
            b.IsBlob() &&
            a.AsInteger() is { } integerA &&
            b.AsInteger() is { } integerB)
        {
            return CompareIntegers(integerA, integerB);
        }

        return
            BuiltinFunction.int_is_sorted_asc(
                PineValue.List([a.Evaluate(), b.Evaluate()])) ==
            PineKernelValues.TrueValue
            ?
            s_tag_LT
            :
            s_tag_GT;
    }

    private static PineValueInProcess CompareIntegers(
        System.Numerics.BigInteger a,
        System.Numerics.BigInteger b) =>
        a < b ? s_tag_LT : a > b ? s_tag_GT : s_tag_EQ;

    private static PineValueInProcess MultiplyIntegers(
        PineValueInProcess a,
        PineValueInProcess b)
    {
        if (a.IsBlob() &&
            b.IsBlob() &&
            a.AsInteger() is { } integerA &&
            b.AsInteger() is { } integerB)
        {
            return PineValueInProcess.CreateInteger(integerA * integerB);
        }

        return
            PineValueInProcess.Create(
                BuiltinFunctionSpecialized.int_mul(a.Evaluate(), b.Evaluate()));
    }

    // ========== .NET implementations of Basics.eq (deep structural equality) ==========

    internal static PineValueInProcess BasicsEq(
        PineValueInProcess a,
        PineValueInProcess b) =>
        PineValueInProcess.CreateBool(BasicsEqual(a, b));

    internal static bool BasicsEqual(
        PineValueInProcess a,
        PineValueInProcess b)
    {
        if (PineValueInProcess.AreEqual(a, b))
        {
            return true;
        }

        var aTag = a.ValueInProcessFromPathOrEmptyList([1]);
        var bTag = b.ValueInProcessFromPathOrEmptyList([1]);

        var aIsFloat =
            PineValueInProcess.AreEqual(aTag, ElmValue.ElmFloatTypeTagNameAsValue);

        var bIsFloat =
            PineValueInProcess.AreEqual(bTag, ElmValue.ElmFloatTypeTagNameAsValue);

        if (aIsFloat)
        {
            var numA = a.ValueInProcessFromPathOrEmptyList([2]);
            var denomA = a.ValueInProcessFromPathOrEmptyList([3]);

            if (bIsFloat)
            {
                var numB = b.ValueInProcessFromPathOrEmptyList([2]);
                var denomB = b.ValueInProcessFromPathOrEmptyList([3]);

                return
                    PineValueInProcess.AreEqual(
                        MultiplyIntegers(numA, denomB),
                        MultiplyIntegers(numB, denomA));
            }

            return
                !PineValueInProcess.AreEqual(denomA, PineValueInProcess.CreateInteger(0)) &&
                PineValueInProcess.AreEqual(numA, MultiplyIntegers(denomA, b));
        }

        if (bIsFloat)
        {
            var numB = b.ValueInProcessFromPathOrEmptyList([2]);
            var denomB = b.ValueInProcessFromPathOrEmptyList([3]);

            return
                !PineValueInProcess.AreEqual(denomB, PineValueInProcess.CreateInteger(0)) &&
                PineValueInProcess.AreEqual(MultiplyIntegers(a, denomB), numB);
        }

        if (a.IsBlob() || !b.IsList())
        {
            return false;
        }

        var lengthA = a.GetLength();

        if (lengthA != b.GetLength())
        {
            return false;
        }

        if (PineValueInProcess.AreEqual(aTag, ElmValue.ElmStringTypeTagNameAsValue))
        {
            return false;
        }

        if (PineValueInProcess.AreEqual(aTag, ElmValue.ElmDictNotEmptyTagNameAsValue))
        {
            return
                ListsEqualRecursive(
                    DictToListRecursive(a),
                    DictToListRecursive(b));
        }

        if (PineValueInProcess.AreEqual(aTag, ElmValue.ElmSetTypeTagNameAsValue))
        {
            return
                ListsEqualRecursive(
                    DictKeysRecursive(a.ValueInProcessFromPathOrEmptyList([2])),
                    DictKeysRecursive(b.ValueInProcessFromPathOrEmptyList([2])));
        }

        for (var i = 0; i < lengthA; ++i)
        {
            if (!BasicsEqual(a.GetElementAt(i), b.GetElementAt(i)))
            {
                return false;
            }
        }

        return true;
    }

    private static IReadOnlyList<PineValueInProcess> DictToListRecursive(
        PineValueInProcess dict)
    {
        var pairs = new List<PineValueInProcess>();

        CollectDictPairs(dict, pairs);

        return pairs;
    }

    private static void CollectDictPairs(
        PineValueInProcess dict,
        List<PineValueInProcess> pairs)
    {
        if (!PineValueInProcess.AreEqual(
                dict.ValueInProcessFromPathOrEmptyList([1]),
                ElmValue.ElmDictNotEmptyTagNameAsValue))
        {
            return;
        }

        CollectDictPairs(dict.ValueInProcessFromPathOrEmptyList([5]), pairs);

        pairs.Add(
            PineValueInProcess.CreateList(
                [
                dict.ValueInProcessFromPathOrEmptyList([3]),
                dict.ValueInProcessFromPathOrEmptyList([4])
                ]));

        CollectDictPairs(dict.ValueInProcessFromPathOrEmptyList([6]), pairs);
    }

    private static IReadOnlyList<PineValueInProcess> DictKeysRecursive(
        PineValueInProcess dict)
    {
        var keys = new List<PineValueInProcess>();

        CollectDictKeys(dict, keys);

        return keys;
    }

    private static void CollectDictKeys(
        PineValueInProcess dict,
        List<PineValueInProcess> keys)
    {
        if (!PineValueInProcess.AreEqual(
                dict.ValueInProcessFromPathOrEmptyList([1]),
                ElmValue.ElmDictNotEmptyTagNameAsValue))
        {
            return;
        }

        CollectDictKeys(dict.ValueInProcessFromPathOrEmptyList([5]), keys);
        keys.Add(dict.ValueInProcessFromPathOrEmptyList([3]));
        CollectDictKeys(dict.ValueInProcessFromPathOrEmptyList([6]), keys);
    }

    private static bool ListsEqualRecursive(
        IReadOnlyList<PineValueInProcess> listA,
        IReadOnlyList<PineValueInProcess> listB)
    {
        if (listA.Count != listB.Count)
        {
            return false;
        }

        for (var i = 0; i < listA.Count; ++i)
        {
            if (!BasicsEqual(listA[i], listB[i]))
            {
                return false;
            }
        }

        return true;
    }
}
