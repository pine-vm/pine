using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
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
    public static PineValue? CompareLeafDelegate(PineValue environment)
    {
        if (!CompareLeafEnvClass.SatisfiedByValue(environment))
        {
            return null;
        }

        var argA = environment.ValueFromPathOrEmptyList([1]);
        var argB = environment.ValueFromPathOrEmptyList([2]);

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
    public static PineValue? EqLeafDelegate(PineValue environment)
    {
        if (!EqLeafEnvClass.SatisfiedByValue(environment))
        {
            return null;
        }

        var argA = environment.ValueFromPathOrEmptyList([1, 0]);
        var argB = environment.ValueFromPathOrEmptyList([1, 1]);

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
    public static PineValue? IdivLeafDelegate(PineValue environment)
    {
        if (!IdivLeafEnvClass.SatisfiedByValue(environment))
        {
            return null;
        }

        var dividendValue = environment.ValueFromPathOrEmptyList([1, 0]);
        var divisorValue = environment.ValueFromPathOrEmptyList([1, 1]);
        var quotientValue = environment.ValueFromPathOrEmptyList([1, 2]);

        if (IntegerEncoding.ParseSignedIntegerRelaxed(dividendValue).IsOkOrNullable() is not { } dividend ||
            IntegerEncoding.ParseSignedIntegerRelaxed(divisorValue).IsOkOrNullable() is not { } divisor ||
            IntegerEncoding.ParseSignedIntegerRelaxed(quotientValue).IsOkOrNullable() is not { } quotient)
        {
            return null;
        }

        if (divisor == System.Numerics.BigInteger.Zero)
        {
            return null;
        }

        return IntegerEncoding.EncodeSignedInteger(quotient + (dividend / divisor));
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
    public static PineValue? GcdLeafDelegate(PineValue environment)
    {
        if (!GcdLeafEnvClass.SatisfiedByValue(environment))
        {
            return null;
        }

        var argA = environment.ValueFromPathOrEmptyList([1, 0]);
        var argB = environment.ValueFromPathOrEmptyList([1, 1]);

        if (IntegerEncoding.ParseSignedIntegerRelaxed(argA).IsOkOrNullable() is not { } a ||
            IntegerEncoding.ParseSignedIntegerRelaxed(argB).IsOkOrNullable() is not { } b)
        {
            return null;
        }

        while (b != System.Numerics.BigInteger.Zero)
        {
            (a, b) = (b, ElmModBy(b, a));
        }

        return IntegerEncoding.EncodeSignedInteger(a);
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
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultLeaves { get; } =
        ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty
        .Add(CompareLeafKey, CompareLeafDelegate)
        .Add(EqLeafKey, EqLeafDelegate)
        .Add(IdivLeafKey, IdivLeafDelegate)
        .Add(GcdLeafKey, GcdLeafDelegate);

    // ========== .NET implementations of Basics.compare ==========
    // The logic below mirrors the legacy Pine.PineVM.Precompiled.BasicsCompare entry point,
    // restated here to avoid a project dependency from Pine.Core onto the pine project.

    private static readonly PineValue s_tag_EQ_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("EQ", []));

    private static readonly PineValue s_tag_LT_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("LT", []));

    private static readonly PineValue s_tag_GT_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("GT", []));

    internal static PineValue BasicsCompare(PineValue a, PineValue b)
    {
        if (a == b)
        {
            return s_tag_EQ_Value;
        }

        var aTag = a.ValueFromPathOrEmptyList([0]);
        var bTag = b.ValueFromPathOrEmptyList([0]);

        if (aTag == ElmValue.ElmStringTypeTagNameAsValue && bTag == ElmValue.ElmStringTypeTagNameAsValue)
        {
            return
                CompareStrings(
                    a.ValueFromPathOrEmptyList([1, 0]),
                    b.ValueFromPathOrEmptyList([1, 0]));
        }

        if (aTag == ElmValue.ElmFloatTypeTagNameAsValue && bTag == ElmValue.ElmFloatTypeTagNameAsValue)
        {
            var aTagArgs = a.ValueFromPathOrEmptyList([1]);
            var bTagArgs = b.ValueFromPathOrEmptyList([1]);

            var numA = aTagArgs.ValueFromPathOrEmptyList([0]);
            var denomA = aTagArgs.ValueFromPathOrEmptyList([1]);

            var numB = bTagArgs.ValueFromPathOrEmptyList([0]);
            var denomB = bTagArgs.ValueFromPathOrEmptyList([1]);

            var leftProduct = BuiltinFunctionSpecialized.int_mul(numA, denomB);
            var rightProduct = BuiltinFunctionSpecialized.int_mul(numB, denomA);

            if (leftProduct == rightProduct)
            {
                return s_tag_EQ_Value;
            }

            if (BuiltinFunction.int_is_sorted_asc(PineValue.List([leftProduct, rightProduct])) ==
                PineKernelValues.TrueValue)
            {
                return s_tag_LT_Value;
            }

            return s_tag_GT_Value;
        }

        if (aTag == ElmValue.ElmFloatTypeTagNameAsValue)
        {
            var aTagArgs = a.ValueFromPathOrEmptyList([1]);

            var numA = aTagArgs.ValueFromPathOrEmptyList([0]);
            var denomA = aTagArgs.ValueFromPathOrEmptyList([1]);

            var rightProduct = BuiltinFunctionSpecialized.int_mul(denomA, b);

            if (numA == rightProduct)
            {
                return s_tag_EQ_Value;
            }

            if (BuiltinFunction.int_is_sorted_asc(PineValue.List([numA, rightProduct])) == PineKernelValues.TrueValue)
            {
                return s_tag_LT_Value;
            }

            return s_tag_GT_Value;
        }

        if (bTag == ElmValue.ElmFloatTypeTagNameAsValue)
        {
            var bTagArgs = b.ValueFromPathOrEmptyList([1]);

            var numB = bTagArgs.ValueFromPathOrEmptyList([0]);
            var denomB = bTagArgs.ValueFromPathOrEmptyList([1]);

            var leftProduct = BuiltinFunctionSpecialized.int_mul(a, denomB);

            if (leftProduct == numB)
            {
                return s_tag_EQ_Value;
            }

            if (BuiltinFunction.int_is_sorted_asc(PineValue.List([leftProduct, numB])) == PineKernelValues.TrueValue)
            {
                return s_tag_LT_Value;
            }

            return s_tag_GT_Value;
        }

        if (a is PineValue.ListValue)
        {
            return CompareLists(a, b);
        }

        if (BuiltinFunction.int_is_sorted_asc(PineValue.List([a, b])) == PineKernelValues.TrueValue)
        {
            return s_tag_LT_Value;
        }

        return s_tag_GT_Value;
    }

    private static PineValue CompareLists(PineValue a, PineValue b)
    {
        if (a == PineValue.EmptyList)
        {
            if (b == PineValue.EmptyList)
            {
                return s_tag_EQ_Value;
            }

            return s_tag_LT_Value;
        }

        if (a is PineValue.ListValue listA && 0 < listA.Items.Length)
        {
            if (b == PineValue.EmptyList)
            {
                return s_tag_GT_Value;
            }

            if (b is PineValue.ListValue listB && 0 < listB.Items.Length)
            {
                var commonLength =
                    listA.Items.Length < listB.Items.Length
                    ?
                    listA.Items.Length
                    :
                    listB.Items.Length;

                for (var i = 0; i < commonLength; ++i)
                {
                    var itemA = listA.Items.Span[i];
                    var itemB = listB.Items.Span[i];

                    var itemOrder = BasicsCompare(itemA, itemB);

                    if (itemOrder != s_tag_EQ_Value)
                    {
                        return itemOrder;
                    }
                }

                if (listA.Items.Length < listB.Items.Length)
                {
                    return s_tag_LT_Value;
                }

                if (listA.Items.Length > listB.Items.Length)
                {
                    return s_tag_GT_Value;
                }

                return s_tag_EQ_Value;
            }

            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    private static PineValue CompareStrings(PineValue stringA, PineValue stringB)
    {
        if (stringA is PineValue.BlobValue blobA && stringB is PineValue.BlobValue blobB)
        {
            var commonLength =
                blobA.Bytes.Length < blobB.Bytes.Length
                ?
                blobA.Bytes.Length
                :
                blobB.Bytes.Length;

            var commonLenghtChars = commonLength / 4;

            for (var i = 0; i < commonLenghtChars; ++i)
            {
                var offset = i * 4;

                var charA = BinaryPrimitives.ReadInt32BigEndian(blobA.Bytes.Span[offset..]);
                var charB = BinaryPrimitives.ReadInt32BigEndian(blobB.Bytes.Span[offset..]);

                if (charA == charB)
                {
                    continue;
                }

                return charA < charB ? s_tag_LT_Value : s_tag_GT_Value;
            }

            return
                blobA.Bytes.Length < blobB.Bytes.Length
                ?
                s_tag_LT_Value
                :
                blobA.Bytes.Length > blobB.Bytes.Length
                ?
                s_tag_GT_Value
                :
                s_tag_EQ_Value;
        }

        return
            stringA == stringB
            ?
            s_tag_EQ_Value
            :
            BuiltinFunction.int_is_sorted_asc(PineValue.List([stringA, stringB])) == PineKernelValues.TrueValue
            ?
            s_tag_LT_Value
            :
            s_tag_GT_Value;
    }

    // ========== .NET implementations of Basics.eq (deep structural equality) ==========
    // The logic below mirrors the legacy Pine.PineVM.Precompiled.BasicsEq entry point and the
    // recursive eqDeep expression built by CoreBasics.BuildEqEncodedBody, restated here to avoid
    // a project dependency from Pine.Core onto the pine project.

    private static readonly PineValue s_integerOneValue =
        IntegerEncoding.EncodeSignedInteger(1);

    internal static PineValue BasicsEq(PineValue a, PineValue b) =>
        BasicsEqRecursive(a, b) ? PineKernelValues.TrueValue : PineKernelValues.FalseValue;

    private static bool BasicsEqRecursive(PineValue a, PineValue b)
    {
        if (a == b)
        {
            return true;
        }

        var aTag = a.ValueFromPathOrEmptyList([0]);
        var bTag = b.ValueFromPathOrEmptyList([0]);

        var aIsFloat = aTag == ElmValue.ElmFloatTypeTagNameAsValue;
        var bIsFloat = bTag == ElmValue.ElmFloatTypeTagNameAsValue;

        if (aIsFloat)
        {
            var aArgs = a.ValueFromPathOrEmptyList([1]);
            var numA = aArgs.ValueFromPathOrEmptyList([0]);
            var denomA = aArgs.ValueFromPathOrEmptyList([1]);

            if (bIsFloat)
            {
                var bArgs = b.ValueFromPathOrEmptyList([1]);
                var numB = bArgs.ValueFromPathOrEmptyList([0]);
                var denomB = bArgs.ValueFromPathOrEmptyList([1]);

                return
                    BuiltinFunctionSpecialized.int_mul(numA, denomB) ==
                    BuiltinFunctionSpecialized.int_mul(numB, denomA);
            }

            return numA == b && denomA == s_integerOneValue;
        }

        if (bIsFloat)
        {
            var bArgs = b.ValueFromPathOrEmptyList([1]);
            var numB = bArgs.ValueFromPathOrEmptyList([0]);
            var denomB = bArgs.ValueFromPathOrEmptyList([1]);

            return a == numB && denomB == s_integerOneValue;
        }

        if (a is PineValue.BlobValue)
        {
            return false;
        }

        var aLength = a is PineValue.ListValue listAForLength ? listAForLength.Items.Length : 0;
        var bLength = b is PineValue.ListValue listBForLength ? listBForLength.Items.Length : 0;

        if (aLength != bLength)
        {
            return false;
        }

        if (aTag == ElmValue.ElmStringTypeTagNameAsValue)
        {
            return false;
        }

        if (aTag == ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            var dictAList = DictToListRecursive(a);
            var dictBList = DictToListRecursive(b);

            return PineValue.List(dictAList) == PineValue.List(dictBList);
        }

        if (aTag == ElmValue.ElmSetTypeTagNameAsValue)
        {
            var dictA = a.ValueFromPathOrEmptyList([1, 0]);
            var dictB = b.ValueFromPathOrEmptyList([1, 0]);

            var dictAKeys = DictKeysRecursive(dictA);
            var dictBKeys = DictKeysRecursive(dictB);

            return PineValue.List(dictAKeys) == PineValue.List(dictBKeys);
        }

        if (a is PineValue.ListValue listA && b is PineValue.ListValue listB)
        {
            return ListsEqualRecursive(listA.Items, listB.Items);
        }

        return false;
    }

    private static ReadOnlyMemory<PineValue> DictToListRecursive(PineValue dict)
    {
        var tag = dict.ValueFromPathOrEmptyList([0]);

        if (tag != ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            return ReadOnlyMemory<PineValue>.Empty;
        }

        var args = dict.ValueFromPathOrEmptyList([1]);

        var key = args.ValueFromPathOrEmptyList([1]);
        var value = args.ValueFromPathOrEmptyList([2]);
        var left = args.ValueFromPathOrEmptyList([3]);
        var right = args.ValueFromPathOrEmptyList([4]);

        var fromLeft = DictToListRecursive(left);
        var fromRight = DictToListRecursive(right);

        var result = new PineValue[fromLeft.Length + fromRight.Length + 1];

        fromLeft.Span.CopyTo(result);

        result[fromLeft.Length] = PineValue.List([key, value]);

        fromRight.Span.CopyTo(result.AsSpan(fromLeft.Length + 1));

        return result;
    }

    private static ReadOnlyMemory<PineValue> DictKeysRecursive(PineValue dict)
    {
        var tag = dict.ValueFromPathOrEmptyList([0]);

        if (tag != ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            return ReadOnlyMemory<PineValue>.Empty;
        }

        var args = dict.ValueFromPathOrEmptyList([1]);

        var key = args.ValueFromPathOrEmptyList([1]);
        var left = args.ValueFromPathOrEmptyList([3]);
        var right = args.ValueFromPathOrEmptyList([4]);

        var fromLeft = DictKeysRecursive(left);
        var fromRight = DictKeysRecursive(right);

        var result = new PineValue[fromLeft.Length + fromRight.Length + 1];

        fromLeft.Span.CopyTo(result);

        result[fromLeft.Length] = key;

        fromRight.Span.CopyTo(result.AsSpan(fromLeft.Length + 1));

        return result;
    }

    private static bool ListsEqualRecursive(
        ReadOnlyMemory<PineValue> listA,
        ReadOnlyMemory<PineValue> listB)
    {
        for (var i = 0; i < listA.Length; ++i)
        {
            if (!BasicsEqRecursive(listA.Span[i], listB.Span[i]))
            {
                return false;
            }
        }

        return true;
    }
}
