using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using Pine.Core.PineVM;
using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;

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
            [new KeyValuePair<IReadOnlyList<int>, PineValue>(new[] { 0 }, PineValue.EmptyList)]);

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
    /// Default precompiled-leaves dictionary contributed by <see cref="CoreBasics"/>.
    /// Suitable for merging into the dictionary consumed by the intermediate VM.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultLeaves { get; } =
        ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty
        .Add(CompareLeafKey, CompareLeafDelegate);

    // ========== .NET implementations of Basics.compare ==========
    // The logic below mirrors the legacy Pine.PineVM.Precompiled.BasicsCompare entry point,
    // restated here to avoid a project dependency from Pine.Core onto the pine project.

    private static readonly PineValue Tag_EQ_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("EQ", []));

    private static readonly PineValue Tag_LT_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("LT", []));

    private static readonly PineValue Tag_GT_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("GT", []));

    internal static PineValue BasicsCompare(PineValue a, PineValue b)
    {
        if (a == b)
        {
            return Tag_EQ_Value;
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

            var leftProduct = KernelFunctionSpecialized.int_mul(numA, denomB);
            var rightProduct = KernelFunctionSpecialized.int_mul(numB, denomA);

            if (leftProduct == rightProduct)
            {
                return Tag_EQ_Value;
            }

            if (KernelFunction.int_is_sorted_asc(PineValue.List([leftProduct, rightProduct])) ==
                PineKernelValues.TrueValue)
            {
                return Tag_LT_Value;
            }

            return Tag_GT_Value;
        }

        if (aTag == ElmValue.ElmFloatTypeTagNameAsValue)
        {
            var aTagArgs = a.ValueFromPathOrEmptyList([1]);

            var numA = aTagArgs.ValueFromPathOrEmptyList([0]);
            var denomA = aTagArgs.ValueFromPathOrEmptyList([1]);

            var rightProduct = KernelFunctionSpecialized.int_mul(denomA, b);

            if (numA == rightProduct)
            {
                return Tag_EQ_Value;
            }

            if (KernelFunction.int_is_sorted_asc(PineValue.List([numA, rightProduct])) == PineKernelValues.TrueValue)
            {
                return Tag_LT_Value;
            }

            return Tag_GT_Value;
        }

        if (bTag == ElmValue.ElmFloatTypeTagNameAsValue)
        {
            var bTagArgs = b.ValueFromPathOrEmptyList([1]);

            var numB = bTagArgs.ValueFromPathOrEmptyList([0]);
            var denomB = bTagArgs.ValueFromPathOrEmptyList([1]);

            var leftProduct = KernelFunctionSpecialized.int_mul(a, denomB);

            if (leftProduct == numB)
            {
                return Tag_EQ_Value;
            }

            if (KernelFunction.int_is_sorted_asc(PineValue.List([leftProduct, numB])) == PineKernelValues.TrueValue)
            {
                return Tag_LT_Value;
            }

            return Tag_GT_Value;
        }

        if (a is PineValue.ListValue)
        {
            return CompareLists(a, b);
        }

        if (KernelFunction.int_is_sorted_asc(PineValue.List([a, b])) == PineKernelValues.TrueValue)
        {
            return Tag_LT_Value;
        }

        return Tag_GT_Value;
    }

    private static PineValue CompareLists(PineValue a, PineValue b)
    {
        if (a == PineValue.EmptyList)
        {
            if (b == PineValue.EmptyList)
            {
                return Tag_EQ_Value;
            }

            return Tag_LT_Value;
        }

        if (a is PineValue.ListValue listA && 0 < listA.Items.Length)
        {
            if (b == PineValue.EmptyList)
            {
                return Tag_GT_Value;
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

                    if (itemOrder != Tag_EQ_Value)
                    {
                        return itemOrder;
                    }
                }

                if (listA.Items.Length < listB.Items.Length)
                {
                    return Tag_LT_Value;
                }

                if (listA.Items.Length > listB.Items.Length)
                {
                    return Tag_GT_Value;
                }

                return Tag_EQ_Value;
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

                return charA < charB ? Tag_LT_Value : Tag_GT_Value;
            }

            return
                blobA.Bytes.Length < blobB.Bytes.Length
                ?
                Tag_LT_Value
                :
                blobA.Bytes.Length > blobB.Bytes.Length
                ?
                Tag_GT_Value
                :
                Tag_EQ_Value;
        }

        return
            stringA == stringB
            ?
            Tag_EQ_Value
            :
            KernelFunction.int_is_sorted_asc(PineValue.List([stringA, stringB])) == PineKernelValues.TrueValue
            ?
            Tag_LT_Value
            :
            Tag_GT_Value;
    }
}
