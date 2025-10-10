namespace Pine.Core.DotNet.Builtins;

/// <summary>
/// Builder pattern for efficiently creating slices (subsets) of <see cref="PineValue"/> collections.
/// This builder lazily tracks skip and take operations without immediately materializing the result,
/// allowing for optimized composition of multiple slice operations.
/// </summary>
/// <remarks>
/// <para>
/// The builder defers evaluation until <see cref="Evaluate"/> is called, which enables:
/// <list type="bullet">
/// <item><description>Composing multiple skip/take operations before computing the slice</description></item>
/// <item><description>Short-circuiting when the result is known to be empty</description></item>
/// <item><description>Avoiding creation of intermediate collections</description></item>
/// </list>
/// </para>
/// <para>
/// This is particularly useful for optimizing operations like <c>List.drop</c> and <c>List.take</c>
/// in compiled Pine programs, where multiple operations can be fused into a single efficient slice.
/// </para>
/// </remarks>
/// <param name="Original">The source <see cref="PineValue"/> to slice (can be a list or blob).</param>
/// <param name="SkipCount">Number of elements to skip from the start of the original value.</param>
/// <param name="TakeCount">Maximum number of elements to take after skipping.</param>
/// <param name="FinalValue">Cached final result if already computed (e.g., when known to be empty).</param>
public record ImmutableSliceBuilder(
    PineValue Original,
    int SkipCount,
    int TakeCount,
    PineValue? FinalValue)
{
    /// <summary>
    /// Creates a new <see cref="ImmutableSliceBuilder"/> initialized to slice the entire original value.
    /// </summary>
    /// <param name="original">The source <see cref="PineValue"/> to slice.</param>
    /// <returns>A new builder with skip count of 0 and take count of <see cref="int.MaxValue"/>.</returns>
    public static ImmutableSliceBuilder Create(PineValue original) =>
        new(
            Original: original,
            SkipCount: 0,
            TakeCount: int.MaxValue,
            FinalValue: null);

    /// <summary>
    /// Apply <see cref="KernelFunction.skip(PineValue)"/> on the value resulting from the previous operations.
    /// </summary>
    /// <param name="count">The number of additional elements to skip.</param>
    /// <returns>A new builder with the skip count increased by <paramref name="count"/>.</returns>
    /// <remarks>
    /// Multiple skip operations are accumulated, so calling <c>Skip(5).Skip(3)</c> 
    /// will skip a total of 8 elements from the original value.
    /// </remarks>
    public ImmutableSliceBuilder Skip(int count) =>
        new(
            Original,
            SkipCount: SkipCount + count,
            TakeCount: TakeCount,
            FinalValue);

    /// <summary>
    /// Apply <see cref="KernelFunction.take(PineValue)"/> on the value resulting from the previous operations.
    /// </summary>
    /// <param name="count">The maximum number of elements to include in the result.</param>
    /// <returns>A new builder with the take count set to the minimum of the current take count and <paramref name="count"/>.</returns>
    /// <remarks>
    /// Multiple take operations use the most restrictive limit, so calling <c>Take(10).Take(5)</c>
    /// will take at most 5 elements.
    /// </remarks>
    public ImmutableSliceBuilder Take(int count) =>
        new(
            Original,
            SkipCount: SkipCount,
            TakeCount: TakeCount < count ? TakeCount : count,
            FinalValue);

    /// <summary>
    /// Apply <see cref="KernelFunction.skip(PineValue)"/> on the value resulting from the previous operations,
    /// trying to parse the skip count from a <see cref="PineValue"/>.
    /// </summary>
    /// <param name="skipCountValue">A <see cref="PineValue"/> representing the number of elements to skip.</param>
    /// <returns>
    /// A new builder with updated skip count, or a builder with <see cref="PineValue.EmptyList"/> 
    /// as the final value if <paramref name="skipCountValue"/> cannot be converted to an integer.
    /// </returns>
    /// <remarks>
    /// Negative skip counts are treated as zero. If the value cannot be parsed as an integer,
    /// the result is set to an empty list.
    /// </remarks>
    public ImmutableSliceBuilder Skip(PineValue skipCountValue)
    {
        if (KernelFunction.SignedIntegerFromValueRelaxed(skipCountValue) is not { } skipCount)
        {
            return new(
                Original,
                SkipCount,
                TakeCount,
                FinalValue: PineValue.EmptyList);
        }

        if (skipCount < 0)
        {
            skipCount = 0;
        }

        return Skip((int)skipCount);
    }

    /// <summary>
    /// Apply <see cref="KernelFunction.take(PineValue)"/> on the value resulting from the previous operations,
    /// trying to parse the take count from a <see cref="PineValue"/>.
    /// </summary>
    /// <param name="takeCountValue">A <see cref="PineValue"/> representing the maximum number of elements to take.</param>
    /// <returns>
    /// A new builder with updated take count, or a builder with <see cref="PineValue.EmptyList"/>
    /// as the final value if <paramref name="takeCountValue"/> cannot be converted to an integer.
    /// </returns>
    /// <remarks>
    /// Negative take counts are treated as zero. If the value cannot be parsed as an integer,
    /// the result is set to an empty list.
    /// </remarks>
    public ImmutableSliceBuilder Take(PineValue takeCountValue)
    {
        if (KernelFunction.SignedIntegerFromValueRelaxed(takeCountValue) is not { } takeCount)
        {
            return new(
                Original,
                SkipCount,
                TakeCount: 0,
                FinalValue: PineValue.EmptyList);
        }

        if (takeCount < 0)
        {
            takeCount = 0;
        }

        return Take((int)takeCount);
    }

    /// <summary>
    /// Computes the length of the resulting slice based on the original value's length and all skip/take operations.
    /// </summary>
    /// <param name="originalLength">The length of the original value.</param>
    /// <returns>The computed length after applying skip and take operations.</returns>
    /// <remarks>
    /// This method accounts for negative skip counts (treated as zero) and ensures
    /// the result is never negative, even if skip count exceeds the original length.
    /// </remarks>
    private int ComputeResultLengthFromOriginalLength(int originalLength)
    {
        var skip = SkipCount < 0 ? 0 : SkipCount;

        var remaining = originalLength - skip;

        if (remaining <= 0)
        {
            return 0;
        }

        return TakeCount < remaining ? TakeCount : remaining;
    }

    /// <summary>
    /// Gets the length of the slice result without fully evaluating it.
    /// </summary>
    /// <returns>The number of elements that would be in the resulting slice.</returns>
    /// <remarks>
    /// This method efficiently computes the result length based on the original value's length
    /// and the accumulated skip/take operations, without materializing the actual slice.
    /// If a final value has already been cached, returns its length instead.
    /// </remarks>
    /// <exception cref="System.NotImplementedException">
    /// Thrown if the original value is neither a <see cref="PineValue.ListValue"/> nor a <see cref="PineValue.BlobValue"/>.
    /// </exception>
    public int GetLength()
    {
        if (FinalValue is { } finalValue)
        {
            return Internal.KernelFunctionSpecialized.length_as_int(finalValue);
        }

        return Original switch
        {
            PineValue.ListValue listValue =>
            ComputeResultLengthFromOriginalLength(listValue.Items.Length),

            PineValue.BlobValue blobValue =>
            ComputeResultLengthFromOriginalLength(blobValue.Bytes.Length),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected original type: " + Original.GetType().FullName)
        };
    }

    /// <summary>
    /// Checks whether the slice result would be an empty list without fully evaluating it.
    /// </summary>
    /// <returns><see langword="true"/> if the slice result would be empty; otherwise, <see langword="false"/>.</returns>
    /// <remarks>
    /// This method provides an efficient early-exit check for empty results. It returns <see langword="true"/> if:
    /// <list type="bullet">
    /// <item><description>A final value has been cached and it equals <see cref="PineValue.EmptyList"/>, or</description></item>
    /// <item><description>The original is a list and the computed result length is zero.</description></item>
    /// </list>
    /// For blob values, this method returns <see langword="false"/> even if the result would be empty.
    /// </remarks>
    public bool IsEmptyList()
    {
        if (FinalValue is { } finalValue)
        {
            return finalValue == PineValue.EmptyList;
        }

        return
            Original is PineValue.ListValue listValue &&
            ComputeResultLengthFromOriginalLength(listValue.Items.Length) == 0;
    }

    /// <summary>
    /// Gets the first element from the slice result.
    /// </summary>
    /// <returns>
    /// For list values: the first element of the slice, or <see cref="PineValue.EmptyList"/> if empty.
    /// For blob values: a single-byte blob containing the first byte, or <see cref="PineValue.EmptyBlob"/> if empty.
    /// </returns>
    /// <remarks>
    /// This method is optimized to avoid materializing the entire slice when only the head is needed.
    /// If a final value has been cached, it directly returns the head of that cached value.
    /// </remarks>
    public PineValue GetHead()
    {
        if (FinalValue is { } finalValue)
        {
            return KernelFunction.head(finalValue);
        }

        // Optimize: instead of materializing the slice with take(1,...), directly get the element
        return
            KernelFunction.head(
                Internal.KernelFunctionFused.SkipAndTake(
                    takeCount: 1,
                    skipCount: SkipCount,
                    Original));
    }

    /// <summary>
    /// Evaluates the builder and returns the final sliced <see cref="PineValue"/>.
    /// </summary>
    /// <returns>The resulting <see cref="PineValue"/> after applying all skip and take operations.</returns>
    /// <remarks>
    /// <para>
    /// This method materializes the slice result. If a final value has already been cached
    /// (for example, if the builder detected an empty result during construction), that cached
    /// value is returned immediately.
    /// </para>
    /// <para>
    /// Otherwise, the method delegates to <see cref="Internal.KernelFunctionFused.SkipAndTake(int, int, PineValue)"/>
    /// to perform the actual slicing operation on the original value.
    /// </para>
    /// </remarks>
    public PineValue Evaluate()
    {
        if (FinalValue is not null)
        {
            return FinalValue;
        }

        return
            Internal.KernelFunctionFused.SkipAndTake(
                takeCount: TakeCount,
                skipCount: SkipCount,
                Original);
    }
}
