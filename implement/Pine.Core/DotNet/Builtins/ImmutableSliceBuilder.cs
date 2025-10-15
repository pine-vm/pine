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
    int? TakeCount,
    PineValue? FinalValue)
{
    /// <summary>
    /// Creates a new <see cref="ImmutableSliceBuilder"/> initialized to slice the entire original value.
    /// </summary>
    /// <param name="original">The source <see cref="PineValue"/> to slice.</param>
    /// <returns>A new builder with skip count of 0 and no take limit.</returns>
    public static ImmutableSliceBuilder Create(PineValue original) =>
        new(
            Original: original,
            SkipCount: 0,
            TakeCount: null,
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
    public ImmutableSliceBuilder Skip(int count)
    {
        if (count <= 0)
        {
            return this;
        }

        var newSkipCount = checked(SkipCount + count);

        var newTakeCount = TakeCount;

        if (newTakeCount is { } currentTake)
        {
            var reduction = count <= currentTake ? count : currentTake;

            var adjusted = currentTake - reduction;

            newTakeCount = adjusted <= 0 ? 0 : adjusted;
        }

        return new(
            Original,
            SkipCount: newSkipCount,
            TakeCount: newTakeCount,
            FinalValue);
    }

    /// <summary>
    /// Apply <see cref="KernelFunction.take(PineValue)"/> on the value resulting from the previous operations.
    /// </summary>
    /// <param name="count">The maximum number of elements to include in the result.</param>
    /// <returns>A new builder with the take count set to the minimum of the current take count and <paramref name="count"/>.</returns>
    /// <remarks>
    /// Multiple take operations use the most restrictive limit, so calling <c>Take(10).Take(5)</c>
    /// will take at most 5 elements.
    /// </remarks>
    public ImmutableSliceBuilder Take(int count)
    {
        if (count < 0)
        {
            count = 0;
        }

        var newTakeCount = TakeCount is { } currentTake ? (currentTake < count ? currentTake : count) : count;

        return new(
            Original,
            SkipCount: SkipCount,
            TakeCount: newTakeCount,
            FinalValue);
    }

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
    /// Take elements from the end of the value resulting from the previous operations.
    /// </summary>
    /// <param name="count">The number of elements to take from the end.</param>
    /// <returns>A new builder configured to take <paramref name="count"/> elements from the end.</returns>
    /// <remarks>
    /// <para>
    /// This operation is equivalent to applying reverse, then take, then reverse again,
    /// but is implemented more efficiently by calculating the appropriate skip amount.
    /// </para>
    /// <para>
    /// For example, on a sequence of [1, 2, 3, 4, 5], calling TakeLast(2) will result in [4, 5].
    /// </para>
    /// <para>
    /// Negative counts are treated as zero. If the count exceeds the available elements,
    /// all remaining elements are returned.
    /// </para>
    /// </remarks>
    public ImmutableSliceBuilder TakeLast(int count)
    {
        if (count < 0)
        {
            count = 0;
        }

        var currentLength = GetLength();

        if (currentLength <= count)
        {
            // Taking more or equal than what's available - return as is
            return this;
        }

        if (count is 0)
        {
            // TakeLast(0) should return empty
            PineValue emptyFinalValue = Original switch
            {
                PineValue.ListValue =>
                PineValue.EmptyList,

                PineValue.BlobValue =>
                PineValue.EmptyBlob,

                _ =>
                throw new System.NotImplementedException(
                    "Unexpected original type: " + Original.GetType().FullName)
            };

            return new(
                Original,
                SkipCount,
                TakeCount: 0,
                FinalValue: emptyFinalValue);
        }

        // To take the last N elements, we need to skip (length - N) elements
        var skipAmount = currentLength - count;

        return Skip(skipAmount).Take(count);
    }

    /// <summary>
    /// Take elements from the end of the value resulting from the previous operations,
    /// trying to parse the count from a <see cref="PineValue"/>.
    /// </summary>
    /// <param name="takeCountValue">A <see cref="PineValue"/> representing the number of elements to take from the end.</param>
    /// <returns>
    /// A new builder with updated operations, or a builder with <see cref="PineValue.EmptyList"/>
    /// as the final value if <paramref name="takeCountValue"/> cannot be converted to an integer.
    /// </returns>
    /// <remarks>
    /// Negative take counts are treated as zero. If the value cannot be parsed as an integer,
    /// the result is set to an empty list.
    /// </remarks>
    public ImmutableSliceBuilder TakeLast(PineValue takeCountValue)
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

        return TakeLast((int)takeCount);
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
        var skip =
            SkipCount < 0 ? 0 : SkipCount;

        var remaining = originalLength - skip;

        if (remaining <= 0)
        {
            return 0;
        }

        if (TakeCount is not { } takeCount)
        {
            return remaining;
        }

        return takeCount < remaining ? takeCount : remaining;
    }

    /// <summary>
    /// Predict the length from <see cref="KernelFunction.length(PineValue)"/> used on the value from <see cref="Evaluate"/>.
    /// </summary>
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
    /// Checks whether the result from <see cref="Evaluate"/> would be an empty <see cref="PineValue.ListValue"/> without fully evaluating it.
    /// </summary>
    /// <returns><see langword="true"/> if the slice result would be empty; otherwise, <see langword="false"/>.</returns>
    public bool IsEmptyList()
    {
        if (FinalValue is { } finalValue)
        {
            return finalValue == PineValue.EmptyList;
        }

        return
            Original is PineValue.ListValue listValue &&
            ComputeResultLengthFromOriginalLength(listValue.Items.Length) is 0;
    }

    /// <summary>
    /// Predict whether the result from <see cref="Evaluate"/> would be a <see cref="PineValue.ListValue"/> value without fully evaluating.
    /// </summary>
    public bool IsList()
    {
        if (FinalValue is { } finalValue)
        {
            return finalValue is PineValue.ListValue;
        }

        return Original is PineValue.ListValue;
    }

    /// <summary>
    /// Predict whether the result from <see cref="Evaluate"/> would be a <see cref="PineValue.BlobValue"/> value without fully evaluating.
    /// </summary>
    public bool IsBlob()
    {
        if (FinalValue is { } finalValue)
        {
            return finalValue is PineValue.BlobValue;
        }

        return Original is PineValue.BlobValue;
    }

    /// <summary>
    /// Application of <see cref="KernelFunction.head(PineValue)"/> on the value from <see cref="Evaluate"/>
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
        return GetElementAt(0);
    }

    /// <summary>
    /// Fused application of the kernel functions <see cref="KernelFunction.skip(PineValue)"/> and
    /// <see cref="KernelFunction.head(PineValue)"/> on the value from <see cref="Evaluate"/>
    /// </summary>
    /// <param name="index">The zero-based index of the element to retrieve from the slice.</param>
    /// <returns>
    /// For list values: the element at the specified index, or <see cref="PineValue.EmptyList"/> if index is out of bounds.
    /// For blob values: a single-byte blob containing the byte at the specified index, or <see cref="PineValue.EmptyBlob"/> if index is out of bounds.
    /// </returns>
    /// <remarks>
    /// <para>
    /// This method is a generalized form of <see cref="GetHead"/>, where <c>GetHead()</c> is equivalent to <c>GetElementAt(0)</c>.
    /// </para>
    /// <para>
    /// This method is optimized to avoid materializing the entire slice when only a single element is needed.
    /// If a final value has been cached, it retrieves the element from that cached value. Otherwise, it directly
    /// accesses the element from the original value using the accumulated skip offset plus the requested index.
    /// </para>
    /// </remarks>
    public PineValue GetElementAt(int index)
    {
        index =
            index < 0 ? 0 : index;

        // Check if index is beyond the take count (if set)
        if (TakeCount is { } takeCount && index >= takeCount)
        {
            return Original switch
            {
                PineValue.ListValue =>
                PineValue.EmptyList,

                PineValue.BlobValue =>
                PineValue.EmptyBlob,

                _ =>
                throw new System.NotImplementedException(
                    "Unexpected source type: " + Original.GetType())
            };
        }

        if (FinalValue is { } finalValue)
        {
            return
                KernelFunction.head(
                    Internal.KernelFunctionFused.SkipAndTake(
                        takeCount: 1,
                        skipCount: index,
                        finalValue));
        }

        // Optimize: instead of materializing the slice, directly get the element at the index
        // The actual index in the original is SkipCount + index
        var actualSkipCount = checked(SkipCount + index);

        return
            KernelFunction.head(
                Internal.KernelFunctionFused.SkipAndTake(
                    takeCount: 1,
                    skipCount: actualSkipCount,
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

        if (TakeCount is not { } takeCount)
        {
            var skipCount =
                SkipCount < 0 ? 0 : SkipCount;

            if (skipCount is 0)
            {
                return Original;
            }

            return
                Internal.KernelFunctionSpecialized.skip(
                    skipCount,
                    Original);
        }

        return
            Internal.KernelFunctionFused.SkipAndTake(
                takeCount: takeCount,
                skipCount: SkipCount,
                Original);
    }
}
