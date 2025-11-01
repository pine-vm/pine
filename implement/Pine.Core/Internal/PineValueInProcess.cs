using Pine.Core.DotNet.Builtins;
using Pine.Core.PineVM;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Pine.Core.Internal;

/// <summary>
/// Representation of a <see cref="PineValue"/> optimized for the most frequent operations in larger computations.
/// <para>
/// While the general implementation of <see cref="PineValue"/> is optimized for cheap equality checks and addressing,
/// this representation is optimized to improve the runtime efficiency of programs that compose and transform values.
/// Some typical optimizations here are caching of popular derivations and lazy evaluation of chained applications of kernel functions.
/// </para>
/// <para>
/// While this particular implementation illustrates how basic optimization via specialized representations can look like, we expect the majority of representations to be learned by profiling on training workloads (similar to Maps/Hidden Classes in V8)
/// </para>
/// </summary>
public class PineValueInProcess
{
    private PineValue? _evaluated;

    private ImmutableSliceBuilder? _sliceBuilder;

    private ImmutableConcatBuilder? _concatBuilder;

    private BigInteger? _integer;

    /// <summary>
    /// Track a list without involving the general <see cref="PineValue.ListValue"/> system.
    /// </summary>
    private IReadOnlyList<PineValueInProcess>? _list;

    /// <summary>
    /// The value of the empty list, <see cref="PineValue.EmptyList"/>.
    /// </summary>
    public static readonly PineValueInProcess EmptyList = Create(PineValue.EmptyList);

    /// <summary>
    /// The kernel boolean true value, <see cref="PineKernelValues.TrueValue"/>.
    /// </summary>
    public static readonly PineValueInProcess KernelTrueValue = Create(PineKernelValues.TrueValue);

    /// <summary>
    /// The kernel boolean false value, <see cref="PineKernelValues.FalseValue"/>.
    /// </summary>
    public static readonly PineValueInProcess KernelFalseValue = Create(PineKernelValues.FalseValue);


    /// <summary>
    /// Create an in-process representation from an already evaluated <see cref="PineValue"/>.
    /// </summary>
    /// <param name="evaluated">The concrete value that should back this instance.</param>
    /// <returns>A new <see cref="PineValueInProcess"/> wrapping <paramref name="evaluated"/>.</returns>
    public static PineValueInProcess Create(PineValue evaluated)
    {
        {
            if (evaluated == PineValue.EmptyList && EmptyList is { } reusedInst)
            {
                return reusedInst;
            }
        }

        {
            if (evaluated == PineKernelValues.TrueValue && KernelTrueValue is { } reusedInst)
            {
                return reusedInst;
            }
        }

        {
            if (evaluated == PineKernelValues.FalseValue && KernelFalseValue is { } reusedInst)
            {
                return reusedInst;
            }
        }

        return new PineValueInProcess
        {
            _evaluated = evaluated,
        };
    }

    /// <summary>
    /// Create an in-process representation from a list of values without immediately constructing a <see cref="PineValue.ListValue"/> instance.
    /// </summary>
    /// <param name="list">The list items.</param>
    /// <remarks>
    /// This avoids computing aggregate derivations (hash codes, counts, etc.) until they are actually needed via <see cref="Evaluate"/>.
    /// </remarks>
    /// <returns>A new <see cref="PineValueInProcess"/> representing the list.</returns>
    public static PineValueInProcess CreateList(IReadOnlyList<PineValueInProcess> list)
    {
        if (list.Count is 0)
        {
            return EmptyList;
        }

        return new PineValueInProcess
        {
            _list = list,
        };
    }

    public static PineValueInProcess CreateInteger(BigInteger integer)
    {
        return new PineValueInProcess
        {
            _integer = integer,
        };
    }

    public static PineValueInProcess CreateBool(bool boolean)
    {
        return boolean ? KernelTrueValue : KernelFalseValue;
    }

    private PineValueInProcess()
    {
    }

    /// <summary>
    /// Gets the evaluated <see cref="PineValue"/> if it has already been computed, without triggering evaluation.
    /// </summary>
    /// <remarks>
    /// This property returns null if the value has not yet been evaluated. Use <see cref="Evaluate"/> to force evaluation.
    /// </remarks>
    /// <returns>The cached <see cref="PineValue"/> or null if not yet evaluated.</returns>
    public PineValue? EvaluatedOrNull => _evaluated;

    /// <summary>
    /// Materializes this instance into a concrete <see cref="PineValue"/>.
    /// </summary>
    /// <remarks>
    /// If the value was already evaluated earlier, the cached instance is returned. Otherwise the required builder
    /// (slice / concat / integer / list) is evaluated lazily and the result cached for subsequent calls.
    /// </remarks>
    /// <returns>The fully evaluated <see cref="PineValue"/>.</returns>
    public PineValue Evaluate()
    {
        if (_evaluated is not null)
        {
            return _evaluated;
        }

        if (_sliceBuilder is not null)
        {
            _evaluated = _sliceBuilder.Evaluate();
            return _evaluated;
        }

        if (_concatBuilder is not null)
        {
            _evaluated = _concatBuilder.Evaluate();
            return _evaluated;
        }

        if (_integer is not null)
        {
            _evaluated = IntegerEncoding.EncodeSignedInteger(_integer.Value);
            return _evaluated;
        }

        if (_list is not null)
        {
            _evaluated = PineValue.List([.. _list.Select(item => item.Evaluate())]);
            return _evaluated;
        }

        throw new NotImplementedException(
            "Missing branch in PineValueInProcess.AsEvaluated");
    }

    /// <summary>
    /// Attempts to obtain the integer interpretation of this value (signed).
    /// </summary>
    /// <remarks>
    /// If the value has not yet been materialized it will be evaluated first. The parsed integer is cached
    /// so repeated calls are cheap. If the value cannot be interpreted as an integer, <c>null</c> is returned.
    /// </remarks>
    /// <returns>The parsed <see cref="BigInteger"/> or <c>null</c>.</returns>
    public BigInteger? AsInteger()
    {
        if (_integer is not null)
        {
            return _integer;
        }

        var asEvaluated = Evaluate();

        _integer = KernelFunction.SignedIntegerFromValueRelaxed(asEvaluated);

        return _integer;
    }

    /// <summary>
    /// Predict whether the result from <see cref="Evaluate"/> would be a <see cref="PineValue.ListValue"/> value without fully evaluating.
    /// </summary>
    public bool IsList()
    {
        if (_list is not null)
            return true;

        if (_evaluated is not null)
            return _evaluated is PineValue.ListValue;

        if (_integer.HasValue)
            return false;

        if (_sliceBuilder is not null)
            return _sliceBuilder.IsList();

        if (_concatBuilder is not null)
            return _concatBuilder.IsList();

        throw new NotImplementedException(
            "Missing branch in " + nameof(IsList));
    }

    /// <summary>
    /// Predict whether the result from <see cref="Evaluate"/> would be a <see cref="PineValue.BlobValue"/> value without fully evaluating.
    /// </summary>
    public bool IsBlob()
    {
        if (_list is not null)
            return false;

        if (_evaluated is not null)
            return _evaluated is PineValue.BlobValue;

        if (_integer.HasValue)
            return true;

        if (_sliceBuilder is not null)
            return _sliceBuilder.IsBlob();

        if (_concatBuilder is not null)
            return _concatBuilder.IsBlob();

        throw new NotImplementedException(
            "Missing branch in " + nameof(IsBlob));
    }

    /// <summary>
    /// Predict the length of the value from <see cref="Evaluate"/> without actually evaluating it.
    /// </summary>
    /// <returns></returns>
    /// <exception cref="NotImplementedException"></exception>
    public int GetLength()
    {
        if (_list is not null)
            return _list.Count;

        if (_evaluated is not null)
            return KernelFunctionSpecialized.length_as_int(_evaluated);

        if (_integer.HasValue)
        {
            var asEncoded = IntegerEncoding.EncodeSignedInteger(_integer.Value);

            return KernelFunctionSpecialized.length_as_int(asEncoded);
        }

        if (_sliceBuilder is not null)
            return _sliceBuilder.GetLength();

        if (_concatBuilder is not null)
        {
            return _concatBuilder.PredictLength();
        }

        throw new NotImplementedException(
            "Missing branch in " + nameof(GetLength));
    }

    /// <summary>
    /// Analog to <see cref="KernelFunctionSpecialized.skip(BigInteger, PineValue)"/>
    /// </summary>
    /// <param name="skipCount">Number of elements/bytes to skip.</param>
    /// <param name="source">The source sequence value.</param>
    /// <returns>A new <see cref="PineValueInProcess"/> wrapping the lazily skipped value.</returns>
    public static PineValueInProcess Skip(int skipCount, PineValueInProcess source)
    {
        if (skipCount <= 0)
        {
            return source;
        }

        if (source._sliceBuilder is { } sliceBuilder)
        {
            return new PineValueInProcess
            {
                _sliceBuilder = sliceBuilder.Skip(skipCount),
            };
        }

        var evaluated = source.Evaluate();

        return new PineValueInProcess
        {
            _sliceBuilder = ImmutableSliceBuilder.Create(evaluated).Skip(skipCount),
        };
    }

    /// <summary>
    /// Analog to <see cref="KernelFunctionSpecialized.take(BigInteger, PineValue)"/>
    /// </summary>
    /// <param name="takeCount">Maximum number of elements/bytes to take.</param>
    /// <param name="source">The source sequence value.</param>
    /// <returns>A new <see cref="PineValueInProcess"/> representing the sliced value.</returns>
    public static PineValueInProcess Take(int takeCount, PineValueInProcess source)
    {
        if (source._sliceBuilder is { } sliceBuilder)
        {
            return new PineValueInProcess
            {
                _sliceBuilder = sliceBuilder.Take(takeCount),
            };
        }

        var evaluated = source.Evaluate();

        return new PineValueInProcess
        {
            _sliceBuilder = ImmutableSliceBuilder.Create(evaluated).Take(takeCount),
        };
    }

    /// <summary>
    /// Analog to a combination of kernel functions as follows:
    /// <see cref="KernelFunction.reverse(PineValue)"/>,
    /// <see cref="KernelFunction.take(PineValue)"/>,
    /// <see cref="KernelFunction.reverse(PineValue)"/>
    /// </summary>
    public static PineValueInProcess TakeLast(int takeCount, PineValueInProcess source)
    {
        if (source._sliceBuilder is { } sliceBuilder)
        {
            return new PineValueInProcess
            {
                _sliceBuilder = sliceBuilder.TakeLast(takeCount),
            };
        }

        var evaluated = source.Evaluate();

        return new PineValueInProcess
        {
            _sliceBuilder = ImmutableSliceBuilder.Create(evaluated).TakeLast(takeCount),
        };
    }

    /// <summary>
    /// Concatenates the binary content of two PineValueInProcess instances into a single value.
    /// </summary>
    /// <remarks>This method preserves any deferred or incremental evaluation present in the input values,
    /// enabling efficient concatenation of large or lazily-evaluated data. The resulting value may not be fully
    /// materialized until evaluated.</remarks>
    /// <param name="left">The first value to concatenate. Represents the left operand in the concatenation.</param>
    /// <param name="right">The second value to concatenate. Represents the right operand in the concatenation.</param>
    /// <returns>A new PineValueInProcess instance containing the concatenated binary content of the left and right values.</returns>
    public static PineValueInProcess ConcatBinary(
        PineValueInProcess left,
        PineValueInProcess right)
    {
        if (left._concatBuilder is { } leftConcatBuilder &&
            right._concatBuilder is { } rightConcatBuilder)
        {
            return new PineValueInProcess
            {
                _concatBuilder = new ImmutableConcatBuilder.Node([leftConcatBuilder, rightConcatBuilder])
            };
        }

        if (left._concatBuilder is { } leftOnlyConcatBuilder)
        {
            var rightEvaluated = right.Evaluate();

            return new PineValueInProcess
            {
                _concatBuilder = leftOnlyConcatBuilder.AppendItems([rightEvaluated]),
            };
        }

        if (right._concatBuilder is { } rightOnlyConcatBuilder)
        {
            var leftEvaluated = left.Evaluate();

            return new PineValueInProcess
            {
                _concatBuilder = rightOnlyConcatBuilder.PrependItems([leftEvaluated]),
            };
        }

        if (AreEqual(left, PineValue.EmptyList))
        {
            return right;
        }

        if (AreEqual(right, PineValue.EmptyList))
        {
            return left;
        }

        var leftEval = left.Evaluate();
        var rightEval = right.Evaluate();

        return new PineValueInProcess
        {
            _concatBuilder = new ImmutableConcatBuilder.Leaf(new PineValue[] { leftEval, rightEval })
        };
    }

    /// <summary>
    /// Fused application of the kernel functions <see cref="KernelFunction.skip(PineValue)"/> and
    /// <see cref="KernelFunction.head(PineValue)"/> on the value from <see cref="Evaluate"/>
    /// </summary>
    /// <param name="index">The zero-based index of the element to retrieve.</param>
    /// <returns>
    /// For list values: the element at the specified index, or <see cref="EmptyList"/> if index is out of bounds.
    /// For blob values: a single-byte blob containing the byte at the specified index, or <see cref="PineValueInProcess"/> wrapping <see cref="PineValue.EmptyBlob"/> if index is out of bounds.
    /// </returns>
    /// <remarks>
    /// This method is optimized to avoid fully evaluating the value when possible, especially for lists and slice builders.
    /// Returns a <see cref="PineValueInProcess"/> to defer evaluation until needed.
    /// </remarks>
    public PineValueInProcess GetElementAt(int index)
    {
        index =
            index < 0 ? 0 : index;

        if (_sliceBuilder is { } sliceBuilder)
        {
            return Create(sliceBuilder.GetElementAt(index));
        }

        if (_list is { } list)
        {
            if (list.Count <= index)
            {
                return EmptyList;
            }

            return list[index];
        }

        var evaluated = Evaluate();

        if (evaluated is PineValue.ListValue listValue)
        {
            if (listValue.Items.Length <= index)
            {
                return EmptyList;
            }

            return Create(listValue.Items.Span[index]);
        }

        if (evaluated is PineValue.BlobValue blobValue)
        {
            if (blobValue.Bytes.Length <= index)
            {
                return Create(PineValue.EmptyBlob);
            }
            return Create(PineValue.BlobSingleByte(blobValue.Bytes.Span[index]));
        }

        throw new InvalidOperationException(
            "Unsupported value type for GetElementAt: " + evaluated.GetType().FullName);
    }

    /// <summary>
    /// Predicts whether two <see cref="PineValueInProcess"/> instances would evaluate to equal <see cref="PineValue"/> instances.
    /// </summary>
    public static bool AreEqual(PineValueInProcess a, PineValueInProcess b)
    {
        if (ReferenceEquals(a, b))
        {
            return true;
        }

        if (a._evaluated is { } evalA && b._evaluated is { } evalB)
        {
            return evalA.Equals(evalB);
        }

        if (a.IsBlob() != b.IsBlob())
        {
            return false;
        }

        if (a._list is { } listA && b._list is { } listB)
        {
            return AreListItemsEqual(listA, listB);
        }

        if (a._sliceBuilder is { } sliceA && b._sliceBuilder is { } sliceB)
        {
            if (ReferenceEquals(sliceA, sliceB))
            {
                return true;
            }

            if (sliceA.FinalValue is { } finalA && sliceB.FinalValue is { } finalB)
            {
                return finalA.Equals(finalB);
            }

            if (sliceA.GetLength() != sliceB.GetLength())
            {
                return false;
            }
        }

        if (a._concatBuilder is { } concatA && b._concatBuilder is { } concatB)
        {
            if (ReferenceEquals(concatA, concatB))
            {
                return true;
            }

            if (concatA is ImmutableConcatBuilder.Leaf leafA &&
                concatB is ImmutableConcatBuilder.Leaf leafB &&
                AreListItemsEqual(leafA.Values, leafB.Values))
            {
                return true;
            }
        }

        if (a._list is { } onlyListA)
        {
            if (b._evaluated is { } bEvaluated)
            {
                if (bEvaluated is PineValue.ListValue listValueB)
                {
                    return AreListItemsEqual(onlyListA, listValueB);
                }
                else
                {
                    return false;
                }
            }
        }

        if (b._list is { } onlyListB)
        {
            if (a._evaluated is { } aEvaluated)
            {
                if (aEvaluated is PineValue.ListValue listValueA)
                {
                    return AreListItemsEqual(onlyListB, listValueA);
                }
                else
                {
                    return false;
                }
            }
        }

        var aEval = a.Evaluate();
        var bEval = b.Evaluate();

        return aEval.Equals(bEval);
    }

    /// <summary>
    /// Predicts whether a <see cref="PineValueInProcess"/> instance would evaluate to be equal to a given <see cref="PineValue"/>.
    /// </summary>
    public static bool AreEqual(PineValueInProcess inProcess, PineValue pineValue)
    {
        if (inProcess._evaluated is { } evaluated)
        {
            return evaluated.Equals(pineValue);
        }

        if (inProcess.IsBlob() != (pineValue is PineValue.BlobValue))
        {
            return false;
        }

        if (inProcess._list is { } list)
        {
            if (pineValue is PineValue.ListValue listValue)
            {
                return AreListItemsEqual(list, listValue);
            }
            else
            {
                return false;
            }
        }

        var eval = inProcess.Evaluate();

        return eval.Equals(pineValue);
    }

    private static bool AreListItemsEqual(IReadOnlyList<PineValueInProcess> left, IReadOnlyList<PineValueInProcess> right)
    {
        if (ReferenceEquals(left, right))
        {
            return true;
        }

        if (left.Count != right.Count)
        {
            return false;
        }

        for (var i = 0; i < left.Count; i++)
        {
            if (!AreEqual(left[i], right[i]))
            {
                return false;
            }
        }

        return true;
    }

    private static bool AreListItemsEqual(ReadOnlyMemory<PineValue> left, ReadOnlyMemory<PineValue> right)
    {
        if (left.Length != right.Length)
        {
            return false;
        }

        for (var i = 0; i < left.Length; i++)
        {
            if (!left.Span[i].Equals(right.Span[i]))
            {
                return false;
            }
        }

        return true;
    }

    private static bool AreListItemsEqual(IReadOnlyList<PineValueInProcess> left, PineValue.ListValue right)
    {
        var rightSpan = right.Items.Span;

        if (left.Count != rightSpan.Length)
        {
            return false;
        }

        for (var i = 0; i < rightSpan.Length; i++)
        {
            if (!AreEqual(left[i], rightSpan[i]))
            {
                return false;
            }
        }

        return true;
    }

    public static PineValue? ValueFromPathOrNull(
        PineValueInProcess root,
        ReadOnlySpan<int> path)
    {
        var current = root;

        for (var i = 0; i < path.Length; i++)
        {
            current = current.GetElementAt(path[i]);

            if (current is null)
            {
                return null;
            }
        }

        return current.Evaluate();
    }

    public static PineValue? ValueFromPathOrNull(
        PineValueInProcess root,
        IReadOnlyList<int> path)
    {
        var current = root;

        for (var i = 0; i < path.Count; i++)
        {
            current = current.GetElementAt(path[i]);

            if (current is null)
            {
                return null;
            }
        }

        return current.Evaluate();
    }

    /// <summary>
    /// The infix equality operators are intentionally disabled to avoid accidental use of expensive comparisons.
    /// Use <see cref="AreEqual(PineValueInProcess, PineValueInProcess)"/> instead.
    /// </summary>
    public static bool operator ==(PineValueInProcess? left, PineValueInProcess? right) =>
        throw new InvalidOperationException("Use PineValueInProcess.AreEqual(...) instead of '=='.");

    /// <summary>
    /// The infix inequality operator is intentionally disabled to avoid accidental use of expensive comparisons.
    /// Use <see cref="AreEqual(PineValueInProcess, PineValueInProcess)"/> instead.
    /// </summary>
    public static bool operator !=(PineValueInProcess? left, PineValueInProcess? right) =>
        throw new InvalidOperationException("Use PineValueInProcess.AreEqual(...) instead of '!='.");

    /// <inheritdoc/>
    public override bool Equals(object? obj)
    {
        throw new InvalidOperationException(
            "Use the static Equal method instead of instance Equals");
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        return base.GetHashCode();
    }

    /// <summary>
    /// Checks if this value starts with a constant prefix at a variable offset.
    /// </summary>
    /// <param name="offset">The offset (in elements for lists, in bytes for blobs) at which to check for the prefix.</param>
    /// <param name="prefix">The constant prefix value to check for.</param>
    /// <returns>
    /// <c>true</c> if the value starts with the prefix at the given offset; otherwise, <c>false</c>.
    /// </returns>
    /// <remarks>
    /// This method is optimized to avoid fully evaluating the value when possible.
    /// If the offset is not a valid integer, returns true only if the prefix is <see cref="PineValue.EmptyList"/>.
    /// </remarks>
    public bool StartsWithConstAtOffsetVar(int offset, PineValue prefix)
    {
        // Check blob case
        if (prefix is PineValue.BlobValue prefixBlob)
        {
            // Ensure this value is also a blob
            if (!IsBlob())
            {
                return false;
            }

            // For optimization, check length without full evaluation if possible
            var thisLength = GetLength();

            var sliceLength = thisLength - offset;

            if (sliceLength < prefixBlob.Bytes.Length)
            {
                return false;
            }

            if (sliceLength is 0)
                return true;

            // Now we need to check the actual bytes
            var evaluated = Evaluate();

            if (evaluated is PineValue.BlobValue thisBlob)
            {
                var valueBytes = thisBlob.Bytes.Span;

                if (valueBytes
                    .Slice(start: offset, length: prefixBlob.Bytes.Length)
                    .SequenceEqual(prefixBlob.Bytes.Span))
                {
                    return true;
                }
            }

            return false;
        }

        // Check list case
        if (prefix is PineValue.ListValue prefixList)
        {
            // Ensure this value is also a list
            if (!IsList())
            {
                return false;
            }

            // For optimization, check length without full evaluation if possible
            var thisLength = GetLength();

            var sliceLength = thisLength - offset;

            if (sliceLength < prefixList.Items.Length)
            {
                return false;
            }

            if (sliceLength is 0)
                return true;

            // Now we need to check the actual items
            var evaluated = Evaluate();

            if (evaluated is PineValue.ListValue thisList)
            {
                var allItemsMatch = true;

                for (var i = 0; i < prefixList.Items.Length; i++)
                {
                    if (thisList.Items.Span[i + offset] != prefixList.Items.Span[i])
                    {
                        allItemsMatch = false;
                        break;
                    }
                }

                if (allItemsMatch)
                {
                    return true;
                }
            }

            return false;
        }

        throw new NotImplementedException(
            "Unsupported prefix type in StartsWithConstAtOffsetVar: " + prefix.GetType().FullName);
    }
}
