using Pine.Core.DotNet.Builtins;
using Pine.Core.PopularEncodings;
using System.Collections.Generic;
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
    private IReadOnlyList<PineValue>? _list;

    /// <summary>
    /// Create an in-process representation from an already evaluated <see cref="PineValue"/>.
    /// </summary>
    /// <param name="evaluated">The concrete value that should back this instance.</param>
    /// <returns>A new <see cref="PineValueInProcess"/> wrapping <paramref name="evaluated"/>.</returns>
    public static PineValueInProcess Create(PineValue evaluated)
    {
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
    public static PineValueInProcess CreateList(IReadOnlyList<PineValue> list)
    {
        return new PineValueInProcess
        {
            _list = list,
        };
    }

    private PineValueInProcess()
    {
    }

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
            _evaluated = PineValue.List([.. _list]);
            return _evaluated;
        }

        throw new System.NotImplementedException(
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

        throw new System.NotImplementedException(
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

        throw new System.NotImplementedException(
            "Missing branch in " + nameof(IsBlob));
    }

    /// <summary>
    /// Predict the length of the value from <see cref="Evaluate"/> without actually evaluating it.
    /// </summary>
    /// <returns></returns>
    /// <exception cref="System.NotImplementedException"></exception>
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
            var evaluated = _concatBuilder.Evaluate();

            return KernelFunctionSpecialized.length_as_int(evaluated);
        }

        throw new System.NotImplementedException(
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

        var leftEval = left.Evaluate();
        var rightEval = right.Evaluate();

        return new PineValueInProcess
        {
            _concatBuilder = new ImmutableConcatBuilder.Leaf([leftEval, rightEval])
        };
    }

    /// <summary>
    /// Predicts whether two <see cref="PineValueInProcess"/> instances would evaluate to equal <see cref="PineValue"/> instances.
    /// </summary>
    public static bool Equal(PineValueInProcess a, PineValueInProcess b)
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

    private static bool AreListItemsEqual(IReadOnlyList<PineValue> left, IReadOnlyList<PineValue> right)
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
            if (!left[i].Equals(right[i]))
            {
                return false;
            }
        }

        return true;
    }

    private static bool AreListItemsEqual(IReadOnlyList<PineValue> left, PineValue.ListValue right)
    {
        var rightSpan = right.Items.Span;

        if (left.Count != rightSpan.Length)
        {
            return false;
        }

        for (var i = 0; i < rightSpan.Length; i++)
        {
            if (!left[i].Equals(rightSpan[i]))
            {
                return false;
            }
        }

        return true;
    }
}
