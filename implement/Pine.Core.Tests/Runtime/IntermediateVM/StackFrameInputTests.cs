using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using Xunit;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class StackFrameInputTests
{
    [Fact]
    public void FromEnvironmentValue_defers_argument_evaluation()
    {
        var left = PineValue.Blob(new byte[16]);
        var right = PineValue.Blob(new byte[16]);

        var deferredEnvironment =
            PineValueInProcess.ConcatBinary(
                PineValueInProcess.Create(left),
                PineValueInProcess.Create(right));

        deferredEnvironment.EvaluatedOrNull.Should().BeNull();

        var input =
            StackFrameInput.FromEnvironmentValue(
                deferredEnvironment,
                StaticFunctionInterface.Generic);

        deferredEnvironment.EvaluatedOrNull.Should().BeNull();

        input.EvaluatedArguments[0].Should().Be(
            BuiltinFunction.concat(PineValue.List([left, right])));

        deferredEnvironment.EvaluatedOrNull.Should().NotBeNull();
    }

    [Fact]
    public void Equivalent_inputs_have_equal_hash_codes_and_values()
    {
        var parameters = StaticFunctionInterface.FromPathsSorted([[0], [1]]);

        var first =
            StackFrameInput.FromEnvironmentValue(
                PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]),
                parameters);

        var second =
            StackFrameInput.FromArguments(
                parameters,
                [
                PineValueInProcess.Create(PineValue.Blob([1])),
                PineValueInProcess.Create(PineValue.Blob([2])),
                ]);

        first.GetHashCode().Should().Be(second.GetHashCode());
        first.Equals(second).Should().BeTrue();
    }

    [Fact]
    public void Eliminated_environment_inputs_participate_in_memory_and_persistent_cache_keys()
    {
        var parameters = StaticFunctionInterface.FromPathsSorted([[1]]);

        var firstConstraint =
            PineValueClass.Create([new([0], IntegerEncoding.EncodeSignedInteger(7))]);

        var secondConstraint =
            PineValueClass.Create([new([0], IntegerEncoding.EncodeSignedInteger(100))]);

        var argument = PineValueInProcess.Create(IntegerEncoding.EncodeSignedInteger(10));
        var first = StackFrameInput.FromArguments(parameters, [argument], firstConstraint);
        var second = StackFrameInput.FromArguments(parameters, [argument], secondConstraint);

        var equivalent =
            StackFrameInput.FromEnvironmentValue(
                PineValue.List([IntegerEncoding.EncodeSignedInteger(7), argument.Evaluate()]),
                parameters,
                firstConstraint);

        var hashes = new StackFrameInputHash();

        first.Should().NotBe(second);
        first.Should().Be(equivalent);

        Convert.ToHexString(hashes.ComposeHashBytes(first).HashBytes.Span).Should()
            .NotBe(Convert.ToHexString(hashes.ComposeHashBytes(second).HashBytes.Span))
            .And.Be(Convert.ToHexString(hashes.ComposeHashBytes(equivalent).HashBytes.Span));

        first.ToValueClass().TryGetValue([0]).Should().Be(IntegerEncoding.EncodeSignedInteger(7));
    }
}
