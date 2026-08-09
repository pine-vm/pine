using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using Pine.Core.Interpreter.IntermediateVM;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

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
}
