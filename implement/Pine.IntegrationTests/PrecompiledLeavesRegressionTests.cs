using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using System;
using System.Collections.Generic;
using Xunit;

namespace Pine.IntegrationTests;

public class PrecompiledLeavesRegressionTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Default_VM_retains_handwritten_leaves_and_allows_explicitly_disabling_them(bool disableLeaves)
    {
        var enteredLeaves = new List<PineValue>();

        var vm =
            IntermediateVM.SetupVM.Create(
                disableReductionInCompilation: true,
                precompiledLeaves:
                disableLeaves ? new Dictionary<PineValue, PrecompiledLeaf>() : null,
                reportEnterPrecompiledLeaf: (key, _) => enteredLeaves.Add(key));

        var leafKey = CoreBasicsPrecompiledLeaves.IdivLeafKey;

        var environment =
            PineValue.List(
                [
                PineValue.List([leafKey]),
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(100),
                    IntegerEncoding.EncodeSignedInteger(3),
                    IntegerEncoding.EncodeSignedInteger(0)
                    ])
                ]);

        var result =
            vm.EvaluateExpression(
                new Expression.Eval(
                    Expression.LitralInst(leafKey),
                    Expression.EnvironmentInstance),
                environment)
            .Extract(error => throw new Exception(error));

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(33));

        if (disableLeaves)
        {
            enteredLeaves.Should().BeEmpty();
        }
        else
        {
            enteredLeaves.Should().Contain(leafKey);
        }
    }
}
