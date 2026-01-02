using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class BasicArithmeticTests
{
    [Fact]
    public void Int_sub_zero()
    {
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_sub,
                ElmValue.Integer(13),
                ElmValue.Integer(0));

        evalResult.value.Should().Be(ElmValue.Integer(13));
    }

    [Fact]
    public void Int_sub_17()
    {
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_sub,
                ElmValue.Integer(13),
                ElmValue.Integer(17));

        evalResult.value.Should().Be(ElmValue.Integer(-4));
    }

    [Fact]
    public void Generic_sub_int_13_int_17()
    {
        var resultValue =
            ApplyGeneric(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Subtract_FunctionValue(),
                [
                    ElmValue.Integer(13),
                    ElmValue.Integer(17)
                ]);

        resultValue.Should().Be(ElmValue.Integer(-4));
    }

    [Fact]
    public void Generic_sub_int_13_float_zero_point_1()
    {
        var resultValue =
            ApplyGeneric(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Subtract_FunctionValue(),
                [
                    ElmValue.ElmFloat.Convert(13),
                    ElmValue.ElmFloat.Convert(0.1),
                ]);

        resultValue.Should().Be(ElmValue.ElmFloat.Convert(12.9));
    }

    [Fact]
    public void Number_negate_17()
    {
        var resultValue =
            ApplyDirectUnary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Negate_FunctionValue(),
                ElmValue.Integer(17));

        resultValue.Should().Be(ElmValue.Integer(-17));
    }

    [Fact]
    public void Number_negate_minus_17()
    {
        var resultValue =
            ApplyDirectUnary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Negate_FunctionValue(),
                ElmValue.Integer(-17));

        resultValue.Should().Be(ElmValue.Integer(17));
    }

    [Fact]
    public void Number_negate_float_17_point_3()
    {
        var resultValue =
            ApplyDirectUnary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Negate_FunctionValue(),
                ElmValue.ElmFloat.Convert(17.3));

        resultValue.Should().Be(ElmValue.ElmFloat.Convert(-17.3));
    }

    // ========== Tests for Int_div ==========

    [Fact]
    public void Int_div_10_by_3()
    {
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_div,
                ElmValue.Integer(10),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(3));
    }

    [Fact]
    public void Int_div_negative_10_by_3()
    {
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_div,
                ElmValue.Integer(-10),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(-3));
    }

    [Fact]
    public void Int_div_negative_1000_by_3()
    {
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_div,
                ElmValue.Integer(-1_000),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(-333));
    }

    [Fact]
    public void Int_div_1000_by_17()
    {
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_div,
                ElmValue.Integer(1_000),
                ElmValue.Integer(17));

        evalResult.value.Should().Be(ElmValue.Integer(58));
    }

    [Fact]
    public void Int_div_1_000_000_by_257()
    {
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_div,
                ElmValue.Integer(1_000_000),
                ElmValue.Integer(257));

        evalResult.value.Should().Be(ElmValue.Integer(3891));

        evalResult.profile.InvocationCount.Should().BeLessThan(40);
    }

    [Fact]
    public void Int_div_10_by_negative_3()
    {
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_div,
                ElmValue.Integer(10),
                ElmValue.Integer(-3));

        evalResult.value.Should().Be(ElmValue.Integer(-3));
    }

    [Fact]
    public void Int_div_by_zero_returns_zero()
    {
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_div,
                ElmValue.Integer(10),
                ElmValue.Integer(0));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    // ========== Tests for Generic_Addition ==========

    [Fact]
    public void Generic_add_int_13_int_17()
    {
        var resultValue =
            ApplyGeneric(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Add_FunctionValue(),
                [
                    ElmValue.Integer(13),
                    ElmValue.Integer(17)
                ]);

        resultValue.Should().Be(ElmValue.Integer(30));
    }

    [Fact]
    public void Generic_add_int_negative_5_int_3()
    {
        var resultValue =
            ApplyGeneric(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Add_FunctionValue(),
                [
                    ElmValue.Integer(-5),
                    ElmValue.Integer(3)
                ]);

        resultValue.Should().Be(ElmValue.Integer(-2));
    }

    // ========== Tests for modBy ==========

    [Fact]
    public void Int_modBy_3_of_10()
    {
        // modBy 3 10 == 1
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_modBy,
                ElmValue.Integer(3),
                ElmValue.Integer(10));

        evalResult.value.Should().Be(ElmValue.Integer(1));
    }

    [Fact]
    public void Int_modBy_3_of_negative_10()
    {
        // modBy 3 -10 == 2  (always positive result in Elm modBy)
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_modBy,
                ElmValue.Integer(3),
                ElmValue.Integer(-10));

        evalResult.value.Should().Be(ElmValue.Integer(2));
    }

    // ========== Tests for remainderBy ==========

    [Fact]
    public void Int_remainderBy_3_of_10()
    {
        // remainderBy 3 10 == 1
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_remainderBy,
                ElmValue.Integer(3),
                ElmValue.Integer(10));

        evalResult.value.Should().Be(ElmValue.Integer(1));
    }

    [Fact]
    public void Int_remainderBy_3_of_negative_10()
    {
        // remainderBy 3 -10 == -1  (sign follows dividend in remainder)
        var evalResult =
            ApplyDirectBinary(
                Core.Elm.ElmCompilerInDotnet.BasicArithmetic.Int_remainderBy,
                ElmValue.Integer(3),
                ElmValue.Integer(-10));

        evalResult.value.Should().Be(ElmValue.Integer(-1));
    }

    private static readonly PineVMParseCache s_pineVMParseCache = new();

    private static (ElmValue value, EvaluationReport profile) ApplyDirectBinary(
        System.Func<Expression, Expression, Expression> function,
        ElmValue left,
        ElmValue right)
    {
        var evalResult =
            ApplyDirectBinary(
                function,
                ElmValueEncoding.ElmValueAsPineValue(left),
                ElmValueEncoding.ElmValueAsPineValue(right));

        var elmValue =
            ElmValueEncoding.PineValueAsElmValue(evalResult.ReturnValue.Evaluate(), null, null)
            .Extract(err => throw new System.Exception("Failed decode as Elm value: " + err));

        return (elmValue, evalResult);
    }

    private static EvaluationReport ApplyDirectBinary(
        System.Func<Expression, Expression, Expression> function,
        PineValue left,
        PineValue right)
    {
        var expression =
            function(Expression.LiteralInstance(left), Expression.LiteralInstance(right));

        return ElmCompilerTestHelper.EvaluateWithProfiling(expression, PineValue.EmptyBlob).evalReport;
    }

    private static ElmValue ApplyDirectUnary(
        PineValue functionValue,
        ElmValue argument)
    {
        return ApplyGeneric(
            functionValue,
            [argument]);
    }

    private static ElmValue ApplyGeneric(
        PineValue functionValue,
        ElmValue[] arguments)
    {
        var pineArguments =
            new PineValue[arguments.Length];

        for (var i = 0; i < arguments.Length; i++)
        {
            pineArguments[i] =
                ElmValueEncoding.ElmValueAsPineValue(arguments[i]);
        }

        var resultPineValue =
            ApplyGeneric(functionValue, pineArguments);

        return
            ElmValueEncoding.PineValueAsElmValue(resultPineValue, null, null)
            .Extract(err => throw new System.Exception("Failed decode as Elm value: " + err));
    }

    /// <summary>
    /// <see href="https://github.com/pine-vm/pine/blob/2c3b26abb48769712eff1ce8834eb6579cb11add/implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md#function-values-and-generic-function-application"/>
    /// </summary>
    private static PineValue ApplyGeneric(
        PineValue functionValue,
        IReadOnlyList<PineValue> arguments)
    {
        var currentValue = functionValue;

        var vm = ElmCompilerTestHelper.PineVMForProfiling(_ => { });

        for (var i = 0; i < arguments.Count; i++)
        {
            var asIndependent =
                new Expression.ParseAndEval(
                    encoded: Expression.LiteralInstance(currentValue),
                    environment: Expression.LiteralInstance(arguments[i]));

            currentValue =
                vm.EvaluateExpression(asIndependent, PineValue.EmptyBlob)
                .Extract(err => throw new System.Exception("Failed eval: " + err));
        }

        return currentValue;
    }
}
