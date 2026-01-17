using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class BasicArithmeticTests
{
    [Fact]
    public void Int_sub_zero()
    {
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_sub,
                ElmValue.Integer(13),
                ElmValue.Integer(0));

        evalResult.value.Should().Be(ElmValue.Integer(13));
    }

    [Fact]
    public void Int_sub_17()
    {
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_sub,
                ElmValue.Integer(13),
                ElmValue.Integer(17));

        evalResult.value.Should().Be(ElmValue.Integer(-4));
    }

    [Fact]
    public void Generic_sub_int_13_int_17()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.Sub_FunctionValue(),
                [
                    ElmValue.Integer(13),
                    ElmValue.Integer(17)
                ]);

        resultValue.Should().Be(ElmValue.Integer(-4));
    }

    [Fact]
    public void Generic_sub_float_3_point_7_negative_float_0_point_3()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.Sub_FunctionValue(),
                [
                    ElmValue.ElmFloat.Convert(3.7),
                    ElmValue.ElmFloat.Convert(-0.3),
                ]);

        resultValue.Should().Be(ElmValue.Integer(4));
    }

    [Fact]
    public void Generic_sub_int_13_float_zero_point_1()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.Sub_FunctionValue(),
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
                BasicArithmetic.Negate_FunctionValue(),
                ElmValue.Integer(17));

        resultValue.Should().Be(ElmValue.Integer(-17));
    }

    [Fact]
    public void Number_negate_minus_17()
    {
        var resultValue =
            ApplyDirectUnary(
                BasicArithmetic.Negate_FunctionValue(),
                ElmValue.Integer(-17));

        resultValue.Should().Be(ElmValue.Integer(17));
    }

    [Fact]
    public void Number_negate_float_17_point_3()
    {
        var resultValue =
            ApplyDirectUnary(
                BasicArithmetic.Negate_FunctionValue(),
                ElmValue.ElmFloat.Convert(17.3));

        resultValue.Should().Be(ElmValue.ElmFloat.Convert(-17.3));
    }

    // ========== Tests for Int_div ==========

    [Fact]
    public void Int_div_10_by_3()
    {
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_div,
                ElmValue.Integer(10),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(3));
    }

    [Fact]
    public void Int_div_negative_10_by_3()
    {
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_div,
                ElmValue.Integer(-10),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(-3));
    }

    [Fact]
    public void Int_div_negative_1000_by_3()
    {
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_div,
                ElmValue.Integer(-1_000),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(-333));
    }

    [Fact]
    public void Int_div_1000_by_17()
    {
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_div,
                ElmValue.Integer(1_000),
                ElmValue.Integer(17));

        evalResult.value.Should().Be(ElmValue.Integer(58));
    }

    [Fact]
    public void Int_div_1_000_000_by_257()
    {
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_div,
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
                BasicArithmetic.Int_div,
                ElmValue.Integer(10),
                ElmValue.Integer(-3));

        evalResult.value.Should().Be(ElmValue.Integer(-3));
    }

    [Fact]
    public void Int_div_by_zero_returns_zero()
    {
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_div,
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
                BasicArithmetic.Add_FunctionValue(),
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
                BasicArithmetic.Add_FunctionValue(),
                [
                    ElmValue.Integer(-5),
                    ElmValue.Integer(3)
                ]);

        resultValue.Should().Be(ElmValue.Integer(-2));
    }

    [Fact]
    public void Generic_add_float_1_point_5_float_2_point_5()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.Add_FunctionValue(),
                [
                    ElmValue.ElmFloat.Convert(1.5),
                    ElmValue.ElmFloat.Convert(2.5)
                ]);

        resultValue.Should().Be(ElmValue.Integer(4));
    }

    // ========== Tests for Int_div_FunctionValue ==========

    [Fact]
    public void Int_div_FunctionValue_10_by_3()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.Int_div_FunctionValue(),
                [
                    ElmValue.Integer(10),
                    ElmValue.Integer(3)
                ]);

        resultValue.Should().Be(ElmValue.Integer(3));
    }

    [Fact]
    public void Int_div_FunctionValue_negative_10_by_3()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.Int_div_FunctionValue(),
                [
                    ElmValue.Integer(-10),
                    ElmValue.Integer(3)
                ]);

        resultValue.Should().Be(ElmValue.Integer(-3));
    }

    [Fact]
    public void Int_div_FunctionValue_10_by_negative_3()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.Int_div_FunctionValue(),
                [
                    ElmValue.Integer(10),
                    ElmValue.Integer(-3)
                ]);

        resultValue.Should().Be(ElmValue.Integer(-3));
    }

    [Fact]
    public void Int_div_FunctionValue_by_zero_returns_zero()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.Int_div_FunctionValue(),
                [
                    ElmValue.Integer(10),
                    ElmValue.Integer(0)
                ]);

        resultValue.Should().Be(ElmValue.Integer(0));
    }

    [Fact]
    public void Int_div_FunctionValue_1000_by_17()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.Int_div_FunctionValue(),
                [
                    ElmValue.Integer(1_000),
                    ElmValue.Integer(17)
                ]);

        resultValue.Should().Be(ElmValue.Integer(58));
    }

    // ========== Tests for modBy ==========

    [Fact]
    public void Int_modBy_3_of_10()
    {
        // modBy 3 10 == 1
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_modBy,
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
                BasicArithmetic.Int_modBy,
                ElmValue.Integer(3),
                ElmValue.Integer(-10));

        evalResult.value.Should().Be(ElmValue.Integer(2));
    }

    [Fact]
    public void Int_modBy_4_of_12()
    {
        // modBy 4 12 == 0  (exactly divisible)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_modBy,
                ElmValue.Integer(4),
                ElmValue.Integer(12));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    [Fact]
    public void Int_modBy_5_of_3()
    {
        // modBy 5 3 == 3  (dividend < divisor)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_modBy,
                ElmValue.Integer(5),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(3));
    }

    [Fact]
    public void Int_modBy_7_of_negative_3()
    {
        // modBy 7 -3 == 4  (result is always non-negative when divisor is positive)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_modBy,
                ElmValue.Integer(7),
                ElmValue.Integer(-3));

        evalResult.value.Should().Be(ElmValue.Integer(4));
    }

    [Fact]
    public void Int_modBy_1_of_100()
    {
        // modBy 1 100 == 0  (any number mod 1 is 0)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_modBy,
                ElmValue.Integer(1),
                ElmValue.Integer(100));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    [Fact]
    public void Int_modBy_17_of_1000()
    {
        // modBy 17 1000 == 14  (1000 = 58*17 + 14)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_modBy,
                ElmValue.Integer(17),
                ElmValue.Integer(1000));

        evalResult.value.Should().Be(ElmValue.Integer(14));
    }

    [Fact]
    public void Int_modBy_10_of_0()
    {
        // modBy 10 0 == 0  (0 mod anything is 0)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_modBy,
                ElmValue.Integer(10),
                ElmValue.Integer(0));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    // ========== Tests for remainderBy ==========

    [Fact]
    public void Int_remainderBy_3_of_10()
    {
        // remainderBy 3 10 == 1
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_remainderBy,
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
                BasicArithmetic.Int_remainderBy,
                ElmValue.Integer(3),
                ElmValue.Integer(-10));

        evalResult.value.Should().Be(ElmValue.Integer(-1));
    }

    [Fact]
    public void Int_remainderBy_4_of_12()
    {
        // remainderBy 4 12 == 0  (exactly divisible)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_remainderBy,
                ElmValue.Integer(4),
                ElmValue.Integer(12));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    [Fact]
    public void Int_remainderBy_5_of_3()
    {
        // remainderBy 5 3 == 3  (dividend < divisor)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_remainderBy,
                ElmValue.Integer(5),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(3));
    }

    [Fact]
    public void Int_remainderBy_7_of_negative_3()
    {
        // remainderBy 7 -3 == -3  (sign follows dividend)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_remainderBy,
                ElmValue.Integer(7),
                ElmValue.Integer(-3));

        evalResult.value.Should().Be(ElmValue.Integer(-3));
    }

    [Fact]
    public void Int_remainderBy_1_of_100()
    {
        // remainderBy 1 100 == 0  (any number remainder 1 is 0)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_remainderBy,
                ElmValue.Integer(1),
                ElmValue.Integer(100));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    [Fact]
    public void Int_remainderBy_17_of_1000()
    {
        // remainderBy 17 1000 == 14  (1000 = 58*17 + 14)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_remainderBy,
                ElmValue.Integer(17),
                ElmValue.Integer(1000));

        evalResult.value.Should().Be(ElmValue.Integer(14));
    }

    [Fact]
    public void Int_remainderBy_17_of_negative_1000()
    {
        // remainderBy 17 -1000 == -14  (sign follows dividend)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_remainderBy,
                ElmValue.Integer(17),
                ElmValue.Integer(-1000));

        evalResult.value.Should().Be(ElmValue.Integer(-14));
    }

    [Fact]
    public void Int_remainderBy_10_of_0()
    {
        // remainderBy 10 0 == 0  (0 remainder anything is 0)
        var evalResult =
            ApplyDirectBinary(
                BasicArithmetic.Int_remainderBy,
                ElmValue.Integer(10),
                ElmValue.Integer(0));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    // ========== Tests for modBy FunctionValue ==========

    [Fact]
    public void Int_modBy_FunctionValue_3_of_10()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.Int_modBy_FunctionValue(),
                [
                    ElmValue.Integer(3),
                    ElmValue.Integer(10)
                ]);

        resultValue.Should().Be(ElmValue.Integer(1));
    }

    [Fact]
    public void Int_modBy_FunctionValue_3_of_negative_10()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.Int_modBy_FunctionValue(),
                [
                    ElmValue.Integer(3),
                    ElmValue.Integer(-10)
                ]);

        resultValue.Should().Be(ElmValue.Integer(2));
    }

    // ========== Tests for remainderBy FunctionValue ==========

    [Fact]
    public void Int_remainderBy_FunctionValue_3_of_10()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.RemainderBy_FunctionValue(),
                [
                    ElmValue.Integer(3),
                    ElmValue.Integer(10)
                ]);

        resultValue.Should().Be(ElmValue.Integer(1));
    }

    [Fact]
    public void Int_remainderBy_FunctionValue_3_of_negative_10()
    {
        var resultValue =
            ApplyGeneric(
                BasicArithmetic.RemainderBy_FunctionValue(),
                [
                    ElmValue.Integer(3),
                    ElmValue.Integer(-10)
                ]);

        resultValue.Should().Be(ElmValue.Integer(-1));
    }

    private static readonly PineVMParseCache s_pineVMParseCache = new();

    #region TryInterpret Tests

    [Fact]
    public void TryInterpret_Add_returns_number_add()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            BasicArithmetic.Generic_Add(leftExpr, rightExpr);

        var result = BasicArithmetic.Identify(composed);

        result.Should().NotBeNull();

        result.Value.declName.Should().Be("add");

        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Sub_returns_number_sub()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            BasicArithmetic.Generic_Sub(leftExpr, rightExpr);

        var result = BasicArithmetic.Identify(composed);

        result.Should().NotBeNull();

        result.Value.declName.Should().Be("sub");

        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Mul_returns_number_mul()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            BasicArithmetic.Generic_Mul(leftExpr, rightExpr);

        var result = BasicArithmetic.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("mul");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Int_div_returns_int_div()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(21)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(7)));

        var composed =
            BasicArithmetic.Int_div(leftExpr, rightExpr);

        var result = BasicArithmetic.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("idiv");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_non_arithmetic_returns_null()
    {
        // A ParseAndEval that doesn't match any arithmetic pattern
        var someExpr = Expression.LiteralInstance(PineValue.EmptyList);

        var parseAndEval = new Expression.ParseAndEval(
            encoded: Expression.LiteralInstance(PineValue.EmptyList),
            environment: Expression.ListInstance([someExpr, someExpr]));

        var result = BasicArithmetic.Identify(parseAndEval);

        result.Should().BeNull();
    }

    [Fact]
    public void TryInterpret_non_ParseAndEval_returns_null()
    {
        // A non-ParseAndEval expression
        var literalExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(42)));

        var result = BasicArithmetic.Identify(literalExpr);

        result.Should().BeNull();
    }

    [Fact]
    public void TryInterpret_wrong_environment_shape_returns_null()
    {
        // ParseAndEval with environment that's not a 2-element list
        var singleArgExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)));

        var parseAndEval = new Expression.ParseAndEval(
            encoded: Expression.LiteralInstance(BasicArithmetic.Add_FunctionValue()),
            environment: Expression.ListInstance([singleArgExpr])); // Only 1 argument

        var result = BasicArithmetic.Identify(parseAndEval);

        result.Should().BeNull();
    }

    #endregion

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
