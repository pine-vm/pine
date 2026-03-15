using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.PineVM;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.CodeAnalysis;

public class StaticExpressionInterpreterTests
{
    private static StaticExpression<StaticFunctionIdentifier> Literal(PineValue value) =>
        StaticExpression<StaticFunctionIdentifier>.LiteralInstance(value);

    private static StaticExpression<StaticFunctionIdentifier> Literal(long intValue) =>
        Literal(IntegerEncoding.EncodeSignedInteger(intValue));

    private static StaticExpression<StaticFunctionIdentifier> Env =>
        StaticExpression<StaticFunctionIdentifier>.EnvironmentInstance;

    private static StaticExpression<StaticFunctionIdentifier> ListExpr(
        params StaticExpression<StaticFunctionIdentifier>[] items) =>
        StaticExpression<StaticFunctionIdentifier>.ListInstance(items);

    private static StaticExpression<StaticFunctionIdentifier> KernelApp(
        string function,
        StaticExpression<StaticFunctionIdentifier> input) =>
        StaticExpression<StaticFunctionIdentifier>.KernelApplicationInstance(function, input);

    private static StaticExpression<StaticFunctionIdentifier> Conditional(
        StaticExpression<StaticFunctionIdentifier> condition,
        StaticExpression<StaticFunctionIdentifier> falseBranch,
        StaticExpression<StaticFunctionIdentifier> trueBranch) =>
        StaticExpression<StaticFunctionIdentifier>.ConditionalInstance(condition, falseBranch, trueBranch);

    private static StaticExpression<StaticFunctionIdentifier> FuncApp(
        StaticFunctionIdentifier functionName,
        StaticExpression<StaticFunctionIdentifier> arguments) =>
        StaticExpression<StaticFunctionIdentifier>.FunctionApplicationInstance(functionName, arguments);

    /// <summary>
    /// Build a path expression like env[i][j] using skip/head kernel applications on Environment.
    /// </summary>
    private static StaticExpression<StaticFunctionIdentifier> EnvPath(params int[] path) =>
        StaticExpressionExtension.BuildPathToExpression(
            path,
            StaticExpression<StaticFunctionIdentifier>.EnvironmentInstance);

    private static StaticFunctionIdentifier MakeFuncId(string name) =>
        new(
            EncodedExpr: StringEncoding.ValueFromString(name),
            EnvClass: PineValueClass.Empty);

    [Fact]
    public void Evaluate_Literal_Integer()
    {
        var interpreter = new StaticExpressionInterpreter();

        var result = interpreter.Evaluate(Literal(42), PineValue.EmptyList);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Evaluate_Literal_EmptyList()
    {
        var interpreter = new StaticExpressionInterpreter();

        var result = interpreter.Evaluate(Literal(PineValue.EmptyList), PineValue.EmptyList);

        result.Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void Evaluate_Environment()
    {
        var interpreter = new StaticExpressionInterpreter();

        var envValue =
            PineValue.List([IntegerEncoding.EncodeSignedInteger(10), IntegerEncoding.EncodeSignedInteger(20)]);

        var result = interpreter.Evaluate(Env, envValue);

        result.Should().Be(envValue);
    }

    [Fact]
    public void Evaluate_List_Of_Literals()
    {
        var interpreter = new StaticExpressionInterpreter();

        var expr = ListExpr(Literal(1), Literal(2), Literal(3));

        var result = interpreter.Evaluate(expr, PineValue.EmptyList);

        result.Should().Be(
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(1),
                IntegerEncoding.EncodeSignedInteger(2),
                IntegerEncoding.EncodeSignedInteger(3),
                ]));
    }

    [Fact]
    public void Evaluate_KernelApplication_Int_Add()
    {
        var interpreter = new StaticExpressionInterpreter();

        var expr =
            KernelApp(
                nameof(KernelFunction.int_add),
                ListExpr(Literal(10), Literal(32)));

        var result = interpreter.Evaluate(expr, PineValue.EmptyList);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Evaluate_KernelApplication_Equal()
    {
        var interpreter = new StaticExpressionInterpreter();

        var exprEqual =
            KernelApp(
                nameof(KernelFunction.equal),
                ListExpr(Literal(5), Literal(5)));

        interpreter.Evaluate(exprEqual, PineValue.EmptyList)
            .Should().Be(PineKernelValues.TrueValue);

        var exprNotEqual =
            KernelApp(
                nameof(KernelFunction.equal),
                ListExpr(Literal(5), Literal(6)));

        interpreter.Evaluate(exprNotEqual, PineValue.EmptyList)
            .Should().Be(PineKernelValues.FalseValue);
    }

    [Fact]
    public void Evaluate_Conditional_True_Branch()
    {
        var interpreter = new StaticExpressionInterpreter();

        var expr =
            Conditional(
                condition: Literal(PineKernelValues.TrueValue),
                trueBranch: Literal(100),
                falseBranch: Literal(200));

        var result = interpreter.Evaluate(expr, PineValue.EmptyList);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(100));
    }

    [Fact]
    public void Evaluate_Conditional_False_Branch()
    {
        var interpreter = new StaticExpressionInterpreter();

        var expr =
            Conditional(
                condition: Literal(PineKernelValues.FalseValue),
                trueBranch: Literal(100),
                falseBranch: Literal(200));

        var result = interpreter.Evaluate(expr, PineValue.EmptyList);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(200));
    }

    [Fact]
    public void Evaluate_Environment_Path_Access()
    {
        var interpreter = new StaticExpressionInterpreter();

        // env = [10, 20, 30]
        var envValue =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(10),
                IntegerEncoding.EncodeSignedInteger(20),
                IntegerEncoding.EncodeSignedInteger(30),
                ]);

        // env[1] should be 20
        var expr = EnvPath(1);

        var result = interpreter.Evaluate(expr, envValue);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(20));
    }

    [Fact]
    public void Evaluate_Simple_Function_Call()
    {
        // Define a function "addOne" that adds 1 to its argument
        // Function env is a list where the argument is passed directly as the env
        var addOneFuncId = MakeFuncId("addOne");

        // addOne body: int_add [env, 1]
        // When called, Arguments is evaluated to produce the function's environment value.
        var addOneBody =
            KernelApp(
                nameof(KernelFunction.int_add),
                ListExpr(Env, Literal(1)));

        var functions =
            new Dictionary<StaticFunctionIdentifier, StaticExpression<StaticFunctionIdentifier>>
            {
                [addOneFuncId] = addOneBody
            };

        var interpreter = new StaticExpressionInterpreter(functions);

        // Call addOne(41): FunctionApplication("addOne", Literal(41))
        var callExpr = FuncApp(addOneFuncId, Literal(41));

        var result = interpreter.Evaluate(callExpr, PineValue.EmptyList);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Evaluate_Chain_Of_Function_Calls()
    {
        // Define two functions: "double" and "addTen"
        // double(x) = int_mul [x, 2]
        // addTen(x) = int_add [x, 10]
        var doubleFuncId = MakeFuncId("double");
        var addTenFuncId = MakeFuncId("addTen");

        var doubleBody =
            KernelApp(
                nameof(KernelFunction.int_mul),
                ListExpr(Env, Literal(2)));

        var addTenBody =
            KernelApp(
                nameof(KernelFunction.int_add),
                ListExpr(Env, Literal(10)));

        var functions =
            new Dictionary<StaticFunctionIdentifier, StaticExpression<StaticFunctionIdentifier>>
            {
                [doubleFuncId] = doubleBody,
                [addTenFuncId] = addTenBody,
            };

        var interpreter = new StaticExpressionInterpreter(functions);

        // addTen(double(5)) should be (5*2)+10 = 20
        var callExpr = FuncApp(addTenFuncId, FuncApp(doubleFuncId, Literal(5)));

        var result = interpreter.Evaluate(callExpr, PineValue.EmptyList);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(20));
    }

    [Fact]
    public void Evaluate_Recursive_Factorial()
    {
        // factorial(n) = if int_is_sorted_asc [n, 1] then 1 else int_mul [factorial(int_add [n, -1]), n]
        // The function receives env = n (a single integer value)
        var factorialId = MakeFuncId("factorial");

        var factorialBody =
            Conditional(
                condition: KernelApp(
                    nameof(KernelFunction.int_is_sorted_asc),
                    ListExpr(Env, Literal(1))),
                trueBranch: Literal(1),
                falseBranch: KernelApp(
                    nameof(KernelFunction.int_mul),
                    ListExpr(
                        FuncApp(
                            factorialId,
                            KernelApp(nameof(KernelFunction.int_add), ListExpr(Env, Literal(-1)))),
                        Env)));

        var functions =
            new Dictionary<StaticFunctionIdentifier, StaticExpression<StaticFunctionIdentifier>>
            {
                [factorialId] = factorialBody
            };

        var interpreter = new StaticExpressionInterpreter(functions);

        // factorial(0) = 1
        interpreter.Evaluate(FuncApp(factorialId, Literal(0)), PineValue.EmptyList)
            .Should().Be(IntegerEncoding.EncodeSignedInteger(1));

        // factorial(1) = 1
        interpreter.Evaluate(FuncApp(factorialId, Literal(1)), PineValue.EmptyList)
            .Should().Be(IntegerEncoding.EncodeSignedInteger(1));

        // factorial(5) = 120
        interpreter.Evaluate(FuncApp(factorialId, Literal(5)), PineValue.EmptyList)
            .Should().Be(IntegerEncoding.EncodeSignedInteger(120));

        // factorial(7) = 5040
        interpreter.Evaluate(FuncApp(factorialId, Literal(7)), PineValue.EmptyList)
            .Should().Be(IntegerEncoding.EncodeSignedInteger(5040));
    }

    [Fact]
    public void Evaluate_Recursive_Fibonacci()
    {
        // fibonacci(n) = if int_is_sorted_asc [n, 1] then n else int_add [fibonacci(int_add [n, -2]), fibonacci(int_add [n, -1])]
        // The function receives env = n (a single integer value)
        var fibId = MakeFuncId("fibonacci");

        var fibBody =
            Conditional(
                condition: KernelApp(
                    nameof(KernelFunction.int_is_sorted_asc),
                    ListExpr(Env, Literal(1))),
                trueBranch: Env,
                falseBranch: KernelApp(
                    nameof(KernelFunction.int_add),
                    ListExpr(
                        FuncApp(
                            fibId,
                            KernelApp(nameof(KernelFunction.int_add), ListExpr(Env, Literal(-2)))),
                        FuncApp(
                            fibId,
                            KernelApp(nameof(KernelFunction.int_add), ListExpr(Env, Literal(-1)))))));

        var functions =
            new Dictionary<StaticFunctionIdentifier, StaticExpression<StaticFunctionIdentifier>>
            {
                [fibId] = fibBody
            };

        var interpreter = new StaticExpressionInterpreter(functions);

        var expectedValues =
            new (int input, int expected)[]
            {
                (0, 0),
                (1, 1),
                (2, 1),
                (3, 2),
                (4, 3),
                (5, 5),
                (6, 8),
                (7, 13),
                (8, 21),
                (9, 34),
                (10, 55)
            };

        foreach (var (input, expected) in expectedValues)
        {
            var result = interpreter.Evaluate(FuncApp(fibId, Literal(input)), PineValue.EmptyList);

            result.Should().Be(
                IntegerEncoding.EncodeSignedInteger(expected),
                $"fibonacci({input}) should be {expected}");
        }
    }

    [Fact]
    public void Evaluate_Mutual_Recursion_IsEven_IsOdd()
    {
        // isEven(n) = if equal [n, 0] then True else isOdd(int_add [n, -1])
        // isOdd(n) = if equal [n, 0] then False else isEven(int_add [n, -1])
        var isEvenId = MakeFuncId("isEven");
        var isOddId = MakeFuncId("isOdd");

        var isEvenBody =
            Conditional(
                condition: KernelApp(nameof(KernelFunction.equal), ListExpr(Env, Literal(0))),
                trueBranch: Literal(PineKernelValues.TrueValue),
                falseBranch: FuncApp(
                    isOddId,
                    KernelApp(nameof(KernelFunction.int_add), ListExpr(Env, Literal(-1)))));

        var isOddBody =
            Conditional(
                condition: KernelApp(nameof(KernelFunction.equal), ListExpr(Env, Literal(0))),
                trueBranch: Literal(PineKernelValues.FalseValue),
                falseBranch: FuncApp(
                    isEvenId,
                    KernelApp(nameof(KernelFunction.int_add), ListExpr(Env, Literal(-1)))));

        var functions =
            new Dictionary<StaticFunctionIdentifier, StaticExpression<StaticFunctionIdentifier>>
            {
                [isEvenId] = isEvenBody,
                [isOddId] = isOddBody,
            };

        var interpreter = new StaticExpressionInterpreter(functions);

        interpreter.Evaluate(FuncApp(isEvenId, Literal(0)), PineValue.EmptyList)
            .Should().Be(PineKernelValues.TrueValue);

        interpreter.Evaluate(FuncApp(isEvenId, Literal(1)), PineValue.EmptyList)
            .Should().Be(PineKernelValues.FalseValue);

        interpreter.Evaluate(FuncApp(isEvenId, Literal(4)), PineValue.EmptyList)
            .Should().Be(PineKernelValues.TrueValue);

        interpreter.Evaluate(FuncApp(isOddId, Literal(3)), PineValue.EmptyList)
            .Should().Be(PineKernelValues.TrueValue);

        interpreter.Evaluate(FuncApp(isOddId, Literal(6)), PineValue.EmptyList)
            .Should().Be(PineKernelValues.FalseValue);
    }

    [Fact]
    public void Evaluate_Function_With_List_Environment()
    {
        // A function that takes env = [a, b] and returns int_add [a, b]
        var sumId = MakeFuncId("sum");

        var sumBody =
            KernelApp(
                nameof(KernelFunction.int_add),
                ListExpr(EnvPath(0), EnvPath(1)));

        var functions =
            new Dictionary<StaticFunctionIdentifier, StaticExpression<StaticFunctionIdentifier>>
            {
                [sumId] = sumBody,
            };

        var interpreter = new StaticExpressionInterpreter(functions);

        // Call sum with args [10, 32]
        var callExpr = FuncApp(sumId, ListExpr(Literal(10), Literal(32)));

        var result = interpreter.Evaluate(callExpr, PineValue.EmptyList);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Evaluate_Nested_Conditional()
    {
        var interpreter = new StaticExpressionInterpreter();

        // if (equal [5, 5]) then (if (equal [3, 4]) then 100 else 200) else 300
        var expr =
            Conditional(
                condition: KernelApp(nameof(KernelFunction.equal), ListExpr(Literal(5), Literal(5))),
                trueBranch: Conditional(
                    condition: KernelApp(nameof(KernelFunction.equal), ListExpr(Literal(3), Literal(4))),
                    trueBranch: Literal(100),
                    falseBranch: Literal(200)),
                falseBranch: Literal(300));

        var result = interpreter.Evaluate(expr, PineValue.EmptyList);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(200));
    }
}
