using Pine.Core.Elm;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public static class CoreLibraryTestHelper
{
    private static readonly Core.Interpreter.IntermediateVM.PineVM s_defaultVM =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    public static (ElmValue value, EvaluationReport profile) ApplyDirectBinary(
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

    public static EvaluationReport ApplyDirectBinary(
        System.Func<Expression, Expression, Expression> function,
        PineValue left,
        PineValue right)
    {
        var expression =
            function(Expression.LiteralInstance(left), Expression.LiteralInstance(right));

        return ElmCompilerTestHelper.EvaluateWithProfiling(expression, PineValue.EmptyBlob).evalReport;
    }

    public static ElmValue ApplyUnary(
        PineValue functionValue,
        ElmValue argument) =>
        ApplyGeneric(functionValue, [argument]);

    public static ElmValue ApplyUnary(
        PineValue functionValue,
        ElmValue argument,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        ApplyGeneric(functionValue, [argument], vm);

    public static ElmValue ApplyBinary(
        PineValue functionValue,
        ElmValue arg1,
        ElmValue arg2) =>
        ApplyGeneric(functionValue, [arg1, arg2]);

    public static ElmValue ApplyBinary(
        PineValue functionValue,
        ElmValue arg1,
        ElmValue arg2,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        ApplyGeneric(functionValue, [arg1, arg2], vm);

    public static ElmValue ApplyTernary(
        PineValue functionValue,
        ElmValue arg1,
        ElmValue arg2,
        ElmValue arg3) =>
        ApplyGeneric(functionValue, [arg1, arg2, arg3]);

    public static ElmValue ApplyTernary(
        PineValue functionValue,
        ElmValue arg1,
        ElmValue arg2,
        ElmValue arg3,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        ApplyGeneric(functionValue, [arg1, arg2, arg3], vm);

    public static ElmValue ApplyGeneric(
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
            ApplyGenericPine(functionValue, pineArguments);

        return
            ElmValueEncoding.PineValueAsElmValue(resultPineValue, null, null)
            .Extract(err => throw new System.Exception("Failed decode as Elm value: " + err));
    }

    public static ElmValue ApplyGeneric(
        PineValue functionValue,
        ElmValue[] arguments,
        Core.Interpreter.IntermediateVM.PineVM vm)
    {
        var pineArguments =
            new PineValue[arguments.Length];

        for (var i = 0; i < arguments.Length; i++)
        {
            pineArguments[i] =
                ElmValueEncoding.ElmValueAsPineValue(arguments[i]);
        }

        var resultPineValue =
            ApplyGenericPine(functionValue, pineArguments, vm);

        return
            ElmValueEncoding.PineValueAsElmValue(resultPineValue, null, null)
            .Extract(err => throw new System.Exception("Failed decode as Elm value: " + err));
    }

    /// <summary>
    /// <see href="https://github.com/pine-vm/pine/blob/2c3b26abb48769712eff1ce8834eb6579cb11add/implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md#function-values-and-generic-function-application"/>
    /// </summary>
    public static PineValue ApplyGenericPine(
        PineValue functionValue,
        IReadOnlyList<PineValue> arguments)
    {
        return ApplyGenericPine(functionValue, arguments, s_defaultVM);
    }

    /// <summary>
    /// <see href="https://github.com/pine-vm/pine/blob/2c3b26abb48769712eff1ce8834eb6579cb11add/implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md#function-values-and-generic-function-application"/>
    /// </summary>
    public static PineValue ApplyGenericPine(
        PineValue functionValue,
        IReadOnlyList<PineValue> arguments,
        Core.Interpreter.IntermediateVM.PineVM vm)
    {
        var currentValue = functionValue;

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

    public static ElmValue ApplyWithPineArgs(
        PineValue functionValue,
        params PineValue[] pineArgs)
    {
        var resultPineValue = ApplyGenericPine(functionValue, pineArgs);

        return
            ElmValueEncoding.PineValueAsElmValue(resultPineValue, null, null)
            .Extract(err => throw new System.Exception("Failed decode as Elm value: " + err));
    }

    public static ElmValue ApplyWithPineArgs(
        Core.Interpreter.IntermediateVM.PineVM vm,
        PineValue functionValue,
        params PineValue[] pineArgs)
    {
        var resultPineValue = ApplyGenericPine(functionValue, pineArgs, vm);

        return
            ElmValueEncoding.PineValueAsElmValue(resultPineValue, null, null)
            .Extract(err => throw new System.Exception("Failed decode as Elm value: " + err));
    }

    public static string FormatCounts(EvaluationReport report) =>
        string.Join(
            "\n",
            "InstructionCount: " + report.InstructionCount,
            "InvocationCount: " + report.InvocationCount,
            "LoopIterationCount: " + report.LoopIterationCount);

    public static (ElmValue value, EvaluationReport report) ApplyAndProfileUnary(
        PineValue functionValue,
        ElmValue argument,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        ApplyGenericWithProfiling(functionValue, [argument], vm);

    public static (ElmValue value, EvaluationReport report) ApplyAndProfileBinary(
        PineValue functionValue,
        ElmValue arg1,
        ElmValue arg2,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        ApplyGenericWithProfiling(functionValue, [arg1, arg2], vm);

    public static (ElmValue value, EvaluationReport report) ApplyGenericWithProfiling(
        PineValue functionValue,
        ElmValue[] arguments,
        Core.Interpreter.IntermediateVM.PineVM vm)
    {
        var pineArguments =
            new PineValue[arguments.Length];

        for (var i = 0; i < arguments.Length; i++)
        {
            pineArguments[i] =
                ElmValueEncoding.ElmValueAsPineValue(arguments[i]);
        }

        var (resultPineValue, report) =
            ApplyGenericPineWithProfiling(functionValue, pineArguments, vm);

        var elmValue =
            ElmValueEncoding.PineValueAsElmValue(resultPineValue, null, null)
            .Extract(err => throw new System.Exception("Failed decode as Elm value: " + err));

        return (elmValue, report);
    }

    public static (PineValue result, EvaluationReport report) ApplyGenericPineWithProfiling(
        PineValue functionValue,
        IReadOnlyList<PineValue> arguments,
        Core.Interpreter.IntermediateVM.PineVM vm)
    {
        if (arguments.Count is 0)
            throw new System.ArgumentException("Expected at least one argument", nameof(arguments));

        var currentValue = functionValue;

        long totalInstructions = 0;
        long totalInvocations = 0;
        long totalLoopIterations = 0;

        EvaluationReport lastReport = null!;

        for (var i = 0; i < arguments.Count; i++)
        {
            var asIndependent =
                new Expression.ParseAndEval(
                    encoded: Expression.LiteralInstance(currentValue),
                    environment: Expression.LiteralInstance(arguments[i]));

            var report =
                vm.EvaluateExpressionOnCustomStack(
                    asIndependent,
                    PineValue.EmptyBlob,
                    config: new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(ParseAndEvalCountLimit: null))
                .Extract(err => throw new System.Exception("Failed eval: " + err));

            currentValue = report.ReturnValue.Evaluate();
            totalInstructions += report.InstructionCount;
            totalInvocations += report.InvocationCount;
            totalLoopIterations += report.LoopIterationCount;
            lastReport = report;
        }

        var aggregated = lastReport with
        {
            InstructionCount = totalInstructions,
            InvocationCount = totalInvocations,
            LoopIterationCount = totalLoopIterations
        };

        return (currentValue, aggregated);
    }
}
