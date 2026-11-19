using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using System;
using System.Collections.Generic;
using System.Threading;

namespace Pine.Core.CodeAnalysis;

/// <summary>Inspects and applies compiled values without source code or an Elm compiler.</summary>
public static class PineValueApplication
{
    /// <summary>
    /// Structurally known remaining arity. Other Pine expressions take one environment;
    /// ordinary data has arity zero. Inspection never evaluates a value.
    /// </summary>
    public sealed record Inspection(int Arity, bool IsExpression);

    private sealed record Callable(Expression? Expression, FunctionRecord? Function)
    {
        public int Arity =>
            Function is { } function
            ?
            function.ParameterCount - function.ArgumentsAlreadyCollected.Length
            :
            Expression is null ? 0 : 1;
    }

    /// <summary>Recognizes compiled function wrappers, legacy function records, expressions, and data.</summary>
    public static Result<string, Inspection> Inspect(PineValue value, PineVMParseCache? parseCache = null) =>
        Parse(value, parseCache ?? new()).Map(
            callable => new Inspection(callable.Arity, callable.Expression is not null));

    /// <summary>
    /// Applies arguments in order, supporting partial application and functions returning functions.
    /// A zero-parameter compiled declaration is evaluated when selected; data with no arguments is unchanged.
    /// </summary>
    public static Result<string, PineValue> Apply(
        IPineVM vm,
        PineValue value,
        IReadOnlyList<PineValue> arguments,
        CancellationToken cancellationToken = default,
        PineVMParseCache? parseCache = null)
    {
        parseCache ??= new();
        var current = value;
        var offset = 0;
        var nullaryValues = new HashSet<PineValue>();

        while (true)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var parsed = Parse(current, parseCache);

            if (parsed.IsErrOrNull() is { } error)
                return error;

            var callable = parsed.Extract(error => throw new InvalidOperationException(error));

            if (callable.Function is { } function)
            {
                if (offset == arguments.Count && callable.Arity > 0)
                    return current;

                if (callable.Arity == 0 && !nullaryValues.Add(current))
                    return "A cycle of zero-parameter declarations cannot consume the remaining arguments.";

                if (arguments.Count - offset >= callable.Arity)
                {
                    var supplied = new PineValue[callable.Arity];

                    for (var index = 0; index < supplied.Length; index++)
                        supplied[index] = arguments[offset + index];

                    var applied = ElmInteractiveEnvironment.ApplyFunction(vm, function, supplied, cancellationToken);

                    if (applied.IsErrOrNull() is { } applicationError)
                        return applicationError;

                    current = applied.Extract(error => throw new InvalidOperationException(error));
                    offset += supplied.Length;

                    if (supplied.Length > 0)
                        nullaryValues.Clear();

                    if (offset == arguments.Count)
                        return current;

                    continue;
                }

                if (callable.Expression is null)
                {
                    return
                        FunctionRecord.EncodeFunctionRecordInValueTagged(
                            function with
                            {
                                ArgumentsAlreadyCollected =
                                new ReadOnlyMemory<PineValue>(
                                [
                                    .. function.ArgumentsAlreadyCollected.Span,
                                    .. ArgumentsSlice(arguments, offset)
                                ])
                            });
                }
            }

            if (offset == arguments.Count)
                return current;

            if (callable.Expression is not { } expression)
                return $"Cannot apply argument {offset + 1}: the value is not a Pine expression or function.";

            var evaluated =
                vm is ICancellablePineVM cancellable
                ?
                cancellable.EvaluateExpression(expression, arguments[offset], cancellationToken)
                :
                vm.EvaluateExpression(expression, arguments[offset]);

            cancellationToken.ThrowIfCancellationRequested();

            if (evaluated.IsErrOrNull() is { } evaluationError)
                return evaluationError;

            current = evaluated.Extract(error => throw new InvalidOperationException(error));
            offset++;
            nullaryValues.Clear();

            if (offset == arguments.Count)
                return current;
        }
    }

    private static IEnumerable<PineValue> ArgumentsSlice(IReadOnlyList<PineValue> arguments, int offset)
    {
        for (var index = offset; index < arguments.Count; index++)
            yield return arguments[index];
    }

    private static Result<string, Callable> Parse(PineValue value, PineVMParseCache parseCache)
    {
        var expression = parseCache.ParseExpression(value).IsOkOrNull();
        FunctionRecord? function = null;
        var tagged = ElmInteractiveEnvironment.ParseTagged(value).IsOkOrNullable() is { name: "Function" };

        if (tagged || expression is Expression.Eval or Expression.List)
        {
            var parsed = FunctionRecord.ParseFunctionRecordTagged(value, parseCache);

            if (tagged && parsed.IsErrOrNull() is { } error)
                return "Invalid function record: " + error;

            function = parsed.IsOkOrNull();
        }

        if (function is not null &&
            (function.ParameterCount < 0 || function.ArgumentsAlreadyCollected.Length > function.ParameterCount))
        {
            return "Invalid function record parameter or collected-argument count.";
        }

        return new Callable(expression, function);
    }
}
