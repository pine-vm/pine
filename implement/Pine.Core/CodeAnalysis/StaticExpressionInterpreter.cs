using Pine.Core.PineVM;
using System;
using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// A minimal interpreter for <see cref="StaticExpression{StaticFunctionIdentifier}"/> expressions.
/// Analog to <see cref="Interpreter.DirectInterpreter"/>, but operating on the static
/// expression model rather than the general Pine <see cref="Expression"/> model.
/// </summary>
/// <remarks>
/// Creates a new interpreter with the given function definitions.
/// </remarks>
/// <param name="functions">
/// A dictionary mapping function identifiers to their body expressions.
/// When a <see cref="StaticExpression{T}.FunctionApplication"/> is evaluated,
/// the interpreter looks up the function body here.
/// </param>
public class StaticExpressionInterpreter(
    IReadOnlyDictionary<StaticFunctionIdentifier, StaticExpression<StaticFunctionIdentifier>> functions)
{
    private readonly IReadOnlyDictionary<StaticFunctionIdentifier, StaticExpression<StaticFunctionIdentifier>> _functions =
        functions;

    /// <summary>
    /// Creates a new interpreter with no pre-defined functions.
    /// Only expressions that do not reference user-defined functions can be evaluated.
    /// </summary>
    public StaticExpressionInterpreter()
        : this(new Dictionary<StaticFunctionIdentifier, StaticExpression<StaticFunctionIdentifier>>())
    {
    }

    /// <summary>
    /// Evaluate a static expression in the given environment.
    /// </summary>
    public PineValue Evaluate(
        StaticExpression<StaticFunctionIdentifier> expression,
        PineValue environment)
    {
        if (expression is StaticExpression<StaticFunctionIdentifier>.Literal literal)
        {
            return literal.Value;
        }

        if (expression is StaticExpression<StaticFunctionIdentifier>.Environment)
        {
            return environment;
        }

        if (expression is StaticExpression<StaticFunctionIdentifier>.List list)
        {
            var items = new PineValue[list.Items.Count];

            for (var i = 0; i < list.Items.Count; i++)
            {
                items[i] = Evaluate(list.Items[i], environment);
            }

            return PineValue.List(items);
        }

        if (expression is StaticExpression<StaticFunctionIdentifier>.KernelApplication kernelApp)
        {
            var inputValue = Evaluate(kernelApp.Input, environment);

            return
                KernelFunction.ApplyKernelFunctionGeneric(
                    function: kernelApp.Function,
                    inputValue: inputValue);
        }

        if (expression is StaticExpression<StaticFunctionIdentifier>.Conditional conditional)
        {
            var conditionValue = Evaluate(conditional.Condition, environment);

            if (conditionValue == PineKernelValues.TrueValue)
            {
                return Evaluate(conditional.TrueBranch, environment);
            }

            return Evaluate(conditional.FalseBranch, environment);
        }

        if (expression is StaticExpression<StaticFunctionIdentifier>.FunctionApplication funcApp)
        {
            return EvaluateFunctionApplication(funcApp, environment);
        }

        throw new NotImplementedException(
            "Unexpected static expression type: " + expression.GetType().FullName);
    }

    private PineValue EvaluateFunctionApplication(
        StaticExpression<StaticFunctionIdentifier>.FunctionApplication funcApp,
        PineValue environment)
    {
        if (!_functions.TryGetValue(funcApp.FunctionName, out var functionBody))
        {
            throw new InvalidOperationException(
                "Function not found: " + funcApp.FunctionName);
        }

        var calleeEnv = Evaluate(funcApp.Arguments, environment);
        return Evaluate(functionBody, calleeEnv);
    }
}
