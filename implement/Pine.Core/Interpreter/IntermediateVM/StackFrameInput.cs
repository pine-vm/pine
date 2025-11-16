using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM;

public record StackFrameInput
{
    public StaticFunctionInterface Parameters { get; }

    public IReadOnlyList<PineValueInProcess> Arguments { get; }

    private readonly PineValue[] _evaluatedArguments;

    public IReadOnlyList<PineValue> EvaluatedArguments => _evaluatedArguments;

    private readonly int _hashCode;

    private StackFrameInput(
        StaticFunctionInterface parameters,
        IReadOnlyList<PineValueInProcess> arguments)
    {
        Parameters = parameters;
        Arguments = arguments;

        var evaluatedArguments = new PineValue[arguments.Count];

        for (var i = 0; i < arguments.Count; i++)
        {
            evaluatedArguments[i] = arguments[i].Evaluate();
        }

        _evaluatedArguments = evaluatedArguments;

        // Pre-compute hash code.
        var hashCode = new HashCode();

        hashCode.Add(Parameters);

        for (var i = 0; i < evaluatedArguments.Length; i++)
        {
            hashCode.Add(evaluatedArguments[i]);
        }

        _hashCode = hashCode.ToHashCode();
    }

    public static StackFrameInput FromEnvironmentValue(
        PineValue environmentValue,
        StaticFunctionInterface parameters)
    {
        var arguments = new PineValueInProcess[parameters.ParamsPaths.Count];

        for (var i = 0; i < parameters.ParamsPaths.Count; i++)
        {
            var envPath = parameters.ParamsPaths[i];

            var valueAtPath =
                PineValueExtension.ValueFromPathOrEmptyList(
                    environmentValue,
                    envPath);

            arguments[i] = PineValueInProcess.Create(valueAtPath);
        }

        return new StackFrameInput(
            parameters,
            arguments);
    }

    public static StackFrameInput FromEnvironmentValue(
        PineValueInProcess environmentValue,
        StaticFunctionInterface parameters)
    {
        var arguments = new PineValueInProcess[parameters.ParamsPaths.Count];

        for (var i = 0; i < parameters.ParamsPaths.Count; i++)
        {
            var envPath = parameters.ParamsPaths[i];

            var valueAtPath =
                PineValueInProcess.ValueFromPathOrNull(
                    environmentValue,
                    envPath)
                ?? PineValueInProcess.EmptyList;

            arguments[i] = valueAtPath;
        }

        return new StackFrameInput(
            parameters,
            arguments);
    }

    public static StackFrameInput GenericFromEnvironmentValue(
        PineValue environmentValue)
    {
        return FromEnvironmentValue(
            environmentValue,
            StaticFunctionInterface.Generic);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        return _hashCode;
    }

    public virtual bool Equals(StackFrameInput? obj)
    {
        if (obj is not StackFrameInput other)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        if (_hashCode != other._hashCode)
            return false;

        if (!Parameters.Equals(other.Parameters))
            return false;

        if (_evaluatedArguments.Length != other._evaluatedArguments.Length)
            return false;

        for (var i = 0; i < _evaluatedArguments.Length; i++)
        {
            if (!_evaluatedArguments[i].Equals(other._evaluatedArguments[i]))
                return false;
        }

        return true;
    }

    override public string ToString()
    {
        return
            "InvocationInput(" +
            Parameters +
            ", Args[" +
            string.Join(
                ", ",
                _evaluatedArguments.Select(a => a.ToString())) +
            "])";
    }

    public PineValueClass ToValueClass()
    {
        IReadOnlyList<KeyValuePair<IReadOnlyList<int>, PineValue>> parsedItems =
            [..Parameters.ParamsPaths
            .Select((envPath, index) =>
                new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    envPath,
                    _evaluatedArguments[index]))
            ];

        return PineValueClass.Create(parsedItems);
    }

    public PineValue CreateMinimalValue()
    {
        var asClass = ToValueClass();

        return PineValueClassExtensions.CreateMinimalValue(asClass);
    }
}

