using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.PineVM;

public record StackFrameInput
{
    public StaticFunctionInterface Parameters { get; }

    public IReadOnlyList<PineValue> Arguments { get; }

    private readonly int _hashCode;

    private StackFrameInput(
        StaticFunctionInterface parameters,
        IReadOnlyList<PineValue> arguments)
    {
        Parameters = parameters;
        Arguments = arguments;

        // Pre-compute hash code.
        var hashCode = new HashCode();

        hashCode.Add(Parameters);

        for (var i = 0; i < Arguments.Count; i++)
        {
            hashCode.Add(Arguments[i]);
        }

        _hashCode = hashCode.ToHashCode();
    }

    public static StackFrameInput FromEnvironmentValue(
        PineValue environmentValue,
        StaticFunctionInterface parameters)
    {
        var arguments = new PineValue[parameters.ParamsPaths.Count];

        for (var i = 0; i < parameters.ParamsPaths.Count; i++)
        {
            var envPath = parameters.ParamsPaths[i];

            var valueAtPath =
                PineValueExtension.ValueFromPathOrEmptyList(
                    environmentValue,
                    envPath);

            arguments[i] = valueAtPath;
        }

        return new StackFrameInput(
            parameters,
            arguments);
    }

    public static StackFrameInput FromEnvironmentValue(
        PineValueInProcess environmentValue,
        StaticFunctionInterface parameters)
    {
        var arguments = new PineValue[parameters.ParamsPaths.Count];

        for (var i = 0; i < parameters.ParamsPaths.Count; i++)
        {
            var envPath = parameters.ParamsPaths[i];

            var valueAtPath =
                PineValueInProcess.ValueFromPathOrNull(
                    environmentValue,
                    envPath)
                ?? PineValue.ListValue.Empty;

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

    public static StackFrameInput GenericFromEnvironmentValue(
        PineValueInProcess environmentValue)
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

        if (Arguments.Count != other.Arguments.Count)
            return false;

        for (var i = 0; i < Arguments.Count; i++)
        {
            if (!Arguments[i].Equals(other.Arguments[i]))
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
                Arguments.Select(a => a.ToString())) +
            "])";
    }

    public PineValueClass ToValueClass()
    {
        IReadOnlyList<KeyValuePair<IReadOnlyList<int>, PineValue>> parsedItems =
            [..Parameters.ParamsPaths
            .Select((envPath, index) =>
                new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    envPath,
                    Arguments[index]))
            ];

        return PineValueClass.Create(parsedItems);
    }

    public PineValue CreateMinimalValue()
    {
        var asClass = ToValueClass();

        return PineValueClassExtensions.CreateMinimalValue(asClass);
    }
}

