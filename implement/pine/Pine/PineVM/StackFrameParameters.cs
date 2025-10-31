using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.PineVM;

public record StackFrameInput
{
    public StackFrameParameters Parameters { get; }

    public IReadOnlyList<PineValue> Arguments { get; }

    private readonly int _hashCode;

    public StackFrameInput(
        StackFrameParameters parameters,
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
        StackFrameParameters parameters)
    {
        var arguments = new PineValue[parameters.EnvPaths.Count];

        for (var i = 0; i < parameters.EnvPaths.Count; i++)
        {
            var envPath = parameters.EnvPaths[i];

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
        StackFrameParameters parameters)
    {
        var arguments = new PineValue[parameters.EnvPaths.Count];

        for (var i = 0; i < parameters.EnvPaths.Count; i++)
        {
            var envPath = parameters.EnvPaths[i];

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
            StackFrameParameters.Generic);
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
            [..Parameters.EnvPaths
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


public record StackFrameParameters
{
    public IReadOnlyList<IReadOnlyList<int>> EnvPaths { get; }

    private readonly int _hashCode;

    /// <summary>
    /// Only a single parameter containing the whole environment.
    /// </summary>
    public static readonly StackFrameParameters Generic = new([[]]);

    public StackFrameParameters(
        IReadOnlyList<IReadOnlyList<int>> envPaths)
    {
        EnvPaths = envPaths;

        // Pre-compute hash code.
        var hashCode = new HashCode();

        foreach (var path in EnvPaths)
        {
            foreach (var index in path)
            {
                hashCode.Add(index);
            }
            hashCode.Add(-1);
        }

        _hashCode = hashCode.ToHashCode();
    }

    /// <inheritdoc/>
    public virtual bool Equals(StackFrameParameters? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is not { } notNull)
            return false;

        if (_hashCode != notNull._hashCode)
            return false;

        if (EnvPaths.Count != notNull.EnvPaths.Count)
            return false;

        for (var i = 0; i < EnvPaths.Count; i++)
        {
            var pathA = EnvPaths[i];
            var pathB = notNull.EnvPaths[i];

            if (pathA.Count != pathB.Count)
                return false;

            for (var j = 0; j < pathA.Count; j++)
            {
                if (pathA[j] != pathB[j])
                    return false;
            }
        }

        return true;
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        return _hashCode;
    }

    override public string ToString()
    {
        return
            "Params[" +
            string.Join(
                ", ",
                EnvPaths.Select(
                    p => "[" + string.Join(", ", p) + "]")) +
            "]";
    }
}
