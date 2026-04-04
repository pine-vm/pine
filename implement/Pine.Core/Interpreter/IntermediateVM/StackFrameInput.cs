using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Represents the input to a stack frame, consisting of a parameter layout and the corresponding argument values.
/// Equality and hashing are based on the evaluated argument values and the parameter layout.
/// </summary>
public record StackFrameInput
{
    /// <summary>
    /// The parameter layout describing which paths in the environment are used as parameters.
    /// </summary>
    public StaticFunctionInterface Parameters { get; }

    /// <summary>
    /// The argument values supplied to this stack frame, before evaluation.
    /// </summary>
    public IReadOnlyList<PineValueInProcess> Arguments { get; }

    private readonly PineValue[] _evaluatedArguments;

    /// <summary>
    /// The argument values after evaluation, corresponding to <see cref="Arguments"/>.
    /// </summary>
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

    /// <summary>
    /// Creates a <see cref="StackFrameInput"/> by extracting argument values from the given environment value
    /// according to the paths defined in the parameter layout.
    /// </summary>
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

        return
            new StackFrameInput(
                parameters,
                arguments);
    }

    /// <summary>
    /// Creates a <see cref="StackFrameInput"/> by extracting argument values from the given
    /// <see cref="PineValueInProcess"/> environment according to the paths defined in the parameter layout.
    /// </summary>
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

        return
            new StackFrameInput(
                parameters,
                arguments);
    }

    /// <summary>
    /// Creates a <see cref="StackFrameInput"/> using the generic parameter layout,
    /// which treats the entire environment value as a single argument.
    /// </summary>
    public static StackFrameInput GenericFromEnvironmentValue(
        PineValue environmentValue)
    {
        return
            FromEnvironmentValue(
                environmentValue,
                StaticFunctionInterface.Generic);
    }

    /// <summary>
    /// Creates a <see cref="StackFrameInput"/> from arguments that are forwarded directly to a stack frame.
    /// </summary>
    /// <param name="parameters">The parameter layout describing the forwarded arguments.</param>
    /// <param name="arguments">The argument values to expose to the stack frame.</param>
    /// <exception cref="ArgumentException">Thrown when the number of arguments does not match the number of parameters.</exception>
    public static StackFrameInput FromArguments(
        StaticFunctionInterface parameters,
        IReadOnlyList<PineValueInProcess> arguments)
    {
        if (parameters.ParamsPaths.Count != arguments.Count)
        {
            throw new ArgumentException(
                "Arguments count does not match parameters count.",
                nameof(arguments));
        }

        return
            new StackFrameInput(
                parameters,
                arguments);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        return _hashCode;
    }

    /// <inheritdoc/>
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

    /// <inheritdoc/>
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

    /// <summary>
    /// Converts this input to a <see cref="PineValueClass"/> by pairing each parameter path
    /// with its corresponding evaluated argument value.
    /// </summary>
    public PineValueClass ToValueClass()
    {
        IReadOnlyList<KeyValuePair<IReadOnlyList<int>, PineValue>> parsedItems =
            [
            ..Parameters.ParamsPaths
            .Select(
                (envPath, index) =>
                new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    envPath,
                    _evaluatedArguments[index]))
            ];

        return PineValueClass.Create(parsedItems);
    }

    /// <summary>
    /// Creates a minimal <see cref="PineValue"/> representation of this input,
    /// containing only the data needed to reconstruct the argument values at their parameter paths.
    /// </summary>
    public PineValue CreateMinimalValue()
    {
        var asClass = ToValueClass();

        return PineValueClassExtensions.CreateMinimalValue(asClass);
    }
}
