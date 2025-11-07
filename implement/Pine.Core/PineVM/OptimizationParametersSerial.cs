using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.PineVM;

/// <summary>
/// Optimization parameters for use in a VM, declarared in a serializable format.
/// Parameters for optimization are keyed by expression, and typically derived from training runs on various workloads.
/// </summary>
public record OptimizationParametersSerial(
    IReadOnlyList<OptimizationParametersSerial.ExpressionEntry> Expressions)
{
    /// <summary>
    /// Instance with no configuration for any expression.
    /// </summary>
    public static readonly OptimizationParametersSerial Empty =
        new([]);

    /// <summary>
    /// Retrieves the configuration associated with the specified expression hash, if available.
    /// </summary>
    /// <param name="hashBytes">A read-only memory buffer containing the hash of the expression for which to retrieve the configuration.</param>
    /// <returns>An <see cref="ExpressionConfig"/> instance representing the configuration for the given expression hash, or <see
    /// langword="null"/> if no matching configuration is found.</returns>
    public ExpressionConfig? ConfigForExpression(
        ReadOnlyMemory<byte> hashBytes)
    {
        return ConfigForExpression(Expressions, hashBytes);
    }

    /// <summary>
    /// Retrieves the configuration associated with the specified expression hash from a collection of expression
    /// entries.
    /// </summary>
    /// <remarks>The method compares the provided hash against each entry's serial hash using a case-sensitive
    /// ordinal comparison. If multiple entries have the same hash, the first match is returned.</remarks>
    /// <param name="entries">A read-only list of expression entries to search for a matching hash.</param>
    /// <param name="hashBytes">The hash value of the expression, represented as a read-only memory block of bytes.</param>
    /// <returns>The configuration corresponding to the expression hash if a match is found; otherwise, null.</returns>
    public static ExpressionConfig? ConfigForExpression(
        IReadOnlyList<ExpressionEntry> entries,
        ReadOnlyMemory<byte> hashBytes)
    {
        var hashBase16 = Convert.ToHexStringLower(hashBytes.Span);

        for (var i = 0; i < entries.Count; i++)
        {
            if (hashBase16.Equals(entries[i].HashSerial, StringComparison.Ordinal))
            {
                return entries[i].Config;
            }
        }

        return null;
    }

    /// <summary>
    /// Configuration detailing how to optimize expression execution, including caching and parallelization strategies.
    /// </summary>
    /// <param name="PersistentCachePredicate">Predicate indicating when persistent caching should be applied.</param>
    /// <param name="ParallelThreadPredicate">Predicate indicating when to execute the expression on parallel threads.</param>
    public record ExpressionConfig(
        InputPredicate? PersistentCachePredicate,
        InputPredicate? ParallelThreadPredicate);

    /// <summary>
    /// Predicate describing input conditions for enabling specific optimization behaviors.
    /// </summary>
    /// <param name="Factors">Collection of factors contributing to the predicate score.</param>
    /// <param name="Threshold">Minimum score required for the predicate to be satisfied.</param>
    public record InputPredicate(
        IReadOnlyList<InputPredicateFactor> Factors,
        long Threshold)
    {
        /// <summary>
        /// Predicate that always evaluates to true regardless of inputs.
        /// </summary>
        public static readonly InputPredicate Unconditional =
            new(Factors: [], Threshold: 0);

        /// <summary>
        /// Determines whether this predicate is satisfied for the provided parameters and arguments.
        /// </summary>
        /// <param name="parameters">Static metadata describing the expression parameters.</param>
        /// <param name="arguments">Actual argument values supplied to the expression.</param>
        /// <returns><see langword="true"/> when the aggregated factor score meets or exceeds the threshold; otherwise, <see langword="false"/>.</returns>
        /// <exception cref="ArgumentException">Thrown when the arguments do not match the declared parameters or a parameter path is missing.</exception>
        public bool SatisfiedBy(
            StaticFunctionInterface parameters,
            IReadOnlyList<PineValue> arguments)
        {
            return SatisfiedBy(this, parameters, arguments);
        }

        /// <summary>
        /// Evaluates whether the supplied predicate is satisfied for the given parameters and arguments.
        /// </summary>
        /// <param name="predicate">Predicate configuration to evaluate.</param>
        /// <param name="parameters">Static metadata describing the expression parameters.</param>
        /// <param name="arguments">Actual argument values supplied to the expression.</param>
        /// <returns><see langword="true"/> when the aggregated factor score meets or exceeds the threshold; otherwise, <see langword="false"/>.</returns>
        /// <exception cref="ArgumentException">Thrown when the arguments do not match the declared parameters or a parameter path is missing.</exception>
        public static bool SatisfiedBy(
            InputPredicate predicate,
            StaticFunctionInterface parameters,
            IReadOnlyList<PineValue> arguments)
        {
            if (arguments.Count != parameters.ParamsPaths.Count)
            {
                throw new ArgumentException(
                    "Arguments count does not match parameters count.",
                    nameof(arguments));
            }

            long totalScore = 0;

            foreach (var factor in predicate.Factors)
            {
                var paramPath = factor.ParamPath;

                // Find the parameter index for the given path.
                var paramIndex = -1;

                for (var i = 0; i < parameters.ParamsPaths.Count; i++)
                {
                    if (parameters.ParamsPaths[i].SequenceEqual(paramPath))
                    {
                        paramIndex = i;
                        break;
                    }
                }

                if (paramIndex is -1)
                {
                    throw new ArgumentException(
                        "Parameter path not found in parameters.",
                        nameof(parameters));
                }

                var argumentValue = arguments[paramIndex];

                var (nodesCount, blobsBytesCount) =
                    argumentValue switch
                    {
                        PineValue.ListValue listValue =>
                        (listValue.NodesCount, listValue.BlobsBytesCount),

                        PineValue.BlobValue blobValue =>
                        (1, blobValue.Bytes.Length),

                        _ =>
                        throw new NotImplementedException(
                            "Unexpected value type for predicate evaluation: " + argumentValue.GetType()),
                    };

                totalScore +=
                    nodesCount * factor.NodesCountFactor +
                    blobsBytesCount * factor.BlobsBytesCountFactor;
            }

            return totalScore >= predicate.Threshold;
        }
    }

    /// <summary>
    /// Factor contributing to predicate evaluation, weighting structural characteristics of an input parameter.
    /// </summary>
    /// <param name="ParamPath">Path identifying the parameter within the <see cref="Expression.Environment"/> value.</param>
    /// <param name="NodesCountFactor">Weight applied to the total number of nodes in the parameter value.</param>
    /// <param name="BlobsBytesCountFactor">Weight applied to the total number of bytes contained in blob values.</param>
    public record InputPredicateFactor(
        IReadOnlyList<int> ParamPath,
        int NodesCountFactor,
        int BlobsBytesCountFactor);

    /// <summary>
    /// Entry linking an expression hash to its optimization configuration.
    /// </summary>
    /// <param name="HashSerial">Base16-encoded hash identifying the expression.</param>
    /// <param name="Config">Optimization configuration associated with the expression.</param>
    public record ExpressionEntry(
        string HashSerial,
        ExpressionConfig Config);
}
