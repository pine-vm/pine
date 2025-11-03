using Pine.Core.PopularEncodings;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Builds an index mapping compiled environment values (function declarations and selected related values)
/// to the fully qualified names of their originating declarations.
/// <para>
/// The index also stores a <see cref="PineValueClass"/> for every declaration mapping so callers can later
/// query for the most specific matching declaration name under a given environment class constraint.
/// </para>
/// <para>
/// Typical usage: Construct an instance once for a compiled Elm interactive environment value and then call
/// <see cref="NamesForDecl"/> repeatedly to resolve human-readable names while analyzing other structures
/// (e.g., expressions referencing environment entries).
/// </para>
/// </summary>
public record NamesFromCompiledEnv
{
    /// <summary>
    /// Creates a <see cref="NamesFromCompiledEnv"/> index from the encoded compiled environment value.
    /// </summary>
    /// <param name="compiledEnvValue">The compiled environment value to parse and index.</param>
    /// <param name="parseCache">Cache used to avoid repeated parsing of embedded expressions/functions.</param>
    /// <returns>A new <see cref="NamesFromCompiledEnv"/> instance for querying declaration names.</returns>
    public static NamesFromCompiledEnv FromCompiledEnvironment(
        PineValue compiledEnvValue,
        PineVMParseCache parseCache)
    {
        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnvValue)
            .Extract(err => throw new Exception("Failed parsing interactive environment: " + err));

        return FromCompiledEnvironment(parsedEnv, parseCache, prioritizeName: _ => false);
    }

    /// <summary>
    /// Creates a <see cref="NamesFromCompiledEnv"/> index from a previously parsed interactive environment.
    /// </summary>
    /// <param name="parsedEnv">The parsed interactive environment produced by the Elm interactive compiler.</param>
    /// <param name="parseCache">Cache used to avoid repeated parsing of embedded expressions/functions.</param>
    /// <param name="prioritizeName">
    /// Predicate to prioritize specific declaration names when multiple declarations resolve to the same encoded value.
    /// Names for which this returns <see langword="true"/> are ordered first in the result.
    /// </param>
    /// <returns>A new <see cref="NamesFromCompiledEnv"/> instance for querying declaration names.</returns>
    public static NamesFromCompiledEnv FromCompiledEnvironment(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv,
        PineVMParseCache parseCache,
        Func<DeclQualifiedName, bool> prioritizeName)
    {
        return new NamesFromCompiledEnv(parsedEnv, parseCache, prioritizeName);
    }

    private NamesFromCompiledEnv(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv,
        PineVMParseCache parseCache,
        Func<DeclQualifiedName, bool> prioritizeName)
    {
        ParsedEnv = parsedEnv;

        var beforeFilter =
            new List<(DeclQualifiedName declName, PineValue origValue, (PineValue encodedExpr, PineValueClass envValueClass) application)>();

        foreach (var parsedModule in parsedEnv.Modules)
        {
            foreach (var decl in parsedModule.moduleContent.FunctionDeclarations)
            {
                var qualifiedName =
                    new DeclQualifiedName(
                        Namespaces: parsedModule.moduleName.Split('.'),
                        DeclName: decl.Key);

                var namedValue = decl.Value;
                var envClass = PineValueClass.Create([]);

                // Attempt to see if the declaration encodes (or references) a function whose body is directly an environment function.
                if (FunctionRecord.ParseFunctionRecordTagged(decl.Value, parseCache).IsOkOrNull() is { } functionRecord)
                {
                    if (BuildApplicationFromFunctionRecord(functionRecord, arguments: [], parseCache) is { } found)
                    {
                        (namedValue, _, envClass) = found;
                    }
                }

                beforeFilter.Add((qualifiedName, decl.Value, (namedValue, envClass)));
            }
        }

        _namesFromCompiledEnv =
            beforeFilter
            .GroupBy(item => item.application.encodedExpr)
            .ToFrozenDictionary(
                keySelector: g => g.Key,
                elementSelector:
                encodedExprGroup =>
                {
                    var filteredList =
                        encodedExprGroup
                        .OrderByDescending(item => prioritizeName(item.declName))
                        .ThenBy(item => item.origValue is PineValue.ListValue origList ? origList.NodesCount : 0)
                        .DistinctBy(item => item.application.envValueClass)
                        .Select(item => (item.declName, item.application.envValueClass))
                        .ToImmutableList();

                    return filteredList;
                });
    }

    /// <summary>
    /// Build a function application from a function record.
    /// </summary>
    public static (PineValue encodedExpr, Expression expr, PineValueClass envValueClass) BuildApplicationFromFunctionRecord(
        FunctionRecord functionRecord,
        IReadOnlyList<PineValue> arguments,
        PineVMParseCache parseCache)
    {
        var outerEnvClass =
            PineValueClass.Create(
                [
                 ..functionRecord.EnvFunctions.ToArray()
                     .Select((envFuncValue, index) =>
                     new KeyValuePair<IReadOnlyList<int>, PineValue>(
                         [0, index],
                         envFuncValue)),

                 ..arguments.Select((argValue, argIndex) =>
                     new KeyValuePair<IReadOnlyList<int>, PineValue>(
                         [1, argIndex],
                         argValue)),
                 ]);

        if (functionRecord.InnerFunction is Expression.ParseAndEval innerParseAndEval)
        {
            // Which env function is the entry pointing to?

            if (CodeAnalysis.TryParseExprAsPathInEnv(innerParseAndEval.Encoded) is { } bodyExprPath)
            {
                if (bodyExprPath.Count is 2 &&
                    bodyExprPath[0] is 0 && bodyExprPath[1] < functionRecord.EnvFunctions.Length)
                {
                    var namedValue = functionRecord.EnvFunctions.Span[bodyExprPath[1]];

                    var innerEnvClass =
                        PineValueClass.MapValueClass(outerEnvClass, innerParseAndEval.Environment);

                    if (innerEnvClass is null)
                    {
                        throw new System.NotImplementedException(
                            "Failed to map outer environment class to inner environment.");
                    }

                    if (parseCache.ParseExpression(namedValue).IsOkOrNull() is { } expr)
                    {
                        return (namedValue, expr, innerEnvClass);
                    }
                }
            }
        }

        {
            var innerFunctionEncoded = ExpressionEncoding.EncodeExpressionAsValue(functionRecord.InnerFunction);

            return (innerFunctionEncoded, functionRecord.InnerFunction, outerEnvClass);
        }
    }

    /// <summary>
    /// The parsed interactive environment used to build this index.
    /// </summary>
    public ElmInteractiveEnvironment.ParsedInteractiveEnvironment ParsedEnv { get; }

    private readonly FrozenDictionary<PineValue, ImmutableList<(DeclQualifiedName declName, PineValueClass envClass)>> _namesFromCompiledEnv;

    /// <summary>
    /// Resolves the declaration names whose encoded value matches <paramref name="pineValue"/> under the
    /// provided <paramref name="envClass"/> constraint.
    /// </summary>
    /// <param name="pineValue">The encoded expression or value from the compiled environment.</param>
    /// <param name="envClass">The environment class describing the paths already satisfied by the caller.</param>
    /// <returns>
    /// Ordered declaration names that match the supplied value; returns <see langword="null"/> when the
    /// value is not indexed.
    /// </returns>
    public IReadOnlyList<DeclQualifiedName>? NamesForDecl(
        PineValue pineValue,
        PineValueClass envClass)
    {
        if (_namesFromCompiledEnv.TryGetValue(pineValue, out var declsClasses))
        {
            var matchesNames = new List<DeclQualifiedName>();

            foreach (var (declName, declClass) in declsClasses)
            {
                if (RemainderAfterSatisfyingConstraint(declClass, envClass) is not { } remainderClass)
                {
                    continue;
                }

                if (remainderClass.ParsedItems.Count is 0)
                {
                    matchesNames.Add(declName);
                }
                else
                {
                    var expandedDeclName =
                        new DeclQualifiedName(
                            Namespaces: declName.Namespaces,
                            DeclName: string.Concat(declName.DeclName, "_", remainderClass.HashBase16.AsSpan(0, 8)));

                    matchesNames.Add(expandedDeclName);
                }
            }

            return matchesNames;
        }

        return null;
    }

    private static PineValueClass? RemainderAfterSatisfyingConstraint(
        PineValueClass constraint,
        PineValueClass valueClass)
    {
        if (valueClass == constraint)
        {
            // Redundant specialized fast path to reduce runtime expenses.
            return PineValueClass.Empty;
        }

        if (!constraint.SatisfiedByConstraint(valueClass))
        {
            return null;
        }

        bool PathIsAlreadyCovered(IReadOnlyList<int> path)
        {
            foreach (var constraintItem in constraint.ParsedItems)
            {
                if (constraintItem.Key.Count < path.Count)
                {
                    continue;
                }

                var foundMismatch = false;

                for (var i = 0; i < path.Count; i++)
                {
                    if (constraintItem.Key[i] != path[i])
                    {
                        foundMismatch = true;
                        break;
                    }
                }

                if (!foundMismatch)
                {
                    return true;
                }
            }

            return false;
        }

        var remainingItems =
            valueClass.ParsedItems
            .Where(vcItem => !PathIsAlreadyCovered(vcItem.Key))
            .ToArray();

        return PineValueClass.Create(remainingItems);
    }
}
