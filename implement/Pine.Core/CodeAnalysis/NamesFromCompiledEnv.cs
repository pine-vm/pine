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
/// <see cref="NameFromDecl"/> repeatedly to resolve human-readable names while analyzing other structures
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
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        return FromCompiledEnvironment(parsedEnv, parseCache);
    }

    /// <summary>
    /// Creates a <see cref="NamesFromCompiledEnv"/> index from a previously parsed interactive environment.
    /// </summary>
    /// <param name="parsedEnv">The parsed interactive environment produced by the Elm interactive compiler.</param>
    /// <param name="parseCache">Cache used to avoid repeated parsing of embedded expressions/functions.</param>
    /// <returns>A new <see cref="NamesFromCompiledEnv"/> instance for querying declaration names.</returns>
    public static NamesFromCompiledEnv FromCompiledEnvironment(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv,
        PineVMParseCache parseCache)
    {
        return new NamesFromCompiledEnv(parsedEnv, parseCache);
    }

    private NamesFromCompiledEnv(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv,
        PineVMParseCache parseCache)
    {
        ParsedEnv = parsedEnv;

        var mutatedDict = new Dictionary<PineValue, ImmutableList<(DeclQualifiedName, PineValueClass envClass)>>();

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
                if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(decl.Value, parseCache).IsOkOrNull() is { } functionRecord)
                {
                    // Which env function is the entry pointing to?
                    if (functionRecord.InnerFunction is Expression.ParseAndEval innerParseAndEval)
                    {
                        if (CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(innerParseAndEval.Encoded) is
                            ExprMappedToParentEnv.PathInParentEnv bodyExprPath)
                        {
                            if (bodyExprPath.Path.Count is 2 &&
                                bodyExprPath.Path[0] is 0 && bodyExprPath.Path[1] < functionRecord.EnvFunctions.Length)
                            {
                                namedValue = functionRecord.EnvFunctions.Span[bodyExprPath.Path[1]];

                                envClass = PineValueClass.Create(
                                    [
                                    ..functionRecord.EnvFunctions.ToArray()
                                    .Select((envFuncValue, index) =>
                                        new KeyValuePair<IReadOnlyList<int>, PineValue>(
                                            [0, index],
                                            envFuncValue))
                                    ]);
                            }
                        }
                    }
                }

                var existingEntries =
                    mutatedDict.GetValueOrDefault(namedValue)
                    ??
                    [];

                mutatedDict[namedValue] = existingEntries.Add((qualifiedName, envClass));
            }
        }

        /*
        Legacy name enumeration logic kept for reference. It also indexed bodies and environment functions explicitly.
        Currently disabled because the primary consumer only needs the declaration itself and its class.
        foreach (var kvp in EnumerateNamesFromCompiledEnv(compiledEnvValue, parseCache))
        {
            var compositeName =
                string.Join(
                    ".",
                    kvp.Value);

            if (mutatedDict.TryGetValue(kvp.Key, out var existingName))
            {
                if (kvp.Value.Count < existingName.Count)
                {
                    mutatedDict[kvp.Key] = kvp.Value;
                }
            }
            else
            {
                mutatedDict[kvp.Key] = kvp.Value;
            }
        }
        */

        _namesFromCompiledEnv = mutatedDict.ToFrozenDictionary();
    }

    /// <summary>
    /// The parsed interactive environment used to build this index.
    /// </summary>
    public ElmInteractiveEnvironment.ParsedInteractiveEnvironment ParsedEnv { get; }

    private readonly FrozenDictionary<PineValue, ImmutableList<(DeclQualifiedName declName, PineValueClass envClass)>> _namesFromCompiledEnv;

    /// <summary>
    /// Attempts to retrieve the fully qualified declaration name associated with the supplied <paramref name="pineValue"/>,
    /// considering only those mappings whose <see cref="PineValueClass"/> satisfies <paramref name="envClass"/>.
    /// </summary>
    /// <param name="pineValue">The value to resolve (e.g., a function declaration value or referenced env function).</param>
    /// <param name="envClass">Constraint describing the expected environment shape when the value is used.</param>
    /// <returns>
    /// The first (most specific) matching fully qualified name if a match is found; otherwise <c>null</c>.
    /// Specificity is determined via <see cref="PineValueClassSpecificityComparer"/>.
    /// </returns>
    public string? NameFromDecl(
        PineValue pineValue,
        PineValueClass envClass)
    {
        if (_namesFromCompiledEnv.TryGetValue(pineValue, out var declsClasses))
        {
            var matchingClasses =
                declsClasses
                .Where(declClass => declClass.envClass.SatisfiedByConstraint(envClass))
                .OrderBy(keySelector: kvp => kvp.envClass, PineValueClassSpecificityComparer.Instance)
                .ToArray();

            if (matchingClasses.Length > 0)
            {
                return matchingClasses[0].declName.FullName;
            }
        }

        return null;
    }
}
