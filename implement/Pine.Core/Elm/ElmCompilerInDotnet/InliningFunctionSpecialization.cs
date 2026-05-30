using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// A named specialization ready for code generation.
/// Produced after the collection pass assigns deterministic names to the
/// deduplicated set of <see cref="FunctionSpecialization"/> requests.
/// </summary>
/// <param name="TargetFunctionName">The fully-qualified function name being specialized.</param>
/// <param name="Specialization">The original specialization request.</param>
/// <param name="SpecializedFunctionName">
/// The generated name for the specialized function, unique within the module.
/// </param>
public sealed record NamedSpecialization(
    DeclQualifiedName TargetFunctionName,
    FunctionSpecialization Specialization,
    string SpecializedFunctionName);

/// <summary>
/// The catalog of all specializations collected and named for a module.
/// Indexed by target function qualified name for fast lookup during the rewrite pass.
/// </summary>
/// <param name="SpecializationsByFunction">
/// Map from function qualified name to the list of named specializations available
/// for that function. Ordered by priority: specializations with more parameters specialized
/// away come first, so the rewrite pass can pick the best match by iterating in order.
/// </param>
public sealed record SpecializationCatalog(
    ImmutableDictionary<DeclQualifiedName, ImmutableList<NamedSpecialization>>
        SpecializationsByFunction)
{
    /// <summary>
    /// An empty catalog with no specializations.
    /// </summary>
    public static readonly SpecializationCatalog Empty =
        new([]);

    /// <summary>
    /// Given a list of available specializations for a function and the concrete arguments
    /// at a call site, finds the best matching specialization.
    /// Returns null if no specialization matches the call site arguments.
    ///
    /// Ranking: specializations are prioritized by the number of function-typed parameters
    /// that are specialized away (most specialized away = closest to first-order = best).
    /// Among ties, the first one in catalog order wins (deterministic).
    /// </summary>
    public static NamedSpecialization? FindBestSpecialization(
        ImmutableList<NamedSpecialization> availableSpecializations,
        IReadOnlyList<SyntaxTypes.Expression> callSiteArguments)
    {
        NamedSpecialization? best = null;
        var bestScore = -1;
        var bestSpecializedParamCount = -1;

        foreach (var candidate in availableSpecializations)
        {
            var spec = candidate.Specialization;
            var allMatch = true;

            foreach (var kvp in spec.ParameterSpecializations)
            {
                if (kvp.Key >= callSiteArguments.Count)
                {
                    allMatch = false;
                    break;
                }

                if (!ParameterSpecialization.ArgumentMatchesSpecialization(callSiteArguments[kvp.Key], kvp.Value))
                {
                    allMatch = false;
                    break;
                }
            }

            if (!allMatch)
                continue;

            var score = spec.SpecializedAwayCount;
            var specializedParamCount = spec.ParameterSpecializations.Count;

            if (score > bestScore ||
                (score == bestScore && specializedParamCount > bestSpecializedParamCount))
            {
                best = candidate;
                bestScore = score;
                bestSpecializedParamCount = specializedParamCount;
            }
        }

        return best;
    }

    /// <summary>
    /// Assigns deterministic names to a set of specialization requests for a given function.
    /// Each distinct specialization gets a unique suffix based on its position in the set.
    /// </summary>
    public static ImmutableList<NamedSpecialization> NameSpecializations(
        DeclQualifiedName targetFunctionName,
        IReadOnlyList<FunctionSpecialization> specializations,
        IReadOnlySet<string>? existingDeclNames = null)
    {
        var result = ImmutableList.CreateBuilder<NamedSpecialization>();
        var usedNames = existingDeclNames is not null ? new HashSet<string>(existingDeclNames) : [];

        for (var i = 0; i < specializations.Count; i++)
        {
            var candidateName = targetFunctionName.DeclName + GeneratedNameSuffixes.Specialized + (i + 1);

            // Derive a unique name that avoids clashing with existing declarations.
            var counter = i + 1;

            while (usedNames.Contains(candidateName))
            {
                counter++;
                candidateName = targetFunctionName.DeclName + GeneratedNameSuffixes.Specialized + counter;
            }

            usedNames.Add(candidateName);
            result.Add(new NamedSpecialization(targetFunctionName, specializations[i], candidateName));
        }

        return result.ToImmutable();
    }

    /// <summary>
    /// Builds a <see cref="SpecializationCatalog"/> from a flat list of named specializations.
    /// Groups them by target function and sorts each group by priority (most specialized first).
    /// </summary>
    public static SpecializationCatalog BuildCatalog(
        IReadOnlyList<NamedSpecialization> allSpecializations)
    {
        var builder =
            new Dictionary<DeclQualifiedName, ImmutableList<NamedSpecialization>.Builder>();

        foreach (var spec in allSpecializations)
        {
            var key = spec.TargetFunctionName;

            if (!builder.TryGetValue(key, out var list))
            {
                list = ImmutableList.CreateBuilder<NamedSpecialization>();
                builder[key] = list;
            }

            list.Add(spec);
        }

        var resultBuilder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, ImmutableList<NamedSpecialization>>();

        foreach (var kvp in builder)
        {
            var sorted =
                kvp.Value
                .ToImmutableList()
                .Sort(
                    (a, b) =>
                    {
                        var cmp =
                            b.Specialization.SpecializedAwayCount.CompareTo(a.Specialization.SpecializedAwayCount);

                        return
                            cmp != 0
                            ?
                            cmp
                            :
                            string.Compare(
                                a.SpecializedFunctionName,
                                b.SpecializedFunctionName,
                                System.StringComparison.Ordinal);
                    });

            resultBuilder[kvp.Key] = sorted;
        }

        return new SpecializationCatalog(resultBuilder.ToImmutable());
    }
}
