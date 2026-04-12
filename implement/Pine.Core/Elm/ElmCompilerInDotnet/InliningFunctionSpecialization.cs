using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Describes how a single function parameter is specialized.
/// This is the value type in the per-parameter dictionary of a <see cref="FunctionSpecialization"/>.
/// </summary>
public abstract record ParameterSpecialization
{
    private ParameterSpecialization() { }

    internal static bool ParameterSpecializationMapEquals(
        ImmutableDictionary<int, ParameterSpecialization> left,
        ImmutableDictionary<int, ParameterSpecialization> right)
    {
        if (left.Count != right.Count)
            return false;

        foreach (var kvp in left)
        {
            if (!right.TryGetValue(kvp.Key, out var otherValue) ||
                !kvp.Value.Equals(otherValue))
            {
                return false;
            }
        }

        return true;
    }

    internal static int ParameterSpecializationMapHashCode(
        ImmutableDictionary<int, ParameterSpecialization> map)
    {
        var hash = new System.HashCode();

        foreach (var kvp in map.OrderBy(kvp => kvp.Key))
        {
            hash.Add(kvp.Key);
            hash.Add(kvp.Value);
        }

        return hash.ToHashCode();
    }

    /// <summary>
    /// The parameter is bound to a concrete function reference from the original source code.
    /// Used when specializing a higher-order function into a first-order variant by
    /// substituting a function-typed parameter with the concrete function passed at the call site.
    /// </summary>
    /// <param name="FunctionQualifiedName">Fully-qualified name of the referenced function.</param>
    public sealed record ConcreteFunctionValue(
        DeclQualifiedName FunctionQualifiedName)
        : ParameterSpecialization;

    /// <summary>
    /// The parameter is bound to a lambda expression from the call site.
    /// This covers cases where the caller passes an inline lambda rather than a named function reference.
    /// </summary>
    /// <param name="Lambda">The lambda structure from the call site argument.</param>
    public sealed record ConcreteLambdaValue(
        SyntaxTypes.LambdaStruct Lambda)
        : ParameterSpecialization
    {
        /// <inheritdoc/>
        public bool Equals(ConcreteLambdaValue? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            return
                other is not null &&
                Lambda.Equals(other.Lambda);
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            Lambda.GetHashCode();
    }

    /// <summary>
    /// The parameter is bound to a record-access function such as <c>.extensionRight</c>.
    /// This is important for parser code where field accessors are passed as higher-order
    /// function arguments and should be treated like other concrete function values.
    /// </summary>
    /// <param name="FunctionName">The record-access function name including the leading dot.</param>
    public sealed record ConcreteRecordAccessFunctionValue(
        string FunctionName)
        : ParameterSpecialization;

    /// <summary>
    /// The parameter carries a single-choice (single-constructor) choice type tag.
    /// Specialization unwraps the tag so the specialized function receives the
    /// inner fields directly. Function-typed fields are substituted inline;
    /// non-function fields become direct parameters.
    /// </summary>
    /// <param name="ConstructorName">Fully-qualified constructor name.</param>
    /// <param name="FieldSpecializations">
    /// Specializations for fields within the constructor.
    /// Function-typed fields are represented directly as concrete function/lambda values.
    /// Nested single-choice constructor fields are represented recursively with another
    /// <see cref="SingleChoiceTagUnwrap"/>.
    /// Empty when all fields are plain data fields.
    /// </param>
    public sealed record SingleChoiceTagUnwrap(
        DeclQualifiedName ConstructorName,
        ImmutableDictionary<int, ParameterSpecialization> FieldSpecializations)
        : ParameterSpecialization
    {
        /// <inheritdoc/>
        public bool Equals(SingleChoiceTagUnwrap? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            return
                other is not null &&
                ConstructorName.Equals(other.ConstructorName) &&
                ParameterSpecializationMapEquals(FieldSpecializations, other.FieldSpecializations);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hash = new System.HashCode();
            hash.Add(ConstructorName);
            hash.Add(ParameterSpecializationMapHashCode(FieldSpecializations));
            return hash.ToHashCode();
        }
    }

    /// <summary>
    /// Checks whether a concrete argument expression at a call site matches the
    /// <see cref="ParameterSpecialization"/> stored in a specialization entry.
    /// Returns true if the argument can satisfy the specialization's requirement.
    /// </summary>
    public static bool ArgumentMatchesSpecialization(
        SyntaxTypes.Expression argument,
        ParameterSpecialization specialization)
    {
        return specialization switch
        {
            ConcreteFunctionValue concreteFunc =>
            argument is SyntaxTypes.Expression.FunctionOrValue fov &&
            fov.Name == concreteFunc.FunctionQualifiedName.DeclName &&
            Enumerable.SequenceEqual(fov.ModuleName, concreteFunc.FunctionQualifiedName.Namespaces),

            ConcreteLambdaValue concreteLambda =>
            argument is SyntaxTypes.Expression.LambdaExpression lambdaExpr
            ?
            lambdaExpr.Lambda.Equals(concreteLambda.Lambda)
            :
            concreteLambda.Lambda.Arguments.Count is 0 &&
            concreteLambda.Lambda.Expression.Value.Equals(argument),

            ConcreteRecordAccessFunctionValue concreteRecordAccessFunction =>
            argument is SyntaxTypes.Expression.RecordAccessFunction recordAccessFunction &&
            recordAccessFunction.FunctionName == concreteRecordAccessFunction.FunctionName,

            SingleChoiceTagUnwrap tagUnwrap =>
            TryMatchSingleChoiceTagArgumentWithSpecializedFields(argument, tagUnwrap),

            _ =>
            false
        };
    }

    /// <summary>
    /// Builds a <see cref="ParameterSpecialization"/> from a concrete argument expression.
    /// Returns null if the argument cannot be classified into any specialization kind.
    /// </summary>
    public static ParameterSpecialization? ClassifyArgument(
        SyntaxTypes.Expression argument)
    {
        var unwrapped = UnwrapParenthesized(argument);

        return unwrapped switch
        {
            SyntaxTypes.Expression.FunctionOrValue fov when fov.ModuleName.Count > 0 =>
            new ConcreteFunctionValue(new DeclQualifiedName(fov.ModuleName, fov.Name)),

            SyntaxTypes.Expression.FunctionOrValue fov when IsKnownStableUnqualifiedFunctionReference(fov.Name) =>
            new ConcreteFunctionValue(new DeclQualifiedName([], fov.Name)),

            SyntaxTypes.Expression.LambdaExpression lambda =>
            new ConcreteLambdaValue(lambda.Lambda),

            SyntaxTypes.Expression.RecordAccessFunction recordAccessFunction =>
            new ConcreteRecordAccessFunctionValue(recordAccessFunction.FunctionName),

            _ =>
            null
        };
    }

    private static bool IsKnownStableUnqualifiedFunctionReference(string name) =>
        name is "::";

    /// <summary>
    /// Builds a <see cref="SingleChoiceTagUnwrap"/> for an argument
    /// that is a single-choice constructor application.
    /// </summary>
    public static ParameterSpecialization? ClassifySingleChoiceTagArgument(
        SyntaxTypes.Expression argument,
        DeclQualifiedName constructorName)
    {
        if (TryMatchSingleChoiceTagArgument(argument, constructorName))
        {
            return
                new SingleChoiceTagUnwrap(
                    constructorName,
                    []);
        }

        return null;
    }

    /// <summary>
    /// Checks whether an expression is a constructor application matching the given constructor name.
    /// </summary>
    private static bool TryMatchSingleChoiceTagArgument(
        SyntaxTypes.Expression argument,
        DeclQualifiedName constructorName)
    {
        while (argument is SyntaxTypes.Expression.ParenthesizedExpression paren)
        {
            argument = paren.Expression.Value;
        }

        if (argument is SyntaxTypes.Expression.Application app &&
            app.Arguments.Count >= 1 &&
            app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue fov)
        {
            return
                fov.Name == constructorName.DeclName &&
                (fov.ModuleName.Count is 0 ||
                Enumerable.SequenceEqual(fov.ModuleName, constructorName.Namespaces));
        }

        if (argument is SyntaxTypes.Expression.FunctionOrValue bareFov)
        {
            return
                bareFov.Name == constructorName.DeclName &&
                (bareFov.ModuleName.Count is 0 ||
                Enumerable.SequenceEqual(bareFov.ModuleName, constructorName.Namespaces));
        }

        return false;
    }

    /// <summary>
    /// Matches a constructor application argument against a <see cref="SingleChoiceTagUnwrap"/>
    /// including checking that function-typed fields match.
    /// </summary>
    private static bool TryMatchSingleChoiceTagArgumentWithSpecializedFields(
        SyntaxTypes.Expression argument,
        SingleChoiceTagUnwrap tagUnwrap)
    {
        if (!TryMatchSingleChoiceTagArgument(argument, tagUnwrap.ConstructorName))
            return false;

        if (tagUnwrap.FieldSpecializations.Count is 0)
            return true;

        var unwrapped = argument;

        while (unwrapped is SyntaxTypes.Expression.ParenthesizedExpression paren)
            unwrapped = paren.Expression.Value;

        if (unwrapped is SyntaxTypes.Expression.Application app && app.Arguments.Count >= 2)
        {
            var fieldExprs = app.Arguments.Skip(1).ToList();

            foreach (var kvp in tagUnwrap.FieldSpecializations)
            {
                if (kvp.Key >= fieldExprs.Count)
                    return false;

                if (!ArgumentMatchesSpecialization(fieldExprs[kvp.Key].Value, kvp.Value))
                    return false;
            }

            return true;
        }

        return tagUnwrap.FieldSpecializations.Count is 0;
    }

    private static SyntaxTypes.Expression UnwrapParenthesized(SyntaxTypes.Expression expr)
    {
        while (expr is SyntaxTypes.Expression.ParenthesizedExpression paren)
        {
            expr = paren.Expression.Value;
        }

        return expr;
    }
}

/// <summary>
/// A specialization of a function: a dictionary mapping parameter indices to the
/// kind of specialization applied at that parameter.
/// Multiple parameters may be specialized simultaneously (e.g., two function-typed
/// parameters can both be concretized in one specialization).
/// </summary>
/// <param name="ParameterSpecializations">
/// Map from parameter index to the specialization for that parameter.
/// Only parameters that are being specialized appear in this dictionary.
/// </param>
public sealed record FunctionSpecialization(
    ImmutableDictionary<int, ParameterSpecialization> ParameterSpecializations)
{
    /// <inheritdoc/>
    public bool Equals(FunctionSpecialization? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        return
            other is not null &&
            ParameterSpecialization.ParameterSpecializationMapEquals(
                ParameterSpecializations,
                other.ParameterSpecializations);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hash = new System.HashCode();
        hash.Add(ParameterSpecialization.ParameterSpecializationMapHashCode(ParameterSpecializations));

        return hash.ToHashCode();
    }

    /// <summary>
    /// The number of parameters that are specialized away (removed from the parameter list
    /// of the generated function). For higher-order specializations, this equals the count
    /// of <see cref="ParameterSpecialization.ConcreteFunctionValue"/> and
    /// <see cref="ParameterSpecialization.ConcreteLambdaValue"/> entries.
    /// For single-choice tag unwrap, this property only counts top-level specialized-away
    /// parameters represented directly in <see cref="ParameterSpecializations"/>. Nested
    /// field specializations inside <see cref="ParameterSpecialization.SingleChoiceTagUnwrap"/>
    /// are not counted recursively here.
    /// </summary>
    public int SpecializedAwayCount
    {
        get
        {
            var count = 0;

            foreach (var kvp in ParameterSpecializations)
            {
                if (kvp.Value is ParameterSpecialization.ConcreteFunctionValue or
                    ParameterSpecialization.ConcreteLambdaValue or
                    ParameterSpecialization.ConcreteRecordAccessFunctionValue)
                {
                    count++;
                }
            }

            return count;
        }
    }

    /// <summary>
    /// Produces a deterministic string key for stable ordering when naming specializations.
    /// Since <see cref="ImmutableHashSet{T}"/> iteration order is non-deterministic,
    /// this key ensures that specialization numbering (e.g. <c>__specialized__1</c>,
    /// <c>__specialized__2</c>) is reproducible across runs.
    /// </summary>
    public string DeterministicSortKey
    {
        get
        {
            var parts =
                ParameterSpecializations
                .OrderBy(kvp => kvp.Key)
                .Select(kvp => kvp.Key + ":" + ParameterSpecializationSortKey(kvp.Value));

            return string.Join("|", parts);
        }
    }

    private static string ParameterSpecializationSortKey(ParameterSpecialization spec)
    {
        return spec switch
        {
            ParameterSpecialization.ConcreteFunctionValue cfv =>
            "F:" + cfv.FunctionQualifiedName.FullName,

            ParameterSpecialization.ConcreteLambdaValue clv =>
            "L:" + clv.Lambda.ToString(),

            ParameterSpecialization.ConcreteRecordAccessFunctionValue cra =>
            "R:" + cra.FunctionName,

            ParameterSpecialization.SingleChoiceTagUnwrap tagUnwrap =>
            "T:" + tagUnwrap.ConstructorName.FullName + "{" +
            string.Join(
                ",",
                tagUnwrap.FieldSpecializations
                .OrderBy(kvp => kvp.Key)
                .Select(kvp => kvp.Key + ":" + ParameterSpecializationSortKey(kvp.Value))) +
            "}",

            _ =>
            spec.ToString() ?? ""
        };
    }
}

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
        IReadOnlyList<FunctionSpecialization> specializations)
    {
        var result = ImmutableList.CreateBuilder<NamedSpecialization>();

        for (var i = 0; i < specializations.Count; i++)
        {
            var name = targetFunctionName.DeclName + "__specialized__" + (i + 1);
            result.Add(new NamedSpecialization(targetFunctionName, specializations[i], name));
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
