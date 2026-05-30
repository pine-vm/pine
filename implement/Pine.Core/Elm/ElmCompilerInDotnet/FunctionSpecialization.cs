using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Immutable;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;


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
/// <param name="ReturnValueSpecialization">
/// How the specialized function's return value relates to the
/// original's return value. Drives caller-side rewriting at
/// fully-saturated call sites of the original. Defaults to
/// <see cref="ReturnValueSpecialization.Identity"/>: no transformation
/// applied by the caller. <see cref="ReturnValueSpecialization.WrapWithConstructor"/>
/// is used by <see cref="WrapperReturnStripping"/> to express that
/// the sibling decl returns the inner (unwrapped) value and the
/// caller must re-introduce the original's outer single-tag
/// constructor.
/// </param>
public sealed record FunctionSpecialization(
    ImmutableDictionary<int, ParameterSpecialization> ParameterSpecializations,
    ReturnValueSpecialization ReturnValueSpecialization)
{
    /// <summary>
    /// Convenience constructor used by call sites that do not transform
    /// the return value (i.e. the vast majority — higher-order
    /// parameter and tag-unwrap specializations). Equivalent to
    /// passing <see cref="ReturnValueSpecialization.Identity.Instance"/>.
    /// </summary>
    public FunctionSpecialization(
        ImmutableDictionary<int, ParameterSpecialization> parameterSpecializations)
        : this(parameterSpecializations, ReturnValueSpecialization.Identity.Instance)
    {
    }

    /// <inheritdoc/>
    public bool Equals(FunctionSpecialization? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        return
            other is not null &&
            ParameterSpecialization.ParameterSpecializationMapEquals(
                ParameterSpecializations,
                other.ParameterSpecializations) &&
            ReturnValueSpecialization.Equals(other.ReturnValueSpecialization);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hash = new System.HashCode();
        hash.Add(ParameterSpecialization.ParameterSpecializationMapHashCode(ParameterSpecializations));
        hash.Add(ReturnValueSpecialization);

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

            var paramKey = string.Join("|", parts);

            var returnKey = ReturnValueSpecialization switch
            {
                ReturnValueSpecialization.Identity =>
                "I",

                ReturnValueSpecialization.WrapWithConstructor wrap =>
                "W:" + wrap.Constructor.FullName,

                _ =>
                ReturnValueSpecialization.ToString() ?? ""
            };

            return paramKey + "#" + returnKey;
        }
    }

    private static string ParameterSpecializationSortKey(ParameterSpecialization spec)
    {
        return spec switch
        {
            ParameterSpecialization.ConcreteFunctionValue cfv =>
            "F:" + cfv.FunctionQualifiedName.FullName,

            ParameterSpecialization.ConcreteLambdaValue clv =>
            "L:" + LambdaStructuralFingerprint(clv.Lambda),

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

            ParameterSpecialization.TupleUnwrap tupleUnwrap =>
            "U:[" +
            string.Join(
                ",",
                tupleUnwrap.ElementSpecializations.Select(ParameterSpecializationSortKey)) + "]",

            _ =>
            spec.ToString() ?? ""
        };
    }

    /// <summary>
    /// Deep structural fingerprint of a lambda, used as a deterministic
    /// tiebreaker when sorting <see cref="FunctionSpecialization"/>s
    /// whose <see cref="ParameterSpecialization.ConcreteLambdaValue"/>
    /// entries are structurally distinct but happen to print the same
    /// way under the default record <c>ToString</c>. The default record
    /// formatter renders <see cref="System.Collections.Generic.IReadOnlyList{T}"/>
    /// properties (e.g. <c>LambdaStruct.Arguments</c>) as their runtime
    /// type name rather than enumerating elements, so two lambdas with
    /// different argument patterns or different list-shaped sub-trees
    /// inside the body would collide on a shallow <c>ToString</c>-based
    /// key. We wrap the lambda in a synthetic value declaration and
    /// reuse <see cref="DeclarationDeduplication.GetStructuralFingerprint"/>,
    /// which delegates to the snapshot formatter — a deep, purely
    /// structural walk.
    /// </summary>
    private static string LambdaStructuralFingerprint(SyntaxTypes.LambdaStruct lambda)
    {
        var range = ElmSyntaxTransformations.s_zeroRange;

        var fn =
            new SyntaxTypes.Declaration.FunctionDeclaration(
                new SyntaxTypes.FunctionStruct(
                    Documentation: null,
                    Signature: null,
                    Declaration: new Node<SyntaxTypes.FunctionImplementation>(
                        range,
                        new SyntaxTypes.FunctionImplementation(
                            Name: new Node<string>(range, "__lambda__"),
                            Arguments: [],
                            Expression: new Node<SyntaxTypes.Expression>(
                                range,
                                new SyntaxTypes.Expression.LambdaExpression(lambda))))));

        return DeclarationDeduplication.GetStructuralFingerprint(fn, ["__LambdaSortKey__"]);
    }
}

