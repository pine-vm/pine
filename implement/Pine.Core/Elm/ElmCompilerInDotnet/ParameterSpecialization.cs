using Pine.Core.CodeAnalysis;
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
    /// The parameter was removed from the specialized function's parameter list.
    /// This typically happens when a parameter became obsolete via instantiation
    /// for a concrete argument that no longer needs to be passed through.
    /// </summary>
    public sealed record Eliminated
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
    /// The parameter is bound to a tuple value whose elements each carry their
    /// own specialization. Used to fold a literal tuple of statically-known
    /// callables (functions, lambdas, record-access functions) into a
    /// first-order sibling by binding each tuple-leaf var-pattern to the
    /// matching element specialization.
    /// <para>
    /// Models the "tuple of knowns" shape that survives lambda-lifting when a
    /// helper takes a <see cref="SyntaxTypes.Pattern.TuplePattern"/> parameter
    /// whose elements are themselves function-typed. See
    /// <c>explore/internal-analysis/2026-05-19-monomorphizing-expressionAfterOpeningSquareBracket-lifted-lambda3.md</c>
    /// §3 for the production scenario.
    /// </para>
    /// <para>
    /// Construction of this variant is the responsibility of the discovery
    /// walker (<c>TryBuildFunctionSpecializationForHigherOrder</c>);
    /// <see cref="ClassifyArgument"/> never produces it directly because
    /// a tuple argument may legitimately be passed to a parameter modelled
    /// as <see cref="ConcreteLambdaValue"/> when the callee's parameter
    /// pattern is a plain <see cref="SyntaxTypes.Pattern.VarPattern"/>.
    /// </para>
    /// </summary>
    /// <param name="ElementSpecializations">
    /// Per-element specializations in tuple declared order. The array
    /// length equals the arity of the tuple; nested
    /// <see cref="TupleUnwrap"/> values model tuple-of-tuple shapes.
    /// </param>
    public sealed record TupleUnwrap(
        ImmutableArray<ParameterSpecialization> ElementSpecializations)
        : ParameterSpecialization
    {
        /// <inheritdoc/>
        public bool Equals(TupleUnwrap? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            if (ElementSpecializations.Length != other.ElementSpecializations.Length)
                return false;

            for (var i = 0; i < ElementSpecializations.Length; i++)
            {
                if (!ElementSpecializations[i].Equals(other.ElementSpecializations[i]))
                    return false;
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hash = new System.HashCode();

            // Length first so empty / arity mismatches diverge immediately.
            hash.Add(ElementSpecializations.Length);

            foreach (var element in ElementSpecializations)
                hash.Add(element);

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

            TupleUnwrap tupleUnwrap =>
            TryMatchTupleArgumentWithElementSpecializations(argument, tupleUnwrap),

            _ =>
            false
        };
    }

    /// <summary>
    /// Matches a tuple argument against a <see cref="TupleUnwrap"/>. Each
    /// tuple element is checked against the matching element specialization
    /// after one round of paren-peeling AND one optional layer of
    /// newtype-wrap peeling (the <c>let (Wrapper name) = ref in name</c>
    /// shape) — see <see cref="PeelOneLayerLetNewtypeWrap"/>.
    /// </summary>
    private static bool TryMatchTupleArgumentWithElementSpecializations(
        SyntaxTypes.Expression argument,
        TupleUnwrap tupleUnwrap)
    {
        var unwrapped = UnwrapParenthesized(argument);

        if (unwrapped is not SyntaxTypes.Expression.TupledExpression tupled)
            return false;

        if (tupled.Elements.Count != tupleUnwrap.ElementSpecializations.Length)
            return false;

        for (var i = 0; i < tupled.Elements.Count; i++)
        {
            var elementExpr = PeelOneLayerLetNewtypeWrap(UnwrapParenthesized(tupled.Elements[i].Value));

            if (!ArgumentMatchesSpecialization(elementExpr, tupleUnwrap.ElementSpecializations[i]))
                return false;
        }

        return true;
    }

    /// <summary>
    /// Peels exactly one layer of single-tag newtype unwrap-then-rewrap of
    /// the shape <c>let (Wrapper name) = &lt;ref&gt; in name</c> from the given
    /// expression. Returns the inner <c>&lt;ref&gt;</c> if the shape matches,
    /// otherwise returns <paramref name="expression"/> unchanged.
    /// <para>
    /// The matcher requires:
    /// <list type="bullet">
    ///   <item><description>Exactly one declaration in the let block.</description></item>
    ///   <item><description>That declaration is a <see cref="SyntaxTypes.Expression.LetDeclaration.LetDestructuring"/>.</description></item>
    ///   <item><description>The destructuring pattern is a <see cref="SyntaxTypes.Pattern.NamedPattern"/> with exactly one <see cref="SyntaxTypes.Pattern.VarPattern"/> argument.</description></item>
    ///   <item><description>The let block's body is a <see cref="SyntaxTypes.Expression.FunctionOrValue"/> with empty <c>ModuleName</c> whose <c>Name</c> matches the bound var name.</description></item>
    /// </list>
    /// </para>
    /// <para>
    /// This is the shape produced by the Elm front-end for code like
    /// <c>let (Parser pA) = parseDouble in pA</c>, which the
    /// <c>WrapperReturnStripping</c> / <c>WrapUnwrapCancellation</c>
    /// passes collapse but that the specialization-discovery walker may
    /// observe before those passes run on a given sub-expression.
    /// </para>
    /// </summary>
    private static SyntaxTypes.Expression PeelOneLayerLetNewtypeWrap(
        SyntaxTypes.Expression expression)
    {
        if (expression is not SyntaxTypes.Expression.LetExpression letExpr)
            return expression;

        if (letExpr.Value.Declarations.Count is not 1)
            return expression;

        if (letExpr.Value.Declarations[0].Value is not SyntaxTypes.Expression.LetDeclaration.LetDestructuring destructuring)
            return expression;

        var patternInner = UnwrapAsPattern(destructuring.Pattern.Value);

        if (patternInner is not SyntaxTypes.Pattern.NamedPattern namedPattern)
            return expression;

        if (namedPattern.Arguments.Count is not 1)
            return expression;

        var argInner = UnwrapAsPattern(namedPattern.Arguments[0].Value);

        if (argInner is not SyntaxTypes.Pattern.VarPattern varPattern)
            return expression;

        var bodyInner = UnwrapParenthesized(letExpr.Value.Expression.Value);

        if (bodyInner is not SyntaxTypes.Expression.FunctionOrValue tailRef)
            return expression;

        if (tailRef.ModuleName.Count is not 0 || tailRef.Name != varPattern.Name)
            return expression;

        return destructuring.Expression.Value;
    }

    private static SyntaxTypes.Pattern UnwrapAsPattern(SyntaxTypes.Pattern pattern)
    {
        while (pattern is SyntaxTypes.Pattern.AsPattern asPattern)
        {
            pattern = asPattern.Pattern.Value;
        }

        if (pattern is SyntaxTypes.Pattern.ParenthesizedPattern paren)
        {
            return UnwrapAsPattern(paren.Pattern.Value);
        }

        return pattern;
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
            new ConcreteFunctionValue(DeclQualifiedName.Create(fov.ModuleName, fov.Name)),

            SyntaxTypes.Expression.FunctionOrValue fov when IsKnownStableUnqualifiedFunctionReference(fov.Name) =>
            new ConcreteFunctionValue(DeclQualifiedName.Create([], fov.Name)),

            SyntaxTypes.Expression.LambdaExpression lambda =>
            new ConcreteLambdaValue(lambda.Lambda),

            SyntaxTypes.Expression.RecordAccessFunction recordAccessFunction =>
            new ConcreteRecordAccessFunctionValue(recordAccessFunction.FunctionName),

            _ =>
            null
        };
    }

    /// <summary>
    /// Variant of <see cref="ClassifyArgument(SyntaxTypes.Expression)"/> that
    /// additionally peels one let-bound hop using the supplied
    /// <paramref name="letRhsByName"/> map before attempting classification.
    /// <para>
    /// When the argument (after paren-peel) is a bare
    /// <see cref="SyntaxTypes.Expression.FunctionOrValue"/> with empty
    /// <c>ModuleName</c> whose <c>Name</c> is present in
    /// <paramref name="letRhsByName"/>, the resolved RHS expression is
    /// classified instead. This lets the discovery walker see through the
    /// <c>let (Wrapper name) = &lt;ref&gt; in ... name ...</c> shape — for
    /// the specific <c>let</c>-destructuring case the discovery walker
    /// pre-populates the map only when the destructuring pattern peels
    /// one layer of a single-tag newtype constructor (see
    /// <c>PeelOneLayerLetNewtypeWrap</c> for the symmetric matcher).
    /// </para>
    /// <para>
    /// Falls through to the no-context overload when the argument is not
    /// a let-bound reference or the lookup misses.
    /// </para>
    /// </summary>
    public static ParameterSpecialization? ClassifyArgument(
        SyntaxTypes.Expression argument,
        ImmutableDictionary<string, SyntaxTypes.Expression> letRhsByName)
    {
        if (letRhsByName is null || letRhsByName.IsEmpty)
            return ClassifyArgument(argument);

        var unwrapped = UnwrapParenthesized(argument);

        if (unwrapped is SyntaxTypes.Expression.FunctionOrValue fov &&
            fov.ModuleName.Count is 0 &&
            letRhsByName.TryGetValue(fov.Name, out var resolvedRhs))
        {
            return ClassifyArgument(resolvedRhs);
        }

        return ClassifyArgument(argument);
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

