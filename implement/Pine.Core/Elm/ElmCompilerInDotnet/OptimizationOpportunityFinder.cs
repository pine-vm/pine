using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Categorises an <see cref="Opportunity"/> by the kind of generic operation
/// (or by the kind of unspecialized parameter usage) it represents. The
/// category is rendered alongside the opportunity's
/// <see cref="Opportunity.Description"/> in
/// <see cref="OptimizationOpportunityRenderer.RenderOpportunities(IEnumerable{Opportunity})"/>
/// and grouped on by
/// <see cref="OptimizationOpportunityRenderer.RenderOpportunitiesByCategory(IEnumerable{Opportunity})"/>.
/// </summary>
public enum OpportunityCategory
{
    /// <summary>
    /// A read of a record field — an open-record, non-monomorphic record
    /// access that the lowering stage is expected to specialize away.
    /// Description is the field name (e.g. <c>"name"</c>).
    /// </summary>
    RecordAccess,

    /// <summary>
    /// A record-update expression — likewise expected to be specialized
    /// to a closed-record / tuple update. Description is the field name.
    /// </summary>
    RecordUpdate,

    /// <summary>
    /// Generic <c>Basics</c> arithmetic over <c>number</c> (e.g.
    /// <c>add</c> / <c>sub</c> / <c>(+)</c> / <c>(-)</c>).
    /// </summary>
    BasicsArithmetic,

    /// <summary>
    /// Generic <c>Basics</c> comparison over <c>comparable</c> (e.g.
    /// <c>compare</c> / <c>lt</c> / <c>(&lt;)</c>).
    /// </summary>
    BasicsCompare,

    /// <summary>
    /// Generic <c>Basics</c> equality (e.g. <c>eq</c> / <c>neq</c> /
    /// <c>(==)</c> / <c>(/=)</c>).
    /// </summary>
    BasicsEq,

    /// <summary>
    /// Generic <c>Basics</c> append over <c>appendable</c> (e.g.
    /// <c>append</c> / <c>(++)</c>).
    /// </summary>
    BasicsAppend,

    /// <summary>
    /// A site that supplies fewer arguments than the head's arity to a
    /// statically known function or binary operator, forcing the runtime
    /// to allocate a closure for the remaining arguments.
    /// </summary>
    PartialApplication,

    /// <summary>
    /// A function parameter (top-level or let-bound) that is itself
    /// applied as the head of an <see cref="SyntaxTypes.Expression.Application"/>
    /// inside its enclosing function body. The lowering stage is expected
    /// to specialize the function on each statically known higher-order
    /// argument so no application on a function-typed parameter remains.
    /// </summary>
    HigherOrderParameter_Direct,

    /// <summary>
    /// A function parameter (top-level) that is not itself applied inside
    /// the containing decl, but is forwarded as an argument to another
    /// top-level function whose corresponding parameter is reported as
    /// <see cref="HigherOrderParameter_Direct"/> (or, transitively, as
    /// <see cref="HigherOrderParameter_Indirect"/>). The description
    /// carries a <c>"distance N"</c> suffix where <c>N = 1</c> when the
    /// callee is directly higher-order, and <c>N = k + 1</c> when the
    /// callee is itself indirect with distance <c>k</c>. The category
    /// exists primarily to instrument and debug the limitations of the
    /// data-flow analysis driving cross-decl specialization: every
    /// indirect finding is an opportunity the specializer must be able
    /// to resolve before all higher-order parameters can be monomorphized
    /// away.
    /// </summary>
    HigherOrderParameter_Indirect,

    /// <summary>
    /// A root-level wrapping of either a top-level function parameter or
    /// the top-level function return value in a single-tag (one-constructor)
    /// custom type. The lowering stage is expected to strip the wrapping
    /// constructor at the root, replacing the wrapped type with the
    /// constructor's argument type (or a tuple of the constructor's
    /// argument types when there is more than one).
    /// <para>
    /// "Root" means strictly at the top of a parameter type or the
    /// outermost return type; nested occurrences inside other type
    /// constructors are intentionally not reported because removing
    /// them would require additional machinery (specialized type
    /// declarations, mapping helpers) we do not pursue at this stage.
    /// </para>
    /// <para>
    /// Description format:
    /// <c>"parameter[&lt;i&gt;] &lt;name&gt;: &lt;CtorFullName&gt; -&gt; &lt;UnwrappedType&gt;"</c>
    /// for parameters and
    /// <c>"return: &lt;CtorFullName&gt; -&gt; &lt;UnwrappedType&gt;"</c>
    /// for the return value. <c>UnwrappedType</c> is the constructor's
    /// single argument type for 1-arg constructors, or a tuple
    /// <c>(T1, T2, ...)</c> of argument types for N-arg constructors.
    /// Generic type variables in the constructor's argument types are
    /// substituted with the actual type arguments of the wrapped type
    /// when the evidence source is a type annotation.
    /// </para>
    /// </summary>
    RootLevelChoiceTagWrapper,
}

/// <summary>
/// A single opportunity to improve runtime efficiency: a use of a generic
/// operation in <see cref="ContainingDecl"/> that the Elm compiler is
/// expected to specialize away (per the runtime-efficiency guide). The
/// <see cref="Category"/> property identifies the kind of opportunity and
/// <see cref="Description"/> is the category-specific detail (e.g. a field
/// name, an operator symbol, a partial-application <c>"name(added/arity)"</c>
/// shape, or a parameter name).
/// </summary>
public sealed record Opportunity(
    DeclQualifiedName ContainingDecl,
    OpportunityCategory Category,
    string Description)
    : System.IComparable<Opportunity>
{
    /// <summary>
    /// Type information available at the finding site. This is optional because some
    /// opportunities describe declarations or escaped function values rather than typed operands.
    /// It is excluded from opportunity identity so adding diagnostics does not change counts.
    /// </summary>
    public OpportunityTypeEvidence? TypeEvidence { get; init; }

    /// <inheritdoc/>
    public bool Equals(Opportunity? other) =>
        other is not null &&
        ContainingDecl.Equals(other.ContainingDecl) &&
        Category == other.Category &&
        Description == other.Description;

    /// <inheritdoc/>
    public override int GetHashCode() =>
        System.HashCode.Combine(ContainingDecl, Category, Description);

    /// <inheritdoc/>
    public int CompareTo(Opportunity? other)
    {
        if (ReferenceEquals(this, other))
            return 0;

        if (other is null)
            return 1;

        var declCompare = ContainingDecl.CompareTo(other.ContainingDecl);

        if (declCompare is not 0)
            return declCompare;

        var categoryCompare = ((int)Category).CompareTo((int)other.Category);

        if (categoryCompare is not 0)
            return categoryCompare;

        return string.Compare(Description, other.Description, System.StringComparison.Ordinal);
    }
}

/// <summary>
/// Optional inferred types associated with an <see cref="Opportunity"/>.
/// </summary>
/// <param name="SubjectType">
/// Type of the subject being operated on, such as the record in a record access or update.
/// </param>
/// <param name="ArgumentTypes">
/// Types of arguments or operands in source order for a generic function or operator application.
/// </param>
public sealed record OpportunityTypeEvidence(
    TypeInference.InferredType? SubjectType = null,
    IReadOnlyList<TypeInference.InferredType>? ArgumentTypes = null);

/// <summary>
/// Static analysis used in tests to verify that the Elm compiler has lowered
/// or specialized away the generic operations described in
/// <c>guide/optimizing-for-runtime-efficiency-in-elm-programs.md</c>.
///
/// <para>
/// The finder walks every function declaration in a flat declaration
/// dictionary (the same shape used by the lowering stage of the Elm
/// compiler) and reports each occurrence of a generic operation that a
/// fully optimizing compiler is expected to remove. The currently detected
/// categories are enumerated by <see cref="OpportunityCategory"/>.
/// </para>
///
/// <para>
/// Filtering by category or by containing declaration is the caller's
/// responsibility: each <see cref="Opportunity"/> exposes a structured
/// <see cref="Opportunity.Category"/> and
/// <see cref="Opportunity.ContainingDecl"/> so consumers can apply
/// any predicate they need with a LINQ <c>Where</c>.
/// </para>
/// </summary>
public static class OptimizationOpportunityFinder
{
    private sealed record ExpressionTypeContext(
        ElmSyntax.SyntaxModel.QualifiedNameRef CurrentFunctionName,
        string CurrentModuleName,
        ImmutableDictionary<string, int> ParameterNames,
        ImmutableDictionary<string, TypeInference.InferredType> ParameterTypes,
        ImmutableDictionary<string, TypeInference.InferredType> LocalBindingTypes,
        ImmutableDictionary<string, SyntaxTypes.Expression> LocalBindingExpressions,
        IReadOnlyDictionary<ElmSyntax.SyntaxModel.QualifiedNameRef, FunctionTypeInfo> FunctionTypes,
        IReadOnlyDictionary<
            ElmSyntax.SyntaxModel.QualifiedNameRef,
            IReadOnlyList<TypeInference.InferredType>> ConstructorArgumentTypes,
        IReadOnlyDictionary<
            ElmSyntax.SyntaxModel.QualifiedNameRef,
            TypeInference.TypeAliasDefinition> AliasTypes,
        ISet<SyntaxTypes.Expression.RecordAccessFunction> ClosedRecordAccessFunctions);

    private sealed record WholeProgramTypeInference(
        ImmutableDictionary<ElmSyntax.SyntaxModel.QualifiedNameRef, FunctionTypeInfo> FunctionTypes,
        ISet<SyntaxTypes.Expression.RecordAccessFunction> ClosedRecordAccessFunctions);

    /// <summary>
    /// Mapping from <c>Basics</c> function name (as used in
    /// <see cref="SyntaxTypes.Expression.Identifier"/>) to the
    /// optimization category it belongs to.
    /// </summary>
    private static readonly ImmutableDictionary<string, OpportunityCategory> s_basicsFunctionToCategory =
        ImmutableDictionary<string, OpportunityCategory>.Empty
        .Add("add", OpportunityCategory.BasicsArithmetic)
        .Add("sub", OpportunityCategory.BasicsArithmetic)
        .Add("mul", OpportunityCategory.BasicsArithmetic)
        .Add("pow", OpportunityCategory.BasicsArithmetic)
        .Add("negate", OpportunityCategory.BasicsArithmetic)
        .Add("compare", OpportunityCategory.BasicsCompare)
        .Add("lt", OpportunityCategory.BasicsCompare)
        .Add("gt", OpportunityCategory.BasicsCompare)
        .Add("le", OpportunityCategory.BasicsCompare)
        .Add("ge", OpportunityCategory.BasicsCompare)
        .Add("min", OpportunityCategory.BasicsCompare)
        .Add("max", OpportunityCategory.BasicsCompare)
        .Add("eq", OpportunityCategory.BasicsEq)
        .Add("neq", OpportunityCategory.BasicsEq)
        .Add("append", OpportunityCategory.BasicsAppend);

    /// <summary>
    /// Mapping from infix operator symbol (as it appears in
    /// <see cref="SyntaxTypes.Expression.OperatorApplication"/> or
    /// <see cref="SyntaxTypes.Expression.PrefixOperator"/>) to its
    /// optimization category. Operators that are already monomorphic at
    /// the source level (for example <c>//</c> on <c>Int</c> or <c>/</c>
    /// on <c>Float</c>) are intentionally absent.
    /// </summary>
    private static readonly ImmutableDictionary<string, OpportunityCategory> s_basicsOperatorToCategory =
        ImmutableDictionary<string, OpportunityCategory>.Empty
        .Add("+", OpportunityCategory.BasicsArithmetic)
        .Add("-", OpportunityCategory.BasicsArithmetic)
        .Add("*", OpportunityCategory.BasicsArithmetic)
        .Add("^", OpportunityCategory.BasicsArithmetic)
        .Add("<", OpportunityCategory.BasicsCompare)
        .Add(">", OpportunityCategory.BasicsCompare)
        .Add("<=", OpportunityCategory.BasicsCompare)
        .Add(">=", OpportunityCategory.BasicsCompare)
        .Add("==", OpportunityCategory.BasicsEq)
        .Add("/=", OpportunityCategory.BasicsEq)
        .Add("++", OpportunityCategory.BasicsAppend);

    /// <summary>
    /// Walks every function declaration in <paramref name="declarations"/>
    /// and returns every occurrence of a generic operation that the Elm
    /// compiler is expected to specialize away, attributed to the
    /// containing top-level declaration.
    ///
    /// <para>
    /// Filtering by category or by containing declaration is intentionally
    /// not built into the finder: each <see cref="Opportunity"/> carries a
    /// structured <see cref="Opportunity.Category"/> and
    /// <see cref="Opportunity.ContainingDecl"/>, so callers needing a
    /// narrower view should apply LINQ filters on the returned set.
    /// </para>
    /// </summary>
    public static ImmutableHashSet<Opportunity> FindOptimizationOpportunities(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        IReadOnlyCollection<DeclQualifiedName>? restrictToReachableFromEntryPoints = null)
    {
        // Build the top-level arity map once. Only function declarations
        // contribute — type aliases / custom types / ports / infix
        // declarations do not have a callable arity in this analysis.
        var topLevelArity = new Dictionary<DeclQualifiedName, int>();

        foreach (var (qualifiedName, declaration) in declarations)
        {
            if (declaration is SyntaxTypes.Declaration.FunctionDeclaration fd)
            {
                topLevelArity[qualifiedName] =
                    fd.Function.Declaration.Arguments.Count;
            }
        }

        var functionSignaturesBuilder =
            ImmutableDictionary.CreateBuilder<string, TypeInference.InferredType>();

        foreach (var (qualifiedName, declaration) in declarations)
        {
            TypeInference.CollectFunctionSignaturesFromDeclaration(
                declaration,
                string.Join(".", qualifiedName.Namespaces),
                functionSignaturesBuilder);
        }

        var aliasTypes = BuildAliasTypes(declarations);

        var functionSignatures =
            functionSignaturesBuilder
            .ToImmutable()
            .ToImmutableDictionary(
                entry => entry.Key,
                entry =>
                {
                    var qualifiedName =
                        QualifiedNameHelper.FromQualifiedNameString(entry.Key);

                    return
                        TypeInference.ExpandTypeAliases(
                            entry.Value,
                            aliasTypes,
                            qualifiedName.ModuleName);
                });

        var wholeProgramTypeInference =
            InferFunctionTypesFromApplications(
                declarations,
                functionSignatures,
                aliasTypes);

        var functionTypes = wholeProgramTypeInference.FunctionTypes;

        var constructorArgumentTypes =
            BuildConstructorArgumentTypes(functionTypes);

        // Build the single-tag custom-type registry once. Indexed by both
        // the type's qualified name and the (sole) constructor's qualified
        // name so detection sites can look up via either direction. Only
        // custom types that have exactly one constructor are recorded
        // here — multi-constructor types are intentionally absent.
        var singleTagRegistry = BuildSingleTagRegistry(declarations);

        // When a reachability filter is supplied, compute the transitive
        // closure of declarations reachable from the entry points by
        // following every `FunctionOrValue` reference and every
        // partial-application head. The walk is purely syntactic — it
        // does not attempt to detect dead branches eliminated by the
        // optimizer. Declarations not in the closure are skipped entirely
        // so monomorphization-style follow-up tests can assert
        // "no remaining HO parameters / partial applications **reachable
        // from this entry point**", which is the property D2 is expected
        // to drive to zero even when generic originals (e.g. publicly
        // exposed `Maybe.map`) survive in the dictionary because of
        // unrelated callers.
        IReadOnlySet<DeclQualifiedName>? reachableSet = null;

        if (restrictToReachableFromEntryPoints is not null)
        {
            reachableSet =
                ComputeReachableDeclarations(
                    declarations,
                    restrictToReachableFromEntryPoints);
        }

        var resultBuilder = ImmutableHashSet.CreateBuilder<Opportunity>();

        foreach (var (qualifiedName, declaration) in declarations)
        {
            if (declaration is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                continue;

            if (reachableSet is not null && !reachableSet.Contains(qualifiedName))
                continue;

            // The top-level function's own parameters introduce names that
            // may be applied as application heads inside the body. Collect
            // them up front so the body walker can flag every site.
            var topLevelParamNames =
                SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPatterns(funcDecl.Function.Declaration.Arguments);

            var parameterNamesBuilder = ImmutableDictionary.CreateBuilder<string, int>();

            for (var argumentIndex = 0;
                argumentIndex < funcDecl.Function.Declaration.Arguments.Count;
                argumentIndex++)
            {
                foreach (var name in
                    SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPattern(
                        funcDecl.Function.Declaration.Arguments[argumentIndex]))
                {
                    parameterNamesBuilder[name] = argumentIndex;
                }
            }

            var inferredFunctionType =
                TypeInference.InferFunctionDeclarationType(
                    funcDecl.Function.Declaration.Expression,
                    funcDecl.Function.Declaration.Arguments,
                    string.Join(".", qualifiedName.Namespaces),
                    functionSignatures);

            var parameterTypes = inferredFunctionType.parameterTypes;

            var qualifiedNameRef =
                QualifiedNameHelper.FromQualifiedNameString(qualifiedName.FullName);

            var inferredParameterTypes =
                functionTypes.TryGetValue(qualifiedNameRef, out var functionTypeInfo)
                ?
                functionTypeInfo.ParameterTypes
                :
                [];

            for (var argumentIndex = 0;
                argumentIndex < inferredParameterTypes.Count &&
                argumentIndex < funcDecl.Function.Declaration.Arguments.Count;
                argumentIndex++)
            {
                parameterTypes =
                    TypeInference.ExtractPatternBindingTypesFromInferred(
                        funcDecl.Function.Declaration.Arguments[argumentIndex],
                        inferredParameterTypes[argumentIndex],
                        parameterTypes,
                        constructorArgumentTypes);
            }

            var expressionTypeContext =
                new ExpressionTypeContext(
                    CurrentFunctionName: qualifiedNameRef,
                    CurrentModuleName: string.Join(".", qualifiedName.Namespaces),
                    ParameterNames: parameterNamesBuilder.ToImmutable(),
                    ParameterTypes: parameterTypes,
                    LocalBindingTypes: [],
                    LocalBindingExpressions: [],
                    FunctionTypes: functionTypes,
                    ConstructorArgumentTypes: constructorArgumentTypes,
                    AliasTypes: aliasTypes,
                    ClosedRecordAccessFunctions:
                    wholeProgramTypeInference.ClosedRecordAccessFunctions);

            CollectFromExpression(
                funcDecl.Function.Declaration.Expression,
                qualifiedName,
                topLevelArity,
                [],
                topLevelParamNames,
                expressionTypeContext,
                resultBuilder);

            // Higher-order parameter detection for the top-level function:
            // a parameter that is the head of an application anywhere in the
            // body is reported once per (decl, parameter name).
            CollectHigherOrderParameterFindings(
                funcDecl.Function.Declaration.Expression,
                topLevelParamNames,
                qualifiedName,
                paramOwnerDescription: null,
                resultBuilder);

            // Root-level single-tag-wrapper detection for top-level
            // parameters and the outermost return value of the function.
            CollectRootLevelChoiceTagWrapperFindings(
                qualifiedName,
                funcDecl.Function,
                singleTagRegistry,
                resultBuilder);
        }

        // Cross-decl indirect-higher-order-parameter detection: a
        // top-level parameter that is forwarded to a callee whose
        // corresponding parameter is itself higher-order (directly or
        // transitively) is reported with a distance counter. Direct
        // findings emitted above act as the distance-0 seeds; this
        // pass converts forwarding edges into HigherOrderParameter_Indirect
        // findings with distance N >= 1.
        CollectIndirectHigherOrderParameterFindings(
            declarations,
            reachableSet,
            resultBuilder);

        return resultBuilder.ToImmutable();
    }

    private static ImmutableDictionary<ElmSyntax.SyntaxModel.QualifiedNameRef, TypeInference.TypeAliasDefinition>
        BuildAliasTypes(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var result =
            ImmutableDictionary.CreateBuilder<
                ElmSyntax.SyntaxModel.QualifiedNameRef,
                TypeInference.TypeAliasDefinition>();

        foreach (var (qualifiedName, declaration) in declarations)
        {
            if (declaration is SyntaxTypes.Declaration.AliasDeclaration aliasDeclaration)
            {
                result[
                    QualifiedNameHelper.ToQualifiedNameRef(
                        qualifiedName.Namespaces,
                        aliasDeclaration.TypeAlias.Name)] =
                    new TypeInference.TypeAliasDefinition(
                        aliasDeclaration.TypeAlias.Generics,
                        TypeInference.TypeAnnotationToInferredType(
                            aliasDeclaration.TypeAlias.TypeAnnotation));
            }
        }

        return result.ToImmutable();
    }

    private static WholeProgramTypeInference
        InferFunctionTypesFromApplications(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        IReadOnlyDictionary<string, TypeInference.InferredType> functionSignatures,
        IReadOnlyDictionary<
            ElmSyntax.SyntaxModel.QualifiedNameRef,
            TypeInference.TypeAliasDefinition> aliasTypes)
    {
        var closedRecordAccessFunctions =
            new HashSet<SyntaxTypes.Expression.RecordAccessFunction>(
                System.Collections.Generic.ReferenceEqualityComparer.Instance);

        var functionTypes =
            ImmutableDictionary.CreateBuilder<
                ElmSyntax.SyntaxModel.QualifiedNameRef,
                FunctionTypeInfo>();

        var declarationsByName =
            declarations
            .Where(entry => entry.Value is SyntaxTypes.Declaration.FunctionDeclaration)
            .ToImmutableDictionary(
                entry => QualifiedNameHelper.FromQualifiedNameString(entry.Key.FullName),
                entry => (SyntaxTypes.Declaration.FunctionDeclaration)entry.Value);

        foreach (var (qualifiedName, declaration) in declarationsByName)
        {
            if (functionSignatures.TryGetValue(
                QualifiedNameFullName(qualifiedName),
                out var annotatedFunctionType))
            {
                functionTypes[qualifiedName] =
                    new FunctionTypeInfo(
                        GetFunctionReturnType(annotatedFunctionType),
                        TypeInference.ExtractArgumentTypesFromFunctionType(annotatedFunctionType));

                continue;
            }

            var inferred =
                TypeInference.InferFunctionDeclarationType(
                    declaration.Function.Declaration.Expression,
                    declaration.Function.Declaration.Arguments,
                    string.Join(".", qualifiedName.ModuleName),
                    functionSignatures);

            var parameterTypes = new List<TypeInference.InferredType>();

            foreach (var argument in declaration.Function.Declaration.Arguments)
            {
                if (argument is SyntaxTypes.Pattern.VarPattern varPattern &&
                    inferred.parameterTypes.TryGetValue(varPattern.Name, out var parameterType))
                {
                    parameterTypes.Add(
                        TypeInference.ExpandTypeAliases(
                            parameterType,
                            aliasTypes,
                            qualifiedName.ModuleName));
                }
                else
                {
                    parameterTypes.Add(new TypeInference.InferredType.UnknownType());
                }
            }

            functionTypes[qualifiedName] =
                new FunctionTypeInfo(
                    TypeInference.ExpandTypeAliases(
                        inferred.returnType,
                        aliasTypes,
                        qualifiedName.ModuleName),
                    parameterTypes);
        }

        foreach (var (qualifiedNameString, functionSignature) in functionSignatures)
        {
            var qualifiedName =
                QualifiedNameHelper.FromQualifiedNameString(qualifiedNameString);

            if (!functionTypes.ContainsKey(qualifiedName))
            {
                functionTypes[qualifiedName] =
                    new FunctionTypeInfo(
                        GetFunctionReturnType(functionSignature),
                        TypeInference.ExtractArgumentTypesFromFunctionType(functionSignature));
            }
        }

        var inferredFunctionNames =
            declarationsByName.Keys
            .Where(qualifiedName => !functionSignatures.ContainsKey(QualifiedNameFullName(qualifiedName)))
            .ToImmutableHashSet();

        for (var iteration = 0; iteration < declarationsByName.Count; iteration++)
        {
            var snapshot = functionTypes.ToImmutable();

            var constructorArgumentTypes =
                BuildConstructorArgumentTypes(snapshot);

            var suggestions =
                new Dictionary<
                    (ElmSyntax.SyntaxModel.QualifiedNameRef FunctionName, int ParameterIndex),
                    TypeInference.InferredType>();

            foreach (var (qualifiedName, declaration) in declarationsByName)
            {
                var implementation = declaration.Function.Declaration;
                var parameterNames = ImmutableDictionary.CreateBuilder<string, int>();
                var parameterTypes = ImmutableDictionary<string, TypeInference.InferredType>.Empty;

                for (var argumentIndex = 0;
                    argumentIndex < implementation.Arguments.Count;
                    argumentIndex++)
                {
                    foreach (var parameterName in
                        SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPattern(
                            implementation.Arguments[argumentIndex]))
                    {
                        parameterNames[parameterName] = argumentIndex;
                    }

                    if (snapshot.TryGetValue(qualifiedName, out var containingFunctionType) &&
                        argumentIndex < containingFunctionType.ParameterTypes.Count)
                    {
                        parameterTypes =
                            TypeInference.ExtractPatternBindingTypesFromInferred(
                                implementation.Arguments[argumentIndex],
                                containingFunctionType.ParameterTypes[argumentIndex],
                                parameterTypes,
                                constructorArgumentTypes);
                    }
                }

                var context =
                    new ExpressionTypeContext(
                        CurrentFunctionName: qualifiedName,
                        CurrentModuleName: string.Join(".", qualifiedName.ModuleName),
                        ParameterNames: parameterNames.ToImmutable(),
                        ParameterTypes: parameterTypes,
                        LocalBindingTypes: [],
                        LocalBindingExpressions: [],
                        FunctionTypes: snapshot,
                        ConstructorArgumentTypes: constructorArgumentTypes,
                        AliasTypes: aliasTypes,
                        ClosedRecordAccessFunctions: closedRecordAccessFunctions);

                CollectFunctionTypeSuggestions(
                    implementation.Expression,
                    context,
                    inferredFunctionNames,
                    suggestions);

                if (inferredFunctionNames.Contains(qualifiedName))
                {
                    var inferredReturnType =
                        InferExpressionType(implementation.Expression, context);

                    if (inferredReturnType is not TypeInference.InferredType.UnknownType)
                        suggestions[(qualifiedName, -1)] = inferredReturnType;
                }
            }

            var changed = false;

            foreach (var ((functionName, parameterIndex), suggestedType) in suggestions)
            {
                if (!functionTypes.TryGetValue(functionName, out var functionType))
                {
                    continue;
                }

                var currentType =
                    parameterIndex is -1
                    ?
                    functionType.ReturnType
                    :
                    parameterIndex < functionType.ParameterTypes.Count
                    ?
                    functionType.ParameterTypes[parameterIndex]
                    :
                    null;

                if (currentType is null ||
                    TypeInference.TryUnify(currentType, suggestedType) is not
                    Result<string, TypeInference.InferredType>.Ok unified ||
                    InferredTypesEquivalent(unified.Value, currentType))
                {
                    continue;
                }

                if (parameterIndex is -1)
                {
                    functionTypes[functionName] =
                        functionType with
                        {
                            ReturnType = unified.Value
                        };

                    changed = true;
                    continue;
                }

                var updatedParameterTypes = functionType.ParameterTypes.ToArray();
                updatedParameterTypes[parameterIndex] = unified.Value;

                functionTypes[functionName] =
                    functionType with
                    {
                        ParameterTypes = updatedParameterTypes
                    };

                changed = true;
            }

            if (!changed)
                break;
        }

        return
            new WholeProgramTypeInference(
                functionTypes.ToImmutable(),
                closedRecordAccessFunctions);
    }

    private static string QualifiedNameFullName(
        ElmSyntax.SyntaxModel.QualifiedNameRef qualifiedName) =>
        string.Join(".", qualifiedName.ModuleName.Append(qualifiedName.Name));

    private static ImmutableDictionary<
        ElmSyntax.SyntaxModel.QualifiedNameRef,
        IReadOnlyList<TypeInference.InferredType>> BuildConstructorArgumentTypes(
        IReadOnlyDictionary<ElmSyntax.SyntaxModel.QualifiedNameRef, FunctionTypeInfo> functionTypes) =>
        functionTypes
        .Where(entry => ElmValueEncoding.StringIsValidTagName(entry.Key.Name))
        .ToImmutableDictionary(
            entry => entry.Key,
            entry => entry.Value.ParameterTypes);

    private static bool InferredTypesEquivalent(
        TypeInference.InferredType left,
        TypeInference.InferredType right)
    {
        switch (left, right)

        {
            case (TypeInference.InferredType.ListType leftList,
                TypeInference.InferredType.ListType rightList):
                return InferredTypesEquivalent(leftList.ElementType, rightList.ElementType);

            case (TypeInference.InferredType.TupleType leftTuple,
                TypeInference.InferredType.TupleType rightTuple):
                return
                    leftTuple.ElementTypes.Count == rightTuple.ElementTypes.Count &&
                    leftTuple.ElementTypes
                    .Zip(rightTuple.ElementTypes)
                    .All(pair => InferredTypesEquivalent(pair.First, pair.Second));

            case (TypeInference.InferredType.FunctionType leftFunction,
                TypeInference.InferredType.FunctionType rightFunction):
                return
                    InferredTypesEquivalent(leftFunction.ArgumentType, rightFunction.ArgumentType) &&
                    InferredTypesEquivalent(leftFunction.ReturnType, rightFunction.ReturnType);

            case (TypeInference.InferredType.ChoiceType leftChoice,
                TypeInference.InferredType.ChoiceType rightChoice):
                return
                    leftChoice.ModuleName.SequenceEqual(rightChoice.ModuleName) &&
                    leftChoice.TypeName == rightChoice.TypeName &&
                    leftChoice.TypeArguments.Count == rightChoice.TypeArguments.Count &&
                    leftChoice.TypeArguments
                    .Zip(rightChoice.TypeArguments)
                    .All(pair => InferredTypesEquivalent(pair.First, pair.Second));

            case (TypeInference.InferredType.RecordType leftRecord,
                TypeInference.InferredType.RecordType rightRecord):
                return
                    leftRecord.Fields.Count == rightRecord.Fields.Count &&
                    leftRecord.Fields
                    .Zip(rightRecord.Fields)
                    .All(
                        pair =>
                        pair.First.FieldName == pair.Second.FieldName &&
                        InferredTypesEquivalent(
                            pair.First.FieldType,
                            pair.Second.FieldType));

            case (TypeInference.InferredType.OpenRecordType leftRecord,
                TypeInference.InferredType.OpenRecordType rightRecord):
                return
                    leftRecord.ExtensionVariable == rightRecord.ExtensionVariable &&
                    leftRecord.KnownFields.Count == rightRecord.KnownFields.Count &&
                    leftRecord.KnownFields
                    .Zip(rightRecord.KnownFields)
                    .All(
                        pair =>
                        pair.First.FieldName == pair.Second.FieldName &&
                        InferredTypesEquivalent(
                            pair.First.FieldType,
                            pair.Second.FieldType));

            case (TypeInference.InferredType.IntType, TypeInference.InferredType.IntType):
            case (TypeInference.InferredType.FloatType, TypeInference.InferredType.FloatType):
            case (TypeInference.InferredType.StringType, TypeInference.InferredType.StringType):
            case (TypeInference.InferredType.CharType, TypeInference.InferredType.CharType):
            case (TypeInference.InferredType.BoolType, TypeInference.InferredType.BoolType):
            case (TypeInference.InferredType.NumberType, TypeInference.InferredType.NumberType):
            case (TypeInference.InferredType.UnknownType, TypeInference.InferredType.UnknownType):
                return true;

            case (TypeInference.InferredType.TypeVariable leftVariable,
                TypeInference.InferredType.TypeVariable rightVariable):
                return
                    leftVariable.Name == rightVariable.Name &&
                    leftVariable.Constraint == rightVariable.Constraint;

            case (TypeInference.InferredType.IntType, _):
            case (TypeInference.InferredType.FloatType, _):
            case (TypeInference.InferredType.StringType, _):
            case (TypeInference.InferredType.CharType, _):
            case (TypeInference.InferredType.BoolType, _):
            case (TypeInference.InferredType.NumberType, _):
            case (TypeInference.InferredType.TupleType, _):
            case (TypeInference.InferredType.RecordType, _):
            case (TypeInference.InferredType.OpenRecordType, _):
            case (TypeInference.InferredType.FunctionType, _):
            case (TypeInference.InferredType.ListType, _):
            case (TypeInference.InferredType.ChoiceType, _):
            case (TypeInference.InferredType.TypeVariable, _):
            case (TypeInference.InferredType.UnknownType, _):
                return false;

            default:
                throw new System.NotImplementedException(
                    "InferredTypesEquivalent does not handle inferred type variants: " +
                    left.GetType().Name + " and " + right.GetType().Name);
        }
    }

    private static TypeInference.InferredType GetFunctionReturnType(
        TypeInference.InferredType functionType)
    {
        while (functionType is TypeInference.InferredType.FunctionType next)
            functionType = next.ReturnType;

        return functionType;
    }

    private static void CollectFunctionTypeSuggestions(
        SyntaxTypes.Expression expression,
        ExpressionTypeContext context,
        IReadOnlySet<ElmSyntax.SyntaxModel.QualifiedNameRef> inferredFunctionNames,
        Dictionary<
            (ElmSyntax.SyntaxModel.QualifiedNameRef FunctionName, int ParameterIndex),
            TypeInference.InferredType> suggestions)
    {
        switch (expression)
        {
            case SyntaxTypes.Expression.Application application:
                var actualArgumentTypes =
                    application.Arguments
                    .Select(argument => InferExpressionType(argument, context))
                    .ToList();

                if (application.Function is SyntaxTypes.Expression.Identifier
                    {
                        QualifiedName.Namespaces.Count: 0
                    } parameterIdentifier &&
                    context.ParameterNames.TryGetValue(
                        parameterIdentifier.QualifiedName.DeclName,
                        out var parameterIndex))
                {
                    context.ParameterTypes.TryGetValue(
                        parameterIdentifier.QualifiedName.DeclName,
                        out var existingParameterType);

                    SuggestFunctionParameterType(
                        context.CurrentFunctionName,
                        parameterIndex,
                        ConstrainFunctionArguments(
                            existingParameterType ??
                            new TypeInference.InferredType.UnknownType(),
                            actualArgumentTypes),
                        inferredFunctionNames,
                        suggestions);
                }

                if (application.Function is SyntaxTypes.Expression.Identifier
                    {
                        QualifiedName.Namespaces: ["Basics"],
                        QualifiedName.DeclName: "apR" or "apL"
                    } pipeIdentifier &&
                    application.Arguments.Count is 2)
                {
                    var valueArgumentIndex =
                        pipeIdentifier.QualifiedName.DeclName is "apR" ? 0 : 1;

                    var functionArgumentIndex = 1 - valueArgumentIndex;

                    SuggestExpectedFunctionType(
                        application.Arguments[functionArgumentIndex],
                        new TypeInference.InferredType.FunctionType(
                            actualArgumentTypes[valueArgumentIndex],
                            new TypeInference.InferredType.UnknownType()),
                        context,
                        inferredFunctionNames,
                        suggestions);
                }

                if (application.Function is SyntaxTypes.Expression.Identifier appliedIdentifier &&
                    TryResolveFunction(
                        appliedIdentifier,
                        context,
                        out var appliedFunctionName,
                        out var appliedFunctionType))
                {
                    for (var argumentIndex = 0;
                        argumentIndex < actualArgumentTypes.Count &&
                        argumentIndex < appliedFunctionType.ParameterTypes.Count;
                        argumentIndex++)
                    {
                        SuggestFunctionParameterType(
                            appliedFunctionName,
                            argumentIndex,
                            actualArgumentTypes[argumentIndex],
                            inferredFunctionNames,
                            suggestions);
                    }

                    var specializedParameterTypes =
                        TypeInference.SpecializeTypesFromArguments(
                            appliedFunctionType.ParameterTypes,
                            actualArgumentTypes);

                    for (var argumentIndex = 0;
                        argumentIndex < application.Arguments.Count &&
                        argumentIndex < specializedParameterTypes.Count;
                        argumentIndex++)
                    {
                        SuggestExpectedFunctionType(
                            application.Arguments[argumentIndex],
                            specializedParameterTypes[argumentIndex],
                            context,
                            inferredFunctionNames,
                            suggestions);
                    }
                }

                CollectFunctionTypeSuggestions(
                    application.Function,
                    context,
                    inferredFunctionNames,
                    suggestions);

                foreach (var argument in application.Arguments)
                {
                    CollectFunctionTypeSuggestions(
                        argument,
                        context,
                        inferredFunctionNames,
                        suggestions);
                }

                break;

            case SyntaxTypes.Expression.LetExpression letExpression:
                var extendedContext =
                    context with
                    {
                        LocalBindingTypes =
                        InferLetExpressionLocalBindingTypes(
                            letExpression,
                            context),
                        LocalBindingExpressions =
                        ExtendLocalBindingExpressions(
                            letExpression,
                            context.LocalBindingExpressions)
                    };

                foreach (var declaration in letExpression.Declarations)
                {
                    switch (declaration)
                    {
                        case SyntaxTypes.LetDeclaration.LetFunction letFunction:
                            CollectFunctionTypeSuggestions(
                                letFunction.Function.Declaration.Expression,
                                extendedContext,
                                inferredFunctionNames,
                                suggestions);

                            break;

                        case SyntaxTypes.LetDeclaration.LetDestructuring letDestructuring:
                            CollectFunctionTypeSuggestions(
                                letDestructuring.Expression,
                                extendedContext,
                                inferredFunctionNames,
                                suggestions);

                            break;

                        default:
                            throw new System.NotImplementedException(
                                "CollectFunctionTypeSuggestions does not handle let declaration variant: " +
                                declaration.GetType().Name);
                    }
                }

                CollectFunctionTypeSuggestions(
                    letExpression.Expression,
                    extendedContext,
                    inferredFunctionNames,
                    suggestions);

                break;

            case SyntaxTypes.Expression.OperatorApplication operatorApplication:
                CollectFunctionTypeSuggestions(
                    operatorApplication.Left,
                    context,
                    inferredFunctionNames,
                    suggestions);

                CollectFunctionTypeSuggestions(
                    operatorApplication.Right,
                    context,
                    inferredFunctionNames,
                    suggestions);

                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                CollectFunctionTypeSuggestions(
                    ifBlock.Condition,
                    context,
                    inferredFunctionNames,
                    suggestions);

                CollectFunctionTypeSuggestions(
                    ifBlock.ThenBlock,
                    context,
                    inferredFunctionNames,
                    suggestions);

                CollectFunctionTypeSuggestions(
                    ifBlock.ElseBlock,
                    context,
                    inferredFunctionNames,
                    suggestions);

                break;

            case SyntaxTypes.Expression.CaseExpression caseExpression:
                var scrutineeType =
                    InferExpressionType(caseExpression.Expression, context);

                CollectFunctionTypeSuggestions(
                    caseExpression.Expression,
                    context,
                    inferredFunctionNames,
                    suggestions);

                foreach (var caseItem in caseExpression.Cases)
                {
                    var caseContext =
                        context with
                        {
                            LocalBindingTypes =
                            ExtractCasePatternBindingTypes(
                                caseItem.Pattern,
                                scrutineeType,
                                context.LocalBindingTypes,
                                context)
                        };

                    CollectFunctionTypeSuggestions(
                        caseItem.Expression,
                        caseContext,
                        inferredFunctionNames,
                        suggestions);
                }

                break;

            case SyntaxTypes.Expression.LambdaExpression lambdaExpression:
                CollectFunctionTypeSuggestions(
                    lambdaExpression.Expression,
                    context,
                    inferredFunctionNames,
                    suggestions);

                break;

            case SyntaxTypes.Expression.ListExpr listExpression:
                foreach (var element in listExpression.Elements)
                {
                    CollectFunctionTypeSuggestions(
                        element,
                        context,
                        inferredFunctionNames,
                        suggestions);
                }

                break;

            case SyntaxTypes.Expression.TupledExpression tupleExpression:
                foreach (var element in tupleExpression.Elements)
                {
                    CollectFunctionTypeSuggestions(
                        element,
                        context,
                        inferredFunctionNames,
                        suggestions);
                }

                break;

            case SyntaxTypes.Expression.RecordExpr recordExpression:
                foreach (var field in recordExpression.Fields)
                {
                    CollectFunctionTypeSuggestions(
                        field.Value,
                        context,
                        inferredFunctionNames,
                        suggestions);
                }

                break;

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                CollectFunctionTypeSuggestions(
                    recordAccess.Record,
                    context,
                    inferredFunctionNames,
                    suggestions);

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields)
                {
                    CollectFunctionTypeSuggestions(
                        field.Value,
                        context,
                        inferredFunctionNames,
                        suggestions);
                }

                break;

            case SyntaxTypes.Expression.Negation negation:
                CollectFunctionTypeSuggestions(
                    negation.Expression,
                    context,
                    inferredFunctionNames,
                    suggestions);

                break;

            case SyntaxTypes.Expression.Identifier:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.GLSLExpression:
                break;

            default:
                throw new System.NotImplementedException(
                    "CollectFunctionTypeSuggestions does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }

    private static TypeInference.InferredType ConstrainFunctionArguments(
        TypeInference.InferredType functionType,
        IReadOnlyList<TypeInference.InferredType> argumentTypes)
    {
        TypeInference.InferredType ConstrainAt(
            TypeInference.InferredType remainingType,
            int argumentIndex)
        {
            if (argumentIndex >= argumentTypes.Count)
                return remainingType;

            var existingFunctionType =
                remainingType as TypeInference.InferredType.FunctionType;

            var existingArgumentType =
                existingFunctionType?.ArgumentType ??
                new TypeInference.InferredType.UnknownType();

            var constrainedArgumentType =
                TypeInference.TryUnify(
                    existingArgumentType,
                    argumentTypes[argumentIndex]) is
                    Result<string, TypeInference.InferredType>.Ok unified
                ?
                unified.Value
                :
                existingArgumentType;

            var returnType =
                existingFunctionType?.ReturnType ??
                new TypeInference.InferredType.UnknownType();

            return
                new TypeInference.InferredType.FunctionType(
                    constrainedArgumentType,
                    ConstrainAt(returnType, argumentIndex + 1));
        }

        return ConstrainAt(functionType, 0);
    }

    private static void SuggestExpectedFunctionType(
        SyntaxTypes.Expression expression,
        TypeInference.InferredType expectedType,
        ExpressionTypeContext context,
        IReadOnlySet<ElmSyntax.SyntaxModel.QualifiedNameRef> inferredFunctionNames,
        Dictionary<
            (ElmSyntax.SyntaxModel.QualifiedNameRef FunctionName, int ParameterIndex),
            TypeInference.InferredType> suggestions)
    {
        if (expression is SyntaxTypes.Expression.Identifier
            {
                QualifiedName.Namespaces.Count: 0
            } localIdentifier &&
            context.LocalBindingExpressions.TryGetValue(
                localIdentifier.QualifiedName.DeclName,
                out var localBindingExpression) &&
            !ReferenceEquals(expression, localBindingExpression))
        {
            SuggestExpectedFunctionType(
                localBindingExpression,
                expectedType,
                context,
                inferredFunctionNames,
                suggestions);

            return;
        }

        if (expression is SyntaxTypes.Expression.RecordAccessFunction recordAccessFunction &&
            expectedType is TypeInference.InferredType.FunctionType
            {
                ArgumentType: TypeInference.InferredType.RecordType
            })
        {
            context.ClosedRecordAccessFunctions.Add(recordAccessFunction);
            return;
        }

        var functionExpression = expression;
        var alreadyAppliedCount = 0;
        IReadOnlyList<SyntaxTypes.Expression> alreadyAppliedArguments = [];

        if (expression is SyntaxTypes.Expression.Application application)
        {
            functionExpression = application.Function;
            alreadyAppliedCount = application.Arguments.Count;
            alreadyAppliedArguments = application.Arguments;
        }

        if (functionExpression is not SyntaxTypes.Expression.Identifier identifier ||
            !TryResolveFunction(
                identifier,
                context,
                out var functionName,
                out var functionType))
        {
            return;
        }

        var partialResultType = functionType.ReturnType;

        for (var parameterIndex = functionType.ParameterTypes.Count - 1;
            parameterIndex >= alreadyAppliedCount;
            parameterIndex--)
        {
            partialResultType =
                new TypeInference.InferredType.FunctionType(
                    functionType.ParameterTypes[parameterIndex],
                    partialResultType);
        }

        var specializedParameterTypes =
            TypeInference.SpecializeTypesFromMatch(
                partialResultType,
                expectedType,
                functionType.ParameterTypes);

        for (var argumentIndex = 0;
            argumentIndex < alreadyAppliedArguments.Count &&
            argumentIndex < specializedParameterTypes.Count;
            argumentIndex++)
        {
            SuggestExpectedFunctionType(
                alreadyAppliedArguments[argumentIndex],
                specializedParameterTypes[argumentIndex],
                context,
                inferredFunctionNames,
                suggestions);
        }

        var remainingExpectedType = expectedType;

        for (var parameterIndex = alreadyAppliedCount;
            parameterIndex < specializedParameterTypes.Count &&
            remainingExpectedType is TypeInference.InferredType.FunctionType expectedFunctionType;
            parameterIndex++)
        {
            SuggestFunctionParameterType(
                functionName,
                parameterIndex,
                expectedFunctionType.ArgumentType,
                inferredFunctionNames,
                suggestions);

            remainingExpectedType = expectedFunctionType.ReturnType;
        }
    }

    private static bool TryResolveFunction(
        SyntaxTypes.Expression.Identifier identifier,
        ExpressionTypeContext context,
        out ElmSyntax.SyntaxModel.QualifiedNameRef qualifiedName,
        out FunctionTypeInfo functionType)
    {
        qualifiedName =
            identifier.QualifiedName.Namespaces.Count > 0
            ?
            QualifiedNameHelper.ToQualifiedNameRef(
                identifier.QualifiedName.Namespaces,
                identifier.QualifiedName.DeclName)
            :
            QualifiedNameHelper.FromQualifiedNameString(
                context.CurrentModuleName + "." + identifier.QualifiedName.DeclName);

        return context.FunctionTypes.TryGetValue(qualifiedName, out functionType!);
    }

    private static ImmutableDictionary<string, TypeInference.InferredType>
        ExtractCasePatternBindingTypes(
        SyntaxTypes.Pattern pattern,
        TypeInference.InferredType scrutineeType,
        ImmutableDictionary<string, TypeInference.InferredType> existingBindings,
        ExpressionTypeContext context)
    {
        var constructorArgumentTypes =
            context.ConstructorArgumentTypes.ToImmutableDictionary();

        if (pattern is SyntaxTypes.Pattern.NamedPattern namedPattern)
        {
            var constructorName =
                QualifiedNameHelper.ToQualifiedNameRef(
                    namedPattern.Name.ModuleName,
                    namedPattern.Name.Name);

            if (context.FunctionTypes.TryGetValue(constructorName, out var constructorType))
            {
                constructorArgumentTypes =
                    constructorArgumentTypes.SetItem(
                        constructorName,
                        TypeInference.SpecializeTypesFromMatch(
                            constructorType.ReturnType,
                            scrutineeType,
                            constructorType.ParameterTypes));
            }
        }

        return
            TypeInference.ExtractPatternBindingTypesFromInferred(
                pattern,
                scrutineeType,
                existingBindings,
                constructorArgumentTypes);
    }

    private static ImmutableDictionary<string, TypeInference.InferredType>
        InferLetExpressionLocalBindingTypes(
        SyntaxTypes.Expression.LetExpression letExpression,
        ExpressionTypeContext context)
    {
        var localBindingTypes =
            TypeInference.InferLetExpressionLocalBindingTypes(
                letExpression,
                context.ParameterNames,
                context.ParameterTypes,
                context.LocalBindingTypes,
                context.CurrentModuleName,
                context.FunctionTypes);

        foreach (var declaration in letExpression.Declarations)
        {
            switch (declaration)
            {
                case SyntaxTypes.LetDeclaration.LetDestructuring letDestructuring:
                    var expressionType =
                        InferExpressionType(
                            letDestructuring.Expression,
                            context with
                            {
                                LocalBindingTypes = localBindingTypes
                            });

                    localBindingTypes =
                        ExtractCasePatternBindingTypes(
                            letDestructuring.Pattern,
                            expressionType,
                            localBindingTypes,
                            context);

                    break;

                case SyntaxTypes.LetDeclaration.LetFunction:
                    break;

                default:
                    throw new System.NotImplementedException(
                        "InferLetExpressionLocalBindingTypes does not handle let declaration variant: " +
                        declaration.GetType().Name);
            }
        }

        return localBindingTypes;
    }

    private static ImmutableDictionary<string, SyntaxTypes.Expression>
        ExtendLocalBindingExpressions(
        SyntaxTypes.Expression.LetExpression letExpression,
        ImmutableDictionary<string, SyntaxTypes.Expression> localBindingExpressions)
    {
        foreach (var declaration in letExpression.Declarations)
        {
            if (declaration is SyntaxTypes.LetDeclaration.LetFunction letFunction &&
                letFunction.Function.Declaration.Arguments.Count is 0)
            {
                localBindingExpressions =
                    localBindingExpressions.SetItem(
                        letFunction.Function.Declaration.Name,
                        letFunction.Function.Declaration.Expression);
            }
        }

        return localBindingExpressions;
    }

    private static void SuggestFunctionParameterType(
        ElmSyntax.SyntaxModel.QualifiedNameRef functionName,
        int parameterIndex,
        TypeInference.InferredType suggestedType,
        IReadOnlySet<ElmSyntax.SyntaxModel.QualifiedNameRef> inferredFunctionNames,
        Dictionary<
            (ElmSyntax.SyntaxModel.QualifiedNameRef FunctionName, int ParameterIndex),
            TypeInference.InferredType> suggestions)
    {
        if (!inferredFunctionNames.Contains(functionName) ||
            suggestedType is TypeInference.InferredType.UnknownType)
        {
            return;
        }

        var key = (functionName, parameterIndex);

        if (suggestions.TryGetValue(key, out var existingSuggestion))
        {
            if (TypeInference.TryUnify(existingSuggestion, suggestedType) is
                Result<string, TypeInference.InferredType>.Ok unified)
            {
                suggestions[key] = unified.Value;
            }

            return;
        }

        suggestions[key] = suggestedType;
    }

    /// <summary>
    /// Computes the set of <see cref="DeclQualifiedName"/> values reachable
    /// (by syntactic <see cref="SyntaxTypes.Expression.Identifier"/>
    /// reference) from any entry point in <paramref name="entryPoints"/>.
    /// Used by the
    /// <c>restrictToReachableFromEntryPoints</c> overload of
    /// <see cref="FindOptimizationOpportunities(IReadOnlyDictionary{DeclQualifiedName, SyntaxTypes.Declaration}, IReadOnlyCollection{DeclQualifiedName}?)"/>.
    /// <para>
    /// Walks bodies, types, and let-declarations. References to
    /// declarations that are not in <paramref name="declarations"/> (e.g.
    /// natively-implemented <c>Basics</c>) are silently ignored.
    /// </para>
    /// </summary>
    public static IReadOnlySet<DeclQualifiedName> ComputeReachableDeclarations(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        IReadOnlyCollection<DeclQualifiedName> entryPoints)
    {
        // Build forward edge map: caller → set of callees that are present
        // in the declaration dictionary.
        var forwardEdges = new Dictionary<DeclQualifiedName, HashSet<DeclQualifiedName>>();

        foreach (var (caller, callee) in EnumerateReferenceEdges(declarations))
        {
            if (!forwardEdges.TryGetValue(caller, out var callees))
            {
                callees = [];
                forwardEdges[caller] = callees;
            }

            callees.Add(callee);
        }

        var reachable = new HashSet<DeclQualifiedName>();
        var queue = new Queue<DeclQualifiedName>();

        foreach (var entry in entryPoints)
        {
            if (declarations.ContainsKey(entry) && reachable.Add(entry))
            {
                queue.Enqueue(entry);
            }
        }

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();

            if (!forwardEdges.TryGetValue(current, out var callees))
                continue;

            foreach (var callee in callees)
            {
                if (reachable.Add(callee))
                    queue.Enqueue(callee);
            }
        }

        return reachable;
    }

    /// <summary>
    /// Returns the set of declarations in <paramref name="declarations"/>
    /// whose body contains at least one syntactic
    /// <see cref="SyntaxTypes.Expression.Identifier"/> reference that
    /// resolves to <paramref name="target"/>.
    /// <para>
    /// Resolution rules match
    /// <see cref="ComputeReachableDeclarations(IReadOnlyDictionary{DeclQualifiedName, SyntaxTypes.Declaration}, IReadOnlyCollection{DeclQualifiedName})"/>:
    /// unqualified references in a declaration body resolve against the
    /// module that declaration belongs to. The target itself is never
    /// reported as its own referrer.
    /// </para>
    /// </summary>
    public static IReadOnlySet<DeclQualifiedName> FindDirectReferrers(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        DeclQualifiedName target)
    {
        var referrers = new HashSet<DeclQualifiedName>();

        foreach (var (caller, callee) in EnumerateReferenceEdges(declarations))
        {
            if (caller.Equals(target))
                continue;

            if (callee.Equals(target))
                referrers.Add(caller);
        }

        return referrers;
    }

    /// <summary>
    /// Yields every syntactic <c>(caller, callee)</c> reference edge
    /// derivable from <paramref name="declarations"/>. Each
    /// <see cref="SyntaxTypes.Expression.Identifier"/> reference in
    /// the body of a function declaration is resolved against
    /// <see cref="SyntaxTypes.SyntaxAnalysis.BuildModuleKeyAndDeclNameIndex(IReadOnlyDictionary{DeclQualifiedName, SyntaxTypes.Declaration})"/>
    /// — unqualified references resolve against the caller's enclosing
    /// module. References to declarations not present in
    /// <paramref name="declarations"/> are silently dropped.
    /// <para>
    /// The same edge may be yielded multiple times if it occurs multiple
    /// times in the source. Callers that need set semantics should
    /// deduplicate.
    /// </para>
    /// </summary>
    private static IEnumerable<(DeclQualifiedName caller, DeclQualifiedName callee)> EnumerateReferenceEdges(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var byModuleAndName =
            SyntaxTypes.SyntaxAnalysis.BuildModuleKeyAndDeclNameIndex(declarations);

        foreach (var (declKey, decl) in declarations)
        {
            if (decl is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                continue;

            var ownModuleKey = string.Join(".", declKey.Namespaces);
            var edges = new List<DeclQualifiedName>();

            CollectReferencesFromExpression(
                funcDecl.Function.Declaration.Expression,
                ownModuleKey,
                byModuleAndName,
                edges.Add);

            foreach (var callee in edges)
                yield return (declKey, callee);
        }
    }

    private static void CollectReferencesFromExpression(
        SyntaxTypes.Expression expression,
        string ownModuleKey,
        IReadOnlyDictionary<(string moduleKey, string declName), DeclQualifiedName> byModuleAndName,
        System.Action<DeclQualifiedName> emit)
    {
        // The reference collector emits at every Identifier regardless
        // of lexical scope: locally-bound names cannot appear as keys in
        // byModuleAndName (which only indexes top-level declarations), so
        // the resolution will silently fail for them anyway.
        SyntaxTypes.SyntaxAnalysis.WalkExpressionsWithScope(
            expression,
            [],
            (node, _) =>
            {
                if (node is not SyntaxTypes.Expression.Identifier identifier)
                    return;

                var moduleKey =
                    identifier.QualifiedName.Namespaces.Count is 0
                    ?
                    ownModuleKey
                    :
                    string.Join(".", identifier.QualifiedName.Namespaces);

                if (byModuleAndName.TryGetValue((moduleKey, identifier.QualifiedName.DeclName), out var resolved))
                    emit(resolved);
            });
    }

    /// <summary>
    /// Convenience overload that parses and canonicalizes the supplied Elm
    /// module texts, then runs
    /// <see cref="FindOptimizationOpportunities(IReadOnlyDictionary{DeclQualifiedName, SyntaxTypes.Declaration}, IReadOnlyCollection{DeclQualifiedName}?)"/>
    /// on the resulting flat declaration dictionary. Useful for short,
    /// inline test scenarios.
    /// </summary>
    public static ImmutableHashSet<Opportunity> FindOptimizationOpportunities(
        ModuleName elmModulesTexts,
        IReadOnlyCollection<DeclQualifiedName>? restrictToReachableFromEntryPoints = null)
    {
        var declarations = ParseAndCanonicalizeToFlatDict(elmModulesTexts);

        return
            FindOptimizationOpportunities(
                declarations,
                restrictToReachableFromEntryPoints);
    }

    private static void CollectFromExpression(
        SyntaxTypes.Expression expression,
        DeclQualifiedName containing,
        IReadOnlyDictionary<DeclQualifiedName, int> topLevelArity,
        ImmutableDictionary<string, int> letScope,
        ImmutableHashSet<string> functionTypedParameterNames,
        ExpressionTypeContext expressionTypeContext,
        ImmutableHashSet<Opportunity>.Builder resultBuilder)
    {
        // The outer switch enumerates every Expression variant explicitly so
        // adding a new variant in the future causes a hard failure here
        // (mandatory because the C# compiler does not statically check
        // exhaustiveness over an open record hierarchy). Variants that do
        // not produce findings are listed as no-op cases; recursion into
        // child expressions happens at the bottom of the switch.
        switch (expression)
        {
            case SyntaxTypes.Expression.RecordAccess recordAccess:
                if (RequiresGenericRecordOperation(recordAccess.Record, expressionTypeContext))
                {
                    MaybeAdd(
                        OpportunityCategory.RecordAccess,
                        recordAccess.FieldName,
                        containing,
                        resultBuilder,
                        new OpportunityTypeEvidence(
                            SubjectType:
                            InferExpressionType(recordAccess.Record, expressionTypeContext)));
                }

                CollectFromExpression(
                    recordAccess.Record,
                    containing,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    expressionTypeContext,
                    resultBuilder);

                break;

            case SyntaxTypes.Expression.RecordAccessFunction recordAccessFunction:
                if (!expressionTypeContext.ClosedRecordAccessFunctions.Contains(recordAccessFunction))
                {
                    MaybeAdd(
                        OpportunityCategory.RecordAccess,
                        recordAccessFunction.FieldName,
                        containing,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                var recordUpdateExpression =
                    new SyntaxTypes.Expression.Identifier(
                        DeclQualifiedName.Create([], recordUpdate.RecordName));

                var isOpenRecordUpdate =
                    RequiresGenericRecordOperation(recordUpdateExpression, expressionTypeContext);

                foreach (var field in recordUpdate.Fields)
                {
                    if (isOpenRecordUpdate)
                    {
                        MaybeAdd(
                            OpportunityCategory.RecordUpdate,
                            field.FieldName,
                            containing,
                            resultBuilder,
                            new OpportunityTypeEvidence(
                                SubjectType:
                                InferExpressionType(recordUpdateExpression, expressionTypeContext)));
                    }

                    CollectFromExpression(
                        field.Value,
                        containing,
                        topLevelArity,
                        letScope,
                        functionTypedParameterNames,
                        expressionTypeContext,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.Identifier identifier:
                if (identifier.QualifiedName.Namespaces.Count is 1 &&
                    identifier.QualifiedName.Namespaces[0] is "Basics" &&
                    s_basicsFunctionToCategory.TryGetValue(identifier.QualifiedName.DeclName, out var funcCategory))
                {
                    MaybeAdd(
                        funcCategory,
                        identifier.QualifiedName.DeclName,
                        containing,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                if (s_basicsOperatorToCategory.TryGetValue(opApp.Operator, out var opCategory))
                {
                    MaybeAdd(
                        opCategory,
                        "(" + opApp.Operator + ")",
                        containing,
                        resultBuilder,
                        new OpportunityTypeEvidence(
                            ArgumentTypes:
                            [
                            InferExpressionType(opApp.Left, expressionTypeContext),
                            InferExpressionType(opApp.Right, expressionTypeContext)
                            ]));
                }

                CollectFromExpression(
                    opApp.Left,
                    containing,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    expressionTypeContext,
                    resultBuilder);

                CollectFromExpression(
                    opApp.Right,
                    containing,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    expressionTypeContext,
                    resultBuilder);

                break;

            case SyntaxTypes.Expression.PrefixOperator prefixOp:
                if (s_basicsOperatorToCategory.TryGetValue(prefixOp.Operator, out var prefixCategory))
                {
                    MaybeAdd(
                        prefixCategory,
                        "(" + prefixOp.Operator + ")",
                        containing,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.Application app:
                if (app.Function is SyntaxTypes.Expression.Identifier appliedIdentifier &&
                    appliedIdentifier.QualifiedName.Namespaces is ["Basics"] &&
                    s_basicsFunctionToCategory.TryGetValue(
                        appliedIdentifier.QualifiedName.DeclName,
                        out var appliedCategory))
                {
                    MaybeAdd(
                        appliedCategory,
                        appliedIdentifier.QualifiedName.DeclName,
                        containing,
                        resultBuilder,
                        new OpportunityTypeEvidence(
                            ArgumentTypes:
                            [
                            .. app.Arguments.Select(
                                argument => InferExpressionType(argument, expressionTypeContext))
                            ]));
                }

                MaybeReportPartialApplication(
                    app,
                    containing,
                    topLevelArity,
                    letScope,
                    resultBuilder);

                CollectFromExpression(
                    app.Function,
                    containing,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    expressionTypeContext,
                    resultBuilder);

                foreach (var arg in app.Arguments)
                {
                    CollectFromExpression(
                        arg,
                        containing,
                        topLevelArity,
                        letScope,
                        functionTypedParameterNames,
                        expressionTypeContext,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.LetExpression letExpr:

                // Let-bound function declarations introduce new arities that
                // are visible inside the let body and inside sibling let
                // bindings (Elm let-rec semantics).
                var extendedLetScope = letScope;

                var extendedTypeContext =
                    expressionTypeContext with
                    {
                        LocalBindingTypes =
                        InferLetExpressionLocalBindingTypes(
                            letExpr,
                            expressionTypeContext),
                        LocalBindingExpressions =
                        ExtendLocalBindingExpressions(
                            letExpr,
                            expressionTypeContext.LocalBindingExpressions)
                    };

                foreach (var decl in letExpr.Declarations)
                {
                    if (decl is SyntaxTypes.LetDeclaration.LetFunction letFunc)
                    {
                        var name = letFunc.Function.Declaration.Name;
                        var arity = letFunc.Function.Declaration.Arguments.Count;

                        extendedLetScope = extendedLetScope.SetItem(name, arity);
                    }
                }

                foreach (var decl in letExpr.Declarations)
                {
                    switch (decl)
                    {
                        case SyntaxTypes.LetDeclaration.LetFunction letFunc:

                            // Each let-bound function introduces its own
                            // parameter scope. Higher-order parameter
                            // findings for those parameters are attributed
                            // to the containing top-level declaration but
                            // qualified with the let-function name in the
                            // description so they do not collide with
                            // identically-named outer parameters.
                            var letParamNames =
                                SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPatterns(
                                    letFunc.Function.Declaration.Arguments);

                            CollectFromExpression(
                                letFunc.Function.Declaration.Expression,
                                containing,
                                topLevelArity,
                                extendedLetScope,
                                // The outer function's parameters are still
                                // in lexical scope unless shadowed; the
                                // simple name-based check here intentionally
                                // accepts that shadowing is rare and the
                                // false-positive rate stays low for the
                                // current test corpus.
                                functionTypedParameterNames,
                                extendedTypeContext,
                                resultBuilder);

                            CollectHigherOrderParameterFindings(
                                letFunc.Function.Declaration.Expression,
                                letParamNames,
                                containing,
                                paramOwnerDescription:
                                letFunc.Function.Declaration.Name,
                                resultBuilder);

                            break;

                        case SyntaxTypes.LetDeclaration.LetDestructuring letDestr:
                            CollectFromExpression(
                                letDestr.Expression,
                                containing,
                                topLevelArity,
                                extendedLetScope,
                                functionTypedParameterNames,
                                extendedTypeContext,
                                resultBuilder);

                            break;

                        default:
                            throw new System.NotImplementedException(
                                "CollectFromExpression does not handle let declaration variant: " +
                                decl.GetType().Name);
                    }
                }

                CollectFromExpression(
                    letExpr.Expression,
                    containing,
                    topLevelArity,
                    extendedLetScope,
                    functionTypedParameterNames,
                    extendedTypeContext,
                    resultBuilder);

                break;

            case SyntaxTypes.Expression.LambdaExpression lambda:
                CollectFromExpression(
                    lambda.Expression,
                    containing,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    expressionTypeContext,
                    resultBuilder);

                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                CollectFromExpression(
                    ifBlock.Condition,
                    containing,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    expressionTypeContext,
                    resultBuilder);

                CollectFromExpression(
                    ifBlock.ThenBlock,
                    containing,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    expressionTypeContext,
                    resultBuilder);

                CollectFromExpression(
                    ifBlock.ElseBlock,
                    containing,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    expressionTypeContext,
                    resultBuilder);

                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                var caseScrutineeType =
                    InferExpressionType(caseExpr.Expression, expressionTypeContext);

                CollectFromExpression(
                    caseExpr.Expression,
                    containing,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    expressionTypeContext,
                    resultBuilder);

                foreach (var caseEntry in caseExpr.Cases)
                {
                    var caseExpressionTypeContext =
                        expressionTypeContext with
                        {
                            LocalBindingTypes =
                            ExtractCasePatternBindingTypes(
                                caseEntry.Pattern,
                                caseScrutineeType,
                                expressionTypeContext.LocalBindingTypes,
                                expressionTypeContext)
                        };

                    CollectFromExpression(
                        caseEntry.Expression,
                        containing,
                        topLevelArity,
                        letScope,
                        functionTypedParameterNames,
                        caseExpressionTypeContext,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var element in listExpr.Elements)
                {
                    CollectFromExpression(
                        element,
                        containing,
                        topLevelArity,
                        letScope,
                        functionTypedParameterNames,
                        expressionTypeContext,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.TupledExpression tupled:
                foreach (var element in tupled.Elements)
                {
                    CollectFromExpression(
                        element,
                        containing,
                        topLevelArity,
                        letScope,
                        functionTypedParameterNames,
                        expressionTypeContext,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                {
                    CollectFromExpression(
                        field.Value,
                        containing,
                        topLevelArity,
                        letScope,
                        functionTypedParameterNames,
                        expressionTypeContext,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.Negation negation:
                CollectFromExpression(
                    negation.Expression,
                    containing,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    expressionTypeContext,
                    resultBuilder);

                break;

            // Leaf variants with no nested Expression children.
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.GLSLExpression:
                break;

            default:
                throw new System.NotImplementedException(
                    "CollectFromExpression does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }

    private static bool RequiresGenericRecordOperation(
        SyntaxTypes.Expression expression,
        ExpressionTypeContext context) =>
        InferExpressionType(expression, context) is not TypeInference.InferredType.RecordType;

    private static TypeInference.InferredType InferExpressionType(
        SyntaxTypes.Expression expression,
        ExpressionTypeContext context)
    {
        if (expression is SyntaxTypes.Expression.RecordAccess recordAccess)
        {
            var recordType = InferExpressionType(recordAccess.Record, context);

            var fieldType =
                recordType switch
                {
                    TypeInference.InferredType.RecordType closedRecord =>
                    closedRecord.Fields
                    .FirstOrDefault(field => field.FieldName == recordAccess.FieldName)
                    .FieldType,

                    TypeInference.InferredType.OpenRecordType openRecord =>
                    openRecord.KnownFields
                    .FirstOrDefault(field => field.FieldName == recordAccess.FieldName)
                    .FieldType,

                    TypeInference.InferredType.IntType => null,
                    TypeInference.InferredType.FloatType => null,
                    TypeInference.InferredType.StringType => null,
                    TypeInference.InferredType.CharType => null,
                    TypeInference.InferredType.BoolType => null,
                    TypeInference.InferredType.NumberType => null,
                    TypeInference.InferredType.TupleType => null,
                    TypeInference.InferredType.FunctionType => null,
                    TypeInference.InferredType.ListType => null,
                    TypeInference.InferredType.ChoiceType => null,
                    TypeInference.InferredType.TypeVariable => null,
                    TypeInference.InferredType.UnknownType => null,

                    _ =>
                    throw new System.NotImplementedException(
                        "InferExpressionType does not handle inferred type variant: " +
                        recordType.GetType().Name)
                };

            if (fieldType is not null)
            {
                return
                    TypeInference.ExpandTypeAliases(
                        fieldType,
                        context.AliasTypes,
                        context.CurrentModuleName.Split('.'));
            }
        }

        if (expression is SyntaxTypes.Expression.Application
            {
                Function: SyntaxTypes.Expression.Identifier
                {
                    QualifiedName.Namespaces: ["Basics"],
                    QualifiedName.DeclName: "apR" or "apL"
                } pipeIdentifier,
                Arguments: [var firstArgument, var secondArgument]
            })
        {
            var valueExpression =
                pipeIdentifier.QualifiedName.DeclName is "apR"
                ?
                firstArgument
                :
                secondArgument;

            var functionExpression =
                pipeIdentifier.QualifiedName.DeclName is "apR"
                ?
                secondArgument
                :
                firstArgument;

            var valueType = InferExpressionType(valueExpression, context);
            var functionType = InferExpressionType(functionExpression, context);

            if (functionType is TypeInference.InferredType.FunctionType inferredFunctionType)
            {
                return
                    TypeInference.SpecializeTypesFromMatch(
                        inferredFunctionType.ArgumentType,
                        valueType,
                        [inferredFunctionType.ReturnType])[0];
            }
        }

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                context.ParameterNames,
                context.ParameterTypes,
                context.LocalBindingTypes,
                context.CurrentModuleName,
                context.FunctionTypes);

        return
            TypeInference.ExpandTypeAliases(
                inferredType,
                context.AliasTypes,
                context.CurrentModuleName.Split('.'));
    }

    /// <summary>
    /// Inspects an <see cref="SyntaxTypes.Expression.Application"/> node
    /// and, if its head is a statically known function whose arity exceeds
    /// the number of supplied arguments, records a
    /// <see cref="OpportunityCategory.PartialApplication"/> opportunity describing the
    /// added-argument vs parameter-count mismatch.
    /// <para>
    /// The head is "statically known" when it is an
    /// <see cref="SyntaxTypes.Expression.Identifier"/> resolving to a
    /// top-level <see cref="SyntaxTypes.Declaration.FunctionDeclaration"/>
    /// in <paramref name="topLevelArity"/>, an in-scope let-bound function
    /// in <paramref name="letScope"/>, or a
    /// <see cref="SyntaxTypes.Expression.PrefixOperator"/> (binary, arity 2).
    /// References to functions whose arity cannot be resolved (for
    /// example imports from native modules such as <c>Basics</c> /
    /// <c>Pine_kernel</c>, or unbound parameters) are skipped — the analysis
    /// only reports cases where the parameter count is known with
    /// certainty.
    /// </para>
    /// </summary>
    private static void MaybeReportPartialApplication(
        SyntaxTypes.Expression.Application app,
        DeclQualifiedName containing,
        IReadOnlyDictionary<DeclQualifiedName, int> topLevelArity,
        ImmutableDictionary<string, int> letScope,
        ImmutableHashSet<Opportunity>.Builder resultBuilder)
    {
        if (app.Arguments.Count < 1)
        {
            // Not actually applying any arguments — just a head reference.
            // Such forms are normally already simplified away by
            // canonicalization but we guard defensively.
            return;
        }

        var head = app.Function;

        var addedArguments = app.Arguments.Count;

        switch (head)
        {
            case SyntaxTypes.Expression.Identifier identifier:
                {
                    int? arity = null;
                    string? displayName = null;

                    if (identifier.QualifiedName.Namespaces.Count > 0)
                    {
                        var qualified = identifier.QualifiedName;

                        if (topLevelArity.TryGetValue(qualified, out var topArity))
                        {
                            arity = topArity;
                            displayName = qualified.FullName;
                        }
                    }
                    else if (letScope.TryGetValue(identifier.QualifiedName.DeclName, out var letArity))
                    {
                        arity = letArity;
                        displayName = identifier.QualifiedName.DeclName;
                    }

                    if (arity is int arityValue &&
                        arityValue > 0 &&
                        addedArguments < arityValue)
                    {
                        MaybeAdd(
                            OpportunityCategory.PartialApplication,
                            displayName +
                            "(" + addedArguments + "/" + arityValue + ")",
                            containing,
                            resultBuilder);
                    }
                }

                break;

            case SyntaxTypes.Expression.PrefixOperator prefixOp:
                {
                    // Source-level binary operators always have arity 2.
                    const int OperatorArity = 2;

                    if (addedArguments < OperatorArity)
                    {
                        MaybeAdd(
                            OpportunityCategory.PartialApplication,
                            "(" + prefixOp.Operator + ")" +
                            "(" + addedArguments + "/" + OperatorArity + ")",
                            containing,
                            resultBuilder);
                    }
                }

                break;
        }
    }

    /// <summary>
    /// Walks <paramref name="body"/> and records one
    /// <see cref="OpportunityCategory.HigherOrderParameter_Direct"/> finding per
    /// parameter in <paramref name="paramNames"/> that is observed as the
    /// head of an <see cref="SyntaxTypes.Expression.Application"/>. The
    /// description is the bare parameter name when
    /// <paramref name="paramOwnerDescription"/> is <c>null</c> (top-level
    /// function) and <c>"&lt;owner&gt;.&lt;param&gt;"</c> when the
    /// parameter belongs to a let-bound function — this keeps findings
    /// from different let scopes inside the same top-level decl from
    /// colliding.
    /// </summary>
    private static void CollectHigherOrderParameterFindings(
        SyntaxTypes.Expression body,
        ImmutableHashSet<string> paramNames,
        DeclQualifiedName containing,
        string? paramOwnerDescription,
        ImmutableHashSet<Opportunity>.Builder resultBuilder)
    {
        if (paramNames.Count is 0)
            return;

        var found = new HashSet<string>();
        FindAppliedParameterNames(body, paramNames, found);

        // Sort to keep snapshot output deterministic across runs.
        foreach (var name in found.OrderBy(n => n, System.StringComparer.Ordinal))
        {
            var description =
                paramOwnerDescription is null
                ?
                name
                :
                paramOwnerDescription + "." + name;

            MaybeAdd(
                OpportunityCategory.HigherOrderParameter_Direct,
                description,
                containing,
                resultBuilder);
        }
    }

    /// <summary>
    /// Cross-decl pass: emits <see cref="OpportunityCategory.HigherOrderParameter_Indirect"/>
    /// findings for owned bindings (top-level parameters or
    /// let-introduced bindings) that flow into a callee's higher-order
    /// parameter (directly higher-order at distance 1, or transitively
    /// at distance k+1).
    ///
    /// <para>
    /// Delegates the entire higher-order findings computation —
    /// destructured-name discovery, let-binding source tracking,
    /// forwarding-edge construction, reverse-BFS distance computation —
    /// to <see cref="HigherOrderParameterAnalysis.FindAllHigherOrderFindings"/>.
    /// This method only renders the result: each finding at distance
    /// <c>N &gt;= 1</c> becomes one <c>_Indirect</c> opportunity.
    /// Distance-0 findings are not converted here; the body walker in
    /// <see cref="CollectHigherOrderParameterFindings"/> already covers
    /// the direct-use cases (and also reports record-access chains
    /// rooted at a parameter and let-bound-function parameters, which
    /// the HOPA distance-0 view does not model).
    /// </para>
    /// </summary>
    private static void CollectIndirectHigherOrderParameterFindings(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        IReadOnlySet<DeclQualifiedName>? reachableSet,
        ImmutableHashSet<Opportunity>.Builder resultBuilder)
    {
        var allFindings =
            HigherOrderParameterAnalysis.FindAllHigherOrderFindings(declarations);

        foreach (var entry in allFindings
            .OrderBy(kvp => kvp.Key))
        {
            var decl = entry.Key;

            if (reachableSet is not null && !reachableSet.Contains(decl))
                continue;

            foreach (var finding in entry.Value
                .OrderBy(f => f.Name, System.StringComparer.Ordinal))
            {
                if (finding.Distance < 1)
                    continue;

                resultBuilder.Add(
                    new Opportunity(
                        decl,
                        OpportunityCategory.HigherOrderParameter_Indirect,
                        finding.Name + " @ distance " +
                        finding.Distance.ToString(System.Globalization.CultureInfo.InvariantCulture)));
            }
        }
    }

    /// <summary>
    /// Walks <paramref name="expression"/> and adds to <paramref name="found"/>
    /// every name in <paramref name="paramNames"/> that appears as the head
    /// of an <see cref="SyntaxTypes.Expression.Application"/>.
    /// </summary>
    private static void FindAppliedParameterNames(
        SyntaxTypes.Expression expression,
        ImmutableHashSet<string> paramNames,
        HashSet<string> found)
    {
        switch (expression)
        {
            case SyntaxTypes.Expression.Application app:
                {
                    var head = app.Function;

                    if (head is SyntaxTypes.Expression.Identifier identifier &&
                        identifier.QualifiedName.Namespaces.Count is 0 &&
                        paramNames.Contains(identifier.QualifiedName.DeclName))
                    {
                        found.Add(identifier.QualifiedName.DeclName);
                    }
                    else if (head is SyntaxTypes.Expression.RecordAccess recordAccessHead &&
                        TryRenderRecordAccessChainRootedAtParam(
                                 recordAccessHead, paramNames) is { } chainPath)
                    {
                        found.Add(chainPath);
                    }
                }

                FindAppliedParameterNames(app.Function, paramNames, found);

                foreach (var arg in app.Arguments)
                    FindAppliedParameterNames(arg, paramNames, found);

                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                FindAppliedParameterNames(opApp.Left, paramNames, found);
                FindAppliedParameterNames(opApp.Right, paramNames, found);
                break;

            case SyntaxTypes.Expression.LetExpression letExpr:

                // Names bound by `let` destructuring patterns extend the
                // visible-parameter set for the let body — when one of these
                // bound names is itself function-typed and gets applied
                // somewhere in the body, that points to the same kind of
                // higher-order opportunity as a directly-named parameter.
                var letDestructuredNames = ImmutableHashSet.CreateBuilder<string>();

                foreach (var decl in letExpr.Declarations)
                {
                    switch (decl)
                    {
                        case SyntaxTypes.LetDeclaration.LetFunction letFunc:

                            // The let-bound function's own parameters
                            // shadow any outer parameters of the same name,
                            // so remove them from the set before descending.
                            var letParamNames =
                                SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPatterns(
                                    letFunc.Function.Declaration.Arguments);

                            var visibleHere =
                                paramNames.Except(letParamNames);

                            FindAppliedParameterNames(
                                letFunc.Function.Declaration.Expression,
                                visibleHere,
                                found);

                            break;

                        case SyntaxTypes.LetDeclaration.LetDestructuring letDestr:
                            FindAppliedParameterNames(letDestr.Expression, paramNames, found);

                            foreach (var n in SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPattern(letDestr.Pattern))
                                letDestructuredNames.Add(n);

                            break;

                        default:
                            throw new System.NotImplementedException(
                                "FindAppliedParameterNames does not handle let declaration variant: " +
                                decl.GetType().Name);
                    }
                }

                FindAppliedParameterNames(
                    letExpr.Expression,
                    paramNames.Union(letDestructuredNames),
                    found);

                break;

            case SyntaxTypes.Expression.LambdaExpression lambda:
                var lambdaParams =
                    SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPatterns(lambda.Arguments);

                FindAppliedParameterNames(
                    lambda.Expression,
                    paramNames.Except(lambdaParams),
                    found);

                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                FindAppliedParameterNames(ifBlock.Condition, paramNames, found);
                FindAppliedParameterNames(ifBlock.ThenBlock, paramNames, found);
                FindAppliedParameterNames(ifBlock.ElseBlock, paramNames, found);
                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                FindAppliedParameterNames(caseExpr.Expression, paramNames, found);

                foreach (var caseEntry in caseExpr.Cases)
                {
                    // Names bound by this branch's pattern extend the
                    // visible-parameter set for the branch body. The
                    // surrounding outer parameters remain visible too,
                    // unless the pattern shadows one (we conservatively
                    // keep both — Elm forbids shadowing in patterns).
                    var branchBound =
                        SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPattern(caseEntry.Pattern);

                    FindAppliedParameterNames(
                        caseEntry.Expression,
                        paramNames.Union(branchBound),
                        found);
                }

                break;

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var element in listExpr.Elements)
                    FindAppliedParameterNames(element, paramNames, found);

                break;

            case SyntaxTypes.Expression.TupledExpression tupled:
                foreach (var element in tupled.Elements)
                    FindAppliedParameterNames(element, paramNames, found);

                break;

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                    FindAppliedParameterNames(field.Value, paramNames, found);

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields)
                    FindAppliedParameterNames(field.Value, paramNames, found);

                break;

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                FindAppliedParameterNames(recordAccess.Record, paramNames, found);
                break;

            case SyntaxTypes.Expression.Negation negation:
                FindAppliedParameterNames(negation.Expression, paramNames, found);
                break;

            case SyntaxTypes.Expression.Identifier:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.GLSLExpression:
                break;

            default:
                throw new System.NotImplementedException(
                    "FindAppliedParameterNames does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }

    /// <summary>
    /// If <paramref name="recordAccess"/> is a chain
    /// <c>p.f1.f2.…fn</c> whose innermost record expression is a bare
    /// <see cref="SyntaxTypes.Expression.Identifier"/> with an empty
    /// module part and name in <paramref name="paramNames"/>, returns
    /// <c>"p.f1.f2.…fn"</c>; otherwise returns <c>null</c>.
    /// </summary>
    private static string? TryRenderRecordAccessChainRootedAtParam(
        SyntaxTypes.Expression.RecordAccess recordAccess,
        ImmutableHashSet<string> paramNames)
    {
        var fields = new List<string>();
        SyntaxTypes.Expression current = recordAccess;

        while (current is SyntaxTypes.Expression.RecordAccess ra)
        {
            fields.Add(ra.FieldName);
            current = ra.Record;
        }

        if (current is not SyntaxTypes.Expression.Identifier rootIdentifier)
            return null;

        if (rootIdentifier.QualifiedName.Namespaces.Count is not 0)
            return null;

        if (!paramNames.Contains(rootIdentifier.QualifiedName.DeclName))
            return null;

        // `fields` was built from outermost to innermost; reverse so the
        // rendered chain reads root-to-leaf.
        fields.Reverse();

        return rootIdentifier.QualifiedName.DeclName + "." + string.Join(".", fields);
    }

    private static void MaybeAdd(
        OpportunityCategory category,
        string description,
        DeclQualifiedName containing,
        ImmutableHashSet<Opportunity>.Builder resultBuilder,
        OpportunityTypeEvidence? typeEvidence = null)
    {
        resultBuilder.Add(
            new Opportunity(containing, category, description)
            {
                TypeEvidence = typeEvidence
            });
    }

    private static IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        ParseAndCanonicalizeToFlatDict(IReadOnlyList<string> elmModulesTexts)
    {
        var parsedModules =
            elmModulesTexts
            .Select(
                moduleText =>
                ElmSyntax.ElmSyntaxParser.ParseModuleText(moduleText)
                .Extract(err => throw new System.Exception("Failed parsing: " + err)))
            .ToList();

        var canonicalized =
            Canonicalization.CanonicalizeOrThrow(parsedModules)
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        var orderedModules =
            parsedModules
            .Select(
                parsedModule =>
                canonicalized[
                    ElmSyntax.SyntaxModel.Module.GetModuleName(
                        parsedModule.ModuleDefinition.Value).Value]
                .Extract(err => throw new System.Exception("Module has errors: " + err)))
            .ToList();

        var orderedModulesAbstract =
            orderedModules
            .Select(SyntaxTypes.ConvertFromConcrete.FromFile)
            .ToList();

        return ElmCompiler.FlattenModulesToDeclarationDictionary(orderedModulesAbstract);
    }

    /// <summary>
    /// Information about a single-tag (one-constructor) custom type used
    /// by <see cref="OpportunityCategory.RootLevelChoiceTagWrapper"/> detection.
    /// Carries the constructor's argument types and the type's generics
    /// so detection sites can substitute generic type variables when
    /// rendering the unwrapped type from a type annotation that supplies
    /// concrete type arguments.
    /// </summary>
    internal sealed record SingleTagShapeInfo(
        DeclQualifiedName TypeName,
        DeclQualifiedName ConstructorName,
        ModuleName TypeGenerics,
        IReadOnlyList<SyntaxTypes.TypeAnnotation> ConstructorArgumentTypes);

    /// <summary>
    /// Builds a registry of every custom type in <paramref name="declarations"/>
    /// that has exactly one constructor (the constructor itself may have
    /// any number of arguments, including zero). Both the type's
    /// qualified name and the constructor's qualified name are mapped to
    /// the same <see cref="SingleTagShapeInfo"/> so detection sites can
    /// resolve from either direction.
    /// </summary>
    internal static ImmutableDictionary<DeclQualifiedName, SingleTagShapeInfo>
        BuildSingleTagRegistry(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SingleTagShapeInfo>();

        foreach (var (declName, decl) in declarations)
        {
            if (decl is not SyntaxTypes.Declaration.ChoiceTypeDeclaration ctd)
                continue;

            if (ctd.TypeDeclaration.Constructors.Count is not 1)
                continue;

            var ctor = ctd.TypeDeclaration.Constructors[0];

            var typeName =
                DeclQualifiedName.Create(declName.Namespaces, ctd.TypeDeclaration.Name);

            var ctorName =
                DeclQualifiedName.Create(declName.Namespaces, ctor.Name);

            var generics =
                ctd.TypeDeclaration.Generics
                .ToList();

            var ctorArgs =
                ctor.Arguments
                .ToList();

            var info = new SingleTagShapeInfo(typeName, ctorName, generics, ctorArgs);

            builder[typeName] = info;
            builder[ctorName] = info;
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Examines the top-level parameters and the outermost return value
    /// of <paramref name="function"/> for evidence that they are wrapped
    /// in a single-tag custom-type constructor and adds matching
    /// <see cref="OpportunityCategory.RootLevelChoiceTagWrapper"/> findings
    /// to <paramref name="resultBuilder"/>.
    ///
    /// <para>
    /// Detection sources for parameters (any one is sufficient; only one
    /// finding per parameter is emitted):
    /// <list type="bullet">
    /// <item>The function's signature names a single-tag type as the
    /// parameter's root type.</item>
    /// <item>The parameter pattern is a <see cref="SyntaxTypes.Pattern.NamedPattern"/>
    /// (optionally wrapped in parens / as-pattern) whose constructor
    /// resolves to a single-tag constructor.</item>
    /// <item>The function body contains a top-level
    /// <c>let (Ctor x ...) = paramName in ...</c> destructuring whose
    /// constructor resolves to a single-tag constructor.</item>
    /// </list>
    /// </para>
    ///
    /// <para>
    /// Detection sources for the return value:
    /// <list type="bullet">
    /// <item>The function's signature names a single-tag type as the
    /// outermost return type.</item>
    /// <item>Every "return leaf" position of the body (case arms, if
    /// branches, let-in body, parens) is an
    /// <c>Application[FunctionOrValue(Ctor), ...]</c> whose constructor
    /// is the same single-tag constructor at every leaf.</item>
    /// </list>
    /// </para>
    /// </summary>
    private static void CollectRootLevelChoiceTagWrapperFindings(
        DeclQualifiedName containing,
        SyntaxTypes.FunctionStruct function,
        ImmutableDictionary<DeclQualifiedName, SingleTagShapeInfo> singleTagRegistry,
        ImmutableHashSet<Opportunity>.Builder resultBuilder)
    {
        if (singleTagRegistry.IsEmpty)
            return;

        var ownModule = containing.Namespaces;

        var implementation = function.Declaration;

        // Walk the type signature into a list of parameter type
        // annotations + a single return type annotation. A function
        // without a signature contributes an empty list and a null
        // return type.
        var sigParamTypes = new List<SyntaxTypes.TypeAnnotation?>();
        SyntaxTypes.TypeAnnotation? sigReturnType = null;

        if (function.Signature is { } signature)
        {
            DecomposeFunctionSignature(
                signature.TypeAnnotation,
                implementation.Arguments.Count,
                sigParamTypes,
                out sigReturnType);
        }

        // Per-parameter detection.
        for (var i = 0; i < implementation.Arguments.Count; i++)
        {
            var paramPattern = implementation.Arguments[i];
            var paramName = SyntaxTypes.SyntaxAnalysis.TryGetParameterDisplayName(paramPattern);

            // 1. Signature-based.
            SingleTagShapeInfo? matchedFromSig = null;
            IReadOnlyList<SyntaxTypes.TypeAnnotation>? unwrappedFromSig = null;

            if (i < sigParamTypes.Count && sigParamTypes[i] is { } sigParamType)
            {
                var (sigInfo, sigUnwrapped) =
                    TryResolveSingleTagWrap(sigParamType, singleTagRegistry, ownModule);

                matchedFromSig = sigInfo;
                unwrappedFromSig = sigUnwrapped;
            }

            // 2. Pattern-based.
            var matchedFromPattern =
                TryMatchSingleTagFromPattern(paramPattern, singleTagRegistry, ownModule);

            // 3. Let-destructuring-based — only at the top level of the
            // body's let chain (we deliberately do not descend into
            // nested expressions to keep the detection root-scoped).
            SingleTagShapeInfo? matchedFromLet = null;

            if (paramName is not null)
            {
                matchedFromLet =
                    TryMatchSingleTagFromTopLevelLetDestructuring(
                        implementation.Expression,
                        paramName,
                        singleTagRegistry,
                        ownModule);
            }

            var anyMatch = matchedFromSig ?? matchedFromPattern ?? matchedFromLet;

            if (anyMatch is null)
                continue;

            var description =
                OptimizationOpportunityRenderer.RenderRootLevelWrapperParameterDescription(
                    i,
                    paramName,
                    anyMatch,
                    unwrappedFromSig);

            MaybeAdd(
                OpportunityCategory.RootLevelChoiceTagWrapper,
                description,
                containing,
                resultBuilder);
        }

        // Return-value detection.
        SingleTagShapeInfo? returnMatched = null;
        IReadOnlyList<SyntaxTypes.TypeAnnotation>? returnUnwrapped = null;

        if (sigReturnType is not null)
        {
            var (sigInfo, sigUnwrapped) =
                TryResolveSingleTagWrap(sigReturnType, singleTagRegistry, ownModule);

            returnMatched = sigInfo;
            returnUnwrapped = sigUnwrapped;
        }

        returnMatched ??=
            TryMatchSingleTagFromAllReturnLeaves(
                implementation.Expression,
                singleTagRegistry,
                ownModule);

        if (returnMatched is not null)
        {
            MaybeAdd(
                OpportunityCategory.RootLevelChoiceTagWrapper,
                OptimizationOpportunityRenderer.RenderRootLevelWrapperReturnDescription(
                    returnMatched,
                    returnUnwrapped),
                containing,
                resultBuilder);
        }
    }

    /// <summary>
    /// Splits a function-typed annotation into the leading
    /// <paramref name="parameterCount"/> parameter type annotations and
    /// the trailing return type annotation, mirroring how Elm desugars
    /// curried functions. When the annotation has fewer arrows than the
    /// implementation has parameters (for example because some
    /// parameters are introduced by an inner lambda), the trailing
    /// "missing" entries are recorded as <c>null</c> in
    /// <paramref name="sigParamTypes"/>.
    /// </summary>
    internal static void DecomposeFunctionSignature(
        SyntaxTypes.TypeAnnotation annotation,
        int parameterCount,
        List<SyntaxTypes.TypeAnnotation?> sigParamTypes,
        out SyntaxTypes.TypeAnnotation? sigReturnType)
    {
        var current = annotation;

        for (var i = 0; i < parameterCount; i++)
        {
            if (current is SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation fta)
            {
                sigParamTypes.Add(fta.ArgumentType);
                current = fta.ReturnType;
            }
            else
            {
                sigParamTypes.Add(null);
            }
        }

        sigReturnType = current;
    }

    /// <summary>
    /// Returns the <see cref="SingleTagShapeInfo"/> for the supplied
    /// type annotation when it is a
    /// <see cref="SyntaxTypes.TypeAnnotation.Typed"/> reference to a
    /// single-tag custom type, plus the unwrapped type annotations with
    /// generic substitution applied. Returns
    /// <c>(null, null)</c> for any other annotation shape.
    /// </summary>
    internal static (SingleTagShapeInfo? Info, IReadOnlyList<SyntaxTypes.TypeAnnotation>? UnwrappedTypes)
        TryResolveSingleTagWrap(
        SyntaxTypes.TypeAnnotation annotation,
        ImmutableDictionary<DeclQualifiedName, SingleTagShapeInfo> singleTagRegistry,
        ModuleName ownModule)
    {
        if (annotation is not SyntaxTypes.TypeAnnotation.Typed typed)
            return (null, null);

        var qualified =
            typed.ModuleName.Count > 0
            ?
            DeclQualifiedName.Create(typed.ModuleName, typed.Name)
            :
            DeclQualifiedName.Create(ownModule, typed.Name);

        if (!singleTagRegistry.TryGetValue(qualified, out var info))
            return (null, null);

        // The matched entry must be the type (not the constructor): we
        // are resolving a type annotation, not a constructor reference.
        if (!info.TypeName.Equals(qualified))
            return (null, null);

        // Build the generic substitution from the type's declared
        // generics to the actual type arguments at this annotation site.
        var substitution = new Dictionary<string, SyntaxTypes.TypeAnnotation>();

        for (var i = 0; i < info.TypeGenerics.Count; i++)
        {
            if (i < typed.TypeArguments.Count)
            {
                substitution[info.TypeGenerics[i]] = typed.TypeArguments[i];
            }
        }

        var substitutedArgs =
            info.ConstructorArgumentTypes
            .Select(a => SubstituteGenerics(a, substitution))
            .ToList();

        return (info, substitutedArgs);
    }

    /// <summary>
    /// Returns the <see cref="SingleTagShapeInfo"/> implied by a
    /// parameter or destructuring pattern when (after peeling
    /// as-patterns) the pattern is a
    /// <see cref="SyntaxTypes.Pattern.NamedPattern"/> whose constructor
    /// resolves to a single-tag constructor; otherwise <c>null</c>.
    /// </summary>
    private static SingleTagShapeInfo? TryMatchSingleTagFromPattern(
        SyntaxTypes.Pattern pattern,
        ImmutableDictionary<DeclQualifiedName, SingleTagShapeInfo> singleTagRegistry,
        ModuleName ownModule)
    {
        var peeled = SyntaxTypes.SyntaxAnalysis.PeelPatternAsBinder(pattern);

        if (peeled is not SyntaxTypes.Pattern.NamedPattern named)
            return null;

        var qualified =
            named.Name.ModuleName.Count > 0
            ?
            DeclQualifiedName.Create(named.Name.ModuleName, named.Name.Name)
            :
            DeclQualifiedName.Create(ownModule, named.Name.Name);

        if (!singleTagRegistry.TryGetValue(qualified, out var info))
            return null;

        // Must resolve to the constructor entry (not the type entry
        // which shares the registry under the type's name).
        if (!info.ConstructorName.Equals(qualified))
            return null;

        return info;
    }

    /// <summary>
    /// Walks <paramref name="body"/>'s leading let chain (only the let
    /// blocks at the very top of the body, not nested ones) and returns
    /// the single-tag <see cref="SingleTagShapeInfo"/> implied by any
    /// <see cref="SyntaxTypes.LetDeclaration.LetDestructuring"/>
    /// that matches a <see cref="SyntaxTypes.Pattern.NamedPattern"/>
    /// applied to the bare parameter named
    /// <paramref name="paramName"/>; <c>null</c> when no such
    /// destructuring exists.
    /// </summary>
    private static SingleTagShapeInfo? TryMatchSingleTagFromTopLevelLetDestructuring(
        SyntaxTypes.Expression body,
        string paramName,
        ImmutableDictionary<DeclQualifiedName, SingleTagShapeInfo> singleTagRegistry,
        ModuleName ownModule)
    {
        var current = body;

        while (current is SyntaxTypes.Expression.LetExpression letExpr)
        {
            foreach (var decl in letExpr.Declarations)
            {
                if (decl is not SyntaxTypes.LetDeclaration.LetDestructuring letDestr)
                    continue;

                var rhs = letDestr.Expression;

                if (rhs is not SyntaxTypes.Expression.Identifier rhsRef)
                    continue;

                if (rhsRef.QualifiedName.Namespaces.Count is not 0 || rhsRef.QualifiedName.DeclName != paramName)
                    continue;

                var match =
                    TryMatchSingleTagFromPattern(
                        letDestr.Pattern,
                        singleTagRegistry,
                        ownModule);

                if (match is not null)
                    return match;
            }

            current = letExpr.Expression;
        }

        return null;
    }

    /// <summary>
    /// Returns the single-tag <see cref="SingleTagShapeInfo"/> that
    /// every "return leaf" of <paramref name="body"/> wraps with at
    /// the root, when this is consistent across every leaf; otherwise
    /// <c>null</c>. Return leaves are followed across
    /// <see cref="SyntaxTypes.Expression.LetExpression"/>,
    /// <see cref="SyntaxTypes.Expression.IfBlock"/>, and
    /// <see cref="SyntaxTypes.Expression.CaseExpression"/>.
    /// </summary>
    private static SingleTagShapeInfo? TryMatchSingleTagFromAllReturnLeaves(
        SyntaxTypes.Expression body,
        ImmutableDictionary<DeclQualifiedName, SingleTagShapeInfo> singleTagRegistry,
        ModuleName ownModule)
    {
        SingleTagShapeInfo? agreed = null;

        var allMatched =
            AllReturnLeavesAgreeOnSingleTagCtor(
                body,
                singleTagRegistry,
                ownModule,
                ref agreed);

        return allMatched ? agreed : null;
    }

    private static bool AllReturnLeavesAgreeOnSingleTagCtor(
        SyntaxTypes.Expression expression,
        ImmutableDictionary<DeclQualifiedName, SingleTagShapeInfo> singleTagRegistry,
        ModuleName ownModule,
        ref SingleTagShapeInfo? agreed)
    {
        foreach (var leaf in SyntaxTypes.SyntaxAnalysis.EnumerateReturnLeaves(expression))
        {
            if (leaf is not SyntaxTypes.Expression.Application app)
                return false;

            if (app.Arguments.Count < 1)
                return false;

            var head = app.Function;

            if (head is not SyntaxTypes.Expression.Identifier identifier)
                return false;

            var qualified =
                identifier.QualifiedName.Namespaces.Count > 0
                ?
                identifier.QualifiedName
                :
                DeclQualifiedName.Create(ownModule, identifier.QualifiedName.DeclName);

            if (!singleTagRegistry.TryGetValue(qualified, out var info))
                return false;

            if (!info.ConstructorName.Equals(qualified))
                return false;

            // The application must supply exactly one positional
            // argument per constructor field (Elm constructors are
            // applied uncurried at the source level).
            var suppliedArgCount = app.Arguments.Count;

            if (suppliedArgCount != info.ConstructorArgumentTypes.Count)
                return false;

            if (agreed is null)
            {
                agreed = info;
                continue;
            }

            if (!agreed.ConstructorName.Equals(info.ConstructorName))
                return false;
        }

        return agreed is not null;
    }

    /// <summary>
    /// Substitutes every <see cref="SyntaxTypes.TypeAnnotation.GenericType"/>
    /// reference in <paramref name="annotation"/> with its corresponding
    /// type argument from <paramref name="substitution"/>; references
    /// not in the dictionary are left untouched.
    /// </summary>
    private static SyntaxTypes.TypeAnnotation SubstituteGenerics(
        SyntaxTypes.TypeAnnotation annotation,
        IReadOnlyDictionary<string, SyntaxTypes.TypeAnnotation> substitution)
    {
        switch (annotation)
        {
            case SyntaxTypes.TypeAnnotation.GenericType g:
                return
                    substitution.TryGetValue(g.Name, out var replacement)
                    ?
                    replacement
                    :
                    g;

            case SyntaxTypes.TypeAnnotation.Typed t:
                {
                    var newArgs =
                        t.TypeArguments
                        .Select(arg => SubstituteGenerics(arg, substitution))
                        .ToList();

                    return new SyntaxTypes.TypeAnnotation.Typed(t.ModuleName, t.Name, newArgs);
                }

            case SyntaxTypes.TypeAnnotation.Unit:
                return annotation;

            case SyntaxTypes.TypeAnnotation.Tupled tupled:
                {
                    var newAnnots =
                        tupled.TypeAnnotations
                        .Select(a => SubstituteGenerics(a, substitution))
                        .ToList();

                    return new SyntaxTypes.TypeAnnotation.Tupled(newAnnots);
                }

            case SyntaxTypes.TypeAnnotation.Record record:
                {
                    var newFields =
                        record.RecordDefinition.Fields
                        .Select(
                            f =>
                            SyntaxTypes.RecordField.Create(
                                f.FieldName,
                                SubstituteGenerics(f.FieldType, substitution)))
                        .ToList();

                    return
                        new SyntaxTypes.TypeAnnotation.Record(
                            new SyntaxTypes.RecordDefinition(newFields));
                }

            case SyntaxTypes.TypeAnnotation.GenericRecord gr:
                {
                    var newFields =
                        gr.RecordDefinition.Fields
                        .Select(
                            f =>
                            SyntaxTypes.RecordField.Create(
                                f.FieldName,
                                SubstituteGenerics(f.FieldType, substitution)))
                        .ToList();

                    return
                        new SyntaxTypes.TypeAnnotation.GenericRecord(
                            gr.GenericName,
                            new SyntaxTypes.RecordDefinition(newFields));
                }

            case SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation fta:
                return
                    new SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation(
                        SubstituteGenerics(fta.ArgumentType, substitution),
                        SubstituteGenerics(fta.ReturnType, substitution));

            default:
                throw new System.NotImplementedException(
                    "SubstituteGenerics does not handle TypeAnnotation variant: " +
                    annotation.GetType().Name);
        }
    }
}
