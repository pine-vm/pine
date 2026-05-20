using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using SyntaxModel = Pine.Core.Elm.ElmSyntax.SyntaxModel;
using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Categorises an <see cref="Opportunity"/> by the kind of generic operation
/// (or by the kind of unspecialized parameter usage) it represents. The
/// category is rendered alongside the opportunity's
/// <see cref="Opportunity.Description"/> in
/// <see cref="OptimizationOpportunityFinder.RenderOpportunities(IEnumerable{Opportunity})"/>
/// and grouped on by
/// <see cref="OptimizationOpportunityFinder.RenderOpportunitiesByCategory(IEnumerable{Opportunity})"/>.
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
    HigherOrderParameter,

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
/// Friendly, human-readable category names used for snapshot rendering.
/// Kept stable across versions so test snapshots remain valid.
/// </summary>
public static class OpportunityCategoryFormatting
{
    /// <summary>
    /// Returns the snapshot-stable name for the given category. Used by
    /// <see cref="OptimizationOpportunityFinder.RenderOpportunities(IEnumerable{Opportunity})"/>
    /// and friends.
    /// </summary>
    public static string ToDisplayName(OpportunityCategory category) =>
        category switch
        {
            OpportunityCategory.RecordAccess => "record-access",
            OpportunityCategory.RecordUpdate => "record-update",
            OpportunityCategory.BasicsArithmetic => "Basics.arithmetic",
            OpportunityCategory.BasicsCompare => "Basics.compare",
            OpportunityCategory.BasicsEq => "Basics.eq",
            OpportunityCategory.BasicsAppend => "Basics.append",
            OpportunityCategory.PartialApplication => "partial-application",
            OpportunityCategory.HigherOrderParameter => "higher-order-parameter",
            OpportunityCategory.RootLevelChoiceTagWrapper => "root-level-choice-tag-wrapper",

            _ =>
            throw new System.NotImplementedException(
                "OpportunityCategoryFormatting.ToDisplayName does not handle category: " +
                category),
        };
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
public record Opportunity(
    DeclQualifiedName ContainingDecl,
    OpportunityCategory Category,
    string Description)
    : System.IComparable<Opportunity>
{
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
/// Per-category whitelist parameters take an <see cref="ImmutableHashSet{T}"/>
/// of declaration name prefixes. A whitelisted prefix matches every
/// declaration whose <see cref="DeclQualifiedName.FullName"/> begins with
/// that prefix — for example <c>"App.test"</c> also covers
/// <c>"App.test_specialized_1"</c>.
/// </para>
/// </summary>
public static class OptimizationOpportunityFinder
{
    /// <summary>
    /// Mapping from <c>Basics</c> function name (as used in
    /// <see cref="SyntaxTypes.Expression.FunctionOrValue"/>) to the
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
    /// Each per-category whitelist parameter (for example
    /// <paramref name="ignoreRecordOperation"/>) is a set of declaration
    /// name prefixes that suppress findings of that category for any
    /// declaration whose <see cref="DeclQualifiedName.FullName"/> starts
    /// with one of the listed prefixes. Pass <c>null</c> for an empty
    /// whitelist.
    /// </para>
    ///
    /// <para>
    /// <paramref name="ignoreRecordOperation"/> applies to both
    /// <see cref="OpportunityCategory.RecordAccess"/> and
    /// <see cref="OpportunityCategory.RecordUpdate"/>.
    /// </para>
    /// </summary>
    public static ImmutableHashSet<Opportunity> FindOptimizationOpportunities(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        ImmutableHashSet<string>? ignoreRecordOperation = null,
        ImmutableHashSet<string>? ignoreBasicsArithmetic = null,
        ImmutableHashSet<string>? ignoreBasicsCompare = null,
        ImmutableHashSet<string>? ignoreBasicsEq = null,
        ImmutableHashSet<string>? ignoreBasicsAppend = null,
        ImmutableHashSet<string>? ignorePartialApplication = null,
        ImmutableHashSet<string>? ignoreHigherOrderParameter = null,
        ImmutableHashSet<string>? ignoreRootLevelChoiceTagWrapper = null,
        IReadOnlyCollection<DeclQualifiedName>? restrictToReachableFromEntryPoints = null)
    {
        var whitelistByCategory =
            new Dictionary<OpportunityCategory, ImmutableHashSet<string>>
            {
                [OpportunityCategory.RecordAccess] = ignoreRecordOperation ?? [],
                [OpportunityCategory.RecordUpdate] = ignoreRecordOperation ?? [],
                [OpportunityCategory.BasicsArithmetic] = ignoreBasicsArithmetic ?? [],
                [OpportunityCategory.BasicsCompare] = ignoreBasicsCompare ?? [],
                [OpportunityCategory.BasicsEq] = ignoreBasicsEq ?? [],
                [OpportunityCategory.BasicsAppend] = ignoreBasicsAppend ?? [],
                [OpportunityCategory.PartialApplication] = ignorePartialApplication ?? [],
                [OpportunityCategory.HigherOrderParameter] = ignoreHigherOrderParameter ?? [],
                [OpportunityCategory.RootLevelChoiceTagWrapper] = ignoreRootLevelChoiceTagWrapper ?? [],
            };

        // Build the top-level arity map once. Only function declarations
        // contribute — type aliases / custom types / ports / infix
        // declarations do not have a callable arity in this analysis.
        var topLevelArity = new Dictionary<DeclQualifiedName, int>();

        foreach (var (qualifiedName, declaration) in declarations)
        {
            if (declaration is SyntaxTypes.Declaration.FunctionDeclaration fd)
            {
                topLevelArity[qualifiedName] =
                    fd.Function.Declaration.Value.Arguments.Count;
            }
        }

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
                CollectVarPatternNames(funcDecl.Function.Declaration.Value.Arguments);

            CollectFromExpression(
                funcDecl.Function.Declaration.Value.Expression.Value,
                qualifiedName,
                whitelistByCategory,
                topLevelArity,
                [],
                topLevelParamNames,
                resultBuilder);

            // Higher-order parameter detection for the top-level function:
            // a parameter that is the head of an application anywhere in the
            // body is reported once per (decl, parameter name).
            CollectHigherOrderParameterFindings(
                funcDecl.Function.Declaration.Value.Expression.Value,
                topLevelParamNames,
                qualifiedName,
                paramOwnerDescription: null,
                whitelistByCategory,
                resultBuilder);

            // Root-level single-tag-wrapper detection for top-level
            // parameters and the outermost return value of the function.
            CollectRootLevelChoiceTagWrapperFindings(
                qualifiedName,
                funcDecl.Function,
                singleTagRegistry,
                whitelistByCategory,
                resultBuilder);
        }

        return resultBuilder.ToImmutable();
    }

    /// <summary>
    /// Computes the set of <see cref="DeclQualifiedName"/> values reachable
    /// (by syntactic <see cref="SyntaxTypes.Expression.FunctionOrValue"/>
    /// reference) from any entry point in <paramref name="entryPoints"/>.
    /// Used by the
    /// <c>restrictToReachableFromEntryPoints</c> overload of
    /// <see cref="FindOptimizationOpportunities(IReadOnlyDictionary{DeclQualifiedName, SyntaxTypes.Declaration},
    /// ImmutableHashSet{string}?, ImmutableHashSet{string}?,
    /// ImmutableHashSet{string}?, ImmutableHashSet{string}?,
    /// ImmutableHashSet{string}?, ImmutableHashSet{string}?,
    /// ImmutableHashSet{string}?, IReadOnlyCollection{DeclQualifiedName}?)"/>.
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
        // Index declarations by simple name within each module so an
        // unqualified `FunctionOrValue` (introduced by canonicalization
        // for in-module references) can be resolved against the module
        // it appears in.
        var byModuleAndName =
            new Dictionary<(string moduleKey, string declName), DeclQualifiedName>();

        foreach (var key in declarations.Keys)
        {
            var moduleKey = string.Join(".", key.Namespaces);
            byModuleAndName[(moduleKey, key.DeclName)] = key;
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

            if (!declarations.TryGetValue(current, out var decl))
                continue;

            if (decl is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                continue;

            var ownModuleKey = string.Join(".", current.Namespaces);

            CollectReferencesFromExpression(
                funcDecl.Function.Declaration.Value.Expression.Value,
                ownModuleKey,
                byModuleAndName,
                reference =>
                {
                    if (reachable.Add(reference))
                    {
                        queue.Enqueue(reference);
                    }
                });
        }

        return reachable;
    }

    private static void CollectReferencesFromExpression(
        SyntaxTypes.Expression expression,
        string ownModuleKey,
        IReadOnlyDictionary<(string moduleKey, string declName), DeclQualifiedName> byModuleAndName,
        System.Action<DeclQualifiedName> emit)
    {
        switch (expression)
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrValue:
                {
                    string moduleKey;

                    if (funcOrValue.ModuleName.Count is 0)
                    {
                        moduleKey = ownModuleKey;
                    }
                    else
                    {
                        moduleKey = string.Join(".", funcOrValue.ModuleName);
                    }

                    if (byModuleAndName.TryGetValue((moduleKey, funcOrValue.Name), out var resolved))
                    {
                        emit(resolved);
                    }
                }

                break;

            case SyntaxTypes.Expression.Application app:
                foreach (var arg in app.Arguments)
                    CollectReferencesFromExpression(arg.Value, ownModuleKey, byModuleAndName, emit);

                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                CollectReferencesFromExpression(opApp.Left.Value, ownModuleKey, byModuleAndName, emit);
                CollectReferencesFromExpression(opApp.Right.Value, ownModuleKey, byModuleAndName, emit);
                break;

            case SyntaxTypes.Expression.LetExpression letExpr:
                foreach (var declNode in letExpr.Value.Declarations)
                {
                    switch (declNode.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            CollectReferencesFromExpression(
                                letFunc.Function.Declaration.Value.Expression.Value,
                                ownModuleKey,
                                byModuleAndName,
                                emit);

                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            CollectReferencesFromExpression(
                                letDestr.Expression.Value,
                                ownModuleKey,
                                byModuleAndName,
                                emit);

                            break;

                        default:
                            throw new System.NotImplementedException(
                                "CollectReferencesFromExpression does not handle let declaration variant: " +
                                declNode.Value.GetType().Name);
                    }
                }

                CollectReferencesFromExpression(
                    letExpr.Value.Expression.Value,
                    ownModuleKey,
                    byModuleAndName,
                    emit);

                break;

            case SyntaxTypes.Expression.LambdaExpression lambda:
                CollectReferencesFromExpression(
                    lambda.Lambda.Expression.Value,
                    ownModuleKey,
                    byModuleAndName,
                    emit);

                break;

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                CollectReferencesFromExpression(paren.Expression.Value, ownModuleKey, byModuleAndName, emit);
                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                CollectReferencesFromExpression(ifBlock.Condition.Value, ownModuleKey, byModuleAndName, emit);
                CollectReferencesFromExpression(ifBlock.ThenBlock.Value, ownModuleKey, byModuleAndName, emit);
                CollectReferencesFromExpression(ifBlock.ElseBlock.Value, ownModuleKey, byModuleAndName, emit);
                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                CollectReferencesFromExpression(
                    caseExpr.CaseBlock.Expression.Value,
                    ownModuleKey,
                    byModuleAndName,
                    emit);

                foreach (var caseEntry in caseExpr.CaseBlock.Cases)
                    CollectReferencesFromExpression(caseEntry.Expression.Value, ownModuleKey, byModuleAndName, emit);

                break;

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var element in listExpr.Elements)
                    CollectReferencesFromExpression(element.Value, ownModuleKey, byModuleAndName, emit);

                break;

            case SyntaxTypes.Expression.TupledExpression tupled:
                foreach (var element in tupled.Elements)
                    CollectReferencesFromExpression(element.Value, ownModuleKey, byModuleAndName, emit);

                break;

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                    CollectReferencesFromExpression(field.Value.valueExpr.Value, ownModuleKey, byModuleAndName, emit);

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields)
                    CollectReferencesFromExpression(field.Value.valueExpr.Value, ownModuleKey, byModuleAndName, emit);

                break;

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                CollectReferencesFromExpression(recordAccess.Record.Value, ownModuleKey, byModuleAndName, emit);
                break;

            case SyntaxTypes.Expression.Negation negation:
                CollectReferencesFromExpression(negation.Expression.Value, ownModuleKey, byModuleAndName, emit);
                break;

            // Leaf variants without nested Expression children — and
            // RecordAccessFunction / PrefixOperator which do not bind to
            // a top-level declaration we can resolve syntactically.
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.Literal:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.Hex:
            case SyntaxTypes.Expression.Floatable:
            case SyntaxTypes.Expression.GLSLExpression:
                break;

            default:
                throw new System.NotImplementedException(
                    "CollectReferencesFromExpression does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }

    /// <summary>
    /// Convenience overload that parses and canonicalizes the supplied Elm
    /// module texts, then runs
    /// <see cref="FindOptimizationOpportunities(IReadOnlyDictionary{DeclQualifiedName, SyntaxTypes.Declaration},
    /// ImmutableHashSet{string}?, ImmutableHashSet{string}?,
    /// ImmutableHashSet{string}?, ImmutableHashSet{string}?,
    /// ImmutableHashSet{string}?, ImmutableHashSet{string}?,
    /// ImmutableHashSet{string}?)"/> on the resulting flat declaration
    /// dictionary. Useful for short, inline test scenarios.
    /// </summary>
    public static ImmutableHashSet<Opportunity> FindOptimizationOpportunities(
        IReadOnlyList<string> elmModulesTexts,
        ImmutableHashSet<string>? ignoreRecordOperation = null,
        ImmutableHashSet<string>? ignoreBasicsArithmetic = null,
        ImmutableHashSet<string>? ignoreBasicsCompare = null,
        ImmutableHashSet<string>? ignoreBasicsEq = null,
        ImmutableHashSet<string>? ignoreBasicsAppend = null,
        ImmutableHashSet<string>? ignorePartialApplication = null,
        ImmutableHashSet<string>? ignoreHigherOrderParameter = null,
        ImmutableHashSet<string>? ignoreRootLevelChoiceTagWrapper = null,
        IReadOnlyCollection<DeclQualifiedName>? restrictToReachableFromEntryPoints = null)
    {
        var declarations = ParseAndCanonicalizeToFlatDict(elmModulesTexts);

        return
            FindOptimizationOpportunities(
                declarations,
                ignoreRecordOperation,
                ignoreBasicsArithmetic,
                ignoreBasicsCompare,
                ignoreBasicsEq,
                ignoreBasicsAppend,
                ignorePartialApplication,
                ignoreHigherOrderParameter,
                ignoreRootLevelChoiceTagWrapper,
                restrictToReachableFromEntryPoints);
    }

    /// <summary>
    /// Renders an unordered set of <see cref="Opportunity"/> values as a
    /// deterministic, line-oriented string suitable for snapshot
    /// assertions. Findings are sorted by containing declaration name,
    /// then by category, then by description; each finding occupies one
    /// line in the format
    /// <c>"<![CDATA[<Module.decl>: <category>: <description>]]>"</c>.
    /// </summary>
    public static string RenderOpportunities(IEnumerable<Opportunity> opportunities)
    {
        var sorted =
            opportunities
            .Distinct()
            .OrderBy(o => o)
            .ToList();

        var sb = new StringBuilder();

        for (var i = 0; i < sorted.Count; i++)
        {
            if (i > 0)
                sb.Append('\n');

            sb.Append(sorted[i].ContainingDecl.FullName);
            sb.Append(": ");
            sb.Append(OpportunityCategoryFormatting.ToDisplayName(sorted[i].Category));
            sb.Append(": ");
            sb.Append(sorted[i].Description);
        }

        return sb.ToString();
    }

    /// <summary>
    /// Renders an unordered set of <see cref="Opportunity"/> values grouped
    /// by <see cref="Opportunity.Category"/>. Each group starts with a
    /// header line of the form <c>"<![CDATA[<category>:]]>"</c> followed by
    /// one indented line per finding in the form
    /// <c>"<![CDATA[  <Module.decl>: <description>]]>"</c>. Groups are
    /// emitted in the declared order of <see cref="OpportunityCategory"/>;
    /// within a group findings are sorted by containing declaration then
    /// by description. Categories with no findings are omitted entirely.
    /// Groups are separated by a single blank line.
    /// </summary>
    public static string RenderOpportunitiesByCategory(IEnumerable<Opportunity> opportunities)
    {
        var distinct =
            opportunities.Distinct().ToList();

        var sb = new StringBuilder();
        var firstGroup = true;

        foreach (OpportunityCategory category in System.Enum.GetValues(typeof(OpportunityCategory)))
        {
            var inCategory =
                distinct
                .Where(o => o.Category == category)
                .OrderBy(o => o.ContainingDecl)
                .ThenBy(o => o.Description, System.StringComparer.Ordinal)
                .ToList();

            if (inCategory.Count is 0)
                continue;

            if (!firstGroup)
                sb.Append("\n\n");

            firstGroup = false;

            sb.Append(OpportunityCategoryFormatting.ToDisplayName(category));
            sb.Append(':');

            foreach (var entry in inCategory)
            {
                sb.Append('\n');
                sb.Append("  ");
                sb.Append(entry.ContainingDecl.FullName);
                sb.Append(": ");
                sb.Append(entry.Description);
            }
        }

        return sb.ToString();
    }

    private static void CollectFromExpression(
        SyntaxTypes.Expression expression,
        DeclQualifiedName containing,
        IReadOnlyDictionary<OpportunityCategory, ImmutableHashSet<string>> whitelistByCategory,
        IReadOnlyDictionary<DeclQualifiedName, int> topLevelArity,
        ImmutableDictionary<string, int> letScope,
        ImmutableHashSet<string> functionTypedParameterNames,
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
                MaybeAdd(
                    OpportunityCategory.RecordAccess,
                    recordAccess.FieldName.Value,
                    containing,
                    whitelistByCategory,
                    resultBuilder);

                CollectFromExpression(
                    recordAccess.Record.Value,
                    containing,
                    whitelistByCategory,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    resultBuilder);

                break;

            case SyntaxTypes.Expression.RecordAccessFunction recordAccessFunction:
                MaybeAdd(
                    OpportunityCategory.RecordAccess,
                    TrimLeadingDot(recordAccessFunction.FunctionName),
                    containing,
                    whitelistByCategory,
                    resultBuilder);

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields)
                {
                    MaybeAdd(
                        OpportunityCategory.RecordUpdate,
                        field.Value.fieldName.Value,
                        containing,
                        whitelistByCategory,
                        resultBuilder);

                    CollectFromExpression(
                        field.Value.valueExpr.Value,
                        containing,
                        whitelistByCategory,
                        topLevelArity,
                        letScope,
                        functionTypedParameterNames,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.FunctionOrValue funcOrValue:
                if (funcOrValue.ModuleName.Count is 1 &&
                    funcOrValue.ModuleName[0] is "Basics" &&
                    s_basicsFunctionToCategory.TryGetValue(funcOrValue.Name, out var funcCategory))
                {
                    MaybeAdd(
                        funcCategory,
                        funcOrValue.Name,
                        containing,
                        whitelistByCategory,
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
                        whitelistByCategory,
                        resultBuilder);
                }

                CollectFromExpression(
                    opApp.Left.Value,
                    containing,
                    whitelistByCategory,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    resultBuilder);

                CollectFromExpression(
                    opApp.Right.Value,
                    containing,
                    whitelistByCategory,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    resultBuilder);

                break;

            case SyntaxTypes.Expression.PrefixOperator prefixOp:
                if (s_basicsOperatorToCategory.TryGetValue(prefixOp.Operator, out var prefixCategory))
                {
                    MaybeAdd(
                        prefixCategory,
                        "(" + prefixOp.Operator + ")",
                        containing,
                        whitelistByCategory,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.Application app:
                MaybeReportPartialApplication(
                    app,
                    containing,
                    whitelistByCategory,
                    topLevelArity,
                    letScope,
                    resultBuilder);

                foreach (var arg in app.Arguments)
                {
                    CollectFromExpression(
                        arg.Value,
                        containing,
                        whitelistByCategory,
                        topLevelArity,
                        letScope,
                        functionTypedParameterNames,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.LetExpression letExpr:

                // Let-bound function declarations introduce new arities that
                // are visible inside the let body and inside sibling let
                // bindings (Elm let-rec semantics).
                var extendedLetScope = letScope;

                foreach (var declNode in letExpr.Value.Declarations)
                {
                    if (declNode.Value is SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
                    {
                        var name = letFunc.Function.Declaration.Value.Name.Value;
                        var arity = letFunc.Function.Declaration.Value.Arguments.Count;

                        extendedLetScope = extendedLetScope.SetItem(name, arity);
                    }
                }

                foreach (var declNode in letExpr.Value.Declarations)
                {
                    switch (declNode.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:

                            // Each let-bound function introduces its own
                            // parameter scope. Higher-order parameter
                            // findings for those parameters are attributed
                            // to the containing top-level declaration but
                            // qualified with the let-function name in the
                            // description so they do not collide with
                            // identically-named outer parameters.
                            var letParamNames =
                                CollectVarPatternNames(
                                    letFunc.Function.Declaration.Value.Arguments);

                            CollectFromExpression(
                                letFunc.Function.Declaration.Value.Expression.Value,
                                containing,
                                whitelistByCategory,
                                topLevelArity,
                                extendedLetScope,
                                // The outer function's parameters are still
                                // in lexical scope unless shadowed; the
                                // simple name-based check here intentionally
                                // accepts that shadowing is rare and the
                                // false-positive rate stays low for the
                                // current test corpus.
                                functionTypedParameterNames,
                                resultBuilder);

                            CollectHigherOrderParameterFindings(
                                letFunc.Function.Declaration.Value.Expression.Value,
                                letParamNames,
                                containing,
                                paramOwnerDescription:
                                letFunc.Function.Declaration.Value.Name.Value,
                                whitelistByCategory,
                                resultBuilder);

                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            CollectFromExpression(
                                letDestr.Expression.Value,
                                containing,
                                whitelistByCategory,
                                topLevelArity,
                                extendedLetScope,
                                functionTypedParameterNames,
                                resultBuilder);

                            break;

                        default:
                            throw new System.NotImplementedException(
                                "CollectFromExpression does not handle let declaration variant: " +
                                declNode.Value.GetType().Name);
                    }
                }

                CollectFromExpression(
                    letExpr.Value.Expression.Value,
                    containing,
                    whitelistByCategory,
                    topLevelArity,
                    extendedLetScope,
                    functionTypedParameterNames,
                    resultBuilder);

                break;

            case SyntaxTypes.Expression.LambdaExpression lambda:
                CollectFromExpression(
                    lambda.Lambda.Expression.Value,
                    containing,
                    whitelistByCategory,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    resultBuilder);

                break;

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                CollectFromExpression(
                    paren.Expression.Value,
                    containing,
                    whitelistByCategory,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    resultBuilder);

                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                CollectFromExpression(
                    ifBlock.Condition.Value,
                    containing,
                    whitelistByCategory,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    resultBuilder);

                CollectFromExpression(
                    ifBlock.ThenBlock.Value,
                    containing,
                    whitelistByCategory,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    resultBuilder);

                CollectFromExpression(
                    ifBlock.ElseBlock.Value,
                    containing,
                    whitelistByCategory,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    resultBuilder);

                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                CollectFromExpression(
                    caseExpr.CaseBlock.Expression.Value,
                    containing,
                    whitelistByCategory,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    resultBuilder);

                foreach (var caseEntry in caseExpr.CaseBlock.Cases)
                {
                    CollectFromExpression(
                        caseEntry.Expression.Value,
                        containing,
                        whitelistByCategory,
                        topLevelArity,
                        letScope,
                        functionTypedParameterNames,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var element in listExpr.Elements)
                {
                    CollectFromExpression(
                        element.Value,
                        containing,
                        whitelistByCategory,
                        topLevelArity,
                        letScope,
                        functionTypedParameterNames,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.TupledExpression tupled:
                foreach (var element in tupled.Elements)
                {
                    CollectFromExpression(
                        element.Value,
                        containing,
                        whitelistByCategory,
                        topLevelArity,
                        letScope,
                        functionTypedParameterNames,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                {
                    CollectFromExpression(
                        field.Value.valueExpr.Value,
                        containing,
                        whitelistByCategory,
                        topLevelArity,
                        letScope,
                        functionTypedParameterNames,
                        resultBuilder);
                }

                break;

            case SyntaxTypes.Expression.Negation negation:
                CollectFromExpression(
                    negation.Expression.Value,
                    containing,
                    whitelistByCategory,
                    topLevelArity,
                    letScope,
                    functionTypedParameterNames,
                    resultBuilder);

                break;

            // Leaf variants with no nested Expression children.
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.Literal:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.Hex:
            case SyntaxTypes.Expression.Floatable:
            case SyntaxTypes.Expression.GLSLExpression:
                break;

            default:
                throw new System.NotImplementedException(
                    "CollectFromExpression does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }

    /// <summary>
    /// Inspects an <see cref="SyntaxTypes.Expression.Application"/> node
    /// and, if its head is a statically known function whose arity exceeds
    /// the number of supplied arguments, records a
    /// <see cref="OpportunityCategory.PartialApplication"/> opportunity describing the
    /// added-argument vs parameter-count mismatch.
    /// <para>
    /// The head is "statically known" when it is a
    /// <see cref="SyntaxTypes.Expression.FunctionOrValue"/> resolving to a
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
        IReadOnlyDictionary<OpportunityCategory, ImmutableHashSet<string>> whitelistByCategory,
        IReadOnlyDictionary<DeclQualifiedName, int> topLevelArity,
        ImmutableDictionary<string, int> letScope,
        ImmutableHashSet<Opportunity>.Builder resultBuilder)
    {
        if (app.Arguments.Count < 2)
        {
            // Not actually applying any arguments — just a head reference.
            // Such forms are normally already simplified away by
            // canonicalization but we guard defensively.
            return;
        }

        var head = UnwrapParen(app.Arguments[0].Value);

        var addedArguments = app.Arguments.Count - 1;

        switch (head)
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrValue:
                {
                    int? arity = null;
                    string? displayName = null;

                    if (funcOrValue.ModuleName.Count > 0)
                    {
                        var qualified =
                            new DeclQualifiedName(funcOrValue.ModuleName, funcOrValue.Name);

                        if (topLevelArity.TryGetValue(qualified, out var topArity))
                        {
                            arity = topArity;
                            displayName = qualified.FullName;
                        }
                    }
                    else if (letScope.TryGetValue(funcOrValue.Name, out var letArity))
                    {
                        arity = letArity;
                        displayName = funcOrValue.Name;
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
                            whitelistByCategory,
                            resultBuilder);
                    }
                }

                break;

            case SyntaxTypes.Expression.PrefixOperator prefixOp:
                {
                    // Source-level binary operators always have arity 2.
                    const int operatorArity = 2;

                    if (addedArguments < operatorArity)
                    {
                        MaybeAdd(
                            OpportunityCategory.PartialApplication,
                            "(" + prefixOp.Operator + ")" +
                            "(" + addedArguments + "/" + operatorArity + ")",
                            containing,
                            whitelistByCategory,
                            resultBuilder);
                    }
                }

                break;
        }
    }

    /// <summary>
    /// Walks <paramref name="body"/> and records one
    /// <see cref="OpportunityCategory.HigherOrderParameter"/> finding per
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
        IReadOnlyDictionary<OpportunityCategory, ImmutableHashSet<string>> whitelistByCategory,
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
                OpportunityCategory.HigherOrderParameter,
                description,
                containing,
                whitelistByCategory,
                resultBuilder);
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
                if (app.Arguments.Count >= 1)
                {
                    var head = UnwrapParen(app.Arguments[0].Value);

                    if (head is SyntaxTypes.Expression.FunctionOrValue funcOrValue &&
                        funcOrValue.ModuleName.Count is 0 &&
                        paramNames.Contains(funcOrValue.Name))
                    {
                        found.Add(funcOrValue.Name);
                    }
                    else if (head is SyntaxTypes.Expression.RecordAccess recordAccessHead &&
                        TryRenderRecordAccessChainRootedAtParam(
                                 recordAccessHead, paramNames) is { } chainPath)
                    {
                        found.Add(chainPath);
                    }
                }

                foreach (var arg in app.Arguments)
                    FindAppliedParameterNames(arg.Value, paramNames, found);

                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                FindAppliedParameterNames(opApp.Left.Value, paramNames, found);
                FindAppliedParameterNames(opApp.Right.Value, paramNames, found);
                break;

            case SyntaxTypes.Expression.LetExpression letExpr:

                // Names bound by `let` destructuring patterns extend the
                // visible-parameter set for the let body — when one of these
                // bound names is itself function-typed and gets applied
                // somewhere in the body, that points to the same kind of
                // higher-order opportunity as a directly-named parameter.
                var letDestructuredNames = ImmutableHashSet.CreateBuilder<string>();

                foreach (var declNode in letExpr.Value.Declarations)
                {
                    switch (declNode.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:

                            // The let-bound function's own parameters
                            // shadow any outer parameters of the same name,
                            // so remove them from the set before descending.
                            var letParamNames =
                                CollectVarPatternNames(
                                    letFunc.Function.Declaration.Value.Arguments);

                            var visibleHere =
                                paramNames.Except(letParamNames);

                            FindAppliedParameterNames(
                                letFunc.Function.Declaration.Value.Expression.Value,
                                visibleHere,
                                found);

                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            FindAppliedParameterNames(letDestr.Expression.Value, paramNames, found);

                            CollectVarPatternNamesFromPattern(
                                letDestr.Pattern.Value,
                                letDestructuredNames);

                            break;

                        default:
                            throw new System.NotImplementedException(
                                "FindAppliedParameterNames does not handle let declaration variant: " +
                                declNode.Value.GetType().Name);
                    }
                }

                FindAppliedParameterNames(
                    letExpr.Value.Expression.Value,
                    paramNames.Union(letDestructuredNames),
                    found);

                break;

            case SyntaxTypes.Expression.LambdaExpression lambda:
                var lambdaParams =
                    CollectVarPatternNames(lambda.Lambda.Arguments);

                FindAppliedParameterNames(
                    lambda.Lambda.Expression.Value,
                    paramNames.Except(lambdaParams),
                    found);

                break;

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                FindAppliedParameterNames(paren.Expression.Value, paramNames, found);
                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                FindAppliedParameterNames(ifBlock.Condition.Value, paramNames, found);
                FindAppliedParameterNames(ifBlock.ThenBlock.Value, paramNames, found);
                FindAppliedParameterNames(ifBlock.ElseBlock.Value, paramNames, found);
                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                FindAppliedParameterNames(caseExpr.CaseBlock.Expression.Value, paramNames, found);

                foreach (var caseEntry in caseExpr.CaseBlock.Cases)
                {
                    // Names bound by this branch's pattern extend the
                    // visible-parameter set for the branch body. The
                    // surrounding outer parameters remain visible too,
                    // unless the pattern shadows one (we conservatively
                    // keep both — Elm forbids shadowing in patterns).
                    var branchBoundBuilder = ImmutableHashSet.CreateBuilder<string>();

                    CollectVarPatternNamesFromPattern(
                        caseEntry.Pattern.Value,
                        branchBoundBuilder);

                    FindAppliedParameterNames(
                        caseEntry.Expression.Value,
                        paramNames.Union(branchBoundBuilder),
                        found);
                }

                break;

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var element in listExpr.Elements)
                    FindAppliedParameterNames(element.Value, paramNames, found);

                break;

            case SyntaxTypes.Expression.TupledExpression tupled:
                foreach (var element in tupled.Elements)
                    FindAppliedParameterNames(element.Value, paramNames, found);

                break;

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                    FindAppliedParameterNames(field.Value.valueExpr.Value, paramNames, found);

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields)
                    FindAppliedParameterNames(field.Value.valueExpr.Value, paramNames, found);

                break;

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                FindAppliedParameterNames(recordAccess.Record.Value, paramNames, found);
                break;

            case SyntaxTypes.Expression.Negation negation:
                FindAppliedParameterNames(negation.Expression.Value, paramNames, found);
                break;

            case SyntaxTypes.Expression.FunctionOrValue:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.Literal:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.Hex:
            case SyntaxTypes.Expression.Floatable:
            case SyntaxTypes.Expression.GLSLExpression:
                break;

            default:
                throw new System.NotImplementedException(
                    "FindAppliedParameterNames does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }

    /// <summary>
    /// Returns the set of variable-pattern names introduced by a function's
    /// or lambda's argument-pattern list. Non-variable patterns (tuple
    /// destructuring, named patterns, etc.) are skipped — they are still
    /// in scope but are not addressable by simple-name lookup so they
    /// cannot be applied as the head of an <c>Application</c>.
    /// </summary>
    private static ImmutableHashSet<string> CollectVarPatternNames(
        IReadOnlyList<SyntaxModel.Node<SyntaxTypes.Pattern>> arguments)
    {
        var builder = ImmutableHashSet.CreateBuilder<string>();

        foreach (var argNode in arguments)
        {
            CollectVarPatternNamesFromPattern(argNode.Value, builder);
        }

        return builder.ToImmutable();
    }

    private static void CollectVarPatternNamesFromPattern(
        SyntaxTypes.Pattern pattern,
        ImmutableHashSet<string>.Builder builder)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPattern:
                builder.Add(varPattern.Name);
                break;

            case SyntaxTypes.Pattern.ParenthesizedPattern paren:
                CollectVarPatternNamesFromPattern(paren.Pattern.Value, builder);
                break;

            case SyntaxTypes.Pattern.AsPattern asPattern:
                builder.Add(asPattern.Name.Value);
                CollectVarPatternNamesFromPattern(asPattern.Pattern.Value, builder);
                break;

            case SyntaxTypes.Pattern.NamedPattern namedPattern:

                // A named-constructor pattern like `(Wrap inner)` binds the
                // names from its argument patterns. The constructor itself
                // does not introduce a name. A type-checking program can
                // only reach this shape — outside of a `case` branch — when
                // the value is of a single-tag type, so the bound names are
                // always in scope.
                foreach (var argNode in namedPattern.Arguments)
                    CollectVarPatternNamesFromPattern(argNode.Value, builder);

                break;

            case SyntaxTypes.Pattern.TuplePattern tuplePattern:
                foreach (var elementNode in tuplePattern.Elements)
                    CollectVarPatternNamesFromPattern(elementNode.Value, builder);

                break;

            case SyntaxTypes.Pattern.RecordPattern recordPattern:

                // A record pattern `{x, y}` binds the field names directly
                // as local names.
                foreach (var fieldNode in recordPattern.Fields)
                    builder.Add(fieldNode.Value);

                break;

            case SyntaxTypes.Pattern.UnConsPattern unCons:
                CollectVarPatternNamesFromPattern(unCons.Head.Value, builder);
                CollectVarPatternNamesFromPattern(unCons.Tail.Value, builder);
                break;

            case SyntaxTypes.Pattern.ListPattern listPattern:
                foreach (var elementNode in listPattern.Elements)
                    CollectVarPatternNamesFromPattern(elementNode.Value, builder);

                break;

            // Other pattern variants do not bind names (literals, AllPattern,
            // UnitPattern, etc.).
            default:
                break;
        }
    }

    private static SyntaxTypes.Expression UnwrapParen(SyntaxTypes.Expression expression)
    {
        while (expression is SyntaxTypes.Expression.ParenthesizedExpression paren)
        {
            expression = paren.Expression.Value;
        }

        return expression;
    }

    /// <summary>
    /// If <paramref name="recordAccess"/> is a chain
    /// <c>p.f1.f2.…fn</c> whose innermost record expression is a bare
    /// <see cref="SyntaxTypes.Expression.FunctionOrValue"/> with module
    /// part empty and name in <paramref name="paramNames"/>, returns
    /// <c>"p.f1.f2.…fn"</c>; otherwise returns <c>null</c>. Parentheses
    /// around the record expression at any level are tolerated.
    /// </summary>
    private static string? TryRenderRecordAccessChainRootedAtParam(
        SyntaxTypes.Expression.RecordAccess recordAccess,
        ImmutableHashSet<string> paramNames)
    {
        var fields = new List<string>();
        SyntaxTypes.Expression current = recordAccess;

        while (UnwrapParen(current) is SyntaxTypes.Expression.RecordAccess ra)
        {
            fields.Add(ra.FieldName.Value);
            current = ra.Record.Value;
        }

        if (UnwrapParen(current) is not SyntaxTypes.Expression.FunctionOrValue rootFov)
            return null;

        if (rootFov.ModuleName.Count is not 0)
            return null;

        if (!paramNames.Contains(rootFov.Name))
            return null;

        // `fields` was built from outermost to innermost; reverse so the
        // rendered chain reads root-to-leaf.
        fields.Reverse();

        return rootFov.Name + "." + string.Join(".", fields);
    }

    private static string TrimLeadingDot(string name) =>
        name.Length > 0 && name[0] is '.' ? name[1..] : name;

    private static void MaybeAdd(
        OpportunityCategory category,
        string description,
        DeclQualifiedName containing,
        IReadOnlyDictionary<OpportunityCategory, ImmutableHashSet<string>> whitelistByCategory,
        ImmutableHashSet<Opportunity>.Builder resultBuilder)
    {
        var whitelist = whitelistByCategory[category];

        var fullName = containing.FullName;

        foreach (var prefix in whitelist)
        {
            if (fullName.StartsWith(prefix, System.StringComparison.Ordinal))
                return;
        }

        resultBuilder.Add(new Opportunity(containing, category, description));
    }

    private static IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        ParseAndCanonicalizeToFlatDict(IReadOnlyList<string> elmModulesTexts)
    {
        var parsedModules =
            elmModulesTexts
            .Select(
                text =>
                ElmSyntaxParser.ParseModuleText(text)
                .Extract(err => throw new System.Exception("Failed parsing: " + err)))
            .Select(SyntaxTypes.FromFullSyntaxModel.Convert)
            .ToList();

        var canonicalized =
            Canonicalization.Canonicalize(parsedModules)
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        var orderedCanonicalizedModules =
            parsedModules
            .Select(
                module =>
                canonicalized[SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value]
                .Extract(
                    err =>
                    throw new System.Exception(
                        "Module " +
                        string.Join(".", SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value) +
                        " has errors: " + err)))
            .ToList();

        return ElmCompiler.FlattenModulesToDeclarationDictionary(orderedCanonicalizedModules);
    }

    /// <summary>
    /// Information about a single-tag (one-constructor) custom type used
    /// by <see cref="OpportunityCategory.RootLevelChoiceTagWrapper"/> detection.
    /// Carries the constructor's argument types and the type's generics
    /// so detection sites can substitute generic type variables when
    /// rendering the unwrapped type from a type annotation that supplies
    /// concrete type arguments.
    /// </summary>
    private sealed record SingleTagShapeInfo(
        DeclQualifiedName TypeName,
        DeclQualifiedName ConstructorName,
        IReadOnlyList<string> TypeGenerics,
        IReadOnlyList<SyntaxTypes.TypeAnnotation> ConstructorArgumentTypes);

    /// <summary>
    /// Builds a registry of every custom type in <paramref name="declarations"/>
    /// that has exactly one constructor (the constructor itself may have
    /// any number of arguments, including zero). Both the type's
    /// qualified name and the constructor's qualified name are mapped to
    /// the same <see cref="SingleTagShapeInfo"/> so detection sites can
    /// resolve from either direction.
    /// </summary>
    private static ImmutableDictionary<DeclQualifiedName, SingleTagShapeInfo>
        BuildSingleTagRegistry(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SingleTagShapeInfo>();

        foreach (var (declName, decl) in declarations)
        {
            if (decl is not SyntaxTypes.Declaration.CustomTypeDeclaration ctd)
                continue;

            if (ctd.TypeDeclaration.Constructors.Count is not 1)
                continue;

            var ctor = ctd.TypeDeclaration.Constructors[0].Value;

            var typeName =
                new DeclQualifiedName(declName.Namespaces, ctd.TypeDeclaration.Name.Value);

            var ctorName =
                new DeclQualifiedName(declName.Namespaces, ctor.Name.Value);

            var generics =
                ctd.TypeDeclaration.Generics
                .Select(g => g.Value)
                .ToList();

            var ctorArgs =
                ctor.Arguments
                .Select(a => a.Value)
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
        IReadOnlyDictionary<OpportunityCategory, ImmutableHashSet<string>> whitelistByCategory,
        ImmutableHashSet<Opportunity>.Builder resultBuilder)
    {
        if (singleTagRegistry.IsEmpty)
            return;

        var ownModule = containing.Namespaces;

        var implementation = function.Declaration.Value;

        // Walk the type signature into a list of parameter type
        // annotations + a single return type annotation. A function
        // without a signature contributes an empty list and a null
        // return type.
        var sigParamTypes = new List<SyntaxTypes.TypeAnnotation?>();
        SyntaxTypes.TypeAnnotation? sigReturnType = null;

        if (function.Signature is { } signatureNode)
        {
            DecomposeFunctionSignature(
                signatureNode.Value.TypeAnnotation.Value,
                implementation.Arguments.Count,
                sigParamTypes,
                out sigReturnType);
        }

        // Per-parameter detection.
        for (var i = 0; i < implementation.Arguments.Count; i++)
        {
            var paramPattern = implementation.Arguments[i].Value;
            var paramName = TryGetVarOrAsName(paramPattern);

            // 1. Signature-based.
            SingleTagShapeInfo? matchedFromSig = null;
            string? unwrappedFromSig = null;

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
                        implementation.Expression.Value,
                        paramName,
                        singleTagRegistry,
                        ownModule);
            }

            var anyMatch = matchedFromSig ?? matchedFromPattern ?? matchedFromLet;

            if (anyMatch is null)
                continue;

            // Prefer the signature-derived "unwrapped" rendering when
            // available because it has the actual concrete type
            // arguments substituted; otherwise fall back to the
            // constructor's argument types as declared.
            var unwrapped =
                unwrappedFromSig ?? RenderUnwrappedFromShape(anyMatch);

            var description =
                "parameter[" + i + "] " + (paramName ?? "_") + ": " +
                anyMatch.ConstructorName.FullName + " -> " + ParenIfTopLevelArrow(unwrapped);

            MaybeAdd(
                OpportunityCategory.RootLevelChoiceTagWrapper,
                description,
                containing,
                whitelistByCategory,
                resultBuilder);
        }

        // Return-value detection.
        SingleTagShapeInfo? returnMatched = null;
        string? returnUnwrapped = null;

        if (sigReturnType is not null)
        {
            var (sigInfo, sigUnwrapped) =
                TryResolveSingleTagWrap(sigReturnType, singleTagRegistry, ownModule);

            returnMatched = sigInfo;
            returnUnwrapped = sigUnwrapped;
        }

        if (returnMatched is null)
        {
            returnMatched =
                TryMatchSingleTagFromAllReturnLeaves(
                    implementation.Expression.Value,
                    singleTagRegistry,
                    ownModule);
        }

        if (returnMatched is not null)
        {
            var unwrappedReturn =
                returnUnwrapped ?? RenderUnwrappedFromShape(returnMatched);

            MaybeAdd(
                OpportunityCategory.RootLevelChoiceTagWrapper,
                "return: " + returnMatched.ConstructorName.FullName + " -> " + ParenIfTopLevelArrow(unwrappedReturn),
                containing,
                whitelistByCategory,
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
    private static void DecomposeFunctionSignature(
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
                sigParamTypes.Add(fta.ArgumentType.Value);
                current = fta.ReturnType.Value;
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
    /// type annotation when (after peeling parens) it is a
    /// <see cref="SyntaxTypes.TypeAnnotation.Typed"/> reference to a
    /// single-tag custom type, plus a textual rendering of the unwrapped
    /// type with generic substitution applied. Returns
    /// <c>(null, null)</c> for any other annotation shape.
    /// </summary>
    private static (SingleTagShapeInfo? Info, string? UnwrappedRendered)
        TryResolveSingleTagWrap(
        SyntaxTypes.TypeAnnotation annotation,
        ImmutableDictionary<DeclQualifiedName, SingleTagShapeInfo> singleTagRegistry,
        ModuleName ownModule)
    {
        if (annotation is not SyntaxTypes.TypeAnnotation.Typed typed)
            return (null, null);

        var typeRef = typed.TypeName.Value;

        var qualified =
            typeRef.ModuleName.Count > 0
            ?
            new DeclQualifiedName(typeRef.ModuleName, typeRef.Name)
            :
            new DeclQualifiedName(ownModule, typeRef.Name);

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
                substitution[info.TypeGenerics[i]] = typed.TypeArguments[i].Value;
            }
        }

        var substitutedArgs =
            info.ConstructorArgumentTypes
            .Select(a => SubstituteGenerics(a, substitution))
            .ToList();

        var unwrapped = RenderUnwrappedTypeAnnotations(substitutedArgs);

        return (info, unwrapped);
    }

    /// <summary>
    /// Returns the <see cref="SingleTagShapeInfo"/> implied by a
    /// parameter or destructuring pattern when (after peeling parens
    /// and as-patterns) the pattern is a
    /// <see cref="SyntaxTypes.Pattern.NamedPattern"/> whose constructor
    /// resolves to a single-tag constructor; otherwise <c>null</c>.
    /// </summary>
    private static SingleTagShapeInfo? TryMatchSingleTagFromPattern(
        SyntaxTypes.Pattern pattern,
        ImmutableDictionary<DeclQualifiedName, SingleTagShapeInfo> singleTagRegistry,
        ModuleName ownModule)
    {
        var peeled = PeelPatternWrappers(pattern);

        if (peeled is not SyntaxTypes.Pattern.NamedPattern named)
            return null;

        var qualified =
            named.Name.ModuleName.Count > 0
            ?
            new DeclQualifiedName(named.Name.ModuleName, named.Name.Name)
            :
            new DeclQualifiedName(ownModule, named.Name.Name);

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
    /// <see cref="SyntaxTypes.Expression.LetDeclaration.LetDestructuring"/>
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
        var current = PeelExpressionParens(body);

        while (current is SyntaxTypes.Expression.LetExpression letExpr)
        {
            foreach (var declNode in letExpr.Value.Declarations)
            {
                if (declNode.Value is not SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
                    continue;

                var rhs = PeelExpressionParens(letDestr.Expression.Value);

                if (rhs is not SyntaxTypes.Expression.FunctionOrValue rhsRef)
                    continue;

                if (rhsRef.ModuleName.Count is not 0 || rhsRef.Name != paramName)
                    continue;

                var match =
                    TryMatchSingleTagFromPattern(
                        letDestr.Pattern.Value,
                        singleTagRegistry,
                        ownModule);

                if (match is not null)
                    return match;
            }

            current = PeelExpressionParens(letExpr.Value.Expression.Value);
        }

        return null;
    }

    /// <summary>
    /// Returns the single-tag <see cref="SingleTagShapeInfo"/> that
    /// every "return leaf" of <paramref name="body"/> wraps with at
    /// the root, when this is consistent across every leaf; otherwise
    /// <c>null</c>. Return leaves are followed across
    /// <see cref="SyntaxTypes.Expression.LetExpression"/>,
    /// <see cref="SyntaxTypes.Expression.IfBlock"/>,
    /// <see cref="SyntaxTypes.Expression.CaseExpression"/>, and
    /// <see cref="SyntaxTypes.Expression.ParenthesizedExpression"/>.
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
        var peeled = PeelExpressionParens(expression);

        switch (peeled)
        {
            case SyntaxTypes.Expression.LetExpression letExpr:
                return
                    AllReturnLeavesAgreeOnSingleTagCtor(
                        letExpr.Value.Expression.Value,
                        singleTagRegistry,
                        ownModule,
                        ref agreed);

            case SyntaxTypes.Expression.IfBlock ifBlock:
                return
                    AllReturnLeavesAgreeOnSingleTagCtor(
                        ifBlock.ThenBlock.Value,
                        singleTagRegistry,
                        ownModule,
                        ref agreed) &&
                    AllReturnLeavesAgreeOnSingleTagCtor(
                        ifBlock.ElseBlock.Value,
                        singleTagRegistry,
                        ownModule,
                        ref agreed);

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                if (caseExpr.CaseBlock.Cases.Count is 0)
                    return false;

                foreach (var arm in caseExpr.CaseBlock.Cases)
                {
                    if (!AllReturnLeavesAgreeOnSingleTagCtor(
                            arm.Expression.Value,
                            singleTagRegistry,
                            ownModule,
                            ref agreed))
                    {
                        return false;
                    }
                }

                return true;

            case SyntaxTypes.Expression.Application app:
                if (app.Arguments.Count < 2)
                    return false;

                var head = PeelExpressionParens(app.Arguments[0].Value);

                if (head is not SyntaxTypes.Expression.FunctionOrValue funcOrValue)
                    return false;

                var qualified =
                    funcOrValue.ModuleName.Count > 0
                    ?
                    new DeclQualifiedName(funcOrValue.ModuleName, funcOrValue.Name)
                    :
                    new DeclQualifiedName(ownModule, funcOrValue.Name);

                if (!singleTagRegistry.TryGetValue(qualified, out var info))
                    return false;

                if (!info.ConstructorName.Equals(qualified))
                    return false;

                // The application must supply exactly one positional
                // argument per constructor field (Elm constructors are
                // applied uncurried at the source level).
                var suppliedArgCount = app.Arguments.Count - 1;

                if (suppliedArgCount != info.ConstructorArgumentTypes.Count)
                    return false;

                if (agreed is null)
                {
                    agreed = info;
                    return true;
                }

                return agreed.ConstructorName.Equals(info.ConstructorName);

            default:
                return false;
        }
    }

    /// <summary>
    /// Peels nested
    /// <see cref="SyntaxTypes.Expression.ParenthesizedExpression"/>
    /// layers from <paramref name="expression"/>.
    /// </summary>
    private static SyntaxTypes.Expression PeelExpressionParens(SyntaxTypes.Expression expression)
    {
        while (expression is SyntaxTypes.Expression.ParenthesizedExpression p)
            expression = p.Expression.Value;

        return expression;
    }

    /// <summary>
    /// Peels nested
    /// <see cref="SyntaxTypes.Pattern.ParenthesizedPattern"/> and
    /// <see cref="SyntaxTypes.Pattern.AsPattern"/> layers from
    /// <paramref name="pattern"/>.
    /// </summary>
    private static SyntaxTypes.Pattern PeelPatternWrappers(SyntaxTypes.Pattern pattern)
    {
        while (true)
        {
            switch (pattern)
            {
                case SyntaxTypes.Pattern.ParenthesizedPattern p:
                    pattern = p.Pattern.Value;
                    continue;

                case SyntaxTypes.Pattern.AsPattern a:
                    pattern = a.Pattern.Value;
                    continue;

                default:
                    return pattern;
            }
        }
    }

    /// <summary>
    /// Returns the bound name of a parameter pattern that is most useful
    /// for display. Recognises:
    /// <list type="bullet">
    /// <item>A bare <see cref="SyntaxTypes.Pattern.VarPattern"/> (the
    /// pattern's own name).</item>
    /// <item>An <see cref="SyntaxTypes.Pattern.AsPattern"/> (the
    /// <c>as</c>-name).</item>
    /// <item>A <see cref="SyntaxTypes.Pattern.NamedPattern"/> with a
    /// single variable argument (the inner variable's name; this is
    /// the destructuring shape <c>(Ctor inner)</c>).</item>
    /// <item>Any of the above wrapped in
    /// <see cref="SyntaxTypes.Pattern.ParenthesizedPattern"/>.</item>
    /// </list>
    /// Returns <c>null</c> for any other pattern shape.
    /// </summary>
    private static string? TryGetVarOrAsName(SyntaxTypes.Pattern pattern)
    {
        while (true)
        {
            switch (pattern)
            {
                case SyntaxTypes.Pattern.VarPattern vp:
                    return vp.Name;

                case SyntaxTypes.Pattern.AsPattern ap:
                    return ap.Name.Value;

                case SyntaxTypes.Pattern.ParenthesizedPattern pp:
                    pattern = pp.Pattern.Value;
                    continue;

                case SyntaxTypes.Pattern.NamedPattern np when np.Arguments.Count is 1:
                    pattern = np.Arguments[0].Value;
                    continue;

                default:
                    return null;
            }
        }
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
                        .Select(
                            arg =>
                            new SyntaxModel.Node<SyntaxTypes.TypeAnnotation>(
                                arg.Range,
                                SubstituteGenerics(arg.Value, substitution)))
                        .ToList();

                    return new SyntaxTypes.TypeAnnotation.Typed(t.TypeName, newArgs);
                }

            case SyntaxTypes.TypeAnnotation.Unit:
                return annotation;

            case SyntaxTypes.TypeAnnotation.Tupled tupled:
                {
                    var newAnnots =
                        tupled.TypeAnnotations
                        .Select(
                            a =>
                            new SyntaxModel.Node<SyntaxTypes.TypeAnnotation>(
                                a.Range,
                                SubstituteGenerics(a.Value, substitution)))
                        .ToList();

                    return new SyntaxTypes.TypeAnnotation.Tupled(newAnnots);
                }

            case SyntaxTypes.TypeAnnotation.Record record:
                {
                    var newFields =
                        record.RecordDefinition.Fields
                        .Select(
                            f =>
                            new SyntaxModel.Node<SyntaxTypes.RecordField>(
                                f.Range,
                                new SyntaxTypes.RecordField(
                                    f.Value.FieldName,
                                    new SyntaxModel.Node<SyntaxTypes.TypeAnnotation>(
                                        f.Value.FieldType.Range,
                                        SubstituteGenerics(f.Value.FieldType.Value, substitution)))))
                        .ToList();

                    return
                        new SyntaxTypes.TypeAnnotation.Record(
                            new SyntaxTypes.RecordDefinition(newFields));
                }

            case SyntaxTypes.TypeAnnotation.GenericRecord gr:
                {
                    var newFields =
                        gr.RecordDefinition.Value.Fields
                        .Select(
                            f =>
                            new SyntaxModel.Node<SyntaxTypes.RecordField>(
                                f.Range,
                                new SyntaxTypes.RecordField(
                                    f.Value.FieldName,
                                    new SyntaxModel.Node<SyntaxTypes.TypeAnnotation>(
                                        f.Value.FieldType.Range,
                                        SubstituteGenerics(f.Value.FieldType.Value, substitution)))))
                        .ToList();

                    return
                        new SyntaxTypes.TypeAnnotation.GenericRecord(
                            gr.GenericName,
                            new SyntaxModel.Node<SyntaxTypes.RecordDefinition>(
                                gr.RecordDefinition.Range,
                                new SyntaxTypes.RecordDefinition(newFields)));
                }

            case SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation fta:
                return
                    new SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation(
                        new SyntaxModel.Node<SyntaxTypes.TypeAnnotation>(
                            fta.ArgumentType.Range,
                            SubstituteGenerics(fta.ArgumentType.Value, substitution)),
                        new SyntaxModel.Node<SyntaxTypes.TypeAnnotation>(
                            fta.ReturnType.Range,
                            SubstituteGenerics(fta.ReturnType.Value, substitution)));

            default:
                throw new System.NotImplementedException(
                    "SubstituteGenerics does not handle TypeAnnotation variant: " +
                    annotation.GetType().Name);
        }
    }

    /// <summary>
    /// Renders the unwrapped type for a single-tag constructor based on
    /// its constructor-argument types in the order they were declared.
    /// 1-arg constructors render as the inner type; N-arg constructors
    /// render as a tuple of the argument types — at this stage of the
    /// compilation tuples of any arity are permissible.
    /// </summary>
    private static string RenderUnwrappedFromShape(SingleTagShapeInfo info) =>
        RenderUnwrappedTypeAnnotations(info.ConstructorArgumentTypes);

    private static string RenderUnwrappedTypeAnnotations(
        IReadOnlyList<SyntaxTypes.TypeAnnotation> annotations)
    {
        if (annotations.Count is 0)
            return "()";

        if (annotations.Count is 1)
            return RenderTypeAnnotation(annotations[0]);

        var sb = new StringBuilder();
        sb.Append('(');

        for (var i = 0; i < annotations.Count; i++)
        {
            if (i > 0)
                sb.Append(", ");

            sb.Append(RenderTypeAnnotation(annotations[i]));
        }

        sb.Append(')');
        return sb.ToString();
    }

    /// <summary>
    /// Returns <paramref name="rendered"/> wrapped in parens when it
    /// itself contains a function arrow at the top level — used by
    /// description and signature rendering so that an unwrapped
    /// function-typed inner does not run into surrounding arrows.
    /// </summary>
    private static string ParenIfTopLevelArrow(string rendered)
    {
        // Cheap top-level scan: the rendered string is well-formed and
        // never contains comments. A " -> " outside any parens means
        // the rendered type is a function arrow at the outer level.
        var depth = 0;

        for (var i = 0; i < rendered.Length; i++)
        {
            var c = rendered[i];

            if (c is '(' or '{')
            {
                depth++;
            }
            else if (c is ')' or '}')
            {
                depth--;
            }
            else if (depth is 0 &&
                c is '-' &&
                i + 1 < rendered.Length &&
                rendered[i + 1] is '>')
            {
                return "(" + rendered + ")";
            }
        }

        return rendered;
    }

    /// <summary>
    /// Stable, snapshot-friendly textual rendering of a type annotation
    /// covering every <see cref="SyntaxTypes.TypeAnnotation"/> variant.
    /// Function arrows, tuple commas and record braces are reproduced
    /// in their canonical surface syntax; nested function arrows on the
    /// left of an outer arrow are parenthesised so that the rendered
    /// form unambiguously matches the source-level grouping.
    /// </summary>
    private static string RenderTypeAnnotation(SyntaxTypes.TypeAnnotation annotation)
    {
        switch (annotation)
        {
            case SyntaxTypes.TypeAnnotation.GenericType g:
                return g.Name;

            case SyntaxTypes.TypeAnnotation.Unit:
                return "()";

            case SyntaxTypes.TypeAnnotation.Typed t:
                {
                    var nameSb = new StringBuilder();

                    foreach (var ns in t.TypeName.Value.ModuleName)
                    {
                        nameSb.Append(ns);
                        nameSb.Append('.');
                    }

                    nameSb.Append(t.TypeName.Value.Name);

                    if (t.TypeArguments.Count is 0)
                        return nameSb.ToString();

                    var sb = new StringBuilder();
                    sb.Append(nameSb);

                    foreach (var arg in t.TypeArguments)
                    {
                        sb.Append(' ');
                        sb.Append(RenderTypeAnnotationParenIfComposite(arg.Value));
                    }

                    return sb.ToString();
                }

            case SyntaxTypes.TypeAnnotation.Tupled tupled:
                {
                    var sb = new StringBuilder();
                    sb.Append('(');

                    for (var i = 0; i < tupled.TypeAnnotations.Count; i++)
                    {
                        if (i > 0)
                            sb.Append(", ");

                        sb.Append(RenderTypeAnnotation(tupled.TypeAnnotations[i].Value));
                    }

                    sb.Append(')');
                    return sb.ToString();
                }

            case SyntaxTypes.TypeAnnotation.Record record:
                return RenderRecordDefinition(record.RecordDefinition);

            case SyntaxTypes.TypeAnnotation.GenericRecord gr:
                {
                    var sb = new StringBuilder();
                    sb.Append("{ ");
                    sb.Append(gr.GenericName.Value);
                    sb.Append(" | ");
                    AppendRecordFields(sb, gr.RecordDefinition.Value);
                    sb.Append(" }");
                    return sb.ToString();
                }

            case SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation fta:
                {
                    var left = RenderTypeAnnotation(fta.ArgumentType.Value);

                    if (fta.ArgumentType.Value is SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation)
                        left = "(" + left + ")";

                    var right = RenderTypeAnnotation(fta.ReturnType.Value);

                    return left + " -> " + right;
                }

            default:
                throw new System.NotImplementedException(
                    "RenderTypeAnnotation does not handle TypeAnnotation variant: " +
                    annotation.GetType().Name);
        }
    }

    private static string RenderRecordDefinition(SyntaxTypes.RecordDefinition def)
    {
        if (def.Fields.Count is 0)
            return "{}";

        var sb = new StringBuilder();
        sb.Append("{ ");
        AppendRecordFields(sb, def);
        sb.Append(" }");
        return sb.ToString();
    }

    private static void AppendRecordFields(StringBuilder sb, SyntaxTypes.RecordDefinition def)
    {
        for (var i = 0; i < def.Fields.Count; i++)
        {
            if (i > 0)
                sb.Append(", ");

            sb.Append(def.Fields[i].Value.FieldName.Value);
            sb.Append(" : ");
            sb.Append(RenderTypeAnnotation(def.Fields[i].Value.FieldType.Value));
        }
    }

    /// <summary>
    /// Renders <paramref name="annotation"/> wrapping it in parens when
    /// it is composite enough that printing it bare next to neighbouring
    /// type-application arguments would be ambiguous.
    /// </summary>
    private static string RenderTypeAnnotationParenIfComposite(SyntaxTypes.TypeAnnotation annotation)
    {
        switch (annotation)
        {
            case SyntaxTypes.TypeAnnotation.GenericType:
            case SyntaxTypes.TypeAnnotation.Unit:
            case SyntaxTypes.TypeAnnotation.Tupled:
            case SyntaxTypes.TypeAnnotation.Record:
                return RenderTypeAnnotation(annotation);

            case SyntaxTypes.TypeAnnotation.Typed t when t.TypeArguments.Count is 0:
                return RenderTypeAnnotation(t);

            default:
                return "(" + RenderTypeAnnotation(annotation) + ")";
        }
    }

    /// <summary>
    /// Produces a transformed function type annotation in which every
    /// root-level wrapping by a single-tag constructor (parameter or
    /// return) has been replaced by the constructor's unwrapped type
    /// (a tuple of constructor-argument types when there is more than
    /// one). Used by tests to assert that a function's signature is
    /// transformed accordingly to the unwrapped type.
    /// <para>
    /// Returns <c>null</c> when no transformation applies — either the
    /// declaration is not a function declaration, the function has no
    /// signature, or no root-level wrapping is detected.
    /// </para>
    /// </summary>
    public static string? TryRenderTransformedSignature(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        DeclQualifiedName functionName)
    {
        if (!declarations.TryGetValue(functionName, out var decl))
            return null;

        if (decl is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            return null;

        if (funcDecl.Function.Signature is not { } signatureNode)
            return null;

        var singleTagRegistry = BuildSingleTagRegistry(declarations);

        if (singleTagRegistry.IsEmpty)
            return RenderTypeAnnotation(signatureNode.Value.TypeAnnotation.Value);

        var ownModule = functionName.Namespaces;
        var implementation = funcDecl.Function.Declaration.Value;
        var paramCount = implementation.Arguments.Count;

        var sigParamTypes = new List<SyntaxTypes.TypeAnnotation?>();
        SyntaxTypes.TypeAnnotation? sigReturnType = null;

        DecomposeFunctionSignature(
            signatureNode.Value.TypeAnnotation.Value,
            paramCount,
            sigParamTypes,
            out sigReturnType);

        var transformedParts = new List<string>();

        for (var i = 0; i < paramCount; i++)
        {
            var paramType = i < sigParamTypes.Count ? sigParamTypes[i] : null;

            transformedParts.Add(
                paramType is null
                ?
                "?"
                :
                RenderTransformedTypeAnnotationAtRoot(
                    paramType,
                    singleTagRegistry,
                    ownModule));
        }

        var renderedReturn =
            sigReturnType is null
            ?
            "?"
            :
            RenderTransformedTypeAnnotationAtRoot(
                sigReturnType,
                singleTagRegistry,
                ownModule);

        var sb = new StringBuilder();

        for (var i = 0; i < transformedParts.Count; i++)
        {
            sb.Append(ParenIfTopLevelArrow(transformedParts[i]));
            sb.Append(" -> ");
        }

        sb.Append(renderedReturn);
        return sb.ToString();
    }

    private static string RenderTransformedTypeAnnotationAtRoot(
        SyntaxTypes.TypeAnnotation annotation,
        ImmutableDictionary<DeclQualifiedName, SingleTagShapeInfo> singleTagRegistry,
        ModuleName ownModule)
    {
        var (info, unwrapped) = TryResolveSingleTagWrap(annotation, singleTagRegistry, ownModule);

        if (info is not null && unwrapped is not null)
            return unwrapped;

        return RenderTypeAnnotation(annotation);
    }
}

