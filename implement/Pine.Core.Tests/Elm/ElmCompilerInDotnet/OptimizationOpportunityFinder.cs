using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using SyntaxModel = Pine.Core.Elm.ElmSyntax.SyntaxModel;

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
        ImmutableHashSet<string>? ignoreHigherOrderParameter = null)
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

        var resultBuilder = ImmutableHashSet.CreateBuilder<Opportunity>();

        foreach (var (qualifiedName, declaration) in declarations)
        {
            if (declaration is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
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
        }

        return resultBuilder.ToImmutable();
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
        ImmutableHashSet<string>? ignoreHigherOrderParameter = null)
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
                ignoreHigherOrderParameter);
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
                }

                foreach (var arg in app.Arguments)
                    FindAppliedParameterNames(arg.Value, paramNames, found);

                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                FindAppliedParameterNames(opApp.Left.Value, paramNames, found);
                FindAppliedParameterNames(opApp.Right.Value, paramNames, found);
                break;

            case SyntaxTypes.Expression.LetExpression letExpr:
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
                            break;

                        default:
                            throw new System.NotImplementedException(
                                "FindAppliedParameterNames does not handle let declaration variant: " +
                                declNode.Value.GetType().Name);
                    }
                }

                FindAppliedParameterNames(letExpr.Value.Expression.Value, paramNames, found);
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
                    FindAppliedParameterNames(caseEntry.Expression.Value, paramNames, found);

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

            // Other pattern variants either bind names that we conservatively
            // ignore (e.g. tuple destructuring of a function-typed value
            // is not common in Elm) or do not bind names at all.
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
}

