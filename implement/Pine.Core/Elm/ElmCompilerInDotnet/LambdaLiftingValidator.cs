using System;
using System.Collections.Generic;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Validates the syntactic invariant that
/// <see cref="LambdaLifting.LiftLambdas(SyntaxTypes.File)"/> is meant to
/// maintain on its output, narrowed to the safety property described in
/// the postmortem document
/// <c>explore/internal-analysis/2026-04-25-lambda-lifting-sibling-capture-defect-postmortem.md</c>.
///
/// <para>
/// Background. The original defect was an erroneous <i>substitution</i>
/// inside the body of one lifted function: a sibling let-bound function
/// name (e.g. <c>inner</c>) was rewritten to its lifted top-level name
/// (e.g. <c>compute__lifted__inner_1</c>) even though the sibling captured
/// an outer binding that the substituted call site no longer supplied. The
/// fix classifies a sibling as substitutable only when it has no external
/// captures.
/// </para>
///
/// <para>
/// Invariant enforced. The validator identifies <i>partial-application
/// let-binding slots</i> &#8212; let bindings of the shape
/// <c>localName = topLevelFn arg1 ... argk</c> with <c>k &gt;= 1</c> and
/// no parameters on <c>localName</c> &#8212; and treats their head
/// function as a "let-bound lifted function with captures". For every
/// such top-level function, its <i>name</i> must NOT be referenced
/// anywhere in the module other than:
/// </para>
///
/// <list type="bullet">
///   <item><description>the partial-application let-binding's right-hand
///       side (exactly once), and</description></item>
///   <item><description>self-recursive references inside its own body.</description></item>
/// </list>
///
/// <para>
/// The local <c>let</c> alias (the binding's left-hand side) is what
/// other code is meant to use; its uses are local-name references, not
/// references to the top-level name, and are therefore not subject to
/// this rule. References to the top-level name from any other body are
/// exactly the post-substitution shape that produced the original bug.
/// </para>
///
/// <para>
/// This rule is purely structural and does not depend on a naming
/// convention for lifted functions. The set of "lifted-function candidates"
/// is supplied explicitly by the caller (see
/// <see cref="Validate(SyntaxTypes.File, IReadOnlySet{string})"/>) so that
/// user-defined top-level functions cannot accidentally be subject to the
/// invariant. Anonymous-lambda lifts are naturally exempt because the
/// lifter substitutes them directly into expression position (e.g.
/// <c>applyToEach (lambdaName captures) input</c>), never through a
/// let-binding partial-application slot, so they never appear as the head
/// of such a slot. Zero-capture lifts are exempt for the symmetric reason:
/// the let binding for a zero-capture lift has no captured arguments
/// (<c>k = 0</c>), so it is not a partial-application slot, and the lifter
/// is free to reference such a function by name from other bodies (the
/// "substitutable sibling" case).
/// </para>
/// </summary>
public static class LambdaLiftingValidator
{
    /// <summary>
    /// Validates a post-lifting Elm <see cref="SyntaxTypes.File"/>, restricting
    /// the invariant check to the supplied set of <paramref name="liftedFunctionNames"/>
    /// (the top-level function names that the lifter itself created). Throws
    /// <see cref="LambdaLiftingValidationException"/> if any invariant is
    /// violated.
    /// </summary>
    public static void Validate(
        SyntaxTypes.File module,
        IReadOnlySet<string> liftedFunctionNames)
    {
        var violations = CollectViolations(module, liftedFunctionNames);

        if (violations.Count > 0)
        {
            throw new LambdaLiftingValidationException(violations);
        }
    }

    /// <summary>
    /// Convenience overload that treats every top-level function declared
    /// in <paramref name="module"/> as a candidate. Intended for tests that
    /// hand-construct post-lifting modules containing only lifter-emitted
    /// shapes; in production use, prefer
    /// <see cref="Validate(SyntaxTypes.File, IReadOnlySet{string})"/> with
    /// the explicit set of lifter-created names so that user-defined
    /// top-level functions cannot accidentally be flagged.
    /// </summary>
    public static void Validate(SyntaxTypes.File module) =>
        Validate(module, CollectTopLevelFunctionNames(module.Declarations));

    /// <summary>
    /// Validates a per-module slice of post-lifting declarations, restricting
    /// the invariant check to the supplied set of <paramref name="liftedFunctionNames"/>
    /// (the top-level function names in this slice that the lifter itself
    /// created). Throws <see cref="LambdaLiftingValidationException"/> if any
    /// invariant is violated.
    ///
    /// <para>
    /// Use this overload when you have a per-module list of post-lifting
    /// declarations (for example, when slicing a flat-dictionary
    /// representation by module namespace).
    /// </para>
    /// </summary>
    public static void Validate(
        IEnumerable<SyntaxTypes.Declaration> declarations,
        IReadOnlySet<string> liftedFunctionNames)
    {
        var violations = CollectViolations(declarations, liftedFunctionNames);

        if (violations.Count > 0)
        {
            throw new LambdaLiftingValidationException(violations);
        }
    }

    /// <summary>
    /// Validates a per-module slice of post-lifting declarations and
    /// returns all detected invariant violations. An empty list means the
    /// slice is well-formed with respect to the validator's rules. The
    /// invariant is only enforced for top-level functions whose names are
    /// in <paramref name="liftedFunctionNames"/>.
    /// </summary>
    public static IReadOnlyList<string> CollectViolations(
        IEnumerable<SyntaxTypes.Declaration> declarations,
        IReadOnlySet<string> liftedFunctionNames)
    {
        var violations = new List<string>();

        if (liftedFunctionNames.Count is 0)
        {
            return violations;
        }

        // Identify "let-bound lifted functions WITH captures" purely
        // structurally: scan every function declaration's body for partial-
        // application let bindings of the shape
        // `localName = liftedFn cap1 cap2 ... capk` where k > 0 and
        // liftedFn is one of the lifter-created top-level functions. The
        // map's value is the name of the function in which the partial-app
        // slot was found (used for diagnostics only).
        var liftedNamesWithCaptures =
            CollectLetBoundLiftedNamesWithCaptures(declarations, liftedFunctionNames);

        if (liftedNamesWithCaptures.Count is 0)
        {
            // Nothing to check.
            return violations;
        }

        // For each lifted-with-captures function, count the references to
        // its name across the module, classified by where they occur.
        var referenceCountsByName =
            new Dictionary<string, ReferenceCounts>(StringComparer.Ordinal);

        foreach (var name in liftedNamesWithCaptures.Keys)
        {
            referenceCountsByName[name] = new ReferenceCounts();
        }

        foreach (var declaration in declarations)
        {
            if (declaration is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                continue;
            }

            var declName = funcDecl.Function.Declaration.Name;

            CountReferences(
                funcDecl.Function.Declaration.Expression,
                liftedNamesWithCaptures,
                ownerDeclName: declName,
                referenceCountsByName,
                inPartialAppSlot: false);
        }

        foreach (var (liftedName, partialAppOwner) in liftedNamesWithCaptures)
        {
            var counts = referenceCountsByName[liftedName];

            if (counts._inPartialAppSlot is not 1)
            {
                violations.Add(
                    $"lifted-with-captures function '{liftedName}' has {counts._inPartialAppSlot} " +
                    "partial-application let-binding slots; expected exactly 1.");
            }

            // Allowed reference sites:
            //   - the partial-application let binding's RHS (counted once
            //     above and excluded from InOtherBodies),
            //   - inside the lifted function's own body (self-recursion).
            // Any reference inside ANOTHER lifted body or another top-level
            // function's body is a violation.
            foreach (var (otherDecl, count) in counts.InOtherBodies)
            {
                if (string.Equals(otherDecl, liftedName, StringComparison.Ordinal))
                {
                    // Self-recursion inside the lifted function's own body
                    // is allowed.
                    continue;
                }

                if (string.Equals(otherDecl, partialAppOwner, StringComparison.Ordinal))
                {
                    // The partial-app slot owner: any reference to the
                    // lifted name there OTHER than the slot itself (e.g.
                    // a stray bare reference in the same containing
                    // function's body) is a violation. The slot itself is
                    // counted in InPartialAppSlot, not in InOtherBodies.
                }

                violations.Add(
                    $"lifted-with-captures function '{liftedName}' is referenced by name " +
                    $"{count} time(s) inside the body of '{otherDecl}'; the lifted name must only " +
                    "appear in its partial-application let-binding RHS (and self-recursive " +
                    "references inside its own body). Other code should use the local let alias " +
                    "rather than the lifted name.");
            }
        }

        return violations;
    }

    /// <summary>
    /// Validates a post-lifting Elm <see cref="SyntaxTypes.File"/> and
    /// returns all detected invariant violations. An empty list means the
    /// module is well-formed with respect to the validator's rules. The
    /// invariant is only enforced for top-level functions whose names are
    /// in <paramref name="liftedFunctionNames"/>.
    /// </summary>
    public static IReadOnlyList<string> CollectViolations(
        SyntaxTypes.File module,
        IReadOnlySet<string> liftedFunctionNames) =>
        CollectViolations(module.Declarations, liftedFunctionNames);

    private sealed class ReferenceCounts
    {
        public int _inPartialAppSlot;

        public Dictionary<string, int> InOtherBodies { get; } =
            new(StringComparer.Ordinal);
    }

    /// <summary>
    /// Convenience overload of
    /// <see cref="CollectViolations(SyntaxTypes.File, IReadOnlySet{string})"/>
    /// that treats every top-level function declared in <paramref name="module"/>
    /// as a candidate.
    /// </summary>
    public static IReadOnlyList<string> CollectViolations(SyntaxTypes.File module) =>
        CollectViolations(
            module.Declarations,
            CollectTopLevelFunctionNames(module.Declarations));

    private static HashSet<string> CollectTopLevelFunctionNames(
        IEnumerable<SyntaxTypes.Declaration> declarations)
    {
        var result = new HashSet<string>(StringComparer.Ordinal);

        foreach (var declaration in declarations)
        {
            if (declaration is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                continue;
            }

            result.Add(funcDecl.Function.Declaration.Name);
        }

        return result;
    }

    /// <summary>
    /// Walks every function declaration's body looking for partial-application
    /// let bindings of the form
    /// <c>localName = topLevelFn arg1 arg2 ... argk</c> where k &gt; 0 and
    /// <paramref name="topLevelFunctionNames"/> contains <c>topLevelFn</c>.
    /// Returns a map from such top-level function names to the name of the
    /// containing function in which the partial-app slot was found.
    /// </summary>
    private static Dictionary<string, string> CollectLetBoundLiftedNamesWithCaptures(
        IEnumerable<SyntaxTypes.Declaration> declarations,
        IReadOnlySet<string> topLevelFunctionNames)
    {
        var result = new Dictionary<string, string>(StringComparer.Ordinal);

        foreach (var declaration in declarations)
        {
            if (declaration is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                continue;
            }

            var declName = funcDecl.Function.Declaration.Name;

            FindPartialAppSlots(
                funcDecl.Function.Declaration.Expression,
                topLevelFunctionNames,
                ownerDeclName: declName,
                result);
        }

        return result;
    }

    private static void FindPartialAppSlots(
        SyntaxTypes.Expression expr,
        IReadOnlySet<string> topLevelFunctionNames,
        string ownerDeclName,
        Dictionary<string, string> resultBuilder)
    {
        if (expr is SyntaxTypes.Expression.LetExpression letExpr)
        {
            foreach (var declaration in letExpr.Declarations)
            {
                if (declaration is SyntaxTypes.LetDeclaration.LetFunction letFunc &&
                    letFunc.Function.Declaration.Arguments.Count is 0)
                {
                    var rhs = letFunc.Function.Declaration.Expression;

                    if (rhs is SyntaxTypes.Expression.Application app &&
                        app.Arguments.Count > 0 &&
                        app.Function is SyntaxTypes.Expression.Identifier head &&
                        topLevelFunctionNames.Contains(head.QualifiedName.DeclName))
                    {
                        // Partial-app slot supplying app.Arguments.Count captures.
                        resultBuilder[head.QualifiedName.DeclName] = ownerDeclName;
                    }
                }
            }
        }

        foreach (var child in EnumerateChildExpressions(expr))
        {
            FindPartialAppSlots(child, topLevelFunctionNames, ownerDeclName, resultBuilder);
        }
    }

    private static void CountReferences(
        SyntaxTypes.Expression expr,
        IReadOnlyDictionary<string, string> liftedNamesWithCaptures,
        string ownerDeclName,
        Dictionary<string, ReferenceCounts> resultBuilder,
        bool inPartialAppSlot)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.Identifier identifier:
                if (liftedNamesWithCaptures.ContainsKey(identifier.QualifiedName.DeclName))
                {
                    var counts = resultBuilder[identifier.QualifiedName.DeclName];

                    if (inPartialAppSlot)
                    {
                        counts._inPartialAppSlot += 1;
                    }
                    else
                    {
                        counts.InOtherBodies.TryGetValue(ownerDeclName, out var prev);
                        counts.InOtherBodies[ownerDeclName] = prev + 1;
                    }
                }

                break;

            case SyntaxTypes.Expression.Application appExpr:
                {
                    // The Application's HEAD position is treated like the
                    // current parent's `inPartialAppSlot` context: if the
                    // whole Application is the let-binding partial-app RHS,
                    // then the head reference here counts as the slot
                    // reference, and the supplied arguments are walked as
                    // ordinary subexpressions.
                    CountReferences(
                        appExpr.Function,
                        liftedNamesWithCaptures,
                        ownerDeclName,
                        resultBuilder,
                        inPartialAppSlot);

                    foreach (var argument in appExpr.Arguments)
                    {
                        CountReferences(
                            argument,
                            liftedNamesWithCaptures,
                            ownerDeclName,
                            resultBuilder,
                            inPartialAppSlot: false);
                    }

                    break;
                }

            case SyntaxTypes.Expression.LetExpression letExpr:
                {
                    foreach (var declaration in letExpr.Declarations)
                    {
                        switch (declaration)
                        {
                            case SyntaxTypes.LetDeclaration.LetFunction letFunc:
                                {
                                    var letFnImpl = letFunc.Function.Declaration;

                                    var isPartialAppSlot =
                                        letFnImpl.Arguments.Count is 0 &&
                                        IsLiftedHeadOrApplication(
                                            letFnImpl.Expression,
                                            liftedNamesWithCaptures);

                                    CountReferences(
                                        letFnImpl.Expression,
                                        liftedNamesWithCaptures,
                                        ownerDeclName,
                                        resultBuilder,
                                        inPartialAppSlot: isPartialAppSlot);

                                    break;
                                }

                            case SyntaxTypes.LetDeclaration.LetDestructuring letDestr:
                                CountReferences(
                                    letDestr.Expression,
                                    liftedNamesWithCaptures,
                                    ownerDeclName,
                                    resultBuilder,
                                    inPartialAppSlot: false);

                                break;

                            default:
                                throw new NotImplementedException(
                                    "CountReferences does not handle let declaration variant: " +
                                    declaration.GetType().Name);
                        }
                    }

                    CountReferences(
                        letExpr.Expression,
                        liftedNamesWithCaptures,
                        ownerDeclName,
                        resultBuilder,
                        inPartialAppSlot: false);

                    break;
                }

            case SyntaxTypes.Expression.OperatorApplication:
            case SyntaxTypes.Expression.Negation:
            case SyntaxTypes.Expression.IfBlock:
            case SyntaxTypes.Expression.CaseExpression:
            case SyntaxTypes.Expression.LambdaExpression:
            case SyntaxTypes.Expression.TupledExpression:
            case SyntaxTypes.Expression.ListExpr:
            case SyntaxTypes.Expression.RecordExpr:
            case SyntaxTypes.Expression.RecordUpdateExpression:
            case SyntaxTypes.Expression.RecordAccess:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                foreach (var child in EnumerateChildExpressions(expr))
                {
                    CountReferences(
                        child,
                        liftedNamesWithCaptures,
                        ownerDeclName,
                        resultBuilder,
                        inPartialAppSlot: false);
                }

                break;

            default:
                throw new NotImplementedException(
                    "CountReferences does not handle expression variant: " + expr.GetType().Name);
        }
    }

    private static bool IsLiftedHeadOrApplication(
        SyntaxTypes.Expression expr,
        IReadOnlyDictionary<string, string> liftedNamesWithCaptures) =>
        (expr is SyntaxTypes.Expression.Identifier identifier &&
        liftedNamesWithCaptures.ContainsKey(identifier.QualifiedName.DeclName)) ||
        (expr is SyntaxTypes.Expression.Application app &&
        app.Function is SyntaxTypes.Expression.Identifier head &&
        liftedNamesWithCaptures.ContainsKey(head.QualifiedName.DeclName));

    private static IEnumerable<SyntaxTypes.Expression> EnumerateChildExpressions(
        SyntaxTypes.Expression expr)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.Application app:
                yield return app.Function;

                foreach (var a in app.Arguments)
                    yield return a;

                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                yield return opApp.Left;
                yield return opApp.Right;
                break;

            case SyntaxTypes.Expression.Negation neg:
                yield return neg.Expression;
                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                yield return ifBlock.Condition;
                yield return ifBlock.ThenBlock;
                yield return ifBlock.ElseBlock;
                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                yield return caseExpr.Expression;

                foreach (var c in caseExpr.Cases)
                {
                    yield return c.Expression;
                }

                break;

            case SyntaxTypes.Expression.LetExpression letExpr:
                foreach (var declaration in letExpr.Declarations)
                {
                    switch (declaration)
                    {
                        case SyntaxTypes.LetDeclaration.LetFunction letFunc:
                            yield return letFunc.Function.Declaration.Expression;
                            break;

                        case SyntaxTypes.LetDeclaration.LetDestructuring letDestr:
                            yield return letDestr.Expression;
                            break;

                        default:
                            throw new NotImplementedException(
                                "EnumerateChildExpressions does not handle let declaration variant: " +
                                declaration.GetType().Name);
                    }
                }

                yield return letExpr.Expression;
                break;

            case SyntaxTypes.Expression.LambdaExpression lambdaExpr:
                yield return lambdaExpr.Expression;
                break;

            case SyntaxTypes.Expression.TupledExpression tuple:
                foreach (var el in tuple.Elements)
                    yield return el;

                break;

            case SyntaxTypes.Expression.ListExpr list:
                foreach (var el in list.Elements)
                    yield return el;

                break;

            case SyntaxTypes.Expression.RecordExpr record:
                foreach (var f in record.Fields)
                    yield return f.Value;

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                foreach (var f in recordUpdate.Fields)
                    yield return f.Value;

                break;

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                yield return recordAccess.Record;
                break;

            case SyntaxTypes.Expression.Identifier:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                yield break;

            default:
                throw new NotImplementedException(
                    "EnumerateChildExpressions does not handle expression variant: " + expr.GetType().Name);
        }
    }
}

/// <summary>
/// Thrown by <see cref="LambdaLiftingValidator.Validate(SyntaxTypes.File)"/>
/// when one or more invariant violations are detected on the post-lifting
/// module.
/// </summary>
/// <remarks>
/// Creates a new exception carrying the supplied <paramref name="violations"/>.
/// </remarks>
public class LambdaLiftingValidationException(IReadOnlyList<string> violations) : Exception(BuildMessage(violations))
{
    /// <summary>
    /// The list of detected violations. Always non-empty.
    /// </summary>
    public IReadOnlyList<string> Violations { get; } = violations;

    private static string BuildMessage(IReadOnlyList<string> violations) =>
        "LambdaLifting validator detected " +
        violations.Count +
        " invariant violation(s):" +
        Environment.NewLine +
        string.Join(Environment.NewLine, violations.Select(v => " - " + v));
}
