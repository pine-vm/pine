using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Validates the syntactic invariants that <see cref="LambdaLifting.LiftLambdas(SyntaxTypes.File)"/>
/// is meant to maintain on its output.
///
/// <para>
/// Background: a previous defect in lambda lifting silently miscompiled
/// programs in which one let-bound function called another sibling let-bound
/// function that captured an outer binding. The bad rewrite produced output
/// that was structurally well-formed Elm syntax (and therefore passed all
/// existing parser/AST checks) but referred to a lifted top-level function
/// in a position where its captured arguments were not supplied. Pine has
/// no value-level type checking, so the error surfaced only later as
/// silently corrupted runtime values.
/// </para>
///
/// <para>
/// This validator enforces the invariants described in the postmortem
/// document <c>explore/internal-analysis/2026-04-25-lambda-lifting-sibling-capture-defect-postmortem.md</c>:
/// </para>
///
/// <list type="bullet">
///   <item>
///     <description>
///       <b>L1</b> &#8211; every lifted top-level function (one whose name
///       contains <c>__lifted__</c>) must, in the body of its <i>containing</i>
///       function, be referenced exactly once via a <c>let</c>-binding
///       partial application of the form
///       <c>localName = liftedName cap1 cap2 ... capk</c> where the binding
///       has zero parameters and supplies between zero and
///       <c>arity(liftedName)</c> arguments. (Strictly fewer than the full
///       arity is allowed; equal to the full arity is also allowed for
///       lifted functions that have no extra lambda parameters.)
///     </description>
///   </item>
///   <item>
///     <description>
///       <b>L3</b> &#8211; every <i>other</i> reference to a lifted name
///       (in the body of its containing function outside the let-binding
///       partial application, in the body of the lifted function itself,
///       or in the body of any other lifted function in the same containing
///       group) must appear at the head of an <see cref="SyntaxTypes.Expression.Application"/>
///       chain that supplies <i>exactly</i> <c>arity(liftedName)</c>
///       arguments &#8211; i.e. the call site is fully saturated.
///     </description>
///   </item>
/// </list>
///
/// <para>
/// Bare references to a lifted name (a <see cref="SyntaxTypes.Expression.FunctionOrValue"/>
/// not at the head of an <see cref="SyntaxTypes.Expression.Application"/> and not
/// in the let-binding partial-application slot) are forbidden: lambda
/// lifting never produces them, and their presence indicates a bug in the
/// lifter or in a downstream rewrite that touched the lifter's output.
/// </para>
/// </summary>
public static class LambdaLiftingValidator
{
    private const string LiftedMarker = "__lifted__";

    /// <summary>
    /// Validates a post-lifting Elm <see cref="SyntaxTypes.File"/>. Throws
    /// <see cref="LambdaLiftingValidationException"/> if any invariant is
    /// violated.
    /// </summary>
    public static void Validate(SyntaxTypes.File module)
    {
        var violations = CollectViolations(module);

        if (violations.Count > 0)
        {
            throw new LambdaLiftingValidationException(violations);
        }
    }

    /// <summary>
    /// Validates a post-lifting Elm <see cref="SyntaxTypes.File"/> and
    /// returns all detected invariant violations. An empty list means the
    /// module is well-formed.
    /// </summary>
    public static IReadOnlyList<string> CollectViolations(SyntaxTypes.File module)
    {
        var violations = new List<string>();

        var arityByLiftedName = BuildArityByLiftedName(module);

        if (arityByLiftedName.Count is 0)
        {
            // Nothing to validate.
            return violations;
        }

        // For L1: every lifted function should be referenced via a partial-
        // application let binding inside its containing function. We count
        // how many such bindings reference each lifted name; expect exactly 1.
        var partialAppBindingCountByLiftedName =
            arityByLiftedName.Keys.ToDictionary(name => name, _ => 0, StringComparer.Ordinal);

        foreach (var declarationNode in module.Declarations)
        {
            if (declarationNode.Value is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                continue;
            }

            var declName = funcDecl.Function.Declaration.Value.Name.Value;

            ValidateExpression(
                funcDecl.Function.Declaration.Value.Expression,
                arityByLiftedName,
                partialAppBindingCountByLiftedName,
                declarationOwnerName: declName,
                violations);
        }

        // L1: every lifted function must be referenced by exactly one
        // partial-application let-binding in its containing function. A
        // count of zero usually means the lifted function is "orphaned"
        // and should not have been emitted; a count > 1 means the lifter
        // accidentally emitted multiple let bindings referring to the same
        // lifted name (also a bug).
        foreach (var (liftedName, count) in partialAppBindingCountByLiftedName)
        {
            if (count is not 1)
            {
                violations.Add(
                    $"L1: lifted function '{liftedName}' is referenced by {count} partial-application " +
                    "let-bindings; expected exactly 1.");
            }
        }

        return violations;
    }

    /// <summary>
    /// Inspects all top-level function declarations and returns a map from
    /// lifted-function name to its declared total arity (number of formal
    /// parameters, i.e. captures + original lambda parameters).
    /// </summary>
    private static Dictionary<string, int> BuildArityByLiftedName(SyntaxTypes.File module)
    {
        var result = new Dictionary<string, int>(StringComparer.Ordinal);

        foreach (var declarationNode in module.Declarations)
        {
            if (declarationNode.Value is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                continue;
            }

            var name = funcDecl.Function.Declaration.Value.Name.Value;

            if (!name.Contains(LiftedMarker, StringComparison.Ordinal))
            {
                continue;
            }

            var arity = funcDecl.Function.Declaration.Value.Arguments.Count;

            result[name] = arity;
        }

        return result;
    }

    /// <summary>
    /// Walks an expression tree, validating L1/L3 for every reference to a
    /// lifted-function name. <paramref name="declarationOwnerName"/> is the
    /// name of the top-level function whose body we are walking; it is used
    /// only in violation messages.
    /// </summary>
    private static void ValidateExpression(
        Node<SyntaxTypes.Expression> exprNode,
        IReadOnlyDictionary<string, int> arityByLiftedName,
        Dictionary<string, int> partialAppBindingCountByLiftedName,
        string declarationOwnerName,
        List<string> violations)
    {
        WalkExpression(
            exprNode,
            arityByLiftedName,
            partialAppBindingCountByLiftedName,
            declarationOwnerName,
            violations,
            inPartialApplicationLetRhs: false);
    }

    private static void WalkExpression(
        Node<SyntaxTypes.Expression> exprNode,
        IReadOnlyDictionary<string, int> arityByLiftedName,
        Dictionary<string, int> partialAppBindingCountByLiftedName,
        string declarationOwnerName,
        List<string> violations,
        bool inPartialApplicationLetRhs)
    {
        var expr = exprNode.Value;

        switch (expr)
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrValue:
                if (IsLiftedReference(funcOrValue, arityByLiftedName))
                {
                    var liftedName = funcOrValue.Name;

                    if (inPartialApplicationLetRhs)
                    {
                        // Bare reference in let-binding partial-application slot.
                        // This corresponds to zero supplied captures, which is
                        // allowed: the let-binding then acts as a pure alias
                        // for the lifted function (used when the lifter
                        // determined the sibling has no external captures).
                        // Any subsequent call site of the alias resolves to a
                        // local FunctionOrValue (NOT a lifted name), so the
                        // saturation check there does not apply.
                        partialAppBindingCountByLiftedName[liftedName] += 1;
                    }
                    else
                    {
                        // Bare lifted reference NOT in a let-binding RHS slot.
                        // Always a violation: such a reference would either be a
                        // partial application that is never consumed, or — more
                        // likely — was supposed to be the head of a fully-saturated
                        // Application that was lost along the way.
                        violations.Add(
                            $"L3 (in '{declarationOwnerName}'): bare reference to lifted name '{liftedName}' " +
                            $"is not the head of an Application and is not a let-binding partial-application " +
                            $"slot.");
                    }
                }

                break;

            case SyntaxTypes.Expression.Application appExpr:
                {
                    if (appExpr.Arguments.Count is 0)
                    {
                        // Degenerate; nothing to validate.
                        break;
                    }

                    var headNode = appExpr.Arguments[0];
                    var argCount = appExpr.Arguments.Count - 1;

                    if (headNode.Value is SyntaxTypes.Expression.FunctionOrValue headFunc &&
                        IsLiftedReference(headFunc, arityByLiftedName))
                    {
                        var liftedName = headFunc.Name;
                        var arity = arityByLiftedName[liftedName];

                        if (inPartialApplicationLetRhs)
                        {
                            // L1: this is the let-binding partial application slot.
                            partialAppBindingCountByLiftedName[liftedName] += 1;

                            if (argCount > arity)
                            {
                                violations.Add(
                                    $"L1 (in '{declarationOwnerName}'): let-binding partial application of " +
                                    $"lifted name '{liftedName}' supplies {argCount} arguments but the lifted " +
                                    $"function only declares {arity} parameters.");
                            }
                        }
                        else
                        {
                            // L3: must supply exactly arity arguments along the spine.
                            if (argCount != arity)
                            {
                                violations.Add(
                                    $"L3 (in '{declarationOwnerName}'): call site of lifted name '{liftedName}' " +
                                    $"supplies {argCount} arguments but the lifted function declares {arity} " +
                                    $"parameters; expected fully-saturated application.");
                            }
                        }

                        // Walk only the argument expressions — NOT the head as
                        // an isolated FunctionOrValue (we already validated it
                        // here in its application-head context).
                        for (var i = 1; i < appExpr.Arguments.Count; i++)
                        {
                            WalkExpression(
                                appExpr.Arguments[i],
                                arityByLiftedName,
                                partialAppBindingCountByLiftedName,
                                declarationOwnerName,
                                violations,
                                inPartialApplicationLetRhs: false);
                        }

                        break;
                    }

                    // Head is not a lifted reference: walk all subexpressions
                    // (including the head).
                    foreach (var argNode in appExpr.Arguments)
                    {
                        WalkExpression(
                            argNode,
                            arityByLiftedName,
                            partialAppBindingCountByLiftedName,
                            declarationOwnerName,
                            violations,
                            inPartialApplicationLetRhs: false);
                    }

                    break;
                }

            case SyntaxTypes.Expression.LetExpression letExpr:
                {
                    foreach (var declNode in letExpr.Value.Declarations)
                    {
                        switch (declNode.Value)
                        {
                            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                                {
                                    var letFnImpl = letFunc.Function.Declaration.Value;

                                    // A 0-parameter let function whose body is either
                                    // a bare lifted reference or an Application whose
                                    // head is a lifted reference is the partial-app
                                    // slot recognised by L1.
                                    var isPartialAppSlot =
                                        letFnImpl.Arguments.Count is 0 &&
                                        IsLiftedHeadOrApplication(
                                            letFnImpl.Expression.Value,
                                            arityByLiftedName);

                                    WalkExpression(
                                        letFnImpl.Expression,
                                        arityByLiftedName,
                                        partialAppBindingCountByLiftedName,
                                        declarationOwnerName,
                                        violations,
                                        inPartialApplicationLetRhs: isPartialAppSlot);

                                    break;
                                }

                            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                                WalkExpression(
                                    letDestr.Expression,
                                    arityByLiftedName,
                                    partialAppBindingCountByLiftedName,
                                    declarationOwnerName,
                                    violations,
                                    inPartialApplicationLetRhs: false);

                                break;
                        }
                    }

                    WalkExpression(
                        letExpr.Value.Expression,
                        arityByLiftedName,
                        partialAppBindingCountByLiftedName,
                        declarationOwnerName,
                        violations,
                        inPartialApplicationLetRhs: false);

                    break;
                }

            case SyntaxTypes.Expression.LambdaExpression lambdaExpr:
                WalkExpression(
                    lambdaExpr.Lambda.Expression,
                    arityByLiftedName,
                    partialAppBindingCountByLiftedName,
                    declarationOwnerName,
                    violations,
                    inPartialApplicationLetRhs: false);

                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                WalkExpression(
                    ifBlock.Condition,
                    arityByLiftedName,
                    partialAppBindingCountByLiftedName,
                    declarationOwnerName,
                    violations,
                    false);

                WalkExpression(
                    ifBlock.ThenBlock,
                    arityByLiftedName,
                    partialAppBindingCountByLiftedName,
                    declarationOwnerName,
                    violations,
                    false);

                WalkExpression(
                    ifBlock.ElseBlock,
                    arityByLiftedName,
                    partialAppBindingCountByLiftedName,
                    declarationOwnerName,
                    violations,
                    false);

                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                WalkExpression(
                    caseExpr.CaseBlock.Expression,
                    arityByLiftedName,
                    partialAppBindingCountByLiftedName,
                    declarationOwnerName,
                    violations,
                    false);

                foreach (var c in caseExpr.CaseBlock.Cases)
                {
                    WalkExpression(
                        c.Expression,
                        arityByLiftedName,
                        partialAppBindingCountByLiftedName,
                        declarationOwnerName,
                        violations,
                        false);
                }

                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                WalkExpression(
                    opApp.Left,
                    arityByLiftedName,
                    partialAppBindingCountByLiftedName,
                    declarationOwnerName,
                    violations,
                    false);

                WalkExpression(
                    opApp.Right,
                    arityByLiftedName,
                    partialAppBindingCountByLiftedName,
                    declarationOwnerName,
                    violations,
                    false);

                break;

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                WalkExpression(
                    paren.Expression,
                    arityByLiftedName,
                    partialAppBindingCountByLiftedName,
                    declarationOwnerName,
                    violations,
                    inPartialApplicationLetRhs);

                break;

            case SyntaxTypes.Expression.Negation neg:
                WalkExpression(
                    neg.Expression,
                    arityByLiftedName,
                    partialAppBindingCountByLiftedName,
                    declarationOwnerName,
                    violations,
                    false);

                break;

            case SyntaxTypes.Expression.TupledExpression tuple:
                foreach (var el in tuple.Elements)
                {
                    WalkExpression(
                        el,
                        arityByLiftedName,
                        partialAppBindingCountByLiftedName,
                        declarationOwnerName,
                        violations,
                        false);
                }

                break;

            case SyntaxTypes.Expression.ListExpr list:
                foreach (var el in list.Elements)
                {
                    WalkExpression(
                        el,
                        arityByLiftedName,
                        partialAppBindingCountByLiftedName,
                        declarationOwnerName,
                        violations,
                        false);
                }

                break;

            case SyntaxTypes.Expression.RecordExpr record:
                foreach (var field in record.Fields)
                {
                    WalkExpression(
                        field.Value.valueExpr,
                        arityByLiftedName,
                        partialAppBindingCountByLiftedName,
                        declarationOwnerName,
                        violations,
                        false);
                }

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields)
                {
                    WalkExpression(
                        field.Value.valueExpr,
                        arityByLiftedName,
                        partialAppBindingCountByLiftedName,
                        declarationOwnerName,
                        violations,
                        false);
                }

                break;

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                WalkExpression(
                    recordAccess.Record,
                    arityByLiftedName,
                    partialAppBindingCountByLiftedName,
                    declarationOwnerName,
                    violations,
                    false);

                break;

            // Leaf expressions with no sub-expressions to walk:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.Literal:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.Hex:
            case SyntaxTypes.Expression.Floatable:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                break;

            default:
                throw new InvalidOperationException(
                    "LambdaLiftingValidator: unhandled expression node type: " + expr.GetType().FullName);
        }
    }

    private static bool IsLiftedReference(
        SyntaxTypes.Expression.FunctionOrValue funcOrValue,
        IReadOnlyDictionary<string, int> arityByLiftedName)
    {
        // The flat-dict variant of LiftLambdas qualifies references to
        // lifted names with the module path. We accept either the bare
        // unqualified name (immediately after lifting) or the qualified
        // form (after the post-lifting qualification pass).
        return arityByLiftedName.ContainsKey(funcOrValue.Name);
    }

    private static bool IsLiftedHeadOrApplication(
        SyntaxTypes.Expression expr,
        IReadOnlyDictionary<string, int> arityByLiftedName)
    {
        if (expr is SyntaxTypes.Expression.FunctionOrValue fov &&
            IsLiftedReference(fov, arityByLiftedName))
        {
            return true;
        }

        if (expr is SyntaxTypes.Expression.Application app &&
            app.Arguments.Count > 0 &&
            app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue head &&
            IsLiftedReference(head, arityByLiftedName))
        {
            return true;
        }

        if (expr is SyntaxTypes.Expression.ParenthesizedExpression paren)
        {
            return IsLiftedHeadOrApplication(paren.Expression.Value, arityByLiftedName);
        }

        return false;
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
