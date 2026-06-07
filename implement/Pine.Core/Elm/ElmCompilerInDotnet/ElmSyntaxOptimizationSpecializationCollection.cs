using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

using CollectedSpecializations =
    System.Collections.Immutable.ImmutableDictionary<
        Pine.Core.CodeAnalysis.DeclQualifiedName,
        System.Collections.Immutable.ImmutableHashSet<Pine.Core.Elm.ElmCompilerInDotnet.FunctionSpecialization>>;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Specialization collection (Pass 1): walks all modules to discover which function
/// specializations are needed before the inlining pass creates them.
/// </summary>
public partial class ElmSyntaxOptimization
{
    /// <summary>
    /// Merges two immutable specialization collections, unioning the sets for matching keys.
    /// </summary>
    private static CollectedSpecializations MergeCollected(
        CollectedSpecializations left,
        CollectedSpecializations right)
    {
        if (left.IsEmpty)
            return right;

        if (right.IsEmpty)
            return left;

        var result = left;

        foreach (var kvp in right)
        {
            result =
                result.SetItem(
                    kvp.Key,
                    result.TryGetValue(kvp.Key, out var existing)
                    ?
                    existing.Union(kvp.Value)
                    :
                    kvp.Value);
        }

        return result;
    }

    /// <summary>
    /// Adds a single specialization to the immutable collection.
    /// </summary>
    private static CollectedSpecializations AddToCollected(
        CollectedSpecializations collected,
        DeclQualifiedName targetName,
        FunctionSpecialization spec)
    {
        return
            collected.SetItem(
                targetName,
                collected.TryGetValue(targetName, out var existing)
                ?
                existing.Add(spec)
                :
                [spec]);
    }

    private static readonly CollectedSpecializations s_emptyCollected =
        [];

    /// <summary>
    /// Like <c>IsFunctionExpression</c> but additionally resolves a bare
    /// local <see cref="SyntaxTypes.Expression.FunctionOrValue"/> through
    /// one let-binding hop (using <see cref="InliningContext.LetRhsByName"/>)
    /// before checking. Lets a higher-order argument that is introduced
    /// by a <c>let (Wrapper localName) = &lt;ref&gt;</c> destructuring pass
    /// the gate so its corresponding <see cref="ParameterSpecialization"/>
    /// can be produced by <c>ClassifyArgument(arg, letRhsByName)</c>.
    /// </summary>
    private static bool IsFunctionExpressionThroughLetRhs(
        SyntaxTypes.Expression expr,
        InliningContext context)
    {
        if (IsFunctionExpression(expr, context))
            return true;

        if (context.LetRhsByName.IsEmpty)
            return false;

        var probe = expr;

        while (probe is SyntaxTypes.Expression.ParenthesizedExpression paren)
            probe = paren.Expression.Value;

        if (probe is SyntaxTypes.Expression.FunctionOrValue fov &&
            fov.ModuleName.Count is 0 &&
            context.LetRhsByName.TryGetValue(fov.Name, out var resolvedRhs))
        {
            return IsFunctionExpression(resolvedRhs, context);
        }

        return false;
    }

    /// of the shape <c>let (Wrapper varName) = &lt;rhs&gt;</c>, or null otherwise.
    /// Pattern is unwrapped through <c>AsPattern</c> / <c>ParenthesizedPattern</c>.
    /// The RHS is unconstrained at this stage; classification happens later.
    /// Mirror of <c>ParameterSpecialization.PeelOneLayerLetNewtypeWrap</c>'s
    /// pattern-side check (PR A introduced that helper for matching;
    /// this one is the discovery-side counterpart for population).
    /// </summary>
    internal static string? TryExtractLetNewtypeBindingName(
        SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
    {
        var pattern = letDestr.Pattern.Value;

        // Unwrap AsPattern and ParenthesizedPattern wrappers.
        while (true)
        {
            if (pattern is SyntaxTypes.Pattern.ParenthesizedPattern paren)
            {
                pattern = paren.Pattern.Value;
                continue;
            }

            if (pattern is SyntaxTypes.Pattern.AsPattern asPat)
            {
                pattern = asPat.Pattern.Value;
                continue;
            }

            break;
        }

        if (pattern is not SyntaxTypes.Pattern.NamedPattern namedPattern)
            return null;

        if (namedPattern.Arguments.Count is not 1)
            return null;

        var argPattern = namedPattern.Arguments[0].Value;

        while (true)
        {
            if (argPattern is SyntaxTypes.Pattern.ParenthesizedPattern paren)
            {
                argPattern = paren.Pattern.Value;
                continue;
            }

            if (argPattern is SyntaxTypes.Pattern.AsPattern asPat)
            {
                argPattern = asPat.Pattern.Value;
                continue;
            }

            break;
        }

        if (argPattern is SyntaxTypes.Pattern.VarPattern varPattern)
            return varPattern.Name;

        return null;
    }

    /// <summary>
    /// Removes any entries from <paramref name="letRhsByName"/> whose key
    /// is in <paramref name="shadowedNames"/>. Used at every binder
    /// (lambda parameters, case-branch patterns, let-block introduced names)
    /// to ensure the discovery walker never resolves a bare reference
    /// to an outer let-binding that has been shadowed.
    /// </summary>
    internal static ImmutableDictionary<string, SyntaxTypes.Expression> RemoveShadowedFromLetRhs(
        ImmutableDictionary<string, SyntaxTypes.Expression> letRhsByName,
        IEnumerable<string> shadowedNames)
    {
        if (letRhsByName.IsEmpty)
            return letRhsByName;

        var result = letRhsByName;

        foreach (var name in shadowedNames)
        {
            if (result.ContainsKey(name))
                result = result.Remove(name);
        }

        return result;
    }

    /// <summary>
    /// Pass 1: Walk all modules and collect specialization requests.
    /// This simulates the inlining walk (including expanding non-recursive functions)
    /// but instead of creating specialized declarations, it records what specializations
    /// are needed via <see cref="FunctionSpecialization"/>.
    /// </summary>
    private static CollectedSpecializations CollectSpecializationsFromDeclarations(
        OptimizedElmSyntaxDeclarations declarations,
        InliningContext context) =>
        CollectSpecializationsFromDeclarations(declarations.RenderAsFlatDictionary(), context);

    private static CollectedSpecializations CollectSpecializationsFromDeclarations(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        InliningContext context)
    {
        var collected = s_emptyCollected;

        foreach (var (key, decl) in declarations.OrderBy(kvp => kvp.Key))
        {
            var moduleContext =
                context with { Resolution = context.Resolution with { CurrentModuleName = key.Namespaces } };

            if (decl is SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                collected =
                    MergeCollected(
                        collected,
                        CollectSpecializationsFromExpression(
                            funcDecl.Function.Declaration.Value.Expression,
                            moduleContext));
            }
        }

        return collected;
    }

    /// <summary>
    /// Recursively walks an expression tree to find call sites that would trigger
    /// specialization, and collects the corresponding <see cref="FunctionSpecialization"/>
    /// requests. Returns an immutable dictionary mapping target function names to their
    /// deduplicated specialization sets.
    /// </summary>
    private static CollectedSpecializations CollectSpecializationsFromExpression(
        Node<SyntaxTypes.Expression> exprNode,
        InliningContext context)
    {
        switch (exprNode.Value)
        {
            case SyntaxTypes.Expression.Application app:
                return CollectSpecializationsFromApplication(app, context);

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                return CollectSpecializationsFromExpression(paren.Expression, context);

            case SyntaxTypes.Expression.IfBlock ifBlock:
                {
                    var result = CollectSpecializationsFromExpression(ifBlock.Condition, context);
                    result = MergeCollected(result, CollectSpecializationsFromExpression(ifBlock.ThenBlock, context));
                    result = MergeCollected(result, CollectSpecializationsFromExpression(ifBlock.ElseBlock, context));
                    return result;
                }

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                {
                    var result = CollectSpecializationsFromExpression(caseExpr.CaseBlock.Expression, context);

                    foreach (var branch in caseExpr.CaseBlock.Cases)
                    {
                        var branchContext =
                            context with
                            {
                                LetRhsByName =
                                RemoveShadowedFromLetRhs(
                                    context.LetRhsByName,
                                    ElmSyntaxTransformations.CollectPatternNames(branch.Pattern.Value)),
                            };

                        result =
                            MergeCollected(result, CollectSpecializationsFromExpression(branch.Expression, branchContext));
                    }

                    return result;
                }

            case SyntaxTypes.Expression.LetExpression letExpr:
                {
                    var result = s_emptyCollected;

                    // Build the let-block's body context: first drop all names introduced
                    // by any binder in this let (a non-newtype destructuring shadows an
                    // outer LetRhsByName entry but does NOT itself produce a new entry);
                    // then add entries for the specific single-tag newtype destructuring
                    // shape `let (Wrapper varName) = <rhs>` that PeelOneLayerLetNewtypeWrap
                    // recognizes — this is the §16 fixture's shape (e.g. `let (Parser pA)
                    // = parseDouble`). The walker uses the resulting map in
                    // TryBuildFunctionSpecializationForHigherOrder to look through one
                    // let-hop when classifying a bare-reference argument.
                    var extendedLetRhs = context.LetRhsByName;

                    foreach (var d in letExpr.Value.Declarations)
                    {
                        switch (d.Value)
                        {
                            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc2:
                                extendedLetRhs =
                                    RemoveShadowedFromLetRhs(
                                        extendedLetRhs,
                                        [letFunc2.Function.Declaration.Value.Name.Value]);

                                break;

                            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr2:
                                extendedLetRhs =
                                    RemoveShadowedFromLetRhs(
                                        extendedLetRhs,
                                        ElmSyntaxTransformations.CollectPatternNames(letDestr2.Pattern.Value));

                                if (TryExtractLetNewtypeBindingName(letDestr2) is { } boundName)
                                {
                                    extendedLetRhs = extendedLetRhs.SetItem(boundName, letDestr2.Expression.Value);
                                }

                                break;
                        }
                    }

                    var letBodyContext = context with { LetRhsByName = extendedLetRhs };

                    foreach (var d in letExpr.Value.Declarations)
                    {
                        switch (d.Value)
                        {
                            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                                result =
                                    MergeCollected(
                                        result,
                                        CollectSpecializationsFromExpression(
                                            letFunc.Function.Declaration.Value.Expression,
                                            letBodyContext));

                                break;

                            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                                result =
                                    MergeCollected(
                                        result,
                                        CollectSpecializationsFromExpression(letDestr.Expression, letBodyContext));

                                break;
                        }
                    }

                    result =
                        MergeCollected(
                            result,
                            CollectSpecializationsFromExpression(letExpr.Value.Expression, letBodyContext));

                    return result;
                }

            case SyntaxTypes.Expression.LambdaExpression lambda:
                {
                    var lambdaParamNames = new HashSet<string>();

                    foreach (var arg in lambda.Lambda.Arguments)
                    {
                        foreach (var n in ElmSyntaxTransformations.CollectPatternNames(arg.Value))
                            lambdaParamNames.Add(n);
                    }

                    var lambdaBodyContext =
                        context with
                        {
                            LetRhsByName = RemoveShadowedFromLetRhs(context.LetRhsByName, lambdaParamNames),
                        };

                    return CollectSpecializationsFromExpression(lambda.Lambda.Expression, lambdaBodyContext);
                }

            case SyntaxTypes.Expression.ListExpr listExpr:
                {
                    var result = s_emptyCollected;

                    foreach (var elem in listExpr.Elements)
                        result = MergeCollected(result, CollectSpecializationsFromExpression(elem, context));

                    return result;
                }

            case SyntaxTypes.Expression.TupledExpression tupled:
                {
                    var result = s_emptyCollected;

                    foreach (var elem in tupled.Elements)
                        result = MergeCollected(result, CollectSpecializationsFromExpression(elem, context));

                    return result;
                }

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                {
                    var result = s_emptyCollected;

                    foreach (var field in recordExpr.Fields)
                        result =
                            MergeCollected(result, CollectSpecializationsFromExpression(field.Value.valueExpr, context));

                    return result;
                }

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                {
                    var result = s_emptyCollected;

                    foreach (var field in recordUpdate.Fields)
                        result =
                            MergeCollected(result, CollectSpecializationsFromExpression(field.Value.valueExpr, context));

                    return result;
                }

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                return CollectSpecializationsFromExpression(recordAccess.Record, context);

            case SyntaxTypes.Expression.Negation negation:
                return CollectSpecializationsFromExpression(negation.Expression, context);

            case SyntaxTypes.Expression.OperatorApplication opApp:
                {
                    var result = CollectSpecializationsFromExpression(opApp.Left, context);
                    result = MergeCollected(result, CollectSpecializationsFromExpression(opApp.Right, context));
                    return result;
                }

            default:
                return s_emptyCollected;
        }
    }

    /// <summary>
    /// Analyzes a function application to determine if it requires specialization.
    /// If so, includes a <see cref="FunctionSpecialization"/> in the returned collection.
    /// Also recurses into inlined function bodies to discover nested specialization needs.
    /// </summary>
    private static CollectedSpecializations CollectSpecializationsFromApplication(
        SyntaxTypes.Expression.Application app,
        InliningContext context)
    {
        if (!EnablesSpecialization(context) || app.Arguments.Count < 2)
        {
            var result = s_emptyCollected;

            foreach (var arg in app.Arguments)
                result = MergeCollected(result, CollectSpecializationsFromExpression(arg, context));

            return result;
        }

        var collected = s_emptyCollected;
        var funcExpr = app.Arguments[0].Value;

        // Handle pipe operators: recurse into desugared form
        if (EnablesClassicInlining(context) &&
            app.Arguments.Count >= 3 &&
            funcExpr is SyntaxTypes.Expression.FunctionOrValue pipeFunc &&
            pipeFunc.ModuleName.Count is 1 && pipeFunc.ModuleName[0] is "Basics")
        {
            if (pipeFunc.Name is "apR")
            {
                var desugaredArgs = new List<Node<SyntaxTypes.Expression>> { app.Arguments[2], app.Arguments[1] };
                desugaredArgs.AddRange(app.Arguments.Skip(3));

                return
                    CollectSpecializationsFromApplication(
                        new SyntaxTypes.Expression.Application([.. desugaredArgs]),
                        context);
            }

            if (pipeFunc.Name is "apL")
            {
                var desugaredArgs = new List<Node<SyntaxTypes.Expression>>(app.Arguments.Skip(1));

                return
                    CollectSpecializationsFromApplication(
                        new SyntaxTypes.Expression.Application([.. desugaredArgs]),
                        context);
            }

            if (pipeFunc.Name is "composeR" or "composeL")
            {
                var result = s_emptyCollected;

                foreach (var arg in app.Arguments.Skip(1))
                    result = MergeCollected(result, CollectSpecializationsFromExpression(arg, context));

                return result;
            }
        }

        // Check for known function call
        if (funcExpr is SyntaxTypes.Expression.FunctionOrValue funcOrValue)
        {
            if (TryResolveKnownFunctionReference(funcOrValue, context) is { } resolved)
            {
                var funcImpl = resolved.FunctionInfo.Function.Declaration.Value;
                var funcParams = funcImpl.Arguments;
                var appArgs = app.Arguments.Skip(1).ToList();
                var targetDeclName = DeclQualifiedName.Create(resolved.FunctionInfo.ModuleName, funcImpl.Name.Value);

                // Check for single-choice tag specialization opportunity
                var tagSpec =
                    TryBuildFunctionSpecializationForSingleChoiceTag(
                        resolved.FunctionInfo,
                        funcImpl,
                        appArgs,
                        context);

                if (tagSpec is not null)
                {
                    collected = AddToCollected(collected, targetDeclName, tagSpec);
                }

                // Check for higher-order recursive specialization opportunity,
                // OR — for non-recursive callees — a TupleUnwrap-driven
                // specialization opportunity. The non-recursive path is the
                // narrow loosening described in Step 5 of
                // explore/internal-analysis/2026-05-19-monomorphizing-expressionAfterOpeningSquareBracket-lifted-lambda3.md:
                // a TupleUnwrap participation is required so we don't re-open
                // the broader "when may a non-recursive HO function be
                // specialized?" question.
                if (ShouldInline(funcParams, funcImpl.Expression, appArgs, context))
                {
                    var (hoSpec, groupMemberSpecs) =
                        TryBuildFunctionSpecializationForHigherOrder(
                            resolved.FunctionInfo,
                            funcImpl,
                            appArgs,
                            context,
                            includeGroupMembers: resolved.FunctionInfo.IsRecursive);

                    if (hoSpec is not null)
                    {
                        // The non-recursive loosening targets the lambda-lifted
                        // helper case (single-caller-by-construction per the
                        // plan) AND hand-written tuple-pattern helpers whose
                        // call site supplies the tuple from let-destructured
                        // newtype binders — exactly the §16/§17 fixture shape.
                        // The "uses local-bound tuple element" check ensures
                        // we don't redundantly specialize calls whose tuple is
                        // already filled with directly-resolvable top-level
                        // function refs (which the existing inliner handles
                        // without our intervention) — that would needlessly
                        // duplicate code and regress ElmParser execution
                        // counters.
                        // Gate: non-recursive + TupleUnwrap + (lifted-lambda OR
                        // tuple arg has at least one local-bound element).
                        // Combined this restricts the loosening to cases where
                        // the new specialization adds value over what the
                        // existing inliner already produces.
                        var keepNonRecursive =
                            !resolved.FunctionInfo.IsRecursive &&
                            hoSpec.ParameterSpecializations.Values
                            .Any(p => p is ParameterSpecialization.TupleUnwrap) &&
                            TupleArgUsesLocalReferences(funcImpl, appArgs, hoSpec.ParameterSpecializations);

                        if (resolved.FunctionInfo.IsRecursive || keepNonRecursive)
                        {
                            collected = AddToCollected(collected, targetDeclName, hoSpec);

                            if (resolved.FunctionInfo.IsRecursive)
                                collected = MergeCollected(collected, groupMemberSpecs);
                        }
                    }
                }

                // For non-recursive functions that would be inlined, recurse into the inlined body
                if (!resolved.FunctionInfo.IsRecursive &&
                    !context.InliningStack.Contains(resolved.QualifiedName) &&
                    (ShouldInline(funcParams, funcImpl.Expression, appArgs, context) ||
                    ShouldInlinePartialApplicationWithCapturedFunction(
                        resolved.FunctionInfo,
                        funcImpl,
                        appArgs,
                        context)))
                {
                    var newContext = context with { InliningStack = context.InliningStack.Add(resolved.QualifiedName) };

                    // Simulate inlining: substitute parameters and collect from the body
                    var substitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();
                    var count = Math.Min(funcParams.Count, appArgs.Count);

                    for (var i = 0; i < count; i++)
                    {
                        if (funcParams[i].Value is SyntaxTypes.Pattern.VarPattern varPattern)
                        {
                            substitutions[varPattern.Name] = appArgs[i];
                        }
                    }

                    var substitutedBody =
                        ElmSyntaxTransformations.SubstituteInExpression(funcImpl.Expression, substitutions);

                    collected =
                        MergeCollected(collected, CollectSpecializationsFromExpression(substitutedBody, newContext));
                }
            }
        }

        // Always recurse into arguments
        foreach (var arg in app.Arguments)
            collected = MergeCollected(collected, CollectSpecializationsFromExpression(arg, context));

        return collected;
    }

    /// <summary>
    /// Builds a <see cref="FunctionSpecialization"/> for a
    /// single-choice tag specialization opportunity.
    /// Returns null if no single-choice tag specialization is applicable.
    /// </summary>
    private static FunctionSpecialization?
        TryBuildFunctionSpecializationForSingleChoiceTag(
        FunctionInfo funcInfo,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        InliningContext context)
    {
        if (appArgs.Count < funcImpl.Arguments.Count)
            return null;

        var specialization = TryBuildSingleChoiceTagSpecialization(funcImpl, appArgs, funcInfo, context);

        if (specialization is null)
            return null;

        var paramSpecs =
            ImmutableDictionary.CreateBuilder<int, ParameterSpecialization>();

        paramSpecs[specialization.ParamIndex] =
            BuildSingleChoiceTagUnwrapSpec(specialization, context);

        return
            new FunctionSpecialization(
                paramSpecs.ToImmutable());
    }

    /// <summary>
    /// Builds a <see cref="ParameterSpecialization.SingleChoiceTagUnwrap"/>
    /// that includes function field specializations for proper deduplication.
    /// </summary>
    private static ParameterSpecialization.SingleChoiceTagUnwrap
        BuildSingleChoiceTagUnwrapSpec(
        SingleChoiceTagSpecialization specialization,
        InliningContext context)
    {
        var fieldSpecs =
            ImmutableDictionary.CreateBuilder<int, ParameterSpecialization>();

        foreach (var fieldPlan in specialization.FieldPlans)
        {
            if (fieldPlan.ParameterSpecialization is { } nestedSpecialization)
            {
                fieldSpecs[fieldPlan.FieldIndex] = nestedSpecialization;
            }
        }

        return
            new ParameterSpecialization.SingleChoiceTagUnwrap(
                DeclQualifiedName.Create(specialization.ConstructorName.ModuleName, specialization.ConstructorName.Name),
                fieldSpecs.ToImmutable());
    }

    /// <summary>
    /// Builds a <see cref="FunctionSpecialization"/> for a
    /// higher-order recursive function specialization opportunity.
    /// Returns null spec if no specialization is applicable.
    /// When <paramref name="includeGroupMembers"/> is true, also returns specializations
    /// for mutual recursion group members via the second tuple element.
    /// </summary>
    private static (FunctionSpecialization? Spec, CollectedSpecializations GroupMemberSpecs)
        TryBuildFunctionSpecializationForHigherOrder(
        FunctionInfo funcInfo,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        InliningContext context,
        bool includeGroupMembers = false)
    {
        var funcParams = funcImpl.Arguments;
        var funcName = funcImpl.Name.Value;
        var funcModuleName = funcInfo.ModuleName;

        var paramSpecs =
            ImmutableDictionary.CreateBuilder<int, ParameterSpecialization>();

        for (var i = 0; i < funcParams.Count && i < appArgs.Count; i++)
        {
            var paramPattern = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(funcParams[i].Value);

            // TuplePattern parameter — try to build a TupleUnwrap by classifying each element.
            // The element specialization-discovery path mirrors the matcher
            // ParameterSpecialization.TryMatchTupleArgumentWithElementSpecializations
            // (PR A) so the caller-side rewrite picks the catalog entry up at compile time.
            if (paramPattern is SyntaxTypes.Pattern.TuplePattern tuplePattern)
            {
                var tupleSpec =
                    TryBuildTupleUnwrapForArgument(
                        tuplePattern,
                        appArgs[i].Value,
                        funcImpl,
                        funcName,
                        funcModuleName,
                        funcInfo.IsRecursive,
                        i,
                        context);

                if (tupleSpec is not null)
                {
                    paramSpecs[i] = tupleSpec;
                }

                continue;
            }

            if (paramPattern is not SyntaxTypes.Pattern.VarPattern varPattern)
                continue;

            if (!IsFunctionExpressionThroughLetRhs(appArgs[i].Value, context))
                continue;

            if (!IsLoopInvariantInRecursiveCalls(
                    funcImpl.Expression.Value, funcName, funcModuleName, varPattern.Name, i))
                continue;

            var classified = ParameterSpecialization.ClassifyArgument(appArgs[i].Value, context.LetRhsByName);

            if (classified is not null)
            {
                paramSpecs[i] = classified;
            }
            else
            {
                // For expressions that can't be classified into the model (e.g. complex expressions),
                // fall back to treating them as a lambda-like specialization keyed by the expression.
                // We still record the specialization since the concrete argument will be substituted.
                paramSpecs[i] =
                    new ParameterSpecialization.ConcreteLambdaValue(
                        new SyntaxTypes.LambdaStruct(
                            Arguments: [],
                            Expression: appArgs[i]));
            }
        }

        if (paramSpecs.Count > 0 &&
            TryBuildSingleChoiceTagSpecialization(
                funcImpl,
                appArgs,
                funcInfo,
                context) is { } singleChoiceTagSpecialization &&
            ElmSyntaxTransformations.TryGetAliasNameFromPattern(funcImpl.Arguments[singleChoiceTagSpecialization.ParamIndex].Value) is { } aliasName &&
            IsLoopInvariantInRecursiveCalls(
                funcImpl.Expression.Value,
                funcName,
                funcModuleName,
                aliasName,
                singleChoiceTagSpecialization.ParamIndex))
        {
            paramSpecs[singleChoiceTagSpecialization.ParamIndex] =
                BuildSingleChoiceTagUnwrapSpec(
                    singleChoiceTagSpecialization,
                    context);
        }

        if (paramSpecs.Count is 0)
            return (null, s_emptyCollected);

        // Skip specialization for very large function bodies
        if (SyntaxTypes.SyntaxAnalysis.CountExpressionNodes(funcImpl.Expression.Value) > 2000)
            return (null, s_emptyCollected);

        var builtParamSpecs = paramSpecs.ToImmutable();

        // Also collect specializations for mutual recursion group members.
        var groupMemberSpecs = s_emptyCollected;

        if (includeGroupMembers)
        {
            var invariantIndicesSet = new HashSet<int>(paramSpecs.Keys);
            var groupMembers = FindMutualRecursionGroupMembers(funcInfo, invariantIndicesSet, context);

            foreach (var member in groupMembers)
            {
                var memberImpl = member.Function.Declaration.Value;

                groupMemberSpecs =
                    AddToCollected(
                        groupMemberSpecs,
                        DeclQualifiedName.Create(member.ModuleName, memberImpl.Name.Value),
                        new FunctionSpecialization(builtParamSpecs));
            }
        }

        return (new FunctionSpecialization(builtParamSpecs), groupMemberSpecs);
    }

    /// <summary>
    /// Returns true when at least one tuple-shaped argument at a
    /// <see cref="ParameterSpecialization.TupleUnwrap"/> parameter position
    /// has at least one element that's a bare local reference (empty
    /// module name). Such references only resolve to top-level function
    /// values via the surrounding let-destructured newtype binders
    /// tracked in <see cref="InliningContext.LetRhsByName"/>, so the
    /// non-recursive specialization adds non-trivial value at the call
    /// site. When every tuple element is already a directly-resolvable
    /// top-level reference, the existing inliner handles the case
    /// without our new emission path and the specialization would only
    /// duplicate code.
    /// </summary>
    private static bool TupleArgUsesLocalReferences(
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        ImmutableDictionary<int, ParameterSpecialization> paramSpecs)
    {
        foreach (var (idx, spec) in paramSpecs)
        {
            if (spec is not ParameterSpecialization.TupleUnwrap)
                continue;

            if (idx >= appArgs.Count)
                continue;

            var argExpr = appArgs[idx].Value;

            while (argExpr is SyntaxTypes.Expression.ParenthesizedExpression paren)
                argExpr = paren.Expression.Value;

            if (argExpr is not SyntaxTypes.Expression.TupledExpression tupled)
                continue;

            foreach (var elem in tupled.Elements)
            {
                var elemExpr = elem.Value;

                while (elemExpr is SyntaxTypes.Expression.ParenthesizedExpression elemParen)
                    elemExpr = elemParen.Expression.Value;

                if (elemExpr is SyntaxTypes.Expression.FunctionOrValue fov &&
                    fov.ModuleName.Count is 0)
                {
                    return true;
                }
            }
        }

        return false;
    }

    /// <summary>
    /// Attempts to build a <see cref="ParameterSpecialization.TupleUnwrap"/>
    /// for a call site whose callee's parameter is a <see cref="SyntaxTypes.Pattern.TuplePattern"/>
    /// (of bare <see cref="SyntaxTypes.Pattern.VarPattern"/> leaves) and whose
    /// argument expression — after paren-peel — is a
    /// <see cref="SyntaxTypes.Expression.TupledExpression"/> of matching arity.
    /// <para>
    /// Each tuple element is classified using
    /// <see cref="ParameterSpecialization.ClassifyArgument(SyntaxTypes.Expression, ImmutableDictionary{string, SyntaxTypes.Expression})"/>
    /// after one optional layer of <c>let (Wrapper name) = ref in name</c>
    /// peel via the same helper used by
    /// <see cref="ParameterSpecialization.ArgumentMatchesSpecialization"/>'s
    /// matcher (see <c>ParameterSpecialization.TryMatchTupleArgumentWithElementSpecializations</c>),
    /// so the discovery side and the caller-side rewrite agree on shape.
    /// </para>
    /// <para>
    /// Returns null when any element fails to classify, when arities mismatch,
    /// or — for recursive callees — when any leaf var is not loop-invariant
    /// across recursive self-calls. The non-recursive case skips the loop-
    /// invariance check (vacuously true with no recursive self-calls).
    /// </para>
    /// </summary>
    private static ParameterSpecialization? TryBuildTupleUnwrapForArgument(
        SyntaxTypes.Pattern.TuplePattern tuplePattern,
        SyntaxTypes.Expression argumentExpr,
        SyntaxTypes.FunctionImplementation funcImpl,
        string funcName,
        IReadOnlyList<string> funcModuleName,
        bool funcIsRecursive,
        int paramIndex,
        InliningContext context)
    {
        var unwrappedArg = argumentExpr;

        while (unwrappedArg is SyntaxTypes.Expression.ParenthesizedExpression paren)
            unwrappedArg = paren.Expression.Value;

        if (unwrappedArg is not SyntaxTypes.Expression.TupledExpression tupledExpr)
            return null;

        if (tupledExpr.Elements.Count != tuplePattern.Elements.Count)
            return null;

        var elementSpecs = ImmutableArray.CreateBuilder<ParameterSpecialization>(tuplePattern.Elements.Count);

        for (var j = 0; j < tuplePattern.Elements.Count; j++)
        {
            var elemPattern = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(tuplePattern.Elements[j].Value);

            // Only bare var-pattern leaves are supported in this PR; non-trivial
            // nested patterns would require recursive decomposition that's out
            // of scope for the §16 fixture.
            if (elemPattern is not SyntaxTypes.Pattern.VarPattern elemVar)
                return null;

            // For recursive callees, the leaf var must be loop-invariant across
            // every recursive self-call. The check operates on the synthetic
            // index space of the tuple's element position; the existing helper
            // is keyed by parameter name so the position is informational only.
            if (funcIsRecursive &&
                !IsLoopInvariantInRecursiveCalls(
                    funcImpl.Expression.Value, funcName, funcModuleName, elemVar.Name, paramIndex))
            {
                return null;
            }

            var elementExpr = tupledExpr.Elements[j].Value;

            var elementClassified =
                ParameterSpecialization.ClassifyArgument(elementExpr, context.LetRhsByName);

            if (elementClassified is null)
                return null;

            // Only allow concrete function-shaped element specializations so the
            // emission side can substitute a single reference for each leaf.
            // (Lambda/RecordAccessFunction are also concrete functions per
            // ClassifyArgument's domain.)
            if (elementClassified is not (
                ParameterSpecialization.ConcreteFunctionValue
                or ParameterSpecialization.ConcreteLambdaValue
                or ParameterSpecialization.ConcreteRecordAccessFunctionValue))
            {
                return null;
            }

            elementSpecs.Add(elementClassified);
        }

        return new ParameterSpecialization.TupleUnwrap(elementSpecs.ToImmutable());
    }

    /// <summary>
    /// Builds a <see cref="SpecializationCatalog"/> from collected specialization requests,
    /// deduplicating against specializations that are already present in
    /// <paramref name="existingDeclarations"/>.
    /// <para>
    /// For each <see cref="OptimizedElmSyntaxFunctionDeclaration"/> in
    /// <paramref name="existingDeclarations"/>, every entry in
    /// <see cref="OptimizedElmSyntaxFunctionDeclaration.Specializations"/> is folded into the
    /// resulting catalog as a pre-named
    /// <see cref="NamedSpecialization"/> (reusing the specialized declaration's own name).
    /// Any matching <c>(originalDeclName, FunctionSpecialization)</c> pair is removed from
    /// <paramref name="collected"/> before naming, so the catalog never assigns a fresh
    /// <c>__specialized__N</c> name to a specialization that already exists in the input.
    /// </para>
    /// <para>
    /// The input dictionary already contains deduplicated specialization sets per function
    /// (via <see cref="ImmutableHashSet{T}"/>), so no additional deduplication is needed.
    /// </para>
    /// </summary>
    private static SpecializationCatalog BuildCatalogFromCollectedSpecializations(
        CollectedSpecializations collected,
        OptimizedElmSyntaxDeclarations existingDeclarations)
    {
        // Collect pre-existing named specializations and the set of qualified names
        // already in use across the input.
        var preExisting = new List<NamedSpecialization>();

        var preExistingByOrig =
            new Dictionary<DeclQualifiedName, HashSet<FunctionSpecialization>>();

        var usedNames = new HashSet<string>();

        foreach (var (origName, optimized) in existingDeclarations.FunctionDeclarations)
        {
            usedNames.Add(origName.DeclName);

            foreach (var (spec, specDecl) in optimized.Specializations)
            {
                var specName = specDecl.Function.Declaration.Value.Name.Value;

                preExisting.Add(new NamedSpecialization(origName, spec, specName));
                usedNames.Add(specName);

                if (!preExistingByOrig.TryGetValue(origName, out var set))
                {
                    set = [];
                    preExistingByOrig[origName] = set;
                }

                set.Add(spec);
            }
        }

        foreach (var (otherName, _) in existingDeclarations.OtherDeclarations)
        {
            usedNames.Add(otherName.DeclName);
        }

        var allNamed = new List<NamedSpecialization>(preExisting);

        foreach (var kvp in collected.OrderBy(kvp => kvp.Key))
        {
            preExistingByOrig.TryGetValue(kvp.Key, out var alreadyDone);

            // Sort deterministically before naming to ensure stable __specialized__N numbering
            // regardless of ImmutableHashSet iteration order. The outer iteration is also
            // sorted (by DeclQualifiedName) so that the per-function name accumulator
            // (`usedNames`) sees specializations in a stable order across runs — without
            // this the same source program could compile to either
            // `oneOf2…__specialized__1__specialized__1` or `…__specialized__1__specialized__2`
            // depending on hash-bucket order.
            var sorted =
                kvp.Value
                .Where(s => alreadyDone is null || !alreadyDone.Contains(s))
                .OrderBy(s => s.DeterministicSortKey)
                .ToList();

            if (sorted.Count is 0)
                continue;

            var named = SpecializationCatalog.NameSpecializations(kvp.Key, sorted, usedNames);

            // Track newly assigned names to avoid clashes across different functions.
            foreach (var n in named)
                usedNames.Add(n.SpecializedFunctionName);

            allNamed.AddRange(named);
        }

        return SpecializationCatalog.BuildCatalog(allNamed);
    }
}
