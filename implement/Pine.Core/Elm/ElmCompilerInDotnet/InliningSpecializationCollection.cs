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
public partial class Inlining
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
    /// Pass 1: Walk all modules and collect specialization requests.
    /// This simulates the inlining walk (including expanding non-recursive functions)
    /// but instead of creating specialized declarations, it records what specializations
    /// are needed via <see cref="FunctionSpecialization"/>.
    /// </summary>
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
                        result =
                            MergeCollected(result, CollectSpecializationsFromExpression(branch.Expression, context));

                    return result;
                }

            case SyntaxTypes.Expression.LetExpression letExpr:
                {
                    var result = s_emptyCollected;

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
                                            context));

                                break;

                            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                                result =
                                    MergeCollected(result, CollectSpecializationsFromExpression(letDestr.Expression, context));

                                break;
                        }
                    }

                    result =
                        MergeCollected(
                            result,
                            CollectSpecializationsFromExpression(letExpr.Value.Expression, context));

                    return result;
                }

            case SyntaxTypes.Expression.LambdaExpression lambda:
                return CollectSpecializationsFromExpression(lambda.Lambda.Expression, context);

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
                var targetDeclName = new DeclQualifiedName(resolved.FunctionInfo.ModuleName, funcImpl.Name.Value);

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

                // Check for higher-order recursive specialization opportunity
                if (resolved.FunctionInfo.IsRecursive &&
                    ShouldInline(funcParams, funcImpl.Expression, appArgs, context))
                {
                    var (hoSpec, groupMemberSpecs) =
                        TryBuildFunctionSpecializationForHigherOrder(
                            resolved.FunctionInfo,
                            funcImpl,
                            appArgs,
                            context,
                            includeGroupMembers: true);

                    if (hoSpec is not null)
                    {
                        collected = AddToCollected(collected, targetDeclName, hoSpec);
                    }

                    collected = MergeCollected(collected, groupMemberSpecs);
                }

                // For non-recursive functions that would be inlined, recurse into the inlined body
                if (!resolved.FunctionInfo.IsRecursive &&
                    !context.InliningStack.Contains(resolved.QualifiedName) &&
                    ShouldInline(funcParams, funcImpl.Expression, appArgs, context))
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
                new DeclQualifiedName(specialization.ConstructorName.ModuleName, specialization.ConstructorName.Name),
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
            if (funcParams[i].Value is not SyntaxTypes.Pattern.VarPattern varPattern)
                continue;

            if (!IsFunctionExpression(appArgs[i].Value, context))
                continue;

            if (!IsLoopInvariantInRecursiveCalls(
                    funcImpl.Expression.Value, funcName, funcModuleName, varPattern.Name, i))
                continue;

            var classified = ParameterSpecialization.ClassifyArgument(appArgs[i].Value);

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
        if (ElmSyntaxTransformations.CountExpressionNodes(funcImpl.Expression.Value) > 2000)
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
                        new DeclQualifiedName(member.ModuleName, memberImpl.Name.Value),
                        new FunctionSpecialization(builtParamSpecs));
            }
        }

        return (new FunctionSpecialization(builtParamSpecs), groupMemberSpecs);
    }

    /// <summary>
    /// Builds a <see cref="SpecializationCatalog"/> from collected specialization requests.
    /// The input dictionary already contains deduplicated specialization sets per function
    /// (via <see cref="ImmutableHashSet{T}"/>), so no additional deduplication is needed.
    /// </summary>
    private static SpecializationCatalog BuildCatalogFromCollectedSpecializations(
        CollectedSpecializations collected,
        IReadOnlySet<string>? existingDeclNames = null)
    {
        // Name and build catalog
        var allNamed = new List<NamedSpecialization>();
        var usedNames = existingDeclNames is not null ? new HashSet<string>(existingDeclNames) : [];

        foreach (var kvp in collected)
        {
            // Sort deterministically before naming to ensure stable __specialized__N numbering
            // regardless of ImmutableHashSet iteration order.
            var sorted = kvp.Value.OrderBy(s => s.DeterministicSortKey).ToList();
            var named = SpecializationCatalog.NameSpecializations(kvp.Key, sorted, usedNames);

            // Track newly assigned names to avoid clashes across different functions.
            foreach (var n in named)
                usedNames.Add(n.SpecializedFunctionName);

            allNamed.AddRange(named);
        }

        return SpecializationCatalog.BuildCatalog(allNamed);
    }
}
