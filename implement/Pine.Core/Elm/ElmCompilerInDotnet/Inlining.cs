using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

// Alias to avoid ambiguity with System.Range
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// See the file 'explore-early-instantiation-stage.md' for design notes.
/// </summary>
public class Inlining
{
    /// <summary>
    /// Zero-based location used for generated syntax nodes.
    /// Per design notes: "we use the value 0 for all locations (row, column) and ranges for newly created syntax nodes."
    /// These will be used in future cross-module inlining when creating new syntax nodes.
    /// </summary>
    private static readonly Location s_zeroLocation = new(Row: 0, Column: 0);

    /// <summary>
    /// Zero range used for generated syntax nodes. See <see cref="s_zeroLocation"/> for details.
    /// </summary>
    private static readonly Range s_zeroRange = new(Start: s_zeroLocation, End: s_zeroLocation);

    /// <summary>
    /// Singleton comparer for module name tuples to avoid repeated allocations.
    /// </summary>
    private static readonly ModuleNameTupleComparer s_moduleNameTupleComparer = new();

    public abstract record Config
    {
        /// <summary>
        /// Attempt inlining only for function applications which supply functions as arguments.
        /// </summary>
        public static readonly Config OnlyFunctions = new InlineOnlyFunctions();

        /// <summary>
        /// Attempt inlining only for function applications which supply functions as arguments.
        /// </summary>
        internal sealed record InlineOnlyFunctions
            : Config;
    }

    /// <summary>
    /// Represents a function declaration with its containing module for inlining purposes.
    /// </summary>
    private record FunctionInfo(
        ModuleName ModuleName,
        SyntaxTypes.FunctionStruct Function,
        bool IsRecursive);

    /// <summary>
    /// Information about a constructor that belongs to a single-constructor custom type.
    /// Built from <see cref="SyntaxTypes.Declaration.CustomTypeDeclaration"/> during pre-processing.
    /// </summary>
    private record SingleChoiceConstructorInfo(
        SyntaxTypes.QualifiedNameRef ConstructorName,
        int FieldCount);

    /// <summary>
    /// Context for inlining operations, including all function definitions and configuration.
    /// </summary>
    private record InliningContext(
        ImmutableDictionary<(ModuleName ModuleName, string FunctionName), FunctionInfo> FunctionsByQualifiedName,
        ImmutableDictionary<SyntaxTypes.QualifiedNameRef, SingleChoiceConstructorInfo> SingleChoiceConstructors,
        ImmutableDictionary<string, TypeInference.InferredType> FunctionSignatures,
        Config Config,
        ImmutableHashSet<(ModuleName ModuleName, string FunctionName)> InliningStack,
        InliningFunctionSpecialization.SpecializationCatalog SpecializationCatalog,
        ModuleName? CurrentModuleName = null);


    public static Result<string, IReadOnlyDictionary<ModuleName, SyntaxTypes.File>> Inline(
        IReadOnlyList<SyntaxTypes.File> modules,
        Config config)
    {
        // Build a dictionary of all function declarations across all modules
        var functionsByQualifiedName = BuildFunctionDictionary(modules);

        // Mark recursive functions
        var functionsWithRecursionInfo = MarkRecursiveFunctions(functionsByQualifiedName);

        // Build type context: identify constructors of single-constructor custom types
        var singleChoiceConstructors = BuildSingleChoiceConstructors(modules);

        // Build function signatures from type annotations for type-aware function detection
        var functionSignatures = BuildFunctionSignatures(modules);

        // --- Pass 1: Collect all specialization requests ---
        var collectionContext =
            new InliningContext(
                functionsWithRecursionInfo,
                singleChoiceConstructors,
                functionSignatures,
                config,
                ImmutableHashSet<(ModuleName ModuleName, string FunctionName)>.Empty
                .WithComparer(s_moduleNameTupleComparer),
                SpecializationCatalog: InliningFunctionSpecialization.SpecializationCatalog.Empty);

        var collectedSpecializations =
            CollectSpecializationsFromModules(modules, collectionContext);

        // --- Naming: Deduplicate and assign deterministic names ---
        var catalog = BuildCatalogFromCollectedSpecializations(collectedSpecializations);

        // --- Pass 2: Rewrite using the catalog ---
        var rewriteContext =
            new InliningContext(
                functionsWithRecursionInfo,
                singleChoiceConstructors,
                functionSignatures,
                config,
                ImmutableHashSet<(ModuleName ModuleName, string FunctionName)>.Empty
                .WithComparer(s_moduleNameTupleComparer),
                SpecializationCatalog: catalog);

        var result =
            new Dictionary<ModuleName, SyntaxTypes.File>(
                EnumerableExtensions.EqualityComparer<ModuleName>());

        foreach (var module in modules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;
            var moduleContext = rewriteContext with { CurrentModuleName = moduleName };
            var inlinedModule = InlineModule(module, moduleContext);
            result[moduleName] = inlinedModule;
        }

        return result;
    }

    /// <summary>
    /// Pass 1: Walk all modules and collect specialization requests.
    /// This simulates the inlining walk (including expanding non-recursive functions)
    /// but instead of creating specialized declarations, it records what specializations
    /// are needed via <see cref="InliningFunctionSpecialization.FunctionSpecialization"/>.
    /// </summary>
    private static List<InliningFunctionSpecialization.FunctionSpecialization> CollectSpecializationsFromModules(
        IReadOnlyList<SyntaxTypes.File> modules,
        InliningContext context)
    {
        var collected = new List<InliningFunctionSpecialization.FunctionSpecialization>();

        foreach (var module in modules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;
            var moduleContext = context with { CurrentModuleName = moduleName };

            foreach (var decl in module.Declarations)
            {
                if (decl.Value is SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                {
                    CollectSpecializationsFromExpression(
                        funcDecl.Function.Declaration.Value.Expression,
                        moduleContext,
                        collected);
                }
            }
        }

        return collected;
    }

    /// <summary>
    /// Recursively walks an expression tree to find call sites that would trigger
    /// specialization, and collects the corresponding <see cref="InliningFunctionSpecialization.FunctionSpecialization"/>
    /// requests.
    /// </summary>
    private static void CollectSpecializationsFromExpression(
        Node<SyntaxTypes.Expression> exprNode,
        InliningContext context,
        List<InliningFunctionSpecialization.FunctionSpecialization> collected)
    {
        switch (exprNode.Value)
        {
            case SyntaxTypes.Expression.Application app:
                CollectSpecializationsFromApplication(app, context, collected);
                break;

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                CollectSpecializationsFromExpression(paren.Expression, context, collected);
                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                CollectSpecializationsFromExpression(ifBlock.Condition, context, collected);
                CollectSpecializationsFromExpression(ifBlock.ThenBlock, context, collected);
                CollectSpecializationsFromExpression(ifBlock.ElseBlock, context, collected);
                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                CollectSpecializationsFromExpression(caseExpr.CaseBlock.Expression, context, collected);

                foreach (var branch in caseExpr.CaseBlock.Cases)
                    CollectSpecializationsFromExpression(branch.Expression, context, collected);

                break;

            case SyntaxTypes.Expression.LetExpression letExpr:
                foreach (var d in letExpr.Value.Declarations)
                {
                    switch (d.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            CollectSpecializationsFromExpression(
                                letFunc.Function.Declaration.Value.Expression,
                                context,
                                collected);

                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            CollectSpecializationsFromExpression(letDestr.Expression, context, collected);
                            break;
                    }
                }

                CollectSpecializationsFromExpression(letExpr.Value.Expression, context, collected);
                break;

            case SyntaxTypes.Expression.LambdaExpression lambda:
                CollectSpecializationsFromExpression(lambda.Lambda.Expression, context, collected);
                break;

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var elem in listExpr.Elements)
                    CollectSpecializationsFromExpression(elem, context, collected);

                break;

            case SyntaxTypes.Expression.TupledExpression tupled:
                foreach (var elem in tupled.Elements)
                    CollectSpecializationsFromExpression(elem, context, collected);

                break;

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                    CollectSpecializationsFromExpression(field.Value.Item2, context, collected);

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields)
                    CollectSpecializationsFromExpression(field.Value.Item2, context, collected);

                break;

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                CollectSpecializationsFromExpression(recordAccess.Record, context, collected);
                break;

            case SyntaxTypes.Expression.Negation negation:
                CollectSpecializationsFromExpression(negation.Expression, context, collected);
                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                CollectSpecializationsFromExpression(opApp.Left, context, collected);
                CollectSpecializationsFromExpression(opApp.Right, context, collected);
                break;
        }
    }

    /// <summary>
    /// Analyzes a function application to determine if it requires specialization.
    /// If so, adds a <see cref="InliningFunctionSpecialization.FunctionSpecialization"/> to the collection.
    /// Also recurses into inlined function bodies to discover nested specialization needs.
    /// </summary>
    private static void CollectSpecializationsFromApplication(
        SyntaxTypes.Expression.Application app,
        InliningContext context,
        List<InliningFunctionSpecialization.FunctionSpecialization> collected)
    {
        if (app.Arguments.Count < 2)
        {
            foreach (var arg in app.Arguments)
                CollectSpecializationsFromExpression(arg, context, collected);

            return;
        }

        var funcExpr = app.Arguments[0].Value;

        // Handle pipe operators: recurse into desugared form
        if (app.Arguments.Count >= 3 &&
            funcExpr is SyntaxTypes.Expression.FunctionOrValue pipeFunc &&
            pipeFunc.ModuleName.Count is 1 && pipeFunc.ModuleName[0] is "Basics")
        {
            if (pipeFunc.Name is "apR")
            {
                var desugaredArgs = new List<Node<SyntaxTypes.Expression>> { app.Arguments[2], app.Arguments[1] };
                desugaredArgs.AddRange(app.Arguments.Skip(3));

                CollectSpecializationsFromApplication(
                    new SyntaxTypes.Expression.Application([.. desugaredArgs]),
                    context,
                    collected);

                return;
            }

            if (pipeFunc.Name is "apL")
            {
                var desugaredArgs = new List<Node<SyntaxTypes.Expression>>(app.Arguments.Skip(1));

                CollectSpecializationsFromApplication(
                    new SyntaxTypes.Expression.Application([.. desugaredArgs]),
                    context,
                    collected);

                return;
            }

            if (pipeFunc.Name is "composeR" or "composeL")
            {
                foreach (var arg in app.Arguments.Skip(1))
                    CollectSpecializationsFromExpression(arg, context, collected);

                return;
            }
        }

        // Check for known function call
        if (funcExpr is SyntaxTypes.Expression.FunctionOrValue funcOrValue)
        {
            var qualifiedName = (funcOrValue.ModuleName, funcOrValue.Name);

            if (context.FunctionsByQualifiedName.TryGetValue(qualifiedName, out var funcInfo))
            {
                var funcImpl = funcInfo.Function.Declaration.Value;
                var funcParams = funcImpl.Arguments;
                var appArgs = app.Arguments.Skip(1).ToList();

                // Check for single-choice tag specialization opportunity
                var tagSpec =
                    TryBuildFunctionSpecializationForSingleChoiceTag(
                        funcInfo,
                        funcImpl,
                        appArgs,
                        context);

                if (tagSpec is not null)
                    collected.Add(tagSpec);

                // Check for higher-order recursive specialization opportunity
                if (funcInfo.IsRecursive && ShouldInline(funcParams, funcImpl.Expression, appArgs, context))
                {
                    var hoSpec =
                        TryBuildFunctionSpecializationForHigherOrder(
                            funcInfo,
                            funcImpl,
                            appArgs,
                            context,
                            additionalCollected: collected);

                    if (hoSpec is not null)
                        collected.Add(hoSpec);
                }

                // For non-recursive functions that would be inlined, recurse into the inlined body
                if (!funcInfo.IsRecursive &&
                    !context.InliningStack.Contains(qualifiedName) &&
                    ShouldInline(funcParams, funcImpl.Expression, appArgs, context))
                {
                    var newContext = context with { InliningStack = context.InliningStack.Add(qualifiedName) };

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

                    var substitutedBody = SubstituteInExpression(funcImpl.Expression, substitutions);
                    CollectSpecializationsFromExpression(substitutedBody, newContext, collected);
                }
            }
        }

        // Always recurse into arguments
        foreach (var arg in app.Arguments)
            CollectSpecializationsFromExpression(arg, context, collected);
    }

    /// <summary>
    /// Builds a <see cref="InliningFunctionSpecialization.FunctionSpecialization"/> for a
    /// single-choice tag specialization opportunity.
    /// Returns null if no single-choice tag specialization is applicable.
    /// </summary>
    private static InliningFunctionSpecialization.FunctionSpecialization?
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
            ImmutableDictionary.CreateBuilder<int, InliningFunctionSpecialization.ParameterSpecialization>();

        paramSpecs[specialization.ParamIndex] =
            BuildSingleChoiceTagUnwrapSpec(specialization, context);

        return
            new InliningFunctionSpecialization.FunctionSpecialization(
                funcImpl.Name.Value,
                funcInfo.ModuleName,
                paramSpecs.ToImmutable());
    }

    /// <summary>
    /// Builds a <see cref="InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap"/>
    /// that includes function field specializations for proper deduplication.
    /// </summary>
    private static InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap
        BuildSingleChoiceTagUnwrapSpec(
        SingleChoiceTagSpecialization specialization,
        InliningContext context)
    {
        var fieldSpecs =
            ImmutableDictionary.CreateBuilder<int, InliningFunctionSpecialization.ParameterSpecialization>();

        foreach (var fieldPlan in specialization.FieldPlans)
        {
            if (fieldPlan.ParameterSpecialization is { } nestedSpecialization)
            {
                fieldSpecs[fieldPlan.FieldIndex] = nestedSpecialization;
            }
        }

        return
            new InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap(
                specialization.ConstructorName,
                fieldSpecs.ToImmutable());
    }

    /// <summary>
    /// Builds a <see cref="InliningFunctionSpecialization.FunctionSpecialization"/> for a
    /// higher-order recursive function specialization opportunity.
    /// Returns null if no specialization is applicable.
    /// </summary>
    private static InliningFunctionSpecialization.FunctionSpecialization?
        TryBuildFunctionSpecializationForHigherOrder(
        FunctionInfo funcInfo,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        InliningContext context,
        List<InliningFunctionSpecialization.FunctionSpecialization>? additionalCollected = null)
    {
        var funcParams = funcImpl.Arguments;
        var funcName = funcImpl.Name.Value;
        var funcModuleName = funcInfo.ModuleName;

        var paramSpecs =
            ImmutableDictionary.CreateBuilder<int, InliningFunctionSpecialization.ParameterSpecialization>();

        for (var i = 0; i < funcParams.Count && i < appArgs.Count; i++)
        {
            if (funcParams[i].Value is not SyntaxTypes.Pattern.VarPattern varPattern)
                continue;

            if (!IsFunctionExpression(appArgs[i].Value, context))
                continue;

            if (!IsLoopInvariantInRecursiveCalls(
                    funcImpl.Expression.Value, funcName, funcModuleName, varPattern.Name, i))
                continue;

            var classified = InliningFunctionSpecialization.ClassifyArgument(appArgs[i].Value);

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
                    new InliningFunctionSpecialization.ParameterSpecialization.ConcreteLambdaValue(
                        new SyntaxTypes.LambdaStruct(
                            Arguments: [],
                            Expression: appArgs[i]));
            }
        }

        if (paramSpecs.Count is 0)
            return null;

        // Skip specialization for very large function bodies
        if (CountExpressionNodes(funcImpl.Expression.Value) > 2000)
            return null;

        var builtParamSpecs = paramSpecs.ToImmutable();

        // Also collect specializations for mutual recursion group members.
        if (additionalCollected is not null)
        {
            var invariantIndicesSet = new HashSet<int>(paramSpecs.Keys);
            var groupMembers = FindMutualRecursionGroupMembers(funcInfo, invariantIndicesSet, context);

            foreach (var member in groupMembers)
            {
                var memberImpl = member.Function.Declaration.Value;

                additionalCollected.Add(
                    new InliningFunctionSpecialization.FunctionSpecialization(
                        memberImpl.Name.Value,
                        member.ModuleName,
                        builtParamSpecs));
            }
        }

        return
            new InliningFunctionSpecialization.FunctionSpecialization(
                funcName,
                funcModuleName,
                builtParamSpecs);
    }

    /// <summary>
    /// Builds a <see cref="InliningFunctionSpecialization.SpecializationCatalog"/> from collected
    /// specialization requests. Deduplicates requests per function and assigns deterministic names.
    /// </summary>
    private static InliningFunctionSpecialization.SpecializationCatalog BuildCatalogFromCollectedSpecializations(
        List<InliningFunctionSpecialization.FunctionSpecialization> collected)
    {
        // Group by target function
        var byFunction =
            new Dictionary<(ModuleName, string), List<InliningFunctionSpecialization.FunctionSpecialization>>(
                InliningFunctionSpecialization.ModuleNameTupleComparer.Instance);

        foreach (var spec in collected)
        {
            var key = (spec.TargetModuleName, spec.TargetFunction);

            if (!byFunction.TryGetValue(key, out var list))
            {
                list = [];
                byFunction[key] = list;
            }

            // Deduplicate: only add if not already present
            var isDuplicate = false;

            foreach (var existing in list)
            {
                if (existing.Equals(spec))
                {
                    isDuplicate = true;
                    break;
                }
            }

            if (!isDuplicate)
                list.Add(spec);
        }

        // Name and build catalog
        var allNamed = new List<InliningFunctionSpecialization.NamedSpecialization>();

        foreach (var kvp in byFunction)
        {
            var named = InliningFunctionSpecialization.NameSpecializations(kvp.Key.Item2, kvp.Value);
            allNamed.AddRange(named);
        }

        return InliningFunctionSpecialization.BuildCatalog(allNamed);
    }

    /// <summary>
    /// Looks up the pre-assigned specialized name from the catalog for a given function
    /// and a single parameter specialization.
    /// Returns null if no matching specialization was found in the catalog.
    /// </summary>
    private static string? LookupSpecializedName(
        string funcName,
        ModuleName funcModuleName,
        int paramIndex,
        InliningFunctionSpecialization.ParameterSpecialization paramSpec,
        InliningContext context)
    {
        var key = ((ModuleName)funcModuleName, funcName);

        if (!context.SpecializationCatalog.SpecializationsByFunction.TryGetValue(key, out var specializations))
            return null;

        var target =
            new InliningFunctionSpecialization.FunctionSpecialization(
                funcName,
                funcModuleName,
                ImmutableDictionary<int, InliningFunctionSpecialization.ParameterSpecialization>.Empty
                .Add(paramIndex, paramSpec));

        foreach (var named in specializations)
        {
            if (named.Specialization.Equals(target))
                return named.SpecializedFunctionName;
        }

        return null;
    }

    /// <summary>
    /// Looks up the pre-assigned specialized name from the catalog for a given function
    /// and a set of parameter specializations (for higher-order recursive specialization).
    /// Returns null if no matching specialization was found in the catalog.
    /// </summary>
    private static string? LookupSpecializedNameForHigherOrder(
        string funcName,
        ModuleName funcModuleName,
        ImmutableDictionary<int, InliningFunctionSpecialization.ParameterSpecialization> paramSpecs,
        InliningContext context)
    {
        var key = ((ModuleName)funcModuleName, funcName);

        if (!context.SpecializationCatalog.SpecializationsByFunction.TryGetValue(key, out var specializations))
            return null;

        var target =
            new InliningFunctionSpecialization.FunctionSpecialization(
                funcName,
                funcModuleName,
                paramSpecs);

        foreach (var named in specializations)
        {
            if (named.Specialization.Equals(target))
                return named.SpecializedFunctionName;
        }

        return null;
    }

    private static ImmutableDictionary<(ModuleName ModuleName, string FunctionName), FunctionInfo> MarkRecursiveFunctions(
        ImmutableDictionary<(ModuleName ModuleName, string FunctionName), FunctionInfo> functions)
    {
        var builder = functions.ToBuilder();

        foreach (var kvp in functions)
        {
            var funcKey = kvp.Key;
            var funcInfo = kvp.Value;

            // Check if this function is recursive (directly or indirectly references itself)
            var isRecursive =
                IsRecursiveFunction(
                    funcKey,
                    funcInfo.Function,
                    functions,
                    ImmutableHashSet<(ModuleName, string)>.Empty.WithComparer(s_moduleNameTupleComparer));

            builder[funcKey] = funcInfo with { IsRecursive = isRecursive };
        }

        return builder.ToImmutable();
    }

    private static bool IsRecursiveFunction(
        (ModuleName ModuleName, string FunctionName) funcKey,
        SyntaxTypes.FunctionStruct func,
        ImmutableDictionary<(ModuleName ModuleName, string FunctionName), FunctionInfo> allFunctions,
        ImmutableHashSet<(ModuleName, string)> visited)
    {
        // Collect all function references in the function body
        var references = CollectFunctionReferences(func.Declaration.Value.Expression.Value);

        foreach (var refKey in references)
        {
            // Check if this reference is the function itself (direct recursion)
            if (s_moduleNameTupleComparer.Equals(refKey, funcKey))
            {
                return true;
            }

            // Check if we've already visited this function in the current path (indirect recursion)
            if (visited.Contains(refKey))
            {
                continue; // Skip to avoid infinite loop in analysis, but this doesn't indicate funcKey is recursive
            }

            // Check if referenced function eventually calls back to this function (indirect recursion)
            if (allFunctions.TryGetValue(refKey, out var refFuncInfo))
            {
                var newVisited = visited.Add(refKey);

                if (IsRecursiveFunction(funcKey, refFuncInfo.Function, allFunctions, newVisited))
                {
                    return true;
                }
            }
        }

        return false;
    }

    private static IEnumerable<(ModuleName ModuleName, string FunctionName)> CollectFunctionReferences(
        SyntaxTypes.Expression expr)
    {
        return expr switch
        {
            SyntaxTypes.Expression.FunctionOrValue funcOrValue =>
            [(funcOrValue.ModuleName, funcOrValue.Name)],

            SyntaxTypes.Expression.Application app =>
            app.Arguments.SelectMany(a => CollectFunctionReferences(a.Value)),

            SyntaxTypes.Expression.ParenthesizedExpression paren =>
            CollectFunctionReferences(paren.Expression.Value),

            SyntaxTypes.Expression.IfBlock ifBlock =>
            CollectFunctionReferences(ifBlock.Condition.Value)
            .Concat(CollectFunctionReferences(ifBlock.ThenBlock.Value))
            .Concat(CollectFunctionReferences(ifBlock.ElseBlock.Value)),

            SyntaxTypes.Expression.CaseExpression caseExpr =>
            CollectFunctionReferences(caseExpr.CaseBlock.Expression.Value)
            .Concat(caseExpr.CaseBlock.Cases.SelectMany(c => CollectFunctionReferences(c.Expression.Value))),

            SyntaxTypes.Expression.LetExpression letExpr =>
            letExpr.Value.Declarations.SelectMany(d => CollectFunctionReferencesFromLetDeclaration(d.Value))
            .Concat(CollectFunctionReferences(letExpr.Value.Expression.Value)),

            SyntaxTypes.Expression.LambdaExpression lambda =>
            CollectFunctionReferences(lambda.Lambda.Expression.Value),

            SyntaxTypes.Expression.ListExpr listExpr =>
            listExpr.Elements.SelectMany(e => CollectFunctionReferences(e.Value)),

            SyntaxTypes.Expression.TupledExpression tupled =>
            tupled.Elements.SelectMany(e => CollectFunctionReferences(e.Value)),

            SyntaxTypes.Expression.RecordExpr recordExpr =>
            recordExpr.Fields.SelectMany(f => CollectFunctionReferences(f.Value.valueExpr.Value)),

            SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
            recordUpdate.Fields.SelectMany(f => CollectFunctionReferences(f.Value.valueExpr.Value)),

            SyntaxTypes.Expression.RecordAccess recordAccess =>
            CollectFunctionReferences(recordAccess.Record.Value),

            SyntaxTypes.Expression.Negation negation =>
            CollectFunctionReferences(negation.Expression.Value),

            SyntaxTypes.Expression.OperatorApplication opApp =>
            CollectFunctionReferences(opApp.Left.Value)
            .Concat(CollectFunctionReferences(opApp.Right.Value)),

            _ =>
            []
        };
    }

    private static IEnumerable<(ModuleName ModuleName, string FunctionName)> CollectFunctionReferencesFromLetDeclaration(
        SyntaxTypes.Expression.LetDeclaration letDecl)
    {
        return letDecl switch
        {
            SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
            CollectFunctionReferences(letFunc.Function.Declaration.Value.Expression.Value),

            SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
            CollectFunctionReferences(letDestr.Expression.Value),

            _ =>
            []
        };
    }

    private static ImmutableDictionary<(ModuleName ModuleName, string FunctionName), FunctionInfo> BuildFunctionDictionary(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        var builder =
            ImmutableDictionary.CreateBuilder<(ModuleName ModuleName, string FunctionName), FunctionInfo>(
                s_moduleNameTupleComparer);

        foreach (var module in modules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;

            foreach (var decl in module.Declarations)
            {
                if (decl.Value is SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                {
                    var funcName = funcDecl.Function.Declaration.Value.Name.Value;
                    var key = (moduleName, funcName);
                    builder[key] = new FunctionInfo(moduleName, funcDecl.Function, IsRecursive: false);
                }
            }
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Builds a dictionary mapping each constructor of single-constructor custom types
    /// to its <see cref="SingleChoiceConstructorInfo"/>. This replaces the former syntactic
    /// body-scanning heuristic with a definitive type-based check.
    /// </summary>
    private static ImmutableDictionary<SyntaxTypes.QualifiedNameRef, SingleChoiceConstructorInfo> BuildSingleChoiceConstructors(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        var builder =
            ImmutableDictionary.CreateBuilder<SyntaxTypes.QualifiedNameRef, SingleChoiceConstructorInfo>();

        foreach (var module in modules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;

            foreach (var decl in module.Declarations)
            {
                if (decl.Value is not SyntaxTypes.Declaration.CustomTypeDeclaration customTypeDecl)
                    continue;

                var typeStruct = customTypeDecl.TypeDeclaration;

                // Only single-constructor types qualify
                if (typeStruct.Constructors.Count is not 1)
                    continue;

                var constructor = typeStruct.Constructors[0].Value;
                var constructorName = new SyntaxTypes.QualifiedNameRef(moduleName, constructor.Name.Value);

                builder[constructorName] =
                    new SingleChoiceConstructorInfo(
                        ConstructorName: constructorName,
                        FieldCount: constructor.Arguments.Count);
            }
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Builds a combined function signatures map from all modules using
    /// <see cref="TypeInference.BuildFunctionSignaturesMap"/>. This enables type-aware
    /// function detection in <see cref="IsFunctionExpression"/>.
    /// </summary>
    private static ImmutableDictionary<string, TypeInference.InferredType> BuildFunctionSignatures(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        var builder =
            ImmutableDictionary.CreateBuilder<string, TypeInference.InferredType>();

        foreach (var module in modules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;
            var moduleNameString = string.Join(".", moduleName);

            var moduleSignatures = TypeInference.BuildFunctionSignaturesMap(module, moduleNameString);

            foreach (var kvp in moduleSignatures)
            {
                builder[kvp.Key] = kvp.Value;
            }
        }

        return builder.ToImmutable();
    }

    private static SyntaxTypes.File InlineModule(SyntaxTypes.File module, InliningContext context)
    {
        var newDecls = ImmutableList.CreateBuilder<Node<SyntaxTypes.Declaration>>();
        var inlinedDeclarations = new List<Node<SyntaxTypes.Declaration>>();

        foreach (var decl in module.Declarations)
        {
            var (inlinedDecl, decls) = InlineDeclaration(decl, context);
            inlinedDeclarations.Add(inlinedDecl);
            newDecls.AddRange(decls);
        }

        // Inline the generated specialized functions to beta-reduce any
        // lambda applications introduced by parameter substitution
        // (e.g., (\x -> [x, x]) first  →  [first, first]).
        foreach (var newDecl in newDecls)
        {
            var (inlinedNewDecl, furtherDecls) = InlineDeclaration(newDecl, context);
            inlinedDeclarations.Add(inlinedNewDecl);
            inlinedDeclarations.AddRange(furtherDecls);
        }

        var simplificationFunctions = context.FunctionsByQualifiedName.ToBuilder();

        foreach (var rewrittenFunction in MarkRecursiveFunctions(
            BuildFunctionDictionary(
                [
                module with
                {
                    Declarations = inlinedDeclarations
                }
                ])))
        {
            simplificationFunctions[rewrittenFunction.Key] = rewrittenFunction.Value;
        }

        var simplificationContext =
            context with
            {
                FunctionsByQualifiedName = simplificationFunctions.ToImmutable()
            };

        var simplified =
            inlinedDeclarations
            .Select(decl => SimplifyGeneratedDeclaration(decl, simplificationContext))
            .ToList();

        // Post-process: ensure all Application arguments are parenthesized where necessary
        var parenthesized =
            simplified
            .Select(ParenthesizeDeclaration)
            .ToList();

        return module with { Declarations = parenthesized };
    }

    private static Node<SyntaxTypes.Declaration> ParenthesizeDeclaration(
        Node<SyntaxTypes.Declaration> declNode)
    {
        if (declNode.Value is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
        {
            return declNode;
        }

        var impl = funcDecl.Function.Declaration.Value;

        var parenthesizedExpr = ParenthesizeApplicationArgumentsRecursive(impl.Expression);

        var newImpl = impl with { Expression = parenthesizedExpr };

        var newFunc =
            funcDecl.Function with
            {
                Declaration =
                new Node<SyntaxTypes.FunctionImplementation>(
                    funcDecl.Function.Declaration.Range,
                    newImpl)
            };

        return
            new Node<SyntaxTypes.Declaration>(
                declNode.Range,
                new SyntaxTypes.Declaration.FunctionDeclaration(newFunc));
    }

    private static Node<SyntaxTypes.Declaration> SimplifyGeneratedDeclaration(
        Node<SyntaxTypes.Declaration> declNode,
        InliningContext context)
    {
        if (declNode.Value is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
        {
            return declNode;
        }

        var impl = funcDecl.Function.Declaration.Value;
        var simplifiedExpr = SimplifyGeneratedExpressionRecursive(impl.Expression, context);
        var newImpl = impl with { Expression = simplifiedExpr };

        var newFunc =
            funcDecl.Function with
            {
                Declaration =
                new Node<SyntaxTypes.FunctionImplementation>(
                    funcDecl.Function.Declaration.Range,
                    newImpl)
            };

        return
            new Node<SyntaxTypes.Declaration>(
                declNode.Range,
                new SyntaxTypes.Declaration.FunctionDeclaration(newFunc));
    }

    private static Node<SyntaxTypes.Expression> SimplifyGeneratedExpressionRecursive(
        Node<SyntaxTypes.Expression> exprNode,
        InliningContext context)
    {
        Node<SyntaxTypes.Expression> Recurse(Node<SyntaxTypes.Expression> expr) =>
            SimplifyGeneratedExpressionRecursive(expr, context);

        switch (exprNode.Value)
        {
            case SyntaxTypes.Expression.LetExpression letExpr:
                {
                    var (simplifiedLet, newDecls) = InlineLetExpression(letExpr, context);

                    if (newDecls.Count is 0 &&
                        !simplifiedLet.Equals(exprNode.Value))
                    {
                        return
                            SimplifyGeneratedExpressionRecursive(
                                new Node<SyntaxTypes.Expression>(s_zeroRange, simplifiedLet),
                                context);
                    }

                    var simplifiedDeclarations =
                        letExpr.Value.Declarations
                        .Select(
                            declaration =>
                            {
                                var rewrittenDeclaration =
                                    declaration.Value switch
                                    {
                                        SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                                        new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                                            letFunc.Function with
                                            {
                                                Declaration =
                                                new Node<SyntaxTypes.FunctionImplementation>(
                                                    letFunc.Function.Declaration.Range,
                                                    letFunc.Function.Declaration.Value with
                                                    {
                                                        Expression =
                                                        Recurse(letFunc.Function.Declaration.Value.Expression)
                                                    })
                                            }),

                                        SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                                        new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                                            letDestr.Pattern,
                                            Recurse(letDestr.Expression)),

                                        _ =>
                                        declaration.Value
                                    };

                                return new Node<SyntaxTypes.Expression.LetDeclaration>(declaration.Range, rewrittenDeclaration);
                            })
                        .ToList();

                    var simplifiedBody = Recurse(letExpr.Value.Expression);

                    if (TryCollapseSingleChoiceWrapperPassThroughLet(
                        simplifiedDeclarations,
                        simplifiedBody,
                        out var collapsedLetExpr))
                    {
                        return SimplifyGeneratedExpressionRecursive(collapsedLetExpr, context);
                    }

                    if (TryCollapseSingleChoiceWrapperSingleUseLet(
                        simplifiedDeclarations,
                        simplifiedBody,
                        context,
                        out var substitutedLetExpr))
                    {
                        return SimplifyGeneratedExpressionRecursive(substitutedLetExpr, context);
                    }

                    return
                        new Node<SyntaxTypes.Expression>(
                            exprNode.Range,
                            new SyntaxTypes.Expression.LetExpression(
                                new SyntaxTypes.Expression.LetBlock(
                                    simplifiedDeclarations,
                                    simplifiedBody)));
                }

            case SyntaxTypes.Expression.Application app:
                {
                    var simplifiedApp =
                        new SyntaxTypes.Expression.Application([.. app.Arguments.Select(Recurse)]);

                    if (TryBetaReduceGeneratedApplication(
                        simplifiedApp,
                        out var reducedApp) &&
                        !reducedApp.Equals(simplifiedApp))
                    {
                        return
                            SimplifyGeneratedExpressionRecursive(
                                new Node<SyntaxTypes.Expression>(exprNode.Range, reducedApp),
                                context);
                    }

                    return
                        new Node<SyntaxTypes.Expression>(
                            exprNode.Range,
                            simplifiedApp);
                }

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                return
                    new Node<SyntaxTypes.Expression>(
                        exprNode.Range,
                        new SyntaxTypes.Expression.ParenthesizedExpression(Recurse(paren.Expression)));

            case SyntaxTypes.Expression.IfBlock ifBlock:
                return
                    new Node<SyntaxTypes.Expression>(
                        exprNode.Range,
                        new SyntaxTypes.Expression.IfBlock(
                            Recurse(ifBlock.Condition),
                            Recurse(ifBlock.ThenBlock),
                            Recurse(ifBlock.ElseBlock)));

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                return
                    new Node<SyntaxTypes.Expression>(
                        exprNode.Range,
                        new SyntaxTypes.Expression.CaseExpression(
                            new SyntaxTypes.CaseBlock(
                                Recurse(caseExpr.CaseBlock.Expression),
                                [
                                .. caseExpr.CaseBlock.Cases.Select(
                                    caseItem =>
                                    new SyntaxTypes.Case(
                                        caseItem.Pattern,
                                        Recurse(caseItem.Expression)))
                                ])));

            case SyntaxTypes.Expression.LambdaExpression lambda:
                return
                    new Node<SyntaxTypes.Expression>(
                        exprNode.Range,
                        new SyntaxTypes.Expression.LambdaExpression(
                            new SyntaxTypes.LambdaStruct(
                                lambda.Lambda.Arguments,
                                Recurse(lambda.Lambda.Expression))));

            case SyntaxTypes.Expression.ListExpr listExpr:
                return
                    new Node<SyntaxTypes.Expression>(
                        exprNode.Range,
                        new SyntaxTypes.Expression.ListExpr([.. listExpr.Elements.Select(Recurse)]));

            case SyntaxTypes.Expression.TupledExpression tupled:
                return
                    new Node<SyntaxTypes.Expression>(
                        exprNode.Range,
                        new SyntaxTypes.Expression.TupledExpression([.. tupled.Elements.Select(Recurse)]));

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                return
                    new Node<SyntaxTypes.Expression>(
                        exprNode.Range,
                        new SyntaxTypes.Expression.RecordExpr(
                            [
                            .. recordExpr.Fields.Select(
                                field =>
                                new Node<(Node<string>, Node<SyntaxTypes.Expression>)>(
                                    field.Range,
                                    (field.Value.fieldName, Recurse(field.Value.valueExpr))))
                            ]));

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                return
                    new Node<SyntaxTypes.Expression>(
                        exprNode.Range,
                        new SyntaxTypes.Expression.RecordUpdateExpression(
                            recordUpdate.RecordName,
                            [
                            .. recordUpdate.Fields.Select(
                                field =>
                                new Node<(Node<string>, Node<SyntaxTypes.Expression>)>(
                                    field.Range,
                                    (field.Value.fieldName, Recurse(field.Value.valueExpr))))
                            ]));

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                return
                    new Node<SyntaxTypes.Expression>(
                        exprNode.Range,
                        new SyntaxTypes.Expression.RecordAccess(
                            Recurse(recordAccess.Record),
                            recordAccess.FieldName));

            case SyntaxTypes.Expression.Negation negation:
                return
                    new Node<SyntaxTypes.Expression>(
                        exprNode.Range,
                        new SyntaxTypes.Expression.Negation(Recurse(negation.Expression)));

            case SyntaxTypes.Expression.OperatorApplication opApp:
                return
                    new Node<SyntaxTypes.Expression>(
                        exprNode.Range,
                        new SyntaxTypes.Expression.OperatorApplication(
                            opApp.Operator,
                            opApp.Direction,
                            Recurse(opApp.Left),
                            Recurse(opApp.Right)));

            default:
                return exprNode;
        }
    }

    private static bool TryCollapseSingleChoiceWrapperPassThroughLet(
        IReadOnlyList<Node<SyntaxTypes.Expression.LetDeclaration>> declarations,
        Node<SyntaxTypes.Expression> body,
        out Node<SyntaxTypes.Expression> collapsed)
    {
        collapsed = null!;

        if (declarations.Count is not 1 ||
            declarations[0].Value is not SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
        {
            return false;
        }

        var namedPattern = TryUnwrapToNamedPattern(letDestr.Pattern.Value);

        if (namedPattern is null)
        {
            return false;
        }

        if (!TryDeconstructConstructorApplication(
            body,
            out var constructorName,
            out var rebuiltFields))
        {
            return false;
        }

        if (!AreEquivalentConstructorNames(namedPattern.Name, constructorName) ||
            namedPattern.Arguments.Count != rebuiltFields.Count)
        {
            return false;
        }

        for (var index = 0; index < namedPattern.Arguments.Count; index++)
        {
            if (UnwrapParenthesizedPattern(namedPattern.Arguments[index].Value) is not SyntaxTypes.Pattern.VarPattern varPattern)
            {
                return false;
            }

            if (!IsReferencePreservingWrapperField(rebuiltFields[index].Value, varPattern.Name))
            {
                return false;
            }
        }

        collapsed = letDestr.Expression;
        return true;
    }

    private static bool TryCollapseSingleChoiceWrapperSingleUseLet(
        IReadOnlyList<Node<SyntaxTypes.Expression.LetDeclaration>> declarations,
        Node<SyntaxTypes.Expression> body,
        InliningContext context,
        out Node<SyntaxTypes.Expression> collapsed)
    {
        collapsed = null!;

        if (declarations.Count is not 1 ||
            declarations[0].Value is not SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
        {
            return false;
        }

        if (!TryDeconstructKnownConstructorApplicationForGeneratedSimplification(
            letDestr.Expression,
            context,
            out var constructorName,
            out var fieldExpressions) ||
            !TryBindSingleChoiceTagPattern(
                letDestr.Pattern.Value,
                constructorName,
                fieldExpressions,
                out var patternBindings) ||
            patternBindings.Count is 0)
        {
            return false;
        }

        foreach (var binding in patternBindings)
        {
            if (CountUnshadowedLocalVariableReferences(body.Value, binding.Key) is not 1)
            {
                return false;
            }
        }

        collapsed = SubstituteInExpression(body, patternBindings);
        return true;
    }

    private static bool TryDeconstructKnownConstructorApplicationForGeneratedSimplification(
        Node<SyntaxTypes.Expression> exprNode,
        InliningContext context,
        out SyntaxTypes.QualifiedNameRef constructorName,
        out IReadOnlyList<Node<SyntaxTypes.Expression>> fieldExpressions)
    {
        if (TryDeconstructExplicitConstructorApplication(
            exprNode.Value,
            out constructorName,
            out fieldExpressions))
        {
            return true;
        }

        switch (exprNode.Value)
        {
            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                return
                    TryDeconstructKnownConstructorApplicationForGeneratedSimplification(
                        paren.Expression,
                        context,
                        out constructorName,
                        out fieldExpressions);

            case SyntaxTypes.Expression.LetExpression letExpr:
                {
                    if (AreLetDeclarationsIgnorableForConstructorResolution(letExpr.Value.Declarations))
                    {
                        return
                            TryDeconstructKnownConstructorApplicationForGeneratedSimplification(
                                letExpr.Value.Expression,
                                context,
                                out constructorName,
                                out fieldExpressions);
                    }

                    var (inlinedLet, letDecls) = InlineLetExpression(letExpr, context);

                    if (letDecls.Count is 0)
                    {
                        if (inlinedLet is SyntaxTypes.Expression.LetExpression inlinedLetExpr &&
                            AreLetDeclarationsIgnorableForConstructorResolution(
                                inlinedLetExpr.Value.Declarations))
                        {
                            return
                                TryDeconstructKnownConstructorApplicationForGeneratedSimplification(
                                    inlinedLetExpr.Value.Expression,
                                    context,
                                    out constructorName,
                                    out fieldExpressions);
                        }

                        if (!inlinedLet.Equals(exprNode.Value))
                        {
                            return
                                TryDeconstructKnownConstructorApplicationForGeneratedSimplification(
                                    new Node<SyntaxTypes.Expression>(s_zeroRange, inlinedLet),
                                    context,
                                    out constructorName,
                                    out fieldExpressions);
                        }
                    }

                    break;
                }

            case SyntaxTypes.Expression.Application app
            when app.Arguments.Count > 0 &&
                 app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue funcOrValue &&
                 TryResolveKnownFunctionReference(funcOrValue, context, out var qualifiedName, out var funcInfo) &&
                 !funcInfo.IsRecursive &&
                 !context.InliningStack.Contains(qualifiedName):

                {
                    var funcImpl = funcInfo.Function.Declaration.Value;
                    var appArgs = app.Arguments.Skip(1).ToList();

                    if (appArgs.Count == funcImpl.Arguments.Count)
                    {
                        var newContext =
                            context with
                            {
                                InliningStack = context.InliningStack.Add(qualifiedName)
                            };

                        var (inlinedResult, inlinedDecls) =
                            InlineFunctionCall(
                                funcInfo.ModuleName,
                                funcImpl,
                                appArgs,
                                newContext);

                        if (inlinedDecls.Count is 0 &&
                            !inlinedResult.Equals(exprNode.Value))
                        {
                            return
                                TryDeconstructKnownConstructorApplicationForGeneratedSimplification(
                                    new Node<SyntaxTypes.Expression>(s_zeroRange, inlinedResult),
                                    context,
                                    out constructorName,
                                    out fieldExpressions);
                        }
                    }

                    break;
                }
        }

        constructorName = default!;
        fieldExpressions = [];
        return false;
    }

    private static bool TryDeconstructExplicitConstructorApplication(
        SyntaxTypes.Expression expr,
        out SyntaxTypes.QualifiedNameRef constructorName,
        out IReadOnlyList<Node<SyntaxTypes.Expression>> fieldExpressions)
    {
        switch (UnwrapParenthesized(expr))
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrValue when LooksLikeConstructorName(funcOrValue.Name):
                constructorName = new SyntaxTypes.QualifiedNameRef(funcOrValue.ModuleName, funcOrValue.Name);
                fieldExpressions = [];
                return true;

            case SyntaxTypes.Expression.Application app
            when app.Arguments.Count > 0 &&
                     app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue constructorRef &&
                     LooksLikeConstructorName(constructorRef.Name):

                constructorName = new SyntaxTypes.QualifiedNameRef(constructorRef.ModuleName, constructorRef.Name);
                fieldExpressions = [.. app.Arguments.Skip(1)];
                return true;

            default:
                constructorName = default!;
                fieldExpressions = [];
                return false;
        }
    }

    private static bool LooksLikeConstructorName(string name) =>
        name.Length > 0 && char.IsUpper(name[0]);

    private static bool TryBetaReduceGeneratedApplication(
        SyntaxTypes.Expression.Application app,
        out SyntaxTypes.Expression reduced)
    {
        reduced = null!;

        if (app.Arguments.Count < 2 ||
            UnwrapParenthesized(app.Arguments[0].Value) is not SyntaxTypes.Expression.LambdaExpression lambda)
        {
            return false;
        }

        var substitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();
        var consumedArgs = Math.Min(lambda.Lambda.Arguments.Count, app.Arguments.Count - 1);

        for (var index = 0; index < consumedArgs; index++)
        {
            if (UnwrapParenthesizedPattern(lambda.Lambda.Arguments[index].Value) is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                substitutions[varPattern.Name] = app.Arguments[index + 1];
            }
        }

        var substitutedBody = SubstituteInExpression(lambda.Lambda.Expression, substitutions);

        if (app.Arguments.Count - 1 < lambda.Lambda.Arguments.Count)
        {
            reduced =
                new SyntaxTypes.Expression.LambdaExpression(
                    new SyntaxTypes.LambdaStruct(
                        lambda.Lambda.Arguments.Skip(app.Arguments.Count - 1).ToList(),
                        substitutedBody));

            return true;
        }

        if (app.Arguments.Count - 1 == lambda.Lambda.Arguments.Count)
        {
            reduced = substitutedBody.Value;
            return true;
        }

        reduced =
            new SyntaxTypes.Expression.Application(
                [.. new[] { substitutedBody }.Concat(app.Arguments.Skip(lambda.Lambda.Arguments.Count + 1))]);

        return true;
    }

    private static bool IsReferencePreservingWrapperField(
        SyntaxTypes.Expression expr,
        string variableName)
    {
        expr = UnwrapParenthesized(expr);

        if (IsLocalVariableReference(expr, variableName))
        {
            return true;
        }

        if (expr is not SyntaxTypes.Expression.LambdaExpression lambda)
        {
            return false;
        }

        var expectedArguments = new List<Node<SyntaxTypes.Expression>>();

        foreach (var parameter in lambda.Lambda.Arguments)
        {
            switch (UnwrapParenthesizedPattern(parameter.Value))
            {
                case SyntaxTypes.Pattern.VarPattern varPattern:
                    expectedArguments.Add(
                        new Node<SyntaxTypes.Expression>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.FunctionOrValue([], varPattern.Name)));

                    break;

                case SyntaxTypes.Pattern.UnitPattern:
                    expectedArguments.Add(
                        new Node<SyntaxTypes.Expression>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.UnitExpr()));

                    break;

                default:
                    return false;
            }
        }

        var lambdaBody = UnwrapParenthesized(lambda.Lambda.Expression.Value);

        if (lambdaBody is not SyntaxTypes.Expression.Application app ||
            app.Arguments.Count != expectedArguments.Count + 1 ||
            !IsLocalVariableReference(UnwrapParenthesized(app.Arguments[0].Value), variableName))
        {
            return false;
        }

        for (var index = 0; index < expectedArguments.Count; index++)
        {
            if (!expectedArguments[index].Value.Equals(UnwrapParenthesized(app.Arguments[index + 1].Value)))
            {
                return false;
            }
        }

        return true;
    }

    private static (Node<SyntaxTypes.Declaration>, ImmutableList<Node<SyntaxTypes.Declaration>>) InlineDeclaration(
        Node<SyntaxTypes.Declaration> declNode,
        InliningContext context)
    {
        if (declNode.Value is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
        {
            return (declNode, ImmutableList<Node<SyntaxTypes.Declaration>>.Empty);
        }

        var (inlinedFunction, newDecls) = InlineFunctionStruct(funcDecl.Function, context);
        var inlinedDeclaration = new SyntaxTypes.Declaration.FunctionDeclaration(inlinedFunction);

        return (new Node<SyntaxTypes.Declaration>(declNode.Range, inlinedDeclaration), newDecls);
    }

    private static (SyntaxTypes.FunctionStruct, ImmutableList<Node<SyntaxTypes.Declaration>>) InlineFunctionStruct(
        SyntaxTypes.FunctionStruct func,
        InliningContext context)
    {
        var impl = func.Declaration.Value;
        var (inlinedExpr, newDecls) = InlineExpression(impl.Expression, context);

        var inlinedImpl =
            new SyntaxTypes.FunctionImplementation(
                Name: impl.Name,
                Arguments: impl.Arguments,
                Expression: inlinedExpr);

        return
            (func with
            {
                Declaration =
                new Node<SyntaxTypes.FunctionImplementation>(
                    func.Declaration.Range,
                    inlinedImpl)
            },
            newDecls);
    }

    private static (Node<SyntaxTypes.Expression>, ImmutableList<Node<SyntaxTypes.Declaration>>) InlineExpression(
        Node<SyntaxTypes.Expression> exprNode,
        InliningContext context)
    {
        var expr = exprNode.Value;
        var newDecls = ImmutableList.CreateBuilder<Node<SyntaxTypes.Declaration>>();

        Node<SyntaxTypes.Expression> Inline(Node<SyntaxTypes.Expression> e)
        {
            var (result, decls) = InlineExpression(e, context);
            newDecls.AddRange(decls);
            return result;
        }

        SyntaxTypes.Expression InlineApp(SyntaxTypes.Expression.Application app)
        {
            var (result, decls) = InlineApplication(app, context);
            newDecls.AddRange(decls);
            return result;
        }

        SyntaxTypes.CaseBlock InlineCaseB(SyntaxTypes.CaseBlock cb)
        {
            var (result, decls) = InlineCaseBlock(cb, context);
            newDecls.AddRange(decls);
            return result;
        }

        SyntaxTypes.Expression InlineLet(SyntaxTypes.Expression.LetExpression le)
        {
            var (result, decls) = InlineLetExpression(le, context);
            newDecls.AddRange(decls);
            return result;
        }

        SyntaxTypes.LambdaStruct InlineLambda(SyntaxTypes.LambdaStruct l)
        {
            var (result, decls) = InlineLambdaStruct(l, context);
            newDecls.AddRange(decls);
            return result;
        }

        Node<(Node<string>, Node<SyntaxTypes.Expression>)> InlineField(
            Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)> f)
        {
            var (result, decls) = InlineRecordField(f, context);
            newDecls.AddRange(decls);
            return result;
        }

        var inlinedExpr =
            expr switch
            {
                SyntaxTypes.Expression.Application app =>
                InlineApp(app),

                SyntaxTypes.Expression.ParenthesizedExpression paren =>
                new SyntaxTypes.Expression.ParenthesizedExpression(
                    Inline(paren.Expression)),

                SyntaxTypes.Expression.IfBlock ifBlock =>
                new SyntaxTypes.Expression.IfBlock(
                    Inline(ifBlock.Condition),
                    Inline(ifBlock.ThenBlock),
                    Inline(ifBlock.ElseBlock)),

                SyntaxTypes.Expression.CaseExpression caseExpr =>
                new SyntaxTypes.Expression.CaseExpression(
                    InlineCaseB(caseExpr.CaseBlock)),

                SyntaxTypes.Expression.LetExpression letExpr =>
                InlineLet(letExpr),

                SyntaxTypes.Expression.LambdaExpression lambda =>
                new SyntaxTypes.Expression.LambdaExpression(
                    InlineLambda(lambda.Lambda)),

                SyntaxTypes.Expression.ListExpr listExpr =>
                new SyntaxTypes.Expression.ListExpr(
                    [.. listExpr.Elements.Select(Inline)]),

                SyntaxTypes.Expression.TupledExpression tupled =>
                new SyntaxTypes.Expression.TupledExpression(
                    [.. tupled.Elements.Select(Inline)]),

                SyntaxTypes.Expression.RecordExpr recordExpr =>
                new SyntaxTypes.Expression.RecordExpr(
                    [.. recordExpr.Fields.Select(InlineField)]),

                SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [.. recordUpdate.Fields.Select(InlineField)]),

                SyntaxTypes.Expression.RecordAccess recordAccess =>
                new SyntaxTypes.Expression.RecordAccess(
                    Inline(recordAccess.Record),
                    recordAccess.FieldName),

                SyntaxTypes.Expression.Negation negation =>
                new SyntaxTypes.Expression.Negation(
                    Inline(negation.Expression)),

                SyntaxTypes.Expression.OperatorApplication opApp =>
                new SyntaxTypes.Expression.OperatorApplication(
                    opApp.Operator,
                    opApp.Direction,
                    Inline(opApp.Left),
                    Inline(opApp.Right)),

                // Leaf expressions that don't need transformation
                SyntaxTypes.Expression.UnitExpr or
                SyntaxTypes.Expression.Literal or
                SyntaxTypes.Expression.CharLiteral or
                SyntaxTypes.Expression.Integer or
                SyntaxTypes.Expression.Hex or
                SyntaxTypes.Expression.Floatable or
                SyntaxTypes.Expression.FunctionOrValue or
                SyntaxTypes.Expression.PrefixOperator or
                SyntaxTypes.Expression.RecordAccessFunction =>
                expr,

                _ =>
                expr
            };

        return (new Node<SyntaxTypes.Expression>(exprNode.Range, inlinedExpr), newDecls.ToImmutable());
    }

    private static (SyntaxTypes.Expression, ImmutableList<Node<SyntaxTypes.Declaration>>) InlineApplication(
        SyntaxTypes.Expression.Application app,
        InliningContext context)
    {
        var newDecls = ImmutableList.CreateBuilder<Node<SyntaxTypes.Declaration>>();

        Node<SyntaxTypes.Expression> Inline(Node<SyntaxTypes.Expression> e)
        {
            var (result, decls) = InlineExpression(e, context);
            newDecls.AddRange(decls);
            return result;
        }

        if (app.Arguments.Count < 2)
        {
            // No actual arguments, just recursively inline
            return
                (new SyntaxTypes.Expression.Application(
                    [.. app.Arguments.Select(Inline)]),
                newDecls.ToImmutable());
        }

        var funcExpr = app.Arguments[0].Value;

        // Desugar pipe operators: Basics.apR x f → f x, Basics.apL f x → f x
        // These operators take exactly 2 arguments (3 including the function reference in the AST).
        // We use >= 3 to also handle any extra arguments that would be applied to the result,
        // while skipping partial applications (count < 3) that cannot be fully desugared.
        if (app.Arguments.Count >= 3 &&
            funcExpr is SyntaxTypes.Expression.FunctionOrValue pipeFunc &&
            pipeFunc.ModuleName.Count is 1 && pipeFunc.ModuleName[0] is "Basics")
        {
            if (pipeFunc.Name is "apR")
            {
                // Basics.apR x f  →  f x
                var desugaredArgs = new List<Node<SyntaxTypes.Expression>> { app.Arguments[2], app.Arguments[1] };
                desugaredArgs.AddRange(app.Arguments.Skip(3));

                var (result, decls) =
                    InlineApplication(new SyntaxTypes.Expression.Application([.. desugaredArgs]), context);

                newDecls.AddRange(decls);

                return
                    (ParenthesizeApplicationArguments(result),
                    newDecls.ToImmutable());
            }

            if (pipeFunc.Name is "apL")
            {
                // Basics.apL f x  →  f x
                var desugaredArgs = new List<Node<SyntaxTypes.Expression>>(app.Arguments.Skip(1));

                var (result, decls) =
                    InlineApplication(new SyntaxTypes.Expression.Application([.. desugaredArgs]), context);

                newDecls.AddRange(decls);

                return
                    (ParenthesizeApplicationArguments(result),
                    newDecls.ToImmutable());
            }

            if (pipeFunc.Name is "composeR" or "composeL")
            {
                // composeR f g  →  \composeArg -> g (f composeArg)
                // composeL g f  →  \composeArg -> g (f composeArg)
                // For composeR: arg[1] = inner (applied first), arg[2] = outer (applied second)
                // For composeL: arg[1] = outer (applied second), arg[2] = inner (applied first)
                var inner = pipeFunc.Name is "composeR" ? app.Arguments[1] : app.Arguments[2];
                var outer = pipeFunc.Name is "composeR" ? app.Arguments[2] : app.Arguments[1];

                // Count == 3 means the operator + 2 function arguments (no application argument yet).
                // Count > 3 means extra arguments are being applied to the composed result.
                if (app.Arguments.Count == 3)
                {
                    // No application argument: produce a lambda \composeArg -> outer (inner composeArg)
                    var param =
                        new Node<SyntaxTypes.Pattern>(
                            s_zeroRange,
                            new SyntaxTypes.Pattern.VarPattern("composeArg"));

                    var paramRef =
                        new Node<SyntaxTypes.Expression>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.FunctionOrValue([], "composeArg"));

                    var innerApp =
                        new Node<SyntaxTypes.Expression>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.Application([inner, paramRef]));

                    var parenInnerApp =
                        new Node<SyntaxTypes.Expression>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.ParenthesizedExpression(innerApp));

                    var bodyExpr = new SyntaxTypes.Expression.Application([outer, parenInnerApp]);
                    var bodyNode = new Node<SyntaxTypes.Expression>(s_zeroRange, bodyExpr);
                    var inlinedBody = Inline(bodyNode);

                    return
                        (new SyntaxTypes.Expression.LambdaExpression(
                            new SyntaxTypes.LambdaStruct([param], inlinedBody)),
                        newDecls.ToImmutable());
                }
                else
                {
                    // Has application argument: composeR f g x [extra...]  →  g (f x) [extra...]
                    var innerApp =
                        new Node<SyntaxTypes.Expression>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.Application([inner, app.Arguments[3]]));

                    var parenInnerApp =
                        new Node<SyntaxTypes.Expression>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.ParenthesizedExpression(innerApp));

                    var desugaredArgs = new List<Node<SyntaxTypes.Expression>> { outer, parenInnerApp };
                    desugaredArgs.AddRange(app.Arguments.Skip(4));

                    var (result, decls) =
                        InlineApplication(new SyntaxTypes.Expression.Application([.. desugaredArgs]), context);

                    newDecls.AddRange(decls);

                    return
                        (ParenthesizeApplicationArguments(result),
                        newDecls.ToImmutable());
                }
            }
        }

        // Beta-reduce lambda applications: (\x -> body) arg  →  body[x := arg]
        {
            var unwrapped = UnwrapParenthesized(funcExpr);

            if (unwrapped is SyntaxTypes.Expression.LambdaExpression lambda)
            {
                var (result, decls) = BetaReduceLambda(lambda.Lambda, [.. app.Arguments.Skip(1)], context);
                newDecls.AddRange(decls);
                return (result, newDecls.ToImmutable());
            }
        }

        // Reduce record-access-function applications when the record argument is known.
        // This turns shapes like `.transform ops first` into `increment first` once `ops`
        // resolves to a concrete record value, which directly reduces remaining
        // higher-order applications in specialized parser-like loops.
        {
            var unwrapped = UnwrapParenthesized(funcExpr);

            if (unwrapped is SyntaxTypes.Expression.RecordAccessFunction recordAccessFunction &&
                app.Arguments.Count >= 2)
            {
                var (resolvedRecord, resolvedDecls) =
                    TryResolveToRecordValue(app.Arguments[1], context);

                newDecls.AddRange(resolvedDecls);

                if (resolvedRecord?.Value is SyntaxTypes.Expression.RecordExpr recordExpr)
                {
                    var fieldName = recordAccessFunction.FunctionName.TrimStart('.');

                    foreach (var field in recordExpr.Fields)
                    {
                        if (field.Value.fieldName.Value != fieldName)
                            continue;

                        if (app.Arguments.Count == 2)
                        {
                            var (fieldExpr, fieldDecls) =
                                InlineExpression(field.Value.valueExpr, context);

                            newDecls.AddRange(fieldDecls);

                            return (fieldExpr.Value, newDecls.ToImmutable());
                        }

                        var reducedArgs =
                            new List<Node<SyntaxTypes.Expression>>
                            {
                                field.Value.valueExpr
                            };

                        reducedArgs.AddRange(app.Arguments.Skip(2));

                        var (reducedResult, reducedDecls) =
                            InlineApplication(
                                new SyntaxTypes.Expression.Application([.. reducedArgs]),
                                context);

                        newDecls.AddRange(reducedDecls);

                        return (reducedResult, newDecls.ToImmutable());
                    }
                }
            }
        }

        // Check if this is a call to a known function
        if (funcExpr is SyntaxTypes.Expression.FunctionOrValue funcOrValue)
        {
            var qualifiedName = (funcOrValue.ModuleName, funcOrValue.Name);

            if (context.FunctionsByQualifiedName.TryGetValue(qualifiedName, out var funcInfo))
            {
                var funcImpl = funcInfo.Function.Declaration.Value;
                var funcParams = funcImpl.Arguments;
                var appArgs = app.Arguments.Skip(1).ToList();

                var (singleChoiceTagSpecialized, singleChoiceDecls) =
                    TrySpecializeSingleChoiceTagCall(
                        funcInfo,
                        funcImpl,
                        appArgs,
                        context);

                newDecls.AddRange(singleChoiceDecls);

                if (singleChoiceTagSpecialized is not null)
                {
                    return (singleChoiceTagSpecialized, newDecls.ToImmutable());
                }

                if (funcInfo.IsRecursive)
                {
                    // Try to specialize the recursive function by substituting
                    // loop-invariant function arguments with concrete values.
                    if (ShouldInline(funcParams, funcImpl.Expression, appArgs, context))
                    {
                        var (specialized, specDecls) =
                            TrySpecializeRecursiveCall(
                                funcInfo,
                                funcImpl,
                                appArgs,
                                context);

                        newDecls.AddRange(specDecls);

                        if (specialized is not null)
                        {
                            return (specialized, newDecls.ToImmutable());
                        }
                    }

                    return
                        (new SyntaxTypes.Expression.Application(
                            [.. app.Arguments.Select(Inline)]),
                        newDecls.ToImmutable());
                }

                // Skip if we're already in the process of inlining this function (prevents infinite recursion)
                if (context.InliningStack.Contains(qualifiedName))
                {
                    return
                        (new SyntaxTypes.Expression.Application(
                            [.. app.Arguments.Select(Inline)]),
                        newDecls.ToImmutable());
                }

                // Check if we should inline based on config
                if (ShouldInline(funcParams, funcImpl.Expression, appArgs, context))
                {
                    // Add this function to the inlining stack to prevent infinite recursion
                    var newContext = context with { InliningStack = context.InliningStack.Add(qualifiedName) };

                    // Inline the function: substitute parameters with arguments
                    var (inlinedResult, inlinedDecls) =
                        InlineFunctionCall(funcInfo.ModuleName, funcImpl, appArgs, newContext);

                    newDecls.AddRange(inlinedDecls);

                    return (inlinedResult, newDecls.ToImmutable());
                }
            }
        }

        // Default: recursively inline arguments
        return
            (new SyntaxTypes.Expression.Application(
                [.. app.Arguments.Select(Inline)]),
            newDecls.ToImmutable());
    }

    /// <summary>
    /// Wraps Application arguments (except the function position) in ParenthesizedExpression
    /// when they are themselves Application expressions with multiple arguments.
    /// This ensures correct rendering after pipe operator desugaring and specialization.
    /// </summary>
    private static SyntaxTypes.Expression ParenthesizeApplicationArguments(SyntaxTypes.Expression expr)
    {
        if (expr is not SyntaxTypes.Expression.Application app || app.Arguments.Count < 2)
        {
            return expr;
        }

        var newArgs = new List<Node<SyntaxTypes.Expression>>(app.Arguments.Count) { app.Arguments[0] };

        for (var i = 1; i < app.Arguments.Count; i++)
        {
            var arg = app.Arguments[i];

            if (NeedsParenthesesInApplicationArgument(arg.Value))
            {
                newArgs.Add(
                    new Node<SyntaxTypes.Expression>(
                        arg.Range,
                        new SyntaxTypes.Expression.ParenthesizedExpression(arg)));
            }
            else
            {
                newArgs.Add(arg);
            }
        }

        return new SyntaxTypes.Expression.Application([.. newArgs]);
    }

    private static bool NeedsParenthesesInApplicationArgument(SyntaxTypes.Expression argument) =>
        argument switch
        {
            SyntaxTypes.Expression.Application innerApp => innerApp.Arguments.Count > 1,
            SyntaxTypes.Expression.LetExpression => true,
            SyntaxTypes.Expression.CaseExpression => true,
            SyntaxTypes.Expression.LambdaExpression => true,
            SyntaxTypes.Expression.IfBlock => true,
            SyntaxTypes.Expression.OperatorApplication => true,
            SyntaxTypes.Expression.Negation => true,

            _ =>
            false
        };

    /// <summary>
    /// Recursively walks all expressions in a declaration and ensures that Application arguments
    /// that are themselves Applications are wrapped in ParenthesizedExpression.
    /// This is applied as a post-processing step after inlining to ensure all generated
    /// expressions have correct parenthesization for rendering.
    /// </summary>
    internal static Node<SyntaxTypes.Expression> ParenthesizeApplicationArgumentsRecursive(
        Node<SyntaxTypes.Expression> exprNode)
    {
        var expr = exprNode.Value;

        static Node<SyntaxTypes.Expression> Recurse(Node<SyntaxTypes.Expression> e) =>
            ParenthesizeApplicationArgumentsRecursive(e);

        var result =
            expr switch
            {
                SyntaxTypes.Expression.Application app when app.Arguments.Count >= 2 =>
                ParenthesizeApplicationArguments(
                    new SyntaxTypes.Expression.Application(
                        [.. app.Arguments.Select(Recurse)])),

                SyntaxTypes.Expression.Application app =>
                new SyntaxTypes.Expression.Application(
                    [.. app.Arguments.Select(Recurse)]),

                SyntaxTypes.Expression.ParenthesizedExpression paren =>
                new SyntaxTypes.Expression.ParenthesizedExpression(
                    Recurse(paren.Expression)),

                SyntaxTypes.Expression.IfBlock ifBlock =>
                new SyntaxTypes.Expression.IfBlock(
                    Recurse(ifBlock.Condition),
                    Recurse(ifBlock.ThenBlock),
                    Recurse(ifBlock.ElseBlock)),

                SyntaxTypes.Expression.CaseExpression caseExpr =>
                new SyntaxTypes.Expression.CaseExpression(
                    new SyntaxTypes.CaseBlock(
                        Recurse(caseExpr.CaseBlock.Expression),
                        [
                        .. caseExpr.CaseBlock.Cases.Select(
                            c =>
                            new SyntaxTypes.Case(c.Pattern, Recurse(c.Expression)))
                        ])),

                SyntaxTypes.Expression.LetExpression letExpr =>
                new SyntaxTypes.Expression.LetExpression(
                    ParenthesizeLetBlock(letExpr.Value)),

                SyntaxTypes.Expression.LambdaExpression lambda =>
                new SyntaxTypes.Expression.LambdaExpression(
                    new SyntaxTypes.LambdaStruct(
                        lambda.Lambda.Arguments,
                        Recurse(lambda.Lambda.Expression))),

                SyntaxTypes.Expression.ListExpr listExpr =>
                new SyntaxTypes.Expression.ListExpr(
                    [.. listExpr.Elements.Select(Recurse)]),

                SyntaxTypes.Expression.TupledExpression tupled =>
                new SyntaxTypes.Expression.TupledExpression(
                    [.. tupled.Elements.Select(Recurse)]),

                SyntaxTypes.Expression.RecordExpr recordExpr =>
                new SyntaxTypes.Expression.RecordExpr(
                    [
                    .. recordExpr.Fields.Select(
                        f =>
                        new Node<(Node<string>, Node<SyntaxTypes.Expression>)>(
                            f.Range,
                            (f.Value.fieldName, Recurse(f.Value.valueExpr))))
                    ]),

                SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [
                    .. recordUpdate.Fields.Select(
                        f =>
                        new Node<(Node<string>, Node<SyntaxTypes.Expression>)>(
                            f.Range,
                            (f.Value.fieldName, Recurse(f.Value.valueExpr))))
                    ]),

                SyntaxTypes.Expression.RecordAccess recordAccess =>
                new SyntaxTypes.Expression.RecordAccess(
                    Recurse(recordAccess.Record),
                    recordAccess.FieldName),

                SyntaxTypes.Expression.Negation negation =>
                new SyntaxTypes.Expression.Negation(
                    Recurse(negation.Expression)),

                SyntaxTypes.Expression.OperatorApplication opApp =>
                new SyntaxTypes.Expression.OperatorApplication(
                    opApp.Operator,
                    opApp.Direction,
                    Recurse(opApp.Left),
                    Recurse(opApp.Right)),

                _ =>
                expr
            };

        return new Node<SyntaxTypes.Expression>(exprNode.Range, result);
    }

    private static SyntaxTypes.Expression.LetBlock ParenthesizeLetBlock(
        SyntaxTypes.Expression.LetBlock letBlock)
    {
        return
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                .. letBlock.Declarations.Select(
                    d =>
                    {
                        var rewrittenDecl =
                            d.Value switch
                            {
                                SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                                new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                                    letFunc.Function with
                                    {
                                        Declaration =
                                        new Node<SyntaxTypes.FunctionImplementation>(
                                            letFunc.Function.Declaration.Range,
                                            letFunc.Function.Declaration.Value with
                                            {
                                                Expression =
                                                ParenthesizeApplicationArgumentsRecursive(
                                                    letFunc.Function.Declaration.Value.Expression)
                                            })
                                    }),

                                SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                                new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                                    letDestr.Pattern,
                                    ParenthesizeApplicationArgumentsRecursive(letDestr.Expression)),

                                _ =>
                                d.Value
                            };

                        return new Node<SyntaxTypes.Expression.LetDeclaration>(d.Range, rewrittenDecl);
                    })
                ],
                Expression:
                ParenthesizeApplicationArgumentsRecursive(letBlock.Expression));
    }

    /// <summary>
    /// Unwraps nested ParenthesizedExpression wrappers to get the inner expression.
    /// </summary>
    private static SyntaxTypes.Expression UnwrapParenthesized(SyntaxTypes.Expression expr)
    {
        while (expr is SyntaxTypes.Expression.ParenthesizedExpression paren)
        {
            expr = paren.Expression.Value;
        }

        return expr;
    }

    /// <summary>
    /// Beta-reduces a lambda application: <c>(\x -> body) arg</c> becomes <c>body[x := arg]</c>.
    /// Handles exact, partial, and over-application cases.
    /// </summary>
    private static (SyntaxTypes.Expression, ImmutableList<Node<SyntaxTypes.Declaration>>) BetaReduceLambda(
        SyntaxTypes.LambdaStruct lambda,
        IReadOnlyList<Node<SyntaxTypes.Expression>> args,
        InliningContext context)
    {
        var newDecls = ImmutableList.CreateBuilder<Node<SyntaxTypes.Declaration>>();

        Node<SyntaxTypes.Expression> Inline(Node<SyntaxTypes.Expression> e)
        {
            var (result, decls) = InlineExpression(e, context);
            newDecls.AddRange(decls);
            return result;
        }

        var substitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();
        var consumedArgs = Math.Min(lambda.Arguments.Count, args.Count);

        for (var i = 0; i < consumedArgs; i++)
        {
            if (lambda.Arguments[i].Value is SyntaxTypes.Pattern.VarPattern varPat)
            {
                substitutions[varPat.Name] = Inline(args[i]);
            }
        }

        var body = SubstituteInExpression(lambda.Expression, substitutions);
        body = Inline(body);

        if (args.Count < lambda.Arguments.Count)
        {
            // Partial application: return remaining lambda
            var remainingParams = lambda.Arguments.Skip(args.Count).ToImmutableArray();

            return
                (new SyntaxTypes.Expression.LambdaExpression(
                    new SyntaxTypes.LambdaStruct(remainingParams, body)),
                newDecls.ToImmutable());
        }

        if (args.Count > lambda.Arguments.Count)
        {
            // Over-application: apply remaining args to the result
            var remainingArgs =
                args.Skip(lambda.Arguments.Count)
                .Select(Inline)
                .ToList();

            var allArgs = new List<Node<SyntaxTypes.Expression>> { body };
            allArgs.AddRange(remainingArgs);

            var (appResult, appDecls) =
                InlineApplication(
                    new SyntaxTypes.Expression.Application([.. allArgs]),
                    context);

            newDecls.AddRange(appDecls);

            return (appResult, newDecls.ToImmutable());
        }

        // Exact application
        return (body.Value, newDecls.ToImmutable());
    }

    private static bool ShouldInline(
        IReadOnlyList<Node<SyntaxTypes.Pattern>> funcParams,
        Node<SyntaxTypes.Expression> funcBody,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        InliningContext context)
    {
        if (appArgs.Count < funcParams.Count)
        {
            return false;
        }

        if (context.Config is not Config.InlineOnlyFunctions)
        {
            return false;
        }

        // Check if any argument is a function (lambda expression) or if the parameter
        // uses constructor pattern matching (indicating it expects a wrapped function type)
        var argCount = Math.Min(funcParams.Count, appArgs.Count);

        for (var i = 0; i < argCount; i++)
        {
            var param = funcParams[i].Value;
            var arg = appArgs[i].Value;

            // An argument is considered a "function" if it's a lambda or a known function reference
            if (IsFunctionExpression(arg, context))
            {
                return true;
            }

            // If the parameter uses a constructor pattern (like `(Parser f)`), only inline
            // when the supplied argument itself looks like a function-bearing computation.
            // This keeps Config.OnlyFunctions focused on higher-order use cases and avoids
            // inlining data-oriented constructor-pattern calls such as String.split before.
            if (IsConstructorPattern(param) && IsFunctionBearingExpression(arg, context))
            {
                return true;
            }

            if (param is SyntaxTypes.Pattern.VarPattern varPattern &&
                IsFunctionBearingExpression(arg, context) &&
                BodyUnwrapsParameterAsConstructor(funcBody, varPattern.Name))
            {
                return true;
            }
        }

        return false;
    }

    private static bool BodyUnwrapsParameterAsConstructor(
        Node<SyntaxTypes.Expression> exprNode,
        string parameterName)
    {
        bool Recurse(Node<SyntaxTypes.Expression> expr) =>
            BodyUnwrapsParameterAsConstructor(expr, parameterName);

        return exprNode.Value switch
        {
            SyntaxTypes.Expression.LetExpression letExpr =>
            letExpr.Value.Declarations.Any(
                declaration =>
                declaration.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr &&
                IsLocalVariableReference(letDestr.Expression.Value, parameterName) &&
                IsConstructorPattern(letDestr.Pattern.Value)) ||
            letExpr.Value.Declarations.Any(
                declaration =>
                declaration.Value switch
                {
                    SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                    Recurse(letFunc.Function.Declaration.Value.Expression),

                    SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                    Recurse(letDestr.Expression),

                    _ =>
                    false
                }) ||
            Recurse(letExpr.Value.Expression),

            SyntaxTypes.Expression.CaseExpression caseExpr =>
            (IsLocalVariableReference(caseExpr.CaseBlock.Expression.Value, parameterName) &&
            caseExpr.CaseBlock.Cases.Any(c => IsConstructorPattern(c.Pattern.Value))) ||
            Recurse(caseExpr.CaseBlock.Expression) ||
            caseExpr.CaseBlock.Cases.Any(c => Recurse(c.Expression)),

            SyntaxTypes.Expression.Application app =>
            app.Arguments.Any(Recurse),

            SyntaxTypes.Expression.ParenthesizedExpression paren =>
            Recurse(paren.Expression),

            SyntaxTypes.Expression.IfBlock ifBlock =>
            Recurse(ifBlock.Condition) ||
            Recurse(ifBlock.ThenBlock) ||
            Recurse(ifBlock.ElseBlock),

            SyntaxTypes.Expression.LambdaExpression lambda =>
            Recurse(lambda.Lambda.Expression),

            SyntaxTypes.Expression.ListExpr listExpr =>
            listExpr.Elements.Any(Recurse),

            SyntaxTypes.Expression.TupledExpression tupled =>
            tupled.Elements.Any(Recurse),

            SyntaxTypes.Expression.RecordExpr recordExpr =>
            recordExpr.Fields.Any(field => Recurse(field.Value.valueExpr)),

            SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
            recordUpdate.Fields.Any(field => Recurse(field.Value.valueExpr)),

            SyntaxTypes.Expression.RecordAccess recordAccess =>
            Recurse(recordAccess.Record),

            SyntaxTypes.Expression.Negation negation =>
            Recurse(negation.Expression),

            SyntaxTypes.Expression.OperatorApplication opApp =>
            Recurse(opApp.Left) || Recurse(opApp.Right),

            _ =>
            false
        };
    }

    private static bool IsConstructorPattern(SyntaxTypes.Pattern pattern)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.NamedPattern => true,

            // Alias patterns like `((Parser parse) as element)` still represent a constructor
            // pattern for specialization purposes, so unwrap the alias and inspect the inner pattern.
            SyntaxTypes.Pattern.AsPattern asPattern => IsConstructorPattern(asPattern.Pattern.Value),
            SyntaxTypes.Pattern.ParenthesizedPattern paren => IsConstructorPattern(paren.Pattern.Value),

            _ =>
            false
        };
    }

    private static bool IsFunctionExpression(SyntaxTypes.Expression expr, InliningContext context)
    {
        return expr switch
        {
            SyntaxTypes.Expression.LambdaExpression => true,

            SyntaxTypes.Expression.FunctionOrValue funcOrValue =>
            IsFunctionReference(funcOrValue, context),

            // Record accessors such as `.extensionRight` are functions in Elm and appear directly
            // on the parser hot path as higher-order arguments.
            SyntaxTypes.Expression.RecordAccessFunction =>
            true,

            SyntaxTypes.Expression.ParenthesizedExpression paren =>
            IsFunctionExpression(paren.Expression.Value, context),

            _ =>
            false
        };
    }

    /// <summary>
    /// Determines if a <see cref="SyntaxTypes.Expression.FunctionOrValue"/> reference is a function,
    /// using both the existing syntax-based check (qualified reference in known functions dictionary)
    /// and a type-based check via <see cref="TypeInference.BuildFunctionSignaturesMap"/> signatures.
    /// The type-based check handles cases the syntax-based check misses, such as functions
    /// passed through variables or partial applications whose types are known from annotations.
    /// </summary>
    private static bool IsFunctionReference(
        SyntaxTypes.Expression.FunctionOrValue funcOrValue,
        InliningContext context)
    {
        // Syntax-based check: qualified reference to a known function definition.
        // This keeps Config.OnlyFunctions focused on stable top-level / cross-module references
        // and avoids inlining based on local let-bound helpers, which can introduce lifted
        // dependencies the current compiler pipeline does not yet propagate robustly.
        if (funcOrValue.ModuleName.Count > 0 &&
            context.FunctionsByQualifiedName.ContainsKey((funcOrValue.ModuleName, funcOrValue.Name)))
        {
            return true;
        }

        if (funcOrValue.ModuleName.Count is 0 &&
            context.CurrentModuleName is { } currentModuleName &&
            context.FunctionsByQualifiedName.ContainsKey((currentModuleName, funcOrValue.Name)))
        {
            return true;
        }

        // Type-based check: look up the qualified name in function signatures.
        // If the type annotation says this is a FunctionType, it's a function.
        if (funcOrValue.ModuleName.Count > 0)
        {
            var qualifiedName = string.Join(".", funcOrValue.ModuleName) + "." + funcOrValue.Name;

            if (context.FunctionSignatures.TryGetValue(qualifiedName, out var inferredType) &&
                inferredType is TypeInference.InferredType.FunctionType)
            {
                return true;
            }
        }

        if (funcOrValue.ModuleName.Count is 0 &&
            context.CurrentModuleName is { } currentModuleNameForSignature)
        {
            var qualifiedName = string.Join(".", currentModuleNameForSignature) + "." + funcOrValue.Name;

            if (context.FunctionSignatures.TryGetValue(qualifiedName, out var inferredType) &&
                inferredType is TypeInference.InferredType.FunctionType)
            {
                return true;
            }
        }

        return false;
    }

    private static bool IsFunctionBearingExpression(SyntaxTypes.Expression expr, InliningContext context)
    {
        return expr switch
        {
            SyntaxTypes.Expression.Application => true,

            SyntaxTypes.Expression.ParenthesizedExpression paren =>
            IsFunctionBearingExpression(paren.Expression.Value, context),

            _ =>
            IsFunctionExpression(expr, context)
        };
    }

    private static (SyntaxTypes.Expression, ImmutableList<Node<SyntaxTypes.Declaration>>) InlineFunctionCall(
        ModuleName calleeModuleName,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> args,
        InliningContext context)
    {
        var newDecls = ImmutableList.CreateBuilder<Node<SyntaxTypes.Declaration>>();

        Node<SyntaxTypes.Expression> Inline(Node<SyntaxTypes.Expression> e)
        {
            var (result, decls) = InlineExpression(e, context);
            newDecls.AddRange(decls);
            return result;
        }

        var funcParams = funcImpl.Arguments;
        var funcBody = funcImpl.Expression;

        // First, recursively inline the arguments
        var inlinedArgs = args.Select(Inline).ToList();

        // Check if any parameter uses constructor pattern matching - if so, we need let bindings
        var letDeclarations = new List<Node<SyntaxTypes.Expression.LetDeclaration>>();

        var substitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();

        var count = Math.Min(funcParams.Count, inlinedArgs.Count);

        for (var i = 0; i < count; i++)
        {
            var param = funcParams[i];
            var arg = inlinedArgs[i];

            if (param.Value is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                // Simple variable pattern - direct substitution
                substitutions[varPattern.Name] = arg;
            }
            else if (UnwrapParenthesizedPattern(param.Value) is SyntaxTypes.Pattern.AllPattern)
            {
                // Wildcard patterns do not bind anything, so there is no need to retain a
                // generated let-destructuring binding for them in the inlined body.
            }
            else if (TryDeconstructKnownConstructorApplication(
                arg,
                context,
                out var constructorName,
                out var fieldExpressions) &&
                TryBindSingleChoiceTagPattern(
                    param.Value,
                    constructorName,
                    fieldExpressions,
                    out var patternBindings))
            {
                foreach (var binding in patternBindings)
                {
                    substitutions[binding.Key] = binding.Value;
                }

                if (TryGetAliasNameFromPattern(param.Value) is { } aliasName)
                {
                    substitutions[aliasName] =
                        BuildConstructorApplication(
                            constructorName,
                            fieldExpressions);
                }
            }
            else
            {
                // Any non-variable pattern needs a let destructuring binding so nested names
                // introduced by the pattern remain available in the inlined body.
                var letDestr =
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Pattern: param,
                        Expression: arg);

                letDeclarations.Add(new Node<SyntaxTypes.Expression.LetDeclaration>(s_zeroRange, letDestr));
            }
        }

        // Substitute in the function body
        var substitutedBody = SubstituteInExpression(funcBody, substitutions);

        var qualifiedLiftedHelperReferences =
            QualifyLiftedHelperReferencesFromCalleeModule(
                substitutedBody,
                calleeModuleName,
                context);

        // Recursively inline in the substituted body
        var inlinedBody = Inline(qualifiedLiftedHelperReferences);

        // If we have let declarations, wrap the body in a let expression
        SyntaxTypes.Expression resultExpr;

        if (letDeclarations.Count > 0)
        {
            var letBlock =
                new SyntaxTypes.Expression.LetBlock(
                    Declarations: [.. letDeclarations],
                    Expression: inlinedBody);

            resultExpr = new SyntaxTypes.Expression.LetExpression(letBlock);
        }
        else
        {
            resultExpr = inlinedBody.Value;
        }

        if (letDeclarations.Count > 0)
        {
            var (normalizedResult, normalizedDecls) =
                InlineExpression(
                    new Node<SyntaxTypes.Expression>(s_zeroRange, resultExpr),
                    context);

            newDecls.AddRange(normalizedDecls);
            resultExpr = normalizedResult.Value;
        }

        // If we have more arguments than parameters, we need to create an application
        if (args.Count > funcParams.Count)
        {
            var remainingArgs =
                args.Skip(funcParams.Count).Select(Inline)
                .ToList();

            var allArgs =
                new List<Node<SyntaxTypes.Expression>>
                {
                    new(s_zeroRange, resultExpr)
                };

            allArgs.AddRange(remainingArgs);

            return (new SyntaxTypes.Expression.Application([.. allArgs]), newDecls.ToImmutable());
        }

        // If we have fewer arguments than parameters, we create a partial application (lambda)
        if (args.Count < funcParams.Count)
        {
            var remainingParams = funcParams.Skip(args.Count).ToList();

            return
                (new SyntaxTypes.Expression.LambdaExpression(
                    new SyntaxTypes.LambdaStruct(
                        Arguments: remainingParams,
                        Expression: new Node<SyntaxTypes.Expression>(s_zeroRange, resultExpr))),
                newDecls.ToImmutable());
        }

        return (resultExpr, newDecls.ToImmutable());
    }

    private sealed record SingleChoiceTagFieldPlan(
        int FieldIndex,
        string? BoundVariableName,
        InliningFunctionSpecialization.ParameterSpecialization? ParameterSpecialization,
        IReadOnlyList<Node<SyntaxTypes.Pattern>> FlattenedParameters,
        IReadOnlyList<Node<SyntaxTypes.Expression>> FlattenedActualArguments,
        Node<SyntaxTypes.Expression> ReplacementExpression,
        IReadOnlyList<SingleChoiceTagFieldPlan> NestedFieldPlans);

    private sealed record SingleChoiceTagSpecialization(
        int ParamIndex,
        SyntaxTypes.QualifiedNameRef ConstructorName,
        IReadOnlyList<SingleChoiceTagFieldPlan> FieldPlans,
        Node<SyntaxTypes.Expression> ReplacementArgument,
        string ParamName);

    private static ImmutableArray<Node<SyntaxTypes.Pattern>> GetFlattenedFieldParameters(
        SingleChoiceTagSpecialization specialization) =>
        [.. specialization.FieldPlans.SelectMany(fieldPlan => fieldPlan.FlattenedParameters)];

    private static ImmutableArray<Node<SyntaxTypes.Expression>> GetFlattenedActualArguments(
        SingleChoiceTagSpecialization specialization) =>
        [.. specialization.FieldPlans.SelectMany(fieldPlan => fieldPlan.FlattenedActualArguments)];

    private static SingleChoiceTagFieldPlan BuildFieldPlanForVarPatternField(
        int fieldIndex,
        string fieldName,
        Node<SyntaxTypes.Expression> actualFieldExpression,
        ModuleName currentModuleName,
        InliningContext context)
    {
        if (IsFunctionExpression(actualFieldExpression.Value, context))
        {
            return
                new SingleChoiceTagFieldPlan(
                    FieldIndex: fieldIndex,
                    BoundVariableName: fieldName,
                    ParameterSpecialization:
                    InliningFunctionSpecialization.ClassifyArgument(actualFieldExpression.Value),
                    FlattenedParameters: [],
                    FlattenedActualArguments: [],
                    ReplacementExpression: actualFieldExpression,
                    NestedFieldPlans: []);
        }

        if (TryBuildNestedSingleChoiceTagFieldPlan(
            fieldIndex,
            fieldName,
            actualFieldExpression,
            currentModuleName,
            context,
            out var nestedFieldPlan))
        {
            return nestedFieldPlan;
        }

        var replacementParameter =
            new Node<SyntaxTypes.Pattern>(
                s_zeroRange,
                new SyntaxTypes.Pattern.VarPattern(fieldName));

        var replacementExpression =
            new Node<SyntaxTypes.Expression>(
                s_zeroRange,
                new SyntaxTypes.Expression.FunctionOrValue([], fieldName));

        return
            new SingleChoiceTagFieldPlan(
                FieldIndex: fieldIndex,
                BoundVariableName: fieldName,
                ParameterSpecialization: null,
                FlattenedParameters: [replacementParameter],
                FlattenedActualArguments: [actualFieldExpression],
                ReplacementExpression: replacementExpression,
                NestedFieldPlans: []);
    }

    private static bool TryBuildNestedSingleChoiceTagFieldPlan(
        int fieldIndex,
        string fieldName,
        Node<SyntaxTypes.Expression> actualFieldExpression,
        ModuleName currentModuleName,
        InliningContext context,
        out SingleChoiceTagFieldPlan fieldPlan)
    {
        fieldPlan = null!;

        if (!TryDeconstructKnownConstructorApplicationForSpecialization(
                actualFieldExpression,
                context,
                out var constructorName,
                out var actualFieldExpressions))
        {
            return false;
        }

        if (constructorName.ModuleName.Count is 0)
        {
            constructorName = new SyntaxTypes.QualifiedNameRef(currentModuleName, constructorName.Name);
        }

        if (!context.SingleChoiceConstructors.TryGetValue(constructorName, out var constructorInfo) ||
            constructorInfo.FieldCount != actualFieldExpressions.Count)
        {
            return false;
        }

        var nestedFieldPlans = new List<SingleChoiceTagFieldPlan>();

        for (var nestedFieldIndex = 0; nestedFieldIndex < actualFieldExpressions.Count; nestedFieldIndex++)
        {
            nestedFieldPlans.Add(
                BuildFieldPlanForVarPatternField(
                    nestedFieldIndex,
                    fieldName + "__field__" + (nestedFieldIndex + 1),
                    actualFieldExpressions[nestedFieldIndex],
                    currentModuleName,
                    context));
        }

        var nestedSpecFields =
            ImmutableDictionary.CreateBuilder<int, InliningFunctionSpecialization.ParameterSpecialization>();

        foreach (var nestedFieldPlan in nestedFieldPlans)
        {
            if (nestedFieldPlan.ParameterSpecialization is { } nestedSpecialization)
            {
                nestedSpecFields[nestedFieldPlan.FieldIndex] = nestedSpecialization;
            }
        }

        fieldPlan =
            new SingleChoiceTagFieldPlan(
                FieldIndex: fieldIndex,
                BoundVariableName: fieldName,
                ParameterSpecialization:
                new InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap(
                    constructorName,
                    nestedSpecFields.ToImmutable()),
                FlattenedParameters: [.. nestedFieldPlans.SelectMany(plan => plan.FlattenedParameters)],
                FlattenedActualArguments: [.. nestedFieldPlans.SelectMany(plan => plan.FlattenedActualArguments)],
                ReplacementExpression:
                BuildConstructorApplication(
                    constructorName,
                    [.. nestedFieldPlans.Select(plan => plan.ReplacementExpression)]),
                NestedFieldPlans: nestedFieldPlans);

        return true;
    }

    private static (SyntaxTypes.Expression?, ImmutableList<Node<SyntaxTypes.Declaration>>) TrySpecializeSingleChoiceTagCall(
        FunctionInfo funcInfo,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        InliningContext context)
    {
        if (appArgs.Count < funcImpl.Arguments.Count)
            return (null, []);

        var specialization =
            TryBuildSingleChoiceTagSpecialization(
                funcImpl,
                appArgs,
                funcInfo,
                context);

        if (specialization is null)
            return (null, []);

        // Look up the pre-assigned name from the specialization catalog
        var specializedName =
            LookupSpecializedName(
                funcImpl.Name.Value,
                funcInfo.ModuleName,
                specialization.ParamIndex,
                BuildSingleChoiceTagUnwrapSpec(specialization, context),
                context);

        if (specializedName is null)
            return (null, []);

        var rewrittenBody =
            funcInfo.IsRecursive
            ?
            RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                funcImpl.Expression,
                funcImpl.Name.Value,
                funcInfo.ModuleName,
                specializedName,
                specialization)
            :
            funcImpl.Expression;

        var substitutedBody =
            SubstituteInExpression(
                rewrittenBody,
                new Dictionary<string, Node<SyntaxTypes.Expression>>
                {
                    [specialization.ParamName] = specialization.ReplacementArgument
                });

        var directPatternFieldSubstitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();

        CollectDirectFieldSubstitutions(
            specialization.FieldPlans,
            directPatternFieldSubstitutions);

        if (directPatternFieldSubstitutions.Count > 0)
        {
            substitutedBody =
                SubstituteInExpression(
                    substitutedBody,
                    directPatternFieldSubstitutions);
        }

        var qualifiedBody =
            QualifyLiftedHelperReferencesFromCalleeModule(
                substitutedBody,
                funcInfo.ModuleName,
                context);

        var specializedParams =
            BuildSingleChoiceTagSpecializedParameters(
                funcImpl.Arguments,
                specialization);

        var specializedImpl =
            new SyntaxTypes.FunctionImplementation(
                Name: new Node<string>(s_zeroRange, specializedName),
                Arguments: specializedParams,
                Expression: qualifiedBody);

        var specializedFunc =
            new SyntaxTypes.FunctionStruct(
                Documentation: null,
                Signature: null,
                Declaration: new Node<SyntaxTypes.FunctionImplementation>(s_zeroRange, specializedImpl));

        var newDecl =
            new Node<SyntaxTypes.Declaration>(
                s_zeroRange,
                new SyntaxTypes.Declaration.FunctionDeclaration(specializedFunc));

        var callArgs =
            BuildSingleChoiceTagSpecializedCallArguments(
                specializedName,
                appArgs,
                funcImpl.Arguments.Count,
                specialization);

        SyntaxTypes.Expression callExpr =
            callArgs.Count is 1
            ?
            callArgs[0].Value
            :
            new SyntaxTypes.Expression.Application([.. callArgs]);

        return (callExpr, ImmutableList.Create(newDecl));
    }

    private static void CollectDirectFieldSubstitutions(
        IReadOnlyList<SingleChoiceTagFieldPlan> fieldPlans,
        Dictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        foreach (var fieldPlan in fieldPlans)
        {
            if (fieldPlan.BoundVariableName is { } boundVariableName &&
                fieldPlan.ParameterSpecialization is not null)
            {
                substitutions[boundVariableName] = fieldPlan.ReplacementExpression;
            }

            if (fieldPlan.NestedFieldPlans.Count > 0)
            {
                CollectDirectFieldSubstitutions(
                    fieldPlan.NestedFieldPlans,
                    substitutions);
            }
        }
    }

    private static SingleChoiceTagSpecialization? TryBuildSingleChoiceTagSpecialization(
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        FunctionInfo funcInfo,
        InliningContext context)
    {
        for (var paramIndex = 0; paramIndex < funcImpl.Arguments.Count && paramIndex < appArgs.Count; paramIndex++)
        {
            if (funcImpl.Arguments[paramIndex].Value is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                // Case 1: VarPattern parameter with case-of unwrap in the body
                // e.g., alfa factor myValue = case myValue of MyConstructor x -> ...

                if (!TryDeconstructKnownConstructorApplicationForSpecialization(
                        appArgs[paramIndex],
                        context,
                        out var constructorName,
                        out var actualFieldExpressions))
                    continue;

                if (constructorName.ModuleName.Count is 0)
                {
                    constructorName = new SyntaxTypes.QualifiedNameRef(funcInfo.ModuleName, constructorName.Name);
                }

                // Use type information: verify this constructor belongs to a single-constructor type.
                if (!context.SingleChoiceConstructors.TryGetValue(constructorName, out var constructorInfo) ||
                    constructorInfo.FieldCount != actualFieldExpressions.Count)
                    continue;

                if (funcInfo.IsRecursive &&
                    !IsLoopInvariantInRecursiveCalls(
                        funcImpl.Expression.Value,
                        funcImpl.Name.Value,
                        funcInfo.ModuleName,
                        varPattern.Name,
                        paramIndex))
                    continue;

                var fieldPlans = new List<SingleChoiceTagFieldPlan>();

                for (var fieldIndex = 0; fieldIndex < actualFieldExpressions.Count; fieldIndex++)
                {
                    fieldPlans.Add(
                        BuildFieldPlanForVarPatternField(
                            fieldIndex,
                            varPattern.Name + "__field__" + (fieldIndex + 1),
                            actualFieldExpressions[fieldIndex],
                            funcInfo.ModuleName,
                            context));
                }

                return
                    new SingleChoiceTagSpecialization(
                        ParamIndex: paramIndex,
                        ConstructorName: constructorName,
                        FieldPlans: fieldPlans,
                        ReplacementArgument:
                        BuildConstructorApplication(
                            constructorName,
                            [.. fieldPlans.Select(plan => plan.ReplacementExpression)]),
                        ParamName: varPattern.Name);
            }

            // Case 2: NamedPattern parameter (direct parameter-level destructuring)
            // e.g., alfa factor (MyConstructor x) = ...
            {
                var namedPattern = TryUnwrapToNamedPattern(funcImpl.Arguments[paramIndex].Value);

                if (namedPattern is null)
                    continue;

                if (!TryDeconstructKnownConstructorApplicationForSpecialization(
                        appArgs[paramIndex],
                        context,
                        out var constructorName,
                        out var actualFieldExpressions))
                    continue;

                if (constructorName.ModuleName.Count is 0)
                {
                    constructorName = new SyntaxTypes.QualifiedNameRef(funcInfo.ModuleName, constructorName.Name);
                }

                // Resolve the pattern constructor name to fully qualified form for comparison
                var patternConstructorName = namedPattern.Name;

                if (patternConstructorName.ModuleName.Count is 0)
                {
                    patternConstructorName =
                        new SyntaxTypes.QualifiedNameRef(funcInfo.ModuleName, patternConstructorName.Name);
                }

                // Use direct equality on fully-qualified names (input is canonicalized)
                if (!patternConstructorName.Equals(constructorName))
                    continue;

                // Use type information: verify this constructor belongs to a single-constructor type.
                if (!context.SingleChoiceConstructors.ContainsKey(constructorName))
                    continue;

                if (namedPattern.Arguments.Count != actualFieldExpressions.Count)
                    continue;

                var fieldPlans = new List<SingleChoiceTagFieldPlan>();
                var allFieldsHandled = true;

                for (var fieldIndex = 0; fieldIndex < actualFieldExpressions.Count; fieldIndex++)
                {
                    var fieldPatternUnwrapped = UnwrapParenthesizedPattern(namedPattern.Arguments[fieldIndex].Value);

                    if (fieldPatternUnwrapped is not SyntaxTypes.Pattern.VarPattern fieldVarPattern)
                    {
                        allFieldsHandled = false;
                        break;
                    }

                    if (IsFunctionExpression(actualFieldExpressions[fieldIndex].Value, context))
                    {
                        fieldPlans.Add(
                            new SingleChoiceTagFieldPlan(
                                FieldIndex: fieldIndex,
                                BoundVariableName: fieldVarPattern.Name,
                                ParameterSpecialization:
                                InliningFunctionSpecialization.ClassifyArgument(
                                    actualFieldExpressions[fieldIndex].Value),
                                FlattenedParameters: [],
                                FlattenedActualArguments: [],
                                ReplacementExpression: actualFieldExpressions[fieldIndex],
                                NestedFieldPlans: []));

                        continue;
                    }

                    fieldPlans.Add(
                        new SingleChoiceTagFieldPlan(
                            FieldIndex: fieldIndex,
                            BoundVariableName: fieldVarPattern.Name,
                            ParameterSpecialization: null,
                            FlattenedParameters:
                            [
                            new Node<SyntaxTypes.Pattern>(
                                s_zeroRange,
                                new SyntaxTypes.Pattern.VarPattern(fieldVarPattern.Name))
                            ],
                            FlattenedActualArguments:
                            [
                            actualFieldExpressions[fieldIndex]
                            ],
                            ReplacementExpression:
                            new Node<SyntaxTypes.Expression>(
                                s_zeroRange,
                                new SyntaxTypes.Expression.FunctionOrValue([], fieldVarPattern.Name)),
                            NestedFieldPlans: []));
                }

                if (!allFieldsHandled)
                    continue;

                // Use a synthetic param name that won't match any variable in the body,
                // so the body substitution is a no-op unless the pattern uses an alias.
                // If the pattern is an alias pattern like `((Parser parse) as element)`,
                // preserve the alias name so recursive-call rewriting can still match it.
                var syntheticParamName =
                    TryGetAliasNameFromPattern(funcImpl.Arguments[paramIndex].Value)
                    ?? "__param_" + paramIndex + "_pattern__";

                return
                    new SingleChoiceTagSpecialization(
                        ParamIndex: paramIndex,
                        ConstructorName: constructorName,
                        FieldPlans: fieldPlans,
                        ReplacementArgument:
                        BuildConstructorApplication(
                            constructorName,
                            [.. fieldPlans.Select(plan => plan.ReplacementExpression)]),
                        ParamName: syntheticParamName);
            }
        }

        return null;
    }

    private static SyntaxTypes.Pattern.NamedPattern? TryUnwrapToNamedPattern(SyntaxTypes.Pattern pattern)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.NamedPattern np => np,
            SyntaxTypes.Pattern.AsPattern ap => TryUnwrapToNamedPattern(ap.Pattern.Value),
            SyntaxTypes.Pattern.ParenthesizedPattern pp => TryUnwrapToNamedPattern(pp.Pattern.Value),

            _ =>
            null
        };
    }

    private static string? TryGetAliasNameFromPattern(SyntaxTypes.Pattern pattern)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.AsPattern ap => ap.Name.Value,
            SyntaxTypes.Pattern.ParenthesizedPattern pp => TryGetAliasNameFromPattern(pp.Pattern.Value),

            _ =>
            null
        };
    }

    private static SyntaxTypes.Pattern UnwrapParenthesizedPattern(SyntaxTypes.Pattern pattern)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.ParenthesizedPattern pp => UnwrapParenthesizedPattern(pp.Pattern.Value),

            _ =>
            pattern
        };
    }

    private static ImmutableArray<Node<SyntaxTypes.Pattern>> BuildSingleChoiceTagSpecializedParameters(
        IReadOnlyList<Node<SyntaxTypes.Pattern>> originalParams,
        SingleChoiceTagSpecialization specialization)
    {
        var specializedParams = new List<Node<SyntaxTypes.Pattern>>();
        var flattenedParameters = GetFlattenedFieldParameters(specialization);

        for (var paramIndex = 0; paramIndex < originalParams.Count; paramIndex++)
        {
            if (paramIndex == specialization.ParamIndex)
            {
                if (flattenedParameters.Length > 1)
                {
                    specializedParams.Add(
                        new Node<SyntaxTypes.Pattern>(
                            s_zeroRange,
                            new SyntaxTypes.Pattern.TuplePattern(
                                [.. flattenedParameters])));
                }
                else
                {
                    specializedParams.AddRange(flattenedParameters);
                }

                continue;
            }

            specializedParams.Add(originalParams[paramIndex]);
        }

        return [.. specializedParams];
    }

    private static List<Node<SyntaxTypes.Expression>> BuildSingleChoiceTagSpecializedCallArguments(
        string specializedName,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        int originalParamCount,
        SingleChoiceTagSpecialization specialization)
    {
        var callArgs =
            new List<Node<SyntaxTypes.Expression>>
            {
                new(s_zeroRange, new SyntaxTypes.Expression.FunctionOrValue([], specializedName))
            };

        var flattenedActualArguments = GetFlattenedActualArguments(specialization);

        for (var argIndex = 0; argIndex < appArgs.Count; argIndex++)
        {
            if (argIndex < originalParamCount && argIndex == specialization.ParamIndex)
            {
                if (flattenedActualArguments.Length > 1)
                {
                    callArgs.Add(
                        new Node<SyntaxTypes.Expression>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.TupledExpression([.. flattenedActualArguments])));
                }
                else
                {
                    callArgs.AddRange(flattenedActualArguments);
                }

                continue;
            }

            callArgs.Add(appArgs[argIndex]);
        }

        return callArgs;
    }

    private static bool TryDeconstructConstructorApplication(
        Node<SyntaxTypes.Expression> exprNode,
        out SyntaxTypes.QualifiedNameRef constructorName,
        out IReadOnlyList<Node<SyntaxTypes.Expression>> fieldExpressions)
    {
        return
            TryDeconstructConstructorApplication(
                exprNode.Value,
                out constructorName,
                out fieldExpressions);
    }

    private static bool TryDeconstructConstructorApplication(
        SyntaxTypes.Expression expr,
        out SyntaxTypes.QualifiedNameRef constructorName,
        out IReadOnlyList<Node<SyntaxTypes.Expression>> fieldExpressions)
    {
        switch (UnwrapParenthesized(expr))
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrValue:
                constructorName = new SyntaxTypes.QualifiedNameRef(funcOrValue.ModuleName, funcOrValue.Name);
                fieldExpressions = [];
                return true;

            case SyntaxTypes.Expression.Application app
            when app.Arguments.Count > 0 &&
                     app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue constructorRef:

                constructorName = new SyntaxTypes.QualifiedNameRef(constructorRef.ModuleName, constructorRef.Name);
                fieldExpressions = [.. app.Arguments.Skip(1)];
                return true;

            default:
                constructorName = default!;
                fieldExpressions = [];
                return false;
        }
    }

    private static bool TryDeconstructKnownConstructorApplication(
        Node<SyntaxTypes.Expression> exprNode,
        InliningContext context,
        out SyntaxTypes.QualifiedNameRef constructorName,
        out IReadOnlyList<Node<SyntaxTypes.Expression>> fieldExpressions)
    {
        if (TryDeconstructConstructorApplication(
            exprNode,
            out constructorName,
            out fieldExpressions))
        {
            return true;
        }

        switch (exprNode.Value)
        {
            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                return
                    TryDeconstructKnownConstructorApplication(
                        paren.Expression,
                        context,
                        out constructorName,
                        out fieldExpressions);

            case SyntaxTypes.Expression.LetExpression letExpr:
                {
                    if (AreLetDeclarationsIgnorableForConstructorResolution(letExpr.Value.Declarations))
                    {
                        return
                            TryDeconstructKnownConstructorApplication(
                                letExpr.Value.Expression,
                                context,
                                out constructorName,
                                out fieldExpressions);
                    }

                    var (inlinedLet, letDecls) = InlineLetExpression(letExpr, context);

                    if (letDecls.Count is 0)
                    {
                        if (inlinedLet is SyntaxTypes.Expression.LetExpression inlinedLetExpr &&
                            AreLetDeclarationsIgnorableForConstructorResolution(
                                inlinedLetExpr.Value.Declarations))
                        {
                            return
                                TryDeconstructKnownConstructorApplication(
                                    inlinedLetExpr.Value.Expression,
                                    context,
                                    out constructorName,
                                    out fieldExpressions);
                        }

                        if (!inlinedLet.Equals(exprNode.Value))
                        {
                            return
                                TryDeconstructKnownConstructorApplication(
                                    new Node<SyntaxTypes.Expression>(s_zeroRange, inlinedLet),
                                    context,
                                    out constructorName,
                                    out fieldExpressions);
                        }
                    }

                    break;
                }

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                {
                    var (resolvedRecord, _) =
                        TryResolveToRecordValue(recordAccess.Record, context);

                    if (resolvedRecord?.Value is SyntaxTypes.Expression.RecordExpr recordExpr)
                    {
                        foreach (var field in recordExpr.Fields)
                        {
                            if (field.Value.fieldName.Value != recordAccess.FieldName.Value)
                                continue;

                            return
                                TryDeconstructKnownConstructorApplication(
                                    field.Value.valueExpr,
                                    context,
                                    out constructorName,
                                    out fieldExpressions);
                        }
                    }

                    break;
                }

            case SyntaxTypes.Expression.Application app
            when app.Arguments.Count > 0 &&
                 app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue funcOrValue &&
                 TryResolveKnownFunctionReference(funcOrValue, context, out var qualifiedName, out var funcInfo) &&
                 !funcInfo.IsRecursive &&
                 !context.InliningStack.Contains(qualifiedName):

                {
                    var funcImpl = funcInfo.Function.Declaration.Value;
                    var appArgs = app.Arguments.Skip(1).ToList();

                    if (appArgs.Count == funcImpl.Arguments.Count)
                    {
                        var newContext =
                            context with
                            {
                                InliningStack = context.InliningStack.Add(qualifiedName)
                            };

                        var (inlinedResult, inlinedDecls) =
                            InlineFunctionCall(
                                funcInfo.ModuleName,
                                funcImpl,
                                appArgs,
                                newContext);

                        if (inlinedDecls.Count is 0 &&
                            !inlinedResult.Equals(exprNode.Value))
                        {
                            return
                                TryDeconstructKnownConstructorApplication(
                                    new Node<SyntaxTypes.Expression>(s_zeroRange, inlinedResult),
                                    context,
                                    out constructorName,
                                    out fieldExpressions);
                        }
                    }

                    break;
                }
        }

        constructorName = default!;
        fieldExpressions = [];
        return false;
    }

    private static bool TryDeconstructKnownConstructorApplicationForSpecialization(
        Node<SyntaxTypes.Expression> exprNode,
        InliningContext context,
        out SyntaxTypes.QualifiedNameRef constructorName,
        out IReadOnlyList<Node<SyntaxTypes.Expression>> fieldExpressions)
    {
        if (TryDeconstructKnownConstructorApplication(
            exprNode,
            context,
            out constructorName,
            out fieldExpressions) &&
            fieldExpressions.Count > 0)
        {
            return true;
        }

        if (exprNode.Value is not SyntaxTypes.Expression.FunctionOrValue knownFunctionRef ||
            !TryResolveKnownFunctionReference(knownFunctionRef, context, out var knownQualifiedName, out var knownFuncInfo) ||
            knownFuncInfo.IsRecursive ||
            context.InliningStack.Contains(knownQualifiedName))
        {
            constructorName = default!;
            fieldExpressions = [];
            return false;
        }

        var funcImpl = knownFuncInfo.Function.Declaration.Value;

        if (funcImpl.Arguments.Count is not 0)
        {
            constructorName = default!;
            fieldExpressions = [];
            return false;
        }

        var newContext =
            context with
            {
                InliningStack = context.InliningStack.Add(knownQualifiedName)
            };

        var (inlinedResult, inlinedDecls) =
            InlineFunctionCall(
                knownFuncInfo.ModuleName,
                funcImpl,
                [],
                newContext);

        if (inlinedDecls.Count is 0 &&
            !inlinedResult.Equals(exprNode.Value) &&
            TryDeconstructKnownConstructorApplication(
                new Node<SyntaxTypes.Expression>(s_zeroRange, inlinedResult),
                context,
                out constructorName,
                out fieldExpressions) &&
            fieldExpressions.Count > 0 &&
            context.SingleChoiceConstructors.ContainsKey(constructorName) &&
            fieldExpressions.Any(
                fieldExpr => UnwrapParenthesized(fieldExpr.Value) is SyntaxTypes.Expression.LambdaExpression))
        {
            return true;
        }

        constructorName = default!;
        fieldExpressions = [];
        return false;
    }

    private static bool AreLetDeclarationsIgnorableForConstructorResolution(
        IReadOnlyList<Node<SyntaxTypes.Expression.LetDeclaration>> declarations)
    {
        foreach (var declaration in declarations)
        {
            if (declaration.Value is not SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
            {
                return false;
            }

            if (UnwrapParenthesizedPattern(letDestr.Pattern.Value) is not SyntaxTypes.Pattern.AllPattern)
            {
                return false;
            }
        }

        return declarations.Count > 0;
    }

    private static Node<SyntaxTypes.Expression> BuildConstructorApplication(
        SyntaxTypes.QualifiedNameRef constructorName,
        IReadOnlyList<Node<SyntaxTypes.Expression>> fieldExpressions)
    {
        var constructorExpr =
            new Node<SyntaxTypes.Expression>(
                s_zeroRange,
                new SyntaxTypes.Expression.FunctionOrValue(constructorName.ModuleName, constructorName.Name));

        if (fieldExpressions.Count is 0)
            return constructorExpr;

        return
            new Node<SyntaxTypes.Expression>(
                s_zeroRange,
                new SyntaxTypes.Expression.Application(
                    [.. new[] { constructorExpr }.Concat(fieldExpressions)]));
    }

    private static bool IsLocalVariableReference(
        SyntaxTypes.Expression expr,
        string variableName)
    {
        return
            UnwrapParenthesized(expr) is SyntaxTypes.Expression.FunctionOrValue funcOrValue &&
            funcOrValue.ModuleName.Count is 0 &&
            funcOrValue.Name == variableName;
    }

    private static bool AreEquivalentConstructorNames(
        SyntaxTypes.QualifiedNameRef left,
        SyntaxTypes.QualifiedNameRef right)
    {
        return
            left.Equals(right) ||
            (left.Name == right.Name &&
            (left.ModuleName.Count is 0 || right.ModuleName.Count is 0));
    }

    private static int CountUnshadowedLocalVariableReferences(
        SyntaxTypes.Expression expr,
        string variableName)
    {
        return CountUnshadowedLocalVariableReferences(expr, variableName, shadowed: false);
    }

    private static int CountUnshadowedLocalVariableReferences(
        SyntaxTypes.Expression expr,
        string variableName,
        bool shadowed)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrValue:
                return
                    shadowed ||
                    funcOrValue.ModuleName.Count is not 0 ||
                    funcOrValue.Name != variableName
                    ?
                    0
                    :
                    1;

            case SyntaxTypes.Expression.Application app:
                return
                    app.Arguments.Sum(
                        argument => CountUnshadowedLocalVariableReferences(argument.Value, variableName, shadowed));

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                return CountUnshadowedLocalVariableReferences(paren.Expression.Value, variableName, shadowed);

            case SyntaxTypes.Expression.IfBlock ifBlock:
                return
                    CountUnshadowedLocalVariableReferences(ifBlock.Condition.Value, variableName, shadowed) +
                    CountUnshadowedLocalVariableReferences(ifBlock.ThenBlock.Value, variableName, shadowed) +
                    CountUnshadowedLocalVariableReferences(ifBlock.ElseBlock.Value, variableName, shadowed);

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                {
                    var caseExprCount =
                        CountUnshadowedLocalVariableReferences(caseExpr.CaseBlock.Expression.Value, variableName, shadowed);

                    foreach (var caseItem in caseExpr.CaseBlock.Cases)
                    {
                        caseExprCount +=
                            CountUnshadowedLocalVariableReferences(
                                caseItem.Expression.Value,
                                variableName,
                                shadowed || CollectPatternNames(caseItem.Pattern.Value).Contains(variableName));
                    }

                    return caseExprCount;
                }

            case SyntaxTypes.Expression.LetExpression letExpr:
                {
                    var letBoundNames = new HashSet<string>();
                    var letExprCount = 0;

                    foreach (var declaration in letExpr.Value.Declarations)
                    {
                        switch (declaration.Value)
                        {
                            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                                {
                                    var functionName = letFunc.Function.Declaration.Value.Name.Value;
                                    var functionShadowed = shadowed || functionName == variableName;

                                    letExprCount +=
                                        CountUnshadowedLocalVariableReferences(
                                            letFunc.Function.Declaration.Value.Expression.Value,
                                            variableName,
                                            functionShadowed ||
                                            letFunc.Function.Declaration.Value.Arguments.Any(
                                                arg => CollectPatternNames(arg.Value).Contains(variableName)));

                                    letBoundNames.Add(functionName);
                                    break;
                                }

                            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                                letExprCount +=
                                    CountUnshadowedLocalVariableReferences(
                                        letDestr.Expression.Value,
                                        variableName,
                                        shadowed);

                                foreach (var boundName in CollectPatternNames(letDestr.Pattern.Value))
                                {
                                    letBoundNames.Add(boundName);
                                }

                                break;
                        }
                    }

                    letExprCount +=
                        CountUnshadowedLocalVariableReferences(
                            letExpr.Value.Expression.Value,
                            variableName,
                            shadowed || letBoundNames.Contains(variableName));

                    return letExprCount;
                }

            case SyntaxTypes.Expression.LambdaExpression lambda:
                return
                    CountUnshadowedLocalVariableReferences(
                        lambda.Lambda.Expression.Value,
                        variableName,
                        shadowed ||
                        lambda.Lambda.Arguments.Any(
                            arg => CollectPatternNames(arg.Value).Contains(variableName)));

            case SyntaxTypes.Expression.ListExpr listExpr:
                return
                    listExpr.Elements.Sum(
                        element => CountUnshadowedLocalVariableReferences(element.Value, variableName, shadowed));

            case SyntaxTypes.Expression.TupledExpression tupled:
                return
                    tupled.Elements.Sum(
                        element => CountUnshadowedLocalVariableReferences(element.Value, variableName, shadowed));

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                return
                    recordExpr.Fields.Sum(
                        field =>
                        CountUnshadowedLocalVariableReferences(field.Value.valueExpr.Value, variableName, shadowed));

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                return
                    recordUpdate.Fields.Sum(
                        field =>
                        CountUnshadowedLocalVariableReferences(field.Value.valueExpr.Value, variableName, shadowed));

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                return CountUnshadowedLocalVariableReferences(recordAccess.Record.Value, variableName, shadowed);

            case SyntaxTypes.Expression.Negation negation:
                return CountUnshadowedLocalVariableReferences(negation.Expression.Value, variableName, shadowed);

            case SyntaxTypes.Expression.OperatorApplication opApp:
                return
                    CountUnshadowedLocalVariableReferences(opApp.Left.Value, variableName, shadowed) +
                    CountUnshadowedLocalVariableReferences(opApp.Right.Value, variableName, shadowed);

            default:
                return 0;
        }
    }

    /// <summary>
    /// Attempts to specialize a recursive function call by substituting loop-invariant
    /// function arguments with concrete values. Returns null if specialization is not applicable.
    /// The specialized function is added as a module-level declaration via the context's
    /// AccumulatedDeclarations list.
    /// </summary>
    /// <remarks>
    /// For a call like <c>Helpers.listMap increment list</c> where <c>listMap</c> is recursive
    /// and <c>increment</c> is a known function, this creates a specialized first-order copy
    /// as a module-level declaration and returns the call expression:
    /// <code>
    /// listMap__specialized__1 list =
    ///     case list of
    ///         [] -> []
    ///         first :: rest -> increment first :: listMap__specialized__1 rest
    /// </code>
    /// The specialized function no longer takes the function parameter, turning higher-order
    /// recursive calls into first-order ones that downstream optimizations can handle efficiently.
    /// </remarks>
    private static (SyntaxTypes.Expression?, ImmutableList<Node<SyntaxTypes.Declaration>>) TrySpecializeRecursiveCall(
        FunctionInfo funcInfo,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        InliningContext context)
    {
        var funcParams = funcImpl.Arguments;
        var funcName = funcImpl.Name.Value;
        var funcModuleName = funcInfo.ModuleName;

        // Identify parameters that are both function-valued at the call site
        // and loop-invariant across all recursive self-calls in the body.
        var invariantFuncParamIndices = new List<int>();

        for (var i = 0; i < funcParams.Count && i < appArgs.Count; i++)
        {
            if (funcParams[i].Value is not SyntaxTypes.Pattern.VarPattern varPattern)
                continue;

            if (!IsFunctionExpression(appArgs[i].Value, context))
                continue;

            if (!IsLoopInvariantInRecursiveCalls(
                    funcImpl.Expression.Value, funcName, funcModuleName, varPattern.Name, i))
                continue;

            invariantFuncParamIndices.Add(i);
        }

        if (invariantFuncParamIndices.Count is 0)
            return (null, []);

        // Skip specialization for very large function bodies to avoid
        // stack overflow in the recursive AST rewriting pass.
        if (CountExpressionNodes(funcImpl.Expression.Value) > 2000)
            return (null, []);

        var invariantIndicesSet = new HashSet<int>(invariantFuncParamIndices);

        // Check for mutual recursion group members.
        // If the function is part of a mutual recursion group, specialize all members together.
        var groupMembers = FindMutualRecursionGroupMembers(funcInfo, invariantIndicesSet, context);

        if (groupMembers.Count > 0)
        {
            return
                TrySpecializeMutualRecursiveGroup(
                    funcInfo,
                    funcImpl,
                    appArgs,
                    invariantIndicesSet,
                    groupMembers,
                    context);
        }

        // Build parameter specializations for catalog lookup.
        var paramSpecs =
            ImmutableDictionary.CreateBuilder<int, InliningFunctionSpecialization.ParameterSpecialization>();

        foreach (var paramIndex in invariantFuncParamIndices)
        {
            var classified = InliningFunctionSpecialization.ClassifyArgument(appArgs[paramIndex].Value);

            if (classified is not null)
            {
                paramSpecs[paramIndex] = classified;
            }
            else
            {
                paramSpecs[paramIndex] =
                    new InliningFunctionSpecialization.ParameterSpecialization.ConcreteLambdaValue(
                        new SyntaxTypes.LambdaStruct(
                            Arguments: [],
                            Expression: appArgs[paramIndex]));
            }
        }

        var specializedName =
            LookupSpecializedNameForHigherOrder(
                funcName,
                funcModuleName,
                paramSpecs.ToImmutable(),
                context);

        if (specializedName is null)
            return (null, []);

        // Step 1: Rewrite recursive self-calls in the body to use the specialized name
        // and strip the invariant arguments.
        var rewrittenBody =
            RewriteRecursiveCallsInExpression(
                funcImpl.Expression,
                funcName,
                funcModuleName,
                specializedName,
                invariantIndicesSet);

        // Step 2: Substitute invariant parameter names with concrete argument expressions.
        var substitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();

        foreach (var paramIndex in invariantFuncParamIndices)
        {
            var varPattern = (SyntaxTypes.Pattern.VarPattern)funcParams[paramIndex].Value;
            substitutions[varPattern.Name] = appArgs[paramIndex];
        }

        var substitutedBody = SubstituteInExpression(rewrittenBody, substitutions);

        // Step 3: Qualify lifted helper references from the callee module.
        var qualifiedBody =
            QualifyLiftedHelperReferencesFromCalleeModule(
                substitutedBody,
                funcInfo.ModuleName,
                context);

        // Build remaining parameter list (non-invariant params only).
        var remainingParams =
            funcParams
            .Where((_, i) => !invariantIndicesSet.Contains(i))
            .ToImmutableArray();

        // Create the specialized function as a module-level declaration.
        var specializedImpl =
            new SyntaxTypes.FunctionImplementation(
                Name: new Node<string>(s_zeroRange, specializedName),
                Arguments: remainingParams,
                Expression: qualifiedBody);

        var specializedFunc =
            new SyntaxTypes.FunctionStruct(
                Documentation: null,
                Signature: null,
                Declaration: new Node<SyntaxTypes.FunctionImplementation>(s_zeroRange, specializedImpl));

        var newDecl =
            new Node<SyntaxTypes.Declaration>(
                s_zeroRange,
                new SyntaxTypes.Declaration.FunctionDeclaration(specializedFunc));

        // Build the call to the specialized function with remaining (non-invariant) arguments.
        var callArgs =
            new List<Node<SyntaxTypes.Expression>>
            {
                new(s_zeroRange, new SyntaxTypes.Expression.FunctionOrValue([], specializedName))
            };

        for (var i = 0; i < appArgs.Count; i++)
        {
            if (i < funcParams.Count && invariantIndicesSet.Contains(i))
                continue;

            callArgs.Add(appArgs[i]);
        }

        SyntaxTypes.Expression callExpr =
            callArgs.Count is 1
            ?
            callArgs[0].Value
            :
            new SyntaxTypes.Expression.Application([.. callArgs]);

        return (callExpr, ImmutableList.Create(newDecl));
    }

    /// <summary>
    /// Checks that the parameter at <paramref name="paramIndex"/> is passed unchanged
    /// in all recursive self-calls to <paramref name="funcName"/> within the expression.
    /// Returns true if the parameter is loop-invariant (or there are no recursive calls).
    /// Uses an iterative worklist to avoid stack overflow on deeply nested ASTs.
    /// </summary>
    private static bool IsLoopInvariantInRecursiveCalls(
        SyntaxTypes.Expression body,
        string funcName,
        ModuleName funcModuleName,
        string paramName,
        int paramIndex)
    {
        var worklist = new Stack<SyntaxTypes.Expression>();
        worklist.Push(body);

        while (worklist.Count > 0)
        {
            var expr = worklist.Pop();

            switch (expr)
            {
                case SyntaxTypes.Expression.Application app:
                    {
                        if (app.Arguments.Count > 0 &&
                            app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue fov &&
                            IsRecursiveCallReference(fov, funcName, funcModuleName))
                        {
                            var argPosition = paramIndex + 1;

                            if (app.Arguments.Count <= argPosition)
                                return false;

                            if (app.Arguments[argPosition].Value is not SyntaxTypes.Expression.FunctionOrValue argRef ||
                                argRef.ModuleName.Count is not 0 ||
                                argRef.Name != paramName)
                            {
                                return false;
                            }
                        }

                        foreach (var arg in app.Arguments)
                            worklist.Push(arg.Value);

                        break;
                    }

                case SyntaxTypes.Expression.ParenthesizedExpression paren:
                    worklist.Push(paren.Expression.Value);
                    break;

                case SyntaxTypes.Expression.IfBlock ifBlock:
                    worklist.Push(ifBlock.Condition.Value);
                    worklist.Push(ifBlock.ThenBlock.Value);
                    worklist.Push(ifBlock.ElseBlock.Value);
                    break;

                case SyntaxTypes.Expression.CaseExpression caseExpr:
                    worklist.Push(caseExpr.CaseBlock.Expression.Value);

                    foreach (var c in caseExpr.CaseBlock.Cases)
                        worklist.Push(c.Expression.Value);

                    break;

                case SyntaxTypes.Expression.LetExpression letExpr:
                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case SyntaxTypes.Expression.LetDeclaration.LetFunction lf:
                                worklist.Push(lf.Function.Declaration.Value.Expression.Value);
                                break;

                            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring ld:
                                worklist.Push(ld.Expression.Value);
                                break;
                        }
                    }

                    worklist.Push(letExpr.Value.Expression.Value);
                    break;

                case SyntaxTypes.Expression.LambdaExpression lambda:
                    worklist.Push(lambda.Lambda.Expression.Value);
                    break;

                case SyntaxTypes.Expression.ListExpr listExpr:
                    foreach (var e in listExpr.Elements)
                        worklist.Push(e.Value);

                    break;

                case SyntaxTypes.Expression.TupledExpression tupled:
                    foreach (var e in tupled.Elements)
                        worklist.Push(e.Value);

                    break;

                case SyntaxTypes.Expression.RecordExpr recordExpr:
                    foreach (var f in recordExpr.Fields)
                        worklist.Push(f.Value.valueExpr.Value);

                    break;

                case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                    foreach (var f in recordUpdate.Fields)
                        worklist.Push(f.Value.valueExpr.Value);

                    break;

                case SyntaxTypes.Expression.RecordAccess recordAccess:
                    worklist.Push(recordAccess.Record.Value);
                    break;

                case SyntaxTypes.Expression.Negation negation:
                    worklist.Push(negation.Expression.Value);
                    break;

                case SyntaxTypes.Expression.OperatorApplication opApp:
                    worklist.Push(opApp.Left.Value);
                    worklist.Push(opApp.Right.Value);
                    break;
            }
        }

        return true;
    }

    /// <summary>
    /// Checks that the parameter at <paramref name="paramIndex"/> is passed unchanged
    /// in all calls to any of the <paramref name="targets"/> functions within the expression.
    /// This generalizes <see cref="IsLoopInvariantInRecursiveCalls"/> to handle mutual recursion groups.
    /// </summary>
    private static bool IsLoopInvariantInCallsToAny(
        SyntaxTypes.Expression body,
        IReadOnlyList<(string FuncName, ModuleName ModuleName)> targets,
        string paramName,
        int paramIndex)
    {
        var worklist = new Stack<SyntaxTypes.Expression>();
        worklist.Push(body);

        while (worklist.Count > 0)
        {
            var expr = worklist.Pop();

            switch (expr)
            {
                case SyntaxTypes.Expression.Application app:
                    {
                        if (app.Arguments.Count > 0 &&
                            app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue fov &&
                            targets.Any(t => IsRecursiveCallReference(fov, t.FuncName, t.ModuleName)))
                        {
                            var argPosition = paramIndex + 1;

                            if (app.Arguments.Count <= argPosition)
                                return false;

                            if (app.Arguments[argPosition].Value is not SyntaxTypes.Expression.FunctionOrValue argRef ||
                                argRef.ModuleName.Count is not 0 ||
                                argRef.Name != paramName)
                            {
                                return false;
                            }
                        }

                        foreach (var arg in app.Arguments)
                            worklist.Push(arg.Value);

                        break;
                    }

                case SyntaxTypes.Expression.ParenthesizedExpression paren:
                    worklist.Push(paren.Expression.Value);
                    break;

                case SyntaxTypes.Expression.IfBlock ifBlock:
                    worklist.Push(ifBlock.Condition.Value);
                    worklist.Push(ifBlock.ThenBlock.Value);
                    worklist.Push(ifBlock.ElseBlock.Value);
                    break;

                case SyntaxTypes.Expression.CaseExpression caseExpr:
                    worklist.Push(caseExpr.CaseBlock.Expression.Value);

                    foreach (var c in caseExpr.CaseBlock.Cases)
                        worklist.Push(c.Expression.Value);

                    break;

                case SyntaxTypes.Expression.LetExpression letExpr:
                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case SyntaxTypes.Expression.LetDeclaration.LetFunction lf:
                                worklist.Push(lf.Function.Declaration.Value.Expression.Value);
                                break;

                            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring ld:
                                worklist.Push(ld.Expression.Value);
                                break;
                        }
                    }

                    worklist.Push(letExpr.Value.Expression.Value);
                    break;

                case SyntaxTypes.Expression.LambdaExpression lambda:
                    worklist.Push(lambda.Lambda.Expression.Value);
                    break;

                case SyntaxTypes.Expression.ListExpr listExpr:
                    foreach (var e in listExpr.Elements)
                        worklist.Push(e.Value);

                    break;

                case SyntaxTypes.Expression.TupledExpression tupled:
                    foreach (var e in tupled.Elements)
                        worklist.Push(e.Value);

                    break;

                case SyntaxTypes.Expression.RecordExpr recordExpr:
                    foreach (var f in recordExpr.Fields)
                        worklist.Push(f.Value.valueExpr.Value);

                    break;

                case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                    foreach (var f in recordUpdate.Fields)
                        worklist.Push(f.Value.valueExpr.Value);

                    break;

                case SyntaxTypes.Expression.RecordAccess recordAccess:
                    worklist.Push(recordAccess.Record.Value);
                    break;

                case SyntaxTypes.Expression.Negation negation:
                    worklist.Push(negation.Expression.Value);
                    break;

                case SyntaxTypes.Expression.OperatorApplication opApp:
                    worklist.Push(opApp.Left.Value);
                    worklist.Push(opApp.Right.Value);
                    break;
            }
        }

        return true;
    }

    /// <summary>
    /// Finds other functions that form a direct mutual recursion group with the given function.
    /// Returns an empty list if no mutual recursion partners are found.
    /// Verifies that all invariant parameter positions are compatible across the group.
    /// </summary>
    private static List<FunctionInfo> FindMutualRecursionGroupMembers(
        FunctionInfo startFunc,
        HashSet<int> invariantParamIndices,
        InliningContext context)
    {
        if (invariantParamIndices.Count is 0)
            return [];

        var startFuncName = startFunc.Function.Declaration.Value.Name.Value;
        var startFuncModule = startFunc.ModuleName;
        var startKey = (startFuncModule, startFuncName);
        var startBody = startFunc.Function.Declaration.Value.Expression.Value;

        var candidates = new List<FunctionInfo>();

        // Find functions referenced directly in the start function's body
        var directRefs =
            CollectFunctionReferences(startBody)
            .Distinct(s_moduleNameTupleComparer);

        foreach (var refKey in directRefs)
        {
            if (s_moduleNameTupleComparer.Equals(refKey, startKey))
                continue;

            if (!context.FunctionsByQualifiedName.TryGetValue(refKey, out var refFunc))
                continue;

            if (!refFunc.IsRecursive)
                continue;

            var refImpl = refFunc.Function.Declaration.Value;
            var refBody = refImpl.Expression.Value;

            // Check if this function references back to the start function (direct mutual recursion)
            var refRefs = CollectFunctionReferences(refBody);

            if (!refRefs.Any(r => s_moduleNameTupleComparer.Equals(r, startKey)))
                continue;

            // Check that the function has enough parameters for all invariant positions
            var maxInvariantIdx = invariantParamIndices.Max();

            if (refImpl.Arguments.Count <= maxInvariantIdx)
                continue;

            // Check that ALL invariant params are VarPatterns
            if (!invariantParamIndices.All(idx => refImpl.Arguments[idx].Value is SyntaxTypes.Pattern.VarPattern))
                continue;

            // Size check
            if (CountExpressionNodes(refBody) > 2000)
                continue;

            candidates.Add(refFunc);
        }

        if (candidates.Count is 0)
            return candidates;

        // Verify loop invariance of all function params across the entire group.
        // Build list of all group members (including start function).
        var allGroupTargets =
            new List<(string FuncName, ModuleName ModuleName)>
            {
                (startFuncName, startFuncModule)
            };

        foreach (var member in candidates)
        {
            var memberImpl = member.Function.Declaration.Value;
            allGroupTargets.Add((memberImpl.Name.Value, member.ModuleName));
        }

        // Check invariance for the START function's body
        foreach (var paramIdx in invariantParamIndices)
        {
            var startVarPattern =
                (SyntaxTypes.Pattern.VarPattern)
                startFunc.Function.Declaration.Value.Arguments[paramIdx].Value;

            if (!IsLoopInvariantInCallsToAny(startBody, allGroupTargets, startVarPattern.Name, paramIdx))
                return [];
        }

        // Check invariance for each candidate member's body
        var validMembers = new List<FunctionInfo>();

        foreach (var member in candidates)
        {
            var memberImpl = member.Function.Declaration.Value;
            var memberBody = memberImpl.Expression.Value;
            var allInvariant = true;

            foreach (var paramIdx in invariantParamIndices)
            {
                var memberVarPattern = (SyntaxTypes.Pattern.VarPattern)memberImpl.Arguments[paramIdx].Value;

                if (!IsLoopInvariantInCallsToAny(memberBody, allGroupTargets, memberVarPattern.Name, paramIdx))
                {
                    allInvariant = false;
                    break;
                }
            }

            if (allInvariant)
                validMembers.Add(member);
        }

        return validMembers;
    }

    /// <summary>
    /// Checks whether a <see cref="SyntaxTypes.Expression.FunctionOrValue"/> reference matches
    /// a recursive call to the given function, allowing both unqualified and qualified references.
    /// After canonicalization, recursive calls may use the qualified form (e.g., <c>Helpers.listMap</c>).
    /// </summary>
    private static bool IsRecursiveCallReference(
        SyntaxTypes.Expression.FunctionOrValue fov,
        string funcName,
        ModuleName funcModuleName)
    {
        if (fov.Name != funcName)
            return false;

        // Match unqualified reference (e.g., `listMap`)
        if (fov.ModuleName.Count is 0)
            return true;

        // Match qualified reference (e.g., `Helpers.listMap`)
        if (fov.ModuleName.Count == funcModuleName.Count)
        {
            for (var i = 0; i < fov.ModuleName.Count; i++)
            {
                if (fov.ModuleName[i] != funcModuleName[i])
                    return false;
            }

            return true;
        }

        return false;
    }

    /// <summary>
    /// Counts the number of expression nodes in the AST, up to a maximum.
    /// Uses an iterative worklist to avoid stack overflow.
    /// </summary>
    private static int CountExpressionNodes(SyntaxTypes.Expression body, int max = 3000)
    {
        var count = 0;
        var worklist = new Stack<SyntaxTypes.Expression>();
        worklist.Push(body);

        while (worklist.Count > 0 && count < max)
        {
            count++;
            var expr = worklist.Pop();

            switch (expr)
            {
                case SyntaxTypes.Expression.Application app:
                    foreach (var arg in app.Arguments)
                        worklist.Push(arg.Value);

                    break;

                case SyntaxTypes.Expression.ParenthesizedExpression paren:
                    worklist.Push(paren.Expression.Value);
                    break;

                case SyntaxTypes.Expression.IfBlock ifBlock:
                    worklist.Push(ifBlock.Condition.Value);
                    worklist.Push(ifBlock.ThenBlock.Value);
                    worklist.Push(ifBlock.ElseBlock.Value);
                    break;

                case SyntaxTypes.Expression.CaseExpression caseExpr:
                    worklist.Push(caseExpr.CaseBlock.Expression.Value);

                    foreach (var c in caseExpr.CaseBlock.Cases)
                        worklist.Push(c.Expression.Value);

                    break;

                case SyntaxTypes.Expression.LetExpression letExpr:
                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case SyntaxTypes.Expression.LetDeclaration.LetFunction lf:
                                worklist.Push(lf.Function.Declaration.Value.Expression.Value);
                                break;

                            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring ld:
                                worklist.Push(ld.Expression.Value);
                                break;
                        }
                    }

                    worklist.Push(letExpr.Value.Expression.Value);
                    break;

                case SyntaxTypes.Expression.LambdaExpression lambda:
                    worklist.Push(lambda.Lambda.Expression.Value);
                    break;

                case SyntaxTypes.Expression.ListExpr listExpr:
                    foreach (var e in listExpr.Elements)
                        worklist.Push(e.Value);

                    break;

                case SyntaxTypes.Expression.TupledExpression tupled:
                    foreach (var e in tupled.Elements)
                        worklist.Push(e.Value);

                    break;

                case SyntaxTypes.Expression.RecordExpr recordExpr:
                    foreach (var f in recordExpr.Fields)
                        worklist.Push(f.Value.valueExpr.Value);

                    break;

                case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                    foreach (var f in recordUpdate.Fields)
                        worklist.Push(f.Value.valueExpr.Value);

                    break;

                case SyntaxTypes.Expression.RecordAccess recordAccess:
                    worklist.Push(recordAccess.Record.Value);
                    break;

                case SyntaxTypes.Expression.Negation negation:
                    worklist.Push(negation.Expression.Value);
                    break;

                case SyntaxTypes.Expression.OperatorApplication opApp:
                    worklist.Push(opApp.Left.Value);
                    worklist.Push(opApp.Right.Value);
                    break;
            }
        }

        return count;
    }

    /// <summary>
    /// Rewrites all recursive self-calls in the expression from the original function name
    /// to the specialized name, stripping arguments at the invariant parameter positions.
    /// </summary>
    private static Node<SyntaxTypes.Expression> RewriteRecursiveCallsInExpression(
        Node<SyntaxTypes.Expression> exprNode,
        string originalFuncName,
        ModuleName originalFuncModuleName,
        string specializedName,
        HashSet<int> invariantParamIndices)
    {
        var expr = exprNode.Value;

        var rewrittenExpr =
            expr switch
            {
                SyntaxTypes.Expression.Application app =>
                RewriteRecursiveCallApplication(
                    app,
                    originalFuncName,
                    originalFuncModuleName,
                    specializedName,
                    invariantParamIndices),

                SyntaxTypes.Expression.ParenthesizedExpression paren =>
                new SyntaxTypes.Expression.ParenthesizedExpression(
                    RewriteRecursiveCallsInExpression(
                        paren.Expression,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        invariantParamIndices)),

                SyntaxTypes.Expression.IfBlock ifBlock =>
                new SyntaxTypes.Expression.IfBlock(
                    RewriteRecursiveCallsInExpression(
                        ifBlock.Condition,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        invariantParamIndices),
                    RewriteRecursiveCallsInExpression(
                        ifBlock.ThenBlock,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        invariantParamIndices),
                    RewriteRecursiveCallsInExpression(
                        ifBlock.ElseBlock,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        invariantParamIndices)),

                SyntaxTypes.Expression.CaseExpression caseExpr =>
                new SyntaxTypes.Expression.CaseExpression(
                    new SyntaxTypes.CaseBlock(
                        RewriteRecursiveCallsInExpression(
                            caseExpr.CaseBlock.Expression,
                            originalFuncName,
                            originalFuncModuleName,
                            specializedName,
                            invariantParamIndices),
                        [
                        .. caseExpr.CaseBlock.Cases.Select(
                            c =>
                            new SyntaxTypes.Case(
                                c.Pattern,
                                RewriteRecursiveCallsInExpression(
                                    c.Expression,
                                    originalFuncName,
                                    originalFuncModuleName,
                                    specializedName,
                                    invariantParamIndices)))
                        ])),

                SyntaxTypes.Expression.LetExpression letExpr =>
                new SyntaxTypes.Expression.LetExpression(
                    RewriteRecursiveCallsInLetBlock(
                        letExpr.Value,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        invariantParamIndices)),

                SyntaxTypes.Expression.LambdaExpression lambda =>
                new SyntaxTypes.Expression.LambdaExpression(
                    new SyntaxTypes.LambdaStruct(
                        lambda.Lambda.Arguments,
                        RewriteRecursiveCallsInExpression(
                            lambda.Lambda.Expression,
                            originalFuncName,
                            originalFuncModuleName,
                            specializedName,
                            invariantParamIndices))),

                SyntaxTypes.Expression.ListExpr listExpr =>
                new SyntaxTypes.Expression.ListExpr(
                    [
                    .. listExpr.Elements.Select(
                        e =>
                        RewriteRecursiveCallsInExpression(
                            e,
                            originalFuncName,
                            originalFuncModuleName,
                            specializedName,
                            invariantParamIndices))
                    ]),

                SyntaxTypes.Expression.TupledExpression tupled =>
                new SyntaxTypes.Expression.TupledExpression(
                    [
                    .. tupled.Elements.Select(
                        e =>
                        RewriteRecursiveCallsInExpression(
                            e,
                            originalFuncName,
                            originalFuncModuleName,
                            specializedName,
                            invariantParamIndices))
                    ]),

                SyntaxTypes.Expression.RecordExpr recordExpr =>
                new SyntaxTypes.Expression.RecordExpr(
                    [
                    .. recordExpr.Fields.Select(
                        f =>
                        new Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>(
                            f.Range,
                            (f.Value.fieldName,
                            RewriteRecursiveCallsInExpression(
                                f.Value.valueExpr,
                                originalFuncName,
                                originalFuncModuleName,
                                specializedName,
                                invariantParamIndices))))
                    ]),

                SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [
                    .. recordUpdate.Fields.Select(
                        f =>
                        new Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>(
                            f.Range,
                            (f.Value.fieldName,
                            RewriteRecursiveCallsInExpression(
                                f.Value.valueExpr,
                                originalFuncName,
                                originalFuncModuleName,
                                specializedName,
                                invariantParamIndices))))
                    ]),

                SyntaxTypes.Expression.RecordAccess recordAccess =>
                new SyntaxTypes.Expression.RecordAccess(
                    RewriteRecursiveCallsInExpression(
                        recordAccess.Record,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        invariantParamIndices),
                    recordAccess.FieldName),

                SyntaxTypes.Expression.Negation negation =>
                new SyntaxTypes.Expression.Negation(
                    RewriteRecursiveCallsInExpression(
                        negation.Expression,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        invariantParamIndices)),

                SyntaxTypes.Expression.OperatorApplication opApp =>
                new SyntaxTypes.Expression.OperatorApplication(
                    opApp.Operator,
                    opApp.Direction,
                    RewriteRecursiveCallsInExpression(
                        opApp.Left,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        invariantParamIndices),
                    RewriteRecursiveCallsInExpression(
                        opApp.Right,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        invariantParamIndices)),

                _ =>
                expr
            };

        return new Node<SyntaxTypes.Expression>(exprNode.Range, rewrittenExpr);
    }

    private static SyntaxTypes.Expression RewriteRecursiveCallApplication(
        SyntaxTypes.Expression.Application app,
        string originalFuncName,
        ModuleName originalFuncModuleName,
        string specializedName,
        HashSet<int> invariantParamIndices)
    {
        // Check if this application is a recursive call to the original function
        if (app.Arguments.Count > 0 &&
            app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue fov &&
            IsRecursiveCallReference(fov, originalFuncName, originalFuncModuleName))
        {
            var newArgs =
                new List<Node<SyntaxTypes.Expression>>
                {
                    new(s_zeroRange, new SyntaxTypes.Expression.FunctionOrValue([], specializedName))
                };

            for (var i = 1; i < app.Arguments.Count; i++)
            {
                var argIndex = i - 1;

                if (!invariantParamIndices.Contains(argIndex))
                {
                    newArgs.Add(
                        RewriteRecursiveCallsInExpression(
                            app.Arguments[i],
                            originalFuncName,
                            originalFuncModuleName,
                            specializedName,
                            invariantParamIndices));
                }
            }

            return
                newArgs.Count is 1
                ?
                newArgs[0].Value
                :
                new SyntaxTypes.Expression.Application([.. newArgs]);
        }

        return
            new SyntaxTypes.Expression.Application(
                [
                .. app.Arguments.Select(
                    a =>
                    RewriteRecursiveCallsInExpression(
                        a,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        invariantParamIndices))
                ]);
    }

    private static SyntaxTypes.Expression.LetBlock RewriteRecursiveCallsInLetBlock(
        SyntaxTypes.Expression.LetBlock letBlock,
        string originalFuncName,
        ModuleName originalFuncModuleName,
        string specializedName,
        HashSet<int> invariantParamIndices)
    {
        return
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                .. letBlock.Declarations.Select(
                    d =>
                    {
                        var rewrittenDecl =
                            d.Value switch
                            {
                                SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                                new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                                    letFunc.Function with
                                    {
                                        Declaration =
                                        new Node<SyntaxTypes.FunctionImplementation>(
                                            letFunc.Function.Declaration.Range,
                                            letFunc.Function.Declaration.Value with
                                            {
                                                Expression =
                                                RewriteRecursiveCallsInExpression(
                                                    letFunc.Function.Declaration.Value.Expression,
                                                    originalFuncName,
                                                    originalFuncModuleName,
                                                    specializedName,
                                                    invariantParamIndices)
                                            })
                                    }),

                                SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                                new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                                    letDestr.Pattern,
                                    RewriteRecursiveCallsInExpression(
                                        letDestr.Expression,
                                        originalFuncName,
                                        originalFuncModuleName,
                                        specializedName,
                                        invariantParamIndices)),

                                _ =>
                                d.Value
                            };

                        return new Node<SyntaxTypes.Expression.LetDeclaration>(d.Range, rewrittenDecl);
                    })
                ],
                Expression:
                RewriteRecursiveCallsInExpression(
                    letBlock.Expression,
                    originalFuncName,
                    originalFuncModuleName,
                    specializedName,
                    invariantParamIndices));
    }

    private static Node<SyntaxTypes.Expression> RewriteRecursiveCallsForSingleChoiceTagSpecialization(
        Node<SyntaxTypes.Expression> exprNode,
        string originalFuncName,
        ModuleName originalFuncModuleName,
        string specializedName,
        SingleChoiceTagSpecialization specialization)
    {
        var expr = exprNode.Value;

        var rewrittenExpr =
            expr switch
            {
                SyntaxTypes.Expression.Application app =>
                RewriteRecursiveCallApplicationForSingleChoiceTagSpecialization(
                    app,
                    originalFuncName,
                    originalFuncModuleName,
                    specializedName,
                    specialization),

                SyntaxTypes.Expression.ParenthesizedExpression paren =>
                new SyntaxTypes.Expression.ParenthesizedExpression(
                    RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                        paren.Expression,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        specialization)),

                SyntaxTypes.Expression.IfBlock ifBlock =>
                new SyntaxTypes.Expression.IfBlock(
                    RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                        ifBlock.Condition,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        specialization),
                    RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                        ifBlock.ThenBlock,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        specialization),
                    RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                        ifBlock.ElseBlock,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        specialization)),

                SyntaxTypes.Expression.CaseExpression caseExpr =>
                new SyntaxTypes.Expression.CaseExpression(
                    new SyntaxTypes.CaseBlock(
                        RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                            caseExpr.CaseBlock.Expression,
                            originalFuncName,
                            originalFuncModuleName,
                            specializedName,
                            specialization),
                        [
                        .. caseExpr.CaseBlock.Cases.Select(
                            c =>
                            new SyntaxTypes.Case(
                                c.Pattern,
                                RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                                    c.Expression,
                                    originalFuncName,
                                    originalFuncModuleName,
                                    specializedName,
                                    specialization)))
                        ])),

                SyntaxTypes.Expression.LetExpression letExpr =>
                new SyntaxTypes.Expression.LetExpression(
                    RewriteRecursiveCallsInLetBlockForSingleChoiceTagSpecialization(
                        letExpr.Value,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        specialization)),

                SyntaxTypes.Expression.LambdaExpression lambda =>
                new SyntaxTypes.Expression.LambdaExpression(
                    new SyntaxTypes.LambdaStruct(
                        lambda.Lambda.Arguments,
                        RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                            lambda.Lambda.Expression,
                            originalFuncName,
                            originalFuncModuleName,
                            specializedName,
                            specialization))),

                SyntaxTypes.Expression.ListExpr listExpr =>
                new SyntaxTypes.Expression.ListExpr(
                    [
                    .. listExpr.Elements.Select(
                        e =>
                        RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                            e,
                            originalFuncName,
                            originalFuncModuleName,
                            specializedName,
                            specialization))
                    ]),

                SyntaxTypes.Expression.TupledExpression tupled =>
                new SyntaxTypes.Expression.TupledExpression(
                    [
                    .. tupled.Elements.Select(
                        e =>
                        RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                            e,
                            originalFuncName,
                            originalFuncModuleName,
                            specializedName,
                            specialization))
                    ]),

                SyntaxTypes.Expression.RecordExpr recordExpr =>
                new SyntaxTypes.Expression.RecordExpr(
                    [
                    .. recordExpr.Fields.Select(
                        f =>
                        new Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>(
                            f.Range,
                            (f.Value.fieldName,
                            RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                                f.Value.valueExpr,
                                originalFuncName,
                                originalFuncModuleName,
                                specializedName,
                                specialization))))
                    ]),

                SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [
                    .. recordUpdate.Fields.Select(
                        f =>
                        new Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>(
                            f.Range,
                            (f.Value.fieldName,
                            RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                                f.Value.valueExpr,
                                originalFuncName,
                                originalFuncModuleName,
                                specializedName,
                                specialization))))
                    ]),

                SyntaxTypes.Expression.RecordAccess recordAccess =>
                new SyntaxTypes.Expression.RecordAccess(
                    RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                        recordAccess.Record,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        specialization),
                    recordAccess.FieldName),

                SyntaxTypes.Expression.Negation negation =>
                new SyntaxTypes.Expression.Negation(
                    RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                        negation.Expression,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        specialization)),

                SyntaxTypes.Expression.OperatorApplication opApp =>
                new SyntaxTypes.Expression.OperatorApplication(
                    opApp.Operator,
                    opApp.Direction,
                    RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                        opApp.Left,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        specialization),
                    RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                        opApp.Right,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        specialization)),

                _ =>
                expr
            };

        return new Node<SyntaxTypes.Expression>(exprNode.Range, rewrittenExpr);
    }

    private static SyntaxTypes.Expression RewriteRecursiveCallApplicationForSingleChoiceTagSpecialization(
        SyntaxTypes.Expression.Application app,
        string originalFuncName,
        ModuleName originalFuncModuleName,
        string specializedName,
        SingleChoiceTagSpecialization specialization)
    {
        if (app.Arguments.Count > 0 &&
            app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue fov &&
            IsRecursiveCallReference(fov, originalFuncName, originalFuncModuleName))
        {
            var newArgs =
                new List<Node<SyntaxTypes.Expression>>
                {
                    new(s_zeroRange, new SyntaxTypes.Expression.FunctionOrValue([], specializedName))
                };

            for (var i = 1; i < app.Arguments.Count; i++)
            {
                var argIndex = i - 1;

                if (argIndex == specialization.ParamIndex)
                {
                    var constructorArgument =
                        IsLocalVariableReference(app.Arguments[i].Value, specialization.ParamName)
                        ?
                        specialization.ReplacementArgument
                        :
                        app.Arguments[i];

                    if (!TryDeconstructConstructorApplication(
                            constructorArgument,
                            out var constructorName,
                            out var fieldExpressions) ||
                        !AreEquivalentConstructorNames(constructorName, specialization.ConstructorName))
                    {
                        return
                            new SyntaxTypes.Expression.Application(
                                [
                                .. app.Arguments.Select(
                                    a =>
                                    RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                                        a,
                                        originalFuncName,
                                        originalFuncModuleName,
                                        specializedName,
                                        specialization))
                                ]);
                    }

                    var flattenedSpecializedArgs = new List<Node<SyntaxTypes.Expression>>();

                    foreach (var fieldPlan in specialization.FieldPlans)
                    {
                        if (fieldPlan.FlattenedParameters.Count is 0)
                            continue;

                        if (!TryRewriteSingleChoiceTagFieldArguments(
                                fieldExpressions[fieldPlan.FieldIndex],
                                fieldPlan,
                                originalFuncName,
                                originalFuncModuleName,
                                specializedName,
                                specialization,
                                out var rewrittenFieldArguments))
                        {
                            return
                                new SyntaxTypes.Expression.Application(
                                    [
                                    .. app.Arguments.Select(
                                        a =>
                                        RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                                            a,
                                            originalFuncName,
                                            originalFuncModuleName,
                                            specializedName,
                                            specialization))
                                    ]);
                        }

                        flattenedSpecializedArgs.AddRange(rewrittenFieldArguments);
                    }

                    if (flattenedSpecializedArgs.Count > 1)
                    {
                        newArgs.Add(
                            new Node<SyntaxTypes.Expression>(
                                s_zeroRange,
                                new SyntaxTypes.Expression.TupledExpression([.. flattenedSpecializedArgs])));
                    }
                    else
                    {
                        newArgs.AddRange(flattenedSpecializedArgs);
                    }

                    continue;
                }

                newArgs.Add(
                    RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                        app.Arguments[i],
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        specialization));
            }

            return
                newArgs.Count is 1
                ?
                newArgs[0].Value
                :
                new SyntaxTypes.Expression.Application([.. newArgs]);
        }

        return
            new SyntaxTypes.Expression.Application(
                [
                .. app.Arguments.Select(
                    a =>
                    RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                        a,
                        originalFuncName,
                        originalFuncModuleName,
                        specializedName,
                        specialization))
                ]);
    }

    private static bool TryRewriteSingleChoiceTagFieldArguments(
        Node<SyntaxTypes.Expression> fieldExpression,
        SingleChoiceTagFieldPlan fieldPlan,
        string originalFuncName,
        ModuleName originalFuncModuleName,
        string specializedName,
        SingleChoiceTagSpecialization specialization,
        out IReadOnlyList<Node<SyntaxTypes.Expression>> rewrittenArguments)
    {
        if (fieldPlan.NestedFieldPlans.Count is 0)
        {
            rewrittenArguments =
                [
                RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                    fieldExpression,
                    originalFuncName,
                    originalFuncModuleName,
                    specializedName,
                    specialization)
                ];

            return true;
        }

        if (fieldPlan.ParameterSpecialization is not InliningFunctionSpecialization.ParameterSpecialization.SingleChoiceTagUnwrap nestedTagUnwrap ||
            !TryDeconstructConstructorApplication(
                fieldExpression,
                out var constructorName,
                out var fieldExpressions) ||
            !AreEquivalentConstructorNames(constructorName, nestedTagUnwrap.ConstructorName))
        {
            rewrittenArguments = [];
            return false;
        }

        var nestedArguments = new List<Node<SyntaxTypes.Expression>>();

        foreach (var nestedFieldPlan in fieldPlan.NestedFieldPlans)
        {
            if (nestedFieldPlan.FlattenedParameters.Count is 0)
                continue;

            if (!TryRewriteSingleChoiceTagFieldArguments(
                    fieldExpressions[nestedFieldPlan.FieldIndex],
                    nestedFieldPlan,
                    originalFuncName,
                    originalFuncModuleName,
                    specializedName,
                    specialization,
                    out var rewrittenNestedArguments))
            {
                rewrittenArguments = [];
                return false;
            }

            nestedArguments.AddRange(rewrittenNestedArguments);
        }

        rewrittenArguments = nestedArguments;
        return true;
    }

    private static SyntaxTypes.Expression.LetBlock RewriteRecursiveCallsInLetBlockForSingleChoiceTagSpecialization(
        SyntaxTypes.Expression.LetBlock letBlock,
        string originalFuncName,
        ModuleName originalFuncModuleName,
        string specializedName,
        SingleChoiceTagSpecialization specialization)
    {
        return
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                .. letBlock.Declarations.Select(
                    d =>
                    {
                        var rewrittenDecl =
                            d.Value switch
                            {
                                SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                                new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                                    letFunc.Function with
                                    {
                                        Declaration =
                                        new Node<SyntaxTypes.FunctionImplementation>(
                                            letFunc.Function.Declaration.Range,
                                            letFunc.Function.Declaration.Value with
                                            {
                                                Expression =
                                                RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                                                    letFunc.Function.Declaration.Value.Expression,
                                                    originalFuncName,
                                                    originalFuncModuleName,
                                                    specializedName,
                                                    specialization)
                                            })
                                    }),

                                SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                                new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                                    letDestr.Pattern,
                                    RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                                        letDestr.Expression,
                                        originalFuncName,
                                        originalFuncModuleName,
                                        specializedName,
                                        specialization)),

                                _ =>
                                d.Value
                            };

                        return new Node<SyntaxTypes.Expression.LetDeclaration>(d.Range, rewrittenDecl);
                    })
                ],
                Expression:
                RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                    letBlock.Expression,
                    originalFuncName,
                    originalFuncModuleName,
                    specializedName,
                    specialization));
    }

    /// <summary>
    /// Specializes a mutual recursion group. Creates specialized versions of all group members
    /// where function parameters are substituted with concrete values, and all intra-group calls
    /// are rewritten to target the specialized versions. Specialized functions are added as
    /// module-level declarations via the context's AccumulatedDeclarations list.
    /// </summary>
    private static (SyntaxTypes.Expression, ImmutableList<Node<SyntaxTypes.Declaration>>) TrySpecializeMutualRecursiveGroup(
        FunctionInfo startFunc,
        SyntaxTypes.FunctionImplementation startImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        HashSet<int> invariantIndicesSet,
        List<FunctionInfo> groupMembers,
        InliningContext context)
    {
        var newDecls = ImmutableList.CreateBuilder<Node<SyntaxTypes.Declaration>>();

        // Build parameter specializations for catalog lookup (same for all group members).
        var paramSpecs =
            ImmutableDictionary.CreateBuilder<int, InliningFunctionSpecialization.ParameterSpecialization>();

        foreach (var paramIndex in invariantIndicesSet)
        {
            var classified = InliningFunctionSpecialization.ClassifyArgument(appArgs[paramIndex].Value);

            if (classified is not null)
            {
                paramSpecs[paramIndex] = classified;
            }
            else
            {
                paramSpecs[paramIndex] =
                    new InliningFunctionSpecialization.ParameterSpecialization.ConcreteLambdaValue(
                        new SyntaxTypes.LambdaStruct(
                            Arguments: [],
                            Expression: appArgs[paramIndex]));
            }
        }

        var builtParamSpecs = paramSpecs.ToImmutable();

        // Look up the start function's specialized name from the catalog.
        var startSpecializedName =
            LookupSpecializedNameForHigherOrder(
                startImpl.Name.Value,
                startFunc.ModuleName,
                builtParamSpecs,
                context);

        if (startSpecializedName is null)
            return (new SyntaxTypes.Expression.UnitExpr(), ImmutableList<Node<SyntaxTypes.Declaration>>.Empty);

        // Build the full group: start function + group members.
        var allMembers =
            new List<(FunctionInfo Info, SyntaxTypes.FunctionImplementation Impl, string SpecializedName)>
            {
                (startFunc, startImpl, startSpecializedName)
            };

        foreach (var member in groupMembers)
        {
            var memberImpl = member.Function.Declaration.Value;

            var memberSpecName =
                LookupSpecializedNameForHigherOrder(
                    memberImpl.Name.Value,
                    member.ModuleName,
                    builtParamSpecs,
                    context);

            // If a group member's specialization wasn't collected, we can't specialize the group.
            if (memberSpecName is null)
                return (new SyntaxTypes.Expression.UnitExpr(), ImmutableList<Node<SyntaxTypes.Declaration>>.Empty);

            allMembers.Add((member, memberImpl, memberSpecName));
        }

        // Build group mapping for call rewriting.
        var groupMapping =
            allMembers
            .Select(m => (m.Impl.Name.Value, m.Info.ModuleName, m.SpecializedName))
            .ToList();

        // Generate specialized versions of each member as module-level declarations.
        foreach (var (memberInfo, memberImpl, specName) in allMembers)
        {
            // Step 1: Rewrite all intra-group calls.
            var rewrittenBody =
                RewriteGroupCallsInExpression(
                    memberImpl.Expression,
                    groupMapping,
                    invariantIndicesSet);

            // Step 2: Substitute invariant params with concrete argument values.
            var substitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();

            foreach (var paramIndex in invariantIndicesSet)
            {
                var varPattern = (SyntaxTypes.Pattern.VarPattern)memberImpl.Arguments[paramIndex].Value;
                substitutions[varPattern.Name] = appArgs[paramIndex];
            }

            var substitutedBody = SubstituteInExpression(rewrittenBody, substitutions);

            // Step 3: Qualify lifted helper references from the callee module.
            var qualifiedBody =
                QualifyLiftedHelperReferencesFromCalleeModule(
                    substitutedBody,
                    memberInfo.ModuleName,
                    context);

            // Build remaining parameter list (non-invariant params only).
            var remainingParams =
                memberImpl.Arguments
                .Where((_, i) => !invariantIndicesSet.Contains(i))
                .ToImmutableArray();

            var specializedImpl =
                new SyntaxTypes.FunctionImplementation(
                    Name: new Node<string>(s_zeroRange, specName),
                    Arguments: remainingParams,
                    Expression: qualifiedBody);

            var specializedFunc =
                new SyntaxTypes.FunctionStruct(
                    Documentation: null,
                    Signature: null,
                    Declaration: new Node<SyntaxTypes.FunctionImplementation>(s_zeroRange, specializedImpl));

            newDecls.Add(
                new Node<SyntaxTypes.Declaration>(
                    s_zeroRange,
                    new SyntaxTypes.Declaration.FunctionDeclaration(specializedFunc)));
        }

        // Build the call to the start function's specialized version with remaining (non-invariant) arguments.
        var startSpecName = allMembers[0].SpecializedName;

        var callArgs =
            new List<Node<SyntaxTypes.Expression>>
            {
                new(s_zeroRange, new SyntaxTypes.Expression.FunctionOrValue([], startSpecName))
            };

        for (var i = 0; i < appArgs.Count; i++)
        {
            if (i < startImpl.Arguments.Count && invariantIndicesSet.Contains(i))
                continue;

            callArgs.Add(appArgs[i]);
        }

        SyntaxTypes.Expression callExpr =
            callArgs.Count is 1
            ?
            callArgs[0].Value
            :
            new SyntaxTypes.Expression.Application([.. callArgs]);

        return (callExpr, newDecls.ToImmutable());
    }

    /// <summary>
    /// Rewrites all calls to members of a mutual recursion group in an expression,
    /// replacing them with calls to the specialized versions and stripping invariant arguments.
    /// </summary>
    private static Node<SyntaxTypes.Expression> RewriteGroupCallsInExpression(
        Node<SyntaxTypes.Expression> exprNode,
        IReadOnlyList<(string OriginalName, ModuleName OriginalModule, string SpecializedName)> groupMapping,
        HashSet<int> invariantParamIndices)
    {
        var expr = exprNode.Value;

        var rewrittenExpr =
            expr switch
            {
                SyntaxTypes.Expression.Application app =>
                RewriteGroupCallApplication(app, groupMapping, invariantParamIndices),

                SyntaxTypes.Expression.ParenthesizedExpression paren =>
                new SyntaxTypes.Expression.ParenthesizedExpression(
                    RewriteGroupCallsInExpression(
                        paren.Expression,
                        groupMapping,
                        invariantParamIndices)),

                SyntaxTypes.Expression.IfBlock ifBlock =>
                new SyntaxTypes.Expression.IfBlock(
                    RewriteGroupCallsInExpression(
                        ifBlock.Condition,
                        groupMapping,
                        invariantParamIndices),
                    RewriteGroupCallsInExpression(
                        ifBlock.ThenBlock,
                        groupMapping,
                        invariantParamIndices),
                    RewriteGroupCallsInExpression(
                        ifBlock.ElseBlock,
                        groupMapping,
                        invariantParamIndices)),

                SyntaxTypes.Expression.CaseExpression caseExpr =>
                new SyntaxTypes.Expression.CaseExpression(
                    new SyntaxTypes.CaseBlock(
                        RewriteGroupCallsInExpression(
                            caseExpr.CaseBlock.Expression,
                            groupMapping,
                            invariantParamIndices),
                        [
                        .. caseExpr.CaseBlock.Cases.Select(
                            c =>
                            new SyntaxTypes.Case(
                                c.Pattern,
                                RewriteGroupCallsInExpression(
                                    c.Expression,
                                    groupMapping,
                                    invariantParamIndices)))
                        ])),

                SyntaxTypes.Expression.LetExpression letExpr =>
                new SyntaxTypes.Expression.LetExpression(
                    RewriteGroupCallsInLetBlock(
                        letExpr.Value,
                        groupMapping,
                        invariantParamIndices)),

                SyntaxTypes.Expression.LambdaExpression lambda =>
                new SyntaxTypes.Expression.LambdaExpression(
                    new SyntaxTypes.LambdaStruct(
                        lambda.Lambda.Arguments,
                        RewriteGroupCallsInExpression(
                            lambda.Lambda.Expression,
                            groupMapping,
                            invariantParamIndices))),

                SyntaxTypes.Expression.ListExpr listExpr =>
                new SyntaxTypes.Expression.ListExpr(
                    [
                    .. listExpr.Elements.Select(
                        e =>
                        RewriteGroupCallsInExpression(
                            e,
                            groupMapping,
                            invariantParamIndices))
                    ]),

                SyntaxTypes.Expression.TupledExpression tupled =>
                new SyntaxTypes.Expression.TupledExpression(
                    [
                    .. tupled.Elements.Select(
                        e =>
                        RewriteGroupCallsInExpression(
                            e,
                            groupMapping,
                            invariantParamIndices))
                    ]),

                SyntaxTypes.Expression.RecordExpr recordExpr =>
                new SyntaxTypes.Expression.RecordExpr(
                    [
                    .. recordExpr.Fields.Select(
                        f =>
                        new Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>(
                            f.Range,
                            (f.Value.fieldName,
                            RewriteGroupCallsInExpression(
                                f.Value.valueExpr,
                                groupMapping,
                                invariantParamIndices))))
                    ]),

                SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [
                    .. recordUpdate.Fields.Select(
                        f =>
                        new Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>(
                            f.Range,
                            (f.Value.fieldName,
                            RewriteGroupCallsInExpression(
                                f.Value.valueExpr,
                                groupMapping,
                                invariantParamIndices))))
                    ]),

                SyntaxTypes.Expression.RecordAccess recordAccess =>
                new SyntaxTypes.Expression.RecordAccess(
                    RewriteGroupCallsInExpression(
                        recordAccess.Record,
                        groupMapping,
                        invariantParamIndices),
                    recordAccess.FieldName),

                SyntaxTypes.Expression.Negation negation =>
                new SyntaxTypes.Expression.Negation(
                    RewriteGroupCallsInExpression(
                        negation.Expression,
                        groupMapping,
                        invariantParamIndices)),

                SyntaxTypes.Expression.OperatorApplication opApp =>
                new SyntaxTypes.Expression.OperatorApplication(
                    opApp.Operator,
                    opApp.Direction,
                    RewriteGroupCallsInExpression(
                        opApp.Left,
                        groupMapping,
                        invariantParamIndices),
                    RewriteGroupCallsInExpression(
                        opApp.Right,
                        groupMapping,
                        invariantParamIndices)),

                _ =>
                expr
            };

        return new Node<SyntaxTypes.Expression>(exprNode.Range, rewrittenExpr);
    }

    private static SyntaxTypes.Expression RewriteGroupCallApplication(
        SyntaxTypes.Expression.Application app,
        IReadOnlyList<(string OriginalName, ModuleName OriginalModule, string SpecializedName)> groupMapping,
        HashSet<int> invariantParamIndices)
    {
        if (app.Arguments.Count > 0 &&
            app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue fov)
        {
            foreach (var (origName, origModule, specName) in groupMapping)
            {
                if (IsRecursiveCallReference(fov, origName, origModule))
                {
                    var newArgs =
                        new List<Node<SyntaxTypes.Expression>>
                        {
                            new(s_zeroRange, new SyntaxTypes.Expression.FunctionOrValue([], specName))
                        };

                    for (var i = 1; i < app.Arguments.Count; i++)
                    {
                        var argIndex = i - 1;

                        if (!invariantParamIndices.Contains(argIndex))
                        {
                            newArgs.Add(
                                RewriteGroupCallsInExpression(
                                    app.Arguments[i],
                                    groupMapping,
                                    invariantParamIndices));
                        }
                    }

                    return
                        newArgs.Count is 1
                        ?
                        newArgs[0].Value
                        :
                        new SyntaxTypes.Expression.Application([.. newArgs]);
                }
            }
        }

        return
            new SyntaxTypes.Expression.Application(
                [
                .. app.Arguments.Select(
                    a =>
                    RewriteGroupCallsInExpression(a, groupMapping, invariantParamIndices))
                ]);
    }

    private static SyntaxTypes.Expression.LetBlock RewriteGroupCallsInLetBlock(
        SyntaxTypes.Expression.LetBlock letBlock,
        IReadOnlyList<(string OriginalName, ModuleName OriginalModule, string SpecializedName)> groupMapping,
        HashSet<int> invariantParamIndices)
    {
        return
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                .. letBlock.Declarations.Select(
                    d =>
                    {
                        var rewrittenDecl =
                            d.Value switch
                            {
                                SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                                new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                                    letFunc.Function with
                                    {
                                        Declaration =
                                        new Node<SyntaxTypes.FunctionImplementation>(
                                            letFunc.Function.Declaration.Range,
                                            letFunc.Function.Declaration.Value with
                                            {
                                                Expression =
                                                RewriteGroupCallsInExpression(
                                                    letFunc.Function.Declaration.Value.Expression,
                                                    groupMapping,
                                                    invariantParamIndices)
                                            })
                                    }),

                                SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                                new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                                    letDestr.Pattern,
                                    RewriteGroupCallsInExpression(
                                        letDestr.Expression,
                                        groupMapping,
                                        invariantParamIndices)),

                                _ =>
                                d.Value
                            };

                        return new Node<SyntaxTypes.Expression.LetDeclaration>(d.Range, rewrittenDecl);
                    })
                ],
                Expression:
                RewriteGroupCallsInExpression(
                    letBlock.Expression,
                    groupMapping,
                    invariantParamIndices));
    }

    private static Node<SyntaxTypes.Expression> QualifyLiftedHelperReferencesFromCalleeModule(
        Node<SyntaxTypes.Expression> exprNode,
        ModuleName calleeModuleName,
        InliningContext context)
    {
        var expr = exprNode.Value;

        SyntaxTypes.Expression qualifiedExpr =
            expr switch
            {
                SyntaxTypes.Expression.FunctionOrValue funcOrValue when funcOrValue.ModuleName.Count is 0 &&
                    funcOrValue.Name.Contains("__lifted__", StringComparison.Ordinal) &&
                    context.FunctionsByQualifiedName.ContainsKey((calleeModuleName, funcOrValue.Name)) =>
                new SyntaxTypes.Expression.FunctionOrValue(calleeModuleName, funcOrValue.Name),

                SyntaxTypes.Expression.Application app =>
                new SyntaxTypes.Expression.Application(
                    [
                    .. app.Arguments.Select(
                        arg =>
                        QualifyLiftedHelperReferencesFromCalleeModule(arg, calleeModuleName, context))
                    ]),

                SyntaxTypes.Expression.ParenthesizedExpression paren =>
                new SyntaxTypes.Expression.ParenthesizedExpression(
                    QualifyLiftedHelperReferencesFromCalleeModule(paren.Expression, calleeModuleName, context)),

                SyntaxTypes.Expression.IfBlock ifBlock =>
                new SyntaxTypes.Expression.IfBlock(
                    QualifyLiftedHelperReferencesFromCalleeModule(ifBlock.Condition, calleeModuleName, context),
                    QualifyLiftedHelperReferencesFromCalleeModule(ifBlock.ThenBlock, calleeModuleName, context),
                    QualifyLiftedHelperReferencesFromCalleeModule(ifBlock.ElseBlock, calleeModuleName, context)),

                SyntaxTypes.Expression.CaseExpression caseExpr =>
                new SyntaxTypes.Expression.CaseExpression(
                    new SyntaxTypes.CaseBlock(
                        QualifyLiftedHelperReferencesFromCalleeModule(
                            caseExpr.CaseBlock.Expression,
                            calleeModuleName,
                            context),
                        [
                        .. caseExpr.CaseBlock.Cases.Select(
                            caseItem =>
                            new SyntaxTypes.Case(
                                caseItem.Pattern,
                                QualifyLiftedHelperReferencesFromCalleeModule(
                                    caseItem.Expression,
                                    calleeModuleName,
                                    context)))
                        ])),

                SyntaxTypes.Expression.LetExpression letExpr =>
                new SyntaxTypes.Expression.LetExpression(
                    new SyntaxTypes.Expression.LetBlock(
                        [
                        .. letExpr.Value.Declarations.Select(
                            decl =>
                            {
                                var qualifiedDeclaration =
                                    decl.Value switch
                                    {
                                        SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                                        new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                                            letFunc.Function with
                                            {
                                                Declaration =
                                                new Node<SyntaxTypes.FunctionImplementation>(
                                                    letFunc.Function.Declaration.Range,
                                                    letFunc.Function.Declaration.Value with
                                                    {
                                                        Expression =
                                                        QualifyLiftedHelperReferencesFromCalleeModule(
                                                            letFunc.Function.Declaration.Value.Expression,
                                                            calleeModuleName,
                                                            context)
                                                    })
                                            }),

                                        SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestructuring =>
                                        new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                                            letDestructuring.Pattern,
                                            QualifyLiftedHelperReferencesFromCalleeModule(
                                                letDestructuring.Expression,
                                                calleeModuleName,
                                                context)),

                                        _ =>
                                        decl.Value
                                    };

                                return new Node<SyntaxTypes.Expression.LetDeclaration>(decl.Range, qualifiedDeclaration);
                            })
                        ],
                        QualifyLiftedHelperReferencesFromCalleeModule(
                            letExpr.Value.Expression,
                            calleeModuleName,
                            context))),

                SyntaxTypes.Expression.LambdaExpression lambda =>
                new SyntaxTypes.Expression.LambdaExpression(
                    new SyntaxTypes.LambdaStruct(
                        lambda.Lambda.Arguments,
                        QualifyLiftedHelperReferencesFromCalleeModule(
                            lambda.Lambda.Expression,
                            calleeModuleName,
                            context))),

                SyntaxTypes.Expression.ListExpr listExpr =>
                new SyntaxTypes.Expression.ListExpr(
                    [
                    .. listExpr.Elements.Select(
                        elem =>
                        QualifyLiftedHelperReferencesFromCalleeModule(elem, calleeModuleName, context))
                    ]),

                SyntaxTypes.Expression.TupledExpression tupled =>
                new SyntaxTypes.Expression.TupledExpression(
                    [
                    .. tupled.Elements.Select(
                        elem =>
                        QualifyLiftedHelperReferencesFromCalleeModule(elem, calleeModuleName, context))
                    ]),

                SyntaxTypes.Expression.RecordExpr recordExpr =>
                new SyntaxTypes.Expression.RecordExpr(
                    [
                    .. recordExpr.Fields.Select(
                        field =>
                        new Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>(
                            field.Range,
                            (field.Value.fieldName,
                            QualifyLiftedHelperReferencesFromCalleeModule(
                                field.Value.valueExpr,
                                calleeModuleName,
                                context))))
                    ]),

                SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [
                    .. recordUpdate.Fields.Select(
                        field =>
                        new Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>(
                            field.Range,
                            (field.Value.fieldName,
                            QualifyLiftedHelperReferencesFromCalleeModule(
                                field.Value.valueExpr,
                                calleeModuleName,
                                context))))
                    ]),

                SyntaxTypes.Expression.RecordAccess recordAccess =>
                new SyntaxTypes.Expression.RecordAccess(
                    QualifyLiftedHelperReferencesFromCalleeModule(recordAccess.Record, calleeModuleName, context),
                    recordAccess.FieldName),

                SyntaxTypes.Expression.Negation negation =>
                new SyntaxTypes.Expression.Negation(
                    QualifyLiftedHelperReferencesFromCalleeModule(negation.Expression, calleeModuleName, context)),

                SyntaxTypes.Expression.OperatorApplication opApp =>
                new SyntaxTypes.Expression.OperatorApplication(
                    opApp.Operator,
                    opApp.Direction,
                    QualifyLiftedHelperReferencesFromCalleeModule(opApp.Left, calleeModuleName, context),
                    QualifyLiftedHelperReferencesFromCalleeModule(opApp.Right, calleeModuleName, context)),

                _ =>
                expr
            };

        return new Node<SyntaxTypes.Expression>(exprNode.Range, qualifiedExpr);
    }

    private static Node<SyntaxTypes.Expression> SubstituteInExpression(
        Node<SyntaxTypes.Expression> exprNode,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        var expr = exprNode.Value;

        var substitutedExpr =
            expr switch
            {
                SyntaxTypes.Expression.FunctionOrValue funcOrValue when funcOrValue.ModuleName.Count is 0 &&
                    substitutions.TryGetValue(funcOrValue.Name, out var replacement) =>
                replacement.Value,

                SyntaxTypes.Expression.Application app =>
                new SyntaxTypes.Expression.Application(
                    [.. app.Arguments.Select(a => SubstituteInExpression(a, substitutions))]),

                SyntaxTypes.Expression.ParenthesizedExpression paren =>
                new SyntaxTypes.Expression.ParenthesizedExpression(
                    SubstituteInExpression(paren.Expression, substitutions)),

                SyntaxTypes.Expression.IfBlock ifBlock =>
                new SyntaxTypes.Expression.IfBlock(
                    SubstituteInExpression(ifBlock.Condition, substitutions),
                    SubstituteInExpression(ifBlock.ThenBlock, substitutions),
                    SubstituteInExpression(ifBlock.ElseBlock, substitutions)),

                SyntaxTypes.Expression.CaseExpression caseExpr =>
                TrySubstituteSingleChoiceTagCase(caseExpr.CaseBlock, substitutions)?.Value ??
                new SyntaxTypes.Expression.CaseExpression(
                    SubstituteInCaseBlock(caseExpr.CaseBlock, substitutions)),

                SyntaxTypes.Expression.LetExpression letExpr =>
                new SyntaxTypes.Expression.LetExpression(
                    SubstituteInLetBlock(letExpr.Value, substitutions)),

                SyntaxTypes.Expression.LambdaExpression lambda =>
                new SyntaxTypes.Expression.LambdaExpression(
                    SubstituteInLambdaStruct(lambda.Lambda, substitutions)),

                SyntaxTypes.Expression.ListExpr listExpr =>
                new SyntaxTypes.Expression.ListExpr(
                    [.. listExpr.Elements.Select(e => SubstituteInExpression(e, substitutions))]),

                SyntaxTypes.Expression.TupledExpression tupled =>
                new SyntaxTypes.Expression.TupledExpression(
                    [.. tupled.Elements.Select(e => SubstituteInExpression(e, substitutions))]),

                SyntaxTypes.Expression.RecordExpr recordExpr =>
                new SyntaxTypes.Expression.RecordExpr(
                    [.. recordExpr.Fields.Select(f => SubstituteInRecordField(f, substitutions))]),

                SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [.. recordUpdate.Fields.Select(f => SubstituteInRecordField(f, substitutions))]),

                SyntaxTypes.Expression.RecordAccess recordAccess =>
                new SyntaxTypes.Expression.RecordAccess(
                    SubstituteInExpression(recordAccess.Record, substitutions),
                    recordAccess.FieldName),

                SyntaxTypes.Expression.Negation negation =>
                new SyntaxTypes.Expression.Negation(
                    SubstituteInExpression(negation.Expression, substitutions)),

                SyntaxTypes.Expression.OperatorApplication opApp =>
                new SyntaxTypes.Expression.OperatorApplication(
                    opApp.Operator,
                    opApp.Direction,
                    SubstituteInExpression(opApp.Left, substitutions),
                    SubstituteInExpression(opApp.Right, substitutions)),

                // Leaf expressions
                _ =>
                expr
            };

        return new Node<SyntaxTypes.Expression>(exprNode.Range, substitutedExpr);
    }

    private static Node<SyntaxTypes.Expression>? TrySubstituteSingleChoiceTagCase(
        SyntaxTypes.CaseBlock caseBlock,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        if (caseBlock.Cases.Count is not 1)
            return null;

        var substitutedScrutinee = SubstituteInExpression(caseBlock.Expression, substitutions);

        if (!TryDeconstructConstructorApplication(
                substitutedScrutinee,
                out var constructorName,
                out var fieldExpressions))
            return null;

        var onlyCase = caseBlock.Cases[0];

        if (!TryBindSingleChoiceTagPattern(
                onlyCase.Pattern.Value,
                constructorName,
                fieldExpressions,
                out var patternBindings))
            return null;

        var shadowedNames = CollectPatternNames(onlyCase.Pattern.Value);

        var combinedSubstitutions =
            substitutions
            .Where(kvp => !shadowedNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        foreach (var binding in patternBindings)
        {
            combinedSubstitutions[binding.Key] = binding.Value;
        }

        return SubstituteInExpression(onlyCase.Expression, combinedSubstitutions);
    }

    private static bool TryBindSingleChoiceTagPattern(
        SyntaxTypes.Pattern pattern,
        SyntaxTypes.QualifiedNameRef constructorName,
        IReadOnlyList<Node<SyntaxTypes.Expression>> fieldExpressions,
        out Dictionary<string, Node<SyntaxTypes.Expression>> bindings)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.ParenthesizedPattern parenthesizedPattern:
                return
                    TryBindSingleChoiceTagPattern(
                        parenthesizedPattern.Pattern.Value,
                        constructorName,
                        fieldExpressions,
                        out bindings);

            case SyntaxTypes.Pattern.NamedPattern namedPattern
            when AreEquivalentConstructorNames(namedPattern.Name, constructorName) &&
                     namedPattern.Arguments.Count == fieldExpressions.Count:

                bindings = new Dictionary<string, Node<SyntaxTypes.Expression>>();

                for (var i = 0; i < namedPattern.Arguments.Count; i++)
                {
                    if (!TryBindSingleChoiceTagFieldPattern(
                            namedPattern.Arguments[i].Value,
                            fieldExpressions[i],
                            bindings))
                    {
                        bindings = [];
                        return false;
                    }
                }

                return true;

            default:
                bindings = [];
                return false;
        }
    }

    private static bool TryBindSingleChoiceTagFieldPattern(
        SyntaxTypes.Pattern pattern,
        Node<SyntaxTypes.Expression> fieldExpression,
        Dictionary<string, Node<SyntaxTypes.Expression>> bindings)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPattern:
                bindings[varPattern.Name] = fieldExpression;
                return true;

            case SyntaxTypes.Pattern.AllPattern:
                return true;

            case SyntaxTypes.Pattern.ParenthesizedPattern parenthesizedPattern:
                return
                    TryBindSingleChoiceTagFieldPattern(
                        parenthesizedPattern.Pattern.Value,
                        fieldExpression,
                        bindings);

            default:
                return false;
        }
    }

    private static SyntaxTypes.CaseBlock SubstituteInCaseBlock(
        SyntaxTypes.CaseBlock caseBlock,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        return
            new SyntaxTypes.CaseBlock(
                Expression: SubstituteInExpression(caseBlock.Expression, substitutions),
                Cases: [.. caseBlock.Cases.Select(c => SubstituteInCase(c, substitutions))]);
    }

    private static SyntaxTypes.Case SubstituteInCase(
        SyntaxTypes.Case caseItem,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        // Remove substitutions shadowed by the pattern
        var shadowedNames = CollectPatternNames(caseItem.Pattern.Value);

        var filteredSubstitutions =
            substitutions
            .Where(kvp => !shadowedNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        return
            new SyntaxTypes.Case(
                Pattern: caseItem.Pattern,
                Expression: SubstituteInExpression(caseItem.Expression, filteredSubstitutions));
    }

    private static SyntaxTypes.Expression.LetBlock SubstituteInLetBlock(
        SyntaxTypes.Expression.LetBlock letBlock,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        // Collect names introduced by let declarations
        var letNames = new HashSet<string>();

        foreach (var decl in letBlock.Declarations)
        {
            if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
            {
                letNames.Add(letFunc.Function.Declaration.Value.Name.Value);
            }
            else if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
            {
                foreach (var name in CollectPatternNames(letDestr.Pattern.Value))
                {
                    letNames.Add(name);
                }
            }
        }

        // Filter substitutions for the body (names introduced by let shadow outer substitutions)
        var filteredSubstitutions =
            substitutions
            .Where(kvp => !letNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        return
            new SyntaxTypes.Expression.LetBlock(
                Declarations: [.. letBlock.Declarations.Select(d => SubstituteInLetDeclaration(d, substitutions))],
                Expression: SubstituteInExpression(letBlock.Expression, filteredSubstitutions));
    }

    private static Node<SyntaxTypes.Expression.LetDeclaration> SubstituteInLetDeclaration(
        Node<SyntaxTypes.Expression.LetDeclaration> declNode,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        var decl = declNode.Value;

        var substitutedDecl =
            decl switch
            {
                SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                    SubstituteInFunctionStruct(letFunc.Function, substitutions)),

                SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                    letDestr.Pattern,
                    SubstituteInExpression(letDestr.Expression, substitutions)),

                _ =>
                decl
            };

        return new Node<SyntaxTypes.Expression.LetDeclaration>(declNode.Range, substitutedDecl);
    }

    private static SyntaxTypes.FunctionStruct SubstituteInFunctionStruct(
        SyntaxTypes.FunctionStruct func,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        var impl = func.Declaration.Value;

        // Remove substitutions shadowed by function parameters
        var paramNames = new HashSet<string>();

        foreach (var param in impl.Arguments)
        {
            foreach (var name in CollectPatternNames(param.Value))
            {
                paramNames.Add(name);
            }
        }

        var filteredSubstitutions =
            substitutions
            .Where(kvp => !paramNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        var substitutedImpl =
            new SyntaxTypes.FunctionImplementation(
                Name: impl.Name,
                Arguments: impl.Arguments,
                Expression: SubstituteInExpression(impl.Expression, filteredSubstitutions));

        return
            func with
            {
                Declaration =
                new Node<SyntaxTypes.FunctionImplementation>(
                    func.Declaration.Range,
                    substitutedImpl)
            };
    }

    private static SyntaxTypes.LambdaStruct SubstituteInLambdaStruct(
        SyntaxTypes.LambdaStruct lambda,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        // Remove substitutions shadowed by lambda parameters
        var paramNames = new HashSet<string>();

        foreach (var param in lambda.Arguments)
        {
            foreach (var name in CollectPatternNames(param.Value))
            {
                paramNames.Add(name);
            }
        }

        var filteredSubstitutions =
            substitutions
            .Where(kvp => !paramNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        return
            new SyntaxTypes.LambdaStruct(
                Arguments: lambda.Arguments,
                Expression: SubstituteInExpression(lambda.Expression, filteredSubstitutions));
    }

    private static Node<(Node<string>, Node<SyntaxTypes.Expression>)> SubstituteInRecordField(
        Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)> fieldNode,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        var (fieldName, valueExpr) = fieldNode.Value;

        return
            new Node<(Node<string>, Node<SyntaxTypes.Expression>)>(
                fieldNode.Range,
                (fieldName, SubstituteInExpression(valueExpr, substitutions)));
    }

    private static HashSet<string> CollectPatternNames(SyntaxTypes.Pattern pattern)
    {
        var names = new HashSet<string>();

        CollectPatternNamesRecursive(pattern, names);

        return names;
    }

    private static void CollectPatternNamesRecursive(SyntaxTypes.Pattern pattern, HashSet<string> names)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPattern:
                names.Add(varPattern.Name);
                break;

            case SyntaxTypes.Pattern.TuplePattern tuplePattern:
                foreach (var elem in tuplePattern.Elements)
                {
                    CollectPatternNamesRecursive(elem.Value, names);
                }

                break;

            case SyntaxTypes.Pattern.RecordPattern recordPattern:
                foreach (var field in recordPattern.Fields)
                {
                    names.Add(field.Value);
                }

                break;

            case SyntaxTypes.Pattern.UnConsPattern unconsPattern:
                CollectPatternNamesRecursive(unconsPattern.Head.Value, names);
                CollectPatternNamesRecursive(unconsPattern.Tail.Value, names);
                break;

            case SyntaxTypes.Pattern.ListPattern listPattern:
                foreach (var elem in listPattern.Elements)
                {
                    CollectPatternNamesRecursive(elem.Value, names);
                }

                break;

            case SyntaxTypes.Pattern.NamedPattern namedPattern:
                foreach (var arg in namedPattern.Arguments)
                {
                    CollectPatternNamesRecursive(arg.Value, names);
                }

                break;

            case SyntaxTypes.Pattern.AsPattern asPattern:
                names.Add(asPattern.Name.Value);
                CollectPatternNamesRecursive(asPattern.Pattern.Value, names);
                break;

            case SyntaxTypes.Pattern.ParenthesizedPattern parenPattern:
                CollectPatternNamesRecursive(parenPattern.Pattern.Value, names);
                break;

                // Other pattern types don't introduce names
        }
    }

    private static (SyntaxTypes.CaseBlock, ImmutableList<Node<SyntaxTypes.Declaration>>) InlineCaseBlock(
        SyntaxTypes.CaseBlock caseBlock,
        InliningContext context)
    {
        var newDecls = ImmutableList.CreateBuilder<Node<SyntaxTypes.Declaration>>();

        var (inlinedExpr, exprDecls) = InlineExpression(caseBlock.Expression, context);
        newDecls.AddRange(exprDecls);

        var inlinedCases = new List<SyntaxTypes.Case>();

        foreach (var c in caseBlock.Cases)
        {
            var (inlinedCase, caseDecls) = InlineCase(c, context);
            inlinedCases.Add(inlinedCase);
            newDecls.AddRange(caseDecls);
        }

        return
            (new SyntaxTypes.CaseBlock(
                Expression: inlinedExpr,
                Cases: [.. inlinedCases]),
            newDecls.ToImmutable());
    }

    private static (SyntaxTypes.Case, ImmutableList<Node<SyntaxTypes.Declaration>>) InlineCase(
        SyntaxTypes.Case caseItem,
        InliningContext context)
    {
        var (inlinedExpr, newDecls) = InlineExpression(caseItem.Expression, context);

        return
            (new SyntaxTypes.Case(
                Pattern: caseItem.Pattern,
                Expression: inlinedExpr),
            newDecls);
    }

    /// <summary>
    /// Inlines a let expression with closure propagation (Phase 3).
    /// Zero-argument let bindings whose right-hand side resolves to a function value
    /// (lambda or known function reference) are propagated into the body expression.
    /// If all declarations are propagated, the let expression is eliminated entirely.
    /// </summary>
    private static (SyntaxTypes.Expression, ImmutableList<Node<SyntaxTypes.Declaration>>) InlineLetExpression(
        SyntaxTypes.Expression.LetExpression letExpr,
        InliningContext context)
    {
        var newDecls = ImmutableList.CreateBuilder<Node<SyntaxTypes.Declaration>>();

        // Step 1: Inline each declaration
        var inlinedDecls = new List<Node<SyntaxTypes.Expression.LetDeclaration>>();

        foreach (var d in letExpr.Value.Declarations)
        {
            var (inlinedD, dDecls) = InlineLetDeclaration(d, context);
            inlinedDecls.Add(inlinedD);
            newDecls.AddRange(dDecls);
        }

        // Step 2: Collect propagatable bindings (zero-arg let functions whose RHS is a function)
        var substitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();
        var remainingDecls = new List<Node<SyntaxTypes.Expression.LetDeclaration>>();

        foreach (var declNode in inlinedDecls)
        {
            var propagated = false;

            if (declNode.Value is SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
            {
                var impl = letFunc.Function.Declaration.Value;

                if (impl.Arguments.Count is 0)
                {
                    // Zero-arg let function: check if RHS can be resolved to a function value
                    var (resolved, resolvedDecls) = TryResolveToFunctionValue(impl.Expression, context);
                    newDecls.AddRange(resolvedDecls);

                    if (resolved is not null)
                    {
                        substitutions[impl.Name.Value] = resolved;
                        propagated = true;
                    }
                }
            }
            else if (declNode.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr &&
                UnwrapParenthesizedPattern(letDestr.Pattern.Value) is SyntaxTypes.Pattern.AllPattern)
            {
                propagated = true;
            }
            else if (declNode.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestrToPropagate &&
                TryDeconstructKnownConstructorApplication(
                    letDestrToPropagate.Expression,
                    context,
                    out var constructorName,
                    out var fieldExpressions) &&
                TryBindSingleChoiceTagPattern(
                    letDestrToPropagate.Pattern.Value,
                    constructorName,
                    fieldExpressions,
                    out var patternBindings))
            {
                foreach (var binding in patternBindings)
                {
                    substitutions[binding.Key] = binding.Value;
                }

                if (TryGetAliasNameFromPattern(letDestrToPropagate.Pattern.Value) is { } aliasName)
                {
                    substitutions[aliasName] =
                        BuildConstructorApplication(
                            constructorName,
                            fieldExpressions);
                }

                propagated = true;
            }

            if (!propagated)
                remainingDecls.Add(declNode);
        }

        // Step 3: Process body with substitutions applied
        var body = letExpr.Value.Expression;

        if (substitutions.Count > 0)
        {
            body = SubstituteInExpression(body, substitutions);
        }

        var (inlinedBody, bodyDecls) = InlineExpression(body, context);
        newDecls.AddRange(bodyDecls);

        // If all declarations were propagated, eliminate the let
        if (remainingDecls.Count is 0)
            return (inlinedBody.Value, newDecls.ToImmutable());

        return
            (new SyntaxTypes.Expression.LetExpression(
                new SyntaxTypes.Expression.LetBlock(
                    Declarations: [.. remainingDecls],
                    Expression: inlinedBody)),
            newDecls.ToImmutable());
    }

    /// <summary>
    /// Attempts to resolve an expression to a function value by trying to inline
    /// function calls that produce lambdas. Returns null if the expression cannot
    /// be resolved to a function value.
    /// </summary>
    private static (Node<SyntaxTypes.Expression>?, ImmutableList<Node<SyntaxTypes.Declaration>>) TryResolveToFunctionValue(
        Node<SyntaxTypes.Expression> exprNode,
        InliningContext context)
    {
        var expr = exprNode.Value;

        // Already a lambda
        if (expr is SyntaxTypes.Expression.LambdaExpression)
            return (exprNode, ImmutableList<Node<SyntaxTypes.Declaration>>.Empty);

        // Already a qualified function reference
        if (expr is SyntaxTypes.Expression.FunctionOrValue fov &&
            fov.ModuleName.Count > 0 &&
            context.FunctionsByQualifiedName.ContainsKey((fov.ModuleName, fov.Name)))
            return (exprNode, ImmutableList<Node<SyntaxTypes.Declaration>>.Empty);

        // Parenthesized expression
        if (expr is SyntaxTypes.Expression.ParenthesizedExpression paren)
            return TryResolveToFunctionValue(paren.Expression, context);

        // Application of a known non-recursive function: try force-inlining to see if result is a lambda
        if (expr is SyntaxTypes.Expression.Application app && app.Arguments.Count >= 2 &&
            app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue funcOrValue)
        {
            var qualifiedName = (funcOrValue.ModuleName, funcOrValue.Name);

            if (context.FunctionsByQualifiedName.TryGetValue(qualifiedName, out var funcInfo) &&
                !funcInfo.IsRecursive &&
                !context.InliningStack.Contains(qualifiedName))
            {
                var funcImpl = funcInfo.Function.Declaration.Value;
                var appArgs = app.Arguments.Skip(1).ToList();

                // Must have enough arguments to fully apply the function
                if (appArgs.Count >= funcImpl.Arguments.Count)
                {
                    var newContext =
                        context with
                        {
                            InliningStack = context.InliningStack.Add(qualifiedName)
                        };

                    var (inlinedResult, inlinedDecls) =
                        InlineFunctionCall(
                            funcInfo.ModuleName,
                            funcImpl,
                            appArgs,
                            newContext);

                    // Check if the inlined result is a function value
                    if (inlinedResult is SyntaxTypes.Expression.LambdaExpression or
                        SyntaxTypes.Expression.FunctionOrValue)
                    {
                        if (inlinedResult is SyntaxTypes.Expression.FunctionOrValue resultFov &&
                            (resultFov.ModuleName.Count is 0 ||
                            !context.FunctionsByQualifiedName.ContainsKey(
                                (resultFov.ModuleName, resultFov.Name))))
                        {
                            // Not a known function reference, discard declarations
                            return (null, []);
                        }

                        return (new Node<SyntaxTypes.Expression>(s_zeroRange, inlinedResult), inlinedDecls);
                    }
                }
            }
        }

        return (null, []);
    }

    private static (Node<SyntaxTypes.Expression>?, ImmutableList<Node<SyntaxTypes.Declaration>>) TryResolveToRecordValue(
        Node<SyntaxTypes.Expression> exprNode,
        InliningContext context)
    {
        var expr = exprNode.Value;

        if (expr is SyntaxTypes.Expression.RecordExpr)
            return (exprNode, []);

        if (expr is SyntaxTypes.Expression.ParenthesizedExpression paren)
            return TryResolveToRecordValue(paren.Expression, context);

        if (expr is SyntaxTypes.Expression.FunctionOrValue funcOrValue &&
            TryResolveKnownFunctionReference(funcOrValue, context, out var qualifiedName, out var funcInfo) &&
            !funcInfo.IsRecursive &&
            !context.InliningStack.Contains(qualifiedName))
        {
            var funcImpl = funcInfo.Function.Declaration.Value;

            if (funcImpl.Arguments.Count is 0)
            {
                var newContext =
                    context with
                    {
                        InliningStack = context.InliningStack.Add(qualifiedName)
                    };

                var (inlinedResult, inlinedDecls) =
                    InlineFunctionCall(
                        funcInfo.ModuleName,
                        funcImpl,
                        [],
                        newContext);

                if (inlinedResult is SyntaxTypes.Expression.RecordExpr)
                {
                    return (new Node<SyntaxTypes.Expression>(s_zeroRange, inlinedResult), inlinedDecls);
                }
            }
        }

        return (null, []);
    }

    private static bool TryResolveKnownFunctionReference(
        SyntaxTypes.Expression.FunctionOrValue funcOrValue,
        InliningContext context,
        out (ModuleName ModuleName, string FunctionName) qualifiedName,
        out FunctionInfo funcInfo)
    {
        if (funcOrValue.ModuleName.Count > 0)
        {
            qualifiedName = (funcOrValue.ModuleName, funcOrValue.Name);
            return context.FunctionsByQualifiedName.TryGetValue(qualifiedName, out funcInfo!);
        }

        if (context.CurrentModuleName is { } currentModuleName)
        {
            qualifiedName = (currentModuleName, funcOrValue.Name);
            return context.FunctionsByQualifiedName.TryGetValue(qualifiedName, out funcInfo!);
        }

        qualifiedName = default;
        funcInfo = null!;
        return false;
    }

    private static (Node<SyntaxTypes.Expression.LetDeclaration>, ImmutableList<Node<SyntaxTypes.Declaration>>) InlineLetDeclaration(
        Node<SyntaxTypes.Expression.LetDeclaration> declNode,
        InliningContext context)
    {
        var decl = declNode.Value;

        switch (decl)
        {
            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                {
                    var (inlinedFunc, funcDecls) = InlineFunctionStruct(letFunc.Function, context);
                    var inlinedDecl = new SyntaxTypes.Expression.LetDeclaration.LetFunction(inlinedFunc);
                    return (new Node<SyntaxTypes.Expression.LetDeclaration>(declNode.Range, inlinedDecl), funcDecls);
                }

            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                {
                    var (inlinedExpr, exprDecls) = InlineExpression(letDestr.Expression, context);

                    var inlinedDecl =
                        new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                            letDestr.Pattern,
                            inlinedExpr);

                    return (new Node<SyntaxTypes.Expression.LetDeclaration>(declNode.Range, inlinedDecl), exprDecls);
                }

            default:
                return (declNode, ImmutableList<Node<SyntaxTypes.Declaration>>.Empty);
        }
    }

    private static (SyntaxTypes.LambdaStruct, ImmutableList<Node<SyntaxTypes.Declaration>>) InlineLambdaStruct(
        SyntaxTypes.LambdaStruct lambda,
        InliningContext context)
    {
        var (inlinedExpr, newDecls) = InlineExpression(lambda.Expression, context);

        return
            (new SyntaxTypes.LambdaStruct(
                Arguments: lambda.Arguments,
                Expression: inlinedExpr),
            newDecls);
    }

    private static (Node<(Node<string>, Node<SyntaxTypes.Expression>)>, ImmutableList<Node<SyntaxTypes.Declaration>>) InlineRecordField(
        Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)> fieldNode,
        InliningContext context)
    {
        var (fieldName, valueExpr) = fieldNode.Value;
        var (inlinedExpr, newDecls) = InlineExpression(valueExpr, context);

        return
            (new Node<(Node<string>, Node<SyntaxTypes.Expression>)>(
                fieldNode.Range,
                (fieldName, inlinedExpr)),
            newDecls);
    }

    /// <summary>
    /// Comparer for module name tuples that handles collection equality properly.
    /// </summary>
    private sealed class ModuleNameTupleComparer : IEqualityComparer<(ModuleName ModuleName, string FunctionName)>
    {
        public bool Equals(
            (ModuleName ModuleName, string FunctionName) x,
            (ModuleName ModuleName, string FunctionName) y)
        {
            return
                x.FunctionName == y.FunctionName &&
                x.ModuleName.SequenceEqual(y.ModuleName);
        }

        public int GetHashCode((ModuleName ModuleName, string FunctionName) obj)
        {
            var hash = new HashCode();

            foreach (var part in obj.ModuleName)
            {
                hash.Add(part);
            }

            hash.Add(obj.FunctionName);

            return hash.ToHashCode();
        }
    }
}
