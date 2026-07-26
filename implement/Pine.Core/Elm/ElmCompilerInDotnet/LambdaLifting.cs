using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Provides lambda lifting services for Elm modules.
/// Lambda lifting transforms closures into top-level functions by making captured variables explicit parameters.
///
/// Design:
/// - Naming convention: containingFunction__lifted__lambdaIdentifier
/// - Zero captured bindings: no extra first parameter (lifted function has same signature as original lambda)
/// - Single captured binding: plain parameter (e.g., `f`)
/// - Multiple captured bindings: tuple parameter, ordered alphabetically (e.g., `( a, b, c )`)
/// - Lifted functions appear AFTER the originating function declaration
///
/// <para>
/// This pass operates entirely on the abstract Elm syntax model
/// (<see cref="ElmSyntax.ElmSyntaxAbstract"/>). Structural fingerprinting
/// (<see cref="DeclarationDeduplication"/>) and the post-lifting invariant
/// check (<see cref="LambdaLiftingValidator"/>) are invoked through their
/// abstract-model overloads, so no concrete syntax model appears here.
/// </para>
/// </summary>
public static class LambdaLifting
{
    /// <summary>
    /// Performs lambda lifting on the given Elm module.
    /// Transforms closures into top-level functions with explicit captured parameters.
    /// </summary>
    /// <param name="module">The Elm module to transform.</param>
    /// <returns>The transformed module with lifted lambdas as top-level functions.</returns>
    public static SyntaxTypes.File LiftLambdas(SyntaxTypes.File module)
    {
        var newDeclarations = new List<SyntaxTypes.Declaration>();

        var liftedFunctionNames = new HashSet<string>(StringComparer.Ordinal);

        var nextLiftedIdentifierByFunctionName =
            BuildNextLiftedIdentifierByFunctionName(module.Declarations);

        foreach (var declaration in module.Declarations)
        {
            if (declaration is SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                var (transformedDecl, liftedFunctions) =
                    LiftLambdasInFunction(funcDecl, nextLiftedIdentifierByFunctionName);

                newDeclarations.Add(transformedDecl);

                // Add lifted functions AFTER the originating function
                foreach (var liftedFunc in liftedFunctions)
                {
                    newDeclarations.Add(liftedFunc);

                    if (liftedFunc is SyntaxTypes.Declaration.FunctionDeclaration liftedFuncDecl)
                    {
                        liftedFunctionNames.Add(
                            liftedFuncDecl.Function.Declaration.Name);
                    }
                }
            }
            else
            {
                // Keep non-function declarations as-is
                newDeclarations.Add(declaration);
            }
        }

        var newFile = module with { Declarations = newDeclarations };

        LambdaLiftingValidator.Validate(
            newFile,
            liftedFunctionNames);

        return newFile;
    }

    /// <summary>
    /// <see cref="OptimizedElmSyntaxDeclarations"/>-flavoured overload of
    /// <see cref="LiftLambdas(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration})"/>.
    /// Renders the structured input to a flat dictionary, runs the lift, and
    /// re-buckets the result via
    /// <see cref="OptimizedElmSyntaxDeclarations.FromFlatDictionary"/>.
    /// </summary>
    public static OptimizedElmSyntaxDeclarations LiftLambdas(
        OptimizedElmSyntaxDeclarations declarations) =>
        OptimizedElmSyntaxDeclarations.FromFlatDictionary(
            LiftLambdas(declarations.RenderAsFlatDictionary()));

    /// <summary>
    /// Performs lambda lifting on a flat declaration dictionary.
    /// Transforms closures into top-level functions with explicit captured parameters.
    /// Declarations are processed in deterministic order (sorted by <see cref="DeclQualifiedName"/>).
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> LiftLambdas(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var nextLiftedIdentifierByFunctionName =
            BuildNextLiftedIdentifierByFunctionName(declarations.Values);

        var resultBuilder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        // Track which lifted function names were newly created, per module namespace.
        var newLiftedNamesByModule =
            new Dictionary<IReadOnlyList<string>, HashSet<string>>(
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        // Per-module shared fingerprint maps used by LiftLambda to
        // detect structurally-equivalent lifted decls already present
        // in the input (or freshly emitted earlier during this pass)
        // and reuse their names instead of emitting duplicates.
        // Seeded below from input function declarations.
        var existingFingerprintByModule =
            new Dictionary<IReadOnlyList<string>, Dictionary<string, DeclQualifiedName>>(
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        // Iterate in deterministic order so that "first occurrence wins"
        // below picks the same representative across runs. The raw
        // ImmutableDictionary enumeration order is hash-bucket order,
        // which is not stable across processes; without the explicit
        // sort, structurally-equivalent decls from different host
        // declarations would non-deterministically swap which one gets
        // to keep its original name (e.g.
        // sequenceEndForbidden__lifted__lambda2 ↔
        // sequenceEndMandatory__stripped__lifted__lambda2). See
        // explore/internal-analysis/2026-05-18-non-deterministic-ordering-in-optimization-pipeline.md.
        foreach (var (key, decl) in declarations.OrderBy(kvp => kvp.Key))
        {
            if (decl is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                continue;

            if (!existingFingerprintByModule.TryGetValue(key.Namespaces, out var mapForModule))
            {
                mapForModule = new Dictionary<string, DeclQualifiedName>(StringComparer.Ordinal);
                existingFingerprintByModule[key.Namespaces] = mapForModule;
            }

            var fp =
                DeclarationDeduplication.GetStructuralFingerprint(
                    funcDecl,
                    key.Namespaces);

            // First occurrence of a fingerprint wins. With the
            // OrderBy(kvp => kvp.Key) above, this matches the
            // lex-by-qualified-name order LiftLambdasInFunction uses
            // to drive emission a few lines down.
            mapForModule.TryAdd(fp, key);
        }

        // Process declarations in deterministic order for stable lifted-function naming.
        foreach (var (key, decl) in declarations.OrderBy(kvp => kvp.Key))
        {
            if (decl is SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                if (!existingFingerprintByModule.TryGetValue(key.Namespaces, out var mapForModule))
                {
                    mapForModule = new Dictionary<string, DeclQualifiedName>(StringComparer.Ordinal);
                    existingFingerprintByModule[key.Namespaces] = mapForModule;
                }

                var (transformedDecl, liftedFunctions) =
                    LiftLambdasInFunction(
                        funcDecl,
                        nextLiftedIdentifierByFunctionName,
                        existingFingerprintToName: mapForModule,
                        moduleNamespaces: key.Namespaces);

                resultBuilder[key] = transformedDecl;

                // Add lifted functions keyed by their qualified name in the same module namespace.
                foreach (var liftedFunc in liftedFunctions)
                {
                    var liftedDeclName = ElmCompiler.GetDeclarationName(liftedFunc);

                    if (liftedDeclName is not null)
                    {
                        var liftedKey = DeclQualifiedName.Create(key.Namespaces, liftedDeclName);
                        resultBuilder[liftedKey] = liftedFunc;

                        if (!newLiftedNamesByModule.TryGetValue(key.Namespaces, out var moduleSet))
                        {
                            moduleSet = [];
                            newLiftedNamesByModule[key.Namespaces] = moduleSet;
                        }

                        moduleSet.Add(liftedDeclName);
                    }
                }
            }
            else
            {
                // Keep non-function declarations as-is
                resultBuilder[key] = decl;
            }
        }

        // Post-process: qualify unqualified references to newly-lifted functions.
        // Lambda lifting creates local (unqualified) references to lifted functions.
        // In the flat-dict representation these must be fully qualified with the module name.
        if (newLiftedNamesByModule.Count > 0)
        {
            foreach (var (key, decl) in resultBuilder.ToArray())
            {
                if (decl is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                    continue;

                if (!newLiftedNamesByModule.TryGetValue(key.Namespaces, out var liftedNamesInModule))
                    continue;

                var impl = funcDecl.Function.Declaration;

                var qualifiedExpr =
                    QualifyLiftedReferences(
                        impl.Expression,
                        key.Namespaces,
                        liftedNamesInModule.Contains);

                if (qualifiedExpr.Equals(impl.Expression))
                    continue;

                var qualifiedImpl = impl with { Expression = qualifiedExpr };

                var qualifiedFunc =
                    funcDecl.Function with { Declaration = qualifiedImpl };

                resultBuilder[key] = new SyntaxTypes.Declaration.FunctionDeclaration(qualifiedFunc);
            }
        }

        var result = resultBuilder.ToImmutable();

        // Validate each module's post-lifting slice against the lambda-lifting
        // invariants, restricting the check to the lifter-created top-level
        // functions in that module (so that user-defined top-level functions
        // are not subject to the invariant).
        foreach (var (moduleName, liftedNamesInModule) in newLiftedNamesByModule)
        {
            if (liftedNamesInModule.Count is 0)
            {
                continue;
            }

            var moduleNameComparer =
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>();

            var moduleDeclarations =
                result
                .Where(kvp => moduleNameComparer.Equals(kvp.Key.Namespaces, moduleName))
                .Select(kvp => kvp.Value);

            LambdaLiftingValidator.Validate(moduleDeclarations, liftedNamesInModule);
        }

        return result;
    }

    /// <summary>
    /// Rewrites unqualified references (empty namespace) to newly-lifted
    /// function names so they are fully qualified with the enclosing module
    /// namespace. This mirrors the concrete-model <c>ReferenceQualifier</c>
    /// used previously, restricted to the "no local-scope tracking" mode:
    /// any unqualified reference whose name is in
    /// <paramref name="isLiftedName"/> is qualified. Lifted function names
    /// are generated and never shadowed by locals, so scope tracking is
    /// unnecessary.
    /// </summary>
    private static SyntaxTypes.Expression QualifyLiftedReferences(
        SyntaxTypes.Expression expr,
        IReadOnlyList<string> moduleNamespaces,
        Func<string, bool> isLiftedName)
    {
        if (expr is SyntaxTypes.Expression.Identifier funcOrVal &&
            funcOrVal.QualifiedName.Namespaces.Count is 0 &&
            isLiftedName(funcOrVal.QualifiedName.DeclName))
        {
            return
                new SyntaxTypes.Expression.Identifier(
                    DeclQualifiedName.Create(moduleNamespaces, funcOrVal.QualifiedName.DeclName));
        }

        return
            ElmSyntaxAbstractTransformations.MapChildExpressions(
                expr,
                child => QualifyLiftedReferences(child, moduleNamespaces, isLiftedName));
    }

    /// <summary>
    /// Transforms a single function declaration by lifting its lambdas.
    /// Returns the transformed declaration and a list of newly-created lifted function declarations.
    /// </summary>
    private static (
        SyntaxTypes.Declaration.FunctionDeclaration TransformedDecl,
        IReadOnlyList<SyntaxTypes.Declaration> LiftedFunctions)
        LiftLambdasInFunction(
        SyntaxTypes.Declaration.FunctionDeclaration funcDecl,
        IReadOnlyDictionary<string, int> nextLiftedIdentifierByFunctionName,
        Dictionary<string, DeclQualifiedName>? existingFingerprintToName = null,
        IReadOnlyList<string>? moduleNamespaces = null)
    {
        var functionName = funcDecl.Function.Declaration.Name;

        var context =
            new LiftingContext(
                functionName,
                BoundVariables: [],
                LambdaCounter:
                nextLiftedIdentifierByFunctionName.TryGetValue(functionName, out var nextIdentifier)
                ?
                nextIdentifier - 1
                :
                0,
                ExistingFingerprintToName: existingFingerprintToName,
                ModuleNamespaces: moduleNamespaces);

        // Collect parameter names as bound variables
        var paramNames = CollectPatternNames(funcDecl.Function.Declaration.Arguments);

        context = context.WithBoundVariables(paramNames);

        // Transform the function body
        var (transformedExpr, liftedFunctions) =
            TransformExpression(
                funcDecl.Function.Declaration.Expression,
                context);

        // Create the transformed function declaration
        var transformedFuncImpl =
            funcDecl.Function.Declaration with
            {
                Expression = transformedExpr
            };

        var transformedFunc =
            funcDecl.Function with { Declaration = transformedFuncImpl };

        return (new SyntaxTypes.Declaration.FunctionDeclaration(transformedFunc), liftedFunctions);
    }

    private static IReadOnlyDictionary<string, int> BuildNextLiftedIdentifierByFunctionName(
        IEnumerable<SyntaxTypes.Declaration> declarations)
    {
        var nextLiftedIdentifierByFunctionName = new Dictionary<string, int>(StringComparer.Ordinal);

        foreach (var decl in declarations)
        {
            if (decl is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                continue;
            }

            var functionName = funcDecl.Function.Declaration.Name;

            if (TryParseExistingLiftedIdentifier(functionName) is not { } liftedIdentifier)
            {
                continue;
            }

            var nextId = liftedIdentifier.identifier + 1;

            if (nextLiftedIdentifierByFunctionName.TryGetValue(
                liftedIdentifier.containingFunctionName,
                out var existingNextIdentifier))
            {
                nextLiftedIdentifierByFunctionName[liftedIdentifier.containingFunctionName] =
                    Math.Max(existingNextIdentifier, nextId);
            }
            else
            {
                nextLiftedIdentifierByFunctionName[liftedIdentifier.containingFunctionName] =
                    nextId;
            }
        }

        return nextLiftedIdentifierByFunctionName;
    }

    /// <summary>
    /// Inverse of the lifted-name producers in
    /// <c>LiftLambda</c> / <c>TransformLetExpression</c>:
    /// given a top-level function name, attempts to recover the
    /// <c>(containingFunctionName, identifier)</c> pair that produced
    /// it.
    /// <para>
    /// Used by <see cref="BuildNextLiftedIdentifierByFunctionName"/>
    /// to re-derive the next-available lambda counter for each
    /// containing function so that a re-invocation of
    /// <see cref="LiftLambdas(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration})"/>
    /// on a dictionary that already contains previously-lifted decls
    /// does not collide with their suffixes.
    /// </para>
    /// <para>
    /// All suffix literals are sourced from
    /// <see cref="GeneratedNameSuffixes"/> (cf.
    /// <c>2026-05-18-eliminate-higher-order-parameters-in-focused-tests.md</c>
    /// §11.6 / §11.7) so future suffix changes need only touch one
    /// place.
    /// </para>
    /// <para>
    /// Future work (§11.7): replace this string-parsing approach by
    /// threading the per-containing-function counter through the
    /// pipeline's iteration state so the producer no longer needs to
    /// re-derive it on every <see cref="LiftLambdas"/> invocation.
    /// </para>
    /// </summary>
    private static (string containingFunctionName, int identifier)? TryParseExistingLiftedIdentifier(
        string functionName)
    {
        var liftedMarkerIndex =
            functionName.IndexOf(GeneratedNameSuffixes.Lifted, StringComparison.Ordinal);

        if (liftedMarkerIndex < 0)
        {
            return null;
        }

        var containingFunctionName = functionName[..liftedMarkerIndex];
        var suffix = functionName[(liftedMarkerIndex + GeneratedNameSuffixes.Lifted.Length)..];

        if (suffix.StartsWith(GeneratedNameSuffixes.LiftedLambdaPrefix, StringComparison.Ordinal) &&
            int.TryParse(suffix[GeneratedNameSuffixes.LiftedLambdaPrefix.Length..], out var lambdaIdentifier))
        {
            return (containingFunctionName, lambdaIdentifier);
        }

        var lastUnderscoreIndex = suffix.LastIndexOf('_');

        if (lastUnderscoreIndex < 0)
        {
            return null;
        }

        return
            int.TryParse(suffix[(lastUnderscoreIndex + 1)..], out var helperIdentifier)
            ?
            (containingFunctionName, helperIdentifier)
            :
            null;
    }

    /// <summary>
    /// Context for lambda lifting, tracking the containing function name, bound variables, and lambda counter.
    ///
    /// <para>
    /// <see cref="ExistingFingerprintToName"/> is a mutable, shared lookup from
    /// the structural fingerprint of a previously-emitted top-level
    /// declaration (existing input or freshly lifted during the current
    /// pass) to its qualified name. Producers consult this map before
    /// adding a new lifted decl: when a structurally-equivalent decl
    /// already exists, the producer reuses the existing name and
    /// suppresses the new emission, avoiding accumulating
    /// <c>__lifted__lambdaN</c> duplicates across repeated lambda-lifting
    /// invocations. Passing <c>null</c> disables the check (used by the
    /// per-module overload that has no access to the surrounding flat
    /// dictionary).
    /// </para>
    /// </summary>
    private record LiftingContext(
        string ContainingFunctionName,
        ImmutableHashSet<string> BoundVariables,
        int LambdaCounter = 0,
        Dictionary<string, DeclQualifiedName>? ExistingFingerprintToName = null,
        IReadOnlyList<string>? ModuleNamespaces = null)
    {
        public LiftingContext(string containingFunctionName)
            : this(containingFunctionName, [], 0)
        {
        }

        public LiftingContext WithBoundVariables(IEnumerable<string> variables) =>
            this with { BoundVariables = BoundVariables.Union(variables) };

        public LiftingContext WithBoundVariable(string variable) =>
            this with { BoundVariables = BoundVariables.Add(variable) };

        public (LiftingContext, int) NextLambdaId()
        {
            var nextId = LambdaCounter + 1;
            return (this with { LambdaCounter = nextId }, nextId);
        }
    }

    /// <summary>
    /// Transforms an expression, lifting any lambdas found within it.
    /// </summary>
    private static (SyntaxTypes.Expression, IReadOnlyList<SyntaxTypes.Declaration>) TransformExpression(
        SyntaxTypes.Expression expr,
        LiftingContext context)
    {
        var liftedFunctions = new List<SyntaxTypes.Declaration>();

        var (transformedExpr, _) = TransformExpressionInner(expr, context, liftedFunctions);

        return (transformedExpr, liftedFunctions);
    }

    private static (SyntaxTypes.Expression, LiftingContext) TransformExpressionInner(
        SyntaxTypes.Expression expr,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.LambdaExpression lambdaExpr:
                return LiftLambda(lambdaExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.LetExpression letExpr:
                return TransformLetExpression(letExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.Application appExpr:
                return TransformApplication(appExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.OperatorApplication opApp:
                return TransformOperatorApplication(opApp, context, liftedFunctions);

            case SyntaxTypes.Expression.IfBlock ifBlock:
                return TransformIfBlock(ifBlock, context, liftedFunctions);

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                return TransformCaseExpression(caseExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.TupledExpression tupled:
                return TransformTupledExpression(tupled, context, liftedFunctions);

            case SyntaxTypes.Expression.ListExpr listExpr:
                return TransformListExpression(listExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                return TransformRecordExpression(recordExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                return TransformRecordAccess(recordAccess, context, liftedFunctions);

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                return TransformRecordUpdateExpression(recordUpdate, context, liftedFunctions);

            case SyntaxTypes.Expression.Negation negation:
                return TransformNegation(negation, context, liftedFunctions);

            // Leaf expressions - no transformation needed
            case SyntaxTypes.Expression.Identifier:
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.GLSLExpression:
                return (expr, context);

            default:
                throw new NotImplementedException(
                    $"Lambda lifting not implemented for expression type: {expr.GetType().Name}");
        }
    }

    private static (SyntaxTypes.Expression, LiftingContext) LiftLambda(
        SyntaxTypes.Expression.LambdaExpression lambdaExpr,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        // Get lambda parameter names
        var lambdaParamNames = CollectPatternNames(lambdaExpr.Arguments);

        // Find free variables in the lambda body (variables used but not bound by lambda params)
        var freeVariables =
            FindFreeVariables(lambdaExpr.Expression, [.. lambdaParamNames])
            .Where(context.BoundVariables.Contains)
            .OrderBy(v => v)
            .ToList();

        // Generate provisional name for the lifted function. The lambda
        // counter is advanced unconditionally so that ID sequences
        // remain monotonic across the pass even when an already-emitted
        // sibling is reused below (we trade a small amount of ID skip
        // for fingerprint-equivalent suppression of duplicates).
        var (newContext, lambdaId) = context.NextLambdaId();

        var liftedFunctionName =
            $"{context.ContainingFunctionName}{GeneratedNameSuffixes.Lifted}{GeneratedNameSuffixes.LiftedLambdaPrefix}{lambdaId}";

        // Transform the lambda body with updated context
        var lambdaBodyContext = newContext.WithBoundVariables(lambdaParamNames);

        var (transformedBody, finalContext) =
            TransformExpressionInner(lambdaExpr.Expression, lambdaBodyContext, liftedFunctions);

        // Create the proposed lifted function
        var liftedFuncDecl =
            CreateLiftedFunction(
                liftedFunctionName,
                freeVariables,
                lambdaExpr.Arguments,
                transformedBody);

        // Check whether a structurally-equivalent top-level declaration
        // already exists (either in the input declaration dictionary or
        // emitted earlier during this same lambda-lifting pass). If so,
        // reuse the existing name rather than emit a fresh
        // <c>__lifted__lambdaN</c> duplicate. This removes the need for
        // a downstream <see cref="DeclarationDeduplication"/> pass to
        // collapse the redundant siblings produced by repeated lift
        // invocations (e.g. when size-based inlining re-inlines a
        // previously-lifted wrapper and the next lift round would
        // otherwise emit a structurally-identical sibling).
        var existingFingerprintMap = finalContext.ExistingFingerprintToName;
        var moduleNamespaces = finalContext.ModuleNamespaces;

        var reuseName = (string?)null;

        if (existingFingerprintMap is not null &&
            moduleNamespaces is not null &&
            liftedFuncDecl is SyntaxTypes.Declaration.FunctionDeclaration proposedFuncDecl)
        {
            var fingerprint =
                DeclarationDeduplication.GetStructuralFingerprint(
                    proposedFuncDecl,
                    moduleNamespaces);

            if (existingFingerprintMap.TryGetValue(fingerprint, out var existingName) &&
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>()
                .Equals(existingName.Namespaces, moduleNamespaces))
            {
                reuseName = existingName.DeclName;
            }
            else
            {
                // Record this newly-emitted lifted decl in the
                // fingerprint map so subsequent lifts (within the same
                // pass) of structurally-equivalent lambdas reuse it.
                existingFingerprintMap[fingerprint] =
                    DeclQualifiedName.Create(moduleNamespaces, liftedFunctionName);
            }
        }

        var effectiveLiftedName = reuseName ?? liftedFunctionName;

        if (reuseName is null)
        {
            liftedFunctions.Add(liftedFuncDecl);
        }

        // Create the replacement expression (reference to lifted function with captured args).
        // When reusing an existing decl, emit a fully-qualified reference directly because the
        // post-pass that qualifies unqualified lifted-references only covers names in
        // newLiftedNamesByModule (i.e. fresh emissions). Reused names already-existed in the
        // input dictionary and would otherwise stay unqualified.
        var replacementExpr =
            reuseName is not null && moduleNamespaces is not null
            ?
            CreateLiftedFunctionCall(
                effectiveLiftedName,
                freeVariables,
                moduleNamespaces)
            :
            CreateLiftedFunctionCall(
                effectiveLiftedName,
                freeVariables);

        return (replacementExpr, finalContext);
    }

    private static (SyntaxTypes.Expression, LiftingContext) TransformLetExpression(
        SyntaxTypes.Expression.LetExpression letExpr,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        var newDeclarations = new List<SyntaxTypes.LetDeclaration>();
        var currentContext = context;

        // First pass: collect all names bound by let declarations
        var letBoundNames = new List<string>();

        foreach (var decl in letExpr.Declarations)
        {
            switch (decl)
            {
                case SyntaxTypes.LetDeclaration.LetFunction letFunc:
                    letBoundNames.Add(letFunc.Function.Declaration.Name);
                    break;

                case SyntaxTypes.LetDeclaration.LetDestructuring letDestr:
                    letBoundNames.AddRange(CollectPatternNames([letDestr.Pattern]));
                    break;
            }
        }

        // Add let-bound names to context
        currentContext = currentContext.WithBoundVariables(letBoundNames);

        // Collect local functions that will be lifted (functions with parameters or lambda assignments)
        // and build a mapping from their local names to their lifted names
        var localFunctionLiftedNames = new Dictionary<string, string>();

        foreach (var decl in letExpr.Declarations)
        {
            if (decl is SyntaxTypes.LetDeclaration.LetFunction letFunc)
            {
                var bindingName = letFunc.Function.Declaration.Name;
                var funcExpr = letFunc.Function.Declaration.Expression;

                // Check if this is a lambda assignment or a local function with parameters
                var isLambdaAssignment =
                    funcExpr is SyntaxTypes.Expression.LambdaExpression &&
                    letFunc.Function.Declaration.Arguments.Count is 0;

                var isLocalFunctionWithParams = letFunc.Function.Declaration.Arguments.Count > 0;

                if (isLambdaAssignment || isLocalFunctionWithParams)
                {
                    var (updatedCtx, uniqueId) = currentContext.NextLambdaId();
                    currentContext = updatedCtx;

                    var liftedFunctionName =
                        $"{context.ContainingFunctionName}{GeneratedNameSuffixes.Lifted}{bindingName}_{uniqueId}";

                    localFunctionLiftedNames[bindingName] = liftedFunctionName;
                }
            }
        }

        // Precompute which sibling lifted-functions have ZERO external
        // captures (i.e. their free variables are all either function
        // parameters or other sibling lifted-functions in this same set).
        //
        // A sibling reference may be "substituted" — replaced with the
        // bare lifted-function name — only if that sibling has zero
        // external captures, so that the substituted call site does not
        // need to supply the missing captured arguments. Sibling
        // references that themselves capture external bindings must
        // instead be CAPTURED (i.e. flow through the let binding which
        // holds the partial application <c>lifted__sibling capturedArgs</c>),
        // otherwise the surrounding lifted function would call the
        // sibling with the wrong first argument and silently corrupt the
        // result.
        //
        // This is computed as a GREATEST fixed point so that mutually
        // recursive sibling functions (which reference each other but
        // have no other external captures) are all classified as
        // substitutable together.
        var siblingsWithoutExternalCaptures =
            new HashSet<string>(localFunctionLiftedNames.Keys, StringComparer.Ordinal);

        bool anyRemoved;

        do
        {
            anyRemoved = false;

            foreach (var decl in letExpr.Declarations)
            {
                if (decl is not SyntaxTypes.LetDeclaration.LetFunction letFuncCheck)
                {
                    continue;
                }

                var siblingName = letFuncCheck.Function.Declaration.Name;

                if (!siblingsWithoutExternalCaptures.Contains(siblingName))
                {
                    continue;
                }

                SyntaxTypes.Expression bodyExpr;
                IReadOnlyList<string> paramNames;

                var args = letFuncCheck.Function.Declaration.Arguments;
                var bodyExprValue = letFuncCheck.Function.Declaration.Expression;

                if (args.Count is 0 &&
                    bodyExprValue is SyntaxTypes.Expression.LambdaExpression innerLambdaCheck)
                {
                    bodyExpr = innerLambdaCheck.Expression;
                    paramNames = CollectPatternNames(innerLambdaCheck.Arguments);
                }
                else
                {
                    bodyExpr = bodyExprValue;
                    paramNames = CollectPatternNames(args);
                }

                var hasExternalCapture =
                    FindFreeVariables(bodyExpr, [.. paramNames])
                    .Any(
                        v =>
                        currentContext.BoundVariables.Contains(v) &&
                        v != siblingName &&
                        !siblingsWithoutExternalCaptures.Contains(v));

                if (hasExternalCapture)
                {
                    siblingsWithoutExternalCaptures.Remove(siblingName);
                    anyRemoved = true;
                }
            }
        }
        while (anyRemoved);

        // Second pass: transform each declaration
        foreach (var decl in letExpr.Declarations)
        {
            switch (decl)
            {
                case SyntaxTypes.LetDeclaration.LetFunction letFunc:
                    {
                        var funcParamNames = CollectPatternNames(letFunc.Function.Declaration.Arguments);
                        var funcContext = currentContext.WithBoundVariables(funcParamNames);
                        var funcExpr = letFunc.Function.Declaration.Expression;

                        // Check if this is a lambda assignment (e.g., filterFn = \x -> ...)
                        if (funcExpr is SyntaxTypes.Expression.LambdaExpression innerLambda &&
                            letFunc.Function.Declaration.Arguments.Count is 0)
                        {
                            // This is a named lambda - lift it with the let-binding name
                            var (transformedLetDecl, updatedContext) =
                                LiftNamedLambda(
                                    letFunc,
                                    innerLambda,
                                    currentContext,
                                    liftedFunctions,
                                    localFunctionLiftedNames,
                                    siblingsWithoutExternalCaptures);

                            newDeclarations.Add(transformedLetDecl);
                            currentContext = updatedContext;
                        }
                        // Check if this is a local function with parameters (e.g., factorial x = ...)
                        else if (letFunc.Function.Declaration.Arguments.Count > 0)
                        {
                            // Lift local function with parameters
                            var (transformedLetDecl, updatedContext) =
                                LiftLocalFunction(
                                    letFunc,
                                    currentContext,
                                    liftedFunctions,
                                    localFunctionLiftedNames,
                                    siblingsWithoutExternalCaptures);

                            newDeclarations.Add(transformedLetDecl);
                            currentContext = updatedContext;
                        }
                        else
                        {
                            var (transformedExpr, updatedCtx) =
                                TransformExpressionInner(
                                    funcExpr,
                                    funcContext,
                                    liftedFunctions);

                            // Propagate the lambda counter advance from inside this
                            // let-binding's value to the outer scope so that sibling
                            // declarations and the let body do not reuse lambda IDs
                            // already consumed here.
                            currentContext =
                                currentContext with { LambdaCounter = updatedCtx.LambdaCounter };

                            var transformedFuncImpl =
                                letFunc.Function.Declaration with
                                {
                                    Expression = transformedExpr
                                };

                            var transformedFunc =
                                letFunc.Function with { Declaration = transformedFuncImpl };

                            var transformedLetFunc =
                                new SyntaxTypes.LetDeclaration.LetFunction(transformedFunc);

                            newDeclarations.Add(transformedLetFunc);
                        }

                        break;
                    }

                case SyntaxTypes.LetDeclaration.LetDestructuring letDestr:
                    {
                        var (transformedExpr, updatedCtx) =
                            TransformExpressionInner(
                                letDestr.Expression,
                                currentContext,
                                liftedFunctions);

                        // Propagate the lambda counter advance from inside this
                        // destructuring's value to the outer scope so that sibling
                        // declarations and the let body do not reuse lambda IDs
                        // already consumed here.
                        currentContext =
                            currentContext with { LambdaCounter = updatedCtx.LambdaCounter };

                        var transformedDestr =
                            new SyntaxTypes.LetDeclaration.LetDestructuring(
                                letDestr.Pattern,
                                transformedExpr);

                        newDeclarations.Add(transformedDestr);
                        break;
                    }
            }
        }

        // Transform the let body expression
        var (transformedBody, finalContext) =
            TransformExpressionInner(
                letExpr.Expression,
                currentContext,
                liftedFunctions);

        var newLetExpr = new SyntaxTypes.Expression.LetExpression(newDeclarations, transformedBody);

        return (newLetExpr, finalContext);
    }

    private static (SyntaxTypes.LetDeclaration, LiftingContext) LiftNamedLambda(
        SyntaxTypes.LetDeclaration.LetFunction letFunc,
        SyntaxTypes.Expression.LambdaExpression lambdaExpr,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions,
        IReadOnlyDictionary<string, string> localFunctionLiftedNames,
        IReadOnlySet<string> siblingsWithoutExternalCaptures)
    {
        var bindingName = letFunc.Function.Declaration.Name;

        // Get lambda parameter names
        var lambdaParamNames = CollectPatternNames(lambdaExpr.Arguments);

        // Find free variables in the lambda body. We exclude the function's
        // own name (self-recursive references are handled via the
        // self-substitution below) and any sibling let-bound functions that
        // have been classified as having no external captures
        // (<paramref name="siblingsWithoutExternalCaptures"/>): for those
        // siblings the substituted call site can directly invoke the bare
        // lifted name without supplying any extra captured arguments.
        // Sibling references that do have external captures must NOT be
        // excluded — they must instead flow through the let binding which
        // holds the partial application <c>lifted__sibling capturedArgs</c>,
        // otherwise the surrounding lifted function would call the sibling
        // with the wrong first argument and silently corrupt the result.
        var freeVariables =
            FindFreeVariables(lambdaExpr.Expression, [.. lambdaParamNames])
            .Where(
                v =>
                context.BoundVariables.Contains(v) &&
                v != bindingName &&
                !siblingsWithoutExternalCaptures.Contains(v))
            .OrderBy(v => v)
            .ToList();

        // Use the pre-computed unique lifted function name
        var liftedFunctionName = localFunctionLiftedNames[bindingName];

        // Transform the lambda body
        var lambdaBodyContext = context.WithBoundVariables(lambdaParamNames);

        var (transformedBody, bodyContextAfter) =
            TransformExpressionInner(lambdaExpr.Expression, lambdaBodyContext, liftedFunctions);

        // Substitute self and the substitutable siblings (those with no
        // external captures) with their lifted names. Siblings that have
        // external captures are intentionally NOT in this map; they remain
        // as references and resolve to the let binding which holds the
        // sibling's partial application.
        var substitutions =
            new Dictionary<string, string>(StringComparer.Ordinal);

        if (freeVariables.Count is 0)
        {
            substitutions[bindingName] = liftedFunctionName;
        }

        foreach (var (siblingName, siblingLifted) in localFunctionLiftedNames)
        {
            if (siblingName == bindingName)
            {
                continue;
            }

            if (siblingsWithoutExternalCaptures.Contains(siblingName))
            {
                substitutions[siblingName] = siblingLifted;
            }
        }

        var substitutedBody = SubstituteVariableReferences(transformedBody, substitutions);

        if (freeVariables.Count > 0)
        {
            // Captures become leading parameter(s); forward them on self-calls so
            // recursive references are not under-applied (which would yield a thunk).
            substitutedBody =
                ForwardCapturesOnSelfReference(
                    substitutedBody,
                    bindingName,
                    liftedFunctionName,
                    freeVariables);
        }

        // Create the lifted function
        var liftedFuncDecl =
            CreateLiftedFunction(
                liftedFunctionName,
                freeVariables,
                lambdaExpr.Arguments,
                substitutedBody);

        liftedFunctions.Add(liftedFuncDecl);

        // Create the replacement: bindingName = liftedFunctionName capturedArgs
        var replacementExpr =
            CreateLiftedFunctionCall(
                liftedFunctionName,
                freeVariables);

        // Create a new let function that just assigns the partial application
        var newFuncImpl =
            new SyntaxTypes.FunctionImplementation(
                bindingName,
                [],
                replacementExpr);

        var newFunc =
            new SyntaxTypes.FunctionStruct(
                null,
                newFuncImpl);

        var newLetFunc = new SyntaxTypes.LetDeclaration.LetFunction(newFunc);

        // Propagate the lambda counter advance from the lambda body so that sibling
        // declarations and the enclosing scope do not reuse lambda IDs consumed here.
        return (newLetFunc, context with { LambdaCounter = bodyContextAfter.LambdaCounter });
    }

    private static (SyntaxTypes.LetDeclaration, LiftingContext) LiftLocalFunction(
        SyntaxTypes.LetDeclaration.LetFunction letFunc,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions,
        IReadOnlyDictionary<string, string> localFunctionLiftedNames,
        IReadOnlySet<string> siblingsWithoutExternalCaptures)
    {
        var bindingName = letFunc.Function.Declaration.Name;
        var funcParams = letFunc.Function.Declaration.Arguments;

        // Get function parameter names
        var funcParamNames = CollectPatternNames(funcParams);

        // Find free variables in the function body. We exclude the
        // function's own name (self-recursive references are handled via the
        // self-substitution below) and any sibling let-bound functions that
        // have been classified as having no external captures
        // (<paramref name="siblingsWithoutExternalCaptures"/>): for those
        // siblings the substituted call site can directly invoke the bare
        // lifted name without supplying any extra captured arguments.
        // Sibling references that do have external captures must NOT be
        // excluded — they must instead flow through the let binding which
        // holds the partial application <c>lifted__sibling capturedArgs</c>,
        // otherwise the surrounding lifted function would call the sibling
        // with the wrong first argument and silently corrupt the result
        // (the call site would supply the original argument as the
        // sibling's first captured parameter).
        var freeVariables =
            FindFreeVariables(letFunc.Function.Declaration.Expression, [.. funcParamNames])
            .Where(
                v =>
                context.BoundVariables.Contains(v) &&
                v != bindingName &&
                !siblingsWithoutExternalCaptures.Contains(v))
            .OrderBy(v => v)
            .ToList();

        // Use the pre-computed unique lifted function name
        var liftedFunctionName = localFunctionLiftedNames[bindingName];

        // Transform the function body (first transform any nested lambdas/local functions)
        var funcBodyContext = context.WithBoundVariables(funcParamNames);

        var (transformedBody, bodyContextAfter) =
            TransformExpressionInner(
                letFunc.Function.Declaration.Expression,
                funcBodyContext,
                liftedFunctions);

        // Substitute self and the substitutable siblings (those with no
        // external captures) with their lifted names. Siblings that have
        // external captures are intentionally NOT in this map; they remain
        // as references and resolve to the let binding which holds the
        // sibling's partial application.
        var substitutions =
            new Dictionary<string, string>(StringComparer.Ordinal);

        if (freeVariables.Count is 0)
        {
            substitutions[bindingName] = liftedFunctionName;
        }

        foreach (var (siblingName, siblingLifted) in localFunctionLiftedNames)
        {
            if (siblingName == bindingName)
            {
                continue;
            }

            if (siblingsWithoutExternalCaptures.Contains(siblingName))
            {
                substitutions[siblingName] = siblingLifted;
            }
        }

        var substitutedBody = SubstituteVariableReferences(transformedBody, substitutions);

        if (freeVariables.Count > 0)
        {
            // Captures become leading parameter(s); forward them on self-calls so
            // recursive references are not under-applied (which would yield a thunk).
            substitutedBody =
                ForwardCapturesOnSelfReference(
                    substitutedBody,
                    bindingName,
                    liftedFunctionName,
                    freeVariables);
        }

        // Create the lifted function
        var liftedFuncDecl =
            CreateLiftedFunction(
                liftedFunctionName,
                freeVariables,
                funcParams,
                substitutedBody);

        liftedFunctions.Add(liftedFuncDecl);

        // Create the replacement: bindingName = liftedFunctionName capturedArgs
        var replacementExpr =
            CreateLiftedFunctionCall(
                liftedFunctionName,
                freeVariables);

        // Create a new let function that just assigns the partial application
        var newFuncImpl =
            new SyntaxTypes.FunctionImplementation(
                bindingName,
                [],
                replacementExpr);

        var newFunc =
            new SyntaxTypes.FunctionStruct(
                null,
                newFuncImpl);

        var newLetFunc = new SyntaxTypes.LetDeclaration.LetFunction(newFunc);

        // Propagate the lambda counter advance from the function body so that sibling
        // declarations and the enclosing scope do not reuse lambda IDs consumed here.
        return (newLetFunc, context with { LambdaCounter = bodyContextAfter.LambdaCounter });
    }

    private static SyntaxTypes.Expression SubstituteVariableReferences(
        SyntaxTypes.Expression expr,
        IReadOnlyDictionary<string, string> substitutions)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.Identifier funcOrVal:

                // Only substitute local references (empty namespace)
                if (funcOrVal.QualifiedName.Namespaces.Count is 0 &&
                    substitutions.TryGetValue(funcOrVal.QualifiedName.DeclName, out var newName))
                {
                    return SyntaxTypes.Expression.Identifier.Create([], newName);
                }

                return funcOrVal;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:

                var newUpdateFields =
                    recordUpdate.Fields
                    .Select(f => f with { Value = SubstituteVariableReferences(f.Value, substitutions) })
                    .ToList();

                // Check if record name needs substitution
                var newRecordName =
                    substitutions.TryGetValue(recordUpdate.RecordName, out var newRecName)
                    ?
                    newRecName
                    :
                    recordUpdate.RecordName;

                return new SyntaxTypes.Expression.RecordUpdateExpression(newRecordName, newUpdateFields);

            default:
                return
                    ElmSyntaxAbstractTransformations.MapChildExpressions(
                        expr,
                        child => SubstituteVariableReferences(child, substitutions));
        }
    }

    /// <summary>
    /// Rewrites references to a lifted function's own name (<paramref name="selfName"/>)
    /// inside its lifted body so that the captured variables are forwarded as the
    /// leading argument(s).
    /// </summary>
    /// <remarks>
    /// Lambda lifting turns captured variables into the lifted function's leading
    /// parameter(s). A bare self-reference (as produced for capture-free functions)
    /// would therefore be under-applied: a recursive self-call would be missing the
    /// captured argument and would evaluate to an un-applied function value (a thunk)
    /// instead of recursing. This rewrite replaces:
    /// <list type="bullet">
    /// <item>a direct self-call <c>self a b</c> with <c>lifted captures a b</c>, and</item>
    /// <item>a bare self-reference used as a value with the partial application
    /// <c>lifted captures</c> (matching the replacement let binding).</item>
    /// </list>
    /// This is only applied when the lifted function actually captures variables.
    /// </remarks>
    private static SyntaxTypes.Expression ForwardCapturesOnSelfReference(
        SyntaxTypes.Expression expr,
        string selfName,
        string liftedName,
        IReadOnlyList<string> capturedVariables)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.Identifier funcOrVal:

                if (funcOrVal.QualifiedName.Namespaces.Count is 0 &&
                    funcOrVal.QualifiedName.DeclName == selfName)
                {
                    // Bare self-reference used as a value: replace with the
                    // partial application 'lifted captures'.
                    return CreateLiftedFunctionCall(liftedName, capturedVariables);
                }

                return funcOrVal;

            case SyntaxTypes.Expression.Application appExpr:

                if (appExpr.Function is SyntaxTypes.Expression.Identifier headRef &&
                    headRef.QualifiedName.Namespaces.Count is 0 &&
                    headRef.QualifiedName.DeclName == selfName)
                {
                    // Direct self-call: forward the captured argument(s) as the
                    // leading argument(s), keeping a single flat application.
                    var liftedCall = CreateLiftedFunctionCall(liftedName, capturedVariables);

                    var newFunction =
                        liftedCall is SyntaxTypes.Expression.Application liftedApp
                        ?
                        liftedApp.Function
                        :
                        liftedCall;

                    var forwardedArgs =
                        liftedCall is SyntaxTypes.Expression.Application liftedApp2
                        ?
                        new List<SyntaxTypes.Expression>(liftedApp2.Arguments)
                        :
                        [];

                    foreach (var arg in appExpr.Arguments)
                    {
                        forwardedArgs.Add(
                            ForwardCapturesOnSelfReference(
                                arg,
                                selfName,
                                liftedName,
                                capturedVariables));
                    }

                    return new SyntaxTypes.Expression.Application(newFunction, forwardedArgs);
                }

                return
                    new SyntaxTypes.Expression.Application(
                        ForwardCapturesOnSelfReference(appExpr.Function, selfName, liftedName, capturedVariables),
                        [
                        .. appExpr.Arguments.Select(
                            a => ForwardCapturesOnSelfReference(a, selfName, liftedName, capturedVariables))
                        ]);

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:

                // The record name is a plain variable and never equals a lifted
                // function name, so only the field values need rewriting.
                var newUpdateFields =
                    recordUpdate.Fields
                    .Select(
                        f =>
                        f with
                        {
                            Value =
                            ForwardCapturesOnSelfReference(f.Value, selfName, liftedName, capturedVariables)
                        })
                    .ToList();

                return new SyntaxTypes.Expression.RecordUpdateExpression(recordUpdate.RecordName, newUpdateFields);

            default:
                return
                    ElmSyntaxAbstractTransformations.MapChildExpressions(
                        expr,
                        child => ForwardCapturesOnSelfReference(child, selfName, liftedName, capturedVariables));
        }
    }

    private static SyntaxTypes.Declaration CreateLiftedFunction(
        string functionName,
        IReadOnlyList<string> capturedVariables,
        IReadOnlyList<SyntaxTypes.Pattern> lambdaParams,
        SyntaxTypes.Expression body)
    {
        var allParams = new List<SyntaxTypes.Pattern>();

        // Add captured variables as first parameter(s)
        if (capturedVariables.Count is 1)
        {
            // Single capture: plain parameter
            allParams.Add(new SyntaxTypes.Pattern.VarPattern(capturedVariables[0]));
        }
        else if (capturedVariables.Count > 1)
        {
            // Multiple captures: tuple pattern
            var tupleElements =
                capturedVariables
                .Select(v => (SyntaxTypes.Pattern)new SyntaxTypes.Pattern.VarPattern(v))
                .ToList();

            allParams.Add(new SyntaxTypes.Pattern.TuplePattern(tupleElements));
        }
        // Zero captures: no extra parameter

        // Add original lambda parameters
        allParams.AddRange(lambdaParams);

        var funcImpl =
            new SyntaxTypes.FunctionImplementation(
                functionName,
                allParams,
                body);

        var funcStruct =
            new SyntaxTypes.FunctionStruct(
                null,
                funcImpl);

        return new SyntaxTypes.Declaration.FunctionDeclaration(funcStruct);
    }

    private static SyntaxTypes.Expression CreateLiftedFunctionCall(
        string functionName,
        IReadOnlyList<string> capturedVariables,
        IReadOnlyList<string>? moduleNamespaces = null)
    {
        // Reference to the lifted function. When moduleNamespaces is supplied, emit a fully-qualified
        // reference (used at reuse sites where the post-pass qualification step would not otherwise
        // cover the name); otherwise emit an unqualified reference relying on the post-pass.
        var funcRef =
            new SyntaxTypes.Expression.Identifier(
                DeclQualifiedName.Create(moduleNamespaces ?? [], functionName));

        if (capturedVariables.Count is 0)
        {
            // No captures: just return the function reference
            return funcRef;
        }
        else if (capturedVariables.Count is 1)
        {
            // Single capture: function application with single argument
            var argRef = SyntaxTypes.Expression.Identifier.Create([], capturedVariables[0]);

            return new SyntaxTypes.Expression.Application(funcRef, [argRef]);
        }
        else
        {
            // Multiple captures: function application with tuple argument
            var tupleElements =
                capturedVariables
                .Select(v => (SyntaxTypes.Expression)SyntaxTypes.Expression.Identifier.Create([], v))
                .ToList();

            var tupleExpr = new SyntaxTypes.Expression.TupledExpression(tupleElements);

            return new SyntaxTypes.Expression.Application(funcRef, [tupleExpr]);
        }
    }

    private static ImmutableHashSet<string> FindFreeVariables(
        SyntaxTypes.Expression expr,
        ImmutableHashSet<string> boundVariables)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.Identifier funcOrVal:

                // Only consider local variables (empty namespace)
                if (funcOrVal.QualifiedName.Namespaces.Count is 0 &&
                    !boundVariables.Contains(funcOrVal.QualifiedName.DeclName))
                {
                    return [funcOrVal.QualifiedName.DeclName];
                }

                return [];

            case SyntaxTypes.Expression.LambdaExpression lambdaExpr:
                var lambdaParams = CollectPatternNames(lambdaExpr.Arguments);
                var newBound = boundVariables.Union(lambdaParams);
                return FindFreeVariables(lambdaExpr.Expression, newBound);

            case SyntaxTypes.Expression.LetExpression letExpr:
                var letBound = boundVariables;

                // Collect function names first (they are mutually recursive in Elm)
                foreach (var decl in letExpr.Declarations)
                {
                    switch (decl)
                    {
                        case SyntaxTypes.LetDeclaration.LetFunction letFunc:
                            letBound = letBound.Add(letFunc.Function.Declaration.Name);
                            break;

                        case SyntaxTypes.LetDeclaration.LetDestructuring letDestr:
                            letBound = letBound.Union(CollectPatternNames([letDestr.Pattern]));
                            break;
                    }
                }

                var letFreeVars = ImmutableHashSet<string>.Empty;

                foreach (var decl in letExpr.Declarations)
                {
                    switch (decl)
                    {
                        case SyntaxTypes.LetDeclaration.LetFunction letFunc:
                            var funcParams = CollectPatternNames(letFunc.Function.Declaration.Arguments);

                            letFreeVars =
                                letFreeVars.Union(
                                    FindFreeVariables(letFunc.Function.Declaration.Expression, letBound.Union(funcParams)));

                            break;

                        case SyntaxTypes.LetDeclaration.LetDestructuring letDestr:

                            // Destructuring patterns are NOT self-referencing: the RHS is
                            // evaluated in the OUTER scope, so the pattern's own bindings
                            // must not shadow variables in the RHS.
                            // Example: `let (a, b) = (a, b)` — the RHS `a` and `b` refer
                            // to outer-scope variables, not the pattern-bound names.
                            var destrPatternNames = CollectPatternNames([letDestr.Pattern]);
                            var destrBound = letBound.Except(destrPatternNames);

                            letFreeVars = letFreeVars.Union(FindFreeVariables(letDestr.Expression, destrBound));
                            break;
                    }
                }

                return letFreeVars.Union(FindFreeVariables(letExpr.Expression, letBound));

            case SyntaxTypes.Expression.Application appExpr:
                return
                    appExpr.Arguments.Aggregate(
                        FindFreeVariables(appExpr.Function, boundVariables),
                        (acc, arg) => acc.Union(FindFreeVariables(arg, boundVariables)));

            case SyntaxTypes.Expression.OperatorApplication opApp:
                return
                    FindFreeVariables(opApp.Left, boundVariables)
                    .Union(FindFreeVariables(opApp.Right, boundVariables));

            case SyntaxTypes.Expression.IfBlock ifBlock:
                return
                    FindFreeVariables(ifBlock.Condition, boundVariables)
                    .Union(FindFreeVariables(ifBlock.ThenBlock, boundVariables))
                    .Union(FindFreeVariables(ifBlock.ElseBlock, boundVariables));

            case SyntaxTypes.Expression.CaseExpression caseExpr:

                var caseFreeVars =
                    FindFreeVariables(caseExpr.Expression, boundVariables);

                foreach (var caseItem in caseExpr.Cases)
                {
                    var casePatternNames = CollectPatternNames([caseItem.Pattern]);

                    caseFreeVars =
                        caseFreeVars.Union(
                            FindFreeVariables(caseItem.Expression, boundVariables.Union(casePatternNames)));
                }

                return caseFreeVars;

            case SyntaxTypes.Expression.TupledExpression tupled:
                return
                    tupled.Elements.Aggregate(
                        ImmutableHashSet<string>.Empty,
                        (acc, elem) => acc.Union(FindFreeVariables(elem, boundVariables)));

            case SyntaxTypes.Expression.ListExpr listExpr:
                return
                    listExpr.Elements.Aggregate(
                        ImmutableHashSet<string>.Empty,
                        (acc, elem) => acc.Union(FindFreeVariables(elem, boundVariables)));

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                return
                    recordExpr.Fields.Aggregate(
                        ImmutableHashSet<string>.Empty,
                        (acc, field) => acc.Union(FindFreeVariables(field.Value, boundVariables)));

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                return FindFreeVariables(recordAccess.Record, boundVariables);

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:

                // The record name is a variable reference
                var recordFreeVars =
                    !boundVariables.Contains(recordUpdate.RecordName)
                    ?
                    [recordUpdate.RecordName]
                    :
                    ImmutableHashSet<string>.Empty;

                return
                    recordUpdate.Fields.Aggregate(
                        recordFreeVars,
                        (acc, field) => acc.Union(FindFreeVariables(field.Value, boundVariables)));

            case SyntaxTypes.Expression.Negation negation:
                return FindFreeVariables(negation.Expression, boundVariables);

            // Leaf expressions - no variables
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.GLSLExpression:
                return [];

            default:
                throw new NotImplementedException(
                    $"FindFreeVariables not implemented for expression type: {expr.GetType().Name}");
        }
    }

    private static ImmutableList<string> CollectPatternNames(IReadOnlyList<SyntaxTypes.Pattern> patterns)
    {
        return
            patterns.Aggregate(
                ImmutableList<string>.Empty,
                (acc, pattern) => acc.AddRange(CollectPatternNamesInner(pattern)));
    }

    private static ImmutableList<string> CollectPatternNamesInner(SyntaxTypes.Pattern pattern)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPat:
                return [varPat.Name];

            case SyntaxTypes.Pattern.TuplePattern tuplePat:
                return
                    tuplePat.Elements.Aggregate(
                        ImmutableList<string>.Empty,
                        (acc, elem) => acc.AddRange(CollectPatternNamesInner(elem)));

            case SyntaxTypes.Pattern.RecordPattern recordPat:
                return
                    recordPat.Fields.Aggregate(
                        ImmutableList<string>.Empty,
                        (acc, field) => acc.Add(field.FieldName));

            case SyntaxTypes.Pattern.AsPattern asPat:
                return CollectPatternNamesInner(asPat.Pattern).Add(asPat.Name);

            case SyntaxTypes.Pattern.ListPattern listPat:
                return
                    listPat.Elements.Aggregate(
                        ImmutableList<string>.Empty,
                        (acc, elem) => acc.AddRange(CollectPatternNamesInner(elem)));

            case SyntaxTypes.Pattern.UnConsPattern unconsPat:
                return
                    CollectPatternNamesInner(unconsPat.Head)
                    .AddRange(CollectPatternNamesInner(unconsPat.Tail));

            case SyntaxTypes.Pattern.NamedPattern namedPat:
                return
                    namedPat.Arguments.Aggregate(
                        ImmutableList<string>.Empty,
                        (acc, arg) => acc.AddRange(CollectPatternNamesInner(arg)));

            // Patterns that don't bind names
            case SyntaxTypes.Pattern.AllPattern:
            case SyntaxTypes.Pattern.UnitPattern:
            case SyntaxTypes.Pattern.CharPattern:
            case SyntaxTypes.Pattern.StringPattern:
            case SyntaxTypes.Pattern.IntPattern:
            case SyntaxTypes.Pattern.FloatPattern:
                return [];

            default:
                throw new NotImplementedException(
                    $"CollectPatternNames not implemented for pattern type: {pattern.GetType().Name}");
        }
    }

    // Transform methods for other expression types

    private static (SyntaxTypes.Expression, LiftingContext) TransformApplication(
        SyntaxTypes.Expression.Application appExpr,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        var (transformedFunc, ctxAfterFunc) =
            TransformExpressionInner(appExpr.Function, context, liftedFunctions);

        var transformedArgs = new List<SyntaxTypes.Expression>();
        var currentContext = ctxAfterFunc;

        foreach (var arg in appExpr.Arguments)
        {
            var (transformedArg, newContext) = TransformExpressionInner(arg, currentContext, liftedFunctions);
            transformedArgs.Add(transformedArg);
            currentContext = newContext;
        }

        var newApp = new SyntaxTypes.Expression.Application(transformedFunc, transformedArgs);
        return (newApp, currentContext);
    }

    private static (SyntaxTypes.Expression, LiftingContext) TransformOperatorApplication(
        SyntaxTypes.Expression.OperatorApplication opApp,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        var (transformedLeft, ctx1) =
            TransformExpressionInner(opApp.Left, context, liftedFunctions);

        var (transformedRight, ctx2) =
            TransformExpressionInner(opApp.Right, ctx1, liftedFunctions);

        var newOpApp =
            new SyntaxTypes.Expression.OperatorApplication(
                opApp.Operator,
                opApp.Direction,
                transformedLeft,
                transformedRight);

        return (newOpApp, ctx2);
    }

    private static (SyntaxTypes.Expression, LiftingContext) TransformIfBlock(
        SyntaxTypes.Expression.IfBlock ifBlock,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        var (transformedCond, ctx1) =
            TransformExpressionInner(ifBlock.Condition, context, liftedFunctions);

        var (transformedThen, ctx2) =
            TransformExpressionInner(ifBlock.ThenBlock, ctx1, liftedFunctions);

        var (transformedElse, ctx3) =
            TransformExpressionInner(ifBlock.ElseBlock, ctx2, liftedFunctions);

        var newIfBlock = new SyntaxTypes.Expression.IfBlock(transformedCond, transformedThen, transformedElse);

        return (newIfBlock, ctx3);
    }

    private static (SyntaxTypes.Expression, LiftingContext) TransformCaseExpression(
        SyntaxTypes.Expression.CaseExpression caseExpr,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        var (transformedScrutinee, ctx1) =
            TransformExpressionInner(caseExpr.Expression, context, liftedFunctions);

        var transformedCases = new List<SyntaxTypes.Case>();
        var currentContext = ctx1;

        foreach (var caseItem in caseExpr.Cases)
        {
            var patternNames = CollectPatternNames([caseItem.Pattern]);
            var caseContext = currentContext.WithBoundVariables(patternNames);

            var (transformedExpr, newContext) =
                TransformExpressionInner(caseItem.Expression, caseContext, liftedFunctions);

            transformedCases.Add(new SyntaxTypes.Case(caseItem.Pattern, transformedExpr));

            currentContext = newContext;
        }

        var newCaseExpr = new SyntaxTypes.Expression.CaseExpression(transformedScrutinee, transformedCases);
        return (newCaseExpr, currentContext);
    }

    private static (SyntaxTypes.Expression, LiftingContext) TransformTupledExpression(
        SyntaxTypes.Expression.TupledExpression tupled,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        var transformedElements = new List<SyntaxTypes.Expression>();
        var currentContext = context;

        foreach (var elem in tupled.Elements)
        {
            var (transformedElem, newContext) =
                TransformExpressionInner(elem, currentContext, liftedFunctions);

            transformedElements.Add(transformedElem);
            currentContext = newContext;
        }

        var newTupled = new SyntaxTypes.Expression.TupledExpression(transformedElements);
        return (newTupled, currentContext);
    }

    private static (SyntaxTypes.Expression, LiftingContext) TransformListExpression(
        SyntaxTypes.Expression.ListExpr listExpr,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        var transformedElements = new List<SyntaxTypes.Expression>();
        var currentContext = context;

        foreach (var elem in listExpr.Elements)
        {
            var (transformedElem, newContext) =
                TransformExpressionInner(elem, currentContext, liftedFunctions);

            transformedElements.Add(transformedElem);
            currentContext = newContext;
        }

        var newListExpr = new SyntaxTypes.Expression.ListExpr(transformedElements);
        return (newListExpr, currentContext);
    }

    private static (SyntaxTypes.Expression, LiftingContext) TransformRecordExpression(
        SyntaxTypes.Expression.RecordExpr recordExpr,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        var transformedFields = new List<SyntaxTypes.RecordSetter>();

        var currentContext = context;

        foreach (var field in recordExpr.Fields)
        {
            var (transformedValue, newContext) =
                TransformExpressionInner(field.Value, currentContext, liftedFunctions);

            transformedFields.Add(field with { Value = transformedValue });
            currentContext = newContext;
        }

        var newRecordExpr = new SyntaxTypes.Expression.RecordExpr(transformedFields);
        return (newRecordExpr, currentContext);
    }

    private static (SyntaxTypes.Expression, LiftingContext) TransformRecordAccess(
        SyntaxTypes.Expression.RecordAccess recordAccess,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        var (transformedRecord, newContext) = TransformExpressionInner(recordAccess.Record, context, liftedFunctions);
        var newRecordAccess = recordAccess with { Record = transformedRecord };
        return (newRecordAccess, newContext);
    }

    private static (SyntaxTypes.Expression, LiftingContext) TransformRecordUpdateExpression(
        SyntaxTypes.Expression.RecordUpdateExpression recordUpdate,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        var transformedFields = new List<SyntaxTypes.RecordSetter>();
        var currentContext = context;

        foreach (var field in recordUpdate.Fields)
        {
            var (transformedValue, newContext) =
                TransformExpressionInner(field.Value, currentContext, liftedFunctions);

            transformedFields.Add(field with { Value = transformedValue });
            currentContext = newContext;
        }

        var newRecordUpdate =
            new SyntaxTypes.Expression.RecordUpdateExpression(recordUpdate.RecordName, transformedFields);

        return (newRecordUpdate, currentContext);
    }

    private static (SyntaxTypes.Expression, LiftingContext) TransformNegation(
        SyntaxTypes.Expression.Negation negation,
        LiftingContext context,
        List<SyntaxTypes.Declaration> liftedFunctions)
    {
        var (transformedExpr, newContext) = TransformExpressionInner(negation.Expression, context, liftedFunctions);
        var newNegation = new SyntaxTypes.Expression.Negation(transformedExpr);
        return (newNegation, newContext);
    }
}
