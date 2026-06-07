using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Transformations of Elm syntax, for optimizing downstream runtime efficiency of interpreted and compiled code.
/// This includes specialization of higher-order functions and inlining of small functions and plain values.
/// 
/// TODO: Link new section in spec
/// </summary>
public partial class ElmSyntaxOptimization
{


    // Config, SmallFunctionsConfig, and RewriteConfig records live in the sibling
    // partial-class file `ElmSyntaxOptimization.Config.cs`.

    /// <summary>
    /// Represents a function declaration with its containing module for inlining purposes.
    /// </summary>
    internal record FunctionInfo(
        ModuleName ModuleName,
        SyntaxTypes.FunctionStruct Function,
        bool IsRecursive);

    /// <summary>
    /// Information about a constructor that belongs to a single-constructor choice type.
    /// Built from <see cref="SyntaxTypes.Declaration.CustomTypeDeclaration"/> during pre-processing.
    /// </summary>
    private record SingleChoiceConstructorInfo(
        SyntaxTypes.QualifiedNameRef ConstructorName,
        int FieldCount);

    private record ModuleResolutionContext(
        ImmutableDictionary<DeclQualifiedName, FunctionInfo> FunctionsByQualifiedName,
        ImmutableDictionary<SyntaxTypes.QualifiedNameRef, SingleChoiceConstructorInfo> SingleChoiceConstructors,
        ImmutableDictionary<string, TypeInference.InferredType> FunctionSignatures,
        ModuleName? CurrentModuleName = null);

    /// <summary>
    /// Context for inlining operations, including all function definitions and configuration.
    /// </summary>
    private record InliningContext(
        ModuleResolutionContext Resolution,
        Config Config,
        ImmutableHashSet<DeclQualifiedName> InliningStack,
        SpecializationCatalog SpecializationCatalog,
        RewriteConfig RewriteConfig,
        ImmutableHashSet<string> LocalNames = default!,
        ImmutableHashSet<string> ModuleLevelNames = default!,
        ImmutableDictionary<string, SyntaxTypes.Expression> LetRhsByName = default!)
    {
        /// <summary>
        /// Names of local variables currently in scope (from let bindings, function parameters,
        /// and case patterns). These shadow module-level function names and must not be resolved
        /// to module-level functions during inlining.
        /// </summary>
        public ImmutableHashSet<string> LocalNames { get; init; } = LocalNames ?? [];

        /// <summary>
        /// Names declared at module level in the current module. New binders introduced while
        /// inlining must avoid these names as well because canonicalization reports shadowing
        /// against top-level declarations.
        /// </summary>
        public ImmutableHashSet<string> ModuleLevelNames { get; init; } = ModuleLevelNames ?? [];

        /// <summary>
        /// Map from let-bound local name to its RHS expression for the specific
        /// <c>let (Wrapper localName) = &lt;rhs&gt; in ...</c> single-tag newtype
        /// destructuring shape. Used by the specialization-discovery walker to
        /// look through one layer of let binding when classifying a tuple element
        /// or higher-order argument that is a bare local reference (see plan in
        /// <c>explore/internal-analysis/2026-05-19-monomorphizing-expressionAfterOpeningSquareBracket-lifted-lambda3.md</c>
        /// §4 Step 1+2). The walker rebuilds shadowed entries at every binder.
        /// </summary>
        public ImmutableDictionary<string, SyntaxTypes.Expression> LetRhsByName { get; init; } =
            LetRhsByName ?? [];
    }


    /// <summary>
    /// Result of resolving a function reference to its qualified name and function info.
    /// </summary>
    private sealed record ResolvedFunctionReference(
        DeclQualifiedName QualifiedName,
        FunctionInfo FunctionInfo);

    /// <summary>
    /// Result of an inlining operation: the rewritten expression and any new top-level
    /// declarations generated during the rewrite (e.g., specialized function declarations).
    /// </summary>
    private sealed record InliningResult(
        SyntaxTypes.Expression Expression,
        ImmutableList<Node<SyntaxTypes.Declaration>> GeneratedDeclarations);


    /// <summary>
    /// Default number of optimization rounds for
    /// <see cref="OptimizeRounds(OptimizedElmSyntaxDeclarations, Config)"/>.
    /// </summary>
    public const int DefaultOptimizationRounds = 4;

    /// <summary>
    /// Runs the combined specialization-and-inlining pipeline with the
    /// <see cref="DefaultOptimizationRounds"/> limit, repeating transformations
    /// until the declaration dictionary converges (structural equality) or the limit is reached.
    /// </summary>
    public static Result<string, OptimizedElmSyntaxDeclarations> OptimizeRounds(
        OptimizedElmSyntaxDeclarations declarations,
        Config config) =>
        OptimizeRounds(declarations, config, rounds: DefaultOptimizationRounds);

    /// <summary>
    /// Runs the combined specialization-and-inlining pipeline up to <paramref name="rounds"/>
    /// times, stopping early in case of convergence.
    /// </summary>
    public static Result<string, OptimizedElmSyntaxDeclarations> OptimizeRounds(
        OptimizedElmSyntaxDeclarations declarations,
        Config config,
        int rounds)
    {
        var current = declarations;

        for (var round = 0; round < rounds; round++)
        {
            var result = SpecializeAndInlineDeclarations(current, config, RewriteConfig.Combined);

            if (result.IsErrOrNull() is { } err)
                return err;

            if (result.IsOkOrNull() is not { } next)
                throw new NotImplementedException("Unexpected result type");

            // Structural equality: if the output matches the input the pipeline has converged.
            if (next.Equals(current))
            {
                return next;
            }

            current = next;
        }

        return current;
    }

    /// <summary>
    /// Run only the specialization rewrite stage: collect higher-order specialization
    /// opportunities and emit the corresponding <c>__specialized__N</c> sibling
    /// declarations, without performing classic inlining at call sites. Because this
    /// stage can emit new declarations, the method name includes "Specialize".
    /// Convenience wrapper for
    /// <see cref="SpecializeAndInlineDeclarations(OptimizedElmSyntaxDeclarations, Config, RewriteConfig)"/>
    /// with <see cref="RewriteConfig.SpecializationOnly"/>.
    /// </summary>
    public static Result<string, OptimizedElmSyntaxDeclarations> SpecializeDeclarations(
        OptimizedElmSyntaxDeclarations declarations,
        Config config) =>
        SpecializeAndInlineDeclarations(declarations, config, RewriteConfig.SpecializationOnly);

    /// <summary>
    /// Run a single combined specialization+inlining round. This is the entry point
    /// used by the inlining phases of the compiler pipeline. Because this can both
    /// add new specialized sibling declarations and substitute callee bodies at
    /// call sites, the name includes "Specialize" and "Inline".
    /// <para>
    /// Phase 2 of the production pipeline runs as Combined (not InliningOnly).
    /// See explore/internal-analysis/2026-05-16-skipWhileWithoutLinebreakHelp-alpha-regression.md
    /// §5.4 for the rationale: running classic inlining without specialization
    /// creates a temporal gap where a recursive higher-order call newly exposed
    /// by Phase 2's wrapper inlining cannot be redirected to its pre-existing
    /// <c>__specialized__N</c> sibling (TrySpecializeRecursiveCall is gated by
    /// EnablesSpecialization, which is false in InliningOnly). Routing Phase 2
    /// through Combined closes the gap by enabling both spec collection and
    /// call-site rewriting within the same call.
    /// </para>
    /// </summary>
    public static Result<string, OptimizedElmSyntaxDeclarations> SpecializeAndInlineDeclarationsCombined(
        OptimizedElmSyntaxDeclarations declarations,
        Config config) =>
        SpecializeAndInlineDeclarations(declarations, config, RewriteConfig.Combined);

    /// <summary>
    /// Workhorse of the specialization+inlining pipeline: in one pass it (a)
    /// collects specialization opportunities and emits the resulting
    /// <c>__specialized__N</c> sibling declarations, (b) substitutes selected
    /// callee bodies at call sites, (c) lifts any lambdas / local functions
    /// introduced during the rewrite, (d) optionally strips wrapper-return
    /// patterns (emitting <c>__stripped</c> sibling declarations) and applies
    /// wrap/unwrap cancellation. Because this method can ADD new top-level
    /// declarations, its name includes "Specialize" (per the project convention
    /// that "Rewrite" is reserved for non-adding transforms).
    /// <para>
    /// Whether the trailing wrap/unwrap cancellation runs is gated on
    /// <see cref="Config.WrapUnwrapCancellationEnabled"/>. The wrapper-return
    /// stripping + cancellation block as a whole is also gated on
    /// <c>config.SmallFunctions is null</c> — see
    /// <c>explore/internal-analysis/2026-05-15-s1-cancel-parser-newtype-wrap-unwrap.md</c>
    /// section 5 for why those passes must not run under size-based inlining.
    /// </para>
    /// </summary>
    public static Result<string, OptimizedElmSyntaxDeclarations> SpecializeAndInlineDeclarations(
        OptimizedElmSyntaxDeclarations declarations,
        Config config,
        RewriteConfig rewriteConfig) =>
        SpecializeAndInlineDeclarations(declarations, config, rewriteConfig, StageToggles.Default);

    /// <summary>
    /// Extended overload of
    /// <see cref="SpecializeAndInlineDeclarations(OptimizedElmSyntaxDeclarations, Config, RewriteConfig)"/>
    /// that accepts a <see cref="StageToggles"/> record selecting which post-passes run
    /// after the specialization+inlining rewrite. Intended for debugging investigations
    /// (e.g. bisecting which sub-stage in a given round introduces an incorrect rewrite):
    /// callers can ask for the raw inliner output, then drive
    /// <see cref="LambdaLifting.LiftLambdas(OptimizedElmSyntaxDeclarations)"/>,
    /// <see cref="ApplicationNormalization.NormalizeApplicationsInDeclarationDictionary"/>,
    /// and the WRS / WUC passes manually in their own test code. The defaults on
    /// <see cref="StageToggles.Default"/> reproduce the production behaviour of the
    /// three-argument overload, so existing callers are unaffected.
    /// </summary>
    public static Result<string, OptimizedElmSyntaxDeclarations> SpecializeAndInlineDeclarations(
        OptimizedElmSyntaxDeclarations declarations,
        Config config,
        RewriteConfig rewriteConfig,
        StageToggles stageToggles)
    {
        // Build a dictionary of all function declarations
        var functionsByQualifiedName = BuildFunctionDictionary(declarations);

        // Mark recursive functions
        var functionsWithRecursionInfo = MarkRecursiveFunctions(functionsByQualifiedName);

        // Build type context: identify constructors of single-constructor choice types
        var singleChoiceConstructors = BuildSingleChoiceConstructors(declarations);

        // Build function signatures from type annotations for type-aware function detection
        var functionSignatures = BuildFunctionSignatures(declarations);

        // --- Pass 1: Collect all specialization requests ---
        var resolution =
            new ModuleResolutionContext(
                functionsWithRecursionInfo,
                singleChoiceConstructors,
                functionSignatures);

        var collectionContext =
            new InliningContext(
                resolution,
                config,
                [],
                SpecializationCatalog: SpecializationCatalog.Empty,
                RewriteConfig: rewriteConfig);

        var collectedSpecializations =
            CollectSpecializationsFromDeclarations(declarations, collectionContext);

        // --- Naming: Deduplicate and assign deterministic names ---
        // Deduplication includes filtering out specializations already present
        // in `declarations.FunctionDeclarations[*].Specializations` so that we
        // never re-emit a fresh `__specialized__N` name for a specialization
        // the input already carries.
        var catalog = BuildCatalogFromCollectedSpecializations(collectedSpecializations, declarations);

        // --- Pass 2: Rewrite using the catalog ---
        var rewriteContext =
            new InliningContext(
                resolution,
                config,
                [],
                SpecializationCatalog: catalog,
                RewriteConfig: rewriteConfig);

        // Group declarations by module for per-module processing, iterating
        // the structured form directly so we never materialise a global
        // flat dictionary in this method.
        var declsByModule =
            new Dictionary<ModuleName, ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>.Builder>(
                EnumerableExtensions.EqualityComparer<ModuleName>());

        foreach (var entry in declarations.EnumerateAllDeclarations())
        {
            if (!declsByModule.TryGetValue(entry.Key.Namespaces, out var moduleBuilder))
            {
                moduleBuilder =
                    ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

                declsByModule[entry.Key.Namespaces] = moduleBuilder;
            }

            moduleBuilder[entry.Key] = entry.Value;
        }

        var resultBuilder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (moduleName, moduleBuilder) in declsByModule)
        {
            var moduleContext =
                rewriteContext with { Resolution = rewriteContext.Resolution with { CurrentModuleName = moduleName } };

            var moduleDecls = moduleBuilder.ToImmutable();

            var inlinedModuleDecls = InlineDeclarations(moduleDecls, moduleName, moduleContext);

            foreach (var (key, decl) in inlinedModuleDecls)
            {
                resultBuilder[key] = decl;
            }
        }

        // Lift inlining output into the structured model so subsequent
        // rewrite passes can each take and return
        // OptimizedElmSyntaxDeclarations.
        var current =
            OptimizedElmSyntaxDeclarations.FromFlatDictionary(resultBuilder.ToImmutable());

        // Lambda lifting: lift any lambdas or local functions introduced during
        // inlining/specialization so that the output is free of disallowed nodes.
        if (stageToggles.LambdaLiftingEnabled)
        {
            current = LambdaLifting.LiftLambdas(current);
        }

        // Normalize all Application nodes so no Application has another
        // Application as its head. Substitution-based rewrites elsewhere
        // in the pipeline (notably the pipe-operator desugaring above)
        // can produce nested-curried Application form, which forces
        // ExpressionCompiler.CompileApplication to fall through to the
        // closure-allocating EmitFunctionValueWithEnvFunctions path
        // instead of taking the direct-call fast path. Flattening here
        // reshapes those into single saturated Applications without
        // changing observable behavior.
        if (stageToggles.NormalizeApplicationsEnabled)
        {
            current = NormalizeApplicationsInDeclarationDictionary(current);
        }

        // D2 Step 2 (WrapperReturnStripping) + S1 (WrapUnwrapCancellation):
        // both passes are gated on Phases 1 + 2 (config.SmallFunctions is null) only —
        // running them under Phase 3 size-based inlining can cause its small-function
        // inliner to re-inline a __stripped sibling back into a forwarding body, undoing
        // the strip. See
        // explore/internal-analysis/2026-05-15-s1-cancel-parser-newtype-wrap-unwrap.md
        // section 5 for the placement rationale.
        if (config.SmallFunctions is null && stageToggles.WrapperReturnStrippingEnabled)
        {
            var newtypeRegistry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(current);

            if (!newtypeRegistry.IsEmpty)
            {
                var stripPlans =
                    WrapperReturnStripping.BuildStripPlans(current, newtypeRegistry);

                current = WrapperReturnStripping.RewriteDeclarationDictionary(current);

                var siblingsByOriginal =
                    stripPlans.ToImmutableDictionary(
                        kvp => kvp.Key,
                        kvp => kvp.Value.ToGeneratedSiblingDecl());

                if (config.WrapUnwrapCancellationEnabled)
                {
                    current =
                        WrapUnwrapCancellation.RewriteDeclarationDictionary(
                            current,
                            siblingsByOriginal);
                }
            }
        }

        return current;
    }

    /// <summary>
    /// Apply only the wrap/unwrap cancellation rewrite pass (Shapes A/B
    /// literal cancellation; no specialization, no inlining, no
    /// wrapper-return stripping). This is a pure rewrite that never adds
    /// new declarations — hence the verb "Apply" rather than "Specialize".
    /// Gated on <see cref="Config.WrapUnwrapCancellationEnabled"/>: when
    /// disabled the input is returned unchanged.
    /// <para>
    /// This is the recommended entry point for unit tests and other
    /// call sites that want only the wrap/unwrap cancellation behaviour
    /// without running the rest of the optimization pipeline.
    /// </para>
    /// </summary>
    public static OptimizedElmSyntaxDeclarations ApplyWrapUnwrapCancellation(
        OptimizedElmSyntaxDeclarations declarations,
        Config config)
    {
        if (!config.WrapUnwrapCancellationEnabled)
            return declarations;

        return WrapUnwrapCancellation.RewriteDeclarationDictionary(declarations);
    }

    /// <summary>
    /// Apply only the wrap/unwrap cancellation rewrite pass with an
    /// explicit sibling-decl registry — enables sibling-aware Shape A'/B'
    /// cancellations in addition to the always-on literal-wrap Shapes A/B.
    /// Like the no-sibling overload, this is a pure rewrite that never
    /// adds new declarations. Gated on
    /// <see cref="Config.WrapUnwrapCancellationEnabled"/>.
    /// </summary>
    internal static OptimizedElmSyntaxDeclarations ApplyWrapUnwrapCancellation(
        OptimizedElmSyntaxDeclarations declarations,
        Config config,
        ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl> siblingsByOriginal)
    {
        if (!config.WrapUnwrapCancellationEnabled)
            return declarations;

        return WrapUnwrapCancellation.RewriteDeclarationDictionary(declarations, siblingsByOriginal);
    }

    // NormalizeApplicationsInDeclarationDictionary / NormalizeApplicationsInDeclaration /
    // NormalizeApplicationsInFunction live in the sibling partial-class file
    // `ApplicationNormalization.cs`.

    private static bool EnablesSpecialization(InliningContext context) =>
        context.RewriteConfig.SpecializationEnabled;

    private static bool EnablesClassicInlining(InliningContext context) =>
        context.RewriteConfig.InliningEnabled;

    /// <summary>
    /// Looks up the pre-assigned specialized name from the catalog for a given function
    /// and a single parameter specialization.
    /// Returns null if no matching specialization was found in the catalog.
    /// </summary>
    private static string? LookupSpecializedName(
        DeclQualifiedName targetFunctionName,
        int paramIndex,
        ParameterSpecialization paramSpec,
        InliningContext context)
    {
        return
            LookupSpecializedNameForHigherOrder(
                targetFunctionName,
                ImmutableDictionary<int, ParameterSpecialization>.Empty
                .Add(paramIndex, paramSpec),
                context);
    }

    /// <summary>
    /// Looks up the pre-assigned specialized name from the catalog for a given function
    /// and a set of parameter specializations (for higher-order recursive specialization).
    /// Returns null if no matching specialization was found in the catalog.
    /// </summary>
    private static string? LookupSpecializedNameForHigherOrder(
        DeclQualifiedName targetFunctionName,
        ImmutableDictionary<int, ParameterSpecialization> paramSpecs,
        InliningContext context)
    {
        if (!context.SpecializationCatalog.SpecializationsByFunction.TryGetValue(targetFunctionName, out var specializations))
            return null;

        var target =
            new FunctionSpecialization(
                paramSpecs);

        foreach (var named in specializations)
        {
            if (named.Specialization.Equals(target))
                return named.SpecializedFunctionName;
        }

        return null;
    }

    internal static ImmutableDictionary<DeclQualifiedName, FunctionInfo> MarkRecursiveFunctions(
        ImmutableDictionary<DeclQualifiedName, FunctionInfo> functions)
    {
        // Phase 1: walk each function body exactly once and collect the set of
        // direct top-level function references (restricted to nodes that exist
        // in `functions`). This avoids the previous O(V * E) blow-up where
        // CollectFunctionReferences was re-invoked for every node visited
        // during the recursion check of every function.
        var directRefs =
            new Dictionary<DeclQualifiedName, HashSet<DeclQualifiedName>>(functions.Count);

        foreach (var kvp in functions)
        {
            var funcKey = kvp.Key;
            var funcInfo = kvp.Value;

            var refs = new HashSet<DeclQualifiedName>();

            foreach (var r in CollectFunctionReferences(funcInfo.Function.Declaration.Value.Expression.Value))
            {
                // Restrict to edges into the function set; references to
                // values / constructors / built-ins are irrelevant for the
                // recursive-call analysis.
                if (functions.ContainsKey(r))
                {
                    refs.Add(r);
                }
                else if (r.Equals(funcKey))
                {
                    // Defensive: keep self-edges even if the dictionary lookup
                    // didn't include the function itself (shouldn't happen, but
                    // preserves the previous semantics of direct-self detection).
                    refs.Add(r);
                }
            }

            directRefs[funcKey] = refs;
        }

        // Phase 2: a function is recursive iff it can reach itself in the
        // direct-call graph. Equivalent: it has a self-loop, OR it belongs to
        // a strongly-connected component of size > 1. Compute SCCs once with
        // Tarjan's algorithm — O(V + E) — and mark every node in any non-
        // trivial SCC, plus any node with a self-edge, as recursive.
        var recursive = ComputeRecursiveNodes(directRefs);

        var builder = functions.ToBuilder();

        foreach (var kvp in functions)
        {
            var funcKey = kvp.Key;
            var funcInfo = kvp.Value;

            builder[funcKey] = funcInfo with { IsRecursive = recursive.Contains(funcKey) };
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Given a directed graph (adjacency map: node → out-neighbours, restricted
    /// to nodes that are themselves keys of <paramref name="graph"/>), returns
    /// the set of nodes that can reach themselves through one or more edges.
    /// That is exactly: nodes with a self-loop ∪ nodes that belong to a
    /// strongly-connected component of size &gt; 1.
    ///
    /// Uses Tarjan's SCC algorithm with an explicit work stack (no recursion)
    /// so deep call graphs cannot blow the .NET runtime stack.
    /// Time and space complexity are O(V + E).
    /// </summary>
    private static HashSet<DeclQualifiedName> ComputeRecursiveNodes(
        Dictionary<DeclQualifiedName, HashSet<DeclQualifiedName>> graph)
    {
        var index = 0;
        var indices = new Dictionary<DeclQualifiedName, int>(graph.Count);
        var lowlink = new Dictionary<DeclQualifiedName, int>(graph.Count);
        var onStack = new HashSet<DeclQualifiedName>();
        var sccStack = new Stack<DeclQualifiedName>();
        var recursive = new HashSet<DeclQualifiedName>();

        // Pre-mark all self-loops; these are recursive regardless of SCC size.
        foreach (var kvp in graph)
        {
            if (kvp.Value.Contains(kvp.Key))
            {
                recursive.Add(kvp.Key);
            }
        }

        // Iterative Tarjan using an explicit frame stack to support deep graphs.
        var work = new Stack<(DeclQualifiedName Node, IEnumerator<DeclQualifiedName> Iter)>();

        foreach (var startNode in graph.Keys)
        {
            if (indices.ContainsKey(startNode))
                continue;

            // strongconnect(startNode)
            indices[startNode] = index;
            lowlink[startNode] = index;
            index++;
            sccStack.Push(startNode);
            onStack.Add(startNode);
            work.Push((startNode, graph[startNode].GetEnumerator()));

            while (work.Count > 0)
            {
                var frame = work.Peek();
                var v = frame.Node;
                var iter = frame.Iter;

                if (iter.MoveNext())
                {
                    var w = iter.Current;

                    if (!graph.ContainsKey(w))
                    {
                        // Out-of-graph edge (shouldn't occur — we restricted the
                        // adjacency lists above — but guard defensively).
                        continue;
                    }

                    if (!indices.ContainsKey(w))
                    {
                        indices[w] = index;
                        lowlink[w] = index;
                        index++;
                        sccStack.Push(w);
                        onStack.Add(w);
                        work.Push((w, graph[w].GetEnumerator()));
                    }
                    else if (onStack.Contains(w))
                    {
                        if (indices[w] < lowlink[v])
                            lowlink[v] = indices[w];
                    }
                }
                else
                {
                    // All successors of v explored — pop frame and possibly emit an SCC.
                    work.Pop();

                    if (lowlink[v] == indices[v])
                    {
                        // Root of an SCC; pop nodes until v.
                        var component = new List<DeclQualifiedName>();
                        DeclQualifiedName w;

                        do
                        {
                            w = sccStack.Pop();
                            onStack.Remove(w);
                            component.Add(w);
                        }
                        while (!w.Equals(v));

                        if (component.Count > 1)
                        {
                            foreach (var n in component)
                                recursive.Add(n);
                        }
                        // (size-1 SCCs only contribute via the self-loop pre-pass.)
                    }

                    // Propagate lowlink to parent frame.
                    if (work.Count > 0)
                    {
                        var parent = work.Peek().Node;

                        if (lowlink[v] < lowlink[parent])
                            lowlink[parent] = lowlink[v];
                    }
                }
            }
        }

        return recursive;
    }

    private static IEnumerable<DeclQualifiedName> CollectFunctionReferences(
        SyntaxTypes.Expression expr)
    {
        var results = new List<DeclQualifiedName>();
        var worklist = new Stack<SyntaxTypes.Expression>();
        worklist.Push(expr);

        while (worklist.Count > 0)
        {
            var current = worklist.Pop();

            if (current is SyntaxTypes.Expression.FunctionOrValue funcOrValue)
            {
                results.Add(DeclQualifiedName.Create(funcOrValue.ModuleName, funcOrValue.Name));
            }

            SyntaxTypes.SyntaxAnalysis.ForEachChildExpression(current, worklist.Push);
        }

        return results;
    }

    internal static ImmutableDictionary<DeclQualifiedName, FunctionInfo> BuildFunctionDictionary(
        OptimizedElmSyntaxDeclarations declarations) =>
        BuildFunctionDictionary(declarations.RenderAsFlatDictionary());

    internal static ImmutableDictionary<DeclQualifiedName, FunctionInfo> BuildFunctionDictionary(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var builder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, FunctionInfo>();

        foreach (var (key, decl) in declarations)
        {
            if (decl is SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                builder[key] = new FunctionInfo(key.Namespaces, funcDecl.Function, IsRecursive: false);
            }
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Builds a dictionary mapping each constructor of single-constructor choice types
    /// to its <see cref="SingleChoiceConstructorInfo"/>. This replaces the former syntactic
    /// body-scanning heuristic with a definitive type-based check.
    /// </summary>
    private static ImmutableDictionary<SyntaxTypes.QualifiedNameRef, SingleChoiceConstructorInfo> BuildSingleChoiceConstructors(
        OptimizedElmSyntaxDeclarations declarations) =>
        BuildSingleChoiceConstructors(declarations.RenderAsFlatDictionary());

    private static ImmutableDictionary<SyntaxTypes.QualifiedNameRef, SingleChoiceConstructorInfo> BuildSingleChoiceConstructors(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var builder =
            ImmutableDictionary.CreateBuilder<SyntaxTypes.QualifiedNameRef, SingleChoiceConstructorInfo>();

        foreach (var (key, decl) in declarations)
        {
            if (decl is not SyntaxTypes.Declaration.CustomTypeDeclaration choiceTypeDecl)
                continue;

            var typeStruct = choiceTypeDecl.TypeDeclaration;

            // Only single-constructor types qualify
            if (typeStruct.Constructors.Count is not 1)
                continue;

            var constructor = typeStruct.Constructors[0].Value;
            var constructorName = new SyntaxTypes.QualifiedNameRef(key.Namespaces, constructor.Name.Value);

            builder[constructorName] =
                new SingleChoiceConstructorInfo(
                    ConstructorName: constructorName,
                    FieldCount: constructor.Arguments.Count);
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Builds a combined function signatures map from all modules using
    /// <see cref="TypeInference.BuildFunctionSignaturesMap"/>. This enables type-aware
    /// function detection in <see cref="IsFunctionExpression"/>.
    /// </summary>
    private static ImmutableDictionary<string, TypeInference.InferredType> BuildFunctionSignatures(
        OptimizedElmSyntaxDeclarations declarations) =>
        BuildFunctionSignatures(declarations.RenderAsFlatDictionary());

    private static ImmutableDictionary<string, TypeInference.InferredType> BuildFunctionSignatures(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var builder =
            ImmutableDictionary.CreateBuilder<string, TypeInference.InferredType>();

        foreach (var (key, decl) in declarations)
        {
            var moduleNameString = string.Join(".", key.Namespaces);

            TypeInference.CollectFunctionSignaturesFromDeclaration(decl, moduleNameString, builder);
        }

        return builder.ToImmutable();
    }

    private static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> InlineDeclarations(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> moduleDecls,
        ModuleName moduleName,
        InliningContext context)
    {
        var moduleLevelNames = CollectModuleLevelDeclarationNames(moduleDecls);

        context =
            context with
            {
                ModuleLevelNames = moduleLevelNames
            };

        var newDecls = ImmutableList.CreateBuilder<Node<SyntaxTypes.Declaration>>();
        var inlinedDeclarations = new List<(DeclQualifiedName Key, Node<SyntaxTypes.Declaration> Decl)>();

        foreach (var (key, decl) in moduleDecls.OrderBy(kvp => kvp.Key))
        {
            var declNode = new Node<SyntaxTypes.Declaration>(ElmSyntaxTransformations.s_zeroRange, decl);
            var (inlinedDecl, generatedDecls) = InlineDeclaration(declNode, context);

            inlinedDeclarations.Add((key, inlinedDecl));
            newDecls.AddRange(generatedDecls);
        }

        // Inline the generated specialized functions to beta-reduce any
        // lambda applications introduced by parameter substitution
        // (e.g., (\x -> [x, x]) first  →  [first, first]).
        foreach (var newDecl in newDecls)
        {
            var (inlinedNewDecl, furtherDecls) = InlineDeclaration(newDecl, context);

            var newDeclName = ElmCompiler.GetDeclarationName(inlinedNewDecl.Value);
            var newKey = DeclQualifiedName.Create(moduleName, newDeclName ?? "unknown");

            inlinedDeclarations.Add((newKey, inlinedNewDecl));

            foreach (var furtherDecl in furtherDecls)
            {
                var furtherDeclName = ElmCompiler.GetDeclarationName(furtherDecl.Value);
                var furtherKey = DeclQualifiedName.Create(moduleName, furtherDeclName ?? "unknown");
                inlinedDeclarations.Add((furtherKey, furtherDecl));
            }
        }

        // Build a flat dict of the inlined declarations for simplification context.
        // Use a builder to handle duplicate keys (same specialized function generated
        // from different call sites) — last value wins, which is safe since duplicates
        // have identical content.
        var inlinedDeclsDictBuilder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (key, declNode) in inlinedDeclarations)
        {
            inlinedDeclsDictBuilder[key] = declNode.Value;
        }

        var inlinedDeclsDict = inlinedDeclsDictBuilder.ToImmutable();

        var simplificationFunctions = context.Resolution.FunctionsByQualifiedName.ToBuilder();

        foreach (var rewrittenFunction in MarkRecursiveFunctions(BuildFunctionDictionary(inlinedDeclsDict)))
        {
            simplificationFunctions[rewrittenFunction.Key] = rewrittenFunction.Value;
        }

        var simplificationContext =
            context with
            {
                Resolution =
                context.Resolution with
                {
                    FunctionsByQualifiedName = simplificationFunctions.ToImmutable()
                }
            };

        // Recompute module-level names to include any newly generated declarations
        // (e.g., specialized functions added during inlining). The original moduleLevelNames
        // only covered the input declarations. Without this update, RenameDeclarationBindingsAvoidingCapture
        // would not know to rename local bindings that clash with the new top-level names.
        var allModuleLevelNames = CollectModuleLevelDeclarationNames(inlinedDeclsDict);

        var resultBuilder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (key, declNode) in inlinedDeclarations)
        {
            var simplified = SimplifyGeneratedDeclaration(declNode, simplificationContext);

            // Safety net: rename all local bindings in the declaration to ensure
            // no naming clashes remain after simplification and inlining.
            simplified = RenameDeclarationBindingsAvoidingCapture(simplified, allModuleLevelNames);

            // Post-process: ensure all Application arguments are parenthesized where necessary
            var parenthesized = ElmSyntaxTransformations.ParenthesizeDeclaration(simplified);

            resultBuilder[key] = parenthesized.Value;
        }

        return resultBuilder.ToImmutable();
    }

    /// <summary>
    /// Renames all local bindings in a function declaration to avoid any naming clashes.
    /// This ensures that function parameters, let-bindings, case patterns, and lambda
    /// parameters do not shadow module-level names or each other.
    /// </summary>
    private static Node<SyntaxTypes.Declaration> RenameDeclarationBindingsAvoidingCapture(
        Node<SyntaxTypes.Declaration> declNode,
        ImmutableHashSet<string> moduleLevelNames)
    {
        if (declNode.Value is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            return declNode;

        var impl = funcDecl.Function.Declaration.Value;

        var freshImpl =
            ElmSyntaxTransformations.RenameBindingsAvoidingCapture(impl, moduleLevelNames);

        if (ReferenceEquals(freshImpl, impl))
            return declNode;

        var newFunc =
            funcDecl.Function with
            {
                Declaration =
                new Node<SyntaxTypes.FunctionImplementation>(
                    funcDecl.Function.Declaration.Range,
                    freshImpl)
            };

        return
            new Node<SyntaxTypes.Declaration>(
                declNode.Range,
                new SyntaxTypes.Declaration.FunctionDeclaration(newFunc));
    }

    /// <summary>
    /// Collects all binding names (let-functions, let-destructuring, case patterns, lambda params)
    /// in an expression tree for diagnostic purposes.
    /// </summary>
    private static void CollectAllBindingNames(SyntaxTypes.Expression expr, HashSet<string> names)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.LetExpression letExpr:
                foreach (var decl in letExpr.Value.Declarations)
                {
                    switch (decl.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            names.Add(letFunc.Function.Declaration.Value.Name.Value);

                            foreach (var arg in letFunc.Function.Declaration.Value.Arguments)
                                ElmSyntaxTransformations.CollectPatternNamesRecursive(arg.Value, names);

                            CollectAllBindingNames(letFunc.Function.Declaration.Value.Expression.Value, names);
                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            ElmSyntaxTransformations.CollectPatternNamesRecursive(letDestr.Pattern.Value, names);
                            CollectAllBindingNames(letDestr.Expression.Value, names);
                            break;
                    }
                }

                CollectAllBindingNames(letExpr.Value.Expression.Value, names);
                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                CollectAllBindingNames(caseExpr.CaseBlock.Expression.Value, names);

                foreach (var c in caseExpr.CaseBlock.Cases)
                {
                    ElmSyntaxTransformations.CollectPatternNamesRecursive(c.Pattern.Value, names);
                    CollectAllBindingNames(c.Expression.Value, names);
                }

                break;

            case SyntaxTypes.Expression.LambdaExpression lambda:
                foreach (var arg in lambda.Lambda.Arguments)
                    ElmSyntaxTransformations.CollectPatternNamesRecursive(arg.Value, names);

                CollectAllBindingNames(lambda.Lambda.Expression.Value, names);
                break;

            case SyntaxTypes.Expression.Application app:
                foreach (var a in app.Arguments)
                    CollectAllBindingNames(a.Value, names);

                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                CollectAllBindingNames(ifBlock.Condition.Value, names);
                CollectAllBindingNames(ifBlock.ThenBlock.Value, names);
                CollectAllBindingNames(ifBlock.ElseBlock.Value, names);
                break;

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                CollectAllBindingNames(paren.Expression.Value, names);
                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                CollectAllBindingNames(opApp.Left.Value, names);
                CollectAllBindingNames(opApp.Right.Value, names);
                break;

            case SyntaxTypes.Expression.TupledExpression tupled:
                foreach (var e in tupled.Elements)
                    CollectAllBindingNames(e.Value, names);

                break;

            case SyntaxTypes.Expression.ListExpr list:
                foreach (var e in list.Elements)
                    CollectAllBindingNames(e.Value, names);

                break;

            case SyntaxTypes.Expression.RecordExpr record:
                foreach (var f in record.Fields)
                    CollectAllBindingNames(f.Value.Item2.Value, names);

                break;

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                CollectAllBindingNames(recordAccess.Record.Value, names);
                break;

            case SyntaxTypes.Expression.Negation negation:
                CollectAllBindingNames(negation.Expression.Value, names);
                break;
        }
    }

    private static ImmutableHashSet<string> CollectModuleLevelDeclarationNames(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var names = ImmutableHashSet.CreateBuilder<string>();

        foreach (var (key, decl) in declarations)
        {
            names.Add(key.DeclName);

            // Also add constructor names for custom types
            if (decl is SyntaxTypes.Declaration.CustomTypeDeclaration customTypeDeclaration)
            {
                foreach (var constructor in customTypeDeclaration.TypeDeclaration.Constructors)
                    names.Add(constructor.Value.Name.Value);
            }
        }

        return names.ToImmutable();
    }

    private static ImmutableHashSet<string> NamesToAvoidForFreshBindings(
        InliningContext context) =>
        context.LocalNames.Union(context.ModuleLevelNames);

    /// <summary>
    /// Builds the full set of names that fresh bindings introduced by the inliner
    /// must avoid colliding with. This is
    ///   (outer scope ∪ module-level names) ∪ ⋃ FV(inlinedArg_i)
    /// — the union of <see cref="NamesToAvoidForFreshBindings(InliningContext)"/>
    /// with the free variables of every expression that will be substituted into
    /// the callee body.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Including <c>FV(inlinedArg_i)</c> is the safety condition that prevents
    /// substitution-capture: when an inlined argument's free reference (e.g. an
    /// outer accumulator named <c>s</c>) is substituted into the callee body, any
    /// callee binding (e.g. a partial-application lambda parameter named <c>s</c>)
    /// that would have been alpha-renamed to a name still occurring free in the
    /// argument must instead be renamed to a name that does NOT occur free in
    /// any argument.
    /// </para>
    /// <para>
    /// This is recommendation #2 from
    /// <c>explore/internal-analysis/2026-05-19-loop-int-list-regression-findings.md</c>:
    /// centralizing this construction in one place ensures every inliner site that
    /// introduces fresh bindings uses the same (correct) avoidance set, instead of
    /// each computing it independently and risking one of them forgetting the
    /// FV-of-args contribution.
    /// </para>
    /// </remarks>
    private static IReadOnlySet<string> NamesToAvoidForFreshBindingsIncludingArgFreeVars(
        InliningContext context,
        IReadOnlyList<Node<SyntaxTypes.Expression>> inlinedArgs)
    {
        var outerNamesToAvoid = NamesToAvoidForFreshBindings(context);

        HashSet<string>? augmented = null;
        IReadOnlySet<string> namesToAvoid = outerNamesToAvoid;

        foreach (var inlinedArg in inlinedArgs)
        {
            var argFreeVars =
                SyntaxTypes.SyntaxAnalysis.CollectRemainingFreeVariables(inlinedArg.Value);

            foreach (var freeVar in argFreeVars)
            {
                if (namesToAvoid.Contains(freeVar))
                    continue;

                if (augmented is null)
                {
                    // Copy on first write so we don't mutate the shared
                    // outer-scope set returned by NamesToAvoidForFreshBindings.
                    augmented = [.. namesToAvoid];
                    namesToAvoid = augmented;
                }

                augmented.Add(freeVar);
            }
        }

        return namesToAvoid;
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

        // After simplification (which can beta-reduce, inline let bindings, etc.),
        // rename all local bindings to avoid shadowing module-level names
        // AND the function's own parameter names (Elm disallows all shadowing).
        var namesToAvoid = NamesToAvoidForFreshBindings(context);

        var freshImpl =
            ElmSyntaxTransformations.RenameBindingsAvoidingCapture(
                impl with { Expression = simplifiedExpr },
                namesToAvoid);

        var newFunc =
            funcDecl.Function with
            {
                Declaration =
                new Node<SyntaxTypes.FunctionImplementation>(
                    funcDecl.Function.Declaration.Range,
                    freshImpl)
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
                                new Node<SyntaxTypes.Expression>(ElmSyntaxTransformations.s_zeroRange, simplifiedLet),
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

                    if (ElmSyntaxTransformations.TryCollapseSingleChoiceWrapperPassThroughLet(
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

                    if (ElmSyntaxTransformations.TryBetaReduceGeneratedApplication(simplifiedApp) is { } reducedApp &&
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

        if (TryDeconstructKnownConstructorApplicationForGeneratedSimplification(
            letDestr.Expression,
            context) is not { } ctorResult ||
            ElmSyntaxTransformations.TryBindSingleChoiceTagPattern(
                letDestr.Pattern.Value,
                ctorResult.ConstructorName,
                ctorResult.FieldExpressions) is not { } patternBindings ||
            patternBindings.Count is 0)
        {
            return false;
        }

        foreach (var binding in patternBindings)
        {
            if (ElmSyntaxTransformations.CountUnshadowedLocalVariableReferences(body.Value, binding.Key) is not 1)
            {
                return false;
            }
        }

        collapsed = ElmSyntaxTransformations.SubstituteInExpression(body, patternBindings);
        return true;
    }

    private static ElmSyntaxTransformations.ConstructorApplication? TryDeconstructKnownConstructorApplicationForGeneratedSimplification(
        Node<SyntaxTypes.Expression> exprNode,
        InliningContext context)
    {
        return TryDeconstructKnownConstructorApplication(exprNode, context, requireExplicitConstructor: true);
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

        // Function parameter names shadow module-level functions in the function body.
        var paramNames = new HashSet<string>();

        foreach (var arg in impl.Arguments)
            ElmSyntaxTransformations.CollectPatternNamesRecursive(arg.Value, paramNames);

        var bodyContext =
            paramNames.Count > 0
            ?
            context with { LocalNames = context.LocalNames.Union(paramNames) }
            :
            context;

        var (inlinedExpr, newDecls) = InlineExpression(impl.Expression, bodyContext);

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

                SyntaxTypes.Expression.FunctionOrValue funcOrValue =>
                TryInlinePlainValue(funcOrValue, context) ?? expr,

                // Leaf expressions that don't need transformation
                SyntaxTypes.Expression.UnitExpr or
                SyntaxTypes.Expression.Literal or
                SyntaxTypes.Expression.CharLiteral or
                SyntaxTypes.Expression.Integer or
                SyntaxTypes.Expression.Hex or
                SyntaxTypes.Expression.Floatable or
                SyntaxTypes.Expression.PrefixOperator or
                SyntaxTypes.Expression.RecordAccessFunction =>
                expr,

                _ =>
                expr
            };

        return (new Node<SyntaxTypes.Expression>(exprNode.Range, inlinedExpr), newDecls.ToImmutable());
    }

    private static InliningResult InlineApplication(
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
                new InliningResult(
                    new SyntaxTypes.Expression.Application(
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
                    new InliningResult(
                        ElmSyntaxTransformations.ParenthesizeApplicationArguments(result),
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
                    new InliningResult(
                        ElmSyntaxTransformations.ParenthesizeApplicationArguments(result),
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

                // Count is 3 means the operator + 2 function arguments (no application argument yet).
                // Count > 3 means extra arguments are being applied to the composed result.
                if (app.Arguments.Count is 3)
                {
                    // No application argument: produce a lambda \composeArg -> outer (inner composeArg)
                    var param =
                        new Node<SyntaxTypes.Pattern>(
                            ElmSyntaxTransformations.s_zeroRange,
                            new SyntaxTypes.Pattern.VarPattern("composeArg"));

                    var paramRef =
                        new Node<SyntaxTypes.Expression>(
                            ElmSyntaxTransformations.s_zeroRange,
                            new SyntaxTypes.Expression.FunctionOrValue([], "composeArg"));

                    var innerApp =
                        new Node<SyntaxTypes.Expression>(
                            ElmSyntaxTransformations.s_zeroRange,
                            new SyntaxTypes.Expression.Application([inner, paramRef]));

                    var parenInnerApp =
                        new Node<SyntaxTypes.Expression>(
                            ElmSyntaxTransformations.s_zeroRange,
                            new SyntaxTypes.Expression.ParenthesizedExpression(innerApp));

                    var bodyExpr = new SyntaxTypes.Expression.Application([outer, parenInnerApp]);
                    var bodyNode = new Node<SyntaxTypes.Expression>(ElmSyntaxTransformations.s_zeroRange, bodyExpr);
                    var inlinedBody = Inline(bodyNode);

                    return
                        new InliningResult(
                            new SyntaxTypes.Expression.LambdaExpression(
                                new SyntaxTypes.LambdaStruct([param], inlinedBody)),
                            newDecls.ToImmutable());
                }
                else
                {
                    // Has application argument: composeR f g x [extra...]  →  g (f x) [extra...]
                    var innerApp =
                        new Node<SyntaxTypes.Expression>(
                            ElmSyntaxTransformations.s_zeroRange,
                            new SyntaxTypes.Expression.Application([inner, app.Arguments[3]]));

                    var parenInnerApp =
                        new Node<SyntaxTypes.Expression>(
                            ElmSyntaxTransformations.s_zeroRange,
                            new SyntaxTypes.Expression.ParenthesizedExpression(innerApp));

                    var desugaredArgs = new List<Node<SyntaxTypes.Expression>> { outer, parenInnerApp };
                    desugaredArgs.AddRange(app.Arguments.Skip(4));

                    var (result, decls) =
                        InlineApplication(new SyntaxTypes.Expression.Application([.. desugaredArgs]), context);

                    newDecls.AddRange(decls);

                    return
                        new InliningResult(
                            ElmSyntaxTransformations.ParenthesizeApplicationArguments(result),
                            newDecls.ToImmutable());
                }
            }
        }

        // Beta-reduce lambda applications: (\x -> body) arg  →  body[x := arg]
        if (EnablesClassicInlining(context))
        {
            var unwrapped = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(funcExpr);

            if (unwrapped is SyntaxTypes.Expression.LambdaExpression lambda)
            {
                var (result, decls) = BetaReduceLambda(lambda.Lambda, [.. app.Arguments.Skip(1)], context);
                newDecls.AddRange(decls);
                return new InliningResult(result, newDecls.ToImmutable());
            }
        }

        // Reduce record-access-function applications when the record argument is known.
        // This turns shapes like `.transform ops first` into `increment first` once `ops`
        // resolves to a concrete record value, which directly reduces remaining
        // higher-order applications in specialized parser-like loops.
        if (EnablesClassicInlining(context))
        {
            var unwrapped = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(funcExpr);

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

                        if (app.Arguments.Count is 2)
                        {
                            var (fieldExpr, fieldDecls) =
                                InlineExpression(field.Value.valueExpr, context);

                            newDecls.AddRange(fieldDecls);

                            return new InliningResult(fieldExpr.Value, newDecls.ToImmutable());
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

                        return new InliningResult(reducedResult, newDecls.ToImmutable());
                    }
                }
            }
        }

        // Check if this is a call to a known function. Unwrap any parenthesization
        // around the head: lambda lifting can introduce parenthesized references such as
        // `(myFunc) arg` that would otherwise prevent us from resolving the callee.
        if (SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(funcExpr) is SyntaxTypes.Expression.FunctionOrValue funcOrValue)
        {
            if (TryResolveKnownFunctionReference(funcOrValue, context) is { } resolved)
            {
                var funcInfo = resolved.FunctionInfo;
                var funcImpl = funcInfo.Function.Declaration.Value;
                var funcParams = funcImpl.Arguments;
                var appArgs = app.Arguments.Skip(1).ToList();

                if (funcInfo.IsRecursive || !EnablesClassicInlining(context))
                {
                    // Try to specialize the recursive function by substituting
                    // loop-invariant function arguments with concrete values.
                    if (funcInfo.IsRecursive &&
                        EnablesSpecialization(context) &&
                        ShouldInline(funcParams, funcImpl.Expression, appArgs, context))
                    {
                        if (TrySpecializeRecursiveCall(
                            funcInfo,
                            funcImpl,
                            appArgs,
                            context) is { } specResult)
                        {
                            newDecls.AddRange(specResult.GeneratedDeclarations);
                            return new InliningResult(specResult.Expression, newDecls.ToImmutable());
                        }
                    }

                    if (EnablesSpecialization(context))
                    {
                        if (TrySpecializeSingleChoiceTagCall(
                                funcInfo,
                                funcImpl,
                                appArgs,
                                context) is { } singleChoiceResult)
                        {
                            newDecls.AddRange(singleChoiceResult.GeneratedDeclarations);
                            return new InliningResult(singleChoiceResult.Expression, newDecls.ToImmutable());
                        }

                        // Non-recursive TupleUnwrap-driven HO specialization
                        // (PR D of explore/internal-analysis/2026-05-19-monomorphizing-expressionAfterOpeningSquareBracket-lifted-lambda3.md).
                        // Materializes a sibling whose tuple-pattern parameter
                        // is destructured into per-leaf direct references.
                        if (!funcInfo.IsRecursive &&
                            TrySpecializeNonRecursiveTupleUnwrapCall(
                                funcInfo,
                                funcImpl,
                                appArgs,
                                context) is { } tupleUnwrapResult)
                        {
                            newDecls.AddRange(tupleUnwrapResult.GeneratedDeclarations);
                            return new InliningResult(tupleUnwrapResult.Expression, newDecls.ToImmutable());
                        }
                    }

                    // Wrapper-with-captured-function partial-application specialization.
                    // Fires for non-recursive callees (e.g. lambda-lifted wrappers) when the
                    // call is a partial application supplying a known function for a parameter
                    // that is forwarded into a recursive callee inside the body. Inlining the
                    // wrapper here exposes the recursive call with a literal function argument,
                    // which the recursive walk then routes through TrySpecializeRecursiveCall to
                    // materialize a first-order specialized variant of the recursive helper.
                    if (!funcInfo.IsRecursive &&
                        EnablesSpecialization(context) &&
                        !context.InliningStack.Contains(resolved.QualifiedName) &&
                        ShouldInlinePartialApplicationWithCapturedFunction(
                            funcInfo,
                            funcImpl,
                            appArgs,
                            context))
                    {
                        var newContext =
                            context with { InliningStack = context.InliningStack.Add(resolved.QualifiedName) };

                        var (inlinedResult, inlinedDecls) =
                            InlineFunctionCall(funcInfo.ModuleName, funcImpl, appArgs, newContext);

                        newDecls.AddRange(inlinedDecls);

                        return new InliningResult(inlinedResult, newDecls.ToImmutable());
                    }

                    return
                        new InliningResult(
                            new SyntaxTypes.Expression.Application(
                                [.. app.Arguments.Select(Inline)]),
                            newDecls.ToImmutable());
                }

                if (EnablesSpecialization(context))
                {
                    if (TrySpecializeSingleChoiceTagCall(
                            funcInfo,
                            funcImpl,
                            appArgs,
                            context) is { } singleChoiceResult2)
                    {
                        newDecls.AddRange(singleChoiceResult2.GeneratedDeclarations);
                        return new InliningResult(singleChoiceResult2.Expression, newDecls.ToImmutable());
                    }

                    // Non-recursive TupleUnwrap-driven HO specialization, also
                    // wired here for the EnablesClassicInlining branch so it
                    // fires regardless of which inliner-mode flag is set.
                    if (!funcInfo.IsRecursive &&
                        TrySpecializeNonRecursiveTupleUnwrapCall(
                            funcInfo,
                            funcImpl,
                            appArgs,
                            context) is { } tupleUnwrapResult2)
                    {
                        newDecls.AddRange(tupleUnwrapResult2.GeneratedDeclarations);
                        return new InliningResult(tupleUnwrapResult2.Expression, newDecls.ToImmutable());
                    }
                }

                // Skip if we're already in the process of inlining this function (prevents infinite recursion)
                if (context.InliningStack.Contains(resolved.QualifiedName))
                {
                    return
                        new InliningResult(
                            new SyntaxTypes.Expression.Application(
                                [.. app.Arguments.Select(Inline)]),
                            newDecls.ToImmutable());
                }

                // Check if we should inline based on config
                var shouldInlineHigherOrder = ShouldInline(funcParams, funcImpl.Expression, appArgs, context);

                var shouldInlineWrapperPartial =
                    !shouldInlineHigherOrder &&
                    EnablesSpecialization(context) &&
                    ShouldInlinePartialApplicationWithCapturedFunction(
                        funcInfo,
                        funcImpl,
                        appArgs,
                        context);

                var isSmallEnough =
                    !shouldInlineHigherOrder &&
                    !shouldInlineWrapperPartial &&
                    IsSmallEnoughToInline(funcImpl.Expression, context);

                if (shouldInlineHigherOrder || shouldInlineWrapperPartial || isSmallEnough)
                {
                    // Add this function to the inlining stack to prevent infinite recursion
                    var newContext = context with { InliningStack = context.InliningStack.Add(resolved.QualifiedName) };

                    if (isSmallEnough)
                    {
                        // For size-based inlining, only substitute parameters without
                        // recursive inlining to avoid cascading inlining that can break
                        // lambda lifting (e.g., inlining `skip` exposes `map2 revAlways`
                        // which triggers higher-order inlining and disrupts captured variables).
                        var (inlinedResult, inlinedDecls) =
                            InlineSmallFunctionCall(funcInfo.ModuleName, funcImpl, appArgs, newContext);

                        newDecls.AddRange(inlinedDecls);

                        return new InliningResult(inlinedResult, newDecls.ToImmutable());
                    }
                    else
                    {
                        // Inline the function: substitute parameters with arguments
                        var (inlinedResult, inlinedDecls) =
                            InlineFunctionCall(funcInfo.ModuleName, funcImpl, appArgs, newContext);

                        newDecls.AddRange(inlinedDecls);

                        return new InliningResult(inlinedResult, newDecls.ToImmutable());
                    }
                }
            }
        }

        // Default: recursively inline arguments
        return
            new InliningResult(
                new SyntaxTypes.Expression.Application(
                    [.. app.Arguments.Select(Inline)]),
                newDecls.ToImmutable());
    }





    /// <summary>
    /// Beta-reduces a lambda application: <c>(\x -> body) arg</c> becomes <c>body[x := arg]</c>.
    /// Handles exact, partial, and over-application cases.
    /// </summary>
    private static InliningResult BetaReduceLambda(
        SyntaxTypes.LambdaStruct lambda,
        IReadOnlyList<Node<SyntaxTypes.Expression>> args,
        InliningContext context)
    {
        var newDecls = ImmutableList.CreateBuilder<Node<SyntaxTypes.Declaration>>();

        var consumedArgs = Math.Min(lambda.Arguments.Count, args.Count);
        var consumedInlinedArgs = new List<Node<SyntaxTypes.Expression>>(consumedArgs);

        for (var i = 0; i < consumedArgs; i++)
        {
            var (inlinedArg, argDecls) = InlineExpression(args[i], context);
            newDecls.AddRange(argDecls);
            consumedInlinedArgs.Add(inlinedArg);
        }

        // Capture-avoiding rename of the lambda's bindings. The names-to-avoid
        // set must include every free variable of the inlined arguments, not
        // just outer-scope/module-level names — otherwise a lambda parameter
        // whose spelling happens to match a free reference inside a
        // substituted-in argument would capture that reference. See the
        // matching fix in InlineFunctionCallCore.
        var namesToAvoid =
            NamesToAvoidForFreshBindingsIncludingArgFreeVars(context, consumedInlinedArgs);

        if (namesToAvoid.Count > 0)
        {
            lambda = ElmSyntaxTransformations.RenameBindingsAvoidingCapture(lambda, namesToAvoid);
        }

        var body =
            ElmSyntaxTransformations.ApplyConsumedArgumentBindings(
                lambda.Expression,
                lambda.Arguments,
                consumedInlinedArgs);

        // Collect any local binding names introduced by the parameter substitution
        // (e.g., pattern destructuring like `(Parser parseA)` becomes `let (Parser parseA) = arg`).
        // These names must be added to the context so that nested inlining knows to avoid them.
        var introducedBindings = new HashSet<string>();

        for (var i = 0; i < consumedArgs; i++)
        {
            ElmSyntaxTransformations.CollectPatternNamesRecursive(
                lambda.Arguments[i].Value,
                introducedBindings);
        }

        var bodyContext =
            introducedBindings.Count > 0
            ?
            context with { LocalNames = context.LocalNames.Union(introducedBindings) }
            :
            context;

        Node<SyntaxTypes.Expression> Inline(Node<SyntaxTypes.Expression> e)
        {
            var (result, decls) = InlineExpression(e, bodyContext);
            newDecls.AddRange(decls);
            return result;
        }

        body = Inline(body);

        if (args.Count < lambda.Arguments.Count)
        {
            // Partial application: return remaining lambda
            var remainingParams = lambda.Arguments.Skip(args.Count).ToImmutableArray();

            var partialApplication =
                new Node<SyntaxTypes.Expression>(
                    ElmSyntaxTransformations.s_zeroRange,
                    new SyntaxTypes.Expression.LambdaExpression(
                        new SyntaxTypes.LambdaStruct(remainingParams, body)));

            if (namesToAvoid.Count > 0)
            {
                partialApplication =
                    ElmSyntaxTransformations.RenameBindingsAvoidingCapture(
                        partialApplication,
                        namesToAvoid);
            }

            return new InliningResult(partialApplication.Value, newDecls.ToImmutable());
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
                    bodyContext);

            newDecls.AddRange(appDecls);

            var applicationResult =
                new Node<SyntaxTypes.Expression>(
                    ElmSyntaxTransformations.s_zeroRange,
                    appResult);

            if (namesToAvoid.Count > 0)
            {
                applicationResult =
                    ElmSyntaxTransformations.RenameBindingsAvoidingCapture(
                        applicationResult,
                        namesToAvoid);
            }

            return new InliningResult(applicationResult.Value, newDecls.ToImmutable());
        }

        // Exact application
        if (namesToAvoid.Count > 0)
        {
            body = ElmSyntaxTransformations.RenameBindingsAvoidingCapture(body, namesToAvoid);
        }

        return new InliningResult(body.Value, newDecls.ToImmutable());
    }

    /// <summary>
    /// Checks whether a function body is small enough to be inlined unconditionally
    /// at every call site based on the thresholds in <see cref="Config.SmallFunctions"/>.
    /// A relaxed restriction excludes function bodies that invoke another function whose
    /// recursion status we cannot rule out: only kernel/builtin calls, constructor
    /// applications, calls into natively-implemented modules (e.g. <c>Basics</c>), and
    /// calls that resolve to a known non-recursive (and non-mutually-recursive) top-level
    /// function are allowed in the body. This still implicitly excludes (mutually)
    /// recursive callees because their <see cref="FunctionInfo.IsRecursive"/> flag is set
    /// during pre-processing.
    /// Used only for non-recursive functions in the classic inlining path.
    /// </summary>
    private static bool IsSmallEnoughToInline(
        Node<SyntaxTypes.Expression> funcBody,
        InliningContext context)
    {
        // Size-based small-function inlining is enabled only when the config carries a
        // non-null SmallFunctions threshold record.
        if (context.Config.SmallFunctions is not { } smallConfig)
        {
            return false;
        }

        // Relaxed restriction (will be replaced once we model an inlining-cost policy
        // in SmallFunctionsConfig): do not inline a function whose body invokes another
        // function whose recursion status we cannot prove safe. Calls to primitive
        // Pine_kernel / Pine_builtin operators, calls into natively-implemented modules,
        // constructor applications, and calls whose head resolves to a known non-recursive
        // (and non-mutually-recursive) top-level function are allowed.
        if (BodyContainsCallToPotentiallyRecursiveFunction(funcBody.Value, context))
        {
            return false;
        }

        if (CalleeBodyHasMultiArmCase(funcBody.Value))
        {
            return false;
        }

        var nodeCount = 0;

        foreach (var node in SyntaxTypes.SyntaxAnalysis.EnumerateDescendantsNodes(funcBody.Value).Prepend(funcBody.Value))
        {
            nodeCount++;

            if (nodeCount > smallConfig.MaxBodyNodeCount)
            {
                return false;
            }
        }

        return true;
    }

    /// <summary>
    /// Returns <see langword="true"/> if <paramref name="body"/> contains a
    /// <see cref="SyntaxTypes.Expression.CaseExpression"/> with more than one case arm
    /// (i.e. a multi-arm <c>case</c> dispatch).
    /// <para>
    /// This is the shared callee-level eligibility predicate used by every inliner
    /// pathway that performs **literal AST inlining** of a callee body
    /// (<see cref="IsSmallEnoughToInline"/>,
    /// <see cref="ShouldInlinePartialApplicationWithCapturedFunction"/>, and the
    /// VarPattern + IsFunctionBearingExpression + BodyUnwrapsParameterAsConstructor
    /// per-arg heuristic inside <see cref="ShouldInline"/>).
    /// Inlining such a callee at every call site duplicates the multi-arm dispatch
    /// into each caller, defeating the original intent of the
    /// <see cref="IsSmallEnoughToInline"/> rule. See
    /// <c>explore/internal-analysis/2026-05-21-inlining-bypasses-small-enough-case-arm-rule.md</c>
    /// for the motivating defect.
    /// </para>
    /// <para>
    /// Inside <see cref="ShouldInline"/> the gate is applied **only** to the
    /// VarPattern + Application heuristic — the lambda-shaped-arg, constructor-pattern
    /// + function-bearing-arg, and data-flow (<see cref="HigherOrderParameterAnalysis"/>)
    /// checks remain unguarded so genuine higher-order callees such as
    /// <c>List.map</c> still inline when their function-typed parameter receives a
    /// lambda or named function-reference.
    /// </para>
    /// <para>
    /// The specialization-sibling-emission pathways
    /// (<see cref="TrySpecializeSingleChoiceTagCall"/>,
    /// <see cref="TrySpecializeNonRecursiveTupleUnwrapCall"/>,
    /// <see cref="TrySpecializeRecursiveCall"/>,
    /// <see cref="TrySpecializeMutualRecursiveGroup"/>)
    /// are deliberately exempt: they emit a single new sibling declaration carrying
    /// the multi-arm <c>case</c> intact, and rewrite the call to reference it.
    /// </para>
    /// </summary>
    private static bool CalleeBodyHasMultiArmCase(SyntaxTypes.Expression body)
    {
        foreach (var node in SyntaxTypes.SyntaxAnalysis.EnumerateDescendantsNodes(body).Prepend(body))
        {
            if (node is SyntaxTypes.Expression.CaseExpression caseExpr && caseExpr.CaseBlock.Cases.Count > 1)
            {
                return true;
            }
        }

        return false;
    }

    /// <summary>
    /// Returns <see langword="true"/> if the expression tree contains an
    /// <see cref="SyntaxTypes.Expression.Application"/> whose head invokes a function
    /// whose recursion status we cannot rule out. Constructor applications (head is a
    /// <see cref="SyntaxTypes.Expression.FunctionOrValue"/> whose name starts with an
    /// uppercase letter), primitive operator applications (head module is
    /// <c>Pine_kernel</c> or <c>Pine_builtin</c>), references into natively-implemented
    /// modules (e.g. <c>Basics</c>), and calls whose head resolves to a known
    /// non-recursive (and non-mutually-recursive) top-level function do not count,
    /// since they are structural, kernel, native-implementation, or definitely-terminating
    /// operations.
    /// </summary>
    private static bool BodyContainsCallToPotentiallyRecursiveFunction(
        SyntaxTypes.Expression expr,
        InliningContext context)
    {
        var worklist = new Stack<SyntaxTypes.Expression>();
        worklist.Push(expr);

        while (worklist.Count > 0)
        {
            var current = worklist.Pop();

            if (current is SyntaxTypes.Expression.Application app && app.Arguments.Count > 0)
            {
                var head = app.Arguments[0].Value;

                if (!IsSafeApplicationHead(head, context))
                {
                    return true;
                }
            }

            SyntaxTypes.SyntaxAnalysis.ForEachChildExpression(current, worklist.Push);
        }

        return false;
    }

    private static bool IsSafeApplicationHead(
        SyntaxTypes.Expression head,
        InliningContext context)
    {
        if (head is not SyntaxTypes.Expression.FunctionOrValue funcOrValue)
        {
            return false;
        }

        // Constructor reference (capitalized name) — structural, not a function call.
        if (funcOrValue.Name.Length > 0 && char.IsUpper(funcOrValue.Name[0]))
        {
            return true;
        }

        // Primitive kernel / builtin operator, or natively-implemented module
        // (currently just "Basics", mirroring the
        // s_nativelyImplementedModuleNames set in ElmCompiler) — not a user-defined
        // Elm function call whose body could recursively expand during inlining.
        if (funcOrValue.ModuleName.Count is 1 &&
            funcOrValue.ModuleName[0] is "Pine_kernel" or "Pine_builtin" or "Basics")
        {
            return true;
        }

        // Relaxed case: the head resolves to a top-level function that is known to be
        // neither directly nor mutually recursive. Inlining a body that calls such a
        // function cannot trigger unbounded code-size expansion, so we allow it.
        // Correctness relies on FunctionInfo.IsRecursive being populated by
        // MarkRecursiveFunctions for every function in the inlining dictionary; an
        // incorrectly false IsRecursive could lead to unbounded inlining expansion.
        if (TryResolveKnownFunctionReference(funcOrValue, context) is { } resolved &&
            !resolved.FunctionInfo.IsRecursive)
        {
            return true;
        }

        return false;
    }

    /// <summary>
    /// Detects the "wrapper-with-captured-function partial application" pattern:
    /// the call is a partial application of a non-recursive function, at least one of the
    /// supplied prefix arguments is a statically-known function expression bound to a
    /// <see cref="SyntaxTypes.Pattern.VarPattern"/>, and that parameter is forwarded as an
    /// argument to a recursive callee inside the body.
    /// <para>
    /// When this pattern matches, inlining the wrapper at the call site (substituting only
    /// the supplied prefix args and emitting a lambda for the remaining params) exposes a
    /// recursive call where the function-typed parameter is now a literal known function,
    /// which then triggers the existing <see cref="TrySpecializeRecursiveCall"/> machinery
    /// to materialize a first-order specialized variant of the recursive helper.
    /// </para>
    /// <para>
    /// This is the primary mechanism that gives lambda-lifted wrappers (such as the
    /// post-lambda-lifting <c>whileWithoutLinebreak__lifted__lambda1</c>) the ability to
    /// propagate their captured function into the recursive helper they wrap.
    /// </para>
    /// </summary>
    private static bool ShouldInlinePartialApplicationWithCapturedFunction(
        FunctionInfo funcInfo,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        InliningContext context)
    {
        if (funcInfo.IsRecursive)
            return false;

        if (!context.Config.IncludeHigherOrder)
            return false;

        // Don't duplicate a multi-arm case dispatch across every call site —
        // see <see cref="CalleeBodyHasMultiArmCase"/> and the analysis doc
        // explore/internal-analysis/2026-05-21-inlining-bypasses-small-enough-case-arm-rule.md.
        // This is the pathway that previously inlined <c>Rope.prependTo</c>
        // (and similar 2-arm-case helpers like <c>Rope.filledPrependTo</c>) into
        // larger parser bodies via partial-application capture.
        if (CalleeBodyHasMultiArmCase(funcImpl.Expression.Value))
            return false;

        var funcParams = funcImpl.Arguments;

        // Only fires for partial applications. Full and over-applications are handled by
        // the existing ShouldInline path.
        if (appArgs.Count >= funcParams.Count)
            return false;

        if (appArgs.Count is 0)
            return false;

        for (var i = 0; i < appArgs.Count; i++)
        {
            if (funcParams[i].Value is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                if (!IsFunctionExpression(appArgs[i].Value, context))
                    continue;

                if (BodyForwardsParameterToRecursiveCallArgument(
                    funcImpl.Expression.Value,
                    varPattern.Name,
                    context))
                {
                    return true;
                }

                // §7.6-adjacent extension (planning doc
                // 2026-05-18-expand-elm-syntax-optimizations-for-higher-order-parameter-elimination.md):
                // for a non-recursive callee whose body is bounded in size,
                // also fire when the captured-function parameter is
                // *directly applied* in the body — i.e. the parameter is a
                // higher-order parameter in the
                // <see cref="HigherOrderParameterAnalysis"/> sense. Inlining
                // the wrapper here substitutes the literal-function arg
                // into the body, and after re-lifting the resulting
                // lambda the surviving HO parameter is gone in the
                // specialised variant.
                // <para>
                // This is the route by which fixture E's
                // <c>Test.lazy__lifted__lambda1 Test.rec2</c> (and the
                // <c>rec1</c> mirror) gets eliminated: substituting
                // <c>thunk := Test.rec2</c> into
                // <c>let (Parser parse) = thunk () in parse s</c> yields
                // <c>let (Parser parse) = Test.rec2 () in parse s</c>,
                // which subsequent size-based inlining + WUC reduce to
                // <c>Test.rec2__stripped () s</c>.
                // </para>
                // <para>
                // Gated on a hard body-size cap (40 AST nodes) — the
                // dominant production wrappers (<c>lazy__lifted__lambda1</c>
                // at ~10 nodes, <c>oneOf2__lifted__lambda1</c> at ~14)
                // are well under this; long-bodied higher-order helpers
                // are excluded to keep specialization counts bounded.
                // </para>
                // <para>
                // <b>Perf tradeoff (documented for review):</b> this
                // broadened gate fires on the canonical
                // <c>lazy</c>/<c>oneOf2</c>/<c>map</c>/<c>andThen</c>-shaped
                // parser combinators in real <c>ParserFast</c> code,
                // not just in the focused fixture E synthetic. The
                // <c>ElmParserExpressionTests</c> perf-counter
                // snapshots shift accordingly:
                // <c>InvocationCount</c> drops 20–30%
                // (e.g. <c>54 → 41</c> for <c>Expression_int_literal</c>),
                // but <c>BuildListCount</c> rises 50–70%
                // (<c>100 → 170</c> for the same fixture) because each
                // specialised sibling allocates a fresh closure
                // environment at runtime. Net <c>InstructionCount</c>
                // impact is mixed — see updated snapshots in
                // <c>ElmParserExpressionTests</c>.
                // </para>
                if (SyntaxTypes.SyntaxAnalysis.CountExpressionNodes(funcImpl.Expression.Value) <= 40 &&
                    BodyAppliesParameterDirectly(
                        funcImpl.Expression.Value,
                        varPattern.Name))
                {
                    return true;
                }

                continue;
            }

            // Tuple-pattern parameter destructured into var-bound names
            // (e.g. lambda-lifted bodies of the form
            // <c>oneOf2__lifted__lambda1 ( pA, pB ) s = …</c>): when
            // the corresponding arg is a literal <c>TupledExpression</c>
            // whose elements are themselves function-typed expressions
            // and at least one tuple-bound name is applied directly in
            // the body, inlining the wrapper substitutes each tuple
            // element for the corresponding bound name via the
            // matched-tuple path in <c>InlineFunctionCallCore</c>.
            // After re-lifting, the surviving HO findings on the
            // tuple-bound names are gone in the specialised variant.
            // Same body-size and HO-application gates as the
            // var-pattern branch above. Tuple-of-functions parameters
            // are rare in real code; this branch primarily fires on
            // <c>WrapperReturnStripping</c>-generated
            // <c>__stripped</c> bodies (fixture E shape).
            if (SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(funcParams[i].Value)
                    is SyntaxTypes.Pattern.TuplePattern tuplePattern
                && SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(appArgs[i].Value)
                    is SyntaxTypes.Expression.TupledExpression tupleArg
                && tuplePattern.Elements.Count == tupleArg.Elements.Count
                && SyntaxTypes.SyntaxAnalysis.CountExpressionNodes(funcImpl.Expression.Value) <= 40)
            {
                var anyTupleElemIsFunctionAndAppliedInBody = false;

                for (var j = 0; j < tuplePattern.Elements.Count; j++)
                {
                    if (tuplePattern.Elements[j].Value is not SyntaxTypes.Pattern.VarPattern elemVar)
                        continue;

                    if (!IsFunctionExpression(tupleArg.Elements[j].Value, context))
                        continue;

                    if (BodyAppliesParameterDirectly(
                        funcImpl.Expression.Value,
                        elemVar.Name))
                    {
                        anyTupleElemIsFunctionAndAppliedInBody = true;
                        break;
                    }
                }

                if (anyTupleElemIsFunctionAndAppliedInBody)
                    return true;
            }
        }

        return false;
    }

    /// <summary>
    /// Returns true iff <paramref name="expr"/> contains an
    /// <see cref="SyntaxTypes.Expression.Application"/> whose head
    /// (after peeling parens) is a bare local reference to
    /// <paramref name="paramName"/>. Used by the §7.6-adjacent
    /// extension of <see cref="ShouldInlinePartialApplicationWithCapturedFunction"/>
    /// to recognise that a parameter is higher-order
    /// (function-typed, applied as a function inside the body).
    /// Scope-naïve by design — re-binding the same name in a nested
    /// scope would produce a false positive, but
    /// <see cref="LambdaLifting"/> guarantees lifted bodies don't
    /// re-bind their parameters, which is the only call shape this
    /// gate is intended to fire on.
    /// </summary>
    private static bool BodyAppliesParameterDirectly(
        SyntaxTypes.Expression expr,
        string paramName)
    {
        var stack = new Stack<SyntaxTypes.Expression>();
        stack.Push(expr);

        while (stack.Count > 0)
        {
            var current = stack.Pop();

            if (current is SyntaxTypes.Expression.Application app && app.Arguments.Count >= 2)
            {
                var head = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value);

                if (head is SyntaxTypes.Expression.FunctionOrValue headRef &&
                    headRef.ModuleName.Count is 0 &&
                    string.Equals(headRef.Name, paramName, StringComparison.Ordinal))
                {
                    return true;
                }
            }

            SyntaxTypes.SyntaxAnalysis.ForEachChildExpression(current, stack.Push);
        }

        return false;
    }

    /// <summary>
    /// Returns true if <paramref name="expr"/> contains an <see cref="SyntaxTypes.Expression.Application"/>
    /// whose head resolves to a recursive top-level function and whose argument list contains a bare
    /// reference to <paramref name="paramName"/>. This indicates that substituting a literal function
    /// for that parameter would produce a higher-order recursive call eligible for
    /// <see cref="TrySpecializeRecursiveCall"/>.
    /// </summary>
    private static bool BodyForwardsParameterToRecursiveCallArgument(
        SyntaxTypes.Expression expr,
        string paramName,
        InliningContext context)
    {
        var stack = new Stack<SyntaxTypes.Expression>();
        stack.Push(expr);

        while (stack.Count > 0)
        {
            var current = stack.Pop();

            if (current is SyntaxTypes.Expression.Application app && app.Arguments.Count >= 2)
            {
                var head = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value);

                if (head is SyntaxTypes.Expression.FunctionOrValue headRef &&
                    TryResolveKnownFunctionReference(headRef, context) is { } resolved &&
                    resolved.FunctionInfo.IsRecursive)
                {
                    for (var i = 1; i < app.Arguments.Count; i++)
                    {
                        var unwrappedArg = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[i].Value);

                        if (unwrappedArg is SyntaxTypes.Expression.FunctionOrValue argRef &&
                            argRef.ModuleName.Count is 0 &&
                            argRef.Name == paramName)
                        {
                            return true;
                        }
                    }
                }
            }

            SyntaxTypes.SyntaxAnalysis.ForEachChildExpression(current, stack.Push);
        }

        return false;
    }


    /// <summary>
    /// Tries to inline a <see cref="SyntaxTypes.Expression.FunctionOrValue"/> reference
    /// that resolves to a plain value declaration (zero parameters).
    /// Plain values whose body is a literal-like expression are always inlined,
    /// replacing the reference with the body expression.
    /// Returns null if the reference cannot be inlined (unknown, recursive, has parameters,
    /// or body is too complex to substitute safely in any expression position).
    /// </summary>
    private static SyntaxTypes.Expression? TryInlinePlainValue(
        SyntaxTypes.Expression.FunctionOrValue funcOrValue,
        InliningContext context)
    {
        // Plain value inlining is enabled only when the config opts in.
        if (!context.Config.IncludePlainValues)
            return null;

        if (TryResolveKnownFunctionReference(funcOrValue, context) is not { } resolved)
            return null;

        var funcInfo = resolved.FunctionInfo;
        var funcImpl = funcInfo.Function.Declaration.Value;

        // Only inline zero-parameter declarations (plain values)
        if (funcImpl.Arguments.Count is not 0)
            return null;

        // Skip recursive values to avoid infinite expansion
        if (funcInfo.IsRecursive)
            return null;

        // Skip if we're already in the process of inlining this value (prevents infinite recursion)
        if (context.InliningStack.Contains(resolved.QualifiedName))
            return null;

        var body = funcImpl.Expression.Value;

        // Only inline expressions that are safe to substitute in any expression position
        // without requiring parenthesization or risking syntax errors.
        if (!ElmSyntaxTransformations.IsPlainValueSafeToInline(body))
            return null;

        return body;
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

        if (!context.Config.IncludeHigherOrder)
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
            if (ElmSyntaxTransformations.IsConstructorPattern(param) && IsFunctionBearingExpression(arg, context))
            {
                return true;
            }

            // The VarPattern + IsFunctionBearingExpression + BodyUnwrapsParameterAsConstructor
            // heuristic treats an Application-shaped argument supplied to a case-matched
            // parameter as "function-bearing enough" to justify inlining. That heuristic was
            // intended for genuinely higher-order helpers but, because Application is the
            // most common syntactic shape for any value-producing expression, it also fires
            // for purely data-consuming callees such as Rope.prependTo (its body
            // case-matches `left`, and pipelines like `commentsBefore |> Rope.prependTo …`
            // supply Application-shaped arguments). For callees whose body contains a
            // multi-arm `case` we must not perform that literal body inlining — it
            // duplicates the dispatch at every call site, defeating the original case-arm
            // rule from <see cref="IsSmallEnoughToInline"/>. See
            // <c>explore/internal-analysis/2026-05-21-inlining-bypasses-small-enough-case-arm-rule.md</c>.
            // The lambda-shaped arg, constructor-pattern arg, and data-flow checks below
            // remain unguarded so genuine higher-order callees (e.g. List.map) still inline.
            if (param is SyntaxTypes.Pattern.VarPattern varPattern &&
                IsFunctionBearingExpression(arg, context) &&
                ElmSyntaxTransformations.BodyUnwrapsParameterAsConstructor(funcBody, varPattern.Name) &&
                !CalleeBodyHasMultiArmCase(funcBody.Value))
            {
                return true;
            }

            // Callee-driven detection (data-flow): if any name introduced by
            // this parameter pattern flows — through any chain of let-bindings,
            // lambdas, case branches, etc. — into the function position of at
            // least one Application in the callee body, the callee is
            // higher-order in this parameter regardless of the call-site
            // argument's syntactic shape and regardless of whether the
            // parameter pattern is a bare variable, a constructor pattern, a
            // tuple/record pattern, or any other pattern that introduces names.
            // Example: `Helpers.apply f x = f x` (var pattern) and
            // `runParser (Parser p) input = p input` (constructor pattern with
            // an inner var) both qualify. See <see cref="HigherOrderParameterAnalysis"/>
            // / <see cref="ElmExpressionDataFlow"/> for the underlying analysis.
            if (HigherOrderParameterAnalysis
                .FindHigherOrderNamesIntroducedByParameter(param, funcBody.Value)
                .Count > 0)
            {
                return true;
            }
        }

        return false;
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

            SyntaxTypes.Expression.Application app =>
            ApplicationReturnsFunction(app, context),

            SyntaxTypes.Expression.ParenthesizedExpression paren =>
            IsFunctionExpression(paren.Expression.Value, context),

            _ =>
            false
        };
    }

    private static bool ApplicationReturnsFunction(
        SyntaxTypes.Expression.Application app,
        InliningContext context)
    {
        if (app.Arguments.Count is 0 ||
            SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value) is not SyntaxTypes.Expression.FunctionOrValue funcOrValue ||
            TryGetFunctionReferenceInferredType(funcOrValue, context) is not { } inferredType)
        {
            return false;
        }

        var resultType = inferredType;

        for (var i = 1; i < app.Arguments.Count && resultType is TypeInference.InferredType.FunctionType functionType; i++)
        {
            resultType = functionType.ReturnType;
        }

        return resultType is TypeInference.InferredType.FunctionType;
    }

    private static TypeInference.InferredType? TryGetFunctionReferenceInferredType(
        SyntaxTypes.Expression.FunctionOrValue funcOrValue,
        InliningContext context)
    {
        if (funcOrValue.ModuleName.Count > 0)
        {
            var qualifiedName = string.Join(".", funcOrValue.ModuleName) + "." + funcOrValue.Name;

            if (context.Resolution.FunctionSignatures.TryGetValue(qualifiedName, out var inferredType))
            {
                return inferredType;
            }
        }

        if (funcOrValue.ModuleName.Count is 0 &&
            context.Resolution.CurrentModuleName is { } currentModuleName)
        {
            var qualifiedName = string.Join(".", currentModuleName) + "." + funcOrValue.Name;

            if (context.Resolution.FunctionSignatures.TryGetValue(qualifiedName, out var inferredType))
            {
                return inferredType;
            }
        }

        return null;
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
            context.Resolution.FunctionsByQualifiedName.ContainsKey(
                DeclQualifiedName.Create(funcOrValue.ModuleName, funcOrValue.Name)))
        {
            return true;
        }

        if (funcOrValue.ModuleName.Count is 0 &&
            context.Resolution.CurrentModuleName is { } currentModuleName &&
            context.Resolution.FunctionsByQualifiedName.ContainsKey(
                DeclQualifiedName.Create(currentModuleName, funcOrValue.Name)))
        {
            return true;
        }

        // Type-based check: look up the qualified name in function signatures.
        // If the type annotation says this is a FunctionType, it's a function.
        if (funcOrValue.ModuleName.Count > 0)
        {
            var qualifiedName = string.Join(".", funcOrValue.ModuleName) + "." + funcOrValue.Name;

            if (context.Resolution.FunctionSignatures.TryGetValue(qualifiedName, out var inferredType) &&
                inferredType is TypeInference.InferredType.FunctionType)
            {
                return true;
            }
        }

        if (funcOrValue.ModuleName.Count is 0 &&
            context.Resolution.CurrentModuleName is { } currentModuleNameForSignature)
        {
            var qualifiedName = string.Join(".", currentModuleNameForSignature) + "." + funcOrValue.Name;

            if (context.Resolution.FunctionSignatures.TryGetValue(qualifiedName, out var inferredType) &&
                inferredType is TypeInference.InferredType.FunctionType)
            {
                return true;
            }
        }

        if (IsKnownCoreFunctionReference(funcOrValue))
        {
            return true;
        }

        return false;
    }

    private static bool IsKnownCoreFunctionReference(
        SyntaxTypes.Expression.FunctionOrValue funcOrValue)
    {
        if (funcOrValue.ModuleName.Count is 0)
        {
            return funcOrValue.Name is "::";
        }

        if (funcOrValue.ModuleName.Count is 1 &&
            funcOrValue.ModuleName[0] is "List" &&
            funcOrValue.Name is "reverse")
        {
            return true;
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

    private static InliningResult InlineFunctionCall(
        ModuleName calleeModuleName,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> args,
        InliningContext context)
    {
        return InlineFunctionCallCore(calleeModuleName, funcImpl, args, context, recursivelyInlineBody: true);
    }

    /// <summary>
    /// Inlines a small function by substituting parameters with arguments but WITHOUT
    /// recursively inlining the resulting body. This prevents cascading inlining where
    /// size-based inlining exposes new call patterns that trigger higher-order inlining,
    /// which can disrupt lambda lifting's variable capture.
    /// </summary>
    private static InliningResult InlineSmallFunctionCall(
        ModuleName calleeModuleName,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> args,
        InliningContext context)
    {
        return InlineFunctionCallCore(calleeModuleName, funcImpl, args, context, recursivelyInlineBody: false);
    }

    private static InliningResult InlineFunctionCallCore(
        ModuleName calleeModuleName,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> args,
        InliningContext context,
        bool recursivelyInlineBody)
    {
        // Build cross-module qualification context when the callee is from a different module.
        // This is passed to RenameBindingsAvoidingCapture so that local-renaming and module-level
        // qualification happen in a single pass. Combining them avoids an ordering conflict:
        // renaming can change local bindings, so a subsequent separate qualification pass
        // wouldn't know which names were originally local vs. module-level references.
        var crossModuleQualification = BuildCrossModuleQualification(calleeModuleName, context);

        var newDecls = ImmutableList.CreateBuilder<Node<SyntaxTypes.Declaration>>();

        // First, recursively inline the arguments (using the original context,
        // before we add the function parameter names to scope). These must be
        // inlined BEFORE we rename the callee's bindings, because the rename
        // must additionally avoid any free variable that appears in the
        // substituted-in argument expressions — otherwise a callee binding
        // whose spelling matches a free variable in an argument would capture
        // that reference after substitution, producing wrong code.
        var inlinedArgs = new List<Node<SyntaxTypes.Expression>>(args.Count);

        foreach (var arg in args)
        {
            var (inlinedArg, argDecls) = InlineExpression(arg, context);
            newDecls.AddRange(argDecls);
            inlinedArgs.Add(inlinedArg);
        }

        // Names that any fresh binding the inliner introduces must avoid:
        //   (outer scope ∪ module-level) ∪ ⋃ FV(inlinedArg_i)
        // Centralized helper — see NamesToAvoidForFreshBindingsIncludingArgFreeVars.
        var namesToAvoid =
            NamesToAvoidForFreshBindingsIncludingArgFreeVars(context, inlinedArgs);

        if (namesToAvoid.Count > 0 || crossModuleQualification is not null)
        {
            funcImpl =
                ElmSyntaxTransformations.RenameBindingsAvoidingCapture(
                    funcImpl,
                    namesToAvoid,
                    crossModuleQualification);
        }

        var funcParams = funcImpl.Arguments;
        var funcBody = funcImpl.Expression;

        // Collect all binding names introduced by the function parameters.
        // These must be added to the context so that nested inlining (of the body)
        // knows to avoid reusing these names — preventing name clashes when the same
        // callee is inlined multiple times in nested scopes.
        var introducedParamBindings = new HashSet<string>();

        foreach (var param in funcParams)
        {
            ElmSyntaxTransformations.CollectPatternNamesRecursive(
                param.Value,
                introducedParamBindings);
        }

        var bodyContext =
            introducedParamBindings.Count > 0
            ?
            context with { LocalNames = context.LocalNames.Union(introducedParamBindings) }
            :
            context;

        Node<SyntaxTypes.Expression> Inline(Node<SyntaxTypes.Expression> e)
        {
            var (result, decls) = InlineExpression(e, bodyContext);
            newDecls.AddRange(decls);
            return result;
        }

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
            else if (SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(param.Value) is SyntaxTypes.Pattern.AllPattern)
            {
                // Wildcard patterns do not bind anything, so there is no need to retain a
                // generated let-destructuring binding for them in the inlined body.
            }
            else if (TryDeconstructKnownConstructorApplication(arg, context) is { } knownCtorApp &&
                ElmSyntaxTransformations.TryBindSingleChoiceTagPattern(
                    param.Value,
                    knownCtorApp.ConstructorName,
                    knownCtorApp.FieldExpressions) is { } patternBindings)
            {
                foreach (var binding in patternBindings)
                {
                    substitutions[binding.Key] = binding.Value;
                }

                if (ElmSyntaxTransformations.TryGetAliasNameFromPattern(param.Value) is { } aliasName)
                {
                    substitutions[aliasName] =
                        ElmSyntaxTransformations.BuildConstructorApplication(
                            knownCtorApp.ConstructorName,
                            knownCtorApp.FieldExpressions);
                }
            }
            else if (SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(param.Value) is SyntaxTypes.Pattern.TuplePattern tuplePattern &&
                arg.Value is SyntaxTypes.Expression.TupledExpression tupleArg &&
                tuplePattern.Elements.Count == tupleArg.Elements.Count)
            {
                // Tuple pattern matched with tuple expression — directly substitute each element.
                // This avoids creating a let-destructuring like `let (a,b) = (x,y)` that can
                // shadow outer variables when the pattern names match the expression's references,
                // which breaks FindFreeVariables and lambda lifting capture analysis.
                for (var j = 0; j < tuplePattern.Elements.Count; j++)
                {
                    var elemPattern = tuplePattern.Elements[j].Value;
                    var elemArg = tupleArg.Elements[j];

                    if (elemPattern is SyntaxTypes.Pattern.VarPattern elemVar)
                    {
                        substitutions[elemVar.Name] = elemArg;
                    }
                    else if (elemPattern is SyntaxTypes.Pattern.AllPattern)
                    {
                        // Wildcard — skip
                    }
                    else
                    {
                        // Nested non-trivial pattern inside tuple — fall back to let-destructuring
                        // for the entire tuple to keep pattern matching intact.
                        var letDestr =
                            new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                                Pattern: param,
                                Expression: arg);

                        letDeclarations.Add(
                            new Node<SyntaxTypes.Expression.LetDeclaration>(ElmSyntaxTransformations.s_zeroRange, letDestr));

                        goto nextParam;
                    }
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

                letDeclarations.Add(
                    new Node<SyntaxTypes.Expression.LetDeclaration>(ElmSyntaxTransformations.s_zeroRange, letDestr));
            }

nextParam:;
        }

        // Substitute in the function body
        var substitutedBody = ElmSyntaxTransformations.SubstituteInExpression(funcBody, substitutions);

        var qualifiedLiftedHelperReferences =
            QualifyLiftedHelperReferencesFromCalleeModule(
                substitutedBody,
                calleeModuleName,
                context);

        // Recursively inline in the substituted body (unless this is a size-based inlining
        // where we skip recursive inlining to prevent cascading that can break lambda lifting).
        var inlinedBody =
            recursivelyInlineBody
            ?
            Inline(qualifiedLiftedHelperReferences)
            :
            qualifiedLiftedHelperReferences;

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
                    new Node<SyntaxTypes.Expression>(ElmSyntaxTransformations.s_zeroRange, resultExpr),
                    bodyContext);

            newDecls.AddRange(normalizedDecls);
            resultExpr = normalizedResult.Value;
        }

        if (namesToAvoid.Count > 0)
        {
            resultExpr =
                ElmSyntaxTransformations.RenameBindingsAvoidingCapture(
                    new Node<SyntaxTypes.Expression>(ElmSyntaxTransformations.s_zeroRange, resultExpr),
                    namesToAvoid)
                .Value;
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
                    new(ElmSyntaxTransformations.s_zeroRange, resultExpr)
                };

            allArgs.AddRange(remainingArgs);

            return new InliningResult(new SyntaxTypes.Expression.Application([.. allArgs]), newDecls.ToImmutable());
        }

        // If we have fewer arguments than parameters, we create a partial application (lambda)
        if (args.Count < funcParams.Count)
        {
            var remainingParams = funcParams.Skip(args.Count).ToList();

            return
                new InliningResult(
                    new SyntaxTypes.Expression.LambdaExpression(
                        new SyntaxTypes.LambdaStruct(
                            Arguments: remainingParams,
                            Expression:
                            new Node<SyntaxTypes.Expression>(ElmSyntaxTransformations.s_zeroRange, resultExpr))),
                    newDecls.ToImmutable());
        }

        return new InliningResult(resultExpr, newDecls.ToImmutable());
    }

    private sealed record SingleChoiceTagFieldPlan(
        int FieldIndex,
        string? BoundVariableName,
        ParameterSpecialization? ParameterSpecialization,
        IReadOnlyList<Node<SyntaxTypes.Pattern>> FlattenedParameters,
        IReadOnlyList<Node<SyntaxTypes.Expression>> FlattenedActualArguments,
        Node<SyntaxTypes.Expression> ReplacementExpression,
        IReadOnlyList<SingleChoiceTagFieldPlan> NestedFieldPlans);

    private sealed record SingleChoiceTagSpecialization(
        int ParamIndex,
        SyntaxTypes.QualifiedNameRef ConstructorName,
        IReadOnlyList<SingleChoiceTagFieldPlan> FieldPlans,
        string ParamName)
    {
        public Node<SyntaxTypes.Expression> ReplacementArgument =>
            ElmSyntaxTransformations.BuildConstructorApplication(
                ConstructorName,
                [.. FieldPlans.Select(plan => plan.ReplacementExpression)]);
    }

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
            var fieldSpecialization =
                ParameterSpecialization.ClassifyArgument(actualFieldExpression.Value) ??
                new ParameterSpecialization.ConcreteLambdaValue(
                    new SyntaxTypes.LambdaStruct(
                        Arguments: [],
                        Expression: actualFieldExpression));

            return
                new SingleChoiceTagFieldPlan(
                    FieldIndex: fieldIndex,
                    BoundVariableName: fieldName,
                    ParameterSpecialization: fieldSpecialization,
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
            context) is { } nestedFieldPlan)
        {
            return nestedFieldPlan;
        }

        var replacementParameter =
            new Node<SyntaxTypes.Pattern>(
                ElmSyntaxTransformations.s_zeroRange,
                new SyntaxTypes.Pattern.VarPattern(fieldName));

        var replacementExpression =
            new Node<SyntaxTypes.Expression>(
                ElmSyntaxTransformations.s_zeroRange,
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

    private static SingleChoiceTagFieldPlan? TryBuildNestedSingleChoiceTagFieldPlan(
        int fieldIndex,
        string fieldName,
        Node<SyntaxTypes.Expression> actualFieldExpression,
        ModuleName currentModuleName,
        InliningContext context)
    {
        if (TryDeconstructKnownConstructorApplicationForSpecialization(
                actualFieldExpression,
                context) is not { } specCtorApp)
        {
            return null;
        }

        var constructorName = specCtorApp.ConstructorName;
        var actualFieldExpressions = specCtorApp.FieldExpressions;

        if (constructorName.ModuleName.Count is 0)
        {
            constructorName = new SyntaxTypes.QualifiedNameRef(currentModuleName, constructorName.Name);
        }

        if (!context.Resolution.SingleChoiceConstructors.TryGetValue(constructorName, out var constructorInfo) ||
            constructorInfo.FieldCount != actualFieldExpressions.Count)
        {
            return null;
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
            ImmutableDictionary.CreateBuilder<int, ParameterSpecialization>();

        foreach (var nestedFieldPlan in nestedFieldPlans)
        {
            if (nestedFieldPlan.ParameterSpecialization is { } nestedSpecialization)
            {
                nestedSpecFields[nestedFieldPlan.FieldIndex] = nestedSpecialization;
            }
        }

        return
            new SingleChoiceTagFieldPlan(
                FieldIndex: fieldIndex,
                BoundVariableName: fieldName,
                ParameterSpecialization:
                new ParameterSpecialization.SingleChoiceTagUnwrap(
                    DeclQualifiedName.Create(constructorName.ModuleName, constructorName.Name),
                    nestedSpecFields.ToImmutable()),
                FlattenedParameters: [.. nestedFieldPlans.SelectMany(plan => plan.FlattenedParameters)],
                FlattenedActualArguments: [.. nestedFieldPlans.SelectMany(plan => plan.FlattenedActualArguments)],
                ReplacementExpression:
                ElmSyntaxTransformations.BuildConstructorApplication(
                    constructorName,
                    [.. nestedFieldPlans.Select(plan => plan.ReplacementExpression)]),
                NestedFieldPlans: nestedFieldPlans);
    }

    private static InliningResult? TrySpecializeSingleChoiceTagCall(
        FunctionInfo funcInfo,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        InliningContext context)
    {
        if (appArgs.Count < funcImpl.Arguments.Count)
            return null;

        var specialization =
            TryBuildSingleChoiceTagSpecialization(
                funcImpl,
                appArgs,
                funcInfo,
                context);

        if (specialization is null)
            return null;

        // Look up the pre-assigned name from the specialization catalog
        var specializedName =
            LookupSpecializedName(
                DeclQualifiedName.Create(funcInfo.ModuleName, funcImpl.Name.Value),
                specialization.ParamIndex,
                BuildSingleChoiceTagUnwrapSpec(specialization, context),
                context);

        if (specializedName is null)
            return null;

        var specializedModuleName = context.Resolution.CurrentModuleName ?? funcInfo.ModuleName;

        var rewrittenBody =
            funcInfo.IsRecursive
            ?
            RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                funcImpl.Expression,
                funcImpl.Name.Value,
                funcInfo.ModuleName,
                specializedModuleName,
                specializedName,
                specialization)
            :
            funcImpl.Expression;

        var substitutedBody =
            ElmSyntaxTransformations.SubstituteInExpression(
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
                ElmSyntaxTransformations.SubstituteInExpression(
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
                Name: new Node<string>(ElmSyntaxTransformations.s_zeroRange, specializedName),
                Arguments: specializedParams,
                Expression: qualifiedBody);

        var specializedFunc =
            new SyntaxTypes.FunctionStruct(
                Documentation: null,
                Signature: null,
                Declaration:
                new Node<SyntaxTypes.FunctionImplementation>(ElmSyntaxTransformations.s_zeroRange, specializedImpl));

        var newDecl =
            new Node<SyntaxTypes.Declaration>(
                ElmSyntaxTransformations.s_zeroRange,
                new SyntaxTypes.Declaration.FunctionDeclaration(specializedFunc));

        var callArgs =
            BuildSingleChoiceTagSpecializedCallArguments(
                specializedModuleName,
                specializedName,
                appArgs,
                funcImpl.Arguments.Count,
                specialization);

        var callExpr =
            callArgs.Count is 1
            ?
            callArgs[0].Value
            :
            new SyntaxTypes.Expression.Application([.. callArgs]);

        return new InliningResult(callExpr, [newDecl]);
    }

    /// <summary>
    /// Materializes a specialized sibling for a non-recursive higher-order
    /// callee whose catalog entry contains at least one
    /// <see cref="ParameterSpecialization.TupleUnwrap"/>, and rewrites the
    /// call site to invoke the sibling with only the non-specialized
    /// arguments. Returns null when the call site doesn't match any catalog
    /// entry or when the entry uses a parameter-spec kind this path doesn't
    /// support.
    /// <para>
    /// Implements the emission half of Step 4 + Step 5 of
    /// <c>explore/internal-analysis/2026-05-19-monomorphizing-expressionAfterOpeningSquareBracket-lifted-lambda3.md</c>.
    /// Discovery is in
    /// <c>ElmSyntaxOptimizationSpecializationCollection.TryBuildTupleUnwrapForArgument</c>.
    /// </para>
    /// </summary>
    private static InliningResult? TrySpecializeNonRecursiveTupleUnwrapCall(
        FunctionInfo funcInfo,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        InliningContext context)
    {
        if (funcInfo.IsRecursive)
            return null;

        if (appArgs.Count < funcImpl.Arguments.Count)
            return null;

        if (!context.SpecializationCatalog.SpecializationsByFunction.TryGetValue(
                DeclQualifiedName.Create(funcInfo.ModuleName, funcImpl.Name.Value),
                out var availableSpecializations))
        {
            return null;
        }

        // Pre-resolve bare-local references through context.LetRhsByName for
        // tuple-shaped arguments so the existing
        // ParameterSpecialization.ArgumentMatchesSpecialization tuple matcher
        // (which does only a paren-peel + one optional let-newtype-wrap peel
        // per element, not a global let-binding lookup) can pair up tuple
        // elements like `pA` with the catalog's
        // TupleUnwrap[ConcreteFunctionValue(parseDouble), ...] entries.
        // This is the emission-side counterpart of the discovery-side
        // ClassifyArgument(arg, letRhsByName) overload introduced by PR C.
        var matchingArgs = ResolveArgsForSpecializationLookup(appArgs, context);

        var bestSpecialization =
            SpecializationCatalog.FindBestSpecialization(
                availableSpecializations,
                [.. matchingArgs.Select(a => a.Value)]);

        if (bestSpecialization is null)
            return null;

        var paramSpecs = bestSpecialization.Specialization.ParameterSpecializations;

        if (!paramSpecs.Values.Any(p => p is ParameterSpecialization.TupleUnwrap))
            return null;

        // All param-spec kinds in this entry must be substitutable to a single
        // concrete callable expression at each leaf.
        foreach (var (idx, spec) in paramSpecs)
        {
            if (idx >= funcImpl.Arguments.Count)
                return null;

            var paramPatternUnwrapped =
                SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(funcImpl.Arguments[idx].Value);

            if (spec is ParameterSpecialization.TupleUnwrap)
            {
                if (paramPatternUnwrapped is not SyntaxTypes.Pattern.TuplePattern)
                    return null;
            }
            else if (spec is ParameterSpecialization.ConcreteFunctionValue
                or ParameterSpecialization.ConcreteLambdaValue
                or ParameterSpecialization.ConcreteRecordAccessFunctionValue)
            {
                if (paramPatternUnwrapped is not SyntaxTypes.Pattern.VarPattern)
                    return null;
            }
            else
            {
                return null;
            }
        }

        var specializedName = bestSpecialization.SpecializedFunctionName;
        var specializedModuleName = context.Resolution.CurrentModuleName ?? funcInfo.ModuleName;

        // Build substitutions and remaining parameter list.
        var substitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();
        var remainingParams = new List<Node<SyntaxTypes.Pattern>>();

        for (var i = 0; i < funcImpl.Arguments.Count; i++)
        {
            if (!paramSpecs.TryGetValue(i, out var spec))
            {
                remainingParams.Add(funcImpl.Arguments[i]);
                continue;
            }

            var paramPatternUnwrapped =
                SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(funcImpl.Arguments[i].Value);

            if (spec is ParameterSpecialization.TupleUnwrap tupleUnwrap)
            {
                var tuplePattern = (SyntaxTypes.Pattern.TuplePattern)paramPatternUnwrapped;

                if (tuplePattern.Elements.Count != tupleUnwrap.ElementSpecializations.Length)
                    return null;

                for (var j = 0; j < tuplePattern.Elements.Count; j++)
                {
                    var elemPattern =
                        SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(tuplePattern.Elements[j].Value);

                    if (elemPattern is not SyntaxTypes.Pattern.VarPattern elemVar)
                        return null;

                    var replacement = BuildReplacementForFunctionSpec(tupleUnwrap.ElementSpecializations[j]);

                    if (replacement is null)
                        return null;

                    substitutions[elemVar.Name] =
                        new Node<SyntaxTypes.Expression>(ElmSyntaxTransformations.s_zeroRange, replacement);
                }
            }
            else if (paramPatternUnwrapped is SyntaxTypes.Pattern.VarPattern vp)
            {
                var replacement = BuildReplacementForFunctionSpec(spec);

                if (replacement is null)
                    return null;

                substitutions[vp.Name] =
                    new Node<SyntaxTypes.Expression>(ElmSyntaxTransformations.s_zeroRange, replacement);
            }
            else
            {
                return null;
            }
        }

        var substitutedBody =
            ElmSyntaxTransformations.SubstituteInExpression(funcImpl.Expression, substitutions);

        var qualifiedBody =
            QualifyLiftedHelperReferencesFromCalleeModule(
                substitutedBody,
                funcInfo.ModuleName,
                context);

        var specializedImpl =
            new SyntaxTypes.FunctionImplementation(
                Name: new Node<string>(ElmSyntaxTransformations.s_zeroRange, specializedName),
                Arguments: [.. remainingParams],
                Expression: qualifiedBody);

        var specializedFunc =
            new SyntaxTypes.FunctionStruct(
                Documentation: null,
                Signature: null,
                Declaration:
                new Node<SyntaxTypes.FunctionImplementation>(ElmSyntaxTransformations.s_zeroRange, specializedImpl));

        var newDecl =
            new Node<SyntaxTypes.Declaration>(
                ElmSyntaxTransformations.s_zeroRange,
                new SyntaxTypes.Declaration.FunctionDeclaration(specializedFunc));

        // Build the call to the specialized sibling: head + remaining args + any over-applied args.
        var callArgs =
            new List<Node<SyntaxTypes.Expression>>
            {
                new(
                    ElmSyntaxTransformations.s_zeroRange,
                    new SyntaxTypes.Expression.FunctionOrValue(specializedModuleName, specializedName))
            };

        for (var i = 0; i < appArgs.Count; i++)
        {
            if (i < funcImpl.Arguments.Count && paramSpecs.ContainsKey(i))
                continue;

            callArgs.Add(appArgs[i]);
        }

        var callExpr =
            callArgs.Count is 1
            ?
            callArgs[0].Value
            :
            new SyntaxTypes.Expression.Application([.. callArgs]);

        return new InliningResult(callExpr, [newDecl]);
    }

    /// <summary>
    /// Builds a single concrete callable expression from a parameter
    /// specialization that classifies a known function value. Returns null
    /// for kinds outside the supported set (e.g. nested
    /// <see cref="ParameterSpecialization.TupleUnwrap"/>, which would
    /// require its own emission path).
    /// </summary>
    private static SyntaxTypes.Expression? BuildReplacementForFunctionSpec(
        ParameterSpecialization spec)
    {
        return spec switch
        {
            ParameterSpecialization.ConcreteFunctionValue cfv =>
            new SyntaxTypes.Expression.FunctionOrValue(
                cfv.FunctionQualifiedName.Namespaces,
                cfv.FunctionQualifiedName.DeclName),

            ParameterSpecialization.ConcreteLambdaValue clv =>
            new SyntaxTypes.Expression.LambdaExpression(clv.Lambda),

            ParameterSpecialization.ConcreteRecordAccessFunctionValue craf =>
            new SyntaxTypes.Expression.RecordAccessFunction(craf.FunctionName),

            _ =>
            null
        };
    }

    /// <summary>
    /// Returns a parallel list of call-site arguments suitable for catalog
    /// lookup via <see cref="SpecializationCatalog.FindBestSpecialization"/>.
    /// For tuple-shaped arguments whose elements are bare local references
    /// present in <see cref="InliningContext.LetRhsByName"/>, the references
    /// are substituted by their resolved RHS expressions; non-tuple arguments
    /// and unrecognized elements are passed through unchanged.
    /// </summary>
    private static IReadOnlyList<Node<SyntaxTypes.Expression>> ResolveArgsForSpecializationLookup(
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        InliningContext context)
    {
        if (context.LetRhsByName.IsEmpty)
            return appArgs;

        var result = new List<Node<SyntaxTypes.Expression>>(appArgs.Count);

        foreach (var arg in appArgs)
        {
            var unwrapped = arg.Value;

            while (unwrapped is SyntaxTypes.Expression.ParenthesizedExpression paren)
                unwrapped = paren.Expression.Value;

            if (unwrapped is SyntaxTypes.Expression.TupledExpression tupled)
            {
                var anyResolved = false;
                var newElements = new List<Node<SyntaxTypes.Expression>>(tupled.Elements.Count);

                foreach (var elem in tupled.Elements)
                {
                    var elemUnwrapped = elem.Value;

                    while (elemUnwrapped is SyntaxTypes.Expression.ParenthesizedExpression elemParen)
                        elemUnwrapped = elemParen.Expression.Value;

                    if (elemUnwrapped is SyntaxTypes.Expression.FunctionOrValue fov &&
                        fov.ModuleName.Count is 0 &&
                        context.LetRhsByName.TryGetValue(fov.Name, out var resolvedRhs))
                    {
                        newElements.Add(new Node<SyntaxTypes.Expression>(elem.Range, resolvedRhs));
                        anyResolved = true;
                    }
                    else
                    {
                        newElements.Add(elem);
                    }
                }

                if (anyResolved)
                {
                    result.Add(
                        new Node<SyntaxTypes.Expression>(
                            arg.Range,
                            new SyntaxTypes.Expression.TupledExpression([.. newElements])));

                    continue;
                }
            }

            result.Add(arg);
        }

        return result;
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

                if (TryDeconstructKnownConstructorApplicationForSpecialization(
                        appArgs[paramIndex],
                        context) is not { } specCtorApp)
                    continue;

                var constructorName = specCtorApp.ConstructorName;
                var actualFieldExpressions = specCtorApp.FieldExpressions;

                if (constructorName.ModuleName.Count is 0)
                {
                    constructorName = new SyntaxTypes.QualifiedNameRef(funcInfo.ModuleName, constructorName.Name);
                }

                // Use type information: verify this constructor belongs to a single-constructor type.
                if (!context.Resolution.SingleChoiceConstructors.TryGetValue(constructorName, out var constructorInfo) ||
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
                        ParamName: varPattern.Name);
            }

            // Case 2: NamedPattern parameter (direct parameter-level destructuring)
            // e.g., alfa factor (MyConstructor x) = ...
            {
                var namedPattern =
                    ElmSyntaxTransformations.TryUnwrapToNamedPattern(funcImpl.Arguments[paramIndex].Value);

                if (namedPattern is null)
                    continue;

                if (TryDeconstructKnownConstructorApplicationForSpecialization(
                        appArgs[paramIndex],
                        context) is not { } specCtorApp)
                    continue;

                var constructorName = specCtorApp.ConstructorName;
                var actualFieldExpressions = specCtorApp.FieldExpressions;

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
                if (!context.Resolution.SingleChoiceConstructors.ContainsKey(constructorName))
                    continue;

                if (namedPattern.Arguments.Count != actualFieldExpressions.Count)
                    continue;

                var fieldPlans = new List<SingleChoiceTagFieldPlan>();
                var allFieldsHandled = true;

                for (var fieldIndex = 0; fieldIndex < actualFieldExpressions.Count; fieldIndex++)
                {
                    var fieldPatternUnwrapped =
                        SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(namedPattern.Arguments[fieldIndex].Value);

                    if (fieldPatternUnwrapped is not SyntaxTypes.Pattern.VarPattern fieldVarPattern)
                    {
                        allFieldsHandled = false;
                        break;
                    }

                    fieldPlans.Add(
                        BuildFieldPlanForVarPatternField(
                            fieldIndex,
                            fieldVarPattern.Name,
                            actualFieldExpressions[fieldIndex],
                            funcInfo.ModuleName,
                            context));
                }

                if (!allFieldsHandled)
                    continue;

                // Use a synthetic param name that won't match any variable in the body,
                // so the body substitution is a no-op unless the pattern uses an alias.
                // If the pattern is an alias pattern like `((Parser parse) as element)`,
                // preserve the alias name so recursive-call rewriting can still match it.
                var syntheticParamName =
                    ElmSyntaxTransformations.TryGetAliasNameFromPattern(funcImpl.Arguments[paramIndex].Value)
                    ?? "__param_" + paramIndex + "_pattern__";

                return
                    new SingleChoiceTagSpecialization(
                        ParamIndex: paramIndex,
                        ConstructorName: constructorName,
                        FieldPlans: fieldPlans,
                        ParamName: syntheticParamName);
            }
        }

        return null;
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
                            ElmSyntaxTransformations.s_zeroRange,
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
        ModuleName moduleName,
        string specializedName,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        int originalParamCount,
        SingleChoiceTagSpecialization specialization)
    {
        var callArgs =
            new List<Node<SyntaxTypes.Expression>>
            {
                new(
                    ElmSyntaxTransformations.s_zeroRange,
                    new SyntaxTypes.Expression.FunctionOrValue(moduleName, specializedName))
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
                            ElmSyntaxTransformations.s_zeroRange,
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



    private static ElmSyntaxTransformations.ConstructorApplication? TryDeconstructKnownConstructorApplication(
        Node<SyntaxTypes.Expression> exprNode,
        InliningContext context,
        bool requireExplicitConstructor = false)
    {
        if ((requireExplicitConstructor
            ?
            ElmSyntaxTransformations.TryDeconstructExplicitConstructorApplication(exprNode.Value)
            :
            ElmSyntaxTransformations.TryDeconstructConstructorApplication(exprNode))
            is { } directResult)
        {
            return directResult;
        }

        switch (exprNode.Value)
        {
            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                return
                    TryDeconstructKnownConstructorApplication(
                        paren.Expression,
                        context,
                        requireExplicitConstructor);

            case SyntaxTypes.Expression.LetExpression letExpr:
                {
                    if (ElmSyntaxTransformations.AreLetDeclarationsIgnorableForConstructorResolution(
                        letExpr.Value.Declarations))
                    {
                        return
                            TryDeconstructKnownConstructorApplication(
                                letExpr.Value.Expression,
                                context,
                                requireExplicitConstructor);
                    }

                    var (inlinedLet, letDecls) = InlineLetExpression(letExpr, context);

                    if (letDecls.Count is 0)
                    {
                        if (inlinedLet is SyntaxTypes.Expression.LetExpression inlinedLetExpr &&
                            ElmSyntaxTransformations.AreLetDeclarationsIgnorableForConstructorResolution(
                                inlinedLetExpr.Value.Declarations))
                        {
                            return
                                TryDeconstructKnownConstructorApplication(
                                    inlinedLetExpr.Value.Expression,
                                    context,
                                    requireExplicitConstructor);
                        }

                        if (!inlinedLet.Equals(exprNode.Value))
                        {
                            return
                                TryDeconstructKnownConstructorApplication(
                                    new Node<SyntaxTypes.Expression>(ElmSyntaxTransformations.s_zeroRange, inlinedLet),
                                    context,
                                    requireExplicitConstructor);
                        }
                    }

                    break;
                }

            case SyntaxTypes.Expression.RecordAccess recordAccess when !requireExplicitConstructor:
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
                                    context);
                        }
                    }

                    break;
                }

            case SyntaxTypes.Expression.Application app
            when app.Arguments.Count > 0 &&
                 app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue funcOrValue &&
                 TryResolveKnownFunctionReference(funcOrValue, context) is { } resolvedFunc &&
                 !resolvedFunc.FunctionInfo.IsRecursive &&
                 !context.InliningStack.Contains(resolvedFunc.QualifiedName):

                {
                    var funcImpl = resolvedFunc.FunctionInfo.Function.Declaration.Value;
                    var appArgs = app.Arguments.Skip(1).ToList();

                    if (appArgs.Count == funcImpl.Arguments.Count)
                    {
                        var newContext =
                            context with
                            {
                                InliningStack = context.InliningStack.Add(resolvedFunc.QualifiedName)
                            };

                        var (inlinedResult, inlinedDecls) =
                            InlineFunctionCall(
                                resolvedFunc.FunctionInfo.ModuleName,
                                funcImpl,
                                appArgs,
                                newContext);

                        if (inlinedDecls.Count is 0 &&
                            !inlinedResult.Equals(exprNode.Value))
                        {
                            return
                                TryDeconstructKnownConstructorApplication(
                                    new Node<SyntaxTypes.Expression>(ElmSyntaxTransformations.s_zeroRange, inlinedResult),
                                    context,
                                    requireExplicitConstructor);
                        }
                    }

                    break;
                }
        }

        return null;
    }

    private static ElmSyntaxTransformations.ConstructorApplication? TryDeconstructKnownConstructorApplicationForSpecialization(
        Node<SyntaxTypes.Expression> exprNode,
        InliningContext context)
    {
        if (TryDeconstructKnownConstructorApplication(exprNode, context) is { } knownResult &&
            knownResult.FieldExpressions.Count > 0)
        {
            return knownResult;
        }

        if (exprNode.Value is not SyntaxTypes.Expression.FunctionOrValue knownFunctionRef ||
            TryResolveKnownFunctionReference(knownFunctionRef, context) is not { } knownResolved ||
            knownResolved.FunctionInfo.IsRecursive ||
            context.InliningStack.Contains(knownResolved.QualifiedName))
        {
            return null;
        }

        var funcImpl = knownResolved.FunctionInfo.Function.Declaration.Value;

        if (funcImpl.Arguments.Count is not 0)
        {
            return null;
        }

        var newContext =
            context with
            {
                InliningStack = context.InliningStack.Add(knownResolved.QualifiedName)
            };

        var (inlinedResult, inlinedDecls) =
            InlineFunctionCall(
                knownResolved.FunctionInfo.ModuleName,
                funcImpl,
                [],
                newContext);

        if (inlinedDecls.Count is 0 &&
            !inlinedResult.Equals(exprNode.Value) &&
            TryDeconstructKnownConstructorApplication(
                new Node<SyntaxTypes.Expression>(ElmSyntaxTransformations.s_zeroRange, inlinedResult),
                context) is { } resolvedCtorApp &&
            resolvedCtorApp.FieldExpressions.Count > 0 &&
            context.Resolution.SingleChoiceConstructors.ContainsKey(resolvedCtorApp.ConstructorName) &&
            resolvedCtorApp.FieldExpressions.Any(
                fieldExpr =>
                SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(fieldExpr.Value) is
                SyntaxTypes.Expression.LambdaExpression))
        {
            return resolvedCtorApp;
        }

        return null;
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
    private static InliningResult? TrySpecializeRecursiveCall(
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
            return null;

        // Skip specialization for very large function bodies to avoid
        // stack overflow in the recursive AST rewriting pass.
        if (SyntaxTypes.SyntaxAnalysis.CountExpressionNodes(funcImpl.Expression.Value) > 2000)
            return null;

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

        NamedSpecialization? bestSpecialization = null;

        if (context.SpecializationCatalog.SpecializationsByFunction.TryGetValue(
            DeclQualifiedName.Create(funcModuleName, funcName),
            out var availableSpecializations))
        {
            bestSpecialization =
                SpecializationCatalog.FindBestSpecialization(
                    availableSpecializations,
                    [.. appArgs.Select(arg => arg.Value)]);
        }

        if (bestSpecialization is null ||
            bestSpecialization.Specialization.SpecializedAwayCount is 0)
        {
            var fallbackParamSpecs =
                ImmutableDictionary.CreateBuilder<int, ParameterSpecialization>();

            foreach (var paramIndex in invariantFuncParamIndices)
            {
                var classified = ParameterSpecialization.ClassifyArgument(appArgs[paramIndex].Value);

                if (classified is not null)
                {
                    fallbackParamSpecs[paramIndex] = classified;
                }
                else
                {
                    fallbackParamSpecs[paramIndex] =
                        new ParameterSpecialization.ConcreteLambdaValue(
                            new SyntaxTypes.LambdaStruct(
                                Arguments: [],
                                Expression: appArgs[paramIndex]));
                }
            }

            var fallbackSpecializedName =
                LookupSpecializedNameForHigherOrder(
                    DeclQualifiedName.Create(funcModuleName, funcName),
                    fallbackParamSpecs.ToImmutable(),
                    context);

            if (fallbackSpecializedName is not null)
            {
                bestSpecialization =
                    new NamedSpecialization(
                        DeclQualifiedName.Create(funcModuleName, funcName),
                        new FunctionSpecialization(fallbackParamSpecs.ToImmutable()),
                        fallbackSpecializedName);
            }
        }

        if (bestSpecialization is null ||
            bestSpecialization.Specialization.SpecializedAwayCount is 0)
        {
            return null;
        }

        var selectedHigherOrderParamIndices =
            bestSpecialization.Specialization.ParameterSpecializations
            .Where(kvp => kvp.Value is not ParameterSpecialization.SingleChoiceTagUnwrap)
            .Select(kvp => kvp.Key)
            .OrderBy(i => i)
            .ToList();

        var selectedHigherOrderParamIndicesSet = new HashSet<int>(selectedHigherOrderParamIndices);

        var selectedSingleChoiceTagParamIndex =
            bestSpecialization.Specialization.ParameterSpecializations
            .Where(kvp => kvp.Value is ParameterSpecialization.SingleChoiceTagUnwrap)
            .Select(kvp => (int?)kvp.Key)
            .FirstOrDefault();

        if (selectedSingleChoiceTagParamIndex is { } singleChoiceTagParamIndex &&
            TryBuildSingleChoiceTagSpecialization(
                funcImpl,
                appArgs,
                funcInfo,
                context) is { } combinedSingleChoiceTagSpecialization &&
            combinedSingleChoiceTagSpecialization.ParamIndex == singleChoiceTagParamIndex)
        {
            return
                TrySpecializeRecursiveCallWithCombinedSingleChoiceTagAndHigherOrder(
                    funcInfo,
                    funcImpl,
                    appArgs,
                    bestSpecialization.SpecializedFunctionName,
                    combinedSingleChoiceTagSpecialization,
                    context);
        }

        var specializedName = bestSpecialization.SpecializedFunctionName;

        // The specialized function will be placed in the current module being processed.
        var specializedModuleName = context.Resolution.CurrentModuleName ?? funcModuleName;

        // Step 1: Rewrite recursive self-calls in the body to use the specialized name
        // and strip the invariant arguments.
        var rewrittenBody =
            RewriteRecursiveCallsInExpression(
                funcImpl.Expression,
                funcName,
                funcModuleName,
                specializedModuleName,
                specializedName,
                selectedHigherOrderParamIndicesSet);

        // Step 2: Substitute invariant parameter names with concrete argument expressions.
        var substitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();

        foreach (var paramIndex in selectedHigherOrderParamIndices)
        {
            var varPattern = (SyntaxTypes.Pattern.VarPattern)funcParams[paramIndex].Value;
            substitutions[varPattern.Name] = appArgs[paramIndex];
        }

        var substitutedBody = ElmSyntaxTransformations.SubstituteInExpression(rewrittenBody, substitutions);

        // Step 3: Qualify lifted helper references from the callee module.
        var qualifiedBody =
            QualifyLiftedHelperReferencesFromCalleeModule(
                substitutedBody,
                funcInfo.ModuleName,
                context);

        // Build remaining parameter list (non-invariant params only).
        var remainingParams =
            funcParams
            .Where((_, i) => !selectedHigherOrderParamIndices.Contains(i))
            .ToImmutableArray();

        // Step 4: Detect free variables introduced by substitution.
        // When we substitute invariant parameters with concrete argument expressions,
        // those expressions may reference local variables from the call site that are
        // not in scope at the module level. We capture these as extra parameters.
        var remainingParamNames =
            remainingParams
            .SelectMany(p => ElmSyntaxTransformations.CollectPatternNames(p.Value))
            .ToHashSet();

        var freeVarsInBody =
            SyntaxTypes.SyntaxAnalysis.CollectRemainingFreeVariables(qualifiedBody.Value)
            .Except(remainingParamNames)
            .Except(context.ModuleLevelNames)
            .Remove(specializedName);

        var capturedVariables = freeVarsInBody.OrderBy(v => v).ToList();

        var finalBody = qualifiedBody;
        var finalParams = remainingParams;

        if (capturedVariables.Count > 0)
        {
            // Add captured variables as extra parameters at the beginning of the parameter list.
            var capturedParams =
                capturedVariables
                .Select(
                    v => new Node<SyntaxTypes.Pattern>(
                        ElmSyntaxTransformations.s_zeroRange,
                        new SyntaxTypes.Pattern.VarPattern(v)))
                .ToList();

            finalParams = [.. capturedParams, .. remainingParams];

            // Add captured variables as extra arguments to all recursive self-calls
            // in the body. The recursive calls were already rewritten to use the
            // specialized name; we now insert the captured args after the function reference.
            finalBody =
                AddExtraArgsToSelfCalls(
                    qualifiedBody,
                    specializedName,
                    specializedModuleName,
                    capturedVariables);
        }

        // Create the specialized function as a module-level declaration.
        var specializedImpl =
            new SyntaxTypes.FunctionImplementation(
                Name: new Node<string>(ElmSyntaxTransformations.s_zeroRange, specializedName),
                Arguments: finalParams,
                Expression: finalBody);

        var specializedFunc =
            new SyntaxTypes.FunctionStruct(
                Documentation: null,
                Signature: null,
                Declaration:
                new Node<SyntaxTypes.FunctionImplementation>(ElmSyntaxTransformations.s_zeroRange, specializedImpl));

        var newDecl =
            new Node<SyntaxTypes.Declaration>(
                ElmSyntaxTransformations.s_zeroRange,
                new SyntaxTypes.Declaration.FunctionDeclaration(specializedFunc));

        // Build the call to the specialized function with remaining (non-invariant) arguments.
        var callArgs =
            new List<Node<SyntaxTypes.Expression>>
            {
                new(
                    ElmSyntaxTransformations.s_zeroRange,
                    new SyntaxTypes.Expression.FunctionOrValue(specializedModuleName, specializedName))
            };

        // Pass captured variables as arguments (they are in scope at the call site).
        foreach (var capturedVar in capturedVariables)
        {
            callArgs.Add(
                new Node<SyntaxTypes.Expression>(
                    ElmSyntaxTransformations.s_zeroRange,
                    new SyntaxTypes.Expression.FunctionOrValue([], capturedVar)));
        }

        for (var i = 0; i < appArgs.Count; i++)
        {
            if (i < funcParams.Count && selectedHigherOrderParamIndices.Contains(i))
                continue;

            callArgs.Add(appArgs[i]);
        }

        var callExpr =
            callArgs.Count is 1
            ?
            callArgs[0].Value
            :
            new SyntaxTypes.Expression.Application([.. callArgs]);

        return new InliningResult(callExpr, [newDecl]);
    }

    private static InliningResult?
        TrySpecializeRecursiveCallWithCombinedSingleChoiceTagAndHigherOrder(
        FunctionInfo funcInfo,
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<Node<SyntaxTypes.Expression>> appArgs,
        string specializedName,
        SingleChoiceTagSpecialization singleChoiceTagSpecialization,
        InliningContext context)
    {
        var specializedModuleName = context.Resolution.CurrentModuleName ?? funcInfo.ModuleName;

        var bodyAfterTagRewrite =
            RewriteRecursiveCallsForSingleChoiceTagSpecialization(
                funcImpl.Expression,
                funcImpl.Name.Value,
                funcInfo.ModuleName,
                specializedModuleName,
                specializedName,
                singleChoiceTagSpecialization);

        bodyAfterTagRewrite =
            ElmSyntaxTransformations.SubstituteInExpression(
                bodyAfterTagRewrite,
                new Dictionary<string, Node<SyntaxTypes.Expression>>
                {
                    [singleChoiceTagSpecialization.ParamName] = singleChoiceTagSpecialization.ReplacementArgument
                });

        var directPatternFieldSubstitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();

        CollectDirectFieldSubstitutions(
            singleChoiceTagSpecialization.FieldPlans,
            directPatternFieldSubstitutions);

        if (directPatternFieldSubstitutions.Count > 0)
        {
            bodyAfterTagRewrite =
                ElmSyntaxTransformations.SubstituteInExpression(
                    bodyAfterTagRewrite,
                    directPatternFieldSubstitutions);
        }

        var parametersAfterTagRewrite =
            BuildSingleChoiceTagSpecializedParameters(
                funcImpl.Arguments,
                singleChoiceTagSpecialization);

        var callArgumentsAfterTagRewrite =
            BuildSingleChoiceTagSpecializedCallArguments(
                specializedModuleName,
                specializedName,
                appArgs,
                funcImpl.Arguments.Count,
                singleChoiceTagSpecialization)
            .Skip(1)
            .ToList();

        var invariantHigherOrderParamIndices = new HashSet<int>();

        for (var i = 0; i < parametersAfterTagRewrite.Length && i < callArgumentsAfterTagRewrite.Count; i++)
        {
            if (parametersAfterTagRewrite[i].Value is not SyntaxTypes.Pattern.VarPattern varPattern)
                continue;

            if (!IsFunctionExpression(callArgumentsAfterTagRewrite[i].Value, context))
                continue;

            if (!IsLoopInvariantInRecursiveCalls(
                    bodyAfterTagRewrite.Value,
                    specializedName,
                    funcInfo.ModuleName,
                    varPattern.Name,
                    i))
                continue;

            invariantHigherOrderParamIndices.Add(i);
        }

        var bodyAfterHigherOrderRewrite =
            invariantHigherOrderParamIndices.Count is 0
            ?
            bodyAfterTagRewrite
            :
            RewriteGroupCallsInExpression(
                bodyAfterTagRewrite,
                // NOTE: After RewriteRecursiveCallsForSingleChoiceTagSpecialization, the recursive
                // calls in the body already use specializedName (not the original function name).
                // So we must look for calls to specializedName, not funcImpl.Name.Value.
                [(specializedName, specializedModuleName, specializedModuleName, specializedName)],
                invariantHigherOrderParamIndices);

        var higherOrderSubstitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();

        foreach (var paramIndex in invariantHigherOrderParamIndices)
        {
            var varPattern = (SyntaxTypes.Pattern.VarPattern)parametersAfterTagRewrite[paramIndex].Value;
            higherOrderSubstitutions[varPattern.Name] = callArgumentsAfterTagRewrite[paramIndex];
        }

        if (higherOrderSubstitutions.Count > 0)
        {
            bodyAfterHigherOrderRewrite =
                ElmSyntaxTransformations.SubstituteInExpression(
                    bodyAfterHigherOrderRewrite,
                    higherOrderSubstitutions);
        }

        var qualifiedBody =
            QualifyLiftedHelperReferencesFromCalleeModule(
                bodyAfterHigherOrderRewrite,
                funcInfo.ModuleName,
                context);

        var remainingParameters =
            parametersAfterTagRewrite
            .Where((_, i) => !invariantHigherOrderParamIndices.Contains(i))
            .ToImmutableArray();

        // Detect free variables introduced by substitution (same logic as TrySpecializeRecursiveCall).
        var remainingParameterNames =
            remainingParameters
            .SelectMany(p => ElmSyntaxTransformations.CollectPatternNames(p.Value))
            .ToHashSet();

        var freeVarsInBody =
            SyntaxTypes.SyntaxAnalysis.CollectRemainingFreeVariables(qualifiedBody.Value)
            .Except(remainingParameterNames)
            .Except(context.ModuleLevelNames)
            .Remove(specializedName);

        var capturedVariables = freeVarsInBody.OrderBy(v => v).ToList();

        var finalBody = qualifiedBody;
        var finalParams = remainingParameters;

        if (capturedVariables.Count > 0)
        {
            var capturedParams =
                capturedVariables
                .Select(
                    v => new Node<SyntaxTypes.Pattern>(
                        ElmSyntaxTransformations.s_zeroRange,
                        new SyntaxTypes.Pattern.VarPattern(v)))
                .ToList();

            finalParams = [.. capturedParams, .. remainingParameters];

            finalBody =
                AddExtraArgsToSelfCalls(
                    qualifiedBody,
                    specializedName,
                    specializedModuleName,
                    capturedVariables);
        }

        var specializedImpl =
            new SyntaxTypes.FunctionImplementation(
                Name: new Node<string>(ElmSyntaxTransformations.s_zeroRange, specializedName),
                Arguments: finalParams,
                Expression: finalBody);

        var specializedFunc =
            new SyntaxTypes.FunctionStruct(
                Documentation: null,
                Signature: null,
                Declaration:
                new Node<SyntaxTypes.FunctionImplementation>(ElmSyntaxTransformations.s_zeroRange, specializedImpl));

        var newDecl =
            new Node<SyntaxTypes.Declaration>(
                ElmSyntaxTransformations.s_zeroRange,
                new SyntaxTypes.Declaration.FunctionDeclaration(specializedFunc));

        var callArgs =
            new List<Node<SyntaxTypes.Expression>>
            {
                new(
                    ElmSyntaxTransformations.s_zeroRange,
                    new SyntaxTypes.Expression.FunctionOrValue(specializedModuleName, specializedName))
            };

        // Pass captured variables as arguments.
        foreach (var capturedVar in capturedVariables)
        {
            callArgs.Add(
                new Node<SyntaxTypes.Expression>(
                    ElmSyntaxTransformations.s_zeroRange,
                    new SyntaxTypes.Expression.FunctionOrValue([], capturedVar)));
        }

        for (var i = 0; i < callArgumentsAfterTagRewrite.Count; i++)
        {
            if (invariantHigherOrderParamIndices.Contains(i))
                continue;

            callArgs.Add(callArgumentsAfterTagRewrite[i]);
        }

        var callExpr =
            callArgs.Count is 1
            ?
            callArgs[0].Value
            :
            new SyntaxTypes.Expression.Application([.. callArgs]);

        return new InliningResult(callExpr, [newDecl]);
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
        return
            IsLoopInvariantInCallsToAny(
                body,
                [(funcName, funcModuleName)],
                paramName,
                paramIndex);
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

            if (expr is SyntaxTypes.Expression.Application app &&
                app.Arguments.Count > 0 &&
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

            SyntaxTypes.SyntaxAnalysis.ForEachChildExpression(expr, worklist.Push);
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
        var startKey = DeclQualifiedName.Create(startFuncModule, startFuncName);
        var startBody = startFunc.Function.Declaration.Value.Expression.Value;

        var candidates = new List<FunctionInfo>();

        // Find functions referenced directly in the start function's body
        var directRefs =
            CollectFunctionReferences(startBody)
            .Distinct();

        foreach (var refKey in directRefs)
        {
            if (refKey.Equals(startKey))
                continue;

            if (!context.Resolution.FunctionsByQualifiedName.TryGetValue(refKey, out var refFunc))
                continue;

            if (!refFunc.IsRecursive)
                continue;

            var refImpl = refFunc.Function.Declaration.Value;
            var refBody = refImpl.Expression.Value;

            // Check if this function references back to the start function (direct mutual recursion)
            var refRefs = CollectFunctionReferences(refBody);

            if (!refRefs.Any(r => r.Equals(startKey)))
                continue;

            // Check that the function has enough parameters for all invariant positions
            var maxInvariantIdx = invariantParamIndices.Max();

            if (refImpl.Arguments.Count <= maxInvariantIdx)
                continue;

            // Check that ALL invariant params are VarPatterns
            if (!invariantParamIndices.All(idx => refImpl.Arguments[idx].Value is SyntaxTypes.Pattern.VarPattern))
                continue;

            // Size check
            if (SyntaxTypes.SyntaxAnalysis.CountExpressionNodes(refBody, 2001) > 2000)
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
    /// Rewrites all recursive self-calls in the expression from the original function name
    /// to the specialized name, stripping arguments at the invariant parameter positions.
    /// </summary>
    private static Node<SyntaxTypes.Expression> RewriteRecursiveCallsInExpression(
        Node<SyntaxTypes.Expression> exprNode,
        string originalFuncName,
        ModuleName originalFuncModuleName,
        ModuleName specializedModuleName,
        string specializedName,
        HashSet<int> invariantParamIndices)
    {
        return
            ElmSyntaxTransformations.RewriteExpressionTree(
                exprNode,
                (app, recurse) =>
                RewriteRecursiveCallApplication(
                    app,
                    originalFuncName,
                    originalFuncModuleName,
                    specializedModuleName,
                    specializedName,
                    invariantParamIndices,
                    recurse));
    }

    private static SyntaxTypes.Expression RewriteRecursiveCallApplication(
        SyntaxTypes.Expression.Application app,
        string originalFuncName,
        ModuleName originalFuncModuleName,
        ModuleName specializedModuleName,
        string specializedName,
        HashSet<int> invariantParamIndices,
        Func<Node<SyntaxTypes.Expression>, Node<SyntaxTypes.Expression>> recurse)
    {
        // Check if this application is a recursive call to the original function
        if (app.Arguments.Count > 0 &&
            app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue fov &&
            IsRecursiveCallReference(fov, originalFuncName, originalFuncModuleName))
        {
            var newArgs =
                new List<Node<SyntaxTypes.Expression>>
                {
                    new(
                        ElmSyntaxTransformations.s_zeroRange,
                        new SyntaxTypes.Expression.FunctionOrValue(specializedModuleName, specializedName))
                };

            for (var i = 1; i < app.Arguments.Count; i++)
            {
                var argIndex = i - 1;

                if (!invariantParamIndices.Contains(argIndex))
                {
                    newArgs.Add(recurse(app.Arguments[i]));
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
                [.. app.Arguments.Select(a => recurse(a))]);
    }

    /// <summary>
    /// Post-processes a specialized function body to insert captured variable arguments
    /// into all recursive self-calls. The captured variables are inserted right after
    /// the function reference (before the remaining non-invariant arguments).
    /// </summary>
    private static Node<SyntaxTypes.Expression> AddExtraArgsToSelfCalls(
        Node<SyntaxTypes.Expression> body,
        string specializedFuncName,
        ModuleName specializedModuleName,
        ModuleName capturedVariableNames)
    {
        if (capturedVariableNames.Count is 0)
            return body;

        return
            ElmSyntaxTransformations.RewriteExpressionTree(
                body,
                (app, recurse) =>
                {
                    if (app.Arguments.Count > 0 &&
                        app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue fov &&
                        fov.Name == specializedFuncName &&
                        (fov.ModuleName.Count is 0 ||
                        Enumerable.SequenceEqual(fov.ModuleName, specializedModuleName)))
                    {
                        var newArgs =
                            new List<Node<SyntaxTypes.Expression>>
                            {
                                app.Arguments[0] // Keep the function reference
                            };

                        // Insert captured variable references right after the function reference
                        foreach (var capturedVar in capturedVariableNames)
                        {
                            newArgs.Add(
                                new Node<SyntaxTypes.Expression>(
                                    ElmSyntaxTransformations.s_zeroRange,
                                    new SyntaxTypes.Expression.FunctionOrValue([], capturedVar)));
                        }

                        // Add the remaining arguments (recurse into them)
                        for (var i = 1; i < app.Arguments.Count; i++)
                        {
                            newArgs.Add(recurse(app.Arguments[i]));
                        }

                        return new SyntaxTypes.Expression.Application([.. newArgs]);
                    }

                    return
                        new SyntaxTypes.Expression.Application(
                            [.. app.Arguments.Select(a => recurse(a))]);
                });
    }

    private static Node<SyntaxTypes.Expression> RewriteRecursiveCallsForSingleChoiceTagSpecialization(
        Node<SyntaxTypes.Expression> exprNode,
        string originalFuncName,
        ModuleName originalFuncModuleName,
        ModuleName specializedModuleName,
        string specializedName,
        SingleChoiceTagSpecialization specialization)
    {
        return
            ElmSyntaxTransformations.RewriteExpressionTree(
                exprNode,
                (app, recurse) =>
                RewriteRecursiveCallApplicationForSingleChoiceTagSpecialization(
                    app,
                    originalFuncName,
                    originalFuncModuleName,
                    specializedModuleName,
                    specializedName,
                    specialization,
                    recurse));
    }

    private static SyntaxTypes.Expression RewriteRecursiveCallApplicationForSingleChoiceTagSpecialization(
        SyntaxTypes.Expression.Application app,
        string originalFuncName,
        ModuleName originalFuncModuleName,
        ModuleName specializedModuleName,
        string specializedName,
        SingleChoiceTagSpecialization specialization,
        Func<Node<SyntaxTypes.Expression>, Node<SyntaxTypes.Expression>> recurse)
    {
        if (app.Arguments.Count > 0 &&
            app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue fov &&
            IsRecursiveCallReference(fov, originalFuncName, originalFuncModuleName))
        {
            var newArgs =
                new List<Node<SyntaxTypes.Expression>>
                {
                    new(
                        ElmSyntaxTransformations.s_zeroRange,
                        new SyntaxTypes.Expression.FunctionOrValue(specializedModuleName, specializedName))
                };

            for (var i = 1; i < app.Arguments.Count; i++)
            {
                var argIndex = i - 1;

                if (argIndex == specialization.ParamIndex)
                {
                    var constructorArgument =
                        ElmSyntaxTransformations.IsLocalVariableReference(
                            app.Arguments[i].Value,
                            specialization.ParamName)
                        ?
                        specialization.ReplacementArgument
                        :
                        app.Arguments[i];

                    if (ElmSyntaxTransformations.TryDeconstructConstructorApplication(constructorArgument) is not { } ctorApp ||
                        !ElmSyntaxTransformations.AreEquivalentConstructorNames(ctorApp.ConstructorName, specialization.ConstructorName))
                    {
                        return
                            new SyntaxTypes.Expression.Application(
                                [.. app.Arguments.Select(a => recurse(a))]);
                    }

                    var flattenedSpecializedArgs = new List<Node<SyntaxTypes.Expression>>();

                    foreach (var fieldPlan in specialization.FieldPlans)
                    {
                        if (fieldPlan.FlattenedParameters.Count is 0)
                            continue;

                        if (!TryRewriteSingleChoiceTagFieldArguments(
                                ctorApp.FieldExpressions[fieldPlan.FieldIndex],
                                fieldPlan,
                                specialization,
                                recurse,
                                out var rewrittenFieldArguments))
                        {
                            return
                                new SyntaxTypes.Expression.Application(
                                    [.. app.Arguments.Select(a => recurse(a))]);
                        }

                        flattenedSpecializedArgs.AddRange(rewrittenFieldArguments);
                    }

                    if (flattenedSpecializedArgs.Count > 1)
                    {
                        newArgs.Add(
                            new Node<SyntaxTypes.Expression>(
                                ElmSyntaxTransformations.s_zeroRange,
                                new SyntaxTypes.Expression.TupledExpression([.. flattenedSpecializedArgs])));
                    }
                    else
                    {
                        newArgs.AddRange(flattenedSpecializedArgs);
                    }

                    continue;
                }

                newArgs.Add(recurse(app.Arguments[i]));
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
                [.. app.Arguments.Select(a => recurse(a))]);
    }

    private static bool TryRewriteSingleChoiceTagFieldArguments(
        Node<SyntaxTypes.Expression> fieldExpression,
        SingleChoiceTagFieldPlan fieldPlan,
        SingleChoiceTagSpecialization specialization,
        Func<Node<SyntaxTypes.Expression>, Node<SyntaxTypes.Expression>> recurse,
        out IReadOnlyList<Node<SyntaxTypes.Expression>> rewrittenArguments)
    {
        if (fieldPlan.NestedFieldPlans.Count is 0)
        {
            rewrittenArguments = [recurse(fieldExpression)];

            return true;
        }

        if (fieldPlan.ParameterSpecialization is not ParameterSpecialization.SingleChoiceTagUnwrap nestedTagUnwrap ||
            ElmSyntaxTransformations.TryDeconstructConstructorApplication(fieldExpression) is not { } ctorApp ||
            !ElmSyntaxTransformations.AreEquivalentConstructorNames(ctorApp.ConstructorName, nestedTagUnwrap.ConstructorName))
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
                    ctorApp.FieldExpressions[nestedFieldPlan.FieldIndex],
                    nestedFieldPlan,
                    specialization,
                    recurse,
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

    /// <summary>
    /// Specializes a mutual recursion group. Creates specialized versions of all group members
    /// where function parameters are substituted with concrete values, and all intra-group calls
    /// are rewritten to target the specialized versions. Specialized functions are added as
    /// module-level declarations via the context's AccumulatedDeclarations list.
    /// </summary>
    private static InliningResult TrySpecializeMutualRecursiveGroup(
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
            ImmutableDictionary.CreateBuilder<int, ParameterSpecialization>();

        foreach (var paramIndex in invariantIndicesSet)
        {
            var classified = ParameterSpecialization.ClassifyArgument(appArgs[paramIndex].Value);

            if (classified is not null)
            {
                paramSpecs[paramIndex] = classified;
            }
            else
            {
                paramSpecs[paramIndex] =
                    new ParameterSpecialization.ConcreteLambdaValue(
                        new SyntaxTypes.LambdaStruct(
                            Arguments: [],
                            Expression: appArgs[paramIndex]));
            }
        }

        var builtParamSpecs = paramSpecs.ToImmutable();

        // Look up the start function's specialized name from the catalog.
        var startSpecializedName =
            LookupSpecializedNameForHigherOrder(
                DeclQualifiedName.Create(startFunc.ModuleName, startImpl.Name.Value),
                builtParamSpecs,
                context);

        if (startSpecializedName is null)
        {
            return
                new InliningResult(
                    new SyntaxTypes.Expression.UnitExpr(),
                    []);
        }

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
                    DeclQualifiedName.Create(member.ModuleName, memberImpl.Name.Value),
                    builtParamSpecs,
                    context);

            // If a group member's specialization wasn't collected, we can't specialize the group.
            if (memberSpecName is null)
            {
                return
                    new InliningResult(
                        new SyntaxTypes.Expression.UnitExpr(),
                        []);
            }

            allMembers.Add((member, memberImpl, memberSpecName));
        }

        // Build group mapping for call rewriting.
        var specModuleName = context.Resolution.CurrentModuleName ?? allMembers[0].Info.ModuleName;

        var groupMapping =
            allMembers
            .Select(m => (m.Impl.Name.Value, m.Info.ModuleName, specModuleName, m.SpecializedName))
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

            var substitutedBody = ElmSyntaxTransformations.SubstituteInExpression(rewrittenBody, substitutions);

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
                    Name: new Node<string>(ElmSyntaxTransformations.s_zeroRange, specName),
                    Arguments: remainingParams,
                    Expression: qualifiedBody);

            var specializedFunc =
                new SyntaxTypes.FunctionStruct(
                    Documentation: null,
                    Signature: null,
                    Declaration:
                    new Node<SyntaxTypes.FunctionImplementation>(ElmSyntaxTransformations.s_zeroRange, specializedImpl));

            newDecls.Add(
                new Node<SyntaxTypes.Declaration>(
                    ElmSyntaxTransformations.s_zeroRange,
                    new SyntaxTypes.Declaration.FunctionDeclaration(specializedFunc)));
        }

        // Build the call to the start function's specialized version with remaining (non-invariant) arguments.
        var startSpecName = allMembers[0].SpecializedName;
        var mutualRecSpecModuleName = context.Resolution.CurrentModuleName ?? allMembers[0].Info.ModuleName;

        var callArgs =
            new List<Node<SyntaxTypes.Expression>>
            {
                new(
                    ElmSyntaxTransformations.s_zeroRange,
                    new SyntaxTypes.Expression.FunctionOrValue(mutualRecSpecModuleName, startSpecName))
            };

        for (var i = 0; i < appArgs.Count; i++)
        {
            if (i < startImpl.Arguments.Count && invariantIndicesSet.Contains(i))
                continue;

            callArgs.Add(appArgs[i]);
        }

        var callExpr =
            callArgs.Count is 1
            ?
            callArgs[0].Value
            :
            new SyntaxTypes.Expression.Application([.. callArgs]);

        return new InliningResult(callExpr, newDecls.ToImmutable());
    }

    /// <summary>
    /// Rewrites all calls to members of a mutual recursion group in an expression,
    /// replacing them with calls to the specialized versions and stripping invariant arguments.
    /// </summary>
    private static Node<SyntaxTypes.Expression> RewriteGroupCallsInExpression(
        Node<SyntaxTypes.Expression> exprNode,
        IReadOnlyList<(string OriginalName, ModuleName OriginalModule, ModuleName SpecializedModule, string SpecializedName)> groupMapping,
        HashSet<int> invariantParamIndices)
    {
        return
            ElmSyntaxTransformations.RewriteExpressionTree(
                exprNode,
                (app, recurse) =>
                RewriteGroupCallApplication(app, groupMapping, invariantParamIndices, recurse));
    }

    private static SyntaxTypes.Expression RewriteGroupCallApplication(
        SyntaxTypes.Expression.Application app,
        IReadOnlyList<(string OriginalName, ModuleName OriginalModule, ModuleName SpecializedModule, string SpecializedName)> groupMapping,
        HashSet<int> invariantParamIndices,
        Func<Node<SyntaxTypes.Expression>, Node<SyntaxTypes.Expression>> recurse)
    {
        if (app.Arguments.Count > 0 &&
            app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue fov)
        {
            foreach (var (origName, origModule, specModule, specName) in groupMapping)
            {
                if (IsRecursiveCallReference(fov, origName, origModule))
                {
                    var newArgs =
                        new List<Node<SyntaxTypes.Expression>>
                        {
                            new(
                                ElmSyntaxTransformations.s_zeroRange,
                                new SyntaxTypes.Expression.FunctionOrValue(specModule, specName))
                        };

                    for (var i = 1; i < app.Arguments.Count; i++)
                    {
                        var argIndex = i - 1;

                        if (!invariantParamIndices.Contains(argIndex))
                        {
                            newArgs.Add(recurse(app.Arguments[i]));
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
                [.. app.Arguments.Select(a => recurse(a))]);
    }


    private static Node<SyntaxTypes.Expression> QualifyLiftedHelperReferencesFromCalleeModule(
        Node<SyntaxTypes.Expression> exprNode,
        ModuleName calleeModuleName,
        InliningContext context)
    {
        var resolution = context.Resolution;

        return
            ReferenceQualifier.Qualify(
                exprNode,
                calleeModuleName,
                name =>
                (name.Contains(GeneratedNameSuffixes.Lifted, StringComparison.Ordinal) ||
                name.Contains(GeneratedNameSuffixes.Specialized, StringComparison.Ordinal)) &&
                resolution.FunctionsByQualifiedName.ContainsKey(
                    DeclQualifiedName.Create(calleeModuleName, name)),
                trackLocalScope: false);
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
        // Case pattern variables shadow module-level functions in the case branch body.
        var patternNames = ElmSyntaxTransformations.CollectPatternNames(caseItem.Pattern.Value);

        var branchContext =
            patternNames.Count > 0
            ?
            context with { LocalNames = context.LocalNames.Union(patternNames) }
            :
            context;

        var (inlinedExpr, newDecls) = InlineExpression(caseItem.Expression, branchContext);

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
    private static InliningResult InlineLetExpression(
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
                SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(letDestr.Pattern.Value) is
                SyntaxTypes.Pattern.AllPattern)
            {
                propagated = true;
            }
            else if (declNode.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestrToPropagate &&
                TryDeconstructKnownConstructorApplication(
                    letDestrToPropagate.Expression,
                    context) is { } knownCtorApp &&
                ElmSyntaxTransformations.TryBindSingleChoiceTagPattern(
                    letDestrToPropagate.Pattern.Value,
                    knownCtorApp.ConstructorName,
                    knownCtorApp.FieldExpressions) is { } patternBindings)
            {
                foreach (var binding in patternBindings)
                {
                    substitutions[binding.Key] = binding.Value;
                }

                if (ElmSyntaxTransformations.TryGetAliasNameFromPattern(letDestrToPropagate.Pattern.Value) is { } aliasName)
                {
                    substitutions[aliasName] =
                        ElmSyntaxTransformations.BuildConstructorApplication(
                            knownCtorApp.ConstructorName,
                            knownCtorApp.FieldExpressions);
                }

                propagated = true;
            }

            if (!propagated)
                remainingDecls.Add(declNode);
        }

        // Step 3: Apply propagated substitutions to the body AND remaining declarations.
        // This is necessary because a propagated binding (e.g., `(Bytes.Elm_Bytes blob) = expr`)
        // may introduce names that are referenced by other declarations in the same let block
        // (e.g., `blobLength = Pine_kernel.length blob`). Without substituting in remaining
        // declarations, those references become unresolvable after the binding is removed.
        var body = letExpr.Value.Expression;

        if (substitutions.Count > 0)
        {
            body = ElmSyntaxTransformations.SubstituteInExpression(body, substitutions);

            for (var i = 0; i < remainingDecls.Count; i++)
            {
                remainingDecls[i] =
                    ElmSyntaxTransformations.SubstituteInLetDeclaration(remainingDecls[i], substitutions);
            }
        }

        // Collect all variable names bound by the let declarations so they shadow
        // module-level functions of the same name in the body.
        var letBoundNames = ImmutableHashSet.CreateBuilder<string>();

        foreach (var declNode in inlinedDecls)
        {
            switch (declNode.Value)
            {
                case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                    letBoundNames.Add(letFunc.Function.Declaration.Value.Name.Value);
                    break;

                case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                    foreach (var name in ElmSyntaxTransformations.CollectPatternNames(letDestr.Pattern.Value))
                        letBoundNames.Add(name);

                    break;
            }
        }

        var bodyContext =
            letBoundNames.Count > 0
            ?
            context with { LocalNames = context.LocalNames.Union(letBoundNames) }
            :
            context;

        // Extend LetRhsByName so the non-recursive HO emission path
        // (TrySpecializeNonRecursiveTupleUnwrapCall) can resolve bare-local
        // references in tuple-argument elements through the surrounding
        // newtype destructurings of shape `let (Wrapper name) = <rhs>`.
        // Mirrors the discovery-walker extension in
        // ElmSyntaxOptimizationSpecializationCollection.CollectSpecializationsFromExpression.
        var extendedLetRhs =
            RemoveShadowedFromLetRhs(
                bodyContext.LetRhsByName,
                letBoundNames);

        foreach (var declNode in inlinedDecls)
        {
            if (declNode.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestrForCtx &&
                TryExtractLetNewtypeBindingName(letDestrForCtx) is { } innerVarName)
            {
                extendedLetRhs = extendedLetRhs.SetItem(innerVarName, letDestrForCtx.Expression.Value);
            }
        }

        if (!ReferenceEquals(extendedLetRhs, bodyContext.LetRhsByName))
        {
            bodyContext = bodyContext with { LetRhsByName = extendedLetRhs };
        }

        var (inlinedBody, bodyDecls) = InlineExpression(body, bodyContext);
        newDecls.AddRange(bodyDecls);

        // If all declarations were propagated, eliminate the let
        if (remainingDecls.Count is 0)
            return new InliningResult(inlinedBody.Value, newDecls.ToImmutable());

        return
            new InliningResult(
                new SyntaxTypes.Expression.LetExpression(
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
            context.Resolution.FunctionsByQualifiedName.ContainsKey(
                DeclQualifiedName.Create(fov.ModuleName, fov.Name)))
            return (exprNode, ImmutableList<Node<SyntaxTypes.Declaration>>.Empty);

        // Parenthesized expression
        if (expr is SyntaxTypes.Expression.ParenthesizedExpression paren)
            return TryResolveToFunctionValue(paren.Expression, context);

        // Application of a known non-recursive function: try force-inlining to see if result is a lambda
        if (expr is SyntaxTypes.Expression.Application app && app.Arguments.Count >= 2 &&
            app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue funcOrValue)
        {
            if (TryResolveKnownFunctionReference(funcOrValue, context) is { } resolved &&
                !resolved.FunctionInfo.IsRecursive &&
                !context.InliningStack.Contains(resolved.QualifiedName))
            {
                var funcImpl = resolved.FunctionInfo.Function.Declaration.Value;
                var appArgs = app.Arguments.Skip(1).ToList();

                // Must have enough arguments to fully apply the function
                if (appArgs.Count >= funcImpl.Arguments.Count)
                {
                    var newContext =
                        context with
                        {
                            InliningStack = context.InliningStack.Add(resolved.QualifiedName)
                        };

                    var (inlinedResult, inlinedDecls) =
                        InlineFunctionCall(
                            resolved.FunctionInfo.ModuleName,
                            funcImpl,
                            appArgs,
                            newContext);

                    // Check if the inlined result is a function value
                    if (inlinedResult is SyntaxTypes.Expression.LambdaExpression or
                        SyntaxTypes.Expression.FunctionOrValue)
                    {
                        if (inlinedResult is SyntaxTypes.Expression.FunctionOrValue resultFov &&
                            (resultFov.ModuleName.Count is 0 ||
                            !context.Resolution.FunctionsByQualifiedName.ContainsKey(
                                DeclQualifiedName.Create(resultFov.ModuleName, resultFov.Name))))
                        {
                            // Not a known function reference, discard declarations
                            return (null, []);
                        }

                        return (new Node<SyntaxTypes.Expression>(ElmSyntaxTransformations.s_zeroRange, inlinedResult), inlinedDecls);
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
            TryResolveKnownFunctionReference(funcOrValue, context) is { } resolved &&
            !resolved.FunctionInfo.IsRecursive &&
            !context.InliningStack.Contains(resolved.QualifiedName))
        {
            var funcImpl = resolved.FunctionInfo.Function.Declaration.Value;

            if (funcImpl.Arguments.Count is 0)
            {
                var newContext =
                    context with
                    {
                        InliningStack = context.InliningStack.Add(resolved.QualifiedName)
                    };

                var (inlinedResult, inlinedDecls) =
                    InlineFunctionCall(
                        resolved.FunctionInfo.ModuleName,
                        funcImpl,
                        [],
                        newContext);

                if (inlinedResult is SyntaxTypes.Expression.RecordExpr)
                {
                    return (new Node<SyntaxTypes.Expression>(ElmSyntaxTransformations.s_zeroRange, inlinedResult), inlinedDecls);
                }
            }
        }

        return (null, []);
    }

    private static ResolvedFunctionReference? TryResolveKnownFunctionReference(
        SyntaxTypes.Expression.FunctionOrValue funcOrValue,
        InliningContext context)
    {
        if (funcOrValue.ModuleName.Count > 0)
        {
            var qualifiedName = DeclQualifiedName.Create(funcOrValue.ModuleName, funcOrValue.Name);

            if (context.Resolution.FunctionsByQualifiedName.TryGetValue(qualifiedName, out var funcInfo))
                return new ResolvedFunctionReference(qualifiedName, funcInfo);

            // The reference has an explicit module qualifier that didn't resolve.
            // Do NOT fall back to the current module — that would incorrectly resolve
            // e.g. `Pine_kernel.take` as `List.take` when processing the List module.
            return null;
        }

        // If the unqualified name is shadowed by a local variable (from let bindings,
        // function parameters, or case patterns), do NOT resolve it to a module-level function.
        // This prevents bugs where inlining a function body introduces a local variable
        // (e.g., `parse` from destructuring `(Parser parse)`) that collides with a
        // module-level function of the same name (e.g., `Elm.Parser.parse`).
        if (context.LocalNames.Contains(funcOrValue.Name))
            return null;

        if (context.Resolution.CurrentModuleName is { } currentModuleName)
        {
            var qualifiedName = DeclQualifiedName.Create(currentModuleName, funcOrValue.Name);

            if (context.Resolution.FunctionsByQualifiedName.TryGetValue(qualifiedName, out var funcInfo))
                return new ResolvedFunctionReference(qualifiedName, funcInfo);
        }

        return null;
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
        // Lambda parameter names shadow module-level functions in the lambda body.
        var paramNames = new HashSet<string>();

        foreach (var arg in lambda.Arguments)
            ElmSyntaxTransformations.CollectPatternNamesRecursive(arg.Value, paramNames);

        var bodyContext =
            paramNames.Count > 0
            ?
            context with { LocalNames = context.LocalNames.Union(paramNames) }
            :
            context;

        var (inlinedExpr, newDecls) = InlineExpression(lambda.Expression, bodyContext);

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
    /// Builds a <see cref="ElmSyntaxTransformations.CrossModuleQualification"/> context
    /// for inlining a function body from <paramref name="calleeModuleName"/> into the
    /// current module. Returns null when both modules are the same (no qualification needed).
    /// </summary>
    private static ElmSyntaxTransformations.CrossModuleQualification? BuildCrossModuleQualification(
        ModuleName calleeModuleName,
        InliningContext context)
    {
        if (context.Resolution.CurrentModuleName is { } currentModuleName &&
            calleeModuleName.SequenceEqual(currentModuleName))
        {
            return null;
        }

        var calleeModuleLevelNames = new HashSet<string>();

        foreach (var kvp in context.Resolution.FunctionsByQualifiedName)
        {
            if (kvp.Key.Namespaces.SequenceEqual(calleeModuleName))
                calleeModuleLevelNames.Add(kvp.Key.DeclName);
        }

        // Also include single-choice constructor names from the callee module
        foreach (var kvp in context.Resolution.SingleChoiceConstructors)
        {
            if (kvp.Key.ModuleName.SequenceEqual(calleeModuleName))
                calleeModuleLevelNames.Add(kvp.Key.Name);
        }

        if (calleeModuleLevelNames.Count is 0)
            return null;

        return
            new ElmSyntaxTransformations.CrossModuleQualification(
                calleeModuleName,
                calleeModuleLevelNames);
    }

}
