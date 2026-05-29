using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Detects "higher-order" parameters of an Elm function declaration: parameters
/// whose value influences the function position of at least one application
/// inside the function body — directly (intra-procedural) or transitively
/// across top-level declarations via a known higher-order callee.
///
/// <para>
/// A parameter is intra-procedurally higher-order if at least one of the
/// names it introduces (via its pattern) flows — possibly through enclosing
/// <c>let</c>-bindings, lambdas, case branches, etc. — into an expression
/// that is used as the function part of an
/// <see cref="SyntaxTypes.Expression.Application"/> somewhere in the body.
/// </para>
///
/// <para>
/// A parameter is transitively higher-order if at least one of the names it
/// introduces flows into an argument expression that the function passes to
/// a known higher-order parameter of another top-level declaration. For
/// example, given
/// <code>
/// applyN f n x = if n == 0 then x else applyN f (n - 1) (f x)
/// while_ f = Parser (\x -> applyN f 10 x)
/// </code>
/// <c>applyN</c>'s parameter <c>f</c> is intra-procedurally higher-order
/// (it appears in head position in <c>f x</c>), and <c>while_</c>'s
/// parameter <c>f</c> is transitively higher-order because it flows into
/// <c>applyN</c>'s higher-order argument position (index 0).
/// </para>
///
/// <para>
/// The data-flow tracing is delegated to <see cref="SyntaxAnalysis"/>;
/// this module enumerates the names introduced by each parameter pattern
/// and intersects them with either (a) the set of names that flow into
/// application function positions in the body, or (b) the set of names
/// that flow into a known higher-order argument position of a known
/// callee in the body.
/// </para>
/// </summary>
internal static class HigherOrderParameterAnalysis
{
    /// <summary>
    /// Returns the indices of those parameters in <paramref name="parameters"/>
    /// that bind at least one name flowing into the function position of an
    /// Application in <paramref name="body"/>.
    /// </summary>
    public static IReadOnlyList<int> FindHigherOrderParameterIndices(
        IReadOnlyList<Node<SyntaxTypes.Pattern>> parameters,
        SyntaxTypes.Expression body)
    {
        var flowingIntoAppFunctions =
            SyntaxAnalysis.ComputeNamesFlowingIntoApplicationFunctions(body);

        if (flowingIntoAppFunctions.IsEmpty)
            return [];

        var result = new List<int>(parameters.Count);

        for (var i = 0; i < parameters.Count; i++)
        {
            var paramNames =
                SyntaxAnalysis.CollectNamesBoundByPattern(parameters[i].Value);

            foreach (var name in paramNames)
            {
                if (flowingIntoAppFunctions.Contains(name))
                {
                    result.Add(i);
                    break;
                }
            }
        }

        return result;
    }

    /// <summary>
    /// Convenience overload accepting a <see cref="SyntaxTypes.FunctionImplementation"/>.
    /// </summary>
    public static IReadOnlyList<int> FindHigherOrderParameterIndices(
        SyntaxTypes.FunctionImplementation funcImpl)
    {
        return FindHigherOrderParameterIndices(funcImpl.Arguments, funcImpl.Expression.Value);
    }

    /// <summary>
    /// Returns the set of names introduced by the given parameter that flow into
    /// the function position of any application in <paramref name="body"/>.
    /// Useful for diagnostic / explanatory output.
    /// </summary>
    public static ImmutableHashSet<string> FindHigherOrderNamesIntroducedByParameter(
        SyntaxTypes.Pattern parameter,
        SyntaxTypes.Expression body)
    {
        var flowing = SyntaxAnalysis.ComputeNamesFlowingIntoApplicationFunctions(body);
        var bound = SyntaxAnalysis.CollectNamesBoundByPattern(parameter);
        return bound.Intersect(flowing);
    }

    // ------------------------------------------------------------------
    // Cross-top-level-declaration higher-order parameter detection.
    //
    // Two-stage design (mirrors the organisation requested in the
    // problem statement of the originating session):
    //
    //   1. BuildIntraDeclarationHigherOrderParameterDictionary —
    //      build a Dict&lt;DeclQualifiedName, ImmutableSortedSet&lt;int&gt;&gt;
    //      with the intra-procedural HO indices for every top-level
    //      function declaration, using only
    //      ComputeNamesFlowingIntoApplicationFunctions (i.e. names in
    //      head positions of Applications inside the same body).
    //
    //   2. PropagateHigherOrderParametersAcrossDeclarations —
    //      run a fixed-point pass that, for every declaration, walks
    //      its body via SyntaxAnalysis.VisitApplications and, at every
    //      Application whose head resolves to a known callee in the
    //      current dictionary, applies the
    //      AddFlowingNamesOf data-flow primitive to the argument
    //      expressions at the callee's HO indices. Names that flow
    //      in and intersect with the declaration's own parameter
    //      bindings extend the declaration's HO index set; the loop
    //      repeats until the dictionary stops growing.
    // ------------------------------------------------------------------

    /// <summary>
    /// Stage 1 of the cross-decl HO analysis: builds a declaration-level
    /// dictionary of the intra-procedural higher-order parameter indices
    /// for every top-level function declaration in <paramref name="declarations"/>.
    /// Non-function declarations (custom types, type aliases, etc.) are
    /// skipped. The set is empty for declarations with no higher-order
    /// parameter found at this stage.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, ImmutableSortedSet<int>>
        BuildIntraDeclarationHigherOrderParameterDictionary(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, ImmutableSortedSet<int>>();

        foreach (var (qname, decl) in declarations)
        {
            if (decl is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                continue;

            var funcImpl = funcDecl.Function.Declaration.Value;
            var indices = FindHigherOrderParameterIndices(funcImpl);
            builder[qname] = [.. indices];
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Stage 2 of the cross-decl HO analysis: iterates a fixed-point
    /// over <paramref name="declarations"/>, growing each declaration's
    /// higher-order parameter set with the indices of any parameter
    /// whose names flow into a known higher-order argument position of
    /// a known callee (the callee's current entry in
    /// <paramref name="seed"/>). Resolution of the head expression to
    /// a <see cref="DeclQualifiedName"/> qualifies unqualified
    /// references against the enclosing module name of each
    /// declaration. Returns a new dictionary; <paramref name="seed"/>
    /// is not mutated.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, ImmutableSortedSet<int>>
        PropagateHigherOrderParametersAcrossDeclarations(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        ImmutableDictionary<DeclQualifiedName, ImmutableSortedSet<int>> seed)
    {
        var current = seed;
        var changed = true;

        // Cap iterations to defend against unforeseen non-termination
        // (a fix-point is always reachable in finite steps because the
        // index sets are bounded by parameter counts, but the explicit
        // cap is cheap insurance).
        var iterationCap = 1 + 2 * declarations.Count;

        for (var iteration = 0; iteration < iterationCap && changed; iteration++)
        {
            changed = false;

            foreach (var (qname, decl) in declarations)
            {
                if (decl is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                    continue;

                var funcImpl = funcDecl.Function.Declaration.Value;
                var parameters = funcImpl.Arguments;

                if (parameters.Count is 0)
                    continue;

                // Pre-compute the names bound by each parameter pattern.
                var paramNames = new ImmutableHashSet<string>[parameters.Count];

                for (var i = 0; i < parameters.Count; i++)
                    paramNames[i] = SyntaxAnalysis.CollectNamesBoundByPattern(parameters[i].Value);

                var existing =
                    current.TryGetValue(qname, out var existingSet)
                    ?
                    existingSet
                    :
                    [];

                var newSet = existing;

                SyntaxAnalysis.VisitApplications(
                    funcImpl.Expression.Value,
                    letRhsByName: [],
                    bound: [],
                    onApplication: (app, letRhsByName, bound) =>
                    {
                        // app.Arguments[0] is the head; the user-visible
                        // arguments start at index 1.
                        if (app.Arguments.Count < 2)
                            return;

                        var head = SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value);

                        if (head is not SyntaxTypes.Expression.FunctionOrValue fov)
                            return;

                        var calleeQName = ElmSyntaxTransformations.ResolveReference(fov, qname.Namespaces);

                        if (!current.TryGetValue(calleeQName, out var calleeHoIndices))
                            return;

                        if (calleeHoIndices.IsEmpty)
                            return;

                        foreach (var hoIdx in calleeHoIndices)
                        {
                            // The user-visible argument at callee parameter
                            // index `hoIdx` is at app.Arguments[hoIdx + 1].
                            var argPos = hoIdx + 1;

                            if (argPos >= app.Arguments.Count)
                                continue;

                            var argFlow = ImmutableHashSet<string>.Empty.ToBuilder();

                            SyntaxAnalysis.AddFlowingNamesOf(
                                app.Arguments[argPos].Value,
                                letRhsByName,
                                bound,
                                argFlow,
                                visited: []);

                            for (var i = 0; i < parameters.Count; i++)
                            {
                                if (newSet.Contains(i))
                                    continue;

                                if (paramNames[i].Overlaps(argFlow))
                                    newSet = newSet.Add(i);
                            }
                        }
                    });

                if (!newSet.SetEquals(existing))
                {
                    current = current.SetItem(qname, newSet);
                    changed = true;
                }
            }
        }

        return current;
    }

    /// <summary>
    /// Convenience entry point combining
    /// <see cref="BuildIntraDeclarationHigherOrderParameterDictionary"/>
    /// (Stage 1) and
    /// <see cref="PropagateHigherOrderParametersAcrossDeclarations"/>
    /// (Stage 2) for use cases that want the final, transitively
    /// closed higher-order parameter dictionary in one call.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, ImmutableSortedSet<int>>
        BuildHigherOrderParameterDictionary(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var seed = BuildIntraDeclarationHigherOrderParameterDictionary(declarations);
        return PropagateHigherOrderParametersAcrossDeclarations(declarations, seed);
    }

    /// <summary>
    /// Identifies a single higher-order top-level parameter by the
    /// qualified name of the containing declaration plus the simple
    /// binding name of the parameter pattern. Used as the key of the
    /// dictionary returned by
    /// <see cref="BuildHigherOrderParameterDistanceDictionary"/>.
    /// </summary>
    public readonly record struct HigherOrderParameterKey(
        DeclQualifiedName Decl,
        string ParameterName);

    /// <summary>
    /// Cross-decl higher-order parameter analysis with explicit forwarding
    /// distance. Given a set of <c>directSeeds</c> — top-level
    /// (declaration, parameter-name) pairs that are themselves
    /// higher-order inside the body of the containing decl — propagates
    /// the higher-order status along bare-reference forwarding edges and
    /// returns a dictionary mapping every reachable
    /// <see cref="HigherOrderParameterKey"/> to the forwarding distance
    /// to its nearest direct seed:
    /// <list type="bullet">
    /// <item><description>distance <c>0</c>: the entry is itself in
    /// <paramref name="directSeeds"/>;</description></item>
    /// <item><description>distance <c>N &gt;= 1</c>: <c>N</c> forwarding
    /// hops via callee Applications stand between this parameter and a
    /// direct seed.</description></item>
    /// </list>
    /// <para>
    /// A forwarding edge <c>(caller, p) → (g, g.params[k])</c> is
    /// recorded for every <see cref="SyntaxTypes.Expression.Application"/>
    /// inside the body of <c>caller</c> whose head resolves to a known
    /// top-level function <c>g</c> in <paramref name="declarations"/>
    /// and whose argument at position <c>k</c> (parens peeled) is a
    /// bare reference to the top-level parameter <c>p</c>. Bindings
    /// shadowed by inner lambdas / let-function parameters are
    /// respected.
    /// </para>
    /// <para>
    /// The minimum distance over all forwarding paths wins. This is the
    /// shared analysis backing both
    /// <c>HigherOrderParameter_Indirect</c> reporting in
    /// <c>OptimizationOpportunityFinder</c> and the diagnostic
    /// distance-annotated report at
    /// <c>2026-05-10-monomorphization-remaining-opportunities-after-lowering-for-parse-file.md</c>.
    /// </para>
    /// </summary>
    public static ImmutableDictionary<HigherOrderParameterKey, int>
        BuildHigherOrderParameterDistanceDictionary(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        IEnumerable<HigherOrderParameterKey> directSeeds)
    {
        // Per-decl: list of top-level parameter names in declaration order.
        // Non-VarPattern / non-AsPattern parameters keep an empty-string
        // entry so positional indices line up with call-site argument
        // indices.
        var declParamNames =
            new Dictionary<DeclQualifiedName, IReadOnlyList<string>>();

        foreach (var (qualifiedName, declaration) in declarations)
        {
            if (declaration is not SyntaxTypes.Declaration.FunctionDeclaration fd)
                continue;

            var paramList = new List<string>(fd.Function.Declaration.Value.Arguments.Count);

            foreach (var argNode in fd.Function.Declaration.Value.Arguments)
            {
                paramList.Add(ElmSyntaxTransformations.TryGetParameterDisplayName(argNode.Value) ?? "");
            }

            declParamNames[qualifiedName] = paramList;
        }

        // (module-key, simple decl name) → DeclQualifiedName. Resolves
        // unqualified head references against the enclosing module.
        var byModuleAndName =
            ElmSyntaxTransformations.BuildModuleKeyAndDeclNameIndex(declarations);

        // Forwarding edges: caller (decl, param) → callee (decl, param).
        var forwardEdges =
            new Dictionary<HigherOrderParameterKey, HashSet<HigherOrderParameterKey>>();

        foreach (var (qualifiedName, declaration) in declarations)
        {
            if (declaration is not SyntaxTypes.Declaration.FunctionDeclaration fd)
                continue;

            var callerParams = declParamNames[qualifiedName];

            if (callerParams.Count is 0)
                continue;

            var callerParamSet =
                ImmutableHashSet.CreateRange(
                    System.Linq.Enumerable.Where(callerParams, n => n.Length > 0));

            if (callerParamSet.Count is 0)
                continue;

            var ownModuleKey = string.Join(".", qualifiedName.Namespaces);

            CollectForwardingEdgesFromExpression(
                fd.Function.Declaration.Value.Expression.Value,
                qualifiedName,
                callerParamSet,
                ownModuleKey,
                byModuleAndName,
                declParamNames,
                forwardEdges);
        }

        // Reverse adjacency: for each destination node, the set of nodes
        // that forward into it.
        var reverseEdges =
            new Dictionary<HigherOrderParameterKey, HashSet<HigherOrderParameterKey>>();

        foreach (var (src, dsts) in forwardEdges)
        {
            foreach (var dst in dsts)
            {
                if (!reverseEdges.TryGetValue(dst, out var preds))
                {
                    preds = [];
                    reverseEdges[dst] = preds;
                }

                preds.Add(src);
            }
        }

        // Distance map seeded with every direct seed at distance 0.
        var distance = new Dictionary<HigherOrderParameterKey, int>();
        var queue = new Queue<HigherOrderParameterKey>();

        foreach (var seed in directSeeds)
        {
            if (distance.TryAdd(seed, 0))
                queue.Enqueue(seed);
        }

        // Reverse BFS: each visited node assigns (current + 1) to its
        // predecessors unless they already hold a smaller distance.
        while (queue.Count > 0)
        {
            var current = queue.Dequeue();
            var currentDistance = distance[current];

            if (!reverseEdges.TryGetValue(current, out var preds))
                continue;

            foreach (var pred in preds)
            {
                var candidate = currentDistance + 1;

                if (distance.TryGetValue(pred, out var existing) && existing <= candidate)
                    continue;

                distance[pred] = candidate;
                queue.Enqueue(pred);
            }
        }

        return distance.ToImmutableDictionary();
    }

    /// <summary>
    /// Returns the simple variable-pattern name for a top-level function
    /// parameter pattern, or an empty string for non-variable patterns
    /// (tuple destructuring, named patterns, etc.).
    /// </summary>
    private static ImmutableHashSet<string> CollectVarPatternNamesFromParams(
        IReadOnlyList<Node<SyntaxTypes.Pattern>> patterns)
    {
        var builder = ImmutableHashSet.CreateBuilder<string>();

        foreach (var p in patterns)
        {
            foreach (var n in SyntaxAnalysis.CollectNamesBoundByPattern(p.Value))
            {
                builder.Add(n);
            }
        }

        return builder.ToImmutable();
    }

    private static void CollectForwardingEdgesFromExpression(
        SyntaxTypes.Expression expression,
        DeclQualifiedName caller,
        ImmutableHashSet<string> callerParamSet,
        string ownModuleKey,
        IReadOnlyDictionary<(string moduleKey, string declName), DeclQualifiedName> byModuleAndName,
        IReadOnlyDictionary<DeclQualifiedName, IReadOnlyList<string>> declParamNames,
        Dictionary<HigherOrderParameterKey, HashSet<HigherOrderParameterKey>> forwardEdges)
    {
        ElmSyntaxTransformations.WalkExpressionsWithScope(
            expression,
            [],
            (node, scope) =>
            {
                if (node is not SyntaxTypes.Expression.Application app || app.Arguments.Count < 2)
                    return;

                var head = SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value);

                if (head is not SyntaxTypes.Expression.FunctionOrValue fov)
                    return;

                if (!TryResolveTopLevelFunction(
                        fov,
                        ownModuleKey,
                        byModuleAndName,
                        declParamNames,
                        out var calleeDecl,
                        out var calleeParams))
                {
                    return;
                }

                for (var k = 0; k < app.Arguments.Count - 1 && k < calleeParams.Count; k++)
                {
                    var argExpr = SyntaxAnalysis.UnwrapParenthesized(app.Arguments[k + 1].Value);

                    if (argExpr is not SyntaxTypes.Expression.FunctionOrValue argRef ||
                        argRef.ModuleName.Count is not 0 ||
                        !callerParamSet.Contains(argRef.Name) ||
                        scope.Contains(argRef.Name))
                    {
                        continue;
                    }

                    var calleeParamName = calleeParams[k];

                    if (calleeParamName.Length is 0)
                        continue;

                    var srcNode = new HigherOrderParameterKey(caller, argRef.Name);
                    var dstNode = new HigherOrderParameterKey(calleeDecl, calleeParamName);

                    if (!forwardEdges.TryGetValue(srcNode, out var dsts))
                    {
                        dsts = [];
                        forwardEdges[srcNode] = dsts;
                    }

                    dsts.Add(dstNode);
                }
            });
    }

    private static bool TryResolveTopLevelFunction(
        SyntaxTypes.Expression.FunctionOrValue fov,
        string ownModuleKey,
        IReadOnlyDictionary<(string moduleKey, string declName), DeclQualifiedName> byModuleAndName,
        IReadOnlyDictionary<DeclQualifiedName, IReadOnlyList<string>> declParamNames,
        out DeclQualifiedName decl,
        out IReadOnlyList<string> paramNames)
    {
        var moduleKey =
            fov.ModuleName.Count is 0
            ?
            ownModuleKey
            :
            string.Join(".", fov.ModuleName);

        if (byModuleAndName.TryGetValue((moduleKey, fov.Name), out var resolved) &&
            declParamNames.TryGetValue(resolved, out var ps))
        {
            decl = resolved;
            paramNames = ps;
            return true;
        }

        decl = default!;
        paramNames = [];
        return false;
    }



    // ------------------------------------------------------------------
    // Let-binding higher-order distance analysis (Gap B + Gap A extension
    // for the distance API).
    //
    // The existing distance API only tracks top-level parameters as
    // forwarding sources, and only matches bare-reference arguments at
    // whole-parameter positions. Section 16 of
    // 2026-05-10-monomorphization-remaining-opportunities-after-lowering-for-parse-file.md
    // shows the missing scenario:
    //
    //   * the caller has NO top-level parameters; instead it
    //     let-destructures three Parser-wrapped bindings,
    //   * those bindings are packaged into a TupledExpression and passed
    //     as a single argument to a callee whose first parameter is a
    //     TuplePattern, and
    //   * the callee's destructured names are intra-procedurally
    //     higher-order.
    //
    // The new analysis below treats per-decl owned names — both
    // destructured parameter names AND let-destructured binding names —
    // as candidate forwarding sources, and recursively decomposes
    // tuple / record / named-constructor argument expressions against
    // the callee's matching pattern position to record per-position
    // forwarding edges.
    // ------------------------------------------------------------------

    /// <summary>
    /// Identifies a single higher-order let-destructured binding by the
    /// qualified name of the containing top-level declaration plus the
    /// simple binding name.
    /// </summary>
    public readonly record struct LetBindingHigherOrderKey(
        DeclQualifiedName Decl,
        string BindingName);

    /// <summary>
    /// Cross-decl higher-order analysis that tracks let-destructured
    /// bindings as forwarding sources. Returns a dictionary mapping
    /// every <see cref="LetBindingHigherOrderKey"/> whose binding flows
    /// (directly or through forwarding hops) into a directly
    /// higher-order destructured name of any reachable callee to its
    /// shortest forwarding distance.
    /// <para>
    /// Distance <c>1</c> indicates a single Application-hop into a
    /// callee's destructured-parameter position that is itself directly
    /// higher-order in the callee's body; distance <c>N + 1</c> means
    /// <c>N</c> additional forwarding hops.
    /// </para>
    /// <para>
    /// Decomposition rules at call sites (callee parameter pattern at
    /// position <c>k</c>, argument expression at position <c>k + 1</c>):
    /// </para>
    /// <list type="bullet">
    /// <item><description><c>VarPattern</c> + bare reference: edge from
    /// caller's (decl, ref-name) to callee's (decl, param-name) when
    /// the ref-name is owned by the caller decl.</description></item>
    /// <item><description><c>TuplePattern(p1, ..., pN)</c> +
    /// <c>TupledExpression(e1, ..., eN)</c>: zip and recurse on each
    /// pair.</description></item>
    /// <item><description><c>RecordPattern({ f1, ..., fN })</c> +
    /// <c>RecordExpr({ f1 = e1, ..., fN = eN })</c>: for each pattern
    /// field name, recurse on the synthetic pair
    /// (<c>VarPattern(fi)</c>, <c>ei</c>).</description></item>
    /// <item><description><c>NamedPattern(C, [p1, ..., pN])</c> +
    /// <c>Application[FunctionOrValue(C), e1, ..., eN]</c>: zip and
    /// recurse on each constructor-arg / arg pair.</description></item>
    /// <item><description><c>ParenthesizedPattern</c> and
    /// <c>AsPattern</c>: recurse into the inner pattern; <c>AsPattern</c>
    /// additionally emits a VarPattern edge for the alias
    /// name.</description></item>
    /// </list>
    /// </summary>
    public static ImmutableDictionary<LetBindingHigherOrderKey, int>
        BuildLetBindingHigherOrderDistanceDictionary(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        // Step 1: collect per-decl parameter patterns.
        var declParamPatterns =
            new Dictionary<DeclQualifiedName, IReadOnlyList<SyntaxTypes.Pattern>>();

        foreach (var (q, d) in declarations)
        {
            if (d is not SyntaxTypes.Declaration.FunctionDeclaration fd)
                continue;

            var list =
                new List<SyntaxTypes.Pattern>(
                    fd.Function.Declaration.Value.Arguments.Count);

            foreach (var argNode in fd.Function.Declaration.Value.Arguments)
                list.Add(argNode.Value);

            declParamPatterns[q] = list;
        }

        // Step 2: by-module lookup for resolving unqualified callees.
        var byModuleAndName =
            ElmSyntaxTransformations.BuildModuleKeyAndDeclNameIndex(declarations);

        // Step 3: per-decl owned name sets (destructured-param names +
        // every let-destructured / let-function name anywhere in the
        // body), plus the subset that are specifically let-bound (used
        // to filter the output).
        var ownedNamesByDecl =
            new Dictionary<DeclQualifiedName, ImmutableHashSet<string>>();

        var letBindingNamesByDecl =
            new Dictionary<DeclQualifiedName, ImmutableHashSet<string>>();

        foreach (var (q, d) in declarations)
        {
            if (d is not SyntaxTypes.Declaration.FunctionDeclaration fd)
                continue;

            var fi = fd.Function.Declaration.Value;

            var owned = ImmutableHashSet<string>.Empty.ToBuilder();

            foreach (var argNode in fi.Arguments)
            {
                foreach (var n in SyntaxAnalysis.CollectNamesBoundByPattern(argNode.Value))
                    owned.Add(n);
            }

            var letBindings = ImmutableHashSet<string>.Empty.ToBuilder();
            CollectLetIntroducedNames(fi.Expression.Value, letBindings);

            foreach (var n in letBindings)
                owned.Add(n);

            ownedNamesByDecl[q] = owned.ToImmutable();
            letBindingNamesByDecl[q] = letBindings.ToImmutable();
        }

        // Step 4: direct seeds — (decl, name) pairs where `name` is owned
        // by `decl` AND `name` flows into an application-function position
        // inside `decl`'s body.
        var distance = new Dictionary<HigherOrderParameterKey, int>();
        var queue = new Queue<HigherOrderParameterKey>();

        foreach (var (q, d) in declarations)
        {
            if (d is not SyntaxTypes.Declaration.FunctionDeclaration fd)
                continue;

            var owned = ownedNamesByDecl[q];

            var flowing =
                SyntaxAnalysis.ComputeNamesFlowingIntoApplicationFunctions(
                    fd.Function.Declaration.Value.Expression.Value);

            foreach (var name in flowing)
            {
                if (!owned.Contains(name))
                    continue;

                var key = new HigherOrderParameterKey(q, name);

                if (distance.TryAdd(key, 0))
                    queue.Enqueue(key);
            }
        }

        // Step 5: walk each decl's body via VisitApplications and at each
        // Application decompose (calleePattern[k], arg[k+1]) into edges
        // (caller, refName) → (calleeDecl, destructuredCalleeName).
        var forwardEdges =
            new Dictionary<HigherOrderParameterKey, HashSet<HigherOrderParameterKey>>();

        foreach (var (q, d) in declarations)
        {
            if (d is not SyntaxTypes.Declaration.FunctionDeclaration fd)
                continue;

            var fi = fd.Function.Declaration.Value;
            var ownModuleKey = string.Join(".", q.Namespaces);
            var ownedNames = ownedNamesByDecl[q];

            SyntaxAnalysis.VisitApplications(
                fi.Expression.Value,
                letRhsByName: [],
                bound: [],
                onApplication: (app, letRhsByName, bound) =>
                {
                    if (app.Arguments.Count < 2)
                        return;

                    var head = SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value);

                    if (head is not SyntaxTypes.Expression.FunctionOrValue fov)
                        return;

                    var moduleKey =
                        fov.ModuleName.Count is 0
                        ?
                        ownModuleKey
                        :
                        string.Join(".", fov.ModuleName);

                    if (!byModuleAndName.TryGetValue((moduleKey, fov.Name), out var calleeDecl))
                        return;

                    if (!declParamPatterns.TryGetValue(calleeDecl, out var calleeParams))
                        return;

                    // The `bound` set from VisitApplications contains every
                    // name introduced by enclosing scopes — including the
                    // top-level params and any let-destructured / let-function
                    // names of the current decl, which are exactly the
                    // caller-owned names. The shadowing check at decomposition
                    // leaves must only reject names introduced by INNER scopes
                    // (lambdas / let-functions / case patterns inside the
                    // body) — i.e. bound names that are NOT in the decl's
                    // owned set.
                    var innerShadowing = bound.Except(ownedNames);

                    for (var k = 0;
                        k < calleeParams.Count && k + 1 < app.Arguments.Count;
                        k++)
                    {
                        var argExpr = SyntaxAnalysis.UnwrapParenthesized(app.Arguments[k + 1].Value);

                        DecomposePatternAndArgForEdges(
                            calleeParams[k],
                            argExpr,
                            ownedNames,
                            innerShadowing,
                            (calleeName, refName) =>
                            {
                                var src = new HigherOrderParameterKey(q, refName);
                                var dst = new HigherOrderParameterKey(calleeDecl, calleeName);

                                if (!forwardEdges.TryGetValue(src, out var dsts))
                                {
                                    dsts = [];
                                    forwardEdges[src] = dsts;
                                }

                                dsts.Add(dst);
                            });
                    }
                });
        }

        // Step 6: reverse-BFS distance computation (same shape as the
        // existing BuildHigherOrderParameterDistanceDictionary).
        var reverseEdges =
            new Dictionary<HigherOrderParameterKey, HashSet<HigherOrderParameterKey>>();

        foreach (var (src, dsts) in forwardEdges)
        {
            foreach (var dst in dsts)
            {
                if (!reverseEdges.TryGetValue(dst, out var preds))
                {
                    preds = [];
                    reverseEdges[dst] = preds;
                }

                preds.Add(src);
            }
        }

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();
            var currentDistance = distance[current];

            if (!reverseEdges.TryGetValue(current, out var preds))
                continue;

            foreach (var pred in preds)
            {
                var candidate = currentDistance + 1;

                if (distance.TryGetValue(pred, out var existing) && existing <= candidate)
                    continue;

                distance[pred] = candidate;
                queue.Enqueue(pred);
            }
        }

        // Step 7: filter to let-binding keys.
        var result = ImmutableDictionary.CreateBuilder<LetBindingHigherOrderKey, int>();

        foreach (var (k, dist) in distance)
        {
            if (letBindingNamesByDecl.TryGetValue(k.Decl, out var lbn) &&
                lbn.Contains(k.ParameterName))
            {
                result[new LetBindingHigherOrderKey(k.Decl, k.ParameterName)] = dist;
            }
        }

        return result.ToImmutable();
    }

    /// <summary>
    /// Walks <paramref name="expression"/> collecting every name
    /// introduced by a <see cref="SyntaxTypes.Expression.LetDeclaration"/>
    /// (both <c>LetFunction</c> names and every name destructured by a
    /// <c>LetDestructuring</c> pattern). Lambda / case / let-function
    /// parameter names are NOT collected — those are separate scopes,
    /// not let-bound names of the enclosing top-level decl.
    /// </summary>
    private static void CollectLetIntroducedNames(
        SyntaxTypes.Expression expression,
        ImmutableHashSet<string>.Builder result)
    {
        ElmSyntaxTransformations.WalkExpressionsWithScope(
            expression,
            [],
            (node, _) =>
            {
                if (node is not SyntaxTypes.Expression.LetExpression letExpr)
                    return;

                foreach (var declNode in letExpr.Value.Declarations)
                {
                    switch (declNode.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction lf:
                            result.Add(lf.Function.Declaration.Value.Name.Value);
                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring ld:
                            foreach (var n in SyntaxAnalysis.CollectNamesBoundByPattern(ld.Pattern.Value))
                                result.Add(n);

                            break;
                    }
                }
            });
    }

    /// <summary>
    /// Recursively walks a callee parameter pattern in parallel with the
    /// matching call-site argument expression and reports per-position
    /// forwarding edges via <paramref name="emit"/>. See the
    /// summary of <see cref="BuildLetBindingHigherOrderDistanceDictionary"/>
    /// for the decomposition rules.
    /// </summary>
    private static void DecomposePatternAndArgForEdges(
        SyntaxTypes.Pattern calleePattern,
        SyntaxTypes.Expression argExpr,
        ImmutableHashSet<string> callerOwnedNames,
        ImmutableHashSet<string> shadowingBoundInArgScope,
        System.Action<string, string> emit)
    {
        DecomposePatternAndArg(
            calleePattern,
            argExpr,
            (bindingName, leafArg) =>
            {
                if (leafArg is SyntaxTypes.Expression.FunctionOrValue fov &&
                    fov.ModuleName.Count is 0 &&
                    callerOwnedNames.Contains(fov.Name) &&
                    !shadowingBoundInArgScope.Contains(fov.Name))
                {
                    emit(bindingName, fov.Name);
                }
            });
    }

    /// <summary>
    /// Generic pattern/argument decomposer. Walks the callee pattern in
    /// parallel with the call-site argument expression; for every
    /// terminal <c>VarPattern</c> / <c>AsPattern</c> binding it invokes
    /// <paramref name="onLeafBinding"/> with the bound name and the
    /// corresponding argument sub-expression. Tuple/Record/Named
    /// constructor patterns recurse into matching sub-expressions only
    /// when the argument is the matching literal shape; on mismatch the
    /// decomposition silently aborts (no edges emitted).
    /// <para>
    /// Both <see cref="DecomposePatternAndArgForEdges"/> (literal bare
    /// reference) and <see cref="DecomposeWithFlowEdges"/> (free-variable
    /// flow tracing through let-binding chains) are thin wrappers around
    /// this helper differing only in the leaf-binding policy.
    /// </para>
    /// </summary>
    private static void DecomposePatternAndArg(
        SyntaxTypes.Pattern calleePattern,
        SyntaxTypes.Expression argExpr,
        System.Action<string, SyntaxTypes.Expression> onLeafBinding)
    {
        // Peel parentheses on both sides.
        while (calleePattern is SyntaxTypes.Pattern.ParenthesizedPattern paren)
            calleePattern = paren.Pattern.Value;

        argExpr = SyntaxAnalysis.UnwrapParenthesized(argExpr);

        switch (calleePattern)
        {
            case SyntaxTypes.Pattern.VarPattern v:
                onLeafBinding(v.Name, argExpr);
                break;

            case SyntaxTypes.Pattern.AsPattern asPattern:
                onLeafBinding(asPattern.Name.Value, argExpr);

                DecomposePatternAndArg(
                    asPattern.Pattern.Value,
                    argExpr,
                    onLeafBinding);

                break;

            case SyntaxTypes.Pattern.TuplePattern tuplePattern:
                {
                    if (argExpr is not SyntaxTypes.Expression.TupledExpression tupled)
                        break;

                    if (tupled.Elements.Count != tuplePattern.Elements.Count)
                        break;

                    for (var i = 0; i < tuplePattern.Elements.Count; i++)
                    {
                        DecomposePatternAndArg(
                            tuplePattern.Elements[i].Value,
                            tupled.Elements[i].Value,
                            onLeafBinding);
                    }

                    break;
                }

            case SyntaxTypes.Pattern.RecordPattern recordPattern:
                {
                    if (argExpr is not SyntaxTypes.Expression.RecordExpr recordExpr)
                        break;

                    foreach (var fieldNameNode in recordPattern.Fields)
                    {
                        var fieldName = fieldNameNode.Value;

                        SyntaxTypes.Expression? fieldValueExpr = null;

                        foreach (var rf in recordExpr.Fields)
                        {
                            if (rf.Value.fieldName.Value == fieldName)
                            {
                                fieldValueExpr = rf.Value.valueExpr.Value;
                                break;
                            }
                        }

                        if (fieldValueExpr is null)
                            continue;

                        // The record pattern field binds a name equal to the
                        // field name. Treat as a VarPattern(fieldName)
                        // matched against the record-expression's value
                        // expression for that field.
                        DecomposePatternAndArg(
                            new SyntaxTypes.Pattern.VarPattern(fieldName),
                            fieldValueExpr,
                            onLeafBinding);
                    }

                    break;
                }

            case SyntaxTypes.Pattern.NamedPattern namedPattern:
                {
                    // The argument must be Application[FunctionOrValue(C), e1, ..., eN].
                    if (argExpr is not SyntaxTypes.Expression.Application app)
                        break;

                    if (app.Arguments.Count < 1)
                        break;

                    var ctorHead = SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value);

                    if (ctorHead is not SyntaxTypes.Expression.FunctionOrValue ctorRef)
                        break;

                    if (ctorRef.Name != namedPattern.Name.Name)
                        break;

                    // Arity check: pattern arg count must match Application's user-visible arg count.
                    if (app.Arguments.Count - 1 != namedPattern.Arguments.Count)
                        break;

                    for (var i = 0; i < namedPattern.Arguments.Count; i++)
                    {
                        DecomposePatternAndArg(
                            namedPattern.Arguments[i].Value,
                            app.Arguments[i + 1].Value,
                            onLeafBinding);
                    }

                    break;
                }
        }
    }

    // ------------------------------------------------------------------
    // Generic higher-order finding API.
    //
    // Aggregates intra- and cross-declaration higher-order analyses into
    // a single, syntax-form-independent result set: every owned name of
    // a top-level function declaration (its destructured parameter
    // bindings AND its let-introduced bindings) whose value influences
    // the function position of at least one Application — directly in
    // the same body or transitively via the higher-order parameter of a
    // known callee — is reported as a <see cref="HigherOrderFinding"/>
    // with the forwarding distance to its nearest direct seed.
    //
    // The implementation reuses the data-flow primitives in
    // <see cref="SyntaxAnalysis"/> (CollectNamesBoundByPattern,
    // ComputeNamesFlowingIntoApplicationFunctions, VisitApplications,
    // AddFlowingNamesOf, ComputeFreeVariables) so the new API is
    // independent of the syntax form of any specific Application,
    // argument, or pattern.
    // ------------------------------------------------------------------

    /// <summary>
    /// A single higher-order finding for a top-level function
    /// declaration. <paramref name="Name"/> is the simple name of an
    /// owned binding (a destructured parameter name or a let-introduced
    /// binding name) whose value influences an Application's function
    /// position. <paramref name="Distance"/> is the number of forwarding
    /// hops to the nearest "direct" higher-order use:
    /// <list type="bullet">
    /// <item><description><c>0</c>: the name flows into an Application's
    /// head expression inside the same declaration's body.</description></item>
    /// <item><description><c>N &gt;= 1</c>: the name flows into the
    /// argument of an Application whose callee is a known top-level
    /// function with a higher-order destructured parameter, transitively
    /// at distance <c>N - 1</c>.</description></item>
    /// </list>
    /// </summary>
    public readonly record struct HigherOrderFinding(
        string Name,
        int Distance);

    /// <summary>
    /// Generic higher-order finding API: returns, for every top-level
    /// function declaration in <paramref name="declarations"/>, the
    /// complete set of <see cref="HigherOrderFinding"/>s for that
    /// declaration's owned names. Declarations with no higher-order
    /// findings map to an empty array.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, ImmutableArray<HigherOrderFinding>>
        FindAllHigherOrderFindings(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var perName = ComputeAllOwnedNameHigherOrderDistances(declarations);

        // Group by declaration; sort findings by name for stable output.
        var grouped = new Dictionary<DeclQualifiedName, List<HigherOrderFinding>>();

        foreach (var (key, dist) in perName)
        {
            if (!grouped.TryGetValue(key.Decl, out var list))
            {
                list = [];
                grouped[key.Decl] = list;
            }

            list.Add(new HigherOrderFinding(key.ParameterName, dist));
        }

        var builder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, ImmutableArray<HigherOrderFinding>>();

        foreach (var (qname, decl) in declarations)
        {
            if (decl is not SyntaxTypes.Declaration.FunctionDeclaration)
                continue;

            if (grouped.TryGetValue(qname, out var list))
            {
                list.Sort(
                    (a, b) =>
                    {
                        var byName = string.CompareOrdinal(a.Name, b.Name);
                        return byName is not 0 ? byName : a.Distance.CompareTo(b.Distance);
                    });

                builder[qname] = [.. list];
            }
            else
            {
                builder[qname] = [];
            }
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Generic higher-order finding API for a single declaration: returns
    /// the higher-order findings for <paramref name="declaration"/>'s
    /// owned names. Equivalent to
    /// <c>FindAllHigherOrderFindings(declarations)[declaration]</c>; the
    /// dedicated entry point exists so callers can express
    /// per-declaration queries without aggregating the dictionary.
    /// </summary>
    public static ImmutableArray<HigherOrderFinding>
        FindHigherOrderFindingsForDeclaration(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        DeclQualifiedName declaration)
    {
        var all = FindAllHigherOrderFindings(declarations);

        return all.TryGetValue(declaration, out var findings) ? findings : [];
    }

    /// <summary>
    /// Core analysis: for every owned name (destructured parameter or
    /// let-introduced binding) of every top-level function declaration
    /// in <paramref name="declarations"/>, computes the shortest
    /// forwarding distance to a direct higher-order use. Returns a
    /// dictionary keyed by <see cref="HigherOrderParameterKey"/> (decl
    /// + owned name) → distance.
    /// </summary>
    private static Dictionary<HigherOrderParameterKey, int>
        ComputeAllOwnedNameHigherOrderDistances(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        // Step 1: collect per-decl parameter patterns.
        var declParamPatterns =
            new Dictionary<DeclQualifiedName, IReadOnlyList<SyntaxTypes.Pattern>>();

        foreach (var (q, d) in declarations)
        {
            if (d is not SyntaxTypes.Declaration.FunctionDeclaration fd)
                continue;

            var list =
                new List<SyntaxTypes.Pattern>(
                    fd.Function.Declaration.Value.Arguments.Count);

            foreach (var argNode in fd.Function.Declaration.Value.Arguments)
                list.Add(argNode.Value);

            declParamPatterns[q] = list;
        }

        // Step 2: by-module lookup for resolving unqualified callees.
        var byModuleAndName =
            ElmSyntaxTransformations.BuildModuleKeyAndDeclNameIndex(declarations);

        // Step 3: per-decl owned name sets (destructured parameter names
        // plus every let-introduced binding name anywhere in the body).
        var ownedNamesByDecl =
            new Dictionary<DeclQualifiedName, ImmutableHashSet<string>>();

        foreach (var (q, d) in declarations)
        {
            if (d is not SyntaxTypes.Declaration.FunctionDeclaration fd)
                continue;

            var fi = fd.Function.Declaration.Value;

            var owned = ImmutableHashSet<string>.Empty.ToBuilder();

            foreach (var argNode in fi.Arguments)
            {
                foreach (var n in SyntaxAnalysis.CollectNamesBoundByPattern(argNode.Value))
                    owned.Add(n);
            }

            var letBindings = ImmutableHashSet<string>.Empty.ToBuilder();
            CollectLetIntroducedNames(fi.Expression.Value, letBindings);

            foreach (var n in letBindings)
                owned.Add(n);

            ownedNamesByDecl[q] = owned.ToImmutable();
        }

        // Step 4: direct seeds — owned names of `decl` that flow into an
        // Application-function position inside `decl`'s body.
        var distance = new Dictionary<HigherOrderParameterKey, int>();
        var queue = new Queue<HigherOrderParameterKey>();

        foreach (var (q, d) in declarations)
        {
            if (d is not SyntaxTypes.Declaration.FunctionDeclaration fd)
                continue;

            var owned = ownedNamesByDecl[q];

            var flowing =
                SyntaxAnalysis.ComputeNamesFlowingIntoApplicationFunctions(
                    fd.Function.Declaration.Value.Expression.Value);

            foreach (var name in flowing)
            {
                if (!owned.Contains(name))
                    continue;

                var key = new HigherOrderParameterKey(q, name);

                if (distance.TryAdd(key, 0))
                    queue.Enqueue(key);
            }
        }

        // Step 5: collect forwarding edges per Application. At each
        // Application whose head resolves to a known top-level callee,
        // we walk the (callee parameter pattern × call-site argument
        // expression) pairs and emit an edge from every caller-owned
        // name that flows into the argument expression (at any
        // destructured-name position of the callee pattern) to the
        // corresponding destructured callee name.
        var forwardEdges =
            new Dictionary<HigherOrderParameterKey, HashSet<HigherOrderParameterKey>>();

        foreach (var (q, d) in declarations)
        {
            if (d is not SyntaxTypes.Declaration.FunctionDeclaration fd)
                continue;

            var fi = fd.Function.Declaration.Value;
            var ownModuleKey = string.Join(".", q.Namespaces);
            var ownedNames = ownedNamesByDecl[q];

            SyntaxAnalysis.VisitApplications(
                fi.Expression.Value,
                letRhsByName: [],
                bound: [],
                onApplication: (app, letRhsByName, bound) =>
                {
                    if (app.Arguments.Count < 2)
                        return;

                    var head = SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value);

                    if (head is not SyntaxTypes.Expression.FunctionOrValue fov)
                        return;

                    var moduleKey =
                        fov.ModuleName.Count is 0
                        ?
                        ownModuleKey
                        :
                        string.Join(".", fov.ModuleName);

                    if (!byModuleAndName.TryGetValue((moduleKey, fov.Name), out var calleeDecl))
                        return;

                    if (!declParamPatterns.TryGetValue(calleeDecl, out var calleeParams))
                        return;

                    // `bound` contains everything introduced by enclosing
                    // scopes — including caller-owned names. Inner-only
                    // shadowing is `bound \ ownedNames`; these are names
                    // introduced by inner lambdas / let-functions /
                    // case patterns that should NOT trigger edges.
                    var innerShadowing = bound.Except(ownedNames);

                    for (var k = 0;
                        k < calleeParams.Count && k + 1 < app.Arguments.Count;
                        k++)
                    {
                        var argExpr = SyntaxAnalysis.UnwrapParenthesized(app.Arguments[k + 1].Value);

                        DecomposeWithFlowEdges(
                            calleeParams[k],
                            argExpr,
                            ownedNames,
                            innerShadowing,
                            letRhsByName,
                            (calleeName, refName) =>
                            {
                                var src = new HigherOrderParameterKey(q, refName);
                                var dst = new HigherOrderParameterKey(calleeDecl, calleeName);

                                if (!forwardEdges.TryGetValue(src, out var dsts))
                                {
                                    dsts = [];
                                    forwardEdges[src] = dsts;
                                }

                                dsts.Add(dst);
                            });
                    }
                });
        }

        // Step 6: reverse-BFS distance computation.
        var reverseEdges =
            new Dictionary<HigherOrderParameterKey, HashSet<HigherOrderParameterKey>>();

        foreach (var (src, dsts) in forwardEdges)
        {
            foreach (var dst in dsts)
            {
                if (!reverseEdges.TryGetValue(dst, out var preds))
                {
                    preds = [];
                    reverseEdges[dst] = preds;
                }

                preds.Add(src);
            }
        }

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();
            var currentDistance = distance[current];

            if (!reverseEdges.TryGetValue(current, out var preds))
                continue;

            foreach (var pred in preds)
            {
                var candidate = currentDistance + 1;

                if (distance.TryGetValue(pred, out var existing) && existing <= candidate)
                    continue;

                distance[pred] = candidate;
                queue.Enqueue(pred);
            }
        }

        // Step 7: restrict to keys whose name is in the corresponding
        // decl's owned-name set. (Direct-seed names are already owned;
        // edge destinations are callee destructured names which may not
        // be "owned" from the caller's perspective but get their own
        // direct seeds from the callee's own analysis. The reverse BFS
        // only assigns distance to predecessor keys, which are caller
        // owned names — so this filter is conservative.)
        var filtered = new Dictionary<HigherOrderParameterKey, int>();

        foreach (var (key, dist) in distance)
        {
            if (ownedNamesByDecl.TryGetValue(key.Decl, out var owned) &&
                owned.Contains(key.ParameterName))
            {
                filtered[key] = dist;
            }
        }

        return filtered;
    }

    /// <summary>
    /// Like <see cref="DecomposePatternAndArgForEdges"/>, but for the
    /// terminal <c>VarPattern</c> / <c>AsPattern</c> cases it does not
    /// require the argument to be a bare reference. Instead, it uses
    /// <see cref="SyntaxAnalysis.CollectRemainingFreeVariables(SyntaxTypes.Expression)"/> together with
    /// <see cref="SyntaxAnalysis.AddFlowingNamesOf"/> to find every
    /// caller-owned name whose value can flow into the argument
    /// expression — directly (free variable in the argument) or via
    /// chained let-binding right-hand sides recorded in
    /// <paramref name="letRhsByName"/>.
    /// </summary>
    private static void DecomposeWithFlowEdges(
        SyntaxTypes.Pattern calleePattern,
        SyntaxTypes.Expression argExpr,
        ImmutableHashSet<string> callerOwnedNames,
        ImmutableHashSet<string> innerShadowing,
        ImmutableDictionary<string, SyntaxTypes.Expression> letRhsByName,
        System.Action<string, string> emit)
    {
        DecomposePatternAndArg(
            calleePattern,
            argExpr,
            (bindingName, leafArg) =>
            EmitFlowEdgesForBinding(
                bindingName,
                leafArg,
                callerOwnedNames,
                innerShadowing,
                letRhsByName,
                emit));
    }

    /// <summary>
    /// Emits forwarding edges for a single terminal binding name on the
    /// callee side: every caller-owned name reachable from
    /// <paramref name="argExpr"/> (via free variables and let-binding
    /// chains) becomes the source of an edge to
    /// <paramref name="calleeBindingName"/>.
    /// </summary>
    private static void EmitFlowEdgesForBinding(
        string calleeBindingName,
        SyntaxTypes.Expression argExpr,
        ImmutableHashSet<string> callerOwnedNames,
        ImmutableHashSet<string> innerShadowing,
        ImmutableDictionary<string, SyntaxTypes.Expression> letRhsByName,
        System.Action<string, string> emit)
    {
        // Direct free variables of argExpr — captures bare references,
        // record accesses on owned values, etc.
        foreach (var freeName in SyntaxAnalysis.CollectRemainingFreeVariables(argExpr))
        {
            if (callerOwnedNames.Contains(freeName) &&
                !innerShadowing.Contains(freeName))
            {
                emit(calleeBindingName, freeName);
            }
        }

        // Traced names: AddFlowingNamesOf resolves through chained
        // let-binding right-hand sides recorded in letRhsByName.
        var traced = ImmutableHashSet<string>.Empty.ToBuilder();

        SyntaxAnalysis.AddFlowingNamesOf(
            argExpr,
            letRhsByName,
            innerShadowing,
            traced,
            visited: []);

        foreach (var name in traced)
        {
            if (callerOwnedNames.Contains(name))
                emit(calleeBindingName, name);
        }
    }
}
