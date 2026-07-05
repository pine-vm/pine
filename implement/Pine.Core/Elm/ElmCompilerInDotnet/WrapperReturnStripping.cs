using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

// Alias to avoid ambiguity with System.Range
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Step B of solution
/// <em>D2 — Whole-program monomorphization of function-typed parameters</em>:
/// generation of the stripped sibling decl + rewriting of every
/// fully-saturated call site to wrap the sibling's result with the
/// original constructor. Builds on the pure analysis primitives in
/// <see cref="NewtypeWrapperAnalysis"/>.
/// <para>
/// Pure transformations only — this module deliberately does not hook
/// into the <see cref="ElmSyntaxOptimization"/> pipeline. Step C performs that
/// integration.
/// </para>
/// <para>
/// See <c>explore/internal-analysis/2026-05-10-compiled-expressions-diff-for-parse-file-scenarios.md</c>
/// section "D2 — Step 2 plan: stripping the newtype-wrapper from function-return roots"
/// for the design rationale.
/// </para>
/// </summary>
internal static class WrapperReturnStripping
{
    private static readonly Location s_zeroLoc = new(Row: 0, Column: 0);

    private static readonly Range s_zeroRange = new(Start: s_zeroLoc, End: s_zeroLoc);

    /// <summary>Suffix appended to the original decl name to produce the stripped sibling name.</summary>
    public const string StrippedSuffix = GeneratedNameSuffixes.Stripped;

    /// <summary>
    /// Generated rewrite plan for one function decl whose body's root
    /// applies a newtype-wrapper constructor.
    /// </summary>
    /// <param name="OriginalDeclName">Qualified name of the source decl <c>f</c>.</param>
    /// <param name="StrippedDeclName">Qualified name of the generated sibling <c>f__stripped</c>.</param>
    /// <param name="ConstructorName">
    /// Qualified name of the wrapper constructor that
    /// <see cref="OriginalDeclName"/>'s body applies at its root.
    /// Caller-side rewrites re-introduce this constructor at every
    /// fully-saturated call site.
    /// </param>
    /// <param name="OriginalArity">
    /// Number of pattern arguments declared by <see cref="OriginalDeclName"/>.
    /// A call site is "fully saturated" iff its argument list (excluding
    /// the head) has exactly this many entries.
    /// </param>
    /// <param name="StrippedBody">
    /// Body expression of the stripped sibling — derived from the
    /// original body with the outermost wrapper-constructor application
    /// peeled off (re-threaded through <c>Let</c> / <c>CaseOf</c> when
    /// present).
    /// </param>
    /// <remarks>
    /// The fields above are the WrapperReturnStripping-specific data;
    /// the call-site rewriting metadata (per-parameter origins +
    /// result transform) is exposed uniformly for any pass that needs
    /// it via <see cref="ToGeneratedSiblingDecl"/>. WrapperReturnStripping's
    /// strip plans always produce identity parameter origins and a
    /// <see cref="SiblingResultTransform.WrapWithConstructor"/> result
    /// transform that re-introduces the original's outer constructor
    /// at the call site.
    /// </remarks>
    public sealed record WrapperStripPlan(
        DeclQualifiedName OriginalDeclName,
        DeclQualifiedName StrippedDeclName,
        DeclQualifiedName ConstructorName,
        int OriginalArity,
        SyntaxTypes.Expression StrippedBody)
    {
        /// <summary>
        /// Projects this <see cref="WrapperStripPlan"/> onto the
        /// shared <see cref="GeneratedSiblingDecl"/> model used by
        /// downstream passes (notably <c>WrapUnwrapCancellation</c>)
        /// to drive call-site rewriting in a uniform way.
        /// <para>
        /// For wrapper-return-stripping, the sibling has the same
        /// arity as the original and each sibling parameter is
        /// <see cref="SiblingParameterOrigin.Identity"/> at the
        /// matching original index. The result transform is
        /// <see cref="SiblingResultTransform.WrapWithConstructor"/>
        /// over <see cref="ConstructorName"/>.
        /// </para>
        /// </summary>
        public GeneratedSiblingDecl ToGeneratedSiblingDecl()
        {
            var origins = new SiblingParameterOrigin[OriginalArity];

            for (var i = 0; i < OriginalArity; i++)
                origins[i] = new SiblingParameterOrigin.Identity(SiblingIndex: i, OriginalIndex: i);

            return
                new GeneratedSiblingDecl(
                    OriginalDeclName: OriginalDeclName,
                    SiblingDeclName: StrippedDeclName,
                    OriginalArity: OriginalArity,
                    ParameterOrigins: origins,
                    ResultTransform: new SiblingResultTransform.WrapWithConstructor(ConstructorName));
        }
    }

    /// <summary>
    /// Recursive extension of <see cref="NewtypeWrapperAnalysis.TryMatchWrapperReturnBody"/>
    /// that additionally re-threads through:
    /// <list type="bullet">
    ///   <item><description>An outer <see cref="SyntaxTypes.Expression.LetExpression"/> — the let-block's final expression must match.</description></item>
    ///   <item><description>An outer <see cref="SyntaxTypes.Expression.CaseExpression"/> — every arm must match the same constructor.</description></item>
    /// </list>
    /// Both cases produce a stripped body that re-wraps the
    /// <c>Let</c> / <c>CaseOf</c> shell around the per-arm stripped
    /// expression.
    /// </summary>
    public static NewtypeWrapperAnalysis.WrapperReturnMatch? TryMatchExtendedWrapperReturnBody(
        SyntaxTypes.Expression body,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ModuleName currentModuleName)
    {
        // Direct shape: (parens-peeled) `Application[Ctor, inner]`.
        var direct =
            NewtypeWrapperAnalysis.TryMatchWrapperReturnBody(
                body,
                registry,
                currentModuleName);

        if (direct is not null)
            return direct;

        // Peel parens for further structural matching.
        var peeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(body);

        if (peeled is SyntaxTypes.Expression.LetExpression letExpr)
        {
            var inner =
                TryMatchExtendedWrapperReturnBody(
                    letExpr.Value.Expression.Value,
                    registry,
                    currentModuleName);

            if (inner is null)
                return null;

            var rewrittenLet =
                new SyntaxTypes.Expression.LetExpression(
                    new SyntaxTypes.Expression.LetBlock(
                        Declarations: letExpr.Value.Declarations,
                        Expression:
                        new Node<SyntaxTypes.Expression>(
                            letExpr.Value.Expression.Range,
                            inner.StrippedInnerExpression)));

            return
                new NewtypeWrapperAnalysis.WrapperReturnMatch(
                    inner.ConstructorName,
                    rewrittenLet);
        }

        if (peeled is SyntaxTypes.Expression.CaseExpression caseExpr)
        {
            var cases = caseExpr.CaseBlock.Cases;

            // An empty case-of has no arms to match — refuse.
            if (cases.Count is 0)
                return null;

            DeclQualifiedName? sharedCtor = null;
            var rewrittenArms = new List<SyntaxTypes.Case>(cases.Count);

            foreach (var arm in cases)
            {
                var armMatch =
                    TryMatchExtendedWrapperReturnBody(
                        arm.Expression.Value,
                        registry,
                        currentModuleName);

                if (armMatch is null)
                    return null;

                if (sharedCtor is null)
                {
                    sharedCtor = armMatch.ConstructorName;
                }
                else if (!sharedCtor.Equals(armMatch.ConstructorName))
                {
                    // Arms wrap different constructors — cannot strip
                    // uniformly without changing semantics.
                    return null;
                }

                rewrittenArms.Add(
                    new SyntaxTypes.Case(
                        arm.Pattern,
                        new Node<SyntaxTypes.Expression>(
                            arm.Expression.Range,
                            armMatch.StrippedInnerExpression)));
            }

            var rewrittenCase =
                new SyntaxTypes.Expression.CaseExpression(
                    new SyntaxTypes.CaseBlock(
                        Expression: caseExpr.CaseBlock.Expression,
                        Cases: rewrittenArms));

            return
                new NewtypeWrapperAnalysis.WrapperReturnMatch(
                    sharedCtor!,
                    rewrittenCase);
        }

        return null;
    }

    /// <summary>
    /// Reconstructs the <see cref="GeneratedSiblingDecl"/> registry for
    /// every <c>__stripped</c> sibling already present in
    /// <paramref name="declarations"/>. Each entry maps the original
    /// decl's qualified name to the metadata needed for sibling-aware
    /// <see cref="WrapUnwrapCancellation"/> at fully-saturated call
    /// sites.
    /// <para>
    /// This helper exists so the terminal
    /// <see cref="ElmSyntaxOptimization.ApplyWrapUnwrapCancellation(OptimizedElmSyntaxDeclarations, ElmSyntaxOptimization.Config)"/>
    /// pass that runs after Phase 3 size-based inlining can be
    /// sibling-aware as well: the <c>__stripped</c> decls were
    /// emitted by Phases 1+2 and persist into Phase 3, but the
    /// in-memory registry was scoped to the Phase-1+2 step and is
    /// no longer available at the terminal call site.
    /// </para>
    /// <para>
    /// Detection: a decl <c>f</c> is considered to have a registered
    /// stripped sibling <c>f__stripped</c> iff
    /// <list type="number">
    ///   <item>a decl named <c>f__stripped</c> exists in
    ///   <paramref name="declarations"/>;</item>
    ///   <item><c>f</c> exists, is a <c>FunctionDeclaration</c>, and
    ///   has a body in the canonical post-WRS forwarding form
    ///   <c>Wrap (f__stripped a1 a2 …)</c> — i.e. its body is an
    ///   <see cref="SyntaxTypes.Expression.Application"/> whose head
    ///   is a wrapper-constructor reference and whose only argument
    ///   is a fully-saturated call to <c>f__stripped</c> passing
    ///   <c>f</c>'s argument names (or unit literals) in order.</item>
    /// </list>
    /// </para>
    /// <para>
    /// Decls that match (1) but not (2) are silently skipped — they
    /// are not safe to assume came from <see cref="WrapperReturnStripping"/>.
    /// This makes the helper conservative by design: it never
    /// fabricates a registry entry whose <c>WrapWithConstructor</c>
    /// transform would be incorrect.
    /// </para>
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl>
        ReconstructSiblingRegistry(
        OptimizedElmSyntaxDeclarations declarations) =>
        ReconstructSiblingRegistry(declarations.RenderAsFlatDictionary());

    /// <summary>
    /// Flat-dictionary overload of <see cref="ReconstructSiblingRegistry(OptimizedElmSyntaxDeclarations)"/>.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl>
        ReconstructSiblingRegistry(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, GeneratedSiblingDecl>();

        foreach (var (declName, decl) in declarations)
        {
            if (!declName.DeclName.EndsWith(StrippedSuffix, StringComparison.Ordinal))
                continue;

            // Derive the original decl's name by trimming the suffix.
            var originalSimpleName =
                declName.DeclName[..^StrippedSuffix.Length];

            if (originalSimpleName.Length is 0)
                continue;

            var originalDeclName =
                DeclQualifiedName.Create(declName.Namespaces, originalSimpleName);

            if (!declarations.TryGetValue(originalDeclName, out var originalDecl))
                continue;

            if (originalDecl is not SyntaxTypes.Declaration.FunctionDeclaration originalFuncDecl)
                continue;

            var originalImpl = originalFuncDecl.Function.Declaration.Value;

            // Body must be: Application[FunctionOrValue(Wrap),
            //                Application[FunctionOrValue(f__stripped), …pNames]]
            var body =
                SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(originalImpl.Expression.Value);

            if (body is not SyntaxTypes.Expression.Application outerApp)
                continue;

            if (outerApp.Arguments.Count is not 2)
                continue;

            var outerHead =
                SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(outerApp.Arguments[0].Value);

            if (outerHead is not SyntaxTypes.Expression.FunctionOrValue outerHeadFov)
                continue;

            var ctorQName =
                ElmSyntaxTransformations.ResolveReference(outerHeadFov, declName.Namespaces);

            var inner =
                SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(outerApp.Arguments[1].Value);

            if (inner is not SyntaxTypes.Expression.Application innerApp)
                continue;

            // The inner call must be a saturated self-call to the
            // stripped sibling, with exactly the original decl's
            // parameter names (or unit literals for unit-pattern
            // positions) in order.
            if (!IsSaturatedSelfCall(innerApp, declName, originalImpl.Arguments))
                continue;

            // All checks pass: build the GeneratedSiblingDecl entry.
            var origins = new SiblingParameterOrigin[originalImpl.Arguments.Count];

            for (var i = 0; i < origins.Length; i++)
                origins[i] = new SiblingParameterOrigin.Identity(SiblingIndex: i, OriginalIndex: i);

            builder[originalDeclName] =
                new GeneratedSiblingDecl(
                    OriginalDeclName: originalDeclName,
                    SiblingDeclName: declName,
                    OriginalArity: originalImpl.Arguments.Count,
                    ParameterOrigins: origins,
                    ResultTransform: new SiblingResultTransform.WrapWithConstructor(ctorQName));
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Scans every <see cref="SyntaxTypes.Declaration.FunctionDeclaration"/>
    /// in <paramref name="declarations"/> and produces a strip plan
    /// for each whose body matches via
    /// <see cref="TryMatchExtendedWrapperReturnBody"/>. Returns a
    /// dictionary keyed on the original decl's qualified name.
    /// </summary>
    /// <summary>
    /// <see cref="OptimizedElmSyntaxDeclarations"/>-flavoured overload of
    /// <see cref="BuildStripPlans(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration}, ImmutableDictionary{DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo})"/>.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, WrapperStripPlan>
        BuildStripPlans(
        OptimizedElmSyntaxDeclarations declarations,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry) =>
        BuildStripPlans(declarations.RenderAsFlatDictionary(), registry);

    public static ImmutableDictionary<DeclQualifiedName, WrapperStripPlan>
        BuildStripPlans(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, WrapperStripPlan>();

        foreach (var (declName, decl) in declarations)
        {
            if (decl is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                continue;

            var impl = funcDecl.Function.Declaration.Value;

            // Stripping a zero-argument decl provides no benefit (no
            // saturation work to fold) and complicates rewriting since
            // there is no Application form to match — skip.
            if (impl.Arguments.Count is 0)
                continue;

            // Restrict planning to decls whose argument patterns are
            // all simple <see cref="SyntaxTypes.Pattern.VarPattern"/>
            // or <see cref="SyntaxTypes.Pattern.UnitPattern"/>. For
            // non-var/non-unit patterns (e.g. ctor destructuring like
            // <c>oneOf2 (Parser pA) (Parser pB)</c>) we cannot safely
            // synthesise a forwarding body for the original decl
            // (Step 2's anti-inlining-re-introduction trick), and
            // without that body the optimization pipeline re-introduces
            // the partial application every time it inlines the
            // wrapper-form decl. Keep the door open for a future
            // enhancement that handles non-var patterns; for now,
            // restricting to var-or-unit is the safe, useful subset
            // that covers all lambda-lifted wrappers like
            // <c>lazy thunk</c>, <c>oneOf2__lifted__lambda1 a b s</c>,
            // <c>lazy__lifted__lambda1 thunk s</c>, plus zero-arg-on-
            // unit decls like <c>rec1 ()</c> commonly produced by
            // mutually-recursive parser-combinator definitions
            // (see §7.0 of
            // explore/internal-analysis/2026-05-18-expand-elm-syntax-optimizations-for-higher-order-parameter-elimination.md).
            var allArgsAreVarOrUnitPatterns = true;

            foreach (var argNode in impl.Arguments)
            {
                if (argNode.Value is not SyntaxTypes.Pattern.VarPattern
                    and not SyntaxTypes.Pattern.UnitPattern)
                {
                    allArgsAreVarOrUnitPatterns = false;
                    break;
                }
            }

            if (!allArgsAreVarOrUnitPatterns)
                continue;

            var match =
                TryMatchExtendedWrapperReturnBody(
                    impl.Expression.Value,
                    registry,
                    declName.Namespaces);

            if (match is null)
                continue;

            // Refuse to plan a strip whose generated sibling name would
            // collide with an existing decl in the same module — keeps
            // the rewriter idempotent.
            //
            // §11.8: the collision check used to be name-only, which
            // would silently accept a divergent body if a previous
            // pass had already emitted a different <c>f__stripped</c>.
            // Tighten by fingerprinting both the existing entry and
            // the sibling we are about to emit (via
            // <see cref="DeclarationDeduplication.GetStructuralFingerprint"/>)
            // and throwing loudly on mismatch — in a correct pipeline
            // the two must agree, so a mismatch surfaces a real bug.
            //
            // Exception: when the planner is re-run on a dictionary
            // that already contains the output of a previous run, the
            // original decl <c>f</c> will have been rewritten to its
            // forwarding form <c>Wrap (f__stripped …args)</c>. Stripping
            // it again would produce a strip-plan body that is a
            // saturated self-call to the existing sibling. That is
            // *legitimate* idempotent re-application — not a bug — so
            // we detect it structurally and silently skip the plan.
            var strippedName =
                DeclQualifiedName.Create(
                    declName.Namespaces,
                    declName.DeclName + StrippedSuffix);

            if (declarations.TryGetValue(strippedName, out var existingStripped))
            {
                if (existingStripped is not SyntaxTypes.Declaration.FunctionDeclaration existingFuncDecl)
                {
                    throw new InvalidOperationException(
                        "WrapperReturnStripping: name '" + strippedName +
                        "' is occupied by a non-function declaration, blocking strip-plan emission.");
                }

                // Idempotency detection: planned StrippedBody is a
                // fully-saturated <c>strippedName(…paramNames)</c>
                // call — i.e. <c>f</c>'s body is already in
                // post-strip forwarding form. Silently skip.
                if (IsSaturatedSelfCall(match.StrippedInnerExpression, strippedName, impl.Arguments))
                {
                    continue;
                }

                var plannedPlan =
                    new WrapperStripPlan(
                        OriginalDeclName: declName,
                        StrippedDeclName: strippedName,
                        ConstructorName: match.ConstructorName,
                        OriginalArity: impl.Arguments.Count,
                        StrippedBody: match.StrippedInnerExpression);

                var plannedSibling = GenerateStrippedSibling(funcDecl, plannedPlan);

                var existingFingerprint =
                    DeclarationDeduplication.GetStructuralFingerprint(
                        existingFuncDecl,
                        strippedName.Namespaces);

                var plannedFingerprint =
                    DeclarationDeduplication.GetStructuralFingerprint(
                        plannedSibling,
                        strippedName.Namespaces);

                if (!string.Equals(existingFingerprint, plannedFingerprint, StringComparison.Ordinal))
                {
                    throw new InvalidOperationException(
                        "WrapperReturnStripping: existing decl '" +
                        strippedName +
                        "' has a body that diverges from the body this strip-plan would produce — " +
                        "a previous pass appears to have emitted a different sibling under the same name. " +
                        "This indicates a pipeline-ordering bug; the sibling name should be unique per " +
                        "(original decl, strip semantics).");
                }

                // Names + bodies agree: nothing to do — skip emission
                // to keep the rewriter idempotent.
                continue;
            }

            builder[declName] =
                new WrapperStripPlan(
                    OriginalDeclName: declName,
                    StrippedDeclName: strippedName,
                    ConstructorName: match.ConstructorName,
                    OriginalArity: impl.Arguments.Count,
                    StrippedBody: match.StrippedInnerExpression);
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Synthesises the stripped sibling
    /// <see cref="SyntaxTypes.Declaration.FunctionDeclaration"/> from
    /// the original decl and its <see cref="WrapperStripPlan"/>. The
    /// sibling shares the original's argument patterns but uses the
    /// plan's <see cref="WrapperStripPlan.StrippedBody"/> as its
    /// implementation. The type signature is dropped — post-lowering
    /// decls do not always carry one and the stripped sibling's return
    /// type differs from the original's anyway.
    /// </summary>
    public static SyntaxTypes.Declaration.FunctionDeclaration GenerateStrippedSibling(
        SyntaxTypes.Declaration.FunctionDeclaration original,
        WrapperStripPlan plan)
    {
        var origImpl = original.Function.Declaration.Value;

        var strippedImpl =
            new SyntaxTypes.FunctionImplementation(
                Name: new Node<string>(s_zeroRange, plan.StrippedDeclName.DeclName),
                Arguments: origImpl.Arguments,
                Expression:
                new Node<SyntaxTypes.Expression>(s_zeroRange, plan.StrippedBody));

        var strippedFunc =
            new SyntaxTypes.FunctionStruct(
                Documentation: null,
                Signature: null,
                Declaration: new Node<SyntaxTypes.FunctionImplementation>(s_zeroRange, strippedImpl));

        return new SyntaxTypes.Declaration.FunctionDeclaration(strippedFunc);
    }

    /// <summary>
    /// True iff <paramref name="expr"/> is exactly
    /// <c>Application[FunctionOrValue(<paramref name="targetName"/>), …pNames]</c>
    /// where <c>pNames</c> are the var-pattern parameter names of
    /// <paramref name="parameterPatterns"/>, in matching order.
    /// <para>
    /// Used by the §11.8 idempotency detection in <c>BuildStripPlans</c>
    /// to recognise the "this decl is already in post-strip
    /// forwarding form" case (<c>f x = Wrap (f__stripped x)</c>),
    /// which would otherwise look like a divergent-sibling collision.
    /// </para>
    /// </summary>
    private static bool IsSaturatedSelfCall(
        SyntaxTypes.Expression expr,
        DeclQualifiedName targetName,
        IReadOnlyList<Node<SyntaxTypes.Pattern>> parameterPatterns)
    {
        if (expr is not SyntaxTypes.Expression.Application app)
            return false;

        // Application.Arguments holds [head, …callArgs]; head + callArgs.Count must equal arity + 1.
        if (app.Arguments.Count != parameterPatterns.Count + 1)
            return false;

        if (app.Arguments[0].Value is not SyntaxTypes.Expression.FunctionOrValue headFov)
            return false;

        if (!string.Equals(headFov.Name, targetName.DeclName, StringComparison.Ordinal))
            return false;

        // The reference may be qualified (Module.f__stripped) or unqualified
        // (f__stripped). Accept either as long as the unqualified name matches —
        // when the call site is intra-module both forms are semantically equal.
        if (headFov.ModuleName.Count is not 0)
        {
            var moduleNameComparer =
                EnumerableExtensions.EqualityComparer<ModuleName>();

            if (!moduleNameComparer.Equals(headFov.ModuleName, targetName.Namespaces))
                return false;
        }

        for (var i = 0; i < parameterPatterns.Count; i++)
        {
            switch (parameterPatterns[i].Value)
            {
                case SyntaxTypes.Pattern.VarPattern paramVar:
                    if (app.Arguments[i + 1].Value is not SyntaxTypes.Expression.FunctionOrValue argFov)
                        return false;

                    if (argFov.ModuleName.Count is not 0)
                        return false;

                    if (!string.Equals(argFov.Name, paramVar.Name, StringComparison.Ordinal))
                        return false;

                    break;

                case SyntaxTypes.Pattern.UnitPattern:

                    // Unit-pattern forwarding emits a literal UnitExpr
                    // (see BuildForwardingDecl). A previously emitted
                    // forwarding body is recognised iff the argument
                    // at this position is also a UnitExpr.
                    if (app.Arguments[i + 1].Value is not SyntaxTypes.Expression.UnitExpr)
                        return false;

                    break;

                default:
                    return false;
            }
        }

        return true;
    }

    /// <summary>
    /// Rewrites every fully-saturated call site
    /// <c>Application[FunctionOrValue(f), …args]</c> in
    /// <paramref name="expr"/> where <c>f</c> is in
    /// <paramref name="stripPlans"/> and <c>args.Count == plan.OriginalArity</c>
    /// to <c>Application[FunctionOrValue(WrapperCtor),
    /// Application[FunctionOrValue(f__stripped), …args]]</c>.
    /// Partial applications (fewer args than the arity) and over-
    /// saturated forms (more args than the arity) are left alone.
    /// </summary>
    public static SyntaxTypes.Expression RewriteCallSitesInExpression(
        SyntaxTypes.Expression expr,
        ImmutableDictionary<DeclQualifiedName, WrapperStripPlan> stripPlans,
        ModuleName currentModuleName)
    {
        if (stripPlans.IsEmpty)
            return expr;

        Node<SyntaxTypes.Expression> RecurseNode(Node<SyntaxTypes.Expression> child) =>
            new(child.Range, RewriteCallSitesInExpression(child.Value, stripPlans, currentModuleName));

        if (expr is SyntaxTypes.Expression.Application app && app.Arguments.Count >= 2)
        {
            // Recurse into children first so nested call sites are
            // rewritten before we evaluate this Application's own shape.
            var rewrittenChildren =
                app.Arguments.Select(RecurseNode).ToList();

            var headPeeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(rewrittenChildren[0].Value);

            if (headPeeled is SyntaxTypes.Expression.FunctionOrValue head)
            {
                var qname = ElmSyntaxTransformations.ResolveReference(head, currentModuleName);

                if (stripPlans.TryGetValue(qname, out var plan))
                {
                    var argCount = rewrittenChildren.Count - 1;

                    if (argCount == plan.OriginalArity)
                    {
                        // Build: Application[Ctor, Application[f__stripped, …args]]
                        var strippedHead =
                            new SyntaxTypes.Expression.FunctionOrValue(
                                plan.StrippedDeclName.Namespaces,
                                plan.StrippedDeclName.DeclName);

                        var strippedAppArgs =
                            new List<Node<SyntaxTypes.Expression>>(rewrittenChildren.Count)
                            {
                                new(s_zeroRange, strippedHead),
                            };

                        for (var i = 1; i < rewrittenChildren.Count; i++)
                            strippedAppArgs.Add(rewrittenChildren[i]);

                        var strippedApp =
                            new SyntaxTypes.Expression.Application(strippedAppArgs);

                        var ctorRef =
                            new SyntaxTypes.Expression.FunctionOrValue(
                                plan.ConstructorName.Namespaces,
                                plan.ConstructorName.DeclName);

                        return
                            new SyntaxTypes.Expression.Application(
                                [
                                new Node<SyntaxTypes.Expression>(s_zeroRange, ctorRef),
                                new Node<SyntaxTypes.Expression>(s_zeroRange, strippedApp),
                                ]);
                    }
                }
            }

            return new SyntaxTypes.Expression.Application(rewrittenChildren);
        }

        // Default: structural recursion via the central child-mapper.
        return ElmSyntaxTransformations.MapChildExpressions(expr, RecurseNode);
    }

    /// <summary>
    /// Top-level orchestrator: builds the newtype registry, derives
    /// strip plans, replaces every planned decl's body with a
    /// forwarding delegation
    /// <c>Wrap (f__stripped a1 a2 …)</c>, rewrites every other decl's
    /// body's call sites the same way, and adds the generated stripped
    /// sibling decls to the result.
    /// <para>
    /// Replacing the original decl's body — rather than keeping it
    /// verbatim — is crucial: subsequent <c>size-based inlining</c>
    /// passes inline the original decl into call sites, and if the
    /// original still carried the wrapper-form body
    /// <c>Wrap (lifted__lambda thunk)</c>, inlining would re-introduce
    /// the closure-allocating partial application at every site. By
    /// forwarding to the stripped sibling, the inlined form is
    /// <c>Wrap (f__stripped &lt;arg&gt;)</c> — fully saturated, no PA
    /// inside — and the surrounding <c>let (Wrap x) = … in …</c>
    /// destructuring can be reduced by case-of-known-constructor.
    /// </para>
    /// </summary>
    /// <summary>
    /// <see cref="OptimizedElmSyntaxDeclarations"/>-flavoured overload of
    /// <see cref="RewriteDeclarationDictionary(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration})"/>.
    /// </summary>
    public static OptimizedElmSyntaxDeclarations
        RewriteDeclarationDictionary(
        OptimizedElmSyntaxDeclarations declarations) =>
        OptimizedElmSyntaxDeclarations.FromFlatDictionary(
            RewriteDeclarationDictionary(declarations.RenderAsFlatDictionary()));

    public static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        RewriteDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var registry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(declarations);

        if (registry.IsEmpty)
            return declarations;

        var stripPlans = BuildStripPlans(declarations, registry);

        if (stripPlans.IsEmpty)
            return declarations;

        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (declName, decl) in declarations)
        {
            if (stripPlans.TryGetValue(declName, out var planForThisDecl) &&
                decl is SyntaxTypes.Declaration.FunctionDeclaration planFunc)
            {
                // Replace the original decl's body with a forwarding
                // call to the stripped sibling. The arg patterns are
                // guaranteed to be VarPattern (see BuildStripPlans).
                builder[declName] = BuildForwardingDecl(planFunc, planForThisDecl);
            }
            else
            {
                builder[declName] =
                    RewriteCallSitesInDeclaration(decl, stripPlans, declName.Namespaces);
            }
        }

        // Append the generated stripped siblings. Their bodies still
        // reference the strip plans (e.g. recursive calls to the
        // original `f` via FunctionOrValue) — BuildStripPlans copies
        // the original inner expression verbatim, so run the rewriter
        // once on each generated sibling so recursive call sites
        // inside the stripped body get the same treatment.
        //
        // No explicit qualification pass here: the final qualification
        // sweep below visits every FunctionDeclaration in
        // <c>builder</c> (siblings included) and qualifies same-module
        // top-level references in one place. Doing it twice was the
        // redundancy called out as §11.12 in the design plan.
        foreach (var (origName, plan) in stripPlans)
        {
            if (declarations[origName] is not SyntaxTypes.Declaration.FunctionDeclaration origFunc)
                continue;

            var sibling = GenerateStrippedSibling(origFunc, plan);

            var rewrittenSibling =
                RewriteCallSitesInDeclaration(sibling, stripPlans, plan.StrippedDeclName.Namespaces);

            builder[plan.StrippedDeclName] = rewrittenSibling;
        }

        // Precompute the set of top-level declaration names per module
        // namespace. Used to qualify any unqualified
        // <see cref="SyntaxTypes.Expression.FunctionOrValue"/> reference
        // inside an emitted function-declaration body that names a
        // same-module top-level decl. Without qualification, downstream
        // consumers like
        // <see cref="SnapshotTestFormat.RenderQualifiedDeclarations"/>
        // and <see cref="DeclarationDeduplication.GetStructuralFingerprint"/>
        // (which goes through the same renderer) reject the body with
        // "contains unqualified reference '…' that is not a local
        // binding".
        var topLevelNamesByModule =
            BuildTopLevelDeclNamesByModule(declarations);

        // Single qualification sweep: ensure every function declaration
        // emitted by this pass has its body fully qualified for any
        // same-module top-level reference. This covers three cases:
        //   1. Stripped sibling bodies that copy inner expressions
        //      from the original decl verbatim (see
        //      <see cref="BuildStripPlans"/>).
        //   2. Forwarding bodies produced by BuildForwardingDecl —
        //      these already qualify via plan.StrippedDeclName /
        //      plan.ConstructorName, but a defensive sweep keeps the
        //      invariant local to this function.
        //   3. Bodies passed through RewriteCallSitesInDeclaration that
        //      may already contain unqualified references inherited
        //      from upstream pipeline stages (e.g. names introduced by
        //      a previous inlining round after the most recent
        //      lambda-lifting qualification post-pass). Subsequent
        //      passes (DeclarationDeduplication, SnapshotTestFormat
        //      rendering) reject those, so qualify them here.
        var qualifiedBuilder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (declName, decl) in builder)
        {
            if (decl is SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                if (!topLevelNamesByModule.TryGetValue(declName.Namespaces, out var topLevelNames))
                    topLevelNames = [];

                qualifiedBuilder[declName] =
                    QualifySameModuleTopLevelReferencesInFunctionDecl(
                        funcDecl,
                        declName.Namespaces,
                        topLevelNames);
            }
            else
            {
                qualifiedBuilder[declName] = decl;
            }
        }

        return qualifiedBuilder.ToImmutable();
    }

    /// <summary>
    /// Indexes the input declaration dictionary by module namespace,
    /// producing the set of every top-level declaration name in each
    /// module. Used by the stripped-sibling qualification step to
    /// distinguish references to same-module top-level decls (which
    /// must be qualified for downstream rendering) from references to
    /// local bindings (which must stay unqualified).
    /// <para>
    /// Results are memoised keyed on the input dictionary's reference
    /// identity. Declaration dictionaries are immutable, so two
    /// invocations on the same instance can safely share the index.
    /// The cache uses <see cref="ConditionalWeakTable{TKey, TValue}"/>
    /// so dictionaries are not kept alive past their normal lifetime
    /// — the cache simply avoids repeating the
    /// <c>O(|declarations|)</c> traversal when
    /// <see cref="RewriteDeclarationDictionary(OptimizedElmSyntaxDeclarations)"/> is invoked multiple
    /// times against the same input (see §11.11 in
    /// <c>explore/internal-analysis/2026-05-18-eliminate-higher-order-parameters-in-focused-tests.md</c>).
    /// </para>
    /// </summary>
    private static readonly ConditionalWeakTable<
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>,
        ImmutableDictionary<ModuleName, ImmutableHashSet<string>>>
        s_topLevelNamesByModuleCache = [];

    private static ImmutableDictionary<ModuleName, ImmutableHashSet<string>>
        BuildTopLevelDeclNamesByModule(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations) =>
        s_topLevelNamesByModuleCache.GetValue(declarations, BuildTopLevelDeclNamesByModuleUncached);

    private static ImmutableDictionary<ModuleName, ImmutableHashSet<string>>
        BuildTopLevelDeclNamesByModuleUncached(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var perModule =
            new Dictionary<ModuleName, ImmutableHashSet<string>.Builder>(
                EnumerableExtensions.EqualityComparer<ModuleName>());

        foreach (var name in declarations.Keys)
        {
            if (!perModule.TryGetValue(name.Namespaces, out var builder))
            {
                builder = ImmutableHashSet.CreateBuilder<string>(StringComparer.Ordinal);
                perModule[name.Namespaces] = builder;
            }

            builder.Add(name.DeclName);
        }

        var result =
            ImmutableDictionary.CreateBuilder<ModuleName, ImmutableHashSet<string>>(
                EnumerableExtensions.EqualityComparer<ModuleName>());

        foreach (var (mod, b) in perModule)
            result[mod] = b.ToImmutable();

        return result.ToImmutable();
    }

    /// <summary>
    /// Walks <paramref name="funcDecl"/>'s body and rewrites every
    /// unqualified <see cref="SyntaxTypes.Expression.FunctionOrValue"/>
    /// reference whose name matches a top-level declaration in
    /// <paramref name="moduleNamespaces"/> (and is not shadowed by a
    /// local binding) into a fully-qualified reference. Other
    /// unqualified references — to local bindings, lambda/let
    /// parameters, case-of pattern variables, or names defined in
    /// other modules and exposed via <c>import exposing</c> — are
    /// left as-is.
    /// </summary>
    private static SyntaxTypes.Declaration.FunctionDeclaration
        QualifySameModuleTopLevelReferencesInFunctionDecl(
        SyntaxTypes.Declaration.FunctionDeclaration funcDecl,
        ModuleName moduleNamespaces,
        ImmutableHashSet<string> topLevelNamesInModule)
    {
        if (topLevelNamesInModule.IsEmpty)
            return funcDecl;

        var impl = funcDecl.Function.Declaration.Value;

        var argScope = ImmutableHashSet<string>.Empty.WithComparer(StringComparer.Ordinal);

        foreach (var argNode in impl.Arguments)
        {
            var argNames = ElmSyntaxTransformations.CollectPatternNames(argNode.Value);

            if (argNames.Count is not 0)
                argScope = argScope.Union(argNames);
        }

        var qualifiedExpression =
            ReferenceQualifier.Qualify(
                impl.Expression,
                moduleNamespaces,
                topLevelNamesInModule.Contains,
                trackLocalScope: true,
                initialLocalScope: argScope);

        if (ReferenceEquals(qualifiedExpression, impl.Expression))
            return funcDecl;

        var newImpl = impl with { Expression = qualifiedExpression };

        var newFunc =
            funcDecl.Function with
            {
                Declaration =
                new Node<SyntaxTypes.FunctionImplementation>(
                    funcDecl.Function.Declaration.Range,
                    newImpl),
            };

        return new SyntaxTypes.Declaration.FunctionDeclaration(newFunc);
    }

    /// <summary>
    /// Constructs a forwarding declaration that delegates to its
    /// stripped sibling and re-wraps the result with the original
    /// constructor: <c>f a1 a2 … = Wrap (f__stripped a1 a2 …)</c>.
    /// Argument patterns must be <see cref="SyntaxTypes.Pattern.VarPattern"/>
    /// or <see cref="SyntaxTypes.Pattern.UnitPattern"/> —
    /// <see cref="BuildStripPlans(OptimizedElmSyntaxDeclarations, ImmutableDictionary{DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo})"/> enforces this invariant. For a
    /// var-pattern position the forwarding call passes the bound
    /// name; for a unit-pattern position it passes the literal
    /// <c>()</c> via <see cref="SyntaxTypes.Expression.UnitExpr"/>.
    /// </summary>
    private static SyntaxTypes.Declaration.FunctionDeclaration BuildForwardingDecl(
        SyntaxTypes.Declaration.FunctionDeclaration original,
        WrapperStripPlan plan)
    {
        var origImpl = original.Function.Declaration.Value;

        // Build the inner call: f__stripped a1 a2 …
        var strippedAppParts =
            new List<Node<SyntaxTypes.Expression>>(origImpl.Arguments.Count + 1)
            {
                new(
                    s_zeroRange,
                    new SyntaxTypes.Expression.FunctionOrValue(
                        plan.StrippedDeclName.Namespaces,
                        plan.StrippedDeclName.DeclName)),
            };

        foreach (var argPatternNode in origImpl.Arguments)
        {
            switch (argPatternNode.Value)
            {
                case SyntaxTypes.Pattern.VarPattern varPat:
                    strippedAppParts.Add(
                        new Node<SyntaxTypes.Expression>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.FunctionOrValue([], varPat.Name)));

                    break;

                case SyntaxTypes.Pattern.UnitPattern:
                    strippedAppParts.Add(
                        new Node<SyntaxTypes.Expression>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.UnitExpr()));

                    break;

                default:
                    throw new InvalidOperationException(
                        "BuildForwardingDecl invariant violated: non-var/non-unit pattern in "
                        + plan.OriginalDeclName.FullName
                        + "; BuildStripPlans should have filtered this out.");
            }
        }

        var strippedApp = new SyntaxTypes.Expression.Application(strippedAppParts);

        // Wrap in the constructor: Wrap (f__stripped a1 …)
        var ctorRef =
            new SyntaxTypes.Expression.FunctionOrValue(
                plan.ConstructorName.Namespaces,
                plan.ConstructorName.DeclName);

        var forwardingBody =
            new SyntaxTypes.Expression.Application(
                [
                new Node<SyntaxTypes.Expression>(s_zeroRange, ctorRef),
                new Node<SyntaxTypes.Expression>(s_zeroRange, strippedApp),
                ]);

        var newImpl =
            origImpl with
            {
                Expression = new Node<SyntaxTypes.Expression>(origImpl.Expression.Range, forwardingBody),
            };

        var newFunc =
            original.Function with
            {
                Declaration =
                new Node<SyntaxTypes.FunctionImplementation>(
                    original.Function.Declaration.Range,
                    newImpl),
            };

        return new SyntaxTypes.Declaration.FunctionDeclaration(newFunc);
    }

    private static SyntaxTypes.Declaration RewriteCallSitesInDeclaration(
        SyntaxTypes.Declaration decl,
        ImmutableDictionary<DeclQualifiedName, WrapperStripPlan> stripPlans,
        ModuleName currentModuleName)
    {
        switch (decl)
        {
            case SyntaxTypes.Declaration.FunctionDeclaration funcDecl:
                {
                    var impl = funcDecl.Function.Declaration.Value;

                    var rewrittenBody =
                        RewriteCallSitesInExpression(impl.Expression.Value, stripPlans, currentModuleName);

                    if (ReferenceEquals(rewrittenBody, impl.Expression.Value))
                        return decl;

                    var newImpl =
                        impl with
                        {
                            Expression = new Node<SyntaxTypes.Expression>(impl.Expression.Range, rewrittenBody),
                        };

                    var newFunc =
                        funcDecl.Function with
                        {
                            Declaration =
                            new Node<SyntaxTypes.FunctionImplementation>(
                                funcDecl.Function.Declaration.Range,
                                newImpl),
                        };

                    return new SyntaxTypes.Declaration.FunctionDeclaration(newFunc);
                }

            case SyntaxTypes.Declaration.CustomTypeDeclaration:
            case SyntaxTypes.Declaration.AliasDeclaration:
            case SyntaxTypes.Declaration.PortDeclaration:
            case SyntaxTypes.Declaration.InfixDeclaration:
                return decl;

            default:
                throw new NotImplementedException(
                    "RewriteCallSitesInDeclaration does not handle declaration variant: " +
                    decl.GetType().Name);
        }
    }
}
