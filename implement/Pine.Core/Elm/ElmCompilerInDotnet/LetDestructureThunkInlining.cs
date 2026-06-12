using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Implementation of <em>gap (1)</em> from §F.4 of
/// <c>explore/internal-analysis/2026-05-19-optimize-elm-syntax-to-monomorphize-and-eliminate-higher-order-parameters.md</c>:
/// <em>Inline zero-arity wrapper-bodied thunk decls into matching
/// <c>let (Wrap p) = &lt;thunk-ref&gt; in body</c> RHSs.</em>
///
/// <para>
/// Shape recognised, per let-destructure binding:
/// </para>
/// <code>
/// let
///     (Wrap p) = <i>QualifiedRef</i>
/// in
///     body
/// </code>
/// where <c>Wrap</c> is a registered newtype-shaped constructor (1-arg,
/// single ctor) and <c>QualifiedRef</c> resolves to a zero-parameter
/// function declaration whose body matches one of:
/// <list type="bullet">
///   <item><description><c>Wrap inner</c> — a literal wrap call</description></item>
///   <item><description><c>let ... in Wrap inner</c> — a let-block whose tail is a literal wrap</description></item>
///   <item><description>another <c>QualifiedRef</c> to a zero-arity decl whose body matches one of the above (transitive)</description></item>
/// </list>
///
/// <para>
/// When matched, the destructuring RHS is rewritten from the bare
/// <c>FunctionOrValue</c> reference to the inlined thunk body. The
/// downstream <see cref="WrapUnwrapCancellation"/> pass then handles the
/// resulting <c>let (Wrap p) = Wrap inner in body</c> shape (Shape A) and
/// the wrap-through-let shape (Step 0 of <c>TryGetWrappedInnerExpression</c>).
/// </para>
///
/// <para>
/// Design points:
/// </para>
/// <list type="bullet">
///   <item><description>
///     Only fires when the destructuring pattern is itself a registered
///     newtype constructor — this keeps the inlining narrowly targeted at
///     newtype-thunk shapes and avoids unintended growth in code size
///     from inlining arbitrary zero-arity decls into arbitrary positions.
///   </description></item>
///   <item><description>
///     Only fires when the thunk decl's body is itself a wrap call (or
///     reachable via let/thunk-chain to a wrap call) — the narrowing
///     trigger ensures the inlined body will immediately be cancelled by
///     the downstream <see cref="WrapUnwrapCancellation"/> pass, so the
///     pair of passes is a net <em>reduction</em>, never just a code
///     duplication.
///   </description></item>
///   <item><description>
///     Pure peephole rewrite: bottom-up structural recursion over each
///     declaration body, no global flow analysis. Idempotent — a second
///     invocation against an already-rewritten dictionary returns it
///     unchanged.
///   </description></item>
///   <item><description>
///     Off by default. Wired conditionally through the
///     <c>inlineLetDestructureThunks</c> knob on
///     <see cref="ElmCompiler.CompileInteractiveEnvironment{LoweredT}(Pine.Core.Files.FileTree, IReadOnlyList{ModuleName}, System.Func{ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration}, ImmutableHashSet{DeclQualifiedName}, Pine.Core.Result{string, LoweredT}}, System.Func{LoweredT, ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration}}, bool, IReadOnlyList{DeclQualifiedName}?, bool)"/>
///     to limit blast radius until validated against the full
///     <c>Pine.Core.Tests</c> suite. See §H of the same analysis doc.
///   </description></item>
/// </list>
/// </summary>
internal static class LetDestructureThunkInlining
{
    /// <summary>
    /// Top-level orchestrator: walks every declaration and rewrites every
    /// matching <c>let (Wrap p) = &lt;thunk-ref&gt; in body</c> destructuring
    /// by inlining the thunk body. Returns the input unchanged when no
    /// newtype constructors are registered in the input (nothing to match
    /// against).
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        RewriteDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var registry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(declarations);

        if (registry.IsEmpty)
            return declarations;

        // Index zero-parameter function declarations by their qualified
        // name. We look up RHS targets against this index to decide
        // whether a `let (Wrap p) = ref` shape is inlinable.
        var zeroArityFunctionBodies =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, Node<SyntaxTypes.Expression>>();

        foreach (var (declName, decl) in declarations)
        {
            if (decl is SyntaxTypes.Declaration.FunctionDeclaration funcDecl &&
                funcDecl.Function.Declaration.Value.Arguments.Count is 0)
            {
                zeroArityFunctionBodies[declName] = funcDecl.Function.Declaration.Value.Expression;
            }
        }

        var thunkBodies = zeroArityFunctionBodies.ToImmutable();

        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (declName, decl) in declarations)
        {
            builder[declName] =
                RewriteDeclaration(decl, registry, thunkBodies, declName.Namespaces);
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// <see cref="OptimizedElmSyntaxDeclarations"/>-flavoured overload of
    /// <see cref="RewriteDeclarationDictionary(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration})"/>.
    /// </summary>
    public static OptimizedElmSyntaxDeclarations
        RewriteDeclarationDictionary(OptimizedElmSyntaxDeclarations declarations) =>
        OptimizedElmSyntaxDeclarations.FromFlatDictionary(
            RewriteDeclarationDictionary(declarations.RenderAsFlatDictionary()));

    private static SyntaxTypes.Declaration RewriteDeclaration(
        SyntaxTypes.Declaration decl,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ImmutableDictionary<DeclQualifiedName, Node<SyntaxTypes.Expression>> thunkBodies,
        ModuleName currentModuleName)
    {
        switch (decl)
        {
            case SyntaxTypes.Declaration.FunctionDeclaration funcDecl:
                {
                    var impl = funcDecl.Function.Declaration.Value;

                    var newBody =
                        RewriteExpression(impl.Expression.Value, registry, thunkBodies, currentModuleName);

                    if (ReferenceEquals(newBody, impl.Expression.Value))
                        return decl;

                    var newImpl =
                        impl with
                        {
                            Expression = new Node<SyntaxTypes.Expression>(impl.Expression.Range, newBody),
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

                // Defensive: unknown declaration shapes pass through unchanged.
                return decl;
        }
    }

    /// <summary>
    /// Bottom-up rewrite over an expression tree.
    /// </summary>
    private static SyntaxTypes.Expression RewriteExpression(
        SyntaxTypes.Expression expr,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ImmutableDictionary<DeclQualifiedName, Node<SyntaxTypes.Expression>> thunkBodies,
        ModuleName currentModuleName)
    {
        Node<SyntaxTypes.Expression> RecurseNode(Node<SyntaxTypes.Expression> child)
        {
            var rewritten =
                RewriteExpression(child.Value, registry, thunkBodies, currentModuleName);

            if (ReferenceEquals(rewritten, child.Value))
                return child;

            return new Node<SyntaxTypes.Expression>(child.Range, rewritten);
        }

        var withChildrenRewritten = ElmSyntaxTransformations.MapChildExpressions(expr, RecurseNode);

        if (withChildrenRewritten is SyntaxTypes.Expression.LetExpression letExpr)
        {
            var rewrittenLet =
                TryInlineThunksInLetBlock(letExpr, registry, thunkBodies, currentModuleName);

            if (rewrittenLet is not null)
                return rewrittenLet;
        }

        return withChildrenRewritten;
    }

    /// <summary>
    /// Walks the declarations of a <c>let</c> block and rewrites any
    /// <c>let (Wrap p) = &lt;thunk-ref&gt;</c> destructuring whose RHS is a
    /// reference to a zero-arity thunk decl that recursively bottoms out
    /// in a literal <c>Wrap inner</c> call. Returns the rewritten let
    /// expression, or <c>null</c> if no destructuring was rewritten.
    /// </summary>
    private static SyntaxTypes.Expression? TryInlineThunksInLetBlock(
        SyntaxTypes.Expression.LetExpression letExpr,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ImmutableDictionary<DeclQualifiedName, Node<SyntaxTypes.Expression>> thunkBodies,
        ModuleName currentModuleName)
    {
        var letBlock = letExpr.Value;

        List<Node<SyntaxTypes.Expression.LetDeclaration>>? rewrittenDecls = null;

        for (var declIndex = 0; declIndex < letBlock.Declarations.Count; declIndex++)
        {
            var declNode = letBlock.Declarations[declIndex];

            if (declNode.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr &&
                TryInlineThunkInLetDestructuring(letDestr, registry, thunkBodies, currentModuleName)
                    is { } rewrittenDestr)
            {
                rewrittenDecls ??= [.. letBlock.Declarations];
                rewrittenDecls[declIndex] = declNode with { Value = rewrittenDestr };
            }
        }

        if (rewrittenDecls is null)
            return null;

        return
            new SyntaxTypes.Expression.LetExpression(
                new SyntaxTypes.Expression.LetBlock(
                    Declarations: rewrittenDecls,
                    Expression: letBlock.Expression));
    }

    /// <summary>
    /// Rewrites a single <c>let (Wrap p) = ref</c> destructuring by
    /// substituting <c>ref</c> with the inlined body of the zero-arity
    /// thunk it refers to, when both the pattern (a registered newtype
    /// constructor) and the resolved decl body (transitively reaches a
    /// literal <c>Wrap inner</c>) match the gap-(1) shape.
    /// </summary>
    private static SyntaxTypes.Expression.LetDeclaration.LetDestructuring? TryInlineThunkInLetDestructuring(
        SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ImmutableDictionary<DeclQualifiedName, Node<SyntaxTypes.Expression>> thunkBodies,
        ModuleName currentModuleName)
    {
        // LHS pattern: must be a NamedPattern (Wrap inner) for a registered newtype.
        var pattern = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(letDestr.Pattern.Value);

        if (pattern is not SyntaxTypes.Pattern.NamedPattern namedPat)
            return null;

        if (namedPat.Arguments.Count is not 1)
            return null;

        var ctorQName = ElmSyntaxTransformations.ResolveReference(namedPat.Name, currentModuleName);

        if (!registry.TryGetValue(ctorQName, out var ctorInfo) || !ctorInfo.ConstructorName.Equals(ctorQName))
            return null;

        // RHS: must be a bare FunctionOrValue (possibly parenthesised) referencing a
        // zero-arity thunk that, transitively, returns Wrap inner.
        var rhsPeeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(letDestr.Expression.Value);

        if (rhsPeeled is not SyntaxTypes.Expression.FunctionOrValue fov)
            return null;

        var refQName = ElmSyntaxTransformations.ResolveReference(fov, currentModuleName);

        var inlinedBody =
            TryResolveThunkBodyToWrap(refQName, ctorQName, registry, thunkBodies, []);

        if (inlinedBody is null)
            return null;

        return letDestr with { Expression = inlinedBody };
    }

    /// <summary>
    /// Recursively follows a chain of zero-arity thunk references until it
    /// reaches a body whose shape (possibly inside one or more enclosing
    /// <c>let</c> blocks) is a literal <c>Wrap inner</c> for the same
    /// <paramref name="expectedCtor"/>. Returns the resolved body
    /// expression on success, <c>null</c> otherwise.
    /// </summary>
    private static Node<SyntaxTypes.Expression>? TryResolveThunkBodyToWrap(
        DeclQualifiedName refQName,
        DeclQualifiedName expectedCtor,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ImmutableDictionary<DeclQualifiedName, Node<SyntaxTypes.Expression>> thunkBodies,
        ImmutableHashSet<DeclQualifiedName> visited)
    {
        if (visited.Contains(refQName))
            return null;

        if (!thunkBodies.TryGetValue(refQName, out var body))
            return null;

        var bodyValue = body.Value;
        var bodyModule = refQName.Namespaces;

        // Peel any number of enclosing let blocks while checking the tail
        // expression for the wrap shape.
        if (BodyReachesWrapCtor(body, expectedCtor, registry, thunkBodies, visited.Add(refQName), bodyModule))
            return body;

        // Transitive thunk-of-thunk: if the body is itself a bare reference
        // to another zero-arity decl, follow it.
        var bodyPeeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(bodyValue);

        if (bodyPeeled is SyntaxTypes.Expression.FunctionOrValue chainFov)
        {
            var chainQName = ElmSyntaxTransformations.ResolveReference(chainFov, bodyModule);

            return TryResolveThunkBodyToWrap(chainQName, expectedCtor, registry, thunkBodies, visited.Add(refQName));
        }

        return null;
    }

    /// <summary>
    /// True iff the supplied expression — possibly wrapped in one or more
    /// <c>let</c> blocks — eventually reaches a <c>Wrap inner</c>
    /// constructor application for <paramref name="expectedCtor"/>.
    /// Mirrors <see cref="WrapUnwrapCancellation"/>'s
    /// <c>TryGetWrappedInnerExpression</c> Step 0 + Step 1 in a
    /// boolean-only form: we don't need the inner expression here, only
    /// confirmation that the body will be cancellable after inlining.
    /// </summary>
    private static bool BodyReachesWrapCtor(
        Node<SyntaxTypes.Expression> bodyNode,
        DeclQualifiedName expectedCtor,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ImmutableDictionary<DeclQualifiedName, Node<SyntaxTypes.Expression>> thunkBodies,
        ImmutableHashSet<DeclQualifiedName> visited,
        ModuleName currentModuleName)
    {
        var peeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(bodyNode.Value);

        if (peeled is SyntaxTypes.Expression.LetExpression letInner)
        {
            return
                BodyReachesWrapCtor(
                    letInner.Value.Expression,
                    expectedCtor,
                    registry,
                    thunkBodies,
                    visited,
                    currentModuleName);
        }

        var ctorApp = ElmSyntaxTransformations.TryDeconstructConstructorApplication(bodyNode);

        if (ctorApp is not null && ctorApp.FieldExpressions.Count is 1)
        {
            var rhsCtorQName =
                ElmSyntaxTransformations.ResolveReference(ctorApp.ConstructorName, currentModuleName);

            if (rhsCtorQName.Equals(expectedCtor))
                return true;
        }

        // Allow chaining through another zero-arity thunk reference at the tail.
        if (peeled is SyntaxTypes.Expression.FunctionOrValue chainFov)
        {
            var chainQName = ElmSyntaxTransformations.ResolveReference(chainFov, currentModuleName);

            if (!visited.Contains(chainQName) && thunkBodies.TryGetValue(chainQName, out var chainBody))
            {
                return
                    BodyReachesWrapCtor(
                        chainBody,
                        expectedCtor,
                        registry,
                        thunkBodies,
                        visited.Add(chainQName),
                        chainQName.Namespaces);
            }
        }

        return false;
    }
}
