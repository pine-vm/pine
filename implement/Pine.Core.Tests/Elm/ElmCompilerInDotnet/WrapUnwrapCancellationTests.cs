using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Generic;
using System.Collections.Immutable;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using SyntaxModel = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Unit tests for <see cref="WrapUnwrapCancellation"/> at the Stil4m
/// AST level. Mirrors the convention of
/// <see cref="WrapperReturnStrippingTests"/> and
/// <see cref="NewtypeWrapperAnalysisTests"/>: synthesises minimal
/// Stil4m syntax trees, runs the rewrite in isolation, and asserts on
/// the expression shapes produced. No compilation pipeline involved.
///
/// <para>Coverage matrix (see
/// <c>explore/internal-analysis/2026-05-15-s1-cancel-parser-newtype-wrap-unwrap.md</c>
/// section 3 for the shape catalogue):</para>
/// <list type="bullet">
///   <item><description>Shape A — let-destructure of a literal wrap</description></item>
///   <item><description>Shape A' — let-destructure of a sibling-decl call (sibling-aware)</description></item>
///   <item><description>Shape B — case-of single arm on a literal wrap</description></item>
///   <item><description>Shape B' — case-of single arm on a sibling-decl call</description></item>
///   <item><description>Negative cases: mismatched ctor name, unrelated ctor, multi-arm case, no destructure</description></item>
///   <item><description>Idempotence: a second invocation is a no-op</description></item>
/// </list>
/// </summary>
public class WrapUnwrapCancellationTests
{
    private static readonly SyntaxModel.Location s_zeroLoc = new(Row: 0, Column: 0);

    private static readonly SyntaxModel.Range s_zeroRange = new(Start: s_zeroLoc, End: s_zeroLoc);

    private static SyntaxModel.Node<T> N<T>(T value) => new(s_zeroRange, value);

    private static readonly IReadOnlyList<string> s_module = ["Lib"];

    private static SyntaxTypes.TypeAnnotation IntType() =>
        new SyntaxTypes.TypeAnnotation.Typed(
            N(((IReadOnlyList<string>)["Basics"], "Int")),
            []);

    /// <summary>
    /// Newtype-shaped: <c>type Wrap = Wrap Int</c> (single ctor, single arg).
    /// </summary>
    private static SyntaxTypes.Declaration.CustomTypeDeclaration NewtypeDecl(
        string typeName,
        string ctorName)
    {
        var ctor =
            new SyntaxTypes.ValueConstructor(
                Name: N(ctorName),
                Arguments: [N(IntType())]);

        var ts =
            new SyntaxTypes.TypeStruct(
                Documentation: null,
                Name: N(typeName),
                Generics: [],
                Constructors: [N(ctor)]);

        return new SyntaxTypes.Declaration.CustomTypeDeclaration(ts);
    }

    private static SyntaxTypes.Expression FunctionOrValue(IReadOnlyList<string> moduleName, string name) =>
        new SyntaxTypes.Expression.FunctionOrValue(moduleName, name);

    private static SyntaxTypes.Expression.Application App(params SyntaxTypes.Expression[] parts)
    {
        var nodes = new List<SyntaxModel.Node<SyntaxTypes.Expression>>(parts.Length);

        foreach (var p in parts)
            nodes.Add(N(p));

        return new SyntaxTypes.Expression.Application(nodes);
    }

    private static SyntaxTypes.Declaration.FunctionDeclaration FuncDecl(
        string name,
        IReadOnlyList<string> argNames,
        SyntaxTypes.Expression body)
    {
        var argPatterns = new List<SyntaxModel.Node<SyntaxTypes.Pattern>>(argNames.Count);

        foreach (var argName in argNames)
            argPatterns.Add(N<SyntaxTypes.Pattern>(new SyntaxTypes.Pattern.VarPattern(argName)));

        var impl =
            new SyntaxTypes.FunctionImplementation(
                Name: N(name),
                Arguments: argPatterns,
                Expression: N(body));

        var func =
            new SyntaxTypes.FunctionStruct(
                Documentation: null,
                Signature: null,
                Declaration: N(impl));

        return new SyntaxTypes.Declaration.FunctionDeclaration(func);
    }

    private static SyntaxTypes.Pattern.NamedPattern NamedPat(
        IReadOnlyList<string> moduleName,
        string ctorName,
        params SyntaxTypes.Pattern[] argPatterns)
    {
        var nodes = new List<SyntaxModel.Node<SyntaxTypes.Pattern>>(argPatterns.Length);

        foreach (var p in argPatterns)
            nodes.Add(N(p));

        return
            new SyntaxTypes.Pattern.NamedPattern(
                new SyntaxTypes.QualifiedNameRef(moduleName, ctorName),
                nodes);
    }

    private static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> Decls(
        params (string name, SyntaxTypes.Declaration decl)[] items)
    {
        var b = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (n, d) in items)
            b[DeclQualifiedName.Create(s_module, n)] = d;

        return b.ToImmutable();
    }

    /// <summary>Extracts <c>caller</c>'s body from a rewritten dictionary.</summary>
    private static SyntaxTypes.Expression GetCallerBody(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> rewritten,
        string callerName = "caller")
    {
        var decl =
            (SyntaxTypes.Declaration.FunctionDeclaration)
            rewritten[DeclQualifiedName.Create(s_module, callerName)];

        return decl.Function.Declaration.Value.Expression.Value;
    }

    // ------------------------------------------------------------------
    // Shape A — let-destructure of a literal wrap.
    // ------------------------------------------------------------------

    [Fact]
    public void Shape_A_let_destructure_of_literal_wrap_substitutes_when_body_references_p_once()
    {
        // caller = let (Wrap p) = Wrap 42 in p
        // Body references `p` exactly once → after cancellation the
        // let-binding is elided entirely and the body becomes just `42`
        // (the inner expression of the wrap, substituted for `p`).
        var letBlock =
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                N<SyntaxTypes.Expression.LetDeclaration>(
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Wrap", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression:
                        N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "Wrap"), new SyntaxTypes.Expression.Integer(42)))))
                ],
                Expression: N(FunctionOrValue([], "p")));

        var caller = FuncDecl("caller", [], new SyntaxTypes.Expression.LetExpression(letBlock));

        var decls =
            Decls(
                ("Wrap", NewtypeDecl("Wrap", "Wrap")),
                ("caller", caller));

        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls);

        // Body collapses to just the integer literal `42`.
        GetCallerBody(rewritten)
            .Should().BeOfType<SyntaxTypes.Expression.Integer>()
            .Which.Value.Should().Be(42);
    }

    [Fact]
    public void Shape_A_emits_let_binding_when_p_is_referenced_multiple_times()
    {
        // caller = let (Wrap p) = Wrap 42 in [ p, p ]
        // Body references `p` twice → substituting would duplicate
        // (here the RHS is a constructor application; even so the
        // duplication-safety guard is conservative). Expect a
        // let-binding `let p = 42 in [ p, p ]`.
        var letBlock =
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                N<SyntaxTypes.Expression.LetDeclaration>(
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Wrap", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression:
                        N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "Wrap"), new SyntaxTypes.Expression.Integer(42)))))
                ],
                Expression:
                N<SyntaxTypes.Expression>(
                    new SyntaxTypes.Expression.ListExpr(
                        [
                        N(FunctionOrValue([], "p")),
                        N(FunctionOrValue([], "p")),
                        ])));

        var caller = FuncDecl("caller", [], new SyntaxTypes.Expression.LetExpression(letBlock));

        var decls =
            Decls(
                ("Wrap", NewtypeDecl("Wrap", "Wrap")),
                ("caller", caller));

        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls);

        var letExpr = GetCallerBody(rewritten).Should().BeOfType<SyntaxTypes.Expression.LetExpression>().Subject;
        letExpr.Value.Declarations.Should().HaveCount(1);

        var newDestr =
            letExpr.Value.Declarations[0].Value
            .Should().BeOfType<SyntaxTypes.Expression.LetDeclaration.LetDestructuring>().Subject;

        newDestr.Pattern.Value.Should().BeOfType<SyntaxTypes.Pattern.VarPattern>()
            .Which.Name.Should().Be("p");

        newDestr.Expression.Value.Should().BeOfType<SyntaxTypes.Expression.Integer>()
            .Which.Value.Should().Be(42);
    }

    [Fact]
    public void Shape_A_preserves_other_destructurings_in_the_same_let_block()
    {
        // caller = let (Wrap p) = Wrap 1
        //              q        = 2
        //          in p
        // The first destructuring cancels to `p = 1`, then the
        // post-cancellation substitution pass eliminates `p` by inlining
        // the integer literal into the body (refCount of `p` in body is
        // 1, refCount in `q`'s RHS is 0 — safe to substitute). The
        // var-binding `q = 2` is NOT cancelled by the wrap/unwrap pass
        // and therefore stays unchanged (the substitution pass only
        // considers bindings produced by cancellation).
        var letBlock =
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                N<SyntaxTypes.Expression.LetDeclaration>(
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Wrap", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression:
                        N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "Wrap"), new SyntaxTypes.Expression.Integer(1))))),

                N<SyntaxTypes.Expression.LetDeclaration>(
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Pattern: N<SyntaxTypes.Pattern>(new SyntaxTypes.Pattern.VarPattern("q")),
                        Expression: N<SyntaxTypes.Expression>(new SyntaxTypes.Expression.Integer(2)))),
                ],
                Expression: N(FunctionOrValue([], "p")));

        var caller = FuncDecl("caller", [], new SyntaxTypes.Expression.LetExpression(letBlock));

        var decls =
            Decls(
                ("Wrap", NewtypeDecl("Wrap", "Wrap")),
                ("caller", caller));

        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls);

        var letExpr = GetCallerBody(rewritten).Should().BeOfType<SyntaxTypes.Expression.LetExpression>().Subject;
        letExpr.Value.Declarations.Should().HaveCount(1);

        // Only the untouched `q = 2` binding remains; the cancelled
        // `p = 1` was substituted away.
        var only =
            letExpr.Value.Declarations[0].Value
            .Should().BeOfType<SyntaxTypes.Expression.LetDeclaration.LetDestructuring>().Subject;

        only.Pattern.Value.Should().BeOfType<SyntaxTypes.Pattern.VarPattern>()
            .Which.Name.Should().Be("q");

        only.Expression.Value.Should().BeOfType<SyntaxTypes.Expression.Integer>()
            .Which.Value.Should().Be(2);

        // Body is `1` after substituting `p` → `1`.
        letExpr.Value.Expression.Value.Should().BeOfType<SyntaxTypes.Expression.Integer>()
            .Which.Value.Should().Be(1);
    }

    // ------------------------------------------------------------------
    // Shape A' — let-destructure of a sibling-decl call.
    // ------------------------------------------------------------------

    [Fact]
    public void Shape_A_prime_let_destructure_of_sibling_call_redirects_to_stripped_sibling()
    {
        // type Wrap = Wrap Int
        // f x = Wrap x                 -- gets a strip plan because the body is `Wrap x`
        // f__stripped x = x            -- pre-emitted stripped sibling (gating requirement)
        // caller = let (Wrap p) = f 7 in p
        // After cancellation: caller = let p = f__stripped 7 in p
        var fDecl =
            FuncDecl(
                "f",
                ["x"],
                App(FunctionOrValue(s_module, "Wrap"), FunctionOrValue([], "x")));

        // Pre-emitted stripped sibling — required for the sibling-aware
        // rewrite to fire (it gates on the sibling being present in the
        // input dictionary, so the produced reference always resolves).
        var fStrippedDecl =
            FuncDecl(
                "f__stripped",
                ["x"],
                FunctionOrValue([], "x"));

        var letBlock =
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                N<SyntaxTypes.Expression.LetDeclaration>(
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Wrap", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression:
                        N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "f"), new SyntaxTypes.Expression.Integer(7)))))
                ],
                Expression: N(FunctionOrValue([], "p")));

        var caller = FuncDecl("caller", [], new SyntaxTypes.Expression.LetExpression(letBlock));

        var decls =
            Decls(
                ("Wrap", NewtypeDecl("Wrap", "Wrap")),
                ("f", fDecl),
                ("f__stripped", fStrippedDecl),
                ("caller", caller));

        // Drive Shape A' explicitly via the sibling-aware overload.
        // The sibling registry tells the pass that calls to `f` can be
        // rewritten to call `f__stripped` directly, dropping the wrap.
        var siblings =
            ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl>.Empty.Add(
                DeclQualifiedName.Create(s_module, "f"),
                new GeneratedSiblingDecl(
                    OriginalDeclName: DeclQualifiedName.Create(s_module, "f"),
                    SiblingDeclName: DeclQualifiedName.Create(s_module, "f__stripped"),
                    OriginalArity: 1,
                    ParameterOrigins:
                    [
                    new SiblingParameterOrigin.Identity(SiblingIndex: 0, OriginalIndex: 0),
                    ],
                    ResultTransform: new SiblingResultTransform.WrapWithConstructor(
                        DeclQualifiedName.Create(s_module, "Wrap"))));

        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls, siblings);

        // Body collapses to `f__stripped 7` directly (no surrounding let
        // because `p` is referenced exactly once in the body).
        var newApp = GetCallerBody(rewritten).Should().BeOfType<SyntaxTypes.Expression.Application>().Subject;
        newApp.Arguments.Should().HaveCount(2);

        var head = newApp.Arguments[0].Value.Should().BeOfType<SyntaxTypes.Expression.FunctionOrValue>().Subject;
        head.Name.Should().Be("f__stripped");
        head.ModuleName.Should().Equal(s_module);

        newApp.Arguments[1].Value.Should().BeOfType<SyntaxTypes.Expression.Integer>()
            .Which.Value.Should().Be(7);
    }

    [Fact]
    public void Shape_A_prime_does_not_fire_when_no_sibling_registry_provided()
    {
        // Same setup as Shape_A_prime_let_destructure_of_sibling_call_redirects_to_stripped_sibling
        // but invoking the no-sibling-registry overload. The literal-only
        // path must NOT rewrite a destructure whose RHS is a function
        // call (only literal `Wrap inner` RHSes match Shape A).
        var fDecl =
            FuncDecl(
                "f",
                ["x"],
                App(FunctionOrValue(s_module, "Wrap"), FunctionOrValue([], "x")));

        var letBlock =
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                N<SyntaxTypes.Expression.LetDeclaration>(
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Wrap", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression:
                        N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "f"), new SyntaxTypes.Expression.Integer(7)))))
                ],
                Expression: N(FunctionOrValue([], "p")));

        var caller = FuncDecl("caller", [], new SyntaxTypes.Expression.LetExpression(letBlock));

        var decls =
            Decls(
                ("Wrap", NewtypeDecl("Wrap", "Wrap")),
                ("f", fDecl),
                ("caller", caller));

        // No sibling registry — invokes the literal-only overload.
        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls);

        var letExpr = GetCallerBody(rewritten).Should().BeOfType<SyntaxTypes.Expression.LetExpression>().Subject;

        var destr =
            letExpr.Value.Declarations[0].Value
            .Should().BeOfType<SyntaxTypes.Expression.LetDeclaration.LetDestructuring>().Subject;

        // Pattern is still NamedPattern("Wrap", ...) — no rewrite.
        destr.Pattern.Value.Should().BeOfType<SyntaxTypes.Pattern.NamedPattern>()
            .Which.Name.Name.Should().Be("Wrap");
    }

    // ------------------------------------------------------------------
    // Shape B — case-of single arm on a literal wrap.
    // ------------------------------------------------------------------

    [Fact]
    public void Shape_B_case_of_single_arm_on_literal_wrap_substitutes_to_inner_value()
    {
        // caller = case Wrap 99 of Wrap p -> p
        // → caller = 99   (substitute, since `p` is referenced once)
        var caseExpr =
            new SyntaxTypes.Expression.CaseExpression(
                new SyntaxTypes.CaseBlock(
                    Expression:
                    N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "Wrap"), new SyntaxTypes.Expression.Integer(99))),
                    Cases:
                    [
                    new SyntaxTypes.Case(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Wrap", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression: N(FunctionOrValue([], "p")))
                    ]));

        var caller = FuncDecl("caller", [], caseExpr);

        var decls =
            Decls(
                ("Wrap", NewtypeDecl("Wrap", "Wrap")),
                ("caller", caller));

        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls);

        // Case-of fully collapses to just `99`.
        GetCallerBody(rewritten)
            .Should().BeOfType<SyntaxTypes.Expression.Integer>()
            .Which.Value.Should().Be(99);
    }

    [Fact]
    public void Shape_B_emits_let_binding_when_branch_body_references_p_multiple_times()
    {
        // caller = case Wrap 99 of Wrap p -> [ p, p ]
        // Branch body references `p` twice → fall back to let-binding.
        var caseExpr =
            new SyntaxTypes.Expression.CaseExpression(
                new SyntaxTypes.CaseBlock(
                    Expression:
                    N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "Wrap"), new SyntaxTypes.Expression.Integer(99))),
                    Cases:
                    [
                    new SyntaxTypes.Case(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Wrap", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression:
                        N<SyntaxTypes.Expression>(
                            new SyntaxTypes.Expression.ListExpr(
                                [
                                N(FunctionOrValue([], "p")),
                                N(FunctionOrValue([], "p")),
                                ])))
                    ]));

        var caller = FuncDecl("caller", [], caseExpr);

        var decls =
            Decls(
                ("Wrap", NewtypeDecl("Wrap", "Wrap")),
                ("caller", caller));

        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls);

        var letExpr = GetCallerBody(rewritten).Should().BeOfType<SyntaxTypes.Expression.LetExpression>().Subject;

        var destr =
            letExpr.Value.Declarations[0].Value
            .Should().BeOfType<SyntaxTypes.Expression.LetDeclaration.LetDestructuring>().Subject;

        destr.Pattern.Value.Should().BeOfType<SyntaxTypes.Pattern.VarPattern>()
            .Which.Name.Should().Be("p");

        destr.Expression.Value.Should().BeOfType<SyntaxTypes.Expression.Integer>()
            .Which.Value.Should().Be(99);

        // Body of the let is the original `[ p, p ]` list.
        letExpr.Value.Expression.Value.Should().BeOfType<SyntaxTypes.Expression.ListExpr>()
            .Which.Elements.Should().HaveCount(2);
    }

    // ------------------------------------------------------------------
    // Shape B' — case-of single arm on a sibling-decl call.
    // ------------------------------------------------------------------

    [Fact]
    public void Shape_B_prime_case_of_single_arm_on_sibling_call_redirects_to_stripped_sibling()
    {
        // type Wrap = Wrap Int
        // f x = Wrap x
        // f__stripped x = x            -- pre-emitted stripped sibling (gating requirement)
        // caller = case f 5 of Wrap p -> p
        // → caller = let p = f__stripped 5 in p
        var fDecl =
            FuncDecl(
                "f",
                ["x"],
                App(FunctionOrValue(s_module, "Wrap"), FunctionOrValue([], "x")));

        var fStrippedDecl =
            FuncDecl(
                "f__stripped",
                ["x"],
                FunctionOrValue([], "x"));

        var caseExpr =
            new SyntaxTypes.Expression.CaseExpression(
                new SyntaxTypes.CaseBlock(
                    Expression:
                    N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "f"), new SyntaxTypes.Expression.Integer(5))),
                    Cases:
                    [
                    new SyntaxTypes.Case(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Wrap", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression: N(FunctionOrValue([], "p")))
                    ]));

        var caller = FuncDecl("caller", [], caseExpr);

        var decls =
            Decls(
                ("Wrap", NewtypeDecl("Wrap", "Wrap")),
                ("f", fDecl),
                ("f__stripped", fStrippedDecl),
                ("caller", caller));

        // Drive Shape B' explicitly via the sibling-aware overload.
        var siblings =
            ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl>.Empty.Add(
                DeclQualifiedName.Create(s_module, "f"),
                new GeneratedSiblingDecl(
                    OriginalDeclName: DeclQualifiedName.Create(s_module, "f"),
                    SiblingDeclName: DeclQualifiedName.Create(s_module, "f__stripped"),
                    OriginalArity: 1,
                    ParameterOrigins:
                    [
                    new SiblingParameterOrigin.Identity(SiblingIndex: 0, OriginalIndex: 0),
                    ],
                    ResultTransform: new SiblingResultTransform.WrapWithConstructor(
                        DeclQualifiedName.Create(s_module, "Wrap"))));

        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls, siblings);

        // Body collapses to `f__stripped 5` directly.
        var newApp = GetCallerBody(rewritten).Should().BeOfType<SyntaxTypes.Expression.Application>().Subject;
        newApp.Arguments.Should().HaveCount(2);

        newApp.Arguments[0].Value.Should().BeOfType<SyntaxTypes.Expression.FunctionOrValue>()
            .Which.Name.Should().Be("f__stripped");

        newApp.Arguments[1].Value.Should().BeOfType<SyntaxTypes.Expression.Integer>()
            .Which.Value.Should().Be(5);
    }

    // ------------------------------------------------------------------
    // Negative cases.
    // ------------------------------------------------------------------

    [Fact]
    public void Negative_no_rewrite_when_constructor_names_differ()
    {
        // type Wrap = Wrap Int
        // type Other = Other Int
        // caller = let (Other p) = Wrap 1 in p
        // → no rewrite (the destructure / wrap are different constructors;
        //   the original code would be a runtime type error, but the
        //   peephole is conservative).
        var letBlock =
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                N<SyntaxTypes.Expression.LetDeclaration>(
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Other", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression:
                        N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "Wrap"), new SyntaxTypes.Expression.Integer(1)))))
                ],
                Expression: N(FunctionOrValue([], "p")));

        var caller = FuncDecl("caller", [], new SyntaxTypes.Expression.LetExpression(letBlock));

        var decls =
            Decls(
                ("Wrap", NewtypeDecl("Wrap", "Wrap")),
                ("Other", NewtypeDecl("Other", "Other")),
                ("caller", caller));

        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls);

        // Destructuring pattern is still NamedPattern("Other", ...) — unchanged.
        var letExpr = GetCallerBody(rewritten).Should().BeOfType<SyntaxTypes.Expression.LetExpression>().Subject;

        var destr =
            letExpr.Value.Declarations[0].Value
            .Should().BeOfType<SyntaxTypes.Expression.LetDeclaration.LetDestructuring>().Subject;

        destr.Pattern.Value.Should().BeOfType<SyntaxTypes.Pattern.NamedPattern>()
            .Which.Name.Name.Should().Be("Other");
    }

    [Fact]
    public void Negative_no_rewrite_for_unrelated_constructor_not_in_newtype_registry()
    {
        // `Pair` is a 2-arg ctor — not newtype-shaped, so it must not appear
        // in the registry and the cancellation must not fire.
        var pairTs =
            new SyntaxTypes.TypeStruct(
                Documentation: null,
                Name: N("Pair"),
                Generics: [],
                Constructors:
                [
                N(
                    new SyntaxTypes.ValueConstructor(
                        Name: N("Pair"),
                        Arguments:
                        [
                            N(IntType()),
                            N(IntType()),
                        ]))
                ]);

        var letBlock =
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                N<SyntaxTypes.Expression.LetDeclaration>(
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Pair", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression:
                        N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "Pair"), new SyntaxTypes.Expression.Integer(1)))))
                ],
                Expression: N(FunctionOrValue([], "p")));

        var caller = FuncDecl("caller", [], new SyntaxTypes.Expression.LetExpression(letBlock));

        var decls =
            Decls(
                ("Pair", new SyntaxTypes.Declaration.CustomTypeDeclaration(pairTs)),
                ("caller", caller));

        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls);

        var letExpr = GetCallerBody(rewritten).Should().BeOfType<SyntaxTypes.Expression.LetExpression>().Subject;

        letExpr.Value.Declarations[0].Value
            .Should().BeOfType<SyntaxTypes.Expression.LetDeclaration.LetDestructuring>()
            .Which.Pattern.Value.Should().BeOfType<SyntaxTypes.Pattern.NamedPattern>();
    }

    [Fact]
    public void Negative_no_rewrite_for_multi_arm_case_of()
    {
        // case Wrap 1 of
        //   Wrap p -> p
        //   _      -> 0
        // → must not be cancelled (the second arm makes the case-of useful;
        //   the design explicitly scopes single-arm only).
        var caseExpr =
            new SyntaxTypes.Expression.CaseExpression(
                new SyntaxTypes.CaseBlock(
                    Expression:
                    N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "Wrap"), new SyntaxTypes.Expression.Integer(1))),
                    Cases:
                    [
                    new SyntaxTypes.Case(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Wrap", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression: N(FunctionOrValue([], "p"))),

                    new SyntaxTypes.Case(
                        Pattern: N<SyntaxTypes.Pattern>(new SyntaxTypes.Pattern.AllPattern()),
                        Expression: N<SyntaxTypes.Expression>(new SyntaxTypes.Expression.Integer(0))),
                    ]));

        var caller = FuncDecl("caller", [], caseExpr);

        var decls =
            Decls(
                ("Wrap", NewtypeDecl("Wrap", "Wrap")),
                ("caller", caller));

        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls);

        // Body remains a CaseExpression (untouched by the rewrite).
        GetCallerBody(rewritten).Should().BeOfType<SyntaxTypes.Expression.CaseExpression>();
    }

    // ------------------------------------------------------------------
    // Idempotence.
    // ------------------------------------------------------------------

    [Fact]
    public void Idempotence_second_invocation_returns_same_body_shape()
    {
        // Run Shape A twice; the second pass must not change anything.
        // With the substitute-when-single-use optimization the body
        // collapses to just `42` after the first pass; the second pass
        // sees no further opportunity and is a no-op.
        var letBlock =
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                N<SyntaxTypes.Expression.LetDeclaration>(
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Wrap", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression:
                        N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "Wrap"), new SyntaxTypes.Expression.Integer(42)))))
                ],
                Expression: N(FunctionOrValue([], "p")));

        var caller = FuncDecl("caller", [], new SyntaxTypes.Expression.LetExpression(letBlock));

        var decls =
            Decls(
                ("Wrap", NewtypeDecl("Wrap", "Wrap")),
                ("caller", caller));

        var first = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls);
        var second = WrapUnwrapCancellation.RewriteDeclarationDictionary(first);

        // Both rewrites collapse the body to the integer literal `42`.
        GetCallerBody(first).Should().BeOfType<SyntaxTypes.Expression.Integer>()
            .Which.Value.Should().Be(42);

        GetCallerBody(second).Should().BeOfType<SyntaxTypes.Expression.Integer>()
            .Which.Value.Should().Be(42);
    }

    [Fact]
    public void Empty_newtype_registry_returns_input_dictionary_unchanged()
    {
        // No newtypes declared — rewrite must short-circuit and return
        // the input as-is (reference equality is too strong because of
        // builders, but key set + values must match).
        var caller = FuncDecl("caller", [], new SyntaxTypes.Expression.Integer(7));

        var decls = Decls(("caller", caller));

        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls);

        rewritten.Should().BeSameAs(decls);
    }

    // ------------------------------------------------------------------
    // Bottom-up walk: cancellation fires inside nested expressions.
    // ------------------------------------------------------------------

    [Fact]
    public void Cancellation_fires_inside_nested_lambda_body()
    {
        // caller = \_ -> let (Wrap p) = Wrap 7 in p
        // The let-expression is nested inside a lambda body — the
        // bottom-up walk must reach it.
        var innerLetBlock =
            new SyntaxTypes.Expression.LetBlock(
                Declarations:
                [
                N<SyntaxTypes.Expression.LetDeclaration>(
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Pattern:
                        N<SyntaxTypes.Pattern>(NamedPat(s_module, "Wrap", new SyntaxTypes.Pattern.VarPattern("p"))),
                        Expression:
                        N<SyntaxTypes.Expression>(App(FunctionOrValue(s_module, "Wrap"), new SyntaxTypes.Expression.Integer(7)))))
                ],
                Expression: N(FunctionOrValue([], "p")));

        var lambda =
            new SyntaxTypes.Expression.LambdaExpression(
                new SyntaxTypes.LambdaStruct(
                    Arguments:
                    [
                        N<SyntaxTypes.Pattern>(new SyntaxTypes.Pattern.AllPattern()),
                    ],
                    Expression: N<SyntaxTypes.Expression>(new SyntaxTypes.Expression.LetExpression(innerLetBlock))));

        var caller = FuncDecl("caller", [], lambda);

        var decls =
            Decls(
                ("Wrap", NewtypeDecl("Wrap", "Wrap")),
                ("caller", caller));

        var rewritten = WrapUnwrapCancellation.RewriteDeclarationDictionary(decls);

        // Lambda body collapses from `let (Wrap p) = Wrap 7 in p` to
        // just `7` (after Shape A cancellation + single-use substitution).
        var lambdaResult =
            GetCallerBody(rewritten).Should().BeOfType<SyntaxTypes.Expression.LambdaExpression>().Subject;

        lambdaResult.Lambda.Expression.Value
            .Should().BeOfType<SyntaxTypes.Expression.Integer>()
            .Which.Value.Should().Be(7);
    }
}
