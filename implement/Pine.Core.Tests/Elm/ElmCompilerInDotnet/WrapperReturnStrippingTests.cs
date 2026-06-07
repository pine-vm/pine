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
/// Unit tests for the Step B sibling-decl generation + call-site
/// rewriter primitives in <see cref="WrapperReturnStripping"/> — built
/// on the Step A primitives in <see cref="NewtypeWrapperAnalysis"/>.
/// All tests operate on synthetically constructed syntax trees with no
/// compilation pipeline involved.
/// </summary>
public class WrapperReturnStrippingTests
{
    private static readonly SyntaxModel.Location s_zeroLoc = new(Row: 0, Column: 0);

    private static readonly SyntaxModel.Range s_zeroRange = new(Start: s_zeroLoc, End: s_zeroLoc);

    private static SyntaxModel.Node<T> N<T>(T value) => new(s_zeroRange, value);

    private static SyntaxTypes.TypeAnnotation IntType() =>
        new SyntaxTypes.TypeAnnotation.Typed(
            N(((IReadOnlyList<string>)["Basics"], "Int")),
            []);

    private static SyntaxTypes.Declaration.CustomTypeDeclaration NewtypeDecl(
        string typeName,
        string ctorName)
    {
        var ctor =
            new SyntaxTypes.ValueConstructor(
                Name: N(ctorName),
                Arguments:
                [
                    N(IntType()),
                ]);

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

    private static readonly IReadOnlyList<string> s_module = ["Lib"];

    private static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> Decls(
        params (string name, SyntaxTypes.Declaration decl)[] items)
    {
        var b = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (n, d) in items)
            b[DeclQualifiedName.Create(s_module, n)] = d;

        return b.ToImmutable();
    }

    [Fact]
    public void BuildStripPlans_generates_plan_for_simple_wrapper_return_function()
    {
        // type Wrapped = Wrap Int
        // f x = Wrap x
        var inner = FunctionOrValue([], "x");
        var body = App(FunctionOrValue(s_module, "Wrap"), inner);

        var decls =
            Decls(
                ("Wrapped", NewtypeDecl("Wrapped", "Wrap")),
                ("f", FuncDecl("f", ["x"], body)));

        var registry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(decls);
        var plans = WrapperReturnStripping.BuildStripPlans(decls, registry);

        plans.Should().ContainKey(DeclQualifiedName.Create(s_module, "f"));

        var plan = plans[DeclQualifiedName.Create(s_module, "f")];
        plan.OriginalArity.Should().Be(1);
        plan.StrippedDeclName.DeclName.Should().Be("f__stripped");
        plan.StrippedDeclName.Namespaces.Should().Equal(s_module);
        plan.ConstructorName.Should().Be(DeclQualifiedName.Create(s_module, "Wrap"));
        plan.StrippedBody.Should().Be(inner);
    }

    [Fact]
    public void BuildStripPlans_skips_zero_arg_decl()
    {
        // f = Wrap x  — top-level value, no benefit from stripping.
        var body = App(FunctionOrValue(s_module, "Wrap"), FunctionOrValue([], "x"));

        var decls =
            Decls(
                ("Wrapped", NewtypeDecl("Wrapped", "Wrap")),
                ("f", FuncDecl("f", [], body)));

        var registry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(decls);
        WrapperReturnStripping.BuildStripPlans(decls, registry).Should().BeEmpty();
    }

    [Fact]
    public void BuildStripPlans_throws_when_sibling_name_collides_with_divergent_body()
    {
        // Both `f` (planned) and `f__stripped` (existing with a
        // structurally divergent body) are present. §11.8: the
        // planner must fail loudly so the pipeline-ordering bug
        // that produced the collision is visible, rather than
        // silently preserving the existing body.
        var body = App(FunctionOrValue(s_module, "Wrap"), FunctionOrValue([], "x"));

        var decls =
            Decls(
                ("Wrapped", NewtypeDecl("Wrapped", "Wrap")),
                ("f", FuncDecl("f", ["x"], body)),
                ("f__stripped", FuncDecl("f__stripped", ["y"], FunctionOrValue([], "y"))));

        var registry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(decls);

        var act = () => WrapperReturnStripping.BuildStripPlans(decls, registry);

        act.Should()
            .Throw<System.InvalidOperationException>()
            .WithMessage("*f__stripped*diverges*");
    }

    [Fact]
    public void TryMatchExtendedWrapperReturnBody_re_threads_let_block()
    {
        // f x = let y = x in Wrap y
        var letDecl =
            new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                N<SyntaxTypes.Pattern>(new SyntaxTypes.Pattern.VarPattern("y")),
                N(FunctionOrValue([], "x")));

        var letBody =
            new SyntaxTypes.Expression.LetExpression(
                new SyntaxTypes.Expression.LetBlock(
                    Declarations:
                    [
                        N<SyntaxTypes.Expression.LetDeclaration>(letDecl),
                    ],
                    Expression: N<SyntaxTypes.Expression>(
                        App(FunctionOrValue(s_module, "Wrap"), FunctionOrValue([], "y")))));

        var decls = Decls(("Wrapped", NewtypeDecl("Wrapped", "Wrap")));
        var registry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(decls);

        var match =
            WrapperReturnStripping.TryMatchExtendedWrapperReturnBody(
                letBody,
                registry,
                currentModuleName: s_module);

        match.Should().NotBeNull();
        match!.ConstructorName.Should().Be(DeclQualifiedName.Create(s_module, "Wrap"));
        match.StrippedInnerExpression.Should().BeOfType<SyntaxTypes.Expression.LetExpression>();

        var rewrittenLet = (SyntaxTypes.Expression.LetExpression)match.StrippedInnerExpression;
        rewrittenLet.Value.Expression.Value.Should().Be(FunctionOrValue([], "y"));
    }

    [Fact]
    public void TryMatchExtendedWrapperReturnBody_re_threads_caseof_when_all_arms_match_same_constructor()
    {
        // f x = case x of
        //   _ -> Wrap a
        //   _ -> Wrap b
        var caseExpr =
            new SyntaxTypes.Expression.CaseExpression(
                new SyntaxTypes.CaseBlock(
                    Expression: N(FunctionOrValue([], "x")),
                    Cases:
                    [
                        new(
                            N<SyntaxTypes.Pattern>(new SyntaxTypes.Pattern.AllPattern()),
                            N<SyntaxTypes.Expression>(
                                App(FunctionOrValue(s_module, "Wrap"), FunctionOrValue([], "a")))),
                        new(
                            N<SyntaxTypes.Pattern>(new SyntaxTypes.Pattern.AllPattern()),
                            N<SyntaxTypes.Expression>(
                                App(FunctionOrValue(s_module, "Wrap"), FunctionOrValue([], "b")))),
                    ]));

        var decls = Decls(("Wrapped", NewtypeDecl("Wrapped", "Wrap")));
        var registry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(decls);

        var match =
            WrapperReturnStripping.TryMatchExtendedWrapperReturnBody(
                caseExpr,
                registry,
                currentModuleName: s_module);

        match.Should().NotBeNull();
        match!.ConstructorName.Should().Be(DeclQualifiedName.Create(s_module, "Wrap"));

        var rewrittenCase = (SyntaxTypes.Expression.CaseExpression)match.StrippedInnerExpression;
        rewrittenCase.CaseBlock.Cases.Should().HaveCount(2);
        rewrittenCase.CaseBlock.Cases[0].Expression.Value.Should().Be(FunctionOrValue([], "a"));
        rewrittenCase.CaseBlock.Cases[1].Expression.Value.Should().Be(FunctionOrValue([], "b"));
    }

    [Fact]
    public void TryMatchExtendedWrapperReturnBody_returns_null_when_one_caseof_arm_does_not_match()
    {
        var caseExpr =
            new SyntaxTypes.Expression.CaseExpression(
                new SyntaxTypes.CaseBlock(
                    Expression: N(FunctionOrValue([], "x")),
                    Cases:
                    [
                        new(
                            N<SyntaxTypes.Pattern>(new SyntaxTypes.Pattern.AllPattern()),
                            N<SyntaxTypes.Expression>(
                                App(FunctionOrValue(s_module, "Wrap"), FunctionOrValue([], "a")))),
                        new(
                            N<SyntaxTypes.Pattern>(new SyntaxTypes.Pattern.AllPattern()),
                            // This arm is a bare reference — does not match the wrapper shape.
                            N(FunctionOrValue([], "b"))),
                    ]));

        var decls = Decls(("Wrapped", NewtypeDecl("Wrapped", "Wrap")));
        var registry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(decls);

        WrapperReturnStripping.TryMatchExtendedWrapperReturnBody(
            caseExpr,
            registry,
            currentModuleName: s_module).Should().BeNull();
    }

    [Fact]
    public void TryMatchExtendedWrapperReturnBody_returns_null_when_caseof_has_no_arms()
    {
        var emptyCase =
            new SyntaxTypes.Expression.CaseExpression(
                new SyntaxTypes.CaseBlock(
                    Expression: N(FunctionOrValue([], "x")),
                    Cases: []));

        var decls = Decls(("Wrapped", NewtypeDecl("Wrapped", "Wrap")));
        var registry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(decls);

        WrapperReturnStripping.TryMatchExtendedWrapperReturnBody(
            emptyCase,
            registry,
            currentModuleName: s_module).Should().BeNull();
    }

    [Fact]
    public void RewriteCallSitesInExpression_rewrites_fully_saturated_call_to_wrap_stripped_in_constructor()
    {
        // Plan: f arity 1, ctor Wrap, sibling f__stripped.
        var plan =
            new WrapperReturnStripping.WrapperStripPlan(
                OriginalDeclName: DeclQualifiedName.Create(s_module, "f"),
                StrippedDeclName: DeclQualifiedName.Create(s_module, "f__stripped"),
                ConstructorName: DeclQualifiedName.Create(s_module, "Wrap"),
                OriginalArity: 1,
                StrippedBody: FunctionOrValue([], "x"));

        var plans =
            ImmutableDictionary.CreateRange(
                [
                new KeyValuePair<DeclQualifiedName, WrapperReturnStripping.WrapperStripPlan>(
                    plan.OriginalDeclName,
                    plan),
                ]);

        // Call site: f y
        var callSite = App(FunctionOrValue(s_module, "f"), FunctionOrValue([], "y"));

        var rewritten =
            WrapperReturnStripping.RewriteCallSitesInExpression(
                callSite,
                plans,
                currentModuleName: s_module);

        var outerApp = rewritten.Should().BeOfType<SyntaxTypes.Expression.Application>().Subject;
        outerApp.Arguments.Should().HaveCount(2);

        var ctorRef =
            outerApp.Arguments[0].Value.Should().BeOfType<SyntaxTypes.Expression.FunctionOrValue>().Subject;

        ctorRef.Name.Should().Be("Wrap");
        ctorRef.ModuleName.Should().Equal(s_module);

        var strippedApp =
            outerApp.Arguments[1].Value.Should().BeOfType<SyntaxTypes.Expression.Application>().Subject;

        strippedApp.Arguments.Should().HaveCount(2);

        var strippedHead =
            strippedApp.Arguments[0].Value.Should().BeOfType<SyntaxTypes.Expression.FunctionOrValue>().Subject;

        strippedHead.Name.Should().Be("f__stripped");
        strippedHead.ModuleName.Should().Equal(s_module);

        strippedApp.Arguments[1].Value.Should().Be(FunctionOrValue([], "y"));
    }

    [Fact]
    public void RewriteCallSitesInExpression_leaves_partial_application_alone()
    {
        // Plan: f arity 2 — only `f a b` is fully saturated.
        var plan =
            new WrapperReturnStripping.WrapperStripPlan(
                OriginalDeclName: DeclQualifiedName.Create(s_module, "f"),
                StrippedDeclName: DeclQualifiedName.Create(s_module, "f__stripped"),
                ConstructorName: DeclQualifiedName.Create(s_module, "Wrap"),
                OriginalArity: 2,
                StrippedBody: FunctionOrValue([], "x"));

        var plans =
            ImmutableDictionary.CreateRange(
                [
                new KeyValuePair<DeclQualifiedName, WrapperReturnStripping.WrapperStripPlan>(
                    plan.OriginalDeclName,
                    plan),
                ]);

        // Partial application: `f y` (only 1 arg, arity is 2).
        var partial = App(FunctionOrValue(s_module, "f"), FunctionOrValue([], "y"));

        var rewritten =
            WrapperReturnStripping.RewriteCallSitesInExpression(
                partial,
                plans,
                currentModuleName: s_module);

        // Should be structurally identical (we explicitly do NOT touch
        // partial applications in Step B; they are picked up by
        // subsequent rounds after flattening + specialization).
        rewritten.Should().BeOfType<SyntaxTypes.Expression.Application>();
        var resultApp = (SyntaxTypes.Expression.Application)rewritten;
        resultApp.Arguments.Should().HaveCount(2);
        resultApp.Arguments[0].Value.Should().Be(FunctionOrValue(s_module, "f"));
        resultApp.Arguments[1].Value.Should().Be(FunctionOrValue([], "y"));
    }

    [Fact]
    public void RewriteCallSitesInExpression_leaves_unknown_function_alone()
    {
        var plans = ImmutableDictionary<DeclQualifiedName, WrapperReturnStripping.WrapperStripPlan>.Empty;

        var callSite = App(FunctionOrValue(s_module, "g"), FunctionOrValue([], "y"));

        var rewritten =
            WrapperReturnStripping.RewriteCallSitesInExpression(
                callSite,
                plans,
                currentModuleName: s_module);

        rewritten.Should().Be(callSite);
    }

    [Fact]
    public void RewriteDeclarationDictionary_appends_stripped_sibling_and_rewrites_call_sites()
    {
        // type Wrapped = Wrap Int
        // f x = Wrap x
        // g y = f y         -- caller: should be rewritten to `Wrap (f__stripped y)`
        var fBody = App(FunctionOrValue(s_module, "Wrap"), FunctionOrValue([], "x"));
        var gBody = App(FunctionOrValue(s_module, "f"), FunctionOrValue([], "y"));

        var decls =
            Decls(
                ("Wrapped", NewtypeDecl("Wrapped", "Wrap")),
                ("f", FuncDecl("f", ["x"], fBody)),
                ("g", FuncDecl("g", ["y"], gBody)));

        var rewritten = WrapperReturnStripping.RewriteDeclarationDictionary(decls);

        rewritten.Should().ContainKey(DeclQualifiedName.Create(s_module, "f__stripped"));

        var fStripped =
            rewritten[DeclQualifiedName.Create(s_module, "f__stripped")]
            .Should().BeOfType<SyntaxTypes.Declaration.FunctionDeclaration>().Subject;

        fStripped.Function.Declaration.Value.Arguments.Should().HaveCount(1);
        fStripped.Function.Declaration.Value.Expression.Value.Should().Be(FunctionOrValue([], "x"));

        // Caller `g` should now wrap the stripped sibling call.
        var gAfter =
            rewritten[DeclQualifiedName.Create(s_module, "g")]
            .Should().BeOfType<SyntaxTypes.Declaration.FunctionDeclaration>().Subject;

        var gOuter =
            gAfter.Function.Declaration.Value.Expression.Value
            .Should().BeOfType<SyntaxTypes.Expression.Application>().Subject;

        gOuter.Arguments.Should().HaveCount(2);

        var ctorRef =
            gOuter.Arguments[0].Value.Should().BeOfType<SyntaxTypes.Expression.FunctionOrValue>().Subject;

        ctorRef.Name.Should().Be("Wrap");

        var strippedApp =
            gOuter.Arguments[1].Value.Should().BeOfType<SyntaxTypes.Expression.Application>().Subject;

        ((SyntaxTypes.Expression.FunctionOrValue)strippedApp.Arguments[0].Value).Name
            .Should().Be("f__stripped");

        // The original `f` decl's body is replaced with a forwarding
        // delegation to the stripped sibling so that subsequent
        // inlining passes do not re-introduce the wrapper-form body
        // at every call site.
        var fAfter =
            rewritten[DeclQualifiedName.Create(s_module, "f")]
            .Should().BeOfType<SyntaxTypes.Declaration.FunctionDeclaration>().Subject;

        var fOuter =
            fAfter.Function.Declaration.Value.Expression.Value
            .Should().BeOfType<SyntaxTypes.Expression.Application>().Subject;

        fOuter.Arguments.Should().HaveCount(2);

        ((SyntaxTypes.Expression.FunctionOrValue)fOuter.Arguments[0].Value).Name
            .Should().Be("Wrap");

        var fForwardApp =
            fOuter.Arguments[1].Value.Should().BeOfType<SyntaxTypes.Expression.Application>().Subject;

        ((SyntaxTypes.Expression.FunctionOrValue)fForwardApp.Arguments[0].Value).Name
            .Should().Be("f__stripped");

        ((SyntaxTypes.Expression.FunctionOrValue)fForwardApp.Arguments[1].Value).Name
            .Should().Be("x");
    }

    [Fact]
    public void RewriteDeclarationDictionary_handles_recursive_call_inside_stripped_body()
    {
        // type Wrapped = Wrap Int
        // f x = Wrap (f x)   -- recursive call inside the stripped inner expr
        var recursiveCall = App(FunctionOrValue(s_module, "f"), FunctionOrValue([], "x"));
        var fBody = App(FunctionOrValue(s_module, "Wrap"), recursiveCall);

        var decls =
            Decls(
                ("Wrapped", NewtypeDecl("Wrapped", "Wrap")),
                ("f", FuncDecl("f", ["x"], fBody)));

        var rewritten = WrapperReturnStripping.RewriteDeclarationDictionary(decls);

        // f__stripped's body should be `Wrap (f__stripped x)` — the
        // recursive call site inside the stripped body must itself be
        // rewritten to go through the sibling.
        var fStripped =
            rewritten[DeclQualifiedName.Create(s_module, "f__stripped")]
            .Should().BeOfType<SyntaxTypes.Declaration.FunctionDeclaration>().Subject;

        var stripBody = fStripped.Function.Declaration.Value.Expression.Value;

        var outer =
            stripBody.Should().BeOfType<SyntaxTypes.Expression.Application>().Subject;

        outer.Arguments.Should().HaveCount(2);

        var ctorHead =
            outer.Arguments[0].Value.Should().BeOfType<SyntaxTypes.Expression.FunctionOrValue>().Subject;

        ctorHead.Name.Should().Be("Wrap");

        var inner =
            outer.Arguments[1].Value.Should().BeOfType<SyntaxTypes.Expression.Application>().Subject;

        ((SyntaxTypes.Expression.FunctionOrValue)inner.Arguments[0].Value).Name
            .Should().Be("f__stripped");
    }

    [Fact]
    public void RewriteDeclarationDictionary_is_a_no_op_when_no_newtype_is_present()
    {
        var fBody = FunctionOrValue([], "x");
        var decls = Decls(("f", FuncDecl("f", ["x"], fBody)));

        var rewritten = WrapperReturnStripping.RewriteDeclarationDictionary(decls);

        rewritten.Should().BeSameAs(decls);
    }

    [Fact]
    public void RewriteDeclarationDictionary_does_not_double_strip_already_stripped_sibling()
    {
        // Apply the rewrite twice. The sibling-name collision guard in
        // BuildStripPlans must prevent a `f__stripped__stripped` from
        // appearing on the second application. (Note: the second pass
        // can still produce *additional* strip plans for callers whose
        // bodies became wrapper-return shaped as a side effect of the
        // first rewrite — that is the intended cascading behaviour.
        // What we explicitly forbid is double-stripping the same
        // sibling.)
        var fBody = App(FunctionOrValue(s_module, "Wrap"), FunctionOrValue([], "x"));

        var decls =
            Decls(
                ("Wrapped", NewtypeDecl("Wrapped", "Wrap")),
                ("f", FuncDecl("f", ["x"], fBody)));

        var first = WrapperReturnStripping.RewriteDeclarationDictionary(decls);
        var second = WrapperReturnStripping.RewriteDeclarationDictionary(first);

        second.Should().NotContainKey(DeclQualifiedName.Create(s_module, "f__stripped__stripped"));

        // The original `f` and the generated `f__stripped` both still
        // exist after the second pass.
        second.Should().ContainKey(DeclQualifiedName.Create(s_module, "f"));
        second.Should().ContainKey(DeclQualifiedName.Create(s_module, "f__stripped"));
    }
}
