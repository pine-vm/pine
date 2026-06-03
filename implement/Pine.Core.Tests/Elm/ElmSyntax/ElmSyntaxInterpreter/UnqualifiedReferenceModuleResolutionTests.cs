using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Generic;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;
using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Regression tests for resolving <em>unqualified</em> top-level references when the same simple
/// name is declared in more than one module.
/// <para>
/// The compiler's lambda-lifting pass emits helper declarations (e.g.
/// <c>map2__lifted__lambda1</c>) and references them from their home module's body using an
/// <em>unqualified</em> name. Several modules can independently define a helper with the same
/// simple name but different behaviour — for instance <c>ParserFast.map2__lifted__lambda1</c>
/// matches a 2-argument <c>Good</c> constructor while <c>Parser.Advanced.map2__lifted__lambda1</c>
/// matches a 3-argument <c>Good</c>. The interpreter must resolve such an unqualified reference to
/// the helper in the <em>caller's own module</em>; otherwise a value built by one module's
/// constructor can reach the other module's helper and fail to match any case arm.
/// </para>
/// <para>
/// This is the interpreter-oracle gap recorded in
/// <c>explore/internal-analysis/2026-05-30-hover-let-bound-name-specialize-before-lambda-lifting-loop.md</c>
/// (surfaced by <c>Interpreter_route_false</c>). These tests reproduce it in isolation, without
/// the slow full-LanguageService lowering.
/// </para>
/// </summary>
public class UnqualifiedReferenceModuleResolutionTests
{
    private const string ModuleP =
        """"
        module P exposing (..)


        type StepP a
            = Good a Int
            | Bad


        helper : a -> Int -> StepP a
        helper v s =
            Good v s


        entry : a -> Int -> StepP a
        entry v s =
            case helper v s of
                Good a s1 ->
                    Good a s1

                Bad ->
                    Bad
        """";

    private const string ModuleQ =
        """"
        module Q exposing (..)


        type StepQ a
            = Good Bool a Int
            | Bad


        helper : a -> Int -> StepQ a
        helper v s =
            Good True v s
        """";

    /// <summary>
    /// Builds a declaration dictionary keyed by full module-qualified name from the two module
    /// texts. The colliding module (<paramref name="collidingFirst"/> = <c>Q</c>) is inserted
    /// first so that a module-agnostic, iteration-order-dependent resolver would pick the wrong
    /// helper — making the same-module-preference behaviour observable and deterministic.
    /// </summary>
    private static IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> BuildDeclarations()
    {
        var declarations = new Dictionary<DeclQualifiedName, SyntaxTypes.Declaration>();

        // Insert Q before P on purpose.
        AddModule(declarations, ["Q"], ModuleQ);
        AddModule(declarations, ["P"], ModuleP);

        return declarations;
    }

    private static void AddModule(
        Dictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        IReadOnlyList<string> moduleName,
        string moduleText)
    {
        var parsedFile =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("parse failed: " + err));

        foreach (var declNode in parsedFile.Declarations)
        {
            var name = InterpreterTestHelper.DeclarationName(declNode.Value);

            if (name is null)
                continue;

            declarations[new DeclQualifiedName(moduleName, name)] = declNode.Value;
        }
    }

    [Fact]
    public void Unqualified_reference_resolves_to_callers_own_module_helper()
    {
        var declarations = BuildDeclarations();

        // P.entry's body references `helper` unqualified. It must resolve to P.helper (2-arg Good),
        // not Q.helper (3-arg Good), so the value matches P.entry's `Good a s1` case arm.
        var result =
            ElmInterpreter.Interpret(
                new DeclQualifiedName(["P"], "entry"),
                [ElmValue.Integer(7), ElmValue.Integer(0)],
                declarations);

        var value = result.Extract(err => throw new Exception(err.ToString()));

        var rendered = ElmValue.RenderAsElmExpression(value).expressionString;

        rendered.Should().Be("Good 7 0");
    }

    [Fact]
    public void Direct_resolver_prefers_caller_module_for_unqualified_reference()
    {
        var declarations = BuildDeclarations();

        var callerContext =
            new ElmInterpreter.ApplicationContext(
                CurrentTopLevel: new DeclQualifiedName(["P"], "entry"),
                LocalBindings: System.Collections.Immutable.ImmutableDictionary<string, Pine.Core.Internal.PineValueInProcess>.Empty);

        var application =
            new ElmInterpreter.Application(
                FunctionName: new DeclQualifiedName([], "helper"),
                Arguments: [ElmValue.Integer(7), ElmValue.Integer(0)],
                Context: callerContext);

        var resolution = ElmInterpreter.UserDefinedResolver(application, declarations);

        var continueWith =
            resolution.Should().BeOfType<ElmInterpreter.ApplicationResolution.ContinueWithFunction>().Subject;

        // P.helper has a single non-tuple parameter pattern per argument; Q.helper differs only in
        // its body. Assert we got P.helper by checking its body builds a 2-argument Good.
        var body = continueWith.Function.Expression;

        var application2 = body.Should().BeOfType<Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Expression.Application>().Subject;

        var head = application2.Function.Should().BeOfType<Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Expression.FunctionOrValue>().Subject;

        head.Name.Should().Be("Good");

        // P.helper: `Good v s` -> 2 args. Q.helper: `Good True v s` -> 3 args.
        application2.Arguments.Count.Should().Be(2);
    }
}
