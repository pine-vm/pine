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
/// Unit tests for the pure analysis primitives in
/// <see cref="NewtypeWrapperAnalysis"/> — the foundation of D2 Step 2
/// (stripping the newtype-wrapper from function-return roots). These
/// tests exercise the helpers in isolation against synthetically
/// constructed syntax trees, with no compilation pipeline involved.
/// </summary>
public class NewtypeWrapperAnalysisTests
{
    private static readonly SyntaxModel.Location s_zeroLoc =
        new(Row: 0, Column: 0);

    private static readonly SyntaxModel.Range s_zeroRange =
        new(Start: s_zeroLoc, End: s_zeroLoc);

    private static SyntaxModel.Node<T> N<T>(T value) =>
        new(s_zeroRange, value);

    private static SyntaxTypes.TypeAnnotation IntType() =>
        new SyntaxTypes.TypeAnnotation.Typed(
            N(((IReadOnlyList<string>)["Basics"], "Int")),
            []);

    private static SyntaxTypes.TypeStruct MakeTypeStruct(
        string typeName,
        IReadOnlyList<(string ctorName, IReadOnlyList<SyntaxTypes.TypeAnnotation> ctorArgs)> ctors,
        IReadOnlyList<string>? generics = null)
    {
        var genericNodes = new List<SyntaxModel.Node<string>>();

        foreach (var g in generics ?? [])
            genericNodes.Add(N(g));

        var ctorNodes = new List<SyntaxModel.Node<SyntaxTypes.ValueConstructor>>();

        foreach (var c in ctors)
        {
            var argNodes = new List<SyntaxModel.Node<SyntaxTypes.TypeAnnotation>>();

            foreach (var a in c.ctorArgs)
                argNodes.Add(N(a));

            ctorNodes.Add(
                N(
                    new SyntaxTypes.ValueConstructor(
                        Name: N(c.ctorName),
                        Arguments: argNodes)));
        }

        return
            new SyntaxTypes.TypeStruct(
                Documentation: null,
                Name: N(typeName),
                Generics: genericNodes,
                Constructors: ctorNodes);
    }

    [Fact]
    public void IsNewtypeShapedTypeStruct_returns_true_for_single_ctor_single_arg()
    {
        var ts =
            MakeTypeStruct(
                "Wrapped",
                [("Wrapped", new[] { IntType() })],
                generics: ["a"]);

        var ok =
            NewtypeWrapperAnalysis.IsNewtypeShapedTypeStruct(
                ts,
                out var ctorName,
                out var inner);

        ok.Should().BeTrue();
        ctorName.Should().Be("Wrapped");
        inner.Should().BeOfType<SyntaxTypes.TypeAnnotation.Typed>();
    }

    [Fact]
    public void IsNewtypeShapedTypeStruct_returns_false_for_zero_arg_constructor()
    {
        // type Empty = Empty  — the conventional "phantom" / unit-marker shape
        var ts = MakeTypeStruct("Empty", [("Empty", System.Array.Empty<SyntaxTypes.TypeAnnotation>())]);

        var ok =
            NewtypeWrapperAnalysis.IsNewtypeShapedTypeStruct(
                ts,
                out _,
                out _);

        ok.Should().BeFalse();
    }

    [Fact]
    public void IsNewtypeShapedTypeStruct_returns_false_for_two_arg_constructor()
    {
        // type Pair a b = Pair a b
        var ts =
            MakeTypeStruct(
                "Pair",
                [("Pair", new[] { IntType(), IntType() })],
                generics: ["a", "b"]);

        var ok =
            NewtypeWrapperAnalysis.IsNewtypeShapedTypeStruct(
                ts,
                out _,
                out _);

        ok.Should().BeFalse();
    }

    [Fact]
    public void IsNewtypeShapedTypeStruct_returns_false_for_two_constructors()
    {
        // type Maybe a = Just a | Nothing  — not a newtype
        var ts =
            MakeTypeStruct(
                "Maybe",
                [
                ("Just", new[] { IntType() }),
                ("Nothing", System.Array.Empty<SyntaxTypes.TypeAnnotation>()),
                ],
                generics: ["a"]);

        var ok =
            NewtypeWrapperAnalysis.IsNewtypeShapedTypeStruct(
                ts,
                out _,
                out _);

        ok.Should().BeFalse();
    }

    [Fact]
    public void BuildNewtypeRegistry_indexes_by_both_type_name_and_constructor_name()
    {
        var ts =
            MakeTypeStruct(
                "Parser",
                [("Parser", new[] { IntType() })],
                generics: ["a"]);

        var moduleName = (IReadOnlyList<string>)["ParserFast"];

        var decls =
            ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>.Empty
            .Add(
                DeclQualifiedName.Create(moduleName, "Parser"),
                new SyntaxTypes.Declaration.CustomTypeDeclaration(ts));

        var registry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(decls);

        // Both the type and the constructor live in the same module
        // and happen to share the same bare name here, so the registry
        // should still contain a single entry under that one key.
        registry.Should().ContainKey(DeclQualifiedName.Create(moduleName, "Parser"));

        registry[DeclQualifiedName.Create(moduleName, "Parser")].ConstructorName
            .Should().Be(DeclQualifiedName.Create(moduleName, "Parser"));
    }

    [Fact]
    public void BuildNewtypeRegistry_indexes_distinct_type_and_constructor_names_separately()
    {
        // type Wrapped a = MkWrapped a  — type and constructor names differ
        var ts =
            MakeTypeStruct(
                "Wrapped",
                [("MkWrapped", new[] { IntType() })],
                generics: ["a"]);

        var moduleName = (IReadOnlyList<string>)["Lib"];

        var decls =
            ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>.Empty
            .Add(
                DeclQualifiedName.Create(moduleName, "Wrapped"),
                new SyntaxTypes.Declaration.CustomTypeDeclaration(ts));

        var registry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(decls);

        registry.Should().ContainKey(DeclQualifiedName.Create(moduleName, "Wrapped"));
        registry.Should().ContainKey(DeclQualifiedName.Create(moduleName, "MkWrapped"));

        registry[DeclQualifiedName.Create(moduleName, "Wrapped")].Should()
            .BeSameAs(registry[DeclQualifiedName.Create(moduleName, "MkWrapped")]);
    }

    [Fact]
    public void BuildNewtypeRegistry_skips_multi_constructor_types()
    {
        var ts =
            MakeTypeStruct(
                "Maybe",
                [
                ("Just", new[] { IntType() }),
                ("Nothing", System.Array.Empty<SyntaxTypes.TypeAnnotation>()),
                ],
                generics: ["a"]);

        var moduleName = (IReadOnlyList<string>)["Lib"];

        var decls =
            ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>.Empty
            .Add(
                DeclQualifiedName.Create(moduleName, "Maybe"),
                new SyntaxTypes.Declaration.CustomTypeDeclaration(ts));

        NewtypeWrapperAnalysis.BuildNewtypeRegistry(decls).Should().BeEmpty();
    }

    private static ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo>
        SingleEntryRegistry(IReadOnlyList<string> moduleName, string typeName, string ctorName)
    {
        var ts = MakeTypeStruct(typeName, [(ctorName, new[] { IntType() })], generics: ["a"]);

        var decls =
            ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>.Empty
            .Add(
                DeclQualifiedName.Create(moduleName, typeName),
                new SyntaxTypes.Declaration.CustomTypeDeclaration(ts));

        return NewtypeWrapperAnalysis.BuildNewtypeRegistry(decls);
    }

    private static SyntaxTypes.Expression FunctionOrValue(IReadOnlyList<string> moduleName, string name) =>
        new SyntaxTypes.Expression.FunctionOrValue(moduleName, name);

    [Fact]
    public void TryMatchWrapperReturnBody_matches_qualified_constructor_application()
    {
        var moduleName = (IReadOnlyList<string>)["Lib"];
        var registry = SingleEntryRegistry(moduleName, "Wrapped", "MkWrapped");

        // Body: Lib.MkWrapped innerExpr
        var inner = FunctionOrValue([], "innerExpr");

        var body =
            new SyntaxTypes.Expression.Application(
                [
                N(FunctionOrValue(moduleName, "MkWrapped")),
                N(inner),
                ]);

        var match =
            NewtypeWrapperAnalysis.TryMatchWrapperReturnBody(
                body,
                registry,
                currentModuleName: moduleName);

        match.Should().NotBeNull();
        match!.ConstructorName.Should().Be(DeclQualifiedName.Create(moduleName, "MkWrapped"));
        match.StrippedInnerExpression.Should().Be(inner);
    }

    [Fact]
    public void TryMatchWrapperReturnBody_matches_unqualified_constructor_in_same_module()
    {
        var moduleName = (IReadOnlyList<string>)["Lib"];
        var registry = SingleEntryRegistry(moduleName, "Wrapped", "MkWrapped");

        var inner = FunctionOrValue([], "innerExpr");

        var body =
            new SyntaxTypes.Expression.Application(
                [
                N(FunctionOrValue([], "MkWrapped")),
                N(inner),
                ]);

        var match =
            NewtypeWrapperAnalysis.TryMatchWrapperReturnBody(
                body,
                registry,
                currentModuleName: moduleName);

        match.Should().NotBeNull();
        match!.ConstructorName.Should().Be(DeclQualifiedName.Create(moduleName, "MkWrapped"));
    }

    [Fact]
    public void TryMatchWrapperReturnBody_peels_parentheses_around_outer_application()
    {
        var moduleName = (IReadOnlyList<string>)["Lib"];
        var registry = SingleEntryRegistry(moduleName, "Wrapped", "MkWrapped");

        var inner = FunctionOrValue([], "innerExpr");
        var inner2 = FunctionOrValue([], "innerExpr");

        var app =
            new SyntaxTypes.Expression.Application(
                [
                N<SyntaxTypes.Expression>(
                    new SyntaxTypes.Expression.ParenthesizedExpression(N(FunctionOrValue(moduleName, "MkWrapped")))),
                N<SyntaxTypes.Expression>(new SyntaxTypes.Expression.ParenthesizedExpression(N(inner))),
                ]);

        var body = new SyntaxTypes.Expression.ParenthesizedExpression(N<SyntaxTypes.Expression>(app));

        var match =
            NewtypeWrapperAnalysis.TryMatchWrapperReturnBody(
                body,
                registry,
                currentModuleName: moduleName);

        match.Should().NotBeNull();
        match!.StrippedInnerExpression.Should().Be(inner2);
    }

    [Fact]
    public void TryMatchWrapperReturnBody_returns_null_for_non_constructor_head()
    {
        var moduleName = (IReadOnlyList<string>)["Lib"];
        var registry = SingleEntryRegistry(moduleName, "Wrapped", "MkWrapped");

        // Body: someFn x  — head is a regular function, not a registered constructor
        var body =
            new SyntaxTypes.Expression.Application(
                [
                N(FunctionOrValue(moduleName, "someFn")),
                N(FunctionOrValue([], "x")),
                ]);

        NewtypeWrapperAnalysis.TryMatchWrapperReturnBody(
            body,
            registry,
            currentModuleName: moduleName).Should().BeNull();
    }

    [Fact]
    public void TryMatchWrapperReturnBody_returns_null_for_too_many_arguments()
    {
        var moduleName = (IReadOnlyList<string>)["Lib"];
        var registry = SingleEntryRegistry(moduleName, "Wrapped", "MkWrapped");

        // Body: MkWrapped x y  — over-saturated, would not type-check
        // but we should still refuse to match it.
        var body =
            new SyntaxTypes.Expression.Application(
                [
                N(FunctionOrValue(moduleName, "MkWrapped")),
                N(FunctionOrValue([], "x")),
                N(FunctionOrValue([], "y")),
                ]);

        NewtypeWrapperAnalysis.TryMatchWrapperReturnBody(
            body,
            registry,
            currentModuleName: moduleName).Should().BeNull();
    }

    [Fact]
    public void TryMatchWrapperReturnBody_returns_null_for_let_or_case_bodies()
    {
        // Step A intentionally does NOT recurse into Let / CaseExpression
        // arms — Step B handles that re-threading. Document that fact
        // here as a positive test.
        var moduleName = (IReadOnlyList<string>)["Lib"];
        var registry = SingleEntryRegistry(moduleName, "Wrapped", "MkWrapped");

        var caseBody =
            new SyntaxTypes.Expression.CaseExpression(
                new SyntaxTypes.CaseBlock(
                    Expression: N(FunctionOrValue([], "x")),
                    Cases: []));

        NewtypeWrapperAnalysis.TryMatchWrapperReturnBody(
            caseBody,
            registry,
            currentModuleName: moduleName).Should().BeNull();
    }

    [Fact]
    public void TryMatchWrapperReturnBody_returns_null_when_type_lookup_hits_but_constructor_does_not()
    {
        // Distinct type and ctor names — only a reference to the
        // constructor should match, not a reference to the type itself
        // (even though both share the registry).
        var moduleName = (IReadOnlyList<string>)["Lib"];
        var registry = SingleEntryRegistry(moduleName, "Wrapped", "MkWrapped");

        var body =
            new SyntaxTypes.Expression.Application(
                [
                N(FunctionOrValue(moduleName, "Wrapped")), // type name, NOT a value
                N(FunctionOrValue([], "x")),
                ]);

        NewtypeWrapperAnalysis.TryMatchWrapperReturnBody(
            body,
            registry,
            currentModuleName: moduleName).Should().BeNull();
    }
}
