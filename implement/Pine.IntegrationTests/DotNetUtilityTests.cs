using AwesomeAssertions;
using Microsoft.CodeAnalysis.CSharp;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests;

public class DotNetUtilityTests
{
    public static readonly PineValue Value_299b7decef = StringEncoding.ValueFromString("List");

    public static readonly PineValue Value_d597fb92e5 = PineValue.List([Value_299b7decef, PineValue.EmptyList]);

    [Fact]
    public void Test_sort_pine_value_for_declaration()
    {
        Value_d597fb92e5.ContainsInListTransitive(Value_299b7decef).Should().BeTrue(
            "value should contain value_299b7decef in list transitive");

        var listBeforeOrdering =
            new[]
            {
                StringEncoding.ValueFromString("Err"),
                Value_d597fb92e5,
                StringEncoding.ValueFromString("Ok"),
                Value_299b7decef
            };

        var listWithHashes =
            listBeforeOrdering
            .Select(value => (value, hash: System.Convert.ToHexStringLower(PineValueHashTree.ComputeHash(value).Span)))
            .ToImmutableList();

        var orderedValues =
            CSharpDeclarationOrder.OrderValuesForDeclaration(listBeforeOrdering)
            .ToImmutableList();

        orderedValues.Should().BeEquivalentTo(
            [
                StringEncoding.ValueFromString("Ok"),
                StringEncoding.ValueFromString("Err"),
                Value_299b7decef,
                Value_d597fb92e5
            ],
            options => options.WithStrictOrdering());
    }

    [Fact]
    public void Test_compile_syntax_for_type_declared_in_type()
    {
        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(EvalExprDelegate),
                usings: []);

        syntax.ToFullString().Should().Be("Pine.Core.Runtime.EvalExprDelegate");
    }

    [Fact]
    public void Test_compile_syntax_for_generic_type()
    {
        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(Result<string, int>),
                usings: []);

        syntax.ToFullString().Should().Be("Pine.Core.Functional.Result<System.String,System.Int32>");
    }

    [Fact]
    public void Test_compile_syntax_for_generic_IReadOnlyDictionary()
    {
        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(IReadOnlyDictionary<PineValue, string>),
                usings: []);

        syntax.ToFullString().Should().Be(
            "System.Collections.Generic.IReadOnlyDictionary<Pine.Core.PineLanguage.PineValue,System.String>");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_with_using_alias()
    {
        // Create a using alias: using MyDict = System.Collections.Generic.Dictionary<string, int>;
        var usingAlias =
            SyntaxFactory.UsingDirective(
                alias: SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName("MyDict")),
                name: SyntaxFactory.ParseName("System.Collections.Generic.Dictionary"));

        var context = new DeclarationSyntaxContext([usingAlias]);

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(Dictionary<string, int>),
                context);

        syntax.ToFullString().Should().Be("MyDict<System.String,System.Int32>");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_with_using_alias_with_global_prefix()
    {
        // Create a using alias: using MyDict = global::System.Collections.Generic.Dictionary<string, int>;
        var usingAlias =
            SyntaxFactory.UsingDirective(
                alias: SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName("MyDict")),
                name: SyntaxFactory.ParseName("global::System.Collections.Generic.Dictionary"));

        var context = new DeclarationSyntaxContext([usingAlias]);

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(Dictionary<string, int>),
                context);

        syntax.ToFullString().Should().Be("MyDict<System.String,System.Int32>");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_with_current_namespace_shortening()
    {
        var context =
            new DeclarationSyntaxContext(
                UsingDirectives: [],
                CurrentNamespace: "Pine.Core.PineLanguage");

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(PineValue),
                context);

        syntax.ToFullString().Should().Be("PineValue");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_with_current_namespace_partial_shortening()
    {
        var context =
            new DeclarationSyntaxContext(
                UsingDirectives: [],
                CurrentNamespace: "Pine");

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(PineValue),
                context);

        syntax.ToFullString().Should().Be("Core.PineLanguage.PineValue");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_with_no_shortening_when_different_namespace()
    {
        var context =
            new DeclarationSyntaxContext(
                UsingDirectives: [],
                CurrentNamespace: "SomeOther.Namespace");

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(PineValue),
                context);

        syntax.ToFullString().Should().Be("Pine.Core.PineLanguage.PineValue");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_using_directive_takes_precedence_over_namespace()
    {
        var usingDirective =
            SyntaxFactory.UsingDirective(
                SyntaxFactory.ParseName("Pine.Core.PineLanguage"));

        var context =
            new DeclarationSyntaxContext(
                UsingDirectives: [usingDirective],
                CurrentNamespace: "DifferentNamespace");

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(PineValue),
                context);

        syntax.ToFullString().Should().Be("PineValue");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_generic_type_with_namespace_shortening()
    {
        var context =
            new DeclarationSyntaxContext(
                UsingDirectives: [],
                CurrentNamespace: "Pine.Core");

        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(Result<string, PineValue>),
                context);

        syntax.ToFullString().Should().Be("Functional.Result<System.String,PineLanguage.PineValue>");
    }

    [Fact]
    public void Test_TypeSyntaxFromType_backwards_compatibility()
    {
        // Test that the old method signature still works
        var syntax =
            CompileTypeSyntax.TypeSyntaxFromType(
                typeof(PineValue),
                usings: []);

        syntax.ToFullString().Should().Be("Pine.Core.PineLanguage.PineValue");
    }

    [Fact]
    public void Test_ShortestRelativeNamespace_with_current_namespace_shortening()
    {
        var context =
            new DeclarationSyntaxContext(
                UsingDirectives: [],
                CurrentNamespace: "Pine.Core");

        var result =
            CompileTypeSyntax.ShortestRelativeNamespace(
                "Pine.Core.CodeAnalysis.SomeType",
                context);

        result.Should().BeEquivalentTo(["CodeAnalysis", "SomeType"]);
    }

    [Fact]
    public void Test_ShortestRelativeNamespace_backwards_compatibility()
    {
        // Test that the old method signature still works
        var result =
            CompileTypeSyntax.ShortestRelativeNamespace(
                "Pine.Core",
                usings: []);

        result.Should().BeEquivalentTo(["Pine", "Core"]);
    }


    [Fact]
    public void Test_ExprMappedToParentEnv_PathInParentEnv_equality()
    {
        new ExprMappedToParentEnv.PathInParentEnv([])
            .Should().Be(new ExprMappedToParentEnv.PathInParentEnv([]));

        new ExprMappedToParentEnv.PathInParentEnv([1, 3])
            .Should().Be(new ExprMappedToParentEnv.PathInParentEnv([1, 3]));
    }
}
