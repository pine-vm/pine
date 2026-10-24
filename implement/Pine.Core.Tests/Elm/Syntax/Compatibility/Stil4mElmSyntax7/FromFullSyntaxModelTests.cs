using AwesomeAssertions;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7;
using Pine.Core.Elm.Syntax.Concrete;
using System.Collections.Generic;
using Xunit;

using FullTypes = Pine.Core.Elm.Syntax.Concrete;
using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Tests.Elm.Syntax.Compatibility.Stil4mElmSyntax7;

public class FromFullSyntaxModelTests
{
    /// <summary>
    /// Creates a SeparatedSyntaxList with the given items and separator locations.
    /// </summary>
    private static SeparatedSyntaxList<TNode> CreateSeparatedList<TNode>(
        TNode first,
        Location separatorLocation,
        params TNode[] rest)
    {
        var restList = new List<(Location SeparatorLocation, TNode Node)>();

        foreach (var item in rest)
        {
            restList.Add((separatorLocation, item));
        }

        return
            new SeparatedSyntaxList<TNode>.NonEmpty(
                First: first,
                Rest: restList);
    }

    [Fact]
    public void Module_NormalModule_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var moduleTokenLoc = new Location(1, 1);
        var exposingTokenLoc = new Location(1, 15);
        ModuleName moduleName = ["Test"];

        var fullModule =
            new FullTypes.Module.NormalModule(
                moduleTokenLoc,
                new FullTypes.DefaultModuleData(
                    new Node<ModuleName>(range, moduleName),
                    exposingTokenLoc,
                    new Node<FullTypes.Exposing>(range, new FullTypes.Exposing.All(range))));

        var result = FromFullSyntaxModel.Convert(fullModule);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Module.NormalModule>();
        var normalModule = (Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Module.NormalModule)result;
        normalModule.ModuleData.ModuleName.Value.Should().BeEquivalentTo(moduleName);
    }

    [Fact]
    public void Module_PortModule_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var portTokenLoc = new Location(1, 1);
        var moduleTokenLoc = new Location(1, 6);
        var exposingTokenLoc = new Location(1, 15);
        ModuleName moduleName = ["Test"];

        var fullModule =
            new FullTypes.Module.PortModule(
                portTokenLoc,
                moduleTokenLoc,
                new FullTypes.DefaultModuleData(
                    new Node<ModuleName>(range, moduleName),
                    exposingTokenLoc,
                    new Node<FullTypes.Exposing>(range, new FullTypes.Exposing.All(range))));

        var result = FromFullSyntaxModel.Convert(fullModule);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Module.PortModule>();
    }

    [Fact]
    public void Import_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var importLoc = new Location(1, 1);
        ModuleName moduleName = ["List"];

        var fullImport =
            new FullTypes.Import(
                importLoc,
                new Node<ModuleName>(range, moduleName),
                null,
                null);

        var result = FromFullSyntaxModel.Convert(fullImport);

        result.ModuleName.Value.Should().BeEquivalentTo(moduleName);
        result.ModuleAlias.Should().BeNull();
        result.ExposingList.Should().BeNull();
    }

    [Fact]
    public void Import_with_alias_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var importLoc = new Location(1, 1);
        var asLoc = new Location(1, 12);
        ModuleName moduleName = ["Platform", "Cmd"];
        ModuleName aliasName = ["Cmd"];

        var fullImport =
            new FullTypes.Import(
                importLoc,
                new Node<ModuleName>(range, moduleName),
                (asLoc, new Node<ModuleName>(range, aliasName)),
                null);

        var result = FromFullSyntaxModel.Convert(fullImport);

        result.ModuleName.Value.Should().BeEquivalentTo(moduleName);
        result.ModuleAlias.Should().NotBeNull();
        result.ModuleAlias!.Value.Should().BeEquivalentTo(aliasName);
    }

    [Fact]
    public void Exposing_All_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 3));

        var fullExposing = new FullTypes.Exposing.All(range);

        var result = FromFullSyntaxModel.Convert(fullExposing);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Exposing.All>();
        ((Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Exposing.All)result).Range.Should().Be(range);
    }

    [Fact]
    public void Exposing_Explicit_converts_correctly()
    {
        var nodeRange = new Range(new Location(1, 2), new Location(1, 6));

        var node = new Node<FullTypes.TopLevelExpose>(nodeRange, new FullTypes.TopLevelExpose.FunctionExpose("test"));

        var nodesList =
            new SeparatedSyntaxList<Node<FullTypes.TopLevelExpose>>.NonEmpty(
                node,
                []);

        var fullExposing =
            new FullTypes.Exposing.Explicit(
                OpenParenLocation: new Location(1, 1),
                Nodes: nodesList,
                CloseParenLocation: new Location(1, 7));

        var result = FromFullSyntaxModel.Convert(fullExposing);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Exposing.Explicit>();
        var @explicit = (Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Exposing.Explicit)result;
        @explicit.Nodes.Should().HaveCount(1);
    }

    [Fact]
    public void TopLevelExpose_variants_convert_correctly()
    {
        var infix = new FullTypes.TopLevelExpose.InfixExpose("+");
        var infixResult = FromFullSyntaxModel.Convert(infix);
        infixResult.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.TopLevelExpose.InfixExpose>();

        var function = new FullTypes.TopLevelExpose.FunctionExpose("myFunc");
        var functionResult = FromFullSyntaxModel.Convert(function);
        functionResult.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.TopLevelExpose.FunctionExpose>();

        var typeOrAlias = new FullTypes.TopLevelExpose.TypeOrAliasExpose("MyType");
        var typeOrAliasResult = FromFullSyntaxModel.Convert(typeOrAlias);

        typeOrAliasResult.Should()
            .BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.TopLevelExpose.TypeOrAliasExpose>();
    }

    [Fact]
    public void Expression_Integer_converts_correctly()
    {
        var fullExpr = new FullTypes.Expression.IntegerLiteral("42");

        var result = FromFullSyntaxModel.Convert(fullExpr);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.Integer>();

        ((Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.Integer)result).Value.Should().Be(42);
    }

    [Fact]
    public void Expression_Integer_hex_converts_correctly()
    {
        var fullExpr = new FullTypes.Expression.IntegerLiteral("0x49");

        var result = FromFullSyntaxModel.Convert(fullExpr);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.Hex>();

        ((Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.Hex)result).Value.Should().Be(73);
    }

    [Fact]
    public void Expression_Literal_converts_correctly()
    {
        var fullExpr = new FullTypes.Expression.StringLiteral("hello");

        var result = FromFullSyntaxModel.Convert(fullExpr);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.Literal>();
        var literal = (Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.Literal)result;
        literal.Value.Should().Be("hello");
        literal.IsTripleQuoted.Should().BeFalse();
    }

    [Fact]
    public void Expression_MultilineStringLiteral_converts_to_triple_quoted_literal()
    {
        var fullExpr =
            new FullTypes.Expression.MultilineStringLiteral("first\nsecond", SourceLines: ["first", "second"]);

        var result = FromFullSyntaxModel.Convert(fullExpr);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.Literal>();
        var literal = (Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.Literal)result;
        literal.Value.Should().Be("first\nsecond");
        literal.IsTripleQuoted.Should().BeTrue();
    }

    [Fact]
    public void Expression_IfBlock_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var ifLoc = new Location(1, 1);
        var thenLoc = new Location(1, 10);
        var elseLoc = new Location(1, 20);

        var fullExpr =
            new FullTypes.Expression.IfBlock(
                ifLoc,
                new Node<FullTypes.Expression>(range, new FullTypes.Expression.IntegerLiteral("1")),
                thenLoc,
                new Node<FullTypes.Expression>(range, new FullTypes.Expression.IntegerLiteral("2")),
                elseLoc,
                new Node<FullTypes.Expression>(range, new FullTypes.Expression.IntegerLiteral("3")));

        var result = FromFullSyntaxModel.Convert(fullExpr);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.IfBlock>();
        var ifBlock = (Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.IfBlock)result;
        ifBlock.Condition.Value.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.Integer>();
    }

    [Fact]
    public void Expression_ListExpr_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var commaLoc = new Location(1, 5);

        var fullExpr =
            new FullTypes.Expression.ListExpr(
                CreateSeparatedList(
                    new Node<FullTypes.Expression>(range, new FullTypes.Expression.IntegerLiteral("1")),
                    commaLoc,
                    new Node<FullTypes.Expression>(range, new FullTypes.Expression.IntegerLiteral("2"))));

        var result = FromFullSyntaxModel.Convert(fullExpr);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.ListExpr>();
        var listExpr = (Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.ListExpr)result;
        listExpr.Elements.Should().HaveCount(2);
    }


    [Fact]
    public void Expression_FunctionOrValue_converts_correctly()
    {
        ModuleName moduleName = ["List"];
        var fullExpr = new FullTypes.Expression.Identifier(moduleName, "map");

        var result = FromFullSyntaxModel.Convert(fullExpr);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.FunctionOrValue>();
        var functionOrValue = (Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.FunctionOrValue)result;
        functionOrValue.ModuleName.Should().BeEquivalentTo(moduleName);
        functionOrValue.Name.Should().Be("map");
    }

    [Fact]
    public void Pattern_VarPattern_converts_correctly()
    {
        var fullPattern = new FullTypes.Pattern.VarPattern("x");

        var result = FromFullSyntaxModel.Convert(fullPattern);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Pattern.VarPattern>();
        ((Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Pattern.VarPattern)result).Name.Should().Be("x");
    }

    [Fact]
    public void Pattern_TuplePattern_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var firstElem = new Node<FullTypes.Pattern>(range, new FullTypes.Pattern.VarPattern("x"));
        var secondElem = new Node<FullTypes.Pattern>(range, new FullTypes.Pattern.VarPattern("y"));

        var elements =
            new SeparatedSyntaxList<Node<FullTypes.Pattern>>.NonEmpty(
                firstElem,
                [(new Location(1, 5), secondElem)]);

        var fullPattern = new FullTypes.Pattern.TuplePattern(elements);

        var result = FromFullSyntaxModel.Convert(fullPattern);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Pattern.TuplePattern>();
        ((Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Pattern.TuplePattern)result).Elements.Should().HaveCount(2);
    }

    [Fact]
    public void TypeAnnotation_GenericType_converts_correctly()
    {
        var fullType = new FullTypes.TypeAnnotation.GenericType("a");

        var result = FromFullSyntaxModel.Convert(fullType);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.TypeAnnotation.GenericType>();
        ((Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.TypeAnnotation.GenericType)result).Name.Should().Be("a");
    }

    [Fact]
    public void TypeAnnotation_Tupled_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var commaLoc = new Location(1, 5);

        var fullType =
            new FullTypes.TypeAnnotation.Tupled(
                new SeparatedSyntaxList<Node<FullTypes.TypeAnnotation>>.NonEmpty(
                    new Node<FullTypes.TypeAnnotation>(range, new FullTypes.TypeAnnotation.GenericType("a")),
                    [
                    (commaLoc, new Node<FullTypes.TypeAnnotation>(range, new FullTypes.TypeAnnotation.GenericType("b")))
                    ]));

        var result = FromFullSyntaxModel.Convert(fullType);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.TypeAnnotation.Tupled>();

        ((Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.TypeAnnotation.Tupled)result).TypeAnnotations
            .Should().HaveCount(2);
    }

    [Fact]
    public void LambdaStruct_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var backslashLoc = new Location(1, 1);
        var arrowLoc = new Location(1, 5);

        var fullLambda =
            new FullTypes.LambdaStruct(
                backslashLoc,
                [new Node<FullTypes.Pattern>(range, new FullTypes.Pattern.VarPattern("x"))],
                arrowLoc,
                new Node<FullTypes.Expression>(range, new FullTypes.Expression.IntegerLiteral("42")));

        var result = FromFullSyntaxModel.Convert(fullLambda);

        result.Arguments.Should().HaveCount(1);
        result.Expression.Value.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.Integer>();
    }

    [Fact]
    public void CaseBlock_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var caseLoc = new Location(1, 1);
        var ofLoc = new Location(1, 5);
        var arrowLoc = new Location(1, 10);

        var fullCaseBlock =
            new FullTypes.CaseBlock(
                caseLoc,
                new Node<FullTypes.Expression>(range, new FullTypes.Expression.IntegerLiteral("1")),
                ofLoc,
                [
                new FullTypes.Case(
                    new Node<FullTypes.Pattern>(range, new FullTypes.Pattern.VarPattern("x")),
                    arrowLoc,
                    new Node<FullTypes.Expression>(range, new FullTypes.Expression.IntegerLiteral("42")))
                ]);

        var result = FromFullSyntaxModel.Convert(fullCaseBlock);

        result.Cases.Should().HaveCount(1);
        result.Expression.Value.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Expression.Integer>();
    }

    [Fact]
    public void Declaration_FunctionDeclaration_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var equalsLoc = new Location(1, 10);

        var fullDeclaration =
            new FullTypes.Declaration.FunctionDeclaration(
                new FullTypes.FunctionStruct(
                    Documentation: null,
                    Signature: null,
                    Declaration:
                    new Node<FullTypes.FunctionImplementation>(
                        range,
                        new FullTypes.FunctionImplementation(
                            new Node<string>(range, "test"),
                            [],
                            equalsLoc,
                            new Node<FullTypes.Expression>(range, new FullTypes.Expression.UnitExpr())))));

        var result = FromFullSyntaxModel.Convert(fullDeclaration);

        result.Should().BeOfType<Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Declaration.FunctionDeclaration>();
        var functionDecl = (Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7.Declaration.FunctionDeclaration)result;
        functionDecl.Function.Declaration.Value.Name.Value.Should().Be("test");
    }

    [Fact]
    public void QualifiedNameRef_converts_correctly()
    {
        ModuleName moduleName = ["Maybe"];
        var fullRef = new FullTypes.QualifiedNameRef(moduleName, "Just");

        var result = FromFullSyntaxModel.Convert(fullRef);

        result.ModuleName.Should().BeEquivalentTo(moduleName);
        result.Name.Should().Be("Just");
    }

    [Fact]
    public void Roundtrip_various_literals()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl =
                [ 71, 0x49 ]

            """";

        AssertPreservationDespiteConversionFromFullSyntaxModel(elmModuleText);
    }

    private static void AssertPreservationDespiteConversionFromFullSyntaxModel(string moduleText)
    {
        var parsedFile =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new System.Exception(err.ToString()));

        AssertPreservationDespiteConversionFromFullSyntaxModel(parsedFile);
    }

    private static void AssertPreservationDespiteConversionFromFullSyntaxModel(FullTypes.File file)
    {
        // Use Avh4Format.FormatToString for both to ensure consistent formatting.
        // ToFullSyntaxModel sets default locations, so formatting is needed before rendering.
        var originalRendered = Core.Elm.Syntax.Avh4Format.FormatToString(file);

        var converted =
            FromFullSyntaxModel.Convert(file);

        var convertedBack =
            ToFullSyntaxModel.Convert(converted);

        var convertedRendered = Core.Elm.Syntax.Avh4Format.FormatToString(convertedBack);

        convertedRendered.Trim().Should().Be(originalRendered.Trim());
    }
}
