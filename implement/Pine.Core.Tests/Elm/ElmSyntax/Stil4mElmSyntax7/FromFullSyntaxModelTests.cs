using AwesomeAssertions;
using System.Collections.Generic;
using Xunit;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using FullTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mElmSyntax7;

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

        return new SeparatedSyntaxList<TNode>.NonEmpty(
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

        var fullModule = new Module.NormalModule(
            moduleTokenLoc,
            new DefaultModuleData(
                new Node<ModuleName>(range, moduleName),
                exposingTokenLoc,
                new Node<Exposing>(range, new Exposing.All(range))
            )
        );

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullModule);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Module.NormalModule>();
        var normalModule = (Core.Elm.ElmSyntax.Stil4mElmSyntax7.Module.NormalModule)result;
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

        var fullModule = new Module.PortModule(
            portTokenLoc,
            moduleTokenLoc,
            new DefaultModuleData(
                new Node<ModuleName>(range, moduleName),
                exposingTokenLoc,
                new Node<Exposing>(range, new Exposing.All(range))
            )
        );

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullModule);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Module.PortModule>();
    }

    [Fact]
    public void Import_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var importLoc = new Location(1, 1);
        ModuleName moduleName = ["List"];

        var fullImport = new Import(
            importLoc,
            new Node<ModuleName>(range, moduleName),
            null,
            null
        );

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullImport);

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

        var fullImport = new Import(
            importLoc,
            new Node<ModuleName>(range, moduleName),
            (asLoc, new Node<ModuleName>(range, aliasName)),
            null
        );

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullImport);

        result.ModuleName.Value.Should().BeEquivalentTo(moduleName);
        result.ModuleAlias.Should().NotBeNull();
        result.ModuleAlias!.Value.Should().BeEquivalentTo(aliasName);
    }

    [Fact]
    public void Exposing_All_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 3));

        var fullExposing = new Exposing.All(range);

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullExposing);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Exposing.All>();
        ((Core.Elm.ElmSyntax.Stil4mElmSyntax7.Exposing.All)result).Range.Should().Be(range);
    }

    [Fact]
    public void Exposing_Explicit_converts_correctly()
    {
        var nodeRange = new Range(new Location(1, 2), new Location(1, 6));

        var node = new Node<TopLevelExpose>(nodeRange, new TopLevelExpose.FunctionExpose("test"));
        var nodesList = new SeparatedSyntaxList<Node<TopLevelExpose>>.NonEmpty(
            node,
            []);

        var fullExposing = new Exposing.Explicit(
            OpenParenLocation: new Location(1, 1),
            Nodes: nodesList,
            CloseParenLocation: new Location(1, 7)
        );

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullExposing);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Exposing.Explicit>();
        var @explicit = (Core.Elm.ElmSyntax.Stil4mElmSyntax7.Exposing.Explicit)result;
        @explicit.Nodes.Should().HaveCount(1);
    }

    [Fact]
    public void TopLevelExpose_variants_convert_correctly()
    {
        var infix = new TopLevelExpose.InfixExpose("+");
        var infixResult = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(infix);
        infixResult.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.TopLevelExpose.InfixExpose>();

        var function = new TopLevelExpose.FunctionExpose("myFunc");
        var functionResult = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(function);
        functionResult.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.TopLevelExpose.FunctionExpose>();

        var typeOrAlias = new TopLevelExpose.TypeOrAliasExpose("MyType");
        var typeOrAliasResult = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(typeOrAlias);
        typeOrAliasResult.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.TopLevelExpose.TypeOrAliasExpose>();
    }

    [Fact]
    public void Expression_Integer_converts_correctly()
    {
        var fullExpr = new FullTypes.Expression.Integer(42);

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullExpr);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.Integer>();

        ((Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.Integer)result).Value.Should().Be(42);
    }

    [Fact]
    public void Expression_Literal_converts_correctly()
    {
        var fullExpr = new FullTypes.Expression.Literal("hello", false);

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullExpr);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.Literal>();
        var literal = (Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.Literal)result;
        literal.Value.Should().Be("hello");
        literal.IsTripleQuoted.Should().BeFalse();
    }

    [Fact]
    public void Expression_IfBlock_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var ifLoc = new Location(1, 1);
        var thenLoc = new Location(1, 10);
        var elseLoc = new Location(1, 20);

        var fullExpr = new FullTypes.Expression.IfBlock(
            ifLoc,
            new Node<FullTypes.Expression>(range, new FullTypes.Expression.Integer(1)),
            thenLoc,
            new Node<FullTypes.Expression>(range, new FullTypes.Expression.Integer(2)),
            elseLoc,
            new Node<FullTypes.Expression>(range, new FullTypes.Expression.Integer(3))
        );

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullExpr);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.IfBlock>();
        var ifBlock = (Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.IfBlock)result;
        ifBlock.Condition.Value.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.Integer>();
    }

    [Fact]
    public void Expression_ListExpr_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var commaLoc = new Location(1, 5);

        var fullExpr = new FullTypes.Expression.ListExpr(
            CreateSeparatedList(
                new Node<FullTypes.Expression>(range, new FullTypes.Expression.Integer(1)),
                commaLoc,
                new Node<FullTypes.Expression>(range, new FullTypes.Expression.Integer(2))
            )
        );

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullExpr);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.ListExpr>();
        var listExpr = (Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.ListExpr)result;
        listExpr.Elements.Should().HaveCount(2);
    }

    [Fact]
    public void Expression_FunctionOrValue_converts_correctly()
    {
        ModuleName moduleName = ["List"];
        var fullExpr = new FullTypes.Expression.FunctionOrValue(moduleName, "map");

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullExpr);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.FunctionOrValue>();
        var functionOrValue = (Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.FunctionOrValue)result;
        functionOrValue.ModuleName.Should().BeEquivalentTo(moduleName);
        functionOrValue.Name.Should().Be("map");
    }

    [Fact]
    public void Pattern_VarPattern_converts_correctly()
    {
        var fullPattern = new Pattern.VarPattern("x");

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullPattern);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Pattern.VarPattern>();
        ((Core.Elm.ElmSyntax.Stil4mElmSyntax7.Pattern.VarPattern)result).Name.Should().Be("x");
    }

    [Fact]
    public void Pattern_TuplePattern_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var openLoc = new Location(1, 1);
        var closeLoc = new Location(1, 10);

        var firstElem = new Node<Pattern>(range, new Pattern.VarPattern("x"));
        var secondElem = new Node<Pattern>(range, new Pattern.VarPattern("y"));
        var elements = new SeparatedSyntaxList<Node<Pattern>>.NonEmpty(
            firstElem, [(new Location(1, 5), secondElem)]);

        var fullPattern = new Pattern.TuplePattern(elements);

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullPattern);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Pattern.TuplePattern>();
        ((Core.Elm.ElmSyntax.Stil4mElmSyntax7.Pattern.TuplePattern)result).Elements.Should().HaveCount(2);
    }

    [Fact]
    public void TypeAnnotation_GenericType_converts_correctly()
    {
        var fullType = new TypeAnnotation.GenericType("a");

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullType);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.TypeAnnotation.GenericType>();
        ((Core.Elm.ElmSyntax.Stil4mElmSyntax7.TypeAnnotation.GenericType)result).Name.Should().Be("a");
    }

    [Fact]
    public void TypeAnnotation_Tupled_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var commaLoc = new Location(1, 5);

        var fullType = new TypeAnnotation.Tupled(
            new SeparatedSyntaxList<Node<TypeAnnotation>>.NonEmpty(
                new Node<TypeAnnotation>(range, new TypeAnnotation.GenericType("a")),
                [(commaLoc, new Node<TypeAnnotation>(range, new TypeAnnotation.GenericType("b")))])
        );

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullType);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.TypeAnnotation.Tupled>();
        ((Core.Elm.ElmSyntax.Stil4mElmSyntax7.TypeAnnotation.Tupled)result).TypeAnnotations.Should().HaveCount(2);
    }

    [Fact]
    public void LambdaStruct_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var backslashLoc = new Location(1, 1);
        var arrowLoc = new Location(1, 5);

        var fullLambda = new LambdaStruct(
            backslashLoc,
            [new Node<Pattern>(range, new Pattern.VarPattern("x"))],
            arrowLoc,
            new Node<FullTypes.Expression>(range, new FullTypes.Expression.Integer(42))
        );

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullLambda);

        result.Arguments.Should().HaveCount(1);
        result.Expression.Value.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.Integer>();
    }

    [Fact]
    public void CaseBlock_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var caseLoc = new Location(1, 1);
        var ofLoc = new Location(1, 5);
        var arrowLoc = new Location(1, 10);

        var fullCaseBlock = new CaseBlock(
            caseLoc,
            new Node<FullTypes.Expression>(range, new FullTypes.Expression.Integer(1)),
            ofLoc,
            [
                new Case(
                    new Node<Pattern>(range, new Pattern.VarPattern("x")),
                    arrowLoc,
                    new Node<FullTypes.Expression>(range, new FullTypes.Expression.Integer(42))
                )
            ]
        );

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullCaseBlock);

        result.Cases.Should().HaveCount(1);
        result.Expression.Value.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.Integer>();
    }

    [Fact]
    public void Declaration_FunctionDeclaration_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var equalsLoc = new Location(1, 10);

        var fullDeclaration = new Declaration.FunctionDeclaration(
            new FunctionStruct(
                Documentation: null,
                Signature: null,
                Declaration: new Node<FunctionImplementation>(
                    range,
                    new FunctionImplementation(
                        new Node<string>(range, "test"),
                        [],
                        equalsLoc,
                        new Node<FullTypes.Expression>(range, new FullTypes.Expression.UnitExpr())
                    )
                )
            )
        );

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullDeclaration);

        result.Should().BeOfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Declaration.FunctionDeclaration>();
        var functionDecl = (Core.Elm.ElmSyntax.Stil4mElmSyntax7.Declaration.FunctionDeclaration)result;
        functionDecl.Function.Declaration.Value.Name.Value.Should().Be("test");
    }

    [Fact]
    public void QualifiedNameRef_converts_correctly()
    {
        ModuleName moduleName = ["Maybe"];
        var fullRef = new QualifiedNameRef(moduleName, "Just");

        var result = Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(fullRef);

        result.ModuleName.Should().BeEquivalentTo(moduleName);
        result.Name.Should().Be("Just");
    }
}
