using AwesomeAssertions;
using System.Collections.Generic;
using Xunit;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using ConcretizedTypes = Pine.Core.Elm.ElmSyntax.Stil4mConcretized;
using SyntaxV7 = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mElmSyntax7;

public class FromStil4mConcretizedTests
{
    /// <summary>
    /// Creates a SeparatedSyntaxList with the given items and separator locations.
    /// </summary>
    private static ConcretizedTypes.SeparatedSyntaxList<TNode> CreateSeparatedList<TNode>(
        TNode first,
        Location separatorLocation,
        params TNode[] rest)
    {
        var restList = new List<(Location SeparatorLocation, TNode Node)>();
        foreach (var item in rest)
        {
            restList.Add((separatorLocation, item));
        }

        return new ConcretizedTypes.SeparatedSyntaxList<TNode>.NonEmpty(
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

        var concretizedModule = new ConcretizedTypes.Module.NormalModule(
            moduleTokenLoc,
            new ConcretizedTypes.DefaultModuleData(
                new Node<ModuleName>(range, moduleName),
                exposingTokenLoc,
                new Node<ConcretizedTypes.Exposing>(range, new ConcretizedTypes.Exposing.All(range))
            )
        );

        var result = FromStil4mConcretized.Convert(concretizedModule);

        result.Should().BeOfType<Module.NormalModule>();
        var normalModule = (Module.NormalModule)result;
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

        var concretizedModule = new ConcretizedTypes.Module.PortModule(
            portTokenLoc,
            moduleTokenLoc,
            new ConcretizedTypes.DefaultModuleData(
                new Node<ModuleName>(range, moduleName),
                exposingTokenLoc,
                new Node<ConcretizedTypes.Exposing>(range, new ConcretizedTypes.Exposing.All(range))
            )
        );

        var result = FromStil4mConcretized.Convert(concretizedModule);

        result.Should().BeOfType<Module.PortModule>();
    }

    [Fact]
    public void Import_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var importLoc = new Location(1, 1);
        ModuleName moduleName = ["List"];

        var concretizedImport = new ConcretizedTypes.Import(
            importLoc,
            new Node<ModuleName>(range, moduleName),
            null,
            null
        );

        var result = FromStil4mConcretized.Convert(concretizedImport);

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

        var concretizedImport = new ConcretizedTypes.Import(
            importLoc,
            new Node<ModuleName>(range, moduleName),
            (asLoc, new Node<ModuleName>(range, aliasName)),
            null
        );

        var result = FromStil4mConcretized.Convert(concretizedImport);

        result.ModuleName.Value.Should().BeEquivalentTo(moduleName);
        result.ModuleAlias.Should().NotBeNull();
        result.ModuleAlias!.Value.Should().BeEquivalentTo(aliasName);
    }

    [Fact]
    public void Exposing_All_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 3));

        var concretizedExposing = new ConcretizedTypes.Exposing.All(range);

        var result = FromStil4mConcretized.Convert(concretizedExposing);

        result.Should().BeOfType<Exposing.All>();
        ((Exposing.All)result).Range.Should().Be(range);
    }

    [Fact]
    public void Exposing_Explicit_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var concretizedExposing = new ConcretizedTypes.Exposing.Explicit(
            [new Node<ConcretizedTypes.TopLevelExpose>(range, new ConcretizedTypes.TopLevelExpose.FunctionExpose("test"))]
        );

        var result = FromStil4mConcretized.Convert(concretizedExposing);

        result.Should().BeOfType<Exposing.Explicit>();
        var @explicit = (Exposing.Explicit)result;
        @explicit.Nodes.Should().HaveCount(1);
    }

    [Fact]
    public void TopLevelExpose_variants_convert_correctly()
    {
        var infix = new ConcretizedTypes.TopLevelExpose.InfixExpose("+");
        var infixResult = FromStil4mConcretized.Convert(infix);
        infixResult.Should().BeOfType<TopLevelExpose.InfixExpose>();

        var function = new ConcretizedTypes.TopLevelExpose.FunctionExpose("myFunc");
        var functionResult = FromStil4mConcretized.Convert(function);
        functionResult.Should().BeOfType<TopLevelExpose.FunctionExpose>();

        var typeOrAlias = new ConcretizedTypes.TopLevelExpose.TypeOrAliasExpose("MyType");
        var typeOrAliasResult = FromStil4mConcretized.Convert(typeOrAlias);
        typeOrAliasResult.Should().BeOfType<TopLevelExpose.TypeOrAliasExpose>();
    }

    [Fact]
    public void Expression_Integer_converts_correctly()
    {
        var concretizedExpr = new ConcretizedTypes.Expression.Integer(42);

        var result = FromStil4mConcretized.Convert(concretizedExpr);

        result.Should().BeOfType<SyntaxV7.Expression.Integer>();

        ((SyntaxV7.Expression.Integer)result).Value.Should().Be(42);
    }

    [Fact]
    public void Expression_Literal_converts_correctly()
    {
        var concretizedExpr = new ConcretizedTypes.Expression.Literal("hello", false);

        var result = FromStil4mConcretized.Convert(concretizedExpr);

        result.Should().BeOfType<SyntaxV7.Expression.Literal>();
        var literal = (SyntaxV7.Expression.Literal)result;
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

        var concretizedExpr = new ConcretizedTypes.Expression.IfBlock(
            ifLoc,
            new Node<ConcretizedTypes.Expression>(range, new ConcretizedTypes.Expression.Integer(1)),
            thenLoc,
            new Node<ConcretizedTypes.Expression>(range, new ConcretizedTypes.Expression.Integer(2)),
            elseLoc,
            new Node<ConcretizedTypes.Expression>(range, new ConcretizedTypes.Expression.Integer(3))
        );

        var result = FromStil4mConcretized.Convert(concretizedExpr);

        result.Should().BeOfType<SyntaxV7.Expression.IfBlock>();
        var ifBlock = (SyntaxV7.Expression.IfBlock)result;
        ifBlock.Condition.Value.Should().BeOfType<SyntaxV7.Expression.Integer>();
    }

    [Fact]
    public void Expression_ListExpr_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var commaLoc = new Location(1, 5);

        var concretizedExpr = new ConcretizedTypes.Expression.ListExpr(
            CreateSeparatedList(
                new Node<ConcretizedTypes.Expression>(range, new ConcretizedTypes.Expression.Integer(1)),
                commaLoc,
                new Node<ConcretizedTypes.Expression>(range, new ConcretizedTypes.Expression.Integer(2))
            )
        );

        var result = FromStil4mConcretized.Convert(concretizedExpr);

        result.Should().BeOfType<SyntaxV7.Expression.ListExpr>();
        var listExpr = (SyntaxV7.Expression.ListExpr)result;
        listExpr.Elements.Should().HaveCount(2);
    }

    [Fact]
    public void Expression_FunctionOrValue_converts_correctly()
    {
        ModuleName moduleName = ["List"];
        var concretizedExpr = new ConcretizedTypes.Expression.FunctionOrValue(moduleName, "map");

        var result = FromStil4mConcretized.Convert(concretizedExpr);

        result.Should().BeOfType<SyntaxV7.Expression.FunctionOrValue>();
        var functionOrValue = (SyntaxV7.Expression.FunctionOrValue)result;
        functionOrValue.ModuleName.Should().BeEquivalentTo(moduleName);
        functionOrValue.Name.Should().Be("map");
    }

    [Fact]
    public void Pattern_VarPattern_converts_correctly()
    {
        var concretizedPattern = new ConcretizedTypes.Pattern.VarPattern("x");

        var result = FromStil4mConcretized.Convert(concretizedPattern);

        result.Should().BeOfType<Pattern.VarPattern>();
        ((Pattern.VarPattern)result).Name.Should().Be("x");
    }

    [Fact]
    public void Pattern_TuplePattern_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var openLoc = new Location(1, 1);
        var closeLoc = new Location(1, 10);

        var concretizedPattern = new ConcretizedTypes.Pattern.TuplePattern(
            openLoc,
            [
                new Node<ConcretizedTypes.Pattern>(range, new ConcretizedTypes.Pattern.VarPattern("x")),
                new Node<ConcretizedTypes.Pattern>(range, new ConcretizedTypes.Pattern.VarPattern("y"))
            ],
            closeLoc
        );

        var result = FromStil4mConcretized.Convert(concretizedPattern);

        result.Should().BeOfType<Pattern.TuplePattern>();
        ((Pattern.TuplePattern)result).Elements.Should().HaveCount(2);
    }

    [Fact]
    public void TypeAnnotation_GenericType_converts_correctly()
    {
        var concretizedType = new ConcretizedTypes.TypeAnnotation.GenericType("a");

        var result = FromStil4mConcretized.Convert(concretizedType);

        result.Should().BeOfType<TypeAnnotation.GenericType>();
        ((TypeAnnotation.GenericType)result).Name.Should().Be("a");
    }

    [Fact]
    public void TypeAnnotation_Tupled_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var openLoc = new Location(1, 1);
        var closeLoc = new Location(1, 10);
        var commaLoc = new Location(1, 5);

        var concretizedType = new ConcretizedTypes.TypeAnnotation.Tupled(
            openLoc,
            new ConcretizedTypes.SeparatedSyntaxList<Node<ConcretizedTypes.TypeAnnotation>>.NonEmpty(
                new Node<ConcretizedTypes.TypeAnnotation>(range, new ConcretizedTypes.TypeAnnotation.GenericType("a")),
                [(commaLoc, new Node<ConcretizedTypes.TypeAnnotation>(range, new ConcretizedTypes.TypeAnnotation.GenericType("b")))]),
            closeLoc
        );

        var result = FromStil4mConcretized.Convert(concretizedType);

        result.Should().BeOfType<TypeAnnotation.Tupled>();
        ((TypeAnnotation.Tupled)result).TypeAnnotations.Should().HaveCount(2);
    }

    [Fact]
    public void LambdaStruct_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var backslashLoc = new Location(1, 1);
        var arrowLoc = new Location(1, 5);

        var concretizedLambda = new ConcretizedTypes.LambdaStruct(
            backslashLoc,
            [new Node<ConcretizedTypes.Pattern>(range, new ConcretizedTypes.Pattern.VarPattern("x"))],
            arrowLoc,
            new Node<ConcretizedTypes.Expression>(range, new ConcretizedTypes.Expression.Integer(42))
        );

        var result = FromStil4mConcretized.Convert(concretizedLambda);

        result.Arguments.Should().HaveCount(1);
        result.Expression.Value.Should().BeOfType<SyntaxV7.Expression.Integer>();
    }

    [Fact]
    public void CaseBlock_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var caseLoc = new Location(1, 1);
        var ofLoc = new Location(1, 5);
        var arrowLoc = new Location(1, 10);

        var concretizedCaseBlock = new ConcretizedTypes.CaseBlock(
            caseLoc,
            new Node<ConcretizedTypes.Expression>(range, new ConcretizedTypes.Expression.Integer(1)),
            ofLoc,
            [
                new ConcretizedTypes.Case(
                    new Node<ConcretizedTypes.Pattern>(range, new ConcretizedTypes.Pattern.VarPattern("x")),
                    arrowLoc,
                    new Node<ConcretizedTypes.Expression>(range, new ConcretizedTypes.Expression.Integer(42))
                )
            ]
        );

        var result = FromStil4mConcretized.Convert(concretizedCaseBlock);

        result.Cases.Should().HaveCount(1);
        result.Expression.Value.Should().BeOfType<SyntaxV7.Expression.Integer>();
    }

    [Fact]
    public void Declaration_FunctionDeclaration_converts_correctly()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var equalsLoc = new Location(1, 10);

        var concretizedDeclaration = new ConcretizedTypes.Declaration.FunctionDeclaration(
            new ConcretizedTypes.FunctionStruct(
                Documentation: null,
                Signature: null,
                Declaration: new Node<ConcretizedTypes.FunctionImplementation>(
                    range,
                    new ConcretizedTypes.FunctionImplementation(
                        new Node<string>(range, "test"),
                        [],
                        equalsLoc,
                        new Node<ConcretizedTypes.Expression>(range, new ConcretizedTypes.Expression.UnitExpr())
                    )
                )
            )
        );

        var result = FromStil4mConcretized.Convert(concretizedDeclaration);

        result.Should().BeOfType<Declaration.FunctionDeclaration>();
        var functionDecl = (Declaration.FunctionDeclaration)result;
        functionDecl.Function.Declaration.Value.Name.Value.Should().Be("test");
    }

    [Fact]
    public void QualifiedNameRef_converts_correctly()
    {
        ModuleName moduleName = ["Maybe"];
        var concretizedRef = new ConcretizedTypes.QualifiedNameRef(moduleName, "Just");

        var result = FromStil4mConcretized.Convert(concretizedRef);

        result.ModuleName.Should().BeEquivalentTo(moduleName);
        result.Name.Should().Be("Just");
    }
}
