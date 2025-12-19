using AwesomeAssertions;
using System.Collections.Generic;
using Xunit;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mConcretized;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mConcretized;

public class TypesTests
{
    /// <summary>
    /// Creates a SeparatedSyntaxList from items with the given separator location.
    /// </summary>
    private static SyntaxTypes.SeparatedSyntaxList<TNode> CreateSeparatedList<TNode>(
        TNode first,
        Location separatorLocation,
        params TNode[] rest)
    {
        var restList = new List<(Location SeparatorLocation, TNode Node)>();
        foreach (var item in rest)
        {
            restList.Add((separatorLocation, item));
        }

        return new SyntaxTypes.SeparatedSyntaxList<TNode>.NonEmpty(
            First: first,
            Rest: restList);
    }

    [Fact]
    public void SeparatedSyntaxList_Empty_value_equality()
    {
        var list1 = new SyntaxTypes.SeparatedSyntaxList<string>.Empty();
        var list2 = new SyntaxTypes.SeparatedSyntaxList<string>.Empty();

        list1.Should().Be(list2);
        list1.GetHashCode().Should().Be(list2.GetHashCode());
    }

    [Fact]
    public void SeparatedSyntaxList_NonEmpty_value_equality()
    {
        var location = new Location(1, 5);

        var list1 = new SyntaxTypes.SeparatedSyntaxList<string>.NonEmpty(
            "first",
            [(location, "second")]
        );
        var list2 = new SyntaxTypes.SeparatedSyntaxList<string>.NonEmpty(
            "first",
            [(location, "second")]
        );

        list1.Should().Be(list2);
        list1.GetHashCode().Should().Be(list2.GetHashCode());
    }

    [Fact]
    public void Import_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var importLoc = new Location(1, 1);
        ModuleName moduleName = ["List"];

        var import1 = new SyntaxTypes.Import(
            importLoc,
            new Node<ModuleName>(range, moduleName),
            null,
            null
        );
        var import2 = new SyntaxTypes.Import(
            importLoc,
            new Node<ModuleName>(range, moduleName),
            null,
            null
        );

        import1.Should().Be(import2);
        import1.GetHashCode().Should().Be(import2.GetHashCode());
    }

    [Fact]
    public void Module_NormalModule_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var moduleTokenLoc = new Location(1, 1);
        var exposingTokenLoc = new Location(1, 15);
        ModuleName moduleName = ["Test"];

        var moduleData1 = new SyntaxTypes.DefaultModuleData(
            new Node<ModuleName>(range, moduleName),
            exposingTokenLoc,
            new Node<SyntaxTypes.Exposing>(range, new SyntaxTypes.Exposing.All(range))
        );
        var moduleData2 = new SyntaxTypes.DefaultModuleData(
            new Node<ModuleName>(range, moduleName),
            exposingTokenLoc,
            new Node<SyntaxTypes.Exposing>(range, new SyntaxTypes.Exposing.All(range))
        );

        var module1 = new SyntaxTypes.Module.NormalModule(moduleTokenLoc, moduleData1);
        var module2 = new SyntaxTypes.Module.NormalModule(moduleTokenLoc, moduleData2);

        module1.Should().Be(module2);
        module1.GetHashCode().Should().Be(module2.GetHashCode());
    }

    [Fact]
    public void Module_PortModule_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var portTokenLoc = new Location(1, 1);
        var moduleTokenLoc = new Location(1, 6);
        var exposingTokenLoc = new Location(1, 15);
        ModuleName moduleName = ["Test"];

        var moduleData1 = new SyntaxTypes.DefaultModuleData(
            new Node<ModuleName>(range, moduleName),
            exposingTokenLoc,
            new Node<SyntaxTypes.Exposing>(range, new SyntaxTypes.Exposing.All(range))
        );
        var moduleData2 = new SyntaxTypes.DefaultModuleData(
            new Node<ModuleName>(range, moduleName),
            exposingTokenLoc,
            new Node<SyntaxTypes.Exposing>(range, new SyntaxTypes.Exposing.All(range))
        );

        var module1 = new SyntaxTypes.Module.PortModule(portTokenLoc, moduleTokenLoc, moduleData1);
        var module2 = new SyntaxTypes.Module.PortModule(portTokenLoc, moduleTokenLoc, moduleData2);

        module1.Should().Be(module2);
        module1.GetHashCode().Should().Be(module2.GetHashCode());
    }

    [Fact]
    public void Exposing_All_value_equality()
    {
        var range1 = new Range(new Location(1, 1), new Location(1, 3));
        var range2 = new Range(new Location(1, 1), new Location(1, 3));

        var exposing1 = new SyntaxTypes.Exposing.All(range1);
        var exposing2 = new SyntaxTypes.Exposing.All(range2);

        exposing1.Should().Be(exposing2);
        exposing1.GetHashCode().Should().Be(exposing2.GetHashCode());
    }

    [Fact]
    public void Exposing_Explicit_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var exposing1 = new SyntaxTypes.Exposing.Explicit(
            [new Node<SyntaxTypes.TopLevelExpose>(range, new SyntaxTypes.TopLevelExpose.FunctionExpose("test"))]
        );
        var exposing2 = new SyntaxTypes.Exposing.Explicit(
            [new Node<SyntaxTypes.TopLevelExpose>(range, new SyntaxTypes.TopLevelExpose.FunctionExpose("test"))]
        );

        exposing1.Should().Be(exposing2);
        exposing1.GetHashCode().Should().Be(exposing2.GetHashCode());
    }

    [Fact]
    public void TopLevelExpose_FunctionExpose_value_equality()
    {
        var expose1 = new SyntaxTypes.TopLevelExpose.FunctionExpose("myFunction");
        var expose2 = new SyntaxTypes.TopLevelExpose.FunctionExpose("myFunction");

        expose1.Should().Be(expose2);
        expose1.GetHashCode().Should().Be(expose2.GetHashCode());
    }

    [Fact]
    public void TopLevelExpose_InfixExpose_value_equality()
    {
        var expose1 = new SyntaxTypes.TopLevelExpose.InfixExpose("+");
        var expose2 = new SyntaxTypes.TopLevelExpose.InfixExpose("+");

        expose1.Should().Be(expose2);
        expose1.GetHashCode().Should().Be(expose2.GetHashCode());
    }

    [Fact]
    public void ExposedType_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 5));

        var type1 = new SyntaxTypes.ExposedType("Maybe", range);
        var type2 = new SyntaxTypes.ExposedType("Maybe", range);

        type1.Should().Be(type2);
        type1.GetHashCode().Should().Be(type2.GetHashCode());
    }

    [Fact]
    public void QualifiedNameRef_value_equality()
    {
        ModuleName moduleName1 = ["Maybe"];
        ModuleName moduleName2 = ["Maybe"];

        var ref1 = new SyntaxTypes.QualifiedNameRef(moduleName1, "Just");
        var ref2 = new SyntaxTypes.QualifiedNameRef(moduleName2, "Just");

        ref1.Should().Be(ref2);
        ref1.GetHashCode().Should().Be(ref2.GetHashCode());
    }

    [Fact]
    public void Expression_UnitExpr_value_equality()
    {
        var expr1 = new SyntaxTypes.Expression.UnitExpr();
        var expr2 = new SyntaxTypes.Expression.UnitExpr();

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }

    [Fact]
    public void Expression_Integer_value_equality()
    {
        var expr1 = new SyntaxTypes.Expression.Integer(42);
        var expr2 = new SyntaxTypes.Expression.Integer(42);

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }

    [Fact]
    public void Expression_Literal_value_equality()
    {
        var expr1 = new SyntaxTypes.Expression.Literal("hello");
        var expr2 = new SyntaxTypes.Expression.Literal("hello");

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }

    [Fact]
    public void Expression_IfBlock_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var ifLoc = new Location(1, 1);
        var thenLoc = new Location(1, 10);
        var elseLoc = new Location(1, 20);

        var condition = new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1));
        var thenExpr = new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(2));
        var elseExpr = new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(3));

        var expr1 = new SyntaxTypes.Expression.IfBlock(ifLoc, condition, thenLoc, thenExpr, elseLoc, elseExpr);
        var expr2 = new SyntaxTypes.Expression.IfBlock(ifLoc, condition, thenLoc, thenExpr, elseLoc, elseExpr);

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }

    [Fact]
    public void Expression_ListExpr_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var commaLoc = new Location(1, 5);

        var expr1 = new SyntaxTypes.Expression.ListExpr(
            CreateSeparatedList(
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
                commaLoc,
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(2))
            )
        );
        var expr2 = new SyntaxTypes.Expression.ListExpr(
            CreateSeparatedList(
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
                commaLoc,
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(2))
            )
        );

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }

    [Fact]
    public void Expression_TupledExpression_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var openLoc = new Location(1, 1);
        var closeLoc = new Location(1, 10);
        var commaLoc = new Location(1, 5);

        var expr1 = new SyntaxTypes.Expression.TupledExpression(
            openLoc,
            CreateSeparatedList(
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
                commaLoc,
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(2))
            ),
            closeLoc
        );
        var expr2 = new SyntaxTypes.Expression.TupledExpression(
            openLoc,
            CreateSeparatedList(
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
                commaLoc,
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(2))
            ),
            closeLoc
        );

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }

    [Fact]
    public void TypeAnnotation_GenericType_value_equality()
    {
        var type1 = new SyntaxTypes.TypeAnnotation.GenericType("a");
        var type2 = new SyntaxTypes.TypeAnnotation.GenericType("a");

        type1.Should().Be(type2);
        type1.GetHashCode().Should().Be(type2.GetHashCode());
    }

    [Fact]
    public void TypeAnnotation_Unit_value_equality()
    {
        var type1 = new SyntaxTypes.TypeAnnotation.Unit();
        var type2 = new SyntaxTypes.TypeAnnotation.Unit();

        type1.Should().Be(type2);
        type1.GetHashCode().Should().Be(type2.GetHashCode());
    }

    [Fact]
    public void TypeAnnotation_Tupled_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var openLoc = new Location(1, 1);
        var closeLoc = new Location(1, 10);

        var typeAnnotations = new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.TypeAnnotation>>.NonEmpty(
            new Node<SyntaxTypes.TypeAnnotation>(range, new SyntaxTypes.TypeAnnotation.GenericType("a")),
            []);

        var type1 = new SyntaxTypes.TypeAnnotation.Tupled(
            openLoc,
            typeAnnotations,
            closeLoc
        );
        var type2 = new SyntaxTypes.TypeAnnotation.Tupled(
            openLoc,
            typeAnnotations,
            closeLoc
        );

        type1.Should().Be(type2);
        type1.GetHashCode().Should().Be(type2.GetHashCode());
    }

    [Fact]
    public void Pattern_VarPattern_value_equality()
    {
        var pattern1 = new SyntaxTypes.Pattern.VarPattern("x");
        var pattern2 = new SyntaxTypes.Pattern.VarPattern("x");

        pattern1.Should().Be(pattern2);
        pattern1.GetHashCode().Should().Be(pattern2.GetHashCode());
    }

    [Fact]
    public void Pattern_AllPattern_value_equality()
    {
        var pattern1 = new SyntaxTypes.Pattern.AllPattern();
        var pattern2 = new SyntaxTypes.Pattern.AllPattern();

        pattern1.Should().Be(pattern2);
        pattern1.GetHashCode().Should().Be(pattern2.GetHashCode());
    }

    [Fact]
    public void Pattern_TuplePattern_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var openLoc = new Location(1, 1);
        var closeLoc = new Location(1, 10);

        var pattern1 = new SyntaxTypes.Pattern.TuplePattern(
            openLoc,
            [new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x"))],
            closeLoc
        );
        var pattern2 = new SyntaxTypes.Pattern.TuplePattern(
            openLoc,
            [new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x"))],
            closeLoc
        );

        pattern1.Should().Be(pattern2);
        pattern1.GetHashCode().Should().Be(pattern2.GetHashCode());
    }

    [Fact]
    public void Pattern_ListPattern_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var openLoc = new Location(1, 1);
        var closeLoc = new Location(1, 10);

        var pattern1 = new SyntaxTypes.Pattern.ListPattern(
            openLoc,
            [new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x"))],
            closeLoc
        );
        var pattern2 = new SyntaxTypes.Pattern.ListPattern(
            openLoc,
            [new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x"))],
            closeLoc
        );

        pattern1.Should().Be(pattern2);
        pattern1.GetHashCode().Should().Be(pattern2.GetHashCode());
    }

    [Fact]
    public void Pattern_RecordPattern_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var openLoc = new Location(1, 1);
        var closeLoc = new Location(1, 10);

        var pattern1 = new SyntaxTypes.Pattern.RecordPattern(
            openLoc,
            [new Node<string>(range, "field1")],
            closeLoc
        );
        var pattern2 = new SyntaxTypes.Pattern.RecordPattern(
            openLoc,
            [new Node<string>(range, "field1")],
            closeLoc
        );

        pattern1.Should().Be(pattern2);
        pattern1.GetHashCode().Should().Be(pattern2.GetHashCode());
    }

    [Fact]
    public void LambdaStruct_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var backslashLoc = new Location(1, 1);
        var arrowLoc = new Location(1, 5);

        var lambda1 = new SyntaxTypes.LambdaStruct(
            backslashLoc,
            [new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x"))],
            arrowLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))
        );
        var lambda2 = new SyntaxTypes.LambdaStruct(
            backslashLoc,
            [new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x"))],
            arrowLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))
        );

        lambda1.Should().Be(lambda2);
        lambda1.GetHashCode().Should().Be(lambda2.GetHashCode());
    }

    [Fact]
    public void CaseBlock_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var caseLoc = new Location(1, 1);
        var ofLoc = new Location(1, 5);
        var arrowLoc = new Location(1, 10);

        var caseItem = new SyntaxTypes.Case(
            new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x")),
            arrowLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))
        );

        var block1 = new SyntaxTypes.CaseBlock(
            caseLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
            ofLoc,
            [caseItem]
        );
        var block2 = new SyntaxTypes.CaseBlock(
            caseLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
            ofLoc,
            [caseItem]
        );

        block1.Should().Be(block2);
        block1.GetHashCode().Should().Be(block2.GetHashCode());
    }

    [Fact]
    public void RecordDefinition_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var colonLoc = new Location(1, 5);

        var field = new SyntaxTypes.RecordField(
            new Node<string>(range, "name"),
            colonLoc,
            new Node<SyntaxTypes.TypeAnnotation>(range, new SyntaxTypes.TypeAnnotation.GenericType("String"))
        );

        var def1 = new SyntaxTypes.RecordDefinition(
            new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.RecordField>>.NonEmpty(
                new Node<SyntaxTypes.RecordField>(range, field),
                []));
        var def2 = new SyntaxTypes.RecordDefinition(
            new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.RecordField>>.NonEmpty(
                new Node<SyntaxTypes.RecordField>(range, field),
                []));

        def1.Should().Be(def2);
        def1.GetHashCode().Should().Be(def2.GetHashCode());
    }

    [Fact]
    public void ValueConstructor_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var ctor1 = new SyntaxTypes.ValueConstructor(
            new Node<string>(range, "Just"),
            [new Node<SyntaxTypes.TypeAnnotation>(range, new SyntaxTypes.TypeAnnotation.GenericType("a"))]
        );
        var ctor2 = new SyntaxTypes.ValueConstructor(
            new Node<string>(range, "Just"),
            [new Node<SyntaxTypes.TypeAnnotation>(range, new SyntaxTypes.TypeAnnotation.GenericType("a"))]
        );

        ctor1.Should().Be(ctor2);
        ctor1.GetHashCode().Should().Be(ctor2.GetHashCode());
    }

    [Fact]
    public void TypeAlias_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var typeLoc = new Location(1, 1);
        var aliasLoc = new Location(1, 6);
        var equalsLoc = new Location(1, 15);

        var alias1 = new SyntaxTypes.TypeAlias(
            null,
            typeLoc,
            aliasLoc,
            new Node<string>(range, "MyAlias"),
            [],
            equalsLoc,
            new Node<SyntaxTypes.TypeAnnotation>(range, new SyntaxTypes.TypeAnnotation.Unit())
        );
        var alias2 = new SyntaxTypes.TypeAlias(
            null,
            typeLoc,
            aliasLoc,
            new Node<string>(range, "MyAlias"),
            [],
            equalsLoc,
            new Node<SyntaxTypes.TypeAnnotation>(range, new SyntaxTypes.TypeAnnotation.Unit())
        );

        alias1.Should().Be(alias2);
        alias1.GetHashCode().Should().Be(alias2.GetHashCode());
    }

    [Fact]
    public void TypeStruct_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var typeLoc = new Location(1, 1);
        var equalsLoc = new Location(1, 15);

        var type1 = new SyntaxTypes.TypeStruct(
            null,
            typeLoc,
            new Node<string>(range, "MyType"),
            [],
            equalsLoc,
            []
        );
        var type2 = new SyntaxTypes.TypeStruct(
            null,
            typeLoc,
            new Node<string>(range, "MyType"),
            [],
            equalsLoc,
            []
        );

        type1.Should().Be(type2);
        type1.GetHashCode().Should().Be(type2.GetHashCode());
    }

    [Fact]
    public void FunctionImplementation_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var equalsLoc = new Location(1, 10);

        var impl1 = new SyntaxTypes.FunctionImplementation(
            new Node<string>(range, "test"),
            [],
            equalsLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.UnitExpr())
        );
        var impl2 = new SyntaxTypes.FunctionImplementation(
            new Node<string>(range, "test"),
            [],
            equalsLoc,
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.UnitExpr())
        );

        impl1.Should().Be(impl2);
        impl1.GetHashCode().Should().Be(impl2.GetHashCode());
    }

    [Fact]
    public void Infix_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var infixLoc = new Location(1, 1);
        var equalsLoc = new Location(1, 15);

        var infix1 = new SyntaxTypes.Infix(
            infixLoc,
            new Node<InfixDirection>(range, InfixDirection.Left),
            new Node<int>(range, 4),
            new Node<string>(range, "+"),
            equalsLoc,
            new Node<string>(range, "add")
        );
        var infix2 = new SyntaxTypes.Infix(
            infixLoc,
            new Node<InfixDirection>(range, InfixDirection.Left),
            new Node<int>(range, 4),
            new Node<string>(range, "+"),
            equalsLoc,
            new Node<string>(range, "add")
        );

        infix1.Should().Be(infix2);
        infix1.GetHashCode().Should().Be(infix2.GetHashCode());
    }

    [Fact]
    public void Signature_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var colonLoc = new Location(1, 5);

        var sig1 = new SyntaxTypes.Signature(
            new Node<string>(range, "myFunc"),
            colonLoc,
            new Node<SyntaxTypes.TypeAnnotation>(range, new SyntaxTypes.TypeAnnotation.Unit())
        );
        var sig2 = new SyntaxTypes.Signature(
            new Node<string>(range, "myFunc"),
            colonLoc,
            new Node<SyntaxTypes.TypeAnnotation>(range, new SyntaxTypes.TypeAnnotation.Unit())
        );

        sig1.Should().Be(sig2);
        sig1.GetHashCode().Should().Be(sig2.GetHashCode());
    }

    [Fact]
    public void Expression_FunctionOrValue_value_equality()
    {
        ModuleName moduleName1 = ["List"];
        ModuleName moduleName2 = ["List"];

        var expr1 = new SyntaxTypes.Expression.FunctionOrValue(moduleName1, "map");
        var expr2 = new SyntaxTypes.Expression.FunctionOrValue(moduleName2, "map");

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }

    [Fact]
    public void Expression_RecordExpr_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 20));
        var equalsLoc = new Location(1, 10);

        var fieldName = new Node<string>(range, "name");
        var valueExpr = new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Literal("test"));
        var field = new SyntaxTypes.RecordExprField(fieldName, equalsLoc, valueExpr);

        var fieldsList = new SyntaxTypes.SeparatedSyntaxList<SyntaxTypes.RecordExprField>.NonEmpty(
            field, []);

        var expr1 = new SyntaxTypes.Expression.RecordExpr(fieldsList);
        var expr2 = new SyntaxTypes.Expression.RecordExpr(fieldsList);

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }

    [Fact]
    public void Expression_RecordUpdateExpression_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 20));
        var pipeLoc = new Location(1, 10);
        var equalsLoc = new Location(1, 12);

        var fieldName = new Node<string>(range, "name");
        var valueExpr = new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Literal("test"));
        var field = new SyntaxTypes.RecordExprField(fieldName, equalsLoc, valueExpr);

        var fieldsList = new SyntaxTypes.SeparatedSyntaxList<SyntaxTypes.RecordExprField>.NonEmpty(
            field, []);

        var expr1 = new SyntaxTypes.Expression.RecordUpdateExpression(
            new Node<string>(range, "record"),
            pipeLoc,
            fieldsList);
        var expr2 = new SyntaxTypes.Expression.RecordUpdateExpression(
            new Node<string>(range, "record"),
            pipeLoc,
            fieldsList);

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }
}
