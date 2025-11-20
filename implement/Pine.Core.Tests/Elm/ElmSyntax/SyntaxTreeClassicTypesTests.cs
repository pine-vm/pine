using AwesomeAssertions;
using Xunit;
using Pine.Core.Elm.ElmSyntax.SyntaxTreeClassic;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxTreeClassic;

namespace Pine.Core.Tests.Elm.ElmSyntax;

public class SyntaxTreeClassicTypesTests
{
    [Fact]
    public void Node_value_equality()
    {
        var range1 = new Range(new Location(1, 1), new Location(1, 10));
        var range2 = new Range(new Location(1, 1), new Location(1, 10));

        var node1 = new Node<string>(range1, "test");
        var node2 = new Node<string>(range2, "test");

        node1.Should().Be(node2);
        node1.GetHashCode().Should().Be(node2.GetHashCode());
    }

    [Fact]
    public void Node_with_ModuleName_value_equality()
    {
        var range1 = new Range(new Location(1, 1), new Location(1, 10));
        var range2 = new Range(new Location(1, 1), new Location(1, 10));

        ModuleName moduleName1 = ["Module", "Name"];
        ModuleName moduleName2 = ["Module", "Name"];

        var node1 = new Node<ModuleName>(range1, moduleName1);
        var node2 = new Node<ModuleName>(range2, moduleName2);

        node1.Should().Be(node2);
        node1.GetHashCode().Should().Be(node2.GetHashCode());
    }

    [Fact]
    public void Range_value_equality()
    {
        var range1 = new Range(new Location(1, 1), new Location(2, 5));
        var range2 = new Range(new Location(1, 1), new Location(2, 5));

        range1.Should().Be(range2);
        range1.GetHashCode().Should().Be(range2.GetHashCode());
    }

    [Fact]
    public void Location_value_equality()
    {
        var loc1 = new Location(10, 20);
        var loc2 = new Location(10, 20);

        loc1.Should().Be(loc2);
        loc1.GetHashCode().Should().Be(loc2.GetHashCode());
    }

    [Fact]
    public void File_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        ModuleName moduleName = ["Test"];

        var moduleData1 = new DefaultModuleData(
            new Node<ModuleName>(range, moduleName),
            new Node<Exposing>(range, new Exposing.All(range))
        );
        var moduleData2 = new DefaultModuleData(
            new Node<ModuleName>(range, moduleName),
            new Node<Exposing>(range, new Exposing.All(range))
        );

        var file1 =
            new File(
                new Node<Module>(range, new Module.NormalModule(moduleData1)),
                [],
                [],
                []
        );

        var file2 =
            new File(
                new Node<Module>(range, new Module.NormalModule(moduleData2)),
                [],
                [],
                []
        );

        file1.Should().Be(file2);
        file1.GetHashCode().Should().Be(file2.GetHashCode());
    }

    [Fact]
    public void QualifiedNameRef_value_equality()
    {
        ModuleName moduleName1 = ["Maybe"];
        ModuleName moduleName2 = ["Maybe"];

        var ref1 = new QualifiedNameRef(moduleName1, "Just");
        var ref2 = new QualifiedNameRef(moduleName2, "Just");

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
    public void TypeAnnotation_GenericType_value_equality()
    {
        var type1 = new TypeAnnotation.GenericType("a");
        var type2 = new TypeAnnotation.GenericType("a");

        type1.Should().Be(type2);
        type1.GetHashCode().Should().Be(type2.GetHashCode());
    }

    [Fact]
    public void Pattern_VarPattern_value_equality()
    {
        var pattern1 = new Pattern.VarPattern("x");
        var pattern2 = new Pattern.VarPattern("x");

        pattern1.Should().Be(pattern2);
        pattern1.GetHashCode().Should().Be(pattern2.GetHashCode());
    }

    [Fact]
    public void LambdaStruct_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var lambda1 = new LambdaStruct(
            [new Node<Pattern>(range, new Pattern.VarPattern("x"))],
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))
        );
        var lambda2 = new LambdaStruct(
            [new Node<Pattern>(range, new Pattern.VarPattern("x"))],
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))
        );

        lambda1.Should().Be(lambda2);
        lambda1.GetHashCode().Should().Be(lambda2.GetHashCode());
    }

    [Fact]
    public void CaseBlock_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var caseItem = new Case(
            new Node<Pattern>(range, new Pattern.VarPattern("x")),
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))
        );

        var block1 = new CaseBlock(
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
            [caseItem]
        );
        var block2 = new CaseBlock(
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
            [caseItem]
        );

        block1.Should().Be(block2);
        block1.GetHashCode().Should().Be(block2.GetHashCode());
    }

    [Fact]
    public void RecordDefinition_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var field = new RecordField(
            new Node<string>(range, "name"),
            new Node<TypeAnnotation>(range, new TypeAnnotation.GenericType("String"))
        );

        var def1 = new RecordDefinition([new Node<RecordField>(range, field)]);
        var def2 = new RecordDefinition([new Node<RecordField>(range, field)]);

        def1.Should().Be(def2);
        def1.GetHashCode().Should().Be(def2.GetHashCode());
    }

    [Fact]
    public void ValueConstructor_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var ctor1 = new ValueConstructor(
            new Node<string>(range, "Just"),
            [new Node<TypeAnnotation>(range, new TypeAnnotation.GenericType("a"))]
        );
        var ctor2 = new ValueConstructor(
            new Node<string>(range, "Just"),
            [new Node<TypeAnnotation>(range, new TypeAnnotation.GenericType("a"))]
        );

        ctor1.Should().Be(ctor2);
        ctor1.GetHashCode().Should().Be(ctor2.GetHashCode());
    }

    [Fact]
    public void TypeAlias_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var alias1 = new TypeAlias(
            null,
            new Node<string>(range, "MyAlias"),
            [],
            new Node<TypeAnnotation>(range, new TypeAnnotation.Unit())
        );
        var alias2 = new TypeAlias(
            null,
            new Node<string>(range, "MyAlias"),
            [],
            new Node<TypeAnnotation>(range, new TypeAnnotation.Unit())
        );

        alias1.Should().Be(alias2);
        alias1.GetHashCode().Should().Be(alias2.GetHashCode());
    }

    [Fact]
    public void TypeStruct_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var type1 = new TypeStruct(
            null,
            new Node<string>(range, "MyType"),
            [],
            []
        );
        var type2 = new TypeStruct(
            null,
            new Node<string>(range, "MyType"),
            [],
            []
        );

        type1.Should().Be(type2);
        type1.GetHashCode().Should().Be(type2.GetHashCode());
    }

    [Fact]
    public void FunctionImplementation_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var impl1 = new FunctionImplementation(
            new Node<string>(range, "test"),
            [],
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.UnitExpr())
        );
        var impl2 = new FunctionImplementation(
            new Node<string>(range, "test"),
            [],
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.UnitExpr())
        );

        impl1.Should().Be(impl2);
        impl1.GetHashCode().Should().Be(impl2.GetHashCode());
    }

    [Fact]
    public void File_with_items_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        ModuleName moduleName = ["Test"];

        var moduleData1 = new DefaultModuleData(
            new Node<ModuleName>(range, moduleName),
            new Node<Exposing>(range, new Exposing.All(range))
        );
        var moduleData2 = new DefaultModuleData(
            new Node<ModuleName>(range, moduleName),
            new Node<Exposing>(range, new Exposing.All(range))
        );

        var import1 = new Import(
            new Node<ModuleName>(range, ["List"]),
            null,
            null
        );

        var funcImpl = new FunctionImplementation(
            new Node<string>(range, "myFunc"),
            [],
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))
        );
        var funcStruct = new FunctionStruct(null, null, new Node<FunctionImplementation>(range, funcImpl));
        var declaration1 = new Declaration.FunctionDeclaration(funcStruct);

        var file1 = new File(
            new Node<Module>(range, new Module.NormalModule(moduleData1)),
            [new Node<Import>(range, import1)],
            [new Node<Declaration>(range, declaration1)],
            [new Node<string>(range, "-- comment")]
        );

        var file2 = new File(
            new Node<Module>(range, new Module.NormalModule(moduleData2)),
            [new Node<Import>(range, import1)],
            [new Node<Declaration>(range, declaration1)],
            [new Node<string>(range, "-- comment")]
        );

        file1.Should().Be(file2);
        file1.GetHashCode().Should().Be(file2.GetHashCode());
    }

    [Fact]
    public void Expression_ListExpr_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var expr1 = new SyntaxTypes.Expression.ListExpr(
            [
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(2)),
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(3))
            ]
        );
        var expr2 = new SyntaxTypes.Expression.ListExpr(
            [
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(2)),
                new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(3))
            ]
        );

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }
}
