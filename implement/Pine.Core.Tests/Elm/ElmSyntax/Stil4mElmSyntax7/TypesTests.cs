using AwesomeAssertions;
using Xunit;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mElmSyntax7;

public class TypesTests
{
    [Fact]
    public void File_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        ModuleName moduleName = ["Test"];

        var moduleData1 = new SyntaxTypes.DefaultModuleData(
            new Node<ModuleName>(range, moduleName),
            new Node<SyntaxTypes.Exposing>(range, new SyntaxTypes.Exposing.All(range))
        );
        var moduleData2 = new SyntaxTypes.DefaultModuleData(
            new Node<ModuleName>(range, moduleName),
            new Node<SyntaxTypes.Exposing>(range, new SyntaxTypes.Exposing.All(range))
        );

        var file1 =
            new SyntaxTypes.File(
                new Node<SyntaxTypes.Module>(range, new SyntaxTypes.Module.NormalModule(moduleData1)),
                [],
                [],
                []
        );

        var file2 =
            new SyntaxTypes.File(
                new Node<SyntaxTypes.Module>(range, new SyntaxTypes.Module.NormalModule(moduleData2)),
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
    public void TypeAnnotation_GenericType_value_equality()
    {
        var type1 = new SyntaxTypes.TypeAnnotation.GenericType("a");
        var type2 = new SyntaxTypes.TypeAnnotation.GenericType("a");

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
    public void LambdaStruct_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var lambda1 = new SyntaxTypes.LambdaStruct(
            [new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x"))],
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))
        );
        var lambda2 = new SyntaxTypes.LambdaStruct(
            [new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x"))],
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))
        );

        lambda1.Should().Be(lambda2);
        lambda1.GetHashCode().Should().Be(lambda2.GetHashCode());
    }

    [Fact]
    public void CaseBlock_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        var caseItem = new SyntaxTypes.Case(
            new Node<SyntaxTypes.Pattern>(range, new SyntaxTypes.Pattern.VarPattern("x")),
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))
        );

        var block1 = new SyntaxTypes.CaseBlock(
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(1)),
            [caseItem]
        );
        var block2 = new SyntaxTypes.CaseBlock(
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
        var field = new SyntaxTypes.RecordField(
            new Node<string>(range, "name"),
            new Node<SyntaxTypes.TypeAnnotation>(range, new SyntaxTypes.TypeAnnotation.GenericType("String"))
        );

        var def1 = new SyntaxTypes.RecordDefinition([new Node<SyntaxTypes.RecordField>(range, field)]);
        var def2 = new SyntaxTypes.RecordDefinition([new Node<SyntaxTypes.RecordField>(range, field)]);

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

        var alias1 = new SyntaxTypes.TypeAlias(
            null,
            new Node<string>(range, "MyAlias"),
            [],
            new Node<SyntaxTypes.TypeAnnotation>(range, new SyntaxTypes.TypeAnnotation.Unit())
        );
        var alias2 = new SyntaxTypes.TypeAlias(
            null,
            new Node<string>(range, "MyAlias"),
            [],
            new Node<SyntaxTypes.TypeAnnotation>(range, new SyntaxTypes.TypeAnnotation.Unit())
        );

        alias1.Should().Be(alias2);
        alias1.GetHashCode().Should().Be(alias2.GetHashCode());
    }

    [Fact]
    public void TypeStruct_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var type1 = new SyntaxTypes.TypeStruct(
            null,
            new Node<string>(range, "MyType"),
            [],
            []
        );
        var type2 = new SyntaxTypes.TypeStruct(
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

        var impl1 = new SyntaxTypes.FunctionImplementation(
            new Node<string>(range, "test"),
            [],
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.UnitExpr())
        );
        var impl2 = new SyntaxTypes.FunctionImplementation(
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

        var moduleData1 = new SyntaxTypes.DefaultModuleData(
            new Node<ModuleName>(range, moduleName),
            new Node<SyntaxTypes.Exposing>(range, new SyntaxTypes.Exposing.All(range))
        );
        var moduleData2 = new SyntaxTypes.DefaultModuleData(
            new Node<ModuleName>(range, moduleName),
            new Node<SyntaxTypes.Exposing>(range, new SyntaxTypes.Exposing.All(range))
        );

        var import1 = new SyntaxTypes.Import(
            new Node<ModuleName>(range, ["List"]),
            null,
            null
        );

        var funcImpl = new SyntaxTypes.FunctionImplementation(
            new Node<string>(range, "myFunc"),
            [],
            new Node<SyntaxTypes.Expression>(range, new SyntaxTypes.Expression.Integer(42))
        );
        var funcStruct = new SyntaxTypes.FunctionStruct(null, null, new Node<SyntaxTypes.FunctionImplementation>(range, funcImpl));
        var declaration1 = new SyntaxTypes.Declaration.FunctionDeclaration(funcStruct);

        var file1 = new SyntaxTypes.File(
            new Node<SyntaxTypes.Module>(range, new SyntaxTypes.Module.NormalModule(moduleData1)),
            [new Node<SyntaxTypes.Import>(range, import1)],
            [new Node<SyntaxTypes.Declaration>(range, declaration1)],
            [new Node<string>(range, "-- comment")]
        );

        var file2 = new SyntaxTypes.File(
            new Node<SyntaxTypes.Module>(range, new SyntaxTypes.Module.NormalModule(moduleData2)),
            [new Node<SyntaxTypes.Import>(range, import1)],
            [new Node<SyntaxTypes.Declaration>(range, declaration1)],
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
