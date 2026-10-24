using AwesomeAssertions;
using Pine.Core.Elm.Syntax.Concrete;
using Xunit;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using Stil4mElmSyntax = Pine.Core.Elm.Syntax.Compatibility.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.Syntax.Compatibility.Stil4mElmSyntax7;

public class TypesTests
{
    [Fact]
    public void File_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        ModuleName moduleName = ["Test"];

        var moduleData1 =
            new Stil4mElmSyntax.DefaultModuleData(
                new Node<ModuleName>(range, moduleName),
                new Node<Stil4mElmSyntax.Exposing>(range, new Stil4mElmSyntax.Exposing.All(range)));

        var moduleData2 =
            new Stil4mElmSyntax.DefaultModuleData(
                new Node<ModuleName>(range, moduleName),
                new Node<Stil4mElmSyntax.Exposing>(range, new Stil4mElmSyntax.Exposing.All(range)));

        var file1 =
            new Stil4mElmSyntax.File(
                new Node<Stil4mElmSyntax.Module>(range, new Stil4mElmSyntax.Module.NormalModule(moduleData1)),
                [],
                [],
                []);

        var file2 =
            new Stil4mElmSyntax.File(
                new Node<Stil4mElmSyntax.Module>(range, new Stil4mElmSyntax.Module.NormalModule(moduleData2)),
                [],
                [],
                []);

        file1.Should().Be(file2);
        file1.GetHashCode().Should().Be(file2.GetHashCode());
    }

    [Fact]
    public void QualifiedNameRef_value_equality()
    {
        ModuleName moduleName1 = ["Maybe"];
        ModuleName moduleName2 = ["Maybe"];

        var ref1 = new Stil4mElmSyntax.QualifiedNameRef(moduleName1, "Just");
        var ref2 = new Stil4mElmSyntax.QualifiedNameRef(moduleName2, "Just");

        ref1.Should().Be(ref2);
        ref1.GetHashCode().Should().Be(ref2.GetHashCode());
    }

    [Fact]
    public void Expression_UnitExpr_value_equality()
    {
        var expr1 = new Stil4mElmSyntax.Expression.UnitExpr();
        var expr2 = new Stil4mElmSyntax.Expression.UnitExpr();

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }

    [Fact]
    public void Expression_Integer_value_equality()
    {
        var expr1 = new Stil4mElmSyntax.Expression.Integer(42);
        var expr2 = new Stil4mElmSyntax.Expression.Integer(42);

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }

    [Fact]
    public void TypeAnnotation_GenericType_value_equality()
    {
        var type1 = new Stil4mElmSyntax.TypeAnnotation.GenericType("a");
        var type2 = new Stil4mElmSyntax.TypeAnnotation.GenericType("a");

        type1.Should().Be(type2);
        type1.GetHashCode().Should().Be(type2.GetHashCode());
    }

    [Fact]
    public void Pattern_VarPattern_value_equality()
    {
        var pattern1 = new Stil4mElmSyntax.Pattern.VarPattern("x");
        var pattern2 = new Stil4mElmSyntax.Pattern.VarPattern("x");

        pattern1.Should().Be(pattern2);
        pattern1.GetHashCode().Should().Be(pattern2.GetHashCode());
    }

    [Fact]
    public void LambdaStruct_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var lambda1 =
            new Stil4mElmSyntax.LambdaStruct(
                [new Node<Stil4mElmSyntax.Pattern>(range, new Stil4mElmSyntax.Pattern.VarPattern("x"))],
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.Integer(42)));

        var lambda2 =
            new Stil4mElmSyntax.LambdaStruct(
                [new Node<Stil4mElmSyntax.Pattern>(range, new Stil4mElmSyntax.Pattern.VarPattern("x"))],
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.Integer(42)));

        lambda1.Should().Be(lambda2);
        lambda1.GetHashCode().Should().Be(lambda2.GetHashCode());
    }

    [Fact]
    public void CaseBlock_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var caseItem =
            new Stil4mElmSyntax.Case(
                new Node<Stil4mElmSyntax.Pattern>(range, new Stil4mElmSyntax.Pattern.VarPattern("x")),
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.Integer(42)));

        var block1 =
            new Stil4mElmSyntax.CaseBlock(
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.Integer(1)),
                [caseItem]);

        var block2 =
            new Stil4mElmSyntax.CaseBlock(
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.Integer(1)),
                [caseItem]);

        block1.Should().Be(block2);
        block1.GetHashCode().Should().Be(block2.GetHashCode());
    }

    [Fact]
    public void RecordDefinition_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var field =
            new Stil4mElmSyntax.RecordField(
                new Node<string>(range, "name"),
                new Node<Stil4mElmSyntax.TypeAnnotation>(
                    range,
                    new Stil4mElmSyntax.TypeAnnotation.GenericType("String")));

        var def1 = new Stil4mElmSyntax.RecordDefinition([new Node<Stil4mElmSyntax.RecordField>(range, field)]);
        var def2 = new Stil4mElmSyntax.RecordDefinition([new Node<Stil4mElmSyntax.RecordField>(range, field)]);

        def1.Should().Be(def2);
        def1.GetHashCode().Should().Be(def2.GetHashCode());
    }

    [Fact]
    public void ValueConstructor_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var ctor1 =
            new Stil4mElmSyntax.ValueConstructor(
                new Node<string>(range, "Just"),
                [new Node<Stil4mElmSyntax.TypeAnnotation>(range, new Stil4mElmSyntax.TypeAnnotation.GenericType("a"))]);

        var ctor2 =
            new Stil4mElmSyntax.ValueConstructor(
                new Node<string>(range, "Just"),
                [new Node<Stil4mElmSyntax.TypeAnnotation>(range, new Stil4mElmSyntax.TypeAnnotation.GenericType("a"))]);

        ctor1.Should().Be(ctor2);
        ctor1.GetHashCode().Should().Be(ctor2.GetHashCode());
    }

    [Fact]
    public void TypeAlias_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var alias1 =
            new Stil4mElmSyntax.TypeAlias(
                null,
                new Node<string>(range, "MyAlias"),
                [],
                new Node<Stil4mElmSyntax.TypeAnnotation>(range, new Stil4mElmSyntax.TypeAnnotation.Unit()));

        var alias2 =
            new Stil4mElmSyntax.TypeAlias(
                null,
                new Node<string>(range, "MyAlias"),
                [],
                new Node<Stil4mElmSyntax.TypeAnnotation>(range, new Stil4mElmSyntax.TypeAnnotation.Unit()));

        alias1.Should().Be(alias2);
        alias1.GetHashCode().Should().Be(alias2.GetHashCode());
    }

    [Fact]
    public void TypeStruct_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var type1 =
            new Stil4mElmSyntax.TypeStruct(
                null,
                new Node<string>(range, "MyType"),
                [],
                []);

        var type2 =
            new Stil4mElmSyntax.TypeStruct(
                null,
                new Node<string>(range, "MyType"),
                [],
                []);

        type1.Should().Be(type2);
        type1.GetHashCode().Should().Be(type2.GetHashCode());
    }

    [Fact]
    public void FunctionImplementation_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var impl1 =
            new Stil4mElmSyntax.FunctionImplementation(
                new Node<string>(range, "test"),
                [],
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.UnitExpr()));

        var impl2 =
            new Stil4mElmSyntax.FunctionImplementation(
                new Node<string>(range, "test"),
                [],
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.UnitExpr()));

        impl1.Should().Be(impl2);
        impl1.GetHashCode().Should().Be(impl2.GetHashCode());
    }

    [Fact]
    public void File_with_items_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));
        ModuleName moduleName = ["Test"];

        var moduleData1 =
            new Stil4mElmSyntax.DefaultModuleData(
                new Node<ModuleName>(range, moduleName),
                new Node<Stil4mElmSyntax.Exposing>(range, new Stil4mElmSyntax.Exposing.All(range)));

        var moduleData2 =
            new Stil4mElmSyntax.DefaultModuleData(
                new Node<ModuleName>(range, moduleName),
                new Node<Stil4mElmSyntax.Exposing>(range, new Stil4mElmSyntax.Exposing.All(range)));

        var import1 =
            new Stil4mElmSyntax.Import(
                new Node<ModuleName>(range, ["List"]),
                null,
                null);

        var funcImpl =
            new Stil4mElmSyntax.FunctionImplementation(
                new Node<string>(range, "myFunc"),
                [],
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.Integer(42)));

        var funcStruct =
            new Stil4mElmSyntax.FunctionStruct(
                null,
                null,
                new Node<Stil4mElmSyntax.FunctionImplementation>(range, funcImpl));

        var declaration1 = new Stil4mElmSyntax.Declaration.FunctionDeclaration(funcStruct);

        var file1 =
            new Stil4mElmSyntax.File(
                new Node<Stil4mElmSyntax.Module>(range, new Stil4mElmSyntax.Module.NormalModule(moduleData1)),
                [new Node<Stil4mElmSyntax.Import>(range, import1)],
                [new Node<Stil4mElmSyntax.Declaration>(range, declaration1)],
                [new Node<string>(range, "-- comment")]);

        var file2 =
            new Stil4mElmSyntax.File(
                new Node<Stil4mElmSyntax.Module>(range, new Stil4mElmSyntax.Module.NormalModule(moduleData2)),
                [new Node<Stil4mElmSyntax.Import>(range, import1)],
                [new Node<Stil4mElmSyntax.Declaration>(range, declaration1)],
                [new Node<string>(range, "-- comment")]);

        file1.Should().Be(file2);
        file1.GetHashCode().Should().Be(file2.GetHashCode());
    }

    [Fact]
    public void Expression_ListExpr_value_equality()
    {
        var range = new Range(new Location(1, 1), new Location(1, 10));

        var expr1 =
            new Stil4mElmSyntax.Expression.ListExpr(
                [
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.Integer(1)),
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.Integer(2)),
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.Integer(3))
                ]);

        var expr2 =
            new Stil4mElmSyntax.Expression.ListExpr(
                [
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.Integer(1)),
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.Integer(2)),
                new Node<Stil4mElmSyntax.Expression>(range, new Stil4mElmSyntax.Expression.Integer(3))
                ]);

        expr1.Should().Be(expr2);
        expr1.GetHashCode().Should().Be(expr2.GetHashCode());
    }
}
