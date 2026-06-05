using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using System.Numerics;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxAbstractTests;

// 'Expression' is also defined as Pine.Core.Expression in an enclosing namespace; alias to the abstract model.
using InfixDirection = Core.Elm.ElmSyntax.SyntaxModel.InfixDirection;
using Expression = Core.Elm.ElmSyntax.ElmSyntaxAbstract.Expression;

/// <summary>
/// Tests verifying value-equality and <see cref="object.GetHashCode"/> semantics of the abstract
/// Elm syntax model over all kinds of composite structures.
/// </summary>
public class ModelTests
{
    private static Expression.Integer IntExpr(int value) =>
        new(value, IntegerEncoding.EncodeSignedInteger(value));

    private static void AssertValueEqual(object a, object b)
    {
        a.Should().Be(b);
        b.Should().Be(a);
        a.GetHashCode().Should().Be(b.GetHashCode(), "structurally equal values must share a hash code");
    }

    private static void AssertNotEqual(object a, object b)
    {
        a.Should().NotBe(b);
        b.Should().NotBe(a);
    }

    [Fact]
    public void Integer_literal_equality_uses_value_and_pine_value()
    {
        // Two independently encoded PineValue instances of the same integer compare equal.
        var a = new Expression.Integer(42, IntegerEncoding.EncodeSignedInteger(42));
        var b = new Expression.Integer(42, IntegerEncoding.EncodeSignedInteger(42));

        AssertValueEqual(a, b);

        AssertNotEqual(a, new Expression.Integer(43, IntegerEncoding.EncodeSignedInteger(43)));
    }

    [Fact]
    public void Floatable_equality_uses_numerator_and_denominator()
    {
        AssertValueEqual(
            new Expression.Floatable(3, 2),
            new Expression.Floatable(3, 2));

        AssertNotEqual(
            new Expression.Floatable(3, 2),
            new Expression.Floatable(3, 4));
    }

    [Fact]
    public void String_literal_equality()
    {
        AssertValueEqual(
            Expression.StringLiteral.Create("alfa\nbeta"),
            Expression.StringLiteral.Create("alfa\nbeta"));

        AssertNotEqual(
            Expression.StringLiteral.Create("alfa"),
            Expression.StringLiteral.Create("beta"));
    }

    [Fact]
    public void FunctionOrValue_equality_compares_module_name_by_value()
    {
        AssertValueEqual(
            new Expression.FunctionOrValue(["Basics"], "always"),
            new Expression.FunctionOrValue(["Basics"], "always"));

        AssertNotEqual(
            new Expression.FunctionOrValue(["Basics"], "always"),
            new Expression.FunctionOrValue(["List"], "always"));

        AssertNotEqual(
            new Expression.FunctionOrValue(["Basics"], "always"),
            new Expression.FunctionOrValue([], "always"));
    }

    [Fact]
    public void ListExpr_equality_compares_elements_by_value()
    {
        AssertValueEqual(
            new Expression.ListExpr([IntExpr(1), IntExpr(2), IntExpr(3)]),
            new Expression.ListExpr([IntExpr(1), IntExpr(2), IntExpr(3)]));

        AssertNotEqual(
            new Expression.ListExpr([IntExpr(1), IntExpr(2)]),
            new Expression.ListExpr([IntExpr(1), IntExpr(2), IntExpr(3)]));

        AssertNotEqual(
            new Expression.ListExpr([IntExpr(1), IntExpr(2)]),
            new Expression.ListExpr([IntExpr(1), IntExpr(3)]));
    }

    [Fact]
    public void Application_equality_compares_function_and_arguments()
    {
        var function = new Expression.FunctionOrValue([], "f");

        AssertValueEqual(
            new Expression.Application(function, [IntExpr(1), IntExpr(2)]),
            new Expression.Application(function, [IntExpr(1), IntExpr(2)]));

        AssertNotEqual(
            new Expression.Application(function, [IntExpr(1), IntExpr(2)]),
            new Expression.Application(function, [IntExpr(1)]));

        AssertNotEqual(
            new Expression.Application(function, [IntExpr(1)]),
            new Expression.Application(new Expression.FunctionOrValue([], "g"), [IntExpr(1)]));
    }

    [Fact]
    public void TupledExpression_equality()
    {
        AssertValueEqual(
            new Expression.TupledExpression([IntExpr(1), IntExpr(2)]),
            new Expression.TupledExpression([IntExpr(1), IntExpr(2)]));

        AssertNotEqual(
            new Expression.TupledExpression([IntExpr(1), IntExpr(2)]),
            new Expression.TupledExpression([IntExpr(2), IntExpr(1)]));
    }

    [Fact]
    public void RecordAccess_equality_includes_field_name_value()
    {
        AssertValueEqual(
            new Expression.RecordAccess(
                new Expression.FunctionOrValue([], "r"),
                "field",
                StringEncoding.ValueFromString("field")),
            new Expression.RecordAccess(
                new Expression.FunctionOrValue([], "r"),
                "field",
                StringEncoding.ValueFromString("field")));

        AssertNotEqual(
            new Expression.RecordAccess(
                new Expression.FunctionOrValue([], "r"),
                "field",
                StringEncoding.ValueFromString("field")),
            new Expression.RecordAccess(
                new Expression.FunctionOrValue([], "r"),
                "other",
                StringEncoding.ValueFromString("other")));
    }

    [Fact]
    public void RecordAccessFunction_equality()
    {
        AssertValueEqual(
            new Expression.RecordAccessFunction("field", StringEncoding.ValueFromString("field")),
            new Expression.RecordAccessFunction("field", StringEncoding.ValueFromString("field")));

        AssertNotEqual(
            new Expression.RecordAccessFunction("field", StringEncoding.ValueFromString("field")),
            new Expression.RecordAccessFunction("other", StringEncoding.ValueFromString("other")));
    }

    [Fact]
    public void RecordExpr_equality_compares_setters_by_value()
    {
        static RecordSetter Setter(string name, int value) =>
            new(name, StringEncoding.ValueFromString(name), IntExpr(value));

        AssertValueEqual(
            new Expression.RecordExpr([Setter("a", 1), Setter("b", 2)]),
            new Expression.RecordExpr([Setter("a", 1), Setter("b", 2)]));

        AssertNotEqual(
            new Expression.RecordExpr([Setter("a", 1), Setter("b", 2)]),
            new Expression.RecordExpr([Setter("a", 1), Setter("b", 3)]));
    }

    [Fact]
    public void RecordUpdateExpression_equality()
    {
        static RecordSetter Setter(string name, int value) =>
            new(name, StringEncoding.ValueFromString(name), IntExpr(value));

        AssertValueEqual(
            new Expression.RecordUpdateExpression("rec", [Setter("a", 1)]),
            new Expression.RecordUpdateExpression("rec", [Setter("a", 1)]));

        AssertNotEqual(
            new Expression.RecordUpdateExpression("rec", [Setter("a", 1)]),
            new Expression.RecordUpdateExpression("other", [Setter("a", 1)]));

        AssertNotEqual(
            new Expression.RecordUpdateExpression("rec", [Setter("a", 1)]),
            new Expression.RecordUpdateExpression("rec", [Setter("a", 2)]));
    }

    [Fact]
    public void IfBlock_equality()
    {
        AssertValueEqual(
            new Expression.IfBlock(IntExpr(1), IntExpr(2), IntExpr(3)),
            new Expression.IfBlock(IntExpr(1), IntExpr(2), IntExpr(3)));

        AssertNotEqual(
            new Expression.IfBlock(IntExpr(1), IntExpr(2), IntExpr(3)),
            new Expression.IfBlock(IntExpr(1), IntExpr(2), IntExpr(4)));
    }

    [Fact]
    public void OperatorApplication_equality()
    {
        AssertValueEqual(
            new Expression.OperatorApplication("+", InfixDirection.Left, IntExpr(1), IntExpr(2)),
            new Expression.OperatorApplication("+", InfixDirection.Left, IntExpr(1), IntExpr(2)));

        AssertNotEqual(
            new Expression.OperatorApplication("+", InfixDirection.Left, IntExpr(1), IntExpr(2)),
            new Expression.OperatorApplication("-", InfixDirection.Left, IntExpr(1), IntExpr(2)));
    }

    [Fact]
    public void LambdaExpression_equality()
    {
        AssertValueEqual(
            new Expression.LambdaExpression(
                [new Pattern.VarPattern("x"), new Pattern.VarPattern("y")],
                new Expression.FunctionOrValue([], "x")),
            new Expression.LambdaExpression(
                [new Pattern.VarPattern("x"), new Pattern.VarPattern("y")],
                new Expression.FunctionOrValue([], "x")));

        AssertNotEqual(
            new Expression.LambdaExpression(
                [new Pattern.VarPattern("x")],
                new Expression.FunctionOrValue([], "x")),
            new Expression.LambdaExpression(
                [new Pattern.VarPattern("x"), new Pattern.VarPattern("y")],
                new Expression.FunctionOrValue([], "x")));
    }

    [Fact]
    public void CaseExpression_equality()
    {
        var caseA =
            new Expression.CaseExpression(
                new Expression.FunctionOrValue([], "x"),
                [
                    new Case(new Pattern.IntPattern(0), IntExpr(1)),
                    new Case(new Pattern.AllPattern(), IntExpr(2)),
                ]);

        var caseB =
            new Expression.CaseExpression(
                new Expression.FunctionOrValue([], "x"),
                [
                    new Case(new Pattern.IntPattern(0), IntExpr(1)),
                    new Case(new Pattern.AllPattern(), IntExpr(2)),
                ]);

        AssertValueEqual(caseA, caseB);

        AssertNotEqual(
            caseA,
            new Expression.CaseExpression(
                new Expression.FunctionOrValue([], "x"),
                [
                    new Case(new Pattern.IntPattern(0), IntExpr(1)),
                ]));
    }

    [Fact]
    public void LetExpression_equality()
    {
        static LetDeclaration Binding(string name, int value) =>
            new LetDeclaration.LetDestructuring(new Pattern.VarPattern(name), IntExpr(value));

        AssertValueEqual(
            new Expression.LetExpression([Binding("a", 1)], new Expression.FunctionOrValue([], "a")),
            new Expression.LetExpression([Binding("a", 1)], new Expression.FunctionOrValue([], "a")));

        AssertNotEqual(
            new Expression.LetExpression([Binding("a", 1)], new Expression.FunctionOrValue([], "a")),
            new Expression.LetExpression([Binding("a", 2)], new Expression.FunctionOrValue([], "a")));
    }

    [Fact]
    public void Nested_composite_expression_equality()
    {
        static Expression Build() =>
            new Expression.Application(
                new Expression.FunctionOrValue(["List"], "map"),
                [
                    new Expression.LambdaExpression(
                        [new Pattern.VarPattern("x")],
                        new Expression.OperatorApplication(
                            "+",
                            InfixDirection.Left,
                            new Expression.FunctionOrValue([], "x"),
                            IntExpr(1))),
                    new Expression.ListExpr([IntExpr(1), IntExpr(2), IntExpr(3)]),
                ]);

        AssertValueEqual(Build(), Build());
    }

    [Fact]
    public void Pattern_equality_over_composite_structures()
    {
        AssertValueEqual(
            new Pattern.NamedPattern(
                new QualifiedNameRef(["Maybe"], "Just"),
                [new Pattern.VarPattern("x")]),
            new Pattern.NamedPattern(
                new QualifiedNameRef(["Maybe"], "Just"),
                [new Pattern.VarPattern("x")]));

        AssertNotEqual(
            new Pattern.NamedPattern(
                new QualifiedNameRef(["Maybe"], "Just"),
                [new Pattern.VarPattern("x")]),
            new Pattern.NamedPattern(
                new QualifiedNameRef([], "Just"),
                [new Pattern.VarPattern("x")]));

        AssertValueEqual(
            new Pattern.TuplePattern([new Pattern.VarPattern("a"), new Pattern.VarPattern("b")]),
            new Pattern.TuplePattern([new Pattern.VarPattern("a"), new Pattern.VarPattern("b")]));

        AssertValueEqual(
            Pattern.RecordPattern.Create(["a", "b"]),
            Pattern.RecordPattern.Create(["a", "b"]));

        AssertNotEqual(
            Pattern.RecordPattern.Create(["a", "b"]),
            Pattern.RecordPattern.Create(["a", "c"]));

        AssertValueEqual(
            new Pattern.UnConsPattern(new Pattern.VarPattern("h"), new Pattern.VarPattern("t")),
            new Pattern.UnConsPattern(new Pattern.VarPattern("h"), new Pattern.VarPattern("t")));
    }

    [Fact]
    public void IntPattern_normalizes_hex_and_decimal()
    {
        // Hex and decimal int patterns are merged into IntPattern in the abstract model.
        AssertValueEqual(
            new Pattern.IntPattern(255),
            new Pattern.IntPattern(new BigInteger(0xFF)));
    }

    [Fact]
    public void QualifiedNameRef_equality_compares_module_name_by_value()
    {
        AssertValueEqual(
            new QualifiedNameRef(["A", "B"], "C"),
            new QualifiedNameRef(["A", "B"], "C"));

        AssertNotEqual(
            new QualifiedNameRef(["A", "B"], "C"),
            new QualifiedNameRef(["A"], "C"));
    }

    [Fact]
    public void TypeAnnotation_equality_over_composite_structures()
    {
        AssertValueEqual(
            new TypeAnnotation.Typed(["List"], "List", [new TypeAnnotation.GenericType("a")]),
            new TypeAnnotation.Typed(["List"], "List", [new TypeAnnotation.GenericType("a")]));

        AssertNotEqual(
            new TypeAnnotation.Typed(["List"], "List", [new TypeAnnotation.GenericType("a")]),
            new TypeAnnotation.Typed([], "List", [new TypeAnnotation.GenericType("a")]));

        AssertValueEqual(
            new TypeAnnotation.Tupled([new TypeAnnotation.Unit(), new TypeAnnotation.GenericType("a")]),
            new TypeAnnotation.Tupled([new TypeAnnotation.Unit(), new TypeAnnotation.GenericType("a")]));

        AssertValueEqual(
            new TypeAnnotation.FunctionTypeAnnotation(
                new TypeAnnotation.GenericType("a"),
                new TypeAnnotation.GenericType("b")),
            new TypeAnnotation.FunctionTypeAnnotation(
                new TypeAnnotation.GenericType("a"),
                new TypeAnnotation.GenericType("b")));

        AssertValueEqual(
            new TypeAnnotation.Record(
                new RecordDefinition([RecordField.Create("x", new TypeAnnotation.GenericType("a"))])),
            new TypeAnnotation.Record(
                new RecordDefinition([RecordField.Create("x", new TypeAnnotation.GenericType("a"))])));
    }

    [Fact]
    public void Declaration_equality_over_composite_structures()
    {
        static Declaration FunctionDecl(int bodyValue) =>
            new Declaration.FunctionDeclaration(
                new FunctionStruct(
                    Signature: null,
                    new FunctionImplementation(
                        "f",
                        [new Pattern.VarPattern("x")],
                        IntExpr(bodyValue))));

        AssertValueEqual(FunctionDecl(1), FunctionDecl(1));

        AssertNotEqual(FunctionDecl(1), FunctionDecl(2));

        AssertValueEqual(
            new Declaration.ChoiceTypeDeclaration(
                new TypeStruct(
                    "T",
                    ["a"],
                    [ValueConstructor.Create("Ctor", [new TypeAnnotation.GenericType("a")])])),
            new Declaration.ChoiceTypeDeclaration(
                new TypeStruct(
                    "T",
                    ["a"],
                    [ValueConstructor.Create("Ctor", [new TypeAnnotation.GenericType("a")])])));
    }

    [Fact]
    public void File_equality_over_composite_structures()
    {
        static File BuildFile(string functionName) =>
            new(
                new Module.NormalModule(
                    new DefaultModuleData(["Main"], new Exposing.All())),
                [new Import(["List"], null, null)],
                [
                    new Declaration.FunctionDeclaration(
                        new FunctionStruct(
                            Signature: null,
                            new FunctionImplementation(functionName, [], IntExpr(1)))),
                ]);

        AssertValueEqual(BuildFile("main"), BuildFile("main"));

        AssertNotEqual(BuildFile("main"), BuildFile("other"));
    }

    [Fact]
    public void Import_equality_with_alias_and_exposing()
    {
        AssertValueEqual(
            new Import(["Json", "Decode"], ["Decode"], new Exposing.All()),
            new Import(["Json", "Decode"], ["Decode"], new Exposing.All()));

        AssertNotEqual(
            new Import(["Json", "Decode"], ["Decode"], new Exposing.All()),
            new Import(["Json", "Decode"], ["D"], new Exposing.All()));

        AssertNotEqual(
            new Import(["Json", "Decode"], ["Decode"], new Exposing.All()),
            new Import(["Json", "Decode"], null, new Exposing.All()));

        AssertValueEqual(
            new Import(
                ["Html"],
                null,
                new Exposing.Explicit([new TopLevelExpose.FunctionExpose("text")])),
            new Import(
                ["Html"],
                null,
                new Exposing.Explicit([new TopLevelExpose.FunctionExpose("text")])));
    }

    [Fact]
    public void Exposing_explicit_equality()
    {
        AssertValueEqual(
            new Exposing.Explicit(
                [
                    new TopLevelExpose.TypeExpose(new ExposedType("Maybe", ExposesConstructors: true)),
                    new TopLevelExpose.FunctionExpose("map"),
                ]),
            new Exposing.Explicit(
                [
                    new TopLevelExpose.TypeExpose(new ExposedType("Maybe", ExposesConstructors: true)),
                    new TopLevelExpose.FunctionExpose("map"),
                ]));

        AssertNotEqual(
            new Exposing.Explicit(
                [new TopLevelExpose.TypeExpose(new ExposedType("Maybe", ExposesConstructors: true))]),
            new Exposing.Explicit(
                [new TopLevelExpose.TypeExpose(new ExposedType("Maybe", ExposesConstructors: false))]));
    }

    [Fact]
    public void Distinct_expression_cases_are_not_equal()
    {
        AssertNotEqual(new Expression.UnitExpr(), Expression.StringLiteral.Create(""));
        AssertNotEqual(IntExpr(1), Expression.StringLiteral.Create("1"));
        AssertNotEqual(Expression.CharLiteral.Create(65), IntExpr(65));
    }
}
