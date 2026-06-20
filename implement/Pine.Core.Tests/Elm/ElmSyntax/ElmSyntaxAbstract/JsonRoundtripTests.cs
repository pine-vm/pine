using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using System.Collections.Generic;
using System.Numerics;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxAbstract;

// 'Expression' is also defined as Pine.Core.Expression in an enclosing namespace; alias to the abstract model.
using InfixDirection = Core.Elm.ElmSyntax.SyntaxModel.InfixDirection;
using Expression = Core.Elm.ElmSyntax.ElmSyntaxAbstract.Expression;

/// <summary>
/// Tests verifying that the JSON encoding and decoding of the abstract Elm syntax model
/// (see <see cref="ElmSyntaxAbstractJson"/>) round-trips every kind of node reachable from
/// <see cref="File"/>, and that the prepared interpreter representation round-trips as well.
/// </summary>
public class JsonRoundtripTests
{
    private static Expression.Integer IntExpr(int value) =>
        new(value, IntegerEncoding.EncodeSignedInteger(value));

    private static RecordSetter Setter(string name, Expression value) =>
        new(name, StringEncoding.ValueFromString(name), value);

    /// <summary>
    /// Serializes <paramref name="value"/>, deserializes it back, and asserts the result is value-equal
    /// to the original and that re-serialization is byte-for-byte identical.
    /// </summary>
    private static void AssertRoundtrip<T>(T value)
    {
        var json = ElmSyntaxAbstractJson.ToJsonString(value);

        var back = ElmSyntaxAbstractJson.FromJsonString<T>(json);

        back.Should().Be(value);

        ElmSyntaxAbstractJson.ToJsonString(back).Should().Be(json);
    }

    [Fact]
    public void Roundtrip_expression_literals()
    {
        AssertRoundtrip<Expression>(Expression.UnitExpr.Instance);
        AssertRoundtrip<Expression>(Expression.StringLiteral.Create("alfa\nbeta \"quoted\" 😀"));
        AssertRoundtrip<Expression>(Expression.StringLiteral.Create(""));
        AssertRoundtrip<Expression>(Expression.CharLiteral.Create('z'));
        AssertRoundtrip<Expression>(Expression.CharLiteral.Create('\n'));
        AssertRoundtrip<Expression>(Expression.CharLiteral.Create(0x1F600));
        AssertRoundtrip<Expression>(IntExpr(0));
        AssertRoundtrip<Expression>(IntExpr(42));
        AssertRoundtrip<Expression>(new Expression.Integer(-7, IntegerEncoding.EncodeSignedInteger(-7)));

        AssertRoundtrip<Expression>(
            new Expression.Integer(
                BigInteger.Parse("123456789012345678901234567890"),
                IntegerEncoding.EncodeSignedInteger(BigInteger.Parse("123456789012345678901234567890"))));

        AssertRoundtrip<Expression>(new Expression.FloatLiteral(314, 100));
        AssertRoundtrip<Expression>(new Expression.FloatLiteral(-3, 2));
        AssertRoundtrip<Expression>(new Expression.GLSLExpression("void main() {}"));
        AssertRoundtrip<Expression>(new Expression.PrefixOperator("+"));
    }

    [Fact]
    public void Roundtrip_expression_composites()
    {
        AssertRoundtrip<Expression>(new Expression.Negation(IntExpr(3)));

        AssertRoundtrip<Expression>(new Expression.ListExpr([IntExpr(1), IntExpr(2), IntExpr(3)]));
        AssertRoundtrip<Expression>(new Expression.ListExpr([]));

        AssertRoundtrip<Expression>(Expression.FunctionOrValue.Create(["List", "Extra"], "foldl1"));
        AssertRoundtrip<Expression>(Expression.FunctionOrValue.Create([], "x"));

        AssertRoundtrip<Expression>(new Expression.IfBlock(IntExpr(1), IntExpr(2), IntExpr(3)));

        AssertRoundtrip<Expression>(
            new Expression.Application(
                Expression.FunctionOrValue.Create([], "f"),
                [IntExpr(1), IntExpr(2)]));

        AssertRoundtrip<Expression>(
            new Expression.OperatorApplication("+", InfixDirection.Left, IntExpr(1), IntExpr(2)));

        AssertRoundtrip<Expression>(
            new Expression.OperatorApplication("|>", InfixDirection.Right, IntExpr(1), IntExpr(2)));

        AssertRoundtrip<Expression>(
            new Expression.OperatorApplication(":", InfixDirection.Non, IntExpr(1), IntExpr(2)));

        AssertRoundtrip<Expression>(new Expression.TupledExpression([IntExpr(1), IntExpr(2)]));

        AssertRoundtrip<Expression>(
            new Expression.LambdaExpression(
                [new Pattern.VarPattern("x"), new Pattern.VarPattern("y")],
                new Expression.OperatorApplication(
                    "+",
                    InfixDirection.Left,
                    Expression.FunctionOrValue.Create([], "x"),
                    Expression.FunctionOrValue.Create([], "y"))));
    }

    [Fact]
    public void Roundtrip_expression_records()
    {
        AssertRoundtrip<Expression>(
            Expression.RecordExpr.CreateSorted(
                [("a", IntExpr(1)), ("b", IntExpr(2))]));

        AssertRoundtrip<Expression>(Expression.RecordExpr.Empty);

        AssertRoundtrip<Expression>(
            new Expression.RecordExpr([Setter("x", IntExpr(1)), Setter("y", IntExpr(2))]));

        AssertRoundtrip<Expression>(
            new Expression.RecordAccess(
                Expression.FunctionOrValue.Create([], "r"),
                "field",
                StringEncoding.ValueFromString("field")));

        AssertRoundtrip<Expression>(
            new Expression.RecordAccessFunction("field", StringEncoding.ValueFromString("field")));

        AssertRoundtrip<Expression>(
            Expression.RecordUpdateExpression.CreateSorted("rec", [("a", IntExpr(1)), ("b", IntExpr(2))]));
    }

    [Fact]
    public void Roundtrip_expression_case_and_let()
    {
        AssertRoundtrip<Expression>(
            new Expression.CaseExpression(
                Expression.FunctionOrValue.Create([], "x"),
                [
                    new Case(new Pattern.IntPattern(0), Expression.StringLiteral.Create("zero")),
                    new Case(new Pattern.AllPattern(), Expression.StringLiteral.Create("other")),
                ]));

        AssertRoundtrip<Expression>(
            new Expression.LetExpression(
                [
                    new LetDeclaration.LetFunction(
                        new FunctionStruct(
                            Signature: null,
                            new FunctionImplementation(
                                "doubled",
                                [],
                                new Expression.OperatorApplication(
                                    "*",
                                    InfixDirection.Left,
                                    Expression.FunctionOrValue.Create([], "n"),
                                    IntExpr(2))))),
                    new LetDeclaration.LetDestructuring(
                        new Pattern.TuplePattern([new Pattern.VarPattern("a"), new Pattern.VarPattern("b")]),
                        new Expression.TupledExpression([IntExpr(1), IntExpr(2)])),
                ],
                Expression.FunctionOrValue.Create([], "doubled")));
    }

    [Fact]
    public void Roundtrip_patterns()
    {
        AssertRoundtrip<Pattern>(new Pattern.AllPattern());
        AssertRoundtrip<Pattern>(new Pattern.VarPattern("x"));
        AssertRoundtrip<Pattern>(new Pattern.UnitPattern());
        AssertRoundtrip<Pattern>(new Pattern.CharPattern('a'));
        AssertRoundtrip<Pattern>(new Pattern.CharPattern(0x1F600));
        AssertRoundtrip<Pattern>(new Pattern.StringPattern("hello\tworld"));
        AssertRoundtrip<Pattern>(new Pattern.StringPattern(""));
        AssertRoundtrip<Pattern>(new Pattern.IntPattern(0));
        AssertRoundtrip<Pattern>(new Pattern.IntPattern(-123));
        AssertRoundtrip<Pattern>(new Pattern.IntPattern(BigInteger.Parse("99999999999999999999999")));
        AssertRoundtrip<Pattern>(new Pattern.FloatPattern(3.14));
        AssertRoundtrip<Pattern>(new Pattern.FloatPattern(-0.5));

        AssertRoundtrip<Pattern>(
            new Pattern.TuplePattern([new Pattern.VarPattern("a"), new Pattern.VarPattern("b")]));

        AssertRoundtrip<Pattern>(Pattern.RecordPattern.Create(["x", "y", "z"]));
        AssertRoundtrip<Pattern>(Pattern.RecordPattern.Create([]));

        AssertRoundtrip<Pattern>(
            new Pattern.UnConsPattern(new Pattern.VarPattern("head"), new Pattern.VarPattern("tail")));

        AssertRoundtrip<Pattern>(
            new Pattern.ListPattern([new Pattern.VarPattern("a"), new Pattern.AllPattern()]));

        AssertRoundtrip<Pattern>(new Pattern.ListPattern([]));

        AssertRoundtrip<Pattern>(
            new Pattern.NamedPattern(
                new QualifiedNameRef(["Maybe"], "Just"),
                [new Pattern.VarPattern("value")]));

        AssertRoundtrip<Pattern>(
            new Pattern.NamedPattern(
                new QualifiedNameRef([], "Nothing"),
                []));

        AssertRoundtrip<Pattern>(
            new Pattern.AsPattern(new Pattern.VarPattern("inner"), "alias"));
    }

    [Fact]
    public void Roundtrip_type_annotations()
    {
        AssertRoundtrip<TypeAnnotation>(new TypeAnnotation.GenericType("a"));

        AssertRoundtrip<TypeAnnotation>(
            new TypeAnnotation.Typed(["List"], "List", [new TypeAnnotation.GenericType("a")]));

        AssertRoundtrip<TypeAnnotation>(new TypeAnnotation.Typed([], "Int", []));

        AssertRoundtrip<TypeAnnotation>(new TypeAnnotation.Unit());

        AssertRoundtrip<TypeAnnotation>(
            new TypeAnnotation.Tupled([new TypeAnnotation.GenericType("a"), new TypeAnnotation.Unit()]));

        AssertRoundtrip<TypeAnnotation>(
            new TypeAnnotation.Record(
                new RecordDefinition(
                    [
                        RecordField.Create("x", new TypeAnnotation.Typed([], "Int", [])),
                        RecordField.Create("y", new TypeAnnotation.Typed([], "Int", [])),
                    ])));

        AssertRoundtrip<TypeAnnotation>(
            new TypeAnnotation.GenericRecord(
                "base",
                new RecordDefinition([RecordField.Create("x", new TypeAnnotation.Typed([], "Int", []))])));

        AssertRoundtrip<TypeAnnotation>(
            new TypeAnnotation.FunctionTypeAnnotation(
                new TypeAnnotation.Typed([], "Int", []),
                new TypeAnnotation.Typed([], "String", [])));
    }

    [Fact]
    public void Roundtrip_declarations()
    {
        AssertRoundtrip<Declaration>(
            new Declaration.FunctionDeclaration(
                new FunctionStruct(
                    new Signature("add", new TypeAnnotation.Typed([], "Int", [])),
                    new FunctionImplementation(
                        "add",
                        [new Pattern.VarPattern("a"), new Pattern.VarPattern("b")],
                        new Expression.OperatorApplication(
                            "+",
                            InfixDirection.Left,
                            Expression.FunctionOrValue.Create([], "a"),
                            Expression.FunctionOrValue.Create([], "b"))))));

        AssertRoundtrip<Declaration>(
            new Declaration.ChoiceTypeDeclaration(
                new TypeStruct(
                    "Maybe",
                    ["a"],
                    [
                        ValueConstructor.Create("Nothing", []),
                        ValueConstructor.Create("Just", [new TypeAnnotation.GenericType("a")]),
                    ])));

        AssertRoundtrip<Declaration>(
            new Declaration.AliasDeclaration(
                new TypeAlias(
                    "Point",
                    [],
                    new TypeAnnotation.Record(
                        new RecordDefinition([RecordField.Create("x", new TypeAnnotation.Typed([], "Int", []))])))));

        AssertRoundtrip<Declaration>(
            new Declaration.PortDeclaration(
                new Signature("sendMessage", new TypeAnnotation.Typed([], "String", []))));

        AssertRoundtrip<Declaration>(
            new Declaration.InfixDeclaration(
                new Infix(InfixDirection.Left, 6, "+", "add")));
    }

    [Fact]
    public void Roundtrip_module_and_exposing()
    {
        AssertRoundtrip<Exposing>(new Exposing.All());

        AssertRoundtrip<Exposing>(
            new Exposing.Explicit(
                [
                    new TopLevelExpose.InfixExpose("+"),
                    new TopLevelExpose.FunctionExpose("map"),
                    new TopLevelExpose.TypeOrAliasExpose("Point"),
                    new TopLevelExpose.TypeExpose(new ExposedType("Maybe", ExposesConstructors: true)),
                    new TopLevelExpose.TypeExpose(new ExposedType("Hidden", ExposesConstructors: false)),
                ]));

        AssertRoundtrip<Module>(
            new Module.NormalModule(
                new DefaultModuleData(["Sample"], new Exposing.All())));

        AssertRoundtrip<Module>(
            new Module.PortModule(
                new DefaultModuleData(["Ports"], new Exposing.Explicit([new TopLevelExpose.FunctionExpose("out")]))));

        AssertRoundtrip<Module>(
            new Module.EffectModule(
                new EffectModuleData(
                    ["Effects"],
                    new Exposing.All(),
                    Command: "MyCmd",
                    Subscription: "MySub")));

        AssertRoundtrip<Module>(
            new Module.EffectModule(
                new EffectModuleData(["Effects"], new Exposing.All(), Command: null, Subscription: null)));
    }

    [Fact]
    public void Roundtrip_import()
    {
        AssertRoundtrip(new Import(["Dict"], ModuleAlias: null, ExposingList: null));

        AssertRoundtrip(
            new Import(
                ["List"],
                ModuleAlias: ["L"],
                ExposingList: new Exposing.Explicit([new TopLevelExpose.FunctionExpose("map")])));
    }

    private const string SampleModuleText =
        """
        module Sample exposing (..)

        import Dict exposing (Dict)
        import List as L


        type Maybe a
            = Nothing
            | Just a


        type alias Point =
            { x : Int, y : Int }


        add : Int -> Int -> Int
        add a b =
            a + b


        greet : String -> String
        greet name =
            case name of
                "" ->
                    "hi"

                _ ->
                    "hello " ++ name


        compute : Int -> Int
        compute n =
            let
                doubled =
                    n * 2

                helper x =
                    x + 1
            in
            helper doubled


        point : Point
        point =
            { x = 1, y = 2 }


        pick : { a | x : Int } -> Int
        pick r =
            .x r


        floaty : Float
        floaty =
            3.14


        letter : Char
        letter =
            'z'
        """;

    [Fact]
    public void Roundtrip_parsed_file()
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText(SampleModuleText)
            .Extract(err => throw new System.Exception("Failed to parse module: " + err));

        var file = ConvertFromConcrete.FromFile(parsed);

        var json = ElmSyntaxAbstractJson.FileToJsonString(file);

        var back = ElmSyntaxAbstractJson.FileFromJsonString(json);

        back.Should().Be(file);

        ElmSyntaxAbstractJson.FileToJsonString(back).Should().Be(json);
    }

    [Fact]
    public void Roundtrip_prepared()
    {
        var prepared =
            Core.Elm.ElmSyntax.ElmSyntaxInterpreter.PrepareModules([SampleModuleText])
            .Extract(err => throw new System.Exception("Failed to prepare modules: " + err));

        var json = ElmSyntaxInterpreterPreparedJson.ToJsonString(prepared);

        var back = ElmSyntaxInterpreterPreparedJson.FromJsonString(json);

        // The Prepared record does not provide structural equality on its dictionary, so compare entries.
        back.Declarations.Should().HaveCount(prepared.Declarations.Count);

        foreach (var entry in prepared.Declarations)
        {
            back.Declarations.TryGetValue(entry.Key, out var decodedDeclaration)
                .Should().BeTrue("declaration " + entry.Key.FullName + " should be present after roundtrip");

            decodedDeclaration.Should().Be(entry.Value);
        }

        // Re-encoding the decoded value yields identical JSON.
        ElmSyntaxInterpreterPreparedJson.ToJsonString(back).Should().Be(json);
    }

    [Fact]
    public void Roundtrip_prepared_multiple_modules()
    {
        const string moduleA =
            """
            module A exposing (..)

            import B


            value : Int
            value =
                B.base + 1
            """;

        const string moduleB =
            """
            module B exposing (..)


            base : Int
            base =
                41
            """;

        var prepared =
            Core.Elm.ElmSyntax.ElmSyntaxInterpreter.PrepareModules([moduleA, moduleB])
            .Extract(err => throw new System.Exception("Failed to prepare modules: " + err));

        var json = ElmSyntaxInterpreterPreparedJson.ToJsonString(prepared);

        var back = ElmSyntaxInterpreterPreparedJson.FromJsonString(json);

        back.Declarations.Should().HaveCount(prepared.Declarations.Count);

        foreach (var entry in prepared.Declarations)
        {
            back.Declarations.TryGetValue(entry.Key, out var decodedDeclaration)
                .Should().BeTrue();

            decodedDeclaration.Should().Be(entry.Value);
        }

        ElmSyntaxInterpreterPreparedJson.ToJsonString(back).Should().Be(json);
    }

    [Fact]
    public void Roundtrip_empty_prepared()
    {
        var prepared =
            new Core.Elm.ElmSyntax.ElmSyntaxInterpreter.Prepared(
                new Dictionary<Core.CodeAnalysis.DeclQualifiedName, Declaration>());

        var json = ElmSyntaxInterpreterPreparedJson.ToJsonString(prepared);

        var back = ElmSyntaxInterpreterPreparedJson.FromJsonString(json);

        back.Declarations.Should().BeEmpty();
    }
}
