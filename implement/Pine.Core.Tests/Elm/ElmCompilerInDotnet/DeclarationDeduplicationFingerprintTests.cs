using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;
using System.Numerics;
using Xunit;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class DeclarationDeduplicationFingerprintTests
{
    private static Abstract.Declaration.FunctionDeclaration ParseFunction(string declaration)
    {
        var module =
            ElmSyntaxParser.ParseModuleText("module Example exposing (..)\n\n" + declaration)
            .Extract(error => throw new Exception("Failed parsing: " + error));

        return
            (Abstract.Declaration.FunctionDeclaration)
            Abstract.ConvertFromConcrete.FromDeclaration(module.Declarations.Single().Value);
    }

    private static string Fingerprint(string declaration) =>
        DeclarationDeduplication.GetStructuralFingerprint(ParseFunction(declaration), ["Example"]);

    private static string Fingerprint(
        Abstract.Expression body,
        params Abstract.Pattern[] arguments) =>
        DeclarationDeduplication.GetStructuralFingerprint(
            new Abstract.Declaration.FunctionDeclaration(
                new Abstract.FunctionStruct(
                    Signature: null,
                    new Abstract.FunctionImplementation("value", arguments, body))),
            ["Example"]);

    private static string SpecializationSortKey(string lambda)
    {
        var expression =
            (Abstract.Expression.LambdaExpression)
            ParseFunction("value = " + lambda).Function.Declaration.Expression;

        return
            new FunctionSpecialization(
                ImmutableDictionary<int, ParameterSpecialization>.Empty.Add(
                    0,
                    new ParameterSpecialization.ConcreteLambdaValue(expression)))
            .DeterministicSortKey;
    }

    [Fact]
    public void Structural_fingerprint_ignores_name_signature_documentation_and_positions()
    {
        var plain = Fingerprint("first x = helper x");

        var documented =
            Fingerprint(
                """
                {-| Documentation for the other declaration. -}
                second : Int -> Int
                second x =
                    Example.helper x
                """);

        documented.Should().Be(plain);

        plain.Should().Be(
            """function([var("x");],apply(identifier(["Example";],"helper"),[identifier([],"x");]))""");

        Fingerprint("first x = helper x").Should().Be(plain);
    }

    [Theory]
    [InlineData("value = 0x2A", "value = 42")]
    [InlineData("value = 1.0e3", "value = 1000.0")]
    [InlineData("value = \"\\u{0041}\"", "value = \"A\"")]
    [InlineData("value = '\\u{0041}'", "value = 'A'")]
    [InlineData("value (Just (x)) = ((x))", "value (Just x) = x")]
    public void Structural_fingerprint_normalizes_abstract_syntax(string first, string second)
    {
        Fingerprint(first).Should().Be(Fingerprint(second));
    }

    [Theory]
    [InlineData("value x = x", "value y = y")]
    [InlineData("value x = helper x", "value x = Other.helper x")]
    [InlineData("value (Just x) = x", "value (Other.Just x) = x")]
    [InlineData("value x = [ x, 1 ]", "value x = [ 1, x ]")]
    [InlineData("value = 1", "value = 2")]
    public void Structural_fingerprint_distinguishes_structural_differences(string first, string second)
    {
        Fingerprint(first).Should().NotBe(Fingerprint(second));
    }

    [Fact]
    public void Structural_fingerprint_preserves_nested_local_scopes()
    {
        var unqualified =
            """
            value ({ field } as record) (head :: tail) =
                let
                    helper (Just item) =
                        \argument ->
                            case argument of
                                ( first, second ) as pair ->
                                    ( record.field, head, tail, field, item, first, second, pair, external )

                    ( left, right ) =
                        ( head, tail )
                in
                helper ( left, right )
            """;

        var qualified =
            unqualified.Replace(", external )", ", Example.external )", StringComparison.Ordinal);

        var fingerprint = Fingerprint(unqualified);

        fingerprint.Should().Be(Fingerprint(qualified));
        fingerprint.Should().Contain("""identifier(["Example";],"external")""");
        fingerprint.Should().NotContain("""identifier(["Example";],"helper")""");
        fingerprint.Should().NotContain("""identifier(["Example";],"left")""");
        fingerprint.Should().NotContain("""identifier(["Example";],"record")""");
        fingerprint.Should().NotContain("""identifier(["Example";],"item")""");
        fingerprint.Should().NotContain("""identifier(["Example";],"pair")""");
    }

    [Fact]
    public void Structural_fingerprint_qualifies_references_in_composite_expressions()
    {
        var unqualified =
            """
            value record =
                if condition then
                    { record | field = helper (negate number), other = [ external, .field record ] }
                else
                    { field = ( external, -number ), other = 'x' }
            """;

        var qualified =
            """
            value record =
                if Example.condition then
                    { record | field = Example.helper (Example.negate Example.number), other = [ Example.external, .field record ] }
                else
                    { field = ( Example.external, -Example.number ), other = 'x' }
            """;

        Fingerprint(unqualified).Should().Be(Fingerprint(qualified));
    }

    [Fact]
    public void Structural_fingerprint_keeps_let_bindings_local_in_destructuring_values()
    {
        var fingerprint =
            Fingerprint(
                """
                value =
                    let
                        ( left, right ) =
                            helper seed

                        helper x =
                            ( x, x )

                        seed =
                            external
                    in
                    ( left, right )
                """);

        fingerprint.Should().Contain("""apply(identifier([],"helper"),[identifier([],"seed");])""");
        fingerprint.Should().Contain("""identifier(["Example";],"external")""");
        fingerprint.Should().NotContain("""identifier(["Example";],"helper")""");
        fingerprint.Should().NotContain("""identifier(["Example";],"seed")""");
    }

    [Fact]
    public void Structural_fingerprint_preserves_lambda_specialization_sort_keys()
    {
        SpecializationSortKey(@"\x -> [ x, 1 ]").Should().Be(
            """0:L:function([],lambda([var("x");],list([identifier([],"x");int(1);])))#I""");

        SpecializationSortKey(@"\x -> [ x, 1 ]").Should().NotBe(SpecializationSortKey(@"\x -> [ x, 2 ]"));
        SpecializationSortKey(@"\x -> [ x, 1 ]").Should().NotBe(SpecializationSortKey(@"\y -> [ y, 1 ]"));
    }

    [Fact]
    public void Structural_fingerprint_preserves_order_of_list_shaped_lambda_specializations()
    {
        string[] expectedOrder =
            [
            @"\x -> [ x, 1 ]",
            @"\x -> [ x, 2 ]",
            @"\x -> [ x, 20 ]",
            @"\y -> [ y, 1 ]",
            @"\z -> [ z, 1 ]"
            ];

        expectedOrder.Reverse()
            .OrderBy(SpecializationSortKey, StringComparer.Ordinal)
            .Should().Equal(expectedOrder);

        expectedOrder
            .OrderBy(SpecializationSortKey, StringComparer.Ordinal)
            .Should().Equal(expectedOrder);
    }

    [Fact]
    public void Structural_fingerprint_lets_lambda_lifting_reuse_the_first_equivalent_declaration()
    {
        var alphaName = DeclQualifiedName.Create(["Example"], "alpha");
        var betaName = DeclQualifiedName.Create(["Example"], "beta");
        var valueName = DeclQualifiedName.Create(["Example"], "value");

        var declarations =
            ImmutableDictionary<DeclQualifiedName, Abstract.Declaration>.Empty
            .Add(betaName, ParseFunction("beta x = helper x"))
            .Add(valueName, ParseFunction(@"value = \x -> Example.helper x"))
            .Add(alphaName, ParseFunction("alpha x = Example.helper x"));

        var lifted = LambdaLifting.LiftLambdas(declarations);

        lifted.Keys.Should().BeEquivalentTo(declarations.Keys);

        var value = (Abstract.Declaration.FunctionDeclaration)lifted[valueName];

        value.Function.Declaration.Expression.Should().Be(
            new Abstract.Expression.Identifier(alphaName));

        LambdaLifting.LiftLambdas(lifted).Should().BeEquivalentTo(lifted);
    }

    [Fact]
    public void Structural_fingerprint_is_independent_of_current_culture()
    {
        var original = CultureInfo.CurrentCulture;

        try
        {
            CultureInfo.CurrentCulture = CultureInfo.GetCultureInfo("en-US");
            var expected = Fingerprint("value = ( 1.25, -42, \"a\\n\\\"\\\\b\" )");

            CultureInfo.CurrentCulture = CultureInfo.GetCultureInfo("fr-FR");
            Fingerprint("value = ( 1.25, -42, \"a\\n\\\"\\\\b\" )").Should().Be(expected);
        }
        finally
        {
            CultureInfo.CurrentCulture = original;
        }
    }

    [Fact]
    public void Structural_fingerprint_unambiguously_delimits_strings_and_lists()
    {
        var first = Fingerprint("""value = [ "a", "b" ]""");
        var second = Fingerprint("""value = [ "a\",\"b" ]""");

        first.Should().NotBe(second);
        Fingerprint("""value = "string(\"a\")" """).Should().NotBe(Fingerprint("""value = "a" """));
        Fingerprint("""value = "\\u000a" """).Should().NotBe(Fingerprint("""value = "\n" """));
    }

    [Fact]
    public void Structural_fingerprint_retains_nested_function_signatures()
    {
        static string Source(string type) =>
            "value =\n    let\n        local : " + type +
            "\n        local =\n            1\n    in\n    local";

        Fingerprint(Source("Int")).Should().NotBe(Fingerprint(Source("Float")));
    }

    [Fact]
    public void Structural_fingerprint_keeps_exact_rationals_without_rendering_them()
    {
        Fingerprint(new Abstract.Expression.FloatLiteral(1, 3))
            .Should().Be("function([],float(1,3))");

        var denominator = BigInteger.Pow(10, 100);

        Fingerprint(new Abstract.Expression.FloatLiteral(denominator + 1, denominator))
            .Should().NotBe(
            Fingerprint(new Abstract.Expression.FloatLiteral(denominator + 2, denominator)));
    }

    [Fact]
    public void Structural_fingerprint_covers_every_expression_variant()
    {
        var unit = Abstract.Expression.UnitExpr.Instance;
        var identifier = Abstract.Expression.Identifier.Create([], "outside");
        var field = new Abstract.RecordSetter("field", PineValue.EmptyList, unit);

        Abstract.Expression[] expressions =
            [
            unit,
            Abstract.Expression.StringLiteral.Create("a\n\"\\b"),
            Abstract.Expression.CharLiteral.Create(0x1F600),
            ParseFunction("value = 123456789012345678901234567890").Function.Declaration.Expression,
            new Abstract.Expression.FloatLiteral(1, 3),
            new Abstract.Expression.Negation(unit),
            new Abstract.Expression.ListExpr([unit]),
            identifier,
            new Abstract.Expression.IfBlock(unit, unit, unit),
            new Abstract.Expression.PrefixOperator("+"),
            new Abstract.Expression.Application(identifier, [unit]),
            new Abstract.Expression.OperatorApplication(
                "+",
                Core.Elm.ElmSyntax.SyntaxModel.InfixDirection.Left,
                unit,
                unit),
            new Abstract.Expression.TupledExpression([unit, unit]),
            new Abstract.Expression.LambdaExpression(
                [new Abstract.Pattern.VarPattern("argument")],
                Abstract.Expression.Identifier.Create([], "argument")),
            new Abstract.Expression.CaseExpression(unit, [new Abstract.Case(new Abstract.Pattern.AllPattern(), unit)]),
            new Abstract.Expression.LetExpression(
                [
                new Abstract.LetDeclaration.LetFunction(
                    new Abstract.FunctionStruct(null, new Abstract.FunctionImplementation("local", [], unit))),
                new Abstract.LetDeclaration.LetDestructuring(new Abstract.Pattern.AllPattern(), unit)
                ],
                unit),
            new Abstract.Expression.RecordExpr([field]),
            new Abstract.Expression.RecordAccess(unit, "field", PineValue.EmptyList),
            new Abstract.Expression.RecordAccessFunction("field", PineValue.EmptyList),
            new Abstract.Expression.RecordUpdateExpression("record", [field]),
            new Abstract.Expression.GLSLExpression("void main() { }\n\u0000")
            ];

        expressions.Select(expression => expression.GetType()).Should().BeEquivalentTo(
            typeof(Abstract.Expression).GetNestedTypes()
            .Where(type => type.IsSubclassOf(typeof(Abstract.Expression))));

        expressions.Select(expression => Fingerprint(expression))
            .Distinct(StringComparer.Ordinal).Should().HaveCount(expressions.Length);
    }

    [Fact]
    public void Structural_fingerprint_covers_every_pattern_variant()
    {
        var variable = new Abstract.Pattern.VarPattern("variable");

        Abstract.Pattern[] patterns =
            [
            new Abstract.Pattern.AllPattern(),
            variable,
            new Abstract.Pattern.UnitPattern(),
            new Abstract.Pattern.CharPattern(0x1F600),
            new Abstract.Pattern.StringPattern("a\n\"\\b"),
            new Abstract.Pattern.IntPattern(BigInteger.Pow(10, 100)),
            new Abstract.Pattern.FloatPattern(1.25),
            new Abstract.Pattern.TuplePattern([variable]),
            Abstract.Pattern.RecordPattern.Create(["field"]),
            new Abstract.Pattern.UnConsPattern(variable, new Abstract.Pattern.VarPattern("tail")),
            new Abstract.Pattern.ListPattern([variable]),
            new Abstract.Pattern.NamedPattern(new Abstract.QualifiedNameRef(["Maybe"], "Just"), [variable]),
            new Abstract.Pattern.AsPattern(variable, "alias")
            ];

        patterns.Select(pattern => pattern.GetType()).Should().BeEquivalentTo(
            typeof(Abstract.Pattern).GetNestedTypes()
            .Where(type => type.IsSubclassOf(typeof(Abstract.Pattern))));

        patterns.Select(pattern => Fingerprint(Abstract.Expression.UnitExpr.Instance, pattern))
            .Distinct(StringComparer.Ordinal).Should().HaveCount(patterns.Length);
    }

    [Fact]
    public void Structural_fingerprint_covers_every_nested_signature_type_variant()
    {
        var integer = new Abstract.TypeAnnotation.Typed([], "Int", []);
        var record = new Abstract.RecordDefinition([Abstract.RecordField.Create("field", integer)]);

        Abstract.TypeAnnotation[] annotations =
            [
            new Abstract.TypeAnnotation.GenericType("a"),
            integer,
            new Abstract.TypeAnnotation.Unit(),
            new Abstract.TypeAnnotation.Tupled([integer, integer]),
            new Abstract.TypeAnnotation.Record(record),
            new Abstract.TypeAnnotation.GenericRecord("r", record),
            new Abstract.TypeAnnotation.FunctionTypeAnnotation(integer, integer)
            ];

        annotations.Select(annotation => annotation.GetType()).Should().BeEquivalentTo(
            typeof(Abstract.TypeAnnotation).GetNestedTypes()
            .Where(type => type.IsSubclassOf(typeof(Abstract.TypeAnnotation))));

        var fingerprints =
            annotations.Select(
                annotation =>
                Fingerprint(
                    new Abstract.Expression.LetExpression(
                        [
                        new Abstract.LetDeclaration.LetFunction(
                            new Abstract.FunctionStruct(
                                new Abstract.Signature("local", annotation),
                                new Abstract.FunctionImplementation(
                                    "local",
                                    [],
                                    Abstract.Expression.UnitExpr.Instance)))
                        ],
                        Abstract.Expression.Identifier.Create([], "local"))));

        fingerprints.Distinct(StringComparer.Ordinal).Should().HaveCount(annotations.Length);
    }
}
