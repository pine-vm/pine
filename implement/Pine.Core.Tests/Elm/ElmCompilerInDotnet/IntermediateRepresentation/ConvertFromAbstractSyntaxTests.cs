using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Text.Json;
using Xunit;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;
using InfixDirection = Pine.Core.Elm.ElmSyntax.SyntaxModel.InfixDirection;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

public class ConvertFromAbstractSyntaxTests
{
    private static Abstract.Expression Integer(int value) =>
        new Abstract.Expression.IntegerLiteral(value, IntegerEncoding.EncodeSignedInteger(value));

    private static readonly Abstract.TypeAnnotation s_generic = new Abstract.TypeAnnotation.GenericType("a");

    private static readonly Abstract.RecordDefinition s_recordDefinition =
        new(
            [
            Abstract.RecordField.Create("second", new Abstract.TypeAnnotation.Typed(["Basics"], "Int", [])),
            Abstract.RecordField.Create("first", s_generic)
            ]);

    private static readonly Abstract.FunctionStruct s_function =
        new(
            new Abstract.Signature(
                "function",
                new Abstract.TypeAnnotation.FunctionTypeAnnotation(s_generic, s_generic)),
            new Abstract.FunctionImplementation(
                "function",
                [
                new Abstract.Pattern.VarPattern("value")
                ],
                Abstract.Expression.Identifier.Create([], "value")));

    public static IEnumerable<object[]> Expressions()
    {
        Abstract.Expression[] expressions =
            [
            Abstract.Expression.UnitExpr.Instance,
            Abstract.Expression.StringLiteral.Create("line\n\u03bb"),
            Abstract.Expression.CharLiteral.Create(0x1F600),
            new Abstract.Expression.IntegerLiteral(
                BigInteger.One << 100,
                IntegerEncoding.EncodeSignedInteger(BigInteger.One << 100)),
            new Abstract.Expression.FloatLiteral(-17, 8),
            new Abstract.Expression.Negation(Integer(3)),
            new Abstract.Expression.ListExpr([Integer(2), Integer(1)]),
            Abstract.Expression.Identifier.Create(["Some", "Module"], "value"),
            new Abstract.Expression.IfBlock(
                Abstract.Expression.Identifier.Create(["Basics"], "True"),
                Integer(1),
                Integer(2)),
            new Abstract.Expression.PrefixOperator("(++)"),
            new Abstract.Expression.Application(
                Abstract.Expression.Identifier.Create([], "f"),
                [
                Integer(1),
                Integer(2)
                ]),
            new Abstract.Expression.OperatorApplication("+", InfixDirection.Right, Integer(1), Integer(2)),
            new Abstract.Expression.TupledExpression([Integer(2), Abstract.Expression.StringLiteral.Create("first")]),
            new Abstract.Expression.LambdaExpression(
                [
                new Abstract.Pattern.TuplePattern(
                    [
                    new Abstract.Pattern.VarPattern("x"),
                    new Abstract.Pattern.AllPattern()
                    ])
                ],
                Abstract.Expression.Identifier.Create([], "x")),
            new Abstract.Expression.CaseExpression(
                Abstract.Expression.Identifier.Create([], "x"),
                [
                new Abstract.Case(new Abstract.Pattern.IntPattern(0), Integer(2)),
                new Abstract.Case(new Abstract.Pattern.AllPattern(), Integer(1))
                ]),
            new Abstract.Expression.LetExpression(
                [
                new Abstract.LetDeclaration.LetFunction(s_function with { Signature = null }),
                new Abstract.LetDeclaration.LetDestructuring(
                    new Abstract.Pattern.VarPattern("x"),
                    new Abstract.Expression.Application(
                        Abstract.Expression.Identifier.Create([], "function"),
                        [
                        Integer(1)
                        ]))
                ],
                Abstract.Expression.Identifier.Create([], "x")),
            new Abstract.Expression.RecordExpr(
                [
                new Abstract.RecordSetter("z", StringEncoding.ValueFromString("z"), Integer(2)),
                new Abstract.RecordSetter("a", StringEncoding.ValueFromString("a"), Integer(1))
                ]),
            new Abstract.Expression.RecordAccess(
                new Abstract.Expression.Application(
                    Abstract.Expression.Identifier.Create([], "makeRecord"),
                    [
                    Integer(1)
                    ]),
                "field",
                StringEncoding.ValueFromString("field")),
            new Abstract.Expression.RecordAccessFunction("field", StringEncoding.ValueFromString("field")),
            new Abstract.Expression.RecordUpdateExpression(
                "record",
                [
                new Abstract.RecordSetter("z", StringEncoding.ValueFromString("z"), Integer(2)),
                new Abstract.RecordSetter("a", StringEncoding.ValueFromString("a"), Integer(1))
                ]),
            new Abstract.Expression.GLSLExpression("void main() { }")
            ];

        return expressions.Select(expression => new object[] { expression });
    }

    public static IEnumerable<object[]> Patterns()
    {
        Abstract.Pattern[] patterns =
            [
            new Abstract.Pattern.AllPattern(),
            new Abstract.Pattern.VarPattern("value"),
            new Abstract.Pattern.UnitPattern(),
            new Abstract.Pattern.CharPattern(0x1F600),
            new Abstract.Pattern.StringPattern("text\n"),
            new Abstract.Pattern.IntPattern(BigInteger.One << 100),
            new Abstract.Pattern.FloatPattern(1.25),
            new Abstract.Pattern.TuplePattern([new Abstract.Pattern.VarPattern("a"), new Abstract.Pattern.AllPattern()]),
            Abstract.Pattern.RecordPattern.Create(["z", "a"]),
            new Abstract.Pattern.UnConsPattern(
                new Abstract.Pattern.VarPattern("head"),
                new Abstract.Pattern.VarPattern("tail")),
            new Abstract.Pattern.ListPattern(
                [
                new Abstract.Pattern.IntPattern(1),
                new Abstract.Pattern.VarPattern("second")
                ]),
            new Abstract.Pattern.NamedPattern(
                new Abstract.QualifiedNameRef(["Some", "Module"], "Tag"),
                [
                new Abstract.Pattern.TuplePattern(
                    [
                    new Abstract.Pattern.VarPattern("x"),
                    new Abstract.Pattern.AllPattern()
                    ])
                ]),
            new Abstract.Pattern.AsPattern(new Abstract.Pattern.VarPattern("inner"), "outer")
            ];

        return patterns.Select(pattern => new object[] { pattern });
    }

    public static IEnumerable<object[]> TypeAnnotations()
    {
        Abstract.TypeAnnotation[] annotations =
            [
            s_generic,
            new Abstract.TypeAnnotation.Typed(
                [
                "Some",
                "Module"
                ],
                "Container",
                [
                s_generic,
                new Abstract.TypeAnnotation.Unit()
                ]),
            new Abstract.TypeAnnotation.Unit(),
            new Abstract.TypeAnnotation.Tupled([s_generic, new Abstract.TypeAnnotation.Unit()]),
            new Abstract.TypeAnnotation.Record(s_recordDefinition),
            new Abstract.TypeAnnotation.GenericRecord("row", s_recordDefinition),
            new Abstract.TypeAnnotation.FunctionTypeAnnotation(
                s_generic,
                new Abstract.TypeAnnotation.Record(s_recordDefinition))
            ];

        return annotations.Select(annotation => new object[] { annotation });
    }

    public static IEnumerable<object[]> Declarations()
    {
        Abstract.Declaration[] declarations =
            [
            new Abstract.Declaration.FunctionDeclaration(s_function),
            new Abstract.Declaration.ChoiceTypeDeclaration(
                new Abstract.ChoiceTypeStruct(
                    "Choice",
                    ["a"],
                    [
                    Abstract.ValueConstructor.Create("Empty", []),
                    Abstract.ValueConstructor.Create(
                        "Pair",
                        [
                        s_generic,
                        new Abstract.TypeAnnotation.Record(s_recordDefinition)
                        ])
                    ])),
            new Abstract.Declaration.AliasDeclaration(
                new Abstract.TypeAlias(
                    "Alias",
                    [
                    "a",
                    "row"
                    ],
                    new Abstract.TypeAnnotation.GenericRecord("row", s_recordDefinition))),
            new Abstract.Declaration.PortDeclaration(
                new Abstract.Signature(
                    "send",
                    new Abstract.TypeAnnotation.FunctionTypeAnnotation(s_generic, new Abstract.TypeAnnotation.Unit()))),
            new Abstract.Declaration.InfixDeclaration(new Abstract.Infix(InfixDirection.Right, 7, "<?>", "function"))
            ];

        return declarations.Select(declaration => new object[] { declaration });
    }

    [Theory]
    [MemberData(nameof(Expressions))]
    public void Expression_mapping_preserves_every_field_without_lowering(Abstract.Expression source) =>
        AssertMapping(source, IR.ConvertFromAbstractSyntax.FromExpression(source));

    [Theory]
    [MemberData(nameof(Expressions))]
    public void IR_reference_and_parameter_analysis_handle_every_expression_variant(Abstract.Expression source)
    {
        var expression = IR.ConvertFromAbstractSyntax.FromExpression(source);
        var references = new HashSet<string>();

        ExpressionCompiler.CollectExpressionReferences(expression, references);

        TypeInference.InferParameterTypesFromUsage(
            source,
            new Dictionary<string, int>(),
            new Dictionary<string, TypeInference.InferredType>());

        TypeInference.ExtractTypeConstraintsFromTagApplications(
            source,
            new Dictionary<DeclQualifiedName, IReadOnlyList<TypeInference.InferredType>>(),
            []);

        TypeInference.ExtractTypeConstraintsFromFunctionApplications(
            source,
            new Dictionary<DeclQualifiedName, FunctionTypeInfo>(),
            "Test",
            []);
    }

    [Theory]
    [MemberData(nameof(Patterns))]
    public void Pattern_mapping_preserves_every_field_without_lowering(Abstract.Pattern source) =>
        AssertMapping(source, IR.ConvertFromAbstractSyntax.FromPattern(source));

    [Theory]
    [MemberData(nameof(TypeAnnotations))]
    public void Type_annotation_mapping_preserves_every_field(Abstract.TypeAnnotation source) =>
        AssertMapping(source, IR.ConvertFromAbstractSyntax.FromTypeAnnotation(source));

    [Theory]
    [MemberData(nameof(Declarations))]
    public void Declaration_mapping_preserves_every_field(Abstract.Declaration source) =>
        AssertMapping(source, IR.ConvertFromAbstractSyntax.FromDeclaration(source));

    [Fact]
    public void Mapping_fixtures_cover_every_source_variant()
    {
        AssertVariantsCovered(typeof(Abstract.Expression), Expressions());
        AssertVariantsCovered(typeof(Abstract.Pattern), Patterns());
        AssertVariantsCovered(typeof(Abstract.TypeAnnotation), TypeAnnotations());
        AssertVariantsCovered(typeof(Abstract.Declaration), Declarations());
    }

    [Fact]
    public void IR_copies_model_records_except_File_and_adds_ProjectItem()
    {
        var sourceNamespace = typeof(Abstract.File).Namespace!;
        var targetNamespace = typeof(IR.Declaration).Namespace!;

        var sourceRecords = ModelRecords(sourceNamespace).Where(type => type != typeof(Abstract.File)).ToArray();
        var targetRecords = ModelRecords(targetNamespace);

        targetRecords.Select(type => type.FullName).Should().BeEquivalentTo(
            sourceRecords.Select(type => type.FullName!.Replace(sourceNamespace, targetNamespace))
            .Append(typeof(IR.Expression.ProjectItem).FullName));

        foreach (var source in sourceRecords)
        {
            var target =
                source.Assembly.GetType(source.FullName!.Replace(sourceNamespace, targetNamespace), throwOnError: true)!;

            target.BaseType!.ToString().Should().Be(
                source.BaseType!.ToString().Replace(sourceNamespace, targetNamespace));

            target.GetProperties(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly)
                .Select(property => (property.Name, Type: property.PropertyType.ToString()))
                .Should().BeEquivalentTo(
                source.GetProperties(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly)
                .Select(
                    property =>
                    (property.Name, Type: property.PropertyType.ToString().Replace(sourceNamespace, targetNamespace))));
        }
    }

    [Fact]
    public void Mapping_preserves_precomputed_values_and_qualified_name_instances()
    {
        var encoded = PineValue.List([IntegerEncoding.EncodeSignedInteger(123)]);
        var qualifiedName = DeclQualifiedName.Create(["Some", "Module"], "value");

        var integer = new Abstract.Expression.IntegerLiteral(42, encoded);

        ((IR.Expression.IntegerLiteral)IR.ConvertFromAbstractSyntax.FromExpression(integer))
            .ValueAsPineValue.Should().BeSameAs(encoded);

        var named = new Abstract.Pattern.NamedPattern(new Abstract.QualifiedNameRef(["Test"], "Tag"), [], encoded);

        ((IR.Pattern.NamedPattern)IR.ConvertFromAbstractSyntax.FromPattern(named)).TagNameAsPineValue.Should().BeSameAs(
            encoded);

        var record =
            new Abstract.Expression.RecordAccess(new Abstract.Expression.Identifier(qualifiedName), "field", encoded);

        var mapped = (IR.Expression.RecordAccess)IR.ConvertFromAbstractSyntax.FromExpression(record);
        mapped.FieldNameValue.Should().BeSameAs(encoded);
        ((IR.Expression.Identifier)mapped.Record).QualifiedName.Should().BeSameAs(qualifiedName);

        var constructor = new Abstract.ValueConstructor("Tag", encoded, []);
        IR.ConvertFromAbstractSyntax.FromValueConstructor(constructor).NameValue.Should().BeSameAs(encoded);
    }

    [Fact]
    public void Mapping_rejects_unknown_source_variants_explicitly()
    {
        Action[] mappings =
            [
            () => IR.ConvertFromAbstractSyntax.FromExpression(new UnknownExpression()),
            () => IR.ConvertFromAbstractSyntax.FromPattern(new UnknownPattern()),
            () => IR.ConvertFromAbstractSyntax.FromTypeAnnotation(new UnknownTypeAnnotation()),
            () => IR.ConvertFromAbstractSyntax.FromDeclaration(new UnknownDeclaration()),
            () => IR.ConvertFromAbstractSyntax.FromLetDeclaration(new UnknownLetDeclaration())
            ];

        foreach (var mapping in mappings)
            mapping.Should().Throw<NotImplementedException>().WithMessage("*does not handle*Unknown*");
    }

    private static void AssertMapping<TSource, TTarget>(TSource source, TTarget target)
        where TSource : class
        where TTarget : class
    {
        target.Should().NotBeSameAs(source);
        target.GetType().Name.Should().Be(source.GetType().Name);

        var options = Abstract.ElmSyntaxAbstractJson.JsonSerializerOptions;
        JsonSerializer.Serialize(target, options).Should().Be(JsonSerializer.Serialize(source, options));

        var restored = JsonSerializer.Deserialize<TTarget>(JsonSerializer.Serialize(target, options), options);
        restored.Should().Be(target);
    }

    private static void AssertVariantsCovered(Type hierarchy, IEnumerable<object[]> fixtures) =>
        fixtures.Select(row => row[0].GetType()).Should().BeEquivalentTo(hierarchy.GetNestedTypes());

    private static Type[] ModelRecords(string modelNamespace) =>
        [
        .. typeof(Abstract.File).Assembly.GetTypes()
        .Where(
            type =>
            type.Namespace == modelNamespace &&
            type.IsVisible &&
            type.GetMethod("<Clone>$") is not null &&
            (type.DeclaringType is null || type.DeclaringType.GetMethod("<Clone>$") is not null))
        ];

    private sealed record UnknownExpression : Abstract.Expression;

    private sealed record UnknownPattern : Abstract.Pattern;

    private sealed record UnknownTypeAnnotation : Abstract.TypeAnnotation;

    private sealed record UnknownDeclaration : Abstract.Declaration;

    private sealed record UnknownModule : Abstract.Module;

    private sealed record UnknownExposing : Abstract.Exposing;

    private sealed record UnknownExpose : Abstract.TopLevelExpose;

    private sealed record UnknownLetDeclaration : Abstract.LetDeclaration;
}
