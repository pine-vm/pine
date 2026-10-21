using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using Xunit;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;
using InfixDirection = Pine.Core.Elm.ElmSyntax.SyntaxModel.InfixDirection;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

public class FromCanonicalSyntaxTests
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

    private static Core.Elm.ElmCompilerInDotnet.CompilerModel.Program ConvertExpression(Abstract.Expression body)
    {
        var name = DeclQualifiedName.Create(["Test"], "root");

        var function =
            new Abstract.Declaration.FunctionDeclaration(
                new(
                    null,
                    new Abstract.FunctionImplementation(
                        "root",
                        [
                            new Abstract.Pattern.VarPattern("f"),
                            new Abstract.Pattern.VarPattern("x"), new Abstract.Pattern.VarPattern("record")
                        ],
                        body)));

        return
            Core.Elm.ElmCompilerInDotnet.CompilerModel.FromCanonicalSyntax.Convert(
                new Dictionary<DeclQualifiedName, Abstract.Declaration> { [name] = function });
    }

    [Theory]
    [MemberData(nameof(Expressions))]
    public void Source_expression_conversion_resolves_every_variant(Abstract.Expression source)
    {
        var model = Core.Elm.ElmCompilerInDotnet.CompilerModel.LambdaLifting.Lift(ConvertExpression(source));
        Core.Elm.ElmCompilerInDotnet.CompilerModel.ProgramValidation.Validate(model);
        var emitted = Core.Elm.ElmCompilerInDotnet.CompilerModel.ToIntermediateRepresentation.Convert(model);
        emitted.Should().NotBeEmpty();

        foreach (var expression in emitted.Values.SelectMany(function => IR.ExpressionTraversal.Enumerate(function.Expression)))
        {
            if (expression is IR.Expression.Identifier identifier)
                identifier.Reference.Should().NotBeNull();
        }
    }

    [Theory]
    [MemberData(nameof(Patterns))]
    public void Source_patterns_are_resolved_or_lowered_without_name_annotations(Abstract.Pattern source)
    {
        var name = DeclQualifiedName.Create(["Test"], "root");

        var function =
            new Abstract.Declaration.FunctionDeclaration(
                new(
                    null,
                    new Abstract.FunctionImplementation("root", [source], Integer(0))));

        var model =
            Core.Elm.ElmCompilerInDotnet.CompilerModel.FromCanonicalSyntax.Convert(
                new Dictionary<DeclQualifiedName, Abstract.Declaration> { [name] = function });

        Core.Elm.ElmCompilerInDotnet.CompilerModel.ProgramValidation.Validate(model);
        model.Functions.Should().ContainSingle();
    }

    [Theory]
    [MemberData(nameof(TypeAnnotations))]
    public void Source_type_annotations_are_consumed_before_backend_emission(Abstract.TypeAnnotation type)
    {
        var name = DeclQualifiedName.Create(["Test"], "root");

        var function =
            new Abstract.Declaration.FunctionDeclaration(
                new(
                    new Abstract.Signature("root", type),
                    new Abstract.FunctionImplementation("root", [], Integer(0))));

        var model =
            Core.Elm.ElmCompilerInDotnet.CompilerModel.FromCanonicalSyntax.Convert(
                new Dictionary<DeclQualifiedName, Abstract.Declaration> { [name] = function });

        model.Functions.Should().ContainSingle();
        typeof(IR.FunctionStruct).GetProperty("Signature").Should().BeNull();
    }

    [Fact]
    public void Conversion_fixtures_cover_every_expression_and_pattern_variant()
    {
        Expressions().Select(row => row[0].GetType()).Should().BeEquivalentTo(
            typeof(Abstract.Expression).GetNestedTypes());

        Patterns().Select(row => row[0].GetType()).Should().BeEquivalentTo(typeof(Abstract.Pattern).GetNestedTypes());
    }

    [Fact]
    public void Conversion_rejects_unknown_expression_variants_explicitly()
    {
        var convert = () => ConvertExpression(new UnknownExpression());
        convert.Should().Throw<NotImplementedException>().WithMessage("*does not handle*Unknown*");
    }

    private sealed record UnknownExpression : Abstract.Expression;
}
