using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using Xunit;

using AbstractSyntax = Pine.Core.Elm.Syntax.Abstract;
using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;
using InfixDirection = Pine.Core.Elm.Syntax.Concrete.InfixDirection;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

public class FromCanonicalSyntaxTests
{
    private static AbstractSyntax.Expression Integer(int value) =>
        new AbstractSyntax.Expression.IntegerLiteral(value, IntegerEncoding.EncodeSignedInteger(value));

    private static readonly AbstractSyntax.TypeAnnotation s_generic =
        new AbstractSyntax.TypeAnnotation.GenericType("a");

    private static readonly AbstractSyntax.RecordDefinition s_recordDefinition =
        new(
            [
            AbstractSyntax.RecordField.Create("second", new AbstractSyntax.TypeAnnotation.Typed(["Basics"], "Int", [])),
            AbstractSyntax.RecordField.Create("first", s_generic)
            ]);

    private static readonly AbstractSyntax.FunctionStruct s_function =
        new(
            new AbstractSyntax.Signature(
                "function",
                new AbstractSyntax.TypeAnnotation.FunctionTypeAnnotation(s_generic, s_generic)),
            new AbstractSyntax.FunctionImplementation(
                "function",
                [
                new AbstractSyntax.Pattern.VarPattern("value")
                ],
                AbstractSyntax.Expression.Identifier.Create([], "value")));

    public static IEnumerable<object[]> Expressions()
    {
        AbstractSyntax.Expression[] expressions =
            [
            AbstractSyntax.Expression.UnitExpr.Instance,
            AbstractSyntax.Expression.StringLiteral.Create("line\n\u03bb"),
            AbstractSyntax.Expression.CharLiteral.Create(0x1F600),
            new AbstractSyntax.Expression.IntegerLiteral(
                BigInteger.One << 100,
                IntegerEncoding.EncodeSignedInteger(BigInteger.One << 100)),
            new AbstractSyntax.Expression.FloatLiteral(-17, 8),
            new AbstractSyntax.Expression.Negation(Integer(3)),
            new AbstractSyntax.Expression.ListExpr([Integer(2), Integer(1)]),
            AbstractSyntax.Expression.Identifier.Create(["Some", "Module"], "value"),
            new AbstractSyntax.Expression.IfBlock(
                AbstractSyntax.Expression.Identifier.Create(["Basics"], "True"),
                Integer(1),
                Integer(2)),
            new AbstractSyntax.Expression.PrefixOperator("(++)"),
            new AbstractSyntax.Expression.Application(
                AbstractSyntax.Expression.Identifier.Create([], "f"),
                [
                Integer(1),
                Integer(2)
                ]),
            new AbstractSyntax.Expression.OperatorApplication("+", InfixDirection.Right, Integer(1), Integer(2)),
            new AbstractSyntax.Expression.TupledExpression(
                [
                Integer(2),
                AbstractSyntax.Expression.StringLiteral.Create("first")
                ]),
            new AbstractSyntax.Expression.LambdaExpression(
                [
                new AbstractSyntax.Pattern.TuplePattern(
                    [
                    new AbstractSyntax.Pattern.VarPattern("x"),
                    new AbstractSyntax.Pattern.AllPattern()
                    ])
                ],
                AbstractSyntax.Expression.Identifier.Create([], "x")),
            new AbstractSyntax.Expression.CaseExpression(
                AbstractSyntax.Expression.Identifier.Create([], "x"),
                [
                new AbstractSyntax.Case(new AbstractSyntax.Pattern.IntPattern(0), Integer(2)),
                new AbstractSyntax.Case(new AbstractSyntax.Pattern.AllPattern(), Integer(1))
                ]),
            new AbstractSyntax.Expression.LetExpression(
                [
                new AbstractSyntax.LetDeclaration.LetFunction(s_function with { Signature = null }),
                new AbstractSyntax.LetDeclaration.LetDestructuring(
                    new AbstractSyntax.Pattern.VarPattern("x"),
                    new AbstractSyntax.Expression.Application(
                        AbstractSyntax.Expression.Identifier.Create([], "function"),
                        [
                        Integer(1)
                        ]))
                ],
                AbstractSyntax.Expression.Identifier.Create([], "x")),
            new AbstractSyntax.Expression.RecordExpr(
                [
                new AbstractSyntax.RecordSetter("z", StringEncoding.ValueFromString("z"), Integer(2)),
                new AbstractSyntax.RecordSetter("a", StringEncoding.ValueFromString("a"), Integer(1))
                ]),
            new AbstractSyntax.Expression.RecordAccess(
                new AbstractSyntax.Expression.Application(
                    AbstractSyntax.Expression.Identifier.Create([], "makeRecord"),
                    [
                    Integer(1)
                    ]),
                "field",
                StringEncoding.ValueFromString("field")),
            new AbstractSyntax.Expression.RecordAccessFunction("field", StringEncoding.ValueFromString("field")),
            new AbstractSyntax.Expression.RecordUpdateExpression(
                "record",
                [
                new AbstractSyntax.RecordSetter("z", StringEncoding.ValueFromString("z"), Integer(2)),
                new AbstractSyntax.RecordSetter("a", StringEncoding.ValueFromString("a"), Integer(1))
                ]),
            new AbstractSyntax.Expression.GLSLExpression("void main() { }")
            ];

        return expressions.Select(expression => new object[] { expression });
    }

    public static IEnumerable<object[]> Patterns()
    {
        AbstractSyntax.Pattern[] patterns =
            [
            new AbstractSyntax.Pattern.AllPattern(),
            new AbstractSyntax.Pattern.VarPattern("value"),
            new AbstractSyntax.Pattern.UnitPattern(),
            new AbstractSyntax.Pattern.CharPattern(0x1F600),
            new AbstractSyntax.Pattern.StringPattern("text\n"),
            new AbstractSyntax.Pattern.IntPattern(BigInteger.One << 100),
            new AbstractSyntax.Pattern.FloatPattern(1.25),
            new AbstractSyntax.Pattern.TuplePattern(
                [
                new AbstractSyntax.Pattern.VarPattern("a"),
                new AbstractSyntax.Pattern.AllPattern()
                ]),
            AbstractSyntax.Pattern.RecordPattern.Create(["z", "a"]),
            new AbstractSyntax.Pattern.UnConsPattern(
                new AbstractSyntax.Pattern.VarPattern("head"),
                new AbstractSyntax.Pattern.VarPattern("tail")),
            new AbstractSyntax.Pattern.ListPattern(
                [
                new AbstractSyntax.Pattern.IntPattern(1),
                new AbstractSyntax.Pattern.VarPattern("second")
                ]),
            new AbstractSyntax.Pattern.NamedPattern(
                new AbstractSyntax.QualifiedNameRef(["Some", "Module"], "Tag"),
                [
                new AbstractSyntax.Pattern.TuplePattern(
                    [
                    new AbstractSyntax.Pattern.VarPattern("x"),
                    new AbstractSyntax.Pattern.AllPattern()
                    ])
                ]),
            new AbstractSyntax.Pattern.AsPattern(new AbstractSyntax.Pattern.VarPattern("inner"), "outer")
            ];

        return patterns.Select(pattern => new object[] { pattern });
    }

    public static IEnumerable<object[]> TypeAnnotations()
    {
        AbstractSyntax.TypeAnnotation[] annotations =
            [
            s_generic,
            new AbstractSyntax.TypeAnnotation.Typed(
                [
                "Some",
                "Module"
                ],
                "Container",
                [
                s_generic,
                new AbstractSyntax.TypeAnnotation.Unit()
                ]),
            new AbstractSyntax.TypeAnnotation.Unit(),
            new AbstractSyntax.TypeAnnotation.Tupled([s_generic, new AbstractSyntax.TypeAnnotation.Unit()]),
            new AbstractSyntax.TypeAnnotation.Record(s_recordDefinition),
            new AbstractSyntax.TypeAnnotation.GenericRecord("row", s_recordDefinition),
            new AbstractSyntax.TypeAnnotation.FunctionTypeAnnotation(
                s_generic,
                new AbstractSyntax.TypeAnnotation.Record(s_recordDefinition))
            ];

        return annotations.Select(annotation => new object[] { annotation });
    }

    private static Core.Elm.ElmCompilerInDotnet.CompilerModel.Program ConvertExpression(AbstractSyntax.Expression body)
    {
        var name = DeclQualifiedName.Create(["Test"], "root");

        var function =
            new AbstractSyntax.Declaration.FunctionDeclaration(
                new(
                    null,
                    new AbstractSyntax.FunctionImplementation(
                        "root",
                        [
                            new AbstractSyntax.Pattern.VarPattern("f"),
                            new AbstractSyntax.Pattern.VarPattern("x"), new AbstractSyntax.Pattern.VarPattern("record")
                        ],
                        body)));

        return
            Core.Elm.ElmCompilerInDotnet.CompilerModel.FromCanonicalSyntax.Convert(
                new Dictionary<DeclQualifiedName, AbstractSyntax.Declaration> { [name] = function });
    }

    [Theory]
    [MemberData(nameof(Expressions))]
    public void Source_expression_conversion_resolves_every_variant(AbstractSyntax.Expression source)
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
    public void Source_patterns_are_resolved_or_lowered_without_name_annotations(AbstractSyntax.Pattern source)
    {
        var name = DeclQualifiedName.Create(["Test"], "root");

        var function =
            new AbstractSyntax.Declaration.FunctionDeclaration(
                new(
                    null,
                    new AbstractSyntax.FunctionImplementation("root", [source], Integer(0))));

        var model =
            Core.Elm.ElmCompilerInDotnet.CompilerModel.FromCanonicalSyntax.Convert(
                new Dictionary<DeclQualifiedName, AbstractSyntax.Declaration> { [name] = function });

        Core.Elm.ElmCompilerInDotnet.CompilerModel.ProgramValidation.Validate(model);
        model.Functions.Should().ContainSingle();
    }

    [Theory]
    [MemberData(nameof(TypeAnnotations))]
    public void Source_type_annotations_are_consumed_before_backend_emission(AbstractSyntax.TypeAnnotation type)
    {
        var name = DeclQualifiedName.Create(["Test"], "root");

        var function =
            new AbstractSyntax.Declaration.FunctionDeclaration(
                new(
                    new AbstractSyntax.Signature("root", type),
                    new AbstractSyntax.FunctionImplementation("root", [], Integer(0))));

        var model =
            Core.Elm.ElmCompilerInDotnet.CompilerModel.FromCanonicalSyntax.Convert(
                new Dictionary<DeclQualifiedName, AbstractSyntax.Declaration> { [name] = function });

        model.Functions.Should().ContainSingle();
        typeof(IR.FunctionStruct).GetProperty("Signature").Should().BeNull();
    }

    [Fact]
    public void Conversion_fixtures_cover_every_expression_and_pattern_variant()
    {
        Expressions().Select(row => row[0].GetType()).Should().BeEquivalentTo(
            typeof(AbstractSyntax.Expression).GetNestedTypes());

        Patterns().Select(row => row[0].GetType()).Should().BeEquivalentTo(
            typeof(AbstractSyntax.Pattern).GetNestedTypes());
    }

    [Fact]
    public void Conversion_rejects_unknown_expression_variants_explicitly()
    {
        var convert = () => ConvertExpression(new UnknownExpression());
        convert.Should().Throw<NotImplementedException>().WithMessage("*does not handle*Unknown*");
    }

    private sealed record UnknownExpression : AbstractSyntax.Expression;
}
