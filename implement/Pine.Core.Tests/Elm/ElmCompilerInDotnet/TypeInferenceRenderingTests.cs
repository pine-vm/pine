using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using System.Collections.Generic;
using Xunit;

using Type = Pine.Core.Elm.ElmCompilerInDotnet.TypeInference.InferredType;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class TypeInferenceRenderingTests
{
    [Fact]
    public void Inferred_types_render_as_Elm_type_annotations_with_correct_parentheses()
    {
        IReadOnlyList<(Type Type, string Expected)> cases =
            [
            (new Type.IntType(), "Int"),
            (new Type.FloatType(), "Float"),
            (new Type.StringType(), "String"),
            (new Type.CharType(), "Char"),
            (new Type.BoolType(), "Bool"),
            (new Type.NumberType(), "number"),
            (new Type.TypeVariable("a"), "a"),
            (new Type.TypeVariable("internal", TypeVariableConstraint.Comparable), "comparable"),
            (new Type.TypeVariable("internal", TypeVariableConstraint.Appendable), "appendable"),
            (new Type.TupleType([]), "()"),
            (new Type.TupleType([new Type.StringType(), new Type.IntType()]), "(String, Int)"),
            (new Type.RecordType([]), "{  }"),
            (new Type.RecordType([("name", new Type.StringType())]), "{ name : String }"),
            (new Type.OpenRecordType("r", [("name", new Type.StringType())]), "{ r | name : String }"),
            (new Type.ListType(new Type.ListType(new Type.IntType())), "List (List Int)"),
            (new Type.ListType(new Type.FunctionType(new Type.IntType(), new Type.StringType())), "List (Int -> String)"),
            (new Type.FunctionType(
                new Type.FunctionType(new Type.StringType(), new Type.IntType()),
                new Type.FunctionType(new Type.IntType(), new Type.StringType())),
            "(String -> Int) -> Int -> String"),
            (new Type.ChoiceType(
                ["Result"],
                "Result",
                [
                new Type.ListType(new Type.IntType()),
                new Type.ChoiceType(["Maybe"], "Maybe", [new Type.StringType()])
                ]),
            "Result.Result (List Int) (Maybe.Maybe String)")
            ];

        foreach (var (type, expected) in cases)
        {
            var rendered = TypeInference.RenderInferredType(type);
            rendered.Should().Be(expected);

            ElmSyntaxParser.ParseModuleText("module Test exposing (..)\n\nvalue : " + rendered + "\nvalue = 0\n")
                .IsOk().Should().BeTrue(rendered);
        }
    }

    [Fact]
    public void Inferred_type_rendering_marks_undetermined_types_and_supports_source_type_names()
    {
        TypeInference.RenderInferredType(new Type.UnknownType()).Should().Be("?");

        var type =
            new Type.ChoiceType(
                ["Canonical", "Module"],
                "Box",
                [new Type.ChoiceType(["Canonical", "Module"], "Box", [new Type.StringType()])]);

        TypeInference.RenderInferredType(type, choice => choice.TypeName).Should().Be("Box (Box String)");
    }
}
