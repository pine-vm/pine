using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mConcretized;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

using Location = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Location;

using Range = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Range;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mConcretized;

public class SetTypeAnnotationTests
{
    [Fact]
    public void Set_simplest_type_annotation()
    {
        var moduleText =
            """"
            module Test exposing (..)


            foo =
                a

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["foo"],
                TypedAnnotation("Int"));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            foo : Int
            foo =
                a

            """".Trim());
    }

    [Fact]
    public void Replace_simplest_type_annotation()
    {
        var moduleText =
            """"
            module Test exposing (..)


            foo : String
            foo =
                a

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["foo"],
                TypedAnnotation("Int"));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            foo : Int
            foo =
                a

            """".Trim());
    }

    [Fact]
    public void Set_type_annotation_for_function_with_parameter()
    {
        var moduleText =
            """"
            module Test exposing (..)


            add x =
                x + 1

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["add"],
                FunctionAnnotation(TypedAnnotation("Int"), TypedAnnotation("Int")));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            add : Int -> Int
            add x =
                x + 1

            """".Trim());
    }

    [Fact]
    public void Set_type_annotation_in_let_block()
    {
        var moduleText =
            """"
            module Test exposing (..)


            foo =
                let
                    helper =
                        1
                in
                helper

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["foo"],
                TypedAnnotation("Int"))
            .Add(
                ["foo", "helper"],
                TypedAnnotation("Int"));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            foo : Int
            foo =
                let
                    helper : Int
                    helper =
                        1
                in
                helper

            """".Trim());
    }

    [Fact]
    public void Set_type_annotation_in_nested_let_blocks()
    {
        var moduleText =
            """"
            module Test exposing (..)


            foo =
                let
                    outer =
                        let
                            inner =
                                1
                        in
                        inner
                in
                outer

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["foo"],
                TypedAnnotation("Int"))
            .Add(
                ["foo", "outer"],
                TypedAnnotation("Int"))
            .Add(
                ["foo", "outer", "inner"],
                TypedAnnotation("Int"));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            foo : Int
            foo =
                let
                    outer : Int
                    outer =
                        let
                            inner : Int
                            inner =
                                1
                        in
                        inner
                in
                outer

            """".Trim());
    }

    [Fact]
    public void Set_type_annotations_for_multiple_declarations()
    {
        var moduleText =
            """"
            module Test exposing (..)


            foo =
                a


            bar =
                b

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["foo"],
                TypedAnnotation("Int"))
            .Add(
                ["bar"],
                TypedAnnotation("String"));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            foo : Int
            foo =
                a


            bar : String
            bar =
                b

            """".Trim());
    }

    [Fact]
    public void Set_complex_function_type_annotation()
    {
        var moduleText =
            """"
            module Test exposing (..)


            transform f x =
                f x

            """";

        // (a -> b) -> a -> b - need to wrap the first function type in Tupled for parentheses
        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["transform"],
                FunctionAnnotation(
                    TupledAnnotation(FunctionAnnotation(GenericAnnotation("a"), GenericAnnotation("b"))),
                    FunctionAnnotation(GenericAnnotation("a"), GenericAnnotation("b"))));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        // Note: The formatter renders the function type with parentheses when wrapped in Tupled
        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            transform : (a -> b) -> a -> b
            transform f x =
                f x

            """".Trim());
    }

    [Fact]
    public void Set_type_annotation_with_type_parameters()
    {
        var moduleText =
            """"
            module Test exposing (..)


            identity x =
                x

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["identity"],
                FunctionAnnotation(GenericAnnotation("a"), GenericAnnotation("a")));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            identity : a -> a
            identity x =
                x

            """".Trim());
    }

    [Fact]
    public void Set_type_annotation_with_list_type()
    {
        var moduleText =
            """"
            module Test exposing (..)


            numbers =
                [ 1, 2, 3 ]

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["numbers"],
                TypedAnnotation("List", TypedAnnotation("Int")));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            numbers : List Int
            numbers =
                [ 1, 2, 3 ]

            """".Trim());
    }

    [Fact]
    public void Set_record_type_annotation()
    {
        var moduleText =
            """"
            module Test exposing (..)


            person =
                abcd

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["person"],
                RecordAnnotation(
                    ("name", TypedAnnotation("String")),
                    ("age", TypedAnnotation("Int"))));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            person : { name : String, age : Int }
            person =
                abcd

            """".Trim());
    }

    [Fact]
    public void Set_type_annotation_in_chain_of_let_blocks()
    {
        var moduleText =
            """"
            module Test exposing (..)


            foo =
                let
                    first =
                        1
                in
                let
                    second =
                        2
                in
                first + second

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["foo"],
                TypedAnnotation("Int"))
            .Add(
                ["foo", "first"],
                TypedAnnotation("Int"))
            .Add(
                ["foo", "second"],
                TypedAnnotation("Int"));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            foo : Int
            foo =
                let
                    first : Int
                    first =
                        1
                in
                let
                    second : Int
                    second =
                        2
                in
                first + second

            """".Trim());
    }

    [Fact]
    public void Set_type_annotation_with_recursion_into_if_block()
    {
        var moduleText =
            """"
            module Test exposing (..)


            foo x =
                if x > 0 then
                    let
                        helper =
                            x - 1
                    in
                    helper
                else
                    0

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["foo"],
                FunctionAnnotation(TypedAnnotation("Int"), TypedAnnotation("Int")))
            .Add(
                ["foo", "helper"],
                TypedAnnotation("Int"));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            foo : Int -> Int
            foo x =
                if x > 0 then
                    let
                        helper : Int
                        helper =
                            x - 1
                    in
                    helper

                else
                    0

            """".Trim());
    }

    [Fact]
    public void Set_type_annotation_with_recursion_into_if_block_chain()
    {
        var moduleText =
            """"
            module Test exposing (..)


            classify x =
                if x < 0 then
                    let
                        negative =
                            "negative"
                    in
                    negative
                else if x == 0 then
                    let
                        zero =
                            "zero"
                    in
                    zero
                else
                    let
                        positive =
                            "positive"
                    in
                    positive

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["classify"],
                FunctionAnnotation(TypedAnnotation("Int"), TypedAnnotation("String")))
            .Add(
                ["classify", "negative"],
                TypedAnnotation("String"))
            .Add(
                ["classify", "zero"],
                TypedAnnotation("String"))
            .Add(
                ["classify", "positive"],
                TypedAnnotation("String"));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            classify : Int -> String
            classify x =
                if x < 0 then
                    let
                        negative : String
                        negative =
                            "negative"
                    in
                    negative

                else if x == 0 then
                    let
                        zero : String
                        zero =
                            "zero"
                    in
                    zero

                else
                    let
                        positive : String
                        positive =
                            "positive"
                    in
                    positive

            """".Trim());
    }

    [Fact]
    public void Set_type_annotation_for_local_function_in_let_block()
    {
        var moduleText =
            """"
            module Test exposing (..)


            foo x =
                let
                    double y =
                        y * 2
                in
                double x

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["foo"],
                FunctionAnnotation(TypedAnnotation("Int"), TypedAnnotation("Int")))
            .Add(
                ["foo", "double"],
                FunctionAnnotation(TypedAnnotation("Int"), TypedAnnotation("Int")));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            foo : Int -> Int
            foo x =
                let
                    double : Int -> Int
                    double y =
                        y * 2
                in
                double x

            """".Trim());
    }

    [Fact]
    public void Set_type_annotation_with_recursion_into_case_block()
    {
        var moduleText =
            """"
            module Test exposing (..)


            describe maybeValue =
                case maybeValue of
                    Just value ->
                        let
                            prefix =
                                "Value: "
                        in
                        prefix ++ String.fromInt value

                    Nothing ->
                        let
                            empty =
                                "Nothing"
                        in
                        empty

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["describe"],
                FunctionAnnotation(
                    TypedAnnotation("Maybe", TypedAnnotation("Int")),
                    TypedAnnotation("String")))
            .Add(
                ["describe", "prefix"],
                TypedAnnotation("String"))
            .Add(
                ["describe", "empty"],
                TypedAnnotation("String"));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            describe : Maybe Int -> String
            describe maybeValue =
                case maybeValue of
                    Just value ->
                        let
                            prefix : String
                            prefix =
                                "Value: "
                        in
                        prefix ++ String.fromInt value

                    Nothing ->
                        let
                            empty : String
                            empty =
                                "Nothing"
                        in
                        empty

            """".Trim());
    }

    [Fact]
    public void Set_type_annotation_with_nested_local_functions_in_let_blocks()
    {
        var moduleText =
            """"
            module Test exposing (..)


            compute x =
                let
                    outer y =
                        let
                            inner z =
                                z * 2
                        in
                        inner y + 1
                in
                outer x

            """";

        var declarationsTypeAnnotations =
            ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation>.Empty
            .WithComparers(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Add(
                ["compute"],
                FunctionAnnotation(TypedAnnotation("Int"), TypedAnnotation("Int")))
            .Add(
                ["compute", "outer"],
                FunctionAnnotation(TypedAnnotation("Int"), TypedAnnotation("Int")))
            .Add(
                ["compute", "outer", "inner"],
                FunctionAnnotation(TypedAnnotation("Int"), TypedAnnotation("Int")));

        var resultText =
            SetTypeAnnotationsAndFormatToString(
                moduleText,
                declarationsTypeAnnotations);

        resultText.Trim().Should().Be(
            """"
            module Test exposing (..)


            compute : Int -> Int
            compute x =
                let
                    outer : Int -> Int
                    outer y =
                        let
                            inner : Int -> Int
                            inner z =
                                z * 2
                        in
                        inner y + 1
                in
                outer x

            """".Trim());
    }

    private static TypeAnnotation.Typed TypedAnnotation(
        string typeName,
        params TypeAnnotation[] typeArguments)
    {
        return new TypeAnnotation.Typed
        (
            TypeName:
                MakeNode<(IReadOnlyList<string> ModuleName, string Name)>(
                    (ModuleName: Array.Empty<string>(), Name: typeName),
                    s_rangeZero),

            TypeArguments:
                [.. typeArguments.Select(ta => MakeNode(ta, s_rangeZero))]
        );
    }

    private static TypeAnnotation.FunctionTypeAnnotation FunctionAnnotation(
        TypeAnnotation argumentType,
        TypeAnnotation returnType)
    {
        return new TypeAnnotation.FunctionTypeAnnotation(
            ArgumentType: MakeNode(argumentType, s_rangeZero),
            ArrowLocation: s_locationZero,
            ReturnType: MakeNode(returnType, s_rangeZero));
    }

    private static TypeAnnotation.Tupled TupledAnnotation(params TypeAnnotation[] elements)
    {
        if (elements.Length is 0)
        {
            return new TypeAnnotation.Tupled(
                OpenParenLocation: s_locationZero,
                TypeAnnotations: new SeparatedSyntaxList<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Node<TypeAnnotation>>.Empty(),
                CloseParenLocation: s_locationZero);
        }

        var first =
            MakeNode(elements[0], s_rangeZero);

        var rest =
            elements.Skip(1)
            .Select(e => (s_locationZero, MakeNode(e, s_rangeZero)))
            .ToList();

        return new TypeAnnotation.Tupled(
            OpenParenLocation: s_locationZero,
            TypeAnnotations: new SeparatedSyntaxList<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Node<TypeAnnotation>>.NonEmpty(first, rest),
            CloseParenLocation: s_locationZero);
    }

    private static TypeAnnotation.GenericType GenericAnnotation(string name)
    {
        return new TypeAnnotation.GenericType(name);
    }

    private static TypeAnnotation.Record RecordAnnotation(params (string name, TypeAnnotation type)[] fields)
    {
        if (fields.Length is 0)
        {
            return new TypeAnnotation.Record(
                OpenBraceLocation: s_locationZero,
                RecordDefinition: new RecordDefinition(
                    Fields: new SeparatedSyntaxList<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Node<RecordField>>.Empty()),
                CloseBraceLocation: s_locationZero);
        }

        var firstField = new RecordField(
            FieldName: MakeNode(fields[0].name, s_rangeZero),
            ColonLocation: s_locationZero,
            FieldType: MakeNode(fields[0].type, s_rangeZero));

        var restFields = fields.Skip(1)
            .Select(f => (s_locationZero, MakeNode(new RecordField(
                FieldName: MakeNode(f.name, s_rangeZero),
                ColonLocation: s_locationZero,
                FieldType: MakeNode(f.type, s_rangeZero)), s_rangeZero)))
            .ToList();

        return new TypeAnnotation.Record(
            OpenBraceLocation: s_locationZero,
            RecordDefinition: new RecordDefinition(
                Fields: new SeparatedSyntaxList<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Node<RecordField>>.NonEmpty(
                    MakeNode(firstField, s_rangeZero), restFields)),
            CloseBraceLocation: s_locationZero);
    }

    private static readonly Location s_locationZero = new(0, 0);

    private static readonly Range s_rangeZero = new(s_locationZero, s_locationZero);

    private static Core.Elm.ElmSyntax.Stil4mElmSyntax7.Node<T> MakeNode<T>(
        T value,
        Range range)
    {
        return new Core.Elm.ElmSyntax.Stil4mElmSyntax7.Node<T>(range, value);
    }

    private static string SetTypeAnnotationsAndFormatToString(
        string elmModuleText,
        ImmutableDictionary<IReadOnlyList<string>, TypeAnnotation> declarationsTypeAnnotations)
    {
        var fileBefore =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new Exception($"Parsing failed: {err}"));

        var fileAfter =
            SetTypeAnnotation.SetTypeAnnotations(
                fileBefore,
                declarationsTypeAnnotations,
                declarationWithoutEntry:
                (declarationPath) =>
                {
                    throw new Exception($"Declaration without entry: {string.Join(".", declarationPath)}");
                },
                entryWithoutDeclaration:
                (entryPath) =>
                {
                    throw new Exception($"Entry without declaration: {string.Join(".", entryPath)}");
                });

        return Core.Elm.ElmSyntax.Stil4mConcretized.Avh4Format.FormatToString(fileAfter);
    }
}
