using AwesomeAssertions;
using System.Collections.Immutable;
using Xunit;

using ST = Pine.Core.Elm.ElmCompilerInDotnet.StructuralType;
using STPrinting = Pine.Core.Elm.ElmCompilerInDotnet.StructuralTypePrinting;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.StructuralType;

/// <summary>
/// Tests for <see cref="Core.Elm.ElmCompilerInDotnet.StructuralTypePrinting"/> — pretty-printing structural types
/// in Elm-like syntax. Each test uses a full snapshot assertion on the printed string.
/// </summary>
public class StructuralTypePrintingTests
{
    [Fact]
    public void Print_Int()
    {
        var printed = STPrinting.Print(ST.IntType.Instance);

        printed.Should().Be("Int");
    }

    [Fact]
    public void Print_Float()
    {
        var printed = STPrinting.Print(ST.FloatType.Instance);

        printed.Should().Be("Float");
    }

    [Fact]
    public void Print_String()
    {
        var printed = STPrinting.Print(ST.StringType.Instance);

        printed.Should().Be("String");
    }

    [Fact]
    public void Print_Char()
    {
        var printed = STPrinting.Print(ST.CharType.Instance);

        printed.Should().Be("Char");
    }

    [Fact]
    public void Print_Bool()
    {
        var printed = STPrinting.Print(ST.BoolType.Instance);

        printed.Should().Be("Bool");
    }

    [Fact]
    public void Print_type_variable()
    {
        var printed = STPrinting.Print(new ST.TypeVariable(0));

        printed.Should().Be("a");
    }

    [Fact]
    public void Print_constrained_variable_number()
    {
        var printed =
            STPrinting.Print(
                new ST.ConstrainedVariable(0, ST.TypeConstraint.Number));

        printed.Should().Be("a");
    }

    [Fact]
    public void Print_Self_with_default_representation()
    {
        var printed = STPrinting.Print(ST.Self.Instance);

        printed.Should().Be("⟳");
    }

    [Fact]
    public void Print_Self_with_custom_representation()
    {
        var config = new STPrinting.PrintConfig(SelfRepresentation: "Self");

        var printed = STPrinting.Print(ST.Self.Instance, config);

        printed.Should().Be("Self");
    }

    [Fact]
    public void Print_Self_with_recursive_marker()
    {
        var config = new STPrinting.PrintConfig(SelfRepresentation: "<recursive>");

        var printed = STPrinting.Print(ST.Self.Instance, config);

        printed.Should().Be("<recursive>");
    }

    [Fact]
    public void Print_simple_function_type()
    {
        var type =
            new ST.FunctionType(
                ST.IntType.Instance,
                ST.StringType.Instance);

        var printed = STPrinting.Print(type);

        printed.Should().Be("Int -> String");
    }

    [Fact]
    public void Print_curried_function_type()
    {
        var type =
            new ST.FunctionType(
                ST.IntType.Instance,
                new ST.FunctionType(
                    ST.FloatType.Instance,
                    ST.StringType.Instance));

        var printed = STPrinting.Print(type);

        printed.Should().Be("Int -> Float -> String");
    }

    [Fact]
    public void Print_function_type_with_function_argument()
    {
        // (a -> b) -> List a -> List b
        var type =
            new ST.FunctionType(
                new ST.FunctionType(
                    new ST.TypeVariable(0),
                    new ST.TypeVariable(1)),
                new ST.FunctionType(
                    new ST.ListType(new ST.TypeVariable(0)),
                    new ST.ListType(new ST.TypeVariable(1))));

        var printed = STPrinting.Print(type);

        printed.Should().Be("(a -> b) -> List a -> List b");
    }

    [Fact]
    public void Print_List_type()
    {
        var type = new ST.ListType(ST.IntType.Instance);

        var printed = STPrinting.Print(type);

        printed.Should().Be("List Int");
    }

    [Fact]
    public void Print_nested_List_type()
    {
        var type =
            new ST.ListType(
                new ST.ListType(ST.IntType.Instance));

        var printed = STPrinting.Print(type);

        printed.Should().Be("List (List Int)");
    }

    [Fact]
    public void Print_tuple_single_line()
    {
        var type =
            ST.TupleType.Create(
                [ST.IntType.Instance, ST.StringType.Instance]);

        var printed = STPrinting.Print(type);

        printed.Should().Be("( Int, String )");
    }

    [Fact]
    public void Print_three_element_tuple()
    {
        var type =
            ST.TupleType.Create(
                [ST.IntType.Instance, ST.FloatType.Instance, ST.StringType.Instance]);

        var printed = STPrinting.Print(type);

        printed.Should().Be("( Int, Float, String )");
    }

    [Fact]
    public void Print_tuple_multiline_with_small_threshold()
    {
        var type =
            ST.TupleType.Create(
                [ST.IntType.Instance, ST.StringType.Instance]);

        var config = new STPrinting.PrintConfig(LineLengthThreshold: 10);

        var printed = STPrinting.Print(type, config);

        printed.Should().Be(
            """
            ( Int
            , String
            )
            """);
    }

    // === Nested tuples ===

    [Fact]
    public void Print_nested_tuple_single_line()
    {
        var type =
            ST.TupleType.Create(
                [
                ST.TupleType.Create([ST.IntType.Instance, ST.FloatType.Instance]),
                ST.StringType.Instance
                ]);

        var printed = STPrinting.Print(type);

        printed.Should().Be("( ( Int, Float ), String )");
    }

    [Fact]
    public void Print_nested_tuple_multiline()
    {
        var type =
            ST.TupleType.Create(
                [
                ST.TupleType.Create([ST.IntType.Instance, ST.FloatType.Instance]),
                ST.StringType.Instance
                ]);

        // Inner tuple "( Int, Float )" is 14 chars, fits in threshold 15
        // But outer "( ( Int, Float ), String )" is 26 chars, exceeds threshold
        var config = new STPrinting.PrintConfig(LineLengthThreshold: 15);

        var printed = STPrinting.Print(type, config);

        printed.Should().Be(
            """
            ( ( Int, Float )
            , String
            )
            """);
    }

    [Fact]
    public void Print_nested_tuple_both_multiline()
    {
        var type =
            ST.TupleType.Create(
                [
                ST.TupleType.Create([ST.IntType.Instance, ST.FloatType.Instance]),
                ST.StringType.Instance
                ]);

        // Threshold 10 forces both inner and outer tuples to go multiline
        var config = new STPrinting.PrintConfig(LineLengthThreshold: 10);

        var printed = STPrinting.Print(type, config);

        printed.Should().Be(
            """
            (
                ( Int
                , Float
                )
            , String
            )
            """);
    }

    [Fact]
    public void Print_nested_tuple_second_element_multiline()
    {
        var type =
            ST.TupleType.Create(
                [
                ST.IntType.Instance,
                ST.TupleType.Create([ST.StringType.Instance, ST.FloatType.Instance])
                ]);

        // Threshold 10 forces inner tuple "( String, Float )" (18 chars) to go multiline
        var config = new STPrinting.PrintConfig(LineLengthThreshold: 10);

        var printed = STPrinting.Print(type, config);

        printed.Should().Be(
            """
            ( Int
            ,
                ( String
                , Float
                )
            )
            """);
    }

    [Fact]
    public void Print_deeply_nested_tuple_multiline()
    {
        var type =
            ST.TupleType.Create(
                [
                ST.TupleType.Create(
                    [
                    ST.TupleType.Create([ST.IntType.Instance, ST.FloatType.Instance]),
                    ST.CharType.Instance
                    ]),
                ST.StringType.Instance
                ]);

        var config = new STPrinting.PrintConfig(LineLengthThreshold: 10);

        var printed = STPrinting.Print(type, config);

        printed.Should().Be(
            """
            (
                (
                    ( Int
                    , Float
                    )
                , Char
                )
            , String
            )
            """);
    }

    [Fact]
    public void Print_closed_record_single_line()
    {
        var type =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("age", ST.IntType.Instance)
                .Add("name", ST.StringType.Instance));

        var printed = STPrinting.Print(type);

        printed.Should().Be("{ age : Int, name : String }");
    }

    [Fact]
    public void Print_empty_closed_record()
    {
        var type = ST.ClosedRecord.Create(ImmutableDictionary<string, ST>.Empty);

        var printed = STPrinting.Print(type);

        printed.Should().Be("{}");
    }

    [Fact]
    public void Print_closed_record_multiline_with_small_threshold()
    {
        var type =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("age", ST.IntType.Instance)
                .Add("name", ST.StringType.Instance));

        var config = new STPrinting.PrintConfig(LineLengthThreshold: 15);

        var printed = STPrinting.Print(type, config);

        printed.Should().Be(
            """
            { age : Int
            , name : String
            }
            """);
    }

    [Fact]
    public void Print_closed_record_switches_layout_at_threshold()
    {
        var type =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("age", ST.IntType.Instance)
                .Add("name", ST.StringType.Instance));

        // "{ age : Int, name : String }" is 28 chars
        var singleLineResult = STPrinting.Print(type, new(LineLengthThreshold: 28));
        singleLineResult.Should().Be("{ age : Int, name : String }");

        var multiLineResult = STPrinting.Print(type, new(LineLengthThreshold: 27));

        multiLineResult.Should().Be(
            """
            { age : Int
            , name : String
            }
            """);
    }

    [Fact]
    public void Print_open_record_single_line()
    {
        var type =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance),
                0);

        var printed = STPrinting.Print(type);

        printed.Should().Be("{ a | name : String }");
    }

    [Fact]
    public void Print_open_record_with_no_fields()
    {
        var type =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty,
                0);

        var printed = STPrinting.Print(type);

        printed.Should().Be("{ a }");
    }

    [Fact]
    public void Print_open_record_multiline_with_small_threshold()
    {
        var type =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("age", ST.IntType.Instance)
                .Add("name", ST.StringType.Instance),
                0);

        var config = new STPrinting.PrintConfig(LineLengthThreshold: 15);

        var printed = STPrinting.Print(type, config);

        printed.Should().Be(
            """
            { a
            | age : Int
            , name : String
            }
            """);
    }

    [Fact]
    public void Print_choice_type_with_two_tags()
    {
        var type =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Just", [ST.IntType.Instance])
                .Add("Nothing", []));

        var printed = STPrinting.Print(type);

        printed.Should().Be(
            """
              Just Int
            | Nothing
            """);
    }

    [Fact]
    public void Print_choice_type_tags_in_alphabetical_order()
    {
        var type =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Ok", [ST.IntType.Instance])
                .Add("Err", [ST.StringType.Instance]));

        var printed = STPrinting.Print(type);

        printed.Should().Be(
            """
              Err String
            | Ok Int
            """);
    }

    [Fact]
    public void Print_choice_type_with_multiple_fields_per_tag()
    {
        var type =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Branch", [ST.Self.Instance, ST.IntType.Instance, ST.Self.Instance])
                .Add("Leaf", []));

        var printed = STPrinting.Print(type);

        printed.Should().Be(
            """
              Branch ⟳ Int ⟳
            | Leaf
            """);
    }

    [Fact]
    public void Print_choice_type_with_custom_Self_representation()
    {
        var type =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Cons", [ST.IntType.Instance, ST.Self.Instance])
                .Add("Nil", []));

        var config = new STPrinting.PrintConfig(SelfRepresentation: "Self");

        var printed = STPrinting.Print(type, config);

        printed.Should().Be(
            """
              Cons Int Self
            | Nil
            """);
    }

    [Fact]
    public void Print_choice_type_with_function_field_uses_parens()
    {
        var type =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add(
                    "Handler",
                    [
                    new ST.FunctionType(
                        ST.IntType.Instance,
                        ST.StringType.Instance)
                    ])
                .Add("None", []));

        var printed = STPrinting.Print(type);

        printed.Should().Be(
            """
              Handler (Int -> String)
            | None
            """);
    }

    [Fact]
    public void Print_record_fields_are_alphabetically_ordered()
    {
        var type =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("zebra", ST.IntType.Instance)
                .Add("alpha", ST.StringType.Instance)
                .Add("middle", ST.FloatType.Instance));

        var printed = STPrinting.Print(type);

        printed.Should().Be("{ alpha : String, middle : Float, zebra : Int }");
    }

    [Fact]
    public void Print_tuple_same_type_with_different_thresholds()
    {
        var type =
            ST.TupleType.Create(
                [ST.IntType.Instance, ST.FloatType.Instance, ST.StringType.Instance]);

        // "( Int, Float, String )" is 22 chars
        var wideConfig = new STPrinting.PrintConfig(LineLengthThreshold: 30);
        STPrinting.Print(type, wideConfig).Should().Be("( Int, Float, String )");

        var narrowConfig = new STPrinting.PrintConfig(LineLengthThreshold: 15);

        STPrinting.Print(type, narrowConfig).Should().Be(
            """
            ( Int
            , Float
            , String
            )
            """);
    }

    [Fact]
    public void Print_record_same_type_with_different_thresholds()
    {
        var type =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("x", ST.IntType.Instance)
                .Add("y", ST.FloatType.Instance));

        // "{ x : Int, y : Float }" is 22 chars
        var wideConfig = new STPrinting.PrintConfig(LineLengthThreshold: 25);
        STPrinting.Print(type, wideConfig).Should().Be("{ x : Int, y : Float }");

        var narrowConfig = new STPrinting.PrintConfig(LineLengthThreshold: 15);

        STPrinting.Print(type, narrowConfig).Should().Be(
            """
            { x : Int
            , y : Float
            }
            """);
    }

    [Fact]
    public void Print_complex_nested_type()
    {
        // List.map : (a -> b) -> List a -> List b
        var mapType =
            new ST.FunctionType(
                new ST.FunctionType(
                    new ST.TypeVariable(0),
                    new ST.TypeVariable(1)),
                new ST.FunctionType(
                    new ST.ListType(new ST.TypeVariable(0)),
                    new ST.ListType(new ST.TypeVariable(1))));

        var printed = STPrinting.Print(mapType);

        printed.Should().Be("(a -> b) -> List a -> List b");
    }

    [Fact]
    public void Print_single_tag_choice()
    {
        var type =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Meters", [ST.FloatType.Instance]));

        var printed = STPrinting.Print(type);

        printed.Should().Be("  Meters Float");
    }

    [Fact]
    public void Print_List_of_function_type()
    {
        var type =
            new ST.ListType(
                new ST.FunctionType(
                    ST.IntType.Instance,
                    ST.StringType.Instance));

        var printed = STPrinting.Print(type);

        printed.Should().Be("List (Int -> String)");
    }

    // === Nested records ===

    [Fact]
    public void Print_nested_record_single_line()
    {
        var type =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add(
                    "inner",
                    ST.ClosedRecord.Create(
                        ImmutableDictionary<string, ST>.Empty
                        .Add("x", ST.IntType.Instance)))
                .Add("name", ST.StringType.Instance));

        var printed = STPrinting.Print(type);

        printed.Should().Be("{ inner : { x : Int }, name : String }");
    }

    [Fact]
    public void Print_nested_record_multiline()
    {
        var type =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add(
                    "inner",
                    ST.ClosedRecord.Create(
                        ImmutableDictionary<string, ST>.Empty
                        .Add("x", ST.IntType.Instance)
                        .Add("y", ST.FloatType.Instance)))
                .Add("name", ST.StringType.Instance));

        var config = new STPrinting.PrintConfig(LineLengthThreshold: 20);

        var printed = STPrinting.Print(type, config);

        printed.Should().Be(
            """
            { inner :
                { x : Int
                , y : Float
                }
            , name : String
            }
            """);
    }

    [Fact]
    public void Print_deeply_nested_record_multiline()
    {
        var type =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add(
                    "a",
                    ST.ClosedRecord.Create(
                        ImmutableDictionary<string, ST>.Empty
                        .Add("b", ST.IntType.Instance)
                        .Add("c", ST.StringType.Instance)))
                .Add("d", ST.FloatType.Instance));

        var config = new STPrinting.PrintConfig(LineLengthThreshold: 10);

        var printed = STPrinting.Print(type, config);

        printed.Should().Be(
            """
            { a :
                { b : Int
                , c : String
                }
            , d : Float
            }
            """);
    }

    // === Nested choice types ===

    [Fact]
    public void Print_choice_type_with_nested_choice_field()
    {
        // A choice type where one tag has an inline choice type as a field.
        // Since Elm has no syntax for inline anonymous choice types, we use
        // parenthesized multiline layout to distinguish nested choice from the outer one.
        var innerChoice =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Left", [ST.IntType.Instance])
                .Add("Right", [ST.StringType.Instance]));

        var outerChoice =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Wrapper", [innerChoice])
                .Add("None", []));

        var printed = STPrinting.Print(outerChoice);

        printed.Should().Be(
            """
              None
            | Wrapper (Left Int | Right String)
            """);
    }

    // === Type application (concrete instances of generic types) ===

    [Fact]
    public void Print_type_application_Maybe_Int()
    {
        // Maybe a = Just a | Nothing
        // Concrete: Maybe Int → substitutes to Just Int | Nothing
        var maybeGeneric =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Just", [new ST.TypeVariable(0)])
                .Add("Nothing", []));

        var type =
            new ST.TypeApplication(
                maybeGeneric,
                [ST.IntType.Instance]);

        var printed = STPrinting.Print(type);

        // After substitution, prints identically to a concrete ChoiceType
        printed.Should().Be(
            """
              Just Int
            | Nothing
            """);
    }

    [Fact]
    public void Print_type_application_Result_String_Int()
    {
        // Result error value = Err error | Ok value
        // Concrete: Result String Int → substitutes to Err String | Ok Int
        // Type parameter order is by first appearance: index 0 (from Err), then index 1 (from Ok)
        var resultGeneric =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Err", [new ST.TypeVariable(0)])
                .Add("Ok", [new ST.TypeVariable(1)]));

        var type =
            new ST.TypeApplication(
                resultGeneric,
                [ST.StringType.Instance, ST.IntType.Instance]);

        var printed = STPrinting.Print(type);

        printed.Should().Be(
            """
              Err String
            | Ok Int
            """);
    }

    [Fact]
    public void Print_type_application_in_parens_context()
    {
        // List (Maybe Int) → List (Just Int | Nothing)
        var maybeGeneric =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Just", [new ST.TypeVariable(0)])
                .Add("Nothing", []));

        var maybeInt =
            new ST.TypeApplication(
                maybeGeneric,
                [ST.IntType.Instance]);

        var type = new ST.ListType(maybeInt);

        var printed = STPrinting.Print(type);

        // In parens context, the substituted choice type prints inline
        printed.Should().Be("List (Just Int | Nothing)");
    }
}
