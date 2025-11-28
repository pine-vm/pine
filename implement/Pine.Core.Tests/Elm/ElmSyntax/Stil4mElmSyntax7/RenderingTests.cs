using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mElmSyntax7;

public class RenderingTests
{
    private static readonly Rendering.Config s_renderingDefaultConfig =
        Rendering.ConfigNormalizeAllLocations(
            Rendering.LineBreakingConfig.SnapshotTestsDefault);

    private static string RenderDefault(File file) =>
        Rendering.ToString(
            file,
            s_renderingDefaultConfig);

    [Fact]
    public void ToString_EmptyFile()
    {
        var file =
            new File(
                ModuleDefinition:
                NodeWithRangeZero<Module>(
                    new Module.NormalModule(
                        ModuleData: new DefaultModuleData(
                            ModuleName: NodeWithRangeZero((IReadOnlyList<string>)["Test"]),
                            ExposingList: NodeWithRangeZero<Exposing>(
                                new Exposing.All(s_fakeRangeZero))
                    )
                )),
                Imports: [],
                Declarations: [],
                Comments: []);

        var rendered = RenderDefault(file);

        rendered.Trim().Should().Be("module Test exposing (..)");
    }

    [Fact]
    public void Parse_and_render_scenarios()
    {
        var testCases = new[]
        {
            new
            {
                Input =
                """
                module Test exposing (..)
                """,

                Expected =
                """
                module Test exposing (..)
                """
            },

            new
            {
                Input =
                """
                module   Test   exposing   (  ..  )
                """,

                Expected =
                """
                module Test exposing (..)
                """
            },

            new
            {
                Input =
                """
                module Test exposing (..)

                import Html exposing (text)

                main =
                    text "Hello, World!"

                beta = 42
                """,

                Expected =
                """
                module Test exposing (..)

                import Html exposing (text)


                main =
                    text
                        "Hello, World!"


                beta =
                    42
                """
            },

            new
            {
                Input =
                """
                module Test exposing (..)


                sketch a =
                    func "test" (a + 3)

                """,

                Expected =
                """
                module Test exposing (..)


                sketch a =
                    func
                        "test"
                        (a + 3)
                """,
            },
        };

        for (var i = 0; i < testCases.Length; i++)
        {
            var testCase = testCases[i];

            try
            {
                var parsed =
                    ElmSyntaxParser.ParseModuleText(testCase.Input.TrimStart())
                    .Extract(err => throw new System.Exception("Parsing failed: " + err.ToString()));

                var rendered = RenderDefault(parsed);

                rendered.Trim().Should().Be(testCase.Expected.Trim());
            }
            catch (System.Exception e)
            {
                throw new System.Exception(
                    $"Test case {i} failed. Input:\n{testCase.Input}", e);
            }
        }
    }

    [Fact]
    public void Roundtrip_tests()
    {
        var testCases = new[]
        {
            """"
            module Basics exposing
                ( (&&)
                , (*)
                , (+)
                , (++)
                , (-)
                , (/)
                , (//)
                , (/=)
                , (<)
                , (<<)
                , (<=)
                , (<|)
                , (==)
                , (>)
                , (>=)
                , (>>)
                , (^)
                , (|>)
                , (||)
                , Bool(..)
                , Float
                , Int
                , Never
                , Order(..)
                , abs
                , acos
                , always
                , asin
                , atan
                , atan2
                , ceiling
                , clamp
                , compare
                , compareList
                , compareStrings
                , cos
                , degrees
                , e
                , floor
                , fromPolar
                , identity
                , isInfinite
                , isNaN
                , logBase
                , max
                , min
                , modBy
                , negate
                , never
                , not
                , pi
                , radians
                , remainderBy
                , round
                , sin
                , sqrt
                , tan
                , toFloat
                , toPolar
                , truncate
                , turns
                , xor
                )


            infix right 0 (<|) = apL
            infix left  0 (|>) = apR
            infix right 2 (||) = or
            infix right 3 (&&) = and
            infix non   4 (==) = eq
            infix non   4 (/=) = neq
            infix non   4 (<) = lt
            infix non   4 (>) = gt
            infix non   4 (<=) = le
            infix non   4 (>=) = ge
            infix right 5 (++) = append
            infix left  6 (+) = add
            infix left  6 (-) = sub
            infix left  7 (*) = mul
            infix left  7 (//) = idiv
            infix right 8 (^) = pow
            infix left  9 (<<) = composeL
            infix right 9 (>>) = composeR


            type Bool
                = True
                | False


            type String
                = String Int
                    -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
                | AnyOtherKind_String


            type Elm_Float
                = Elm_Float Int Int
                    -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
                | AnyOtherKind_Float


            {-| Represents the relative ordering of two things.
            The relations are less than, equal to, and greater than.
            -}
            type Order
                = LT
                | EQ
                | GT

            """",

            """"
            module Test exposing (..)

            import Browser.Dom as Dom
            import Svg.Attributes exposing (..)
            """",

            """"
            module Test exposing (..)


            decl =
                [ 1, 13, 71, 114 ]
            """",

            """"
            module Test exposing (..)


            alfa =
                13


            beta =
                71
            """",

            """"
            module Test exposing (..)


            beta =
                71


            alfa =
                13
            """",

            """"
            module Test exposing (..)


            decl =
                [ 1
                , 13
                , [ 41, 43 ]
                , 71
                , 114
                ]
            """",

            """"
            module Test exposing (..)


            decl =
                [ 1
                , 13
                , [ 41
                  , func
                        []
                        123
                  ]
                , 71
                , 114
                ]

            """",

            """"
            module Test exposing (..)


            decl =
                [ func
                    13
                    71
                ]

            """",

            """"
            module Test exposing (..)


            add x y =
                x + y

            """",


            """"
            module Test exposing (..)


            decl =
                func
                    13
                    17

            """",

            """"
            module Test exposing (..)


            decl =
                func
                    other
                    17

            """",

            """"
            module Test exposing (..)


            decl =
                func
                    []
                    17

            """",

            """"
            module Test exposing (..)


            decl =
                func
                    "test"
                    17

            """",

            """"
            module Test exposing (..)


            decl =
                {}

            """",

            """"
            module Test exposing (..)


            decl =
                { a = 13
                }

            """",

            """"
            module Test exposing (..)


            decl =
                { a = 13
                , b =
                    { a = 42
                    }
                }

            """",

            """"
            module Test exposing (..)


            decl1 =
                { a = []
                , b =
                    { a = []
                    }
                }


            decl2 =
                [ { a = [] }
                , { a =
                        [ { a = [ 1, 2, 3 ]
                          }
                        ]
                  }
                ]

            """",

            """"
            module Test exposing (..)


            decl : Int
            decl =
                71

            """",

            """"
            module Test exposing (..)


            decl : Maybe Int
            decl =
                Just
                    71

            """",

            """"
            module Test exposing (..)


            decl : Int -> Int
            decl x =
                case x of
                    0 ->
                        0

                    _ ->
                        x + 1

            """",

            """"
            module Test exposing (..)


            decl : List Int -> Int
            decl x =
                case x of
                    [] ->
                        41

                    head :: tail ->
                        [ head - 1
                        ]
                            :: tail

            """",

            """"
            module Test exposing (..)


            decl : List Int -> Int
            decl x =
                case x of
                    [] ->
                        41

                    [ single ] ->
                        71 + single

                    first :: ((second :: rest) as tail) ->
                        [ second
                        , first
                        , List.reverse
                            tail
                        , List.reverse
                            rest
                        ]

            """",

            """"
            module Test exposing (..)


            decl =
                Pine_kernel.int_add
                    [ 13
                    , Pine_kernel.int_mul
                        [ 17
                        , Pine_kernel.int_add
                            [ 21, 23 ]
                        ]
                    ]

            """",

            """"
            module Test exposing (..)


            decl =
                let
                    b =
                        13

                    a =
                        41
                in
                c
                    a
                    b

            """",

            """"
            module Test exposing (..)


            decl =
                let
                    b =
                        13

                    a =
                        41
                in
                let
                    d =
                        a
                            b
                            17
                in
                e
                    d
                    43

            """",

            """"
            module Test exposing (..)


            decl =
                let
                    ( a, b ) =
                        c
                            41
                            13
                in
                c
                    a
                    b

            """",

            """"
            module Test exposing (..)


            decl =
                let
                    ( a, b ) =
                        c
                            41
                            13
                in
                if a < b then
                    a

                else
                    b

            """",

            """"
            module Test exposing (..)


            decl =
                let
                    a : Maybe Int
                    a =
                        Just
                            5
                in
                case a of
                    Just value ->
                        value + 2

                    Nothing ->
                        0

            """",
            """"
            module Test exposing (..)


            decl : { some | a : b } -> b
            decl =
                .a

            """",

            """"
            module Test exposing (..)


            decl =
                (\param1 param2 ->
                    [ param1 + 1
                    , param2 + 3
                    ]
                )
                    4
                    5

            """",

            """"
            module Test exposing (..)


            decl =
                if a < b then
                    13

                else if a > b then
                    func
                        (a * b)

                else
                    17

            """",

            """"
            module App exposing (..)


            result =
                combine
                    (\a ->
                        Pine_kernel.int_add
                            [ a
                            , 1
                            ]
                    )
                    ((\b ->
                        Pine_kernel.int_multiply
                            [ b
                            , 2
                            ]
                     )
                        3
                    )
            """",

            """"
            module App exposing (..)


            decl : Parser InfixDirection
            decl =
                ParserFast.Bad
                    Basics.False
                    (ParserFast.ExpectingOneOf
                        firstX
                        secondX
                        []
                    )

            """",

            """"
            module App exposing (..)


            decl : Parser InfixDirection
            decl =
                let
                    (ParserFast.Parser attemptFirst) =
                        ParserFast.keyword
                            "right"
                            Infix.Right
                in
                123

            """",

            """"
            module Test exposing (..)


            decl : Float
            decl =
                3.14159

            """",

            """"
            module Test exposing (..)


            decl : Float
            decl =
                0.0000000000000001

            """",
        };

        for (var i = 0; i < testCases.Length; i++)
        {
            var testCase = testCases[i];

            try
            {
                var parsed =
                    ElmSyntaxParser.ParseModuleText(testCase.TrimStart())
                    .Extract(err => throw new System.Exception("Parsing failed: " + err.ToString()));

                var rendered = RenderDefault(parsed);

                rendered.Trim().Should().Be(testCase.Trim());
            }
            catch (System.Exception e)
            {
                throw new System.Exception(
                    $"Test case {i} failed:\n{testCase}", e);
            }
        }
    }

    [Fact]
    public void Supports_mapping_qualified_names()
    {
        var inputModuleText =
            """
            module Test exposing (..)

            a : Basics.Int
            a =
                aa


            b : String.String
            b =
                bb


            c : Char.Char
            c =
                cc

            """;

        var expectedModuleText =
            """"
            module Test exposing (..)
            
            
            a : Int
            a =
                aa


            b : String
            b =
                bb


            c : Char
            c =
                cc


            """";

        var parsed =
            ElmSyntaxParser.ParseModuleText(inputModuleText)
            .Extract(err => throw new System.Exception("Parsing failed: " + err.ToString()));

        var namesMap =
            new Dictionary<QualifiedNameRef, QualifiedNameRef>
            {
                [QualifiedNameRef.FromFullName("Basics.Int")] = QualifiedNameRef.FromFullName("Int"),
                [QualifiedNameRef.FromFullName("String.String")] = QualifiedNameRef.FromFullName("String"),
                [QualifiedNameRef.FromFullName("Char.Char")] = QualifiedNameRef.FromFullName("Char"),
            };

        var renderConfig =
            Rendering.ConfigNormalizeAllLocations(
                Rendering.LineBreakingConfig.SnapshotTestsDefault,
                mapQualifiedName: namesMap);

        var rendered =
            Rendering.ToString(
                parsed,
                renderConfig);

        rendered.Trim().Should().Be(expectedModuleText.Trim());
    }

    private static Node<T> NodeWithRangeZero<T>(T value) =>
        new(Range: s_fakeRangeZero, Value: value);

    private static readonly Location s_fakeLocationZero =
        new(Row: 0, Column: 0);

    private static readonly Range s_fakeRangeZero =
        new(Start: s_fakeLocationZero, End: s_fakeLocationZero);
}
