using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mElmSyntax7;

public class RenderingTests
{
    private static readonly Core.Elm.ElmSyntax.Stil4mElmSyntax7.Rendering.Config s_renderingDefaultConfig =
        Core.Elm.ElmSyntax.Stil4mElmSyntax7.Rendering.ConfigNormalizeAllLocations(
            Core.Elm.ElmSyntax.Stil4mElmSyntax7.Rendering.LineBreakingConfig.SnapshotTestsDefault);

    private static string RenderDefault(Core.Elm.ElmSyntax.Stil4mElmSyntax7.File file) =>
        Core.Elm.ElmSyntax.Stil4mElmSyntax7.Rendering.ToString(
            file,
            s_renderingDefaultConfig);

    [Fact]
    public void ToString_EmptyFile()
    {
        var file =
            new Core.Elm.ElmSyntax.Stil4mElmSyntax7.File(
                ModuleDefinition:
                NodeWithRangeZero<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Module>(
                    new Core.Elm.ElmSyntax.Stil4mElmSyntax7.Module.NormalModule(
                        ModuleData: new Core.Elm.ElmSyntax.Stil4mElmSyntax7.DefaultModuleData(
                            ModuleName: NodeWithRangeZero((IReadOnlyList<string>)["Test"]),
                            ExposingList: NodeWithRangeZero<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Exposing>(
                                new Core.Elm.ElmSyntax.Stil4mElmSyntax7.Exposing.All(s_fakeRangeZero))
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
                    text "Hello, World!"


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
                [ func 13 71
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
                func 13 17

            """",

            """"
            module Test exposing (..)


            decl =
                func other 17

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
                Just 71

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
                        , List.reverse tail
                        , List.reverse rest
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

    private static Core.Elm.ElmSyntax.Stil4mElmSyntax7.Node<T> NodeWithRangeZero<T>(T value) =>
        new(Range: s_fakeRangeZero, Value: value);

    private static readonly Core.Elm.ElmSyntax.Stil4mElmSyntax7.Location s_fakeLocationZero =
        new(Row: 0, Column: 0);

    private static readonly Core.Elm.ElmSyntax.Stil4mElmSyntax7.Range s_fakeRangeZero =
        new(Start: s_fakeLocationZero, End: s_fakeLocationZero);
}
