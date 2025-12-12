using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mElmSyntax7.Rendering;

using Rendering = Core.Elm.ElmSyntax.Stil4mElmSyntax7.Rendering;

public class RenderingCompleteTests
{
    private static readonly Rendering.Config s_renderingDefaultConfig =
        Rendering.ConfigPreserveLocations();

    private static string RenderDefault(File file) =>
        Rendering.ToString(
            file,
            s_renderingDefaultConfig);

    private static Range RangeFromStartAndLength(Location start, int length) =>
        new(
            Start: start,
            End: new Location(
                Row: start.Row,
                Column: start.Column + length));

    [Fact]
    public void Simple_file_canonical_format()
    {
        var moduleDefinitionNode =
            new Node<Module>(
                Value:
                new Module.NormalModule(
                    new DefaultModuleData(
                        ModuleName: new Node<IReadOnlyList<string>>(
                            Value: ["Test"],
                            Range: RangeFromStartAndLength(start: new Location(Row: 1, Column: 8), length: 4)),
                        ExposingList:
                        new Node<Exposing>(
                            Value: new Exposing.All(RangeFromStartAndLength(new Location(1, 23), length: 2)),
                            Range: RangeFromStartAndLength(start: new Location(Row: 1, Column: 13), length: 13)))),
                Range:
                RangeFromStartAndLength(new Location(1, 1), length: 25));

        var file =
            new File(
                ModuleDefinition: moduleDefinitionNode,
                Imports: [],
                Declarations: [],
                Comments: []);

        var rendered = RenderDefault(file);

        rendered.Trim().Should().Be(
            """"
            module Test exposing (..)
            """".Trim());
    }

    [Fact]
    public void Simple_file_clamping_to_min_spacing()
    {
        /*
         * If given locations are too small, the renderer should clamp them to minimum spacing that is
         * required to produce a parseable/roundtrippable output.
         * */

        var moduleDefinitionNode =
            new Node<Module>(
                Value:
                new Module.NormalModule(
                    new DefaultModuleData(
                        ModuleName: new Node<IReadOnlyList<string>>(
                            Value: ["Test"],
                            Range: RangeFromStartAndLength(start: new Location(Row: 1, Column: 1), length: 1)),
                        ExposingList:
                        new Node<Exposing>(
                            Value: new Exposing.All(RangeFromStartAndLength(new Location(1, 2), length: 2)),
                            Range: RangeFromStartAndLength(start: new Location(Row: 1, Column: 3), length: 3)))),
                Range:
                RangeFromStartAndLength(new Location(1, 1), length: 11));

        var file =
            new File(
                ModuleDefinition: moduleDefinitionNode,
                Imports: [],
                Declarations: [],
                Comments: []);

        var rendered = RenderDefault(file);

        rendered.Trim().Should().Be(
            """"
            module Test exposing (..)
            """".Trim());
    }

    [Fact]
    public void Simple_file_additional_spaces()
    {
        var moduleDefinitionNode =
            new Node<Module>(
                Value:
                new Module.NormalModule(
                    new DefaultModuleData(
                        ModuleName: new Node<IReadOnlyList<string>>(
                            Value: ["Test"],
                            Range: RangeFromStartAndLength(start: new Location(Row: 1, Column: 9), length: 4)),
                        ExposingList:
                        new Node<Exposing>(
                            Value: new Exposing.All(RangeFromStartAndLength(new Location(1, 26), length: 2)),
                            Range: RangeFromStartAndLength(start: new Location(Row: 1, Column: 16), length: 13)))),
                Range:
                RangeFromStartAndLength(new Location(1, 1), length: 28));

        var file =
            new File(
                ModuleDefinition: moduleDefinitionNode,
                Imports: [],
                Declarations: [],
                Comments: []);

        var rendered = RenderDefault(file);

        rendered.Trim().Should().Be(
            """"
            module  Test   exposing (..)
            """".Trim());
    }

    [Fact]
    public void Roundtrip_tests()
    {
        var testCases = new[]
        {
            """"
            module Test exposing (..)

            decl = 123
            """",

            """"
            module  Test   exposing (..)

            import  Dict  exposing (Dict)

            decl =
                [  13,   17,      19   ]
            """",

            """"
            module  Test   exposing (..)

            import  Dict  exposing (Dict)

            decl =
                [  13,   17
                ,  19   ]
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

            decl =
                alfa   beta    gamma
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
                    b = 13
                    a = 41
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


            decl =
                if a < b then 13 else 17

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
            module App exposing (..)


            decl =

                    -- a comment  somewhere

                71

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

            """"
            module Test exposing (..)

            decl =
                alfa
                 beta      gamma
            """",

            """"
            module Test exposing (..)


            type alias  TypeAlfa =
                {  field1 :  Int
                ,   field2 :       String
                }


            type alias  TypeBeta =   { fieldA :  Float,
                fieldB :  Bool
                }

            """",

            """"
            module Test exposing (..)


            type TypeAlfa
                = A   Int
                | B   String


            type TypeBeta = C Float | D Bool




            type Gamma a
                = E a
                | F (  Int, String )

            type Delta  varA   varB
                = G      varA
                    | H   (  varB,  Int )
                  | I

            """",

            """"
            module Test exposing (..)


            decl :  Int ->   String ->    Bool
            decl   param1   param2 =
                True


            decl_beta : {  field1 :  Int, field2 : String } ->  Float
            decl_beta  recordParam =
                    3.14
            
            decl_gamma : {  field1 :  Int, field2 : String } ->  Maybe  Int ->   String ->   TName ->  Float
            decl_gamma  recordParam   _    _ =
                7.0
            
            decl_delta :  Gen1  Int

                -> Next   String     Bool
                     -> AnotherType   Float

            decl_delta  _ =
                42
            """",

            """"
            module Test exposing (..)


            decl =  {  field1 = 123,   field2 =   "abc"         }
            """",

            """"
            module Test exposing (..)


            decl =  {  field1 = 123
                  , field2 =   "abc" }
            """",

            """"
            module Test exposing (..)


            decl r f =
                { r | field1 = 123,   field2 =   f         }
            """",

            """"
            module Test exposing (decl_beta, decl_alfa, ChoiceType   (..), RecordType)

            """",

            """"
            module Test exposing

               ( decl_beta
                , decl_alfa
                , ChoiceType   (..)
                , RecordType
                )

            """",

            """"
            module Test exposing (..)

            decl =
                alfa +    beta *   gamma
                    / delta - epsilon
            """",

            """"
            module Test exposing (..)

            decl =    alfa + beta

            decl_1 =
                gamma
                    * delta
                    + epsilon
                    - zeta

            """",
        };

        for (var i = 0; i < testCases.Length; i++)
        {
            var testCase = testCases[i];

            try
            {
                var parsed =
                    ElmSyntaxParser.ParseModuleText(testCase.TrimStart(), enableMaxPreservation: true)
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
}
