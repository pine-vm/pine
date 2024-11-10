module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)

import Combine
import Elm.Parser.Base
import Elm.Parser.Layout as Layout
import Elm.Parser.Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)


type Mode
    = Eager
    | Lazy


typeAnnotation : Combine.Parser State (Node TypeAnnotation)
typeAnnotation =
    typeAnnotationNoFn Eager
        |> Combine.andThen
            (\typeRef ->
                Layout.optimisticLayoutWith
                    (\() -> Combine.succeed typeRef)
                    (\() ->
                        Combine.oneOf
                            [ Combine.string "->"
                                |> Combine.ignore (Combine.maybe Layout.layout)
                                |> Combine.continueWith typeAnnotation
                                |> Combine.map (\ta -> Node.combine TypeAnnotation.FunctionTypeAnnotation typeRef ta)
                            , Combine.succeed typeRef
                            ]
                    )
            )


typeAnnotationNonGreedy : Combine.Parser State (Node TypeAnnotation)
typeAnnotationNonGreedy =
    Combine.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotation Lazy
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


typeAnnotationNoFn : Mode -> Combine.Parser State (Node TypeAnnotation)
typeAnnotationNoFn mode =
    Combine.lazy
        (\() ->
            Combine.oneOf
                [ parensTypeAnnotation
                , typedTypeAnnotation mode
                , genericTypeAnnotation
                , recordTypeAnnotation
                ]
        )


parensTypeAnnotation : Combine.Parser State (Node TypeAnnotation)
parensTypeAnnotation =
    let
        commaSep : Combine.Parser State (List (Node TypeAnnotation))
        commaSep =
            Combine.many
                (Combine.string ","
                    |> Combine.ignore (Combine.maybe Layout.layout)
                    |> Combine.continueWith typeAnnotation
                    |> Combine.ignore (Combine.maybe Layout.layout)
                )

        nested : Combine.Parser State TypeAnnotation
        nested =
            Combine.succeed asTypeAnnotation
                |> Combine.ignore (Combine.maybe Layout.layout)
                |> Combine.keep typeAnnotation
                |> Combine.ignore (Combine.maybe Layout.layout)
                |> Combine.keep commaSep
    in
    Combine.string "("
        |> Combine.continueWith
            (Combine.oneOf
                [ Combine.string ")" |> Combine.map (always TypeAnnotation.Unit)
                , nested |> Combine.ignore (Combine.string ")")
                ]
            )
        |> Elm.Parser.Node.parser


asTypeAnnotation : Node TypeAnnotation -> List (Node TypeAnnotation) -> TypeAnnotation
asTypeAnnotation ((Node _ value) as x) xs =
    case xs of
        [] ->
            value

        _ ->
            TypeAnnotation.Tupled (x :: xs)


genericTypeAnnotation : Combine.Parser State (Node TypeAnnotation)
genericTypeAnnotation =
    Elm.Parser.Node.parser (Combine.map TypeAnnotation.GenericType Elm.Parser.Tokens.functionName)


recordFieldsTypeAnnotation : Combine.Parser State TypeAnnotation.RecordDefinition
recordFieldsTypeAnnotation =
    Combine.sepBy1
        (Combine.string ",")
        (Layout.maybeAroundBothSides (Elm.Parser.Node.parser recordFieldDefinition))


recordTypeAnnotation : Combine.Parser State (Node TypeAnnotation)
recordTypeAnnotation =
    Combine.string "{"
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ Combine.succeed (TypeAnnotation.Record [])
                    |> Combine.ignore (Combine.string "}")
                , Elm.Parser.Node.parser Elm.Parser.Tokens.functionName
                    |> Combine.ignore (Combine.maybe Layout.layout)
                    |> Combine.andThen
                        (\fname ->
                            Combine.oneOf
                                [ Combine.succeed (TypeAnnotation.GenericRecord fname)
                                    |> Combine.ignore (Combine.string "|")
                                    |> Combine.keep (Elm.Parser.Node.parser recordFieldsTypeAnnotation)
                                    |> Combine.ignore (Combine.string "}")
                                , Combine.succeed
                                    (\ta rest ->
                                        TypeAnnotation.Record (Node.combine Tuple.pair fname ta :: rest)
                                    )
                                    |> Combine.ignore (Combine.string ":")
                                    |> Combine.ignore (Combine.maybe Layout.layout)
                                    |> Combine.keep typeAnnotation
                                    |> Combine.ignore (Combine.maybe Layout.layout)
                                    |> Combine.keep
                                        (Combine.oneOf
                                            [ -- Skip a comma and then look for at least 1 more field
                                              Combine.string ","
                                                |> Combine.continueWith recordFieldsTypeAnnotation
                                            , -- Single field record, so just end with no additional fields
                                              Combine.succeed []
                                            ]
                                        )
                                    |> Combine.ignore (Combine.string "}")
                                ]
                        )
                ]
            )
        |> Elm.Parser.Node.parser


recordFieldDefinition : Combine.Parser State TypeAnnotation.RecordField
recordFieldDefinition =
    Combine.succeed Tuple.pair
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Tokens.functionName)
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.ignore (Combine.string ":")
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep typeAnnotation


typedTypeAnnotation : Mode -> Combine.Parser State (Node TypeAnnotation)
typedTypeAnnotation mode =
    let
        genericHelper : List (Node TypeAnnotation) -> Combine.Parser State (List (Node TypeAnnotation))
        genericHelper items =
            Combine.oneOf
                [ typeAnnotationNoFn Lazy
                    |> Combine.andThen
                        (\next ->
                            Layout.optimisticLayoutWith
                                (\() -> Combine.succeed (next :: items))
                                (\() -> genericHelper (next :: items))
                                |> Combine.ignore (Combine.maybe Layout.layout)
                        )
                , Combine.succeed items
                ]
    in
    Elm.Parser.Node.parser Elm.Parser.Base.typeIndicator
        |> Combine.andThen
            (\((Node tir _) as original) ->
                Layout.optimisticLayoutWith
                    (\() -> Combine.succeed (Node tir (TypeAnnotation.Typed original [])))
                    (\() ->
                        case mode of
                            Eager ->
                                genericHelper []
                                    |> Combine.map
                                        (\args ->
                                            let
                                                endRange : Range
                                                endRange =
                                                    case args of
                                                        (Node argRange _) :: _ ->
                                                            argRange

                                                        [] ->
                                                            tir
                                            in
                                            Node
                                                { start = tir.start, end = endRange.end }
                                                (TypeAnnotation.Typed original (List.reverse args))
                                        )

                            Lazy ->
                                Combine.succeed (Node tir (TypeAnnotation.Typed original []))
                    )
            )
