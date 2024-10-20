module Elm.Parser.Typings exposing (typeDefinition)

import Combine exposing (Parser, many, maybe, string)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, typeName)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)


typeDefinition : Parser State (Node Declaration.Declaration)
typeDefinition =
    typePrefix
        |> Combine.andThen
            (\(Node { start } _) ->
                Combine.oneOf
                    [ Combine.succeed
                        (\name generics typeAnnotation ->
                            Node
                                { start = start, end = (Node.range typeAnnotation).end }
                                (Declaration.AliasDeclaration
                                    { documentation = Nothing
                                    , name = name
                                    , generics = generics
                                    , typeAnnotation = typeAnnotation
                                    }
                                )
                        )
                        |> Combine.ignore (string "alias")
                        |> Combine.ignore Layout.layout
                        |> Combine.keep (Elm.Parser.Node.parser typeName)
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.keep genericList
                        |> Combine.ignore (string "=")
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.keep typeAnnotation
                    , Combine.succeed
                        (\name generics constructors ->
                            let
                                end : Location
                                end =
                                    case List.head constructors of
                                        Just (Node range _) ->
                                            range.end

                                        Nothing ->
                                            start
                            in
                            Node
                                { start = start, end = end }
                                (Declaration.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = name
                                    , generics = generics
                                    , constructors = List.reverse constructors
                                    }
                                )
                        )
                        |> Combine.keep (Elm.Parser.Node.parser typeName)
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.keep genericList
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.ignore (string "=")
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.keep valueConstructors
                    ]
            )


valueConstructors : Parser State (List (Node ValueConstructor))
valueConstructors =
    Combine.sepBy1WithoutReverse
        (Combine.ignore (maybe Layout.layout) (string "|"))
        valueConstructor


valueConstructor : Parser State (Node ValueConstructor)
valueConstructor =
    Elm.Parser.Node.parser typeName
        |> Combine.andThen
            (\((Node range _) as tnn) ->
                let
                    complete : List (Node TypeAnnotation) -> Parser State (Node ValueConstructor)
                    complete args =
                        let
                            endRange : Range
                            endRange =
                                List.head args |> Maybe.map Node.range |> Maybe.withDefault range
                        in
                        Combine.succeed
                            (Node
                                { start = range.start, end = endRange.end }
                                (ValueConstructor tnn (List.reverse args))
                            )

                    argHelper : List (Node TypeAnnotation) -> Parser State (Node ValueConstructor)
                    argHelper xs =
                        Combine.oneOf
                            [ typeAnnotationNonGreedy
                                |> Combine.andThen
                                    (\ta ->
                                        Layout.optimisticLayoutWith
                                            (\() -> complete (ta :: xs))
                                            (\() -> argHelper (ta :: xs))
                                    )
                            , Combine.succeed xs
                                |> Combine.andThen complete
                            ]
                in
                Layout.optimisticLayoutWith
                    (\() -> complete [])
                    (\() -> argHelper [])
            )


genericList : Parser State (List (Node String))
genericList =
    many (Elm.Parser.Node.parser functionName |> Combine.ignore (maybe Layout.layout))


typePrefix : Parser State (Node String)
typePrefix =
    Combine.string "type"
        |> Elm.Parser.Node.parser
        |> Combine.ignore Layout.layout
