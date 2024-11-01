module Elm.Parser.Typings exposing (typeDefinition)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens
import Elm.Parser.TypeAnnotation
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)


typeDefinition : Parser State (Node Declaration.Declaration)
typeDefinition =
    typePrefix
        |> Combine.andThen
            (\(Node nodeRange _) ->
                Combine.oneOf
                    [ Combine.succeed
                        (\name generics typeAnnotation ->
                            let
                                (Node.Node typeAnnotationRange _) =
                                    typeAnnotation
                            in
                            Node
                                { start = nodeRange.start, end = typeAnnotationRange.end }
                                (Declaration.AliasDeclaration
                                    { documentation = Nothing
                                    , name = name
                                    , generics = generics
                                    , typeAnnotation = typeAnnotation
                                    }
                                )
                        )
                        |> Combine.ignore (Combine.string "alias")
                        |> Combine.ignore Layout.layout
                        |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Tokens.typeName)
                        |> Combine.ignore (Combine.maybe Layout.layout)
                        |> Combine.ignore (Combine.maybe Layout.layout)
                        |> Combine.keep genericList
                        |> Combine.ignore (Combine.string "=")
                        |> Combine.ignore (Combine.maybe Layout.layout)
                        |> Combine.keep Elm.Parser.TypeAnnotation.typeAnnotation
                    , Combine.succeed
                        (\name generics constructors ->
                            let
                                end : Location
                                end =
                                    case List.head constructors of
                                        Just (Node range _) ->
                                            range.end

                                        Nothing ->
                                            nodeRange.start
                            in
                            Node
                                { start = nodeRange.start, end = end }
                                (Declaration.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = name
                                    , generics = generics
                                    , constructors = List.reverse constructors
                                    }
                                )
                        )
                        |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Tokens.typeName)
                        |> Combine.ignore (Combine.maybe Layout.layout)
                        |> Combine.keep genericList
                        |> Combine.ignore (Combine.maybe Layout.layout)
                        |> Combine.ignore (Combine.string "=")
                        |> Combine.ignore (Combine.maybe Layout.layout)
                        |> Combine.keep valueConstructors
                    ]
            )


valueConstructors : Parser State (List (Node ValueConstructor))
valueConstructors =
    Combine.sepBy1WithoutReverse
        (Combine.ignore (Combine.maybe Layout.layout) (Combine.string "|"))
        valueConstructor


valueConstructor : Parser State (Node ValueConstructor)
valueConstructor =
    Elm.Parser.Node.parser Elm.Parser.Tokens.typeName
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
                            [ Elm.Parser.TypeAnnotation.typeAnnotationNonGreedy
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
    Combine.many
        (Elm.Parser.Node.parser Elm.Parser.Tokens.functionName
            |> Combine.ignore (Combine.maybe Layout.layout)
        )


typePrefix : Parser State (Node String)
typePrefix =
    Combine.string "type"
        |> Elm.Parser.Node.parser
        |> Combine.ignore Layout.layout
