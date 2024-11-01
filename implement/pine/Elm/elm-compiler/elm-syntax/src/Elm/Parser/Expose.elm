module Elm.Parser.Expose exposing (exposeDefinition)

import Combine exposing (Parser)
import Combine.Char
import Elm.Parser.Layout as Layout
import Elm.Parser.Node
import Elm.Parser.Ranges as Ranges
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node as Node exposing (Node(..))


exposeDefinition : Parser State Exposing
exposeDefinition =
    Elm.Parser.Tokens.exposingToken
        |> Combine.continueWith (Combine.maybe Layout.layout)
        |> Combine.continueWith exposeListWith


exposeListWith : Parser State Exposing
exposeListWith =
    Combine.parens
        (Layout.optimisticLayout
            |> Combine.continueWith exposingListInner
            |> Combine.ignore Layout.optimisticLayout
        )


exposingListInner : Parser State Exposing
exposingListInner =
    Combine.oneOf
        [ Ranges.withRange (Combine.succeed All |> Combine.ignore (Layout.maybeAroundBothSides (Combine.string "..")))
        , Combine.map Explicit (Combine.sepBy1 (Combine.Char.char ',') (Layout.maybeAroundBothSides exposable))
        ]


exposable : Parser State (Node TopLevelExpose)
exposable =
    Combine.oneOf
        [ typeExpose
        , infixExpose
        , functionExpose
        ]


infixExpose : Parser State (Node TopLevelExpose)
infixExpose =
    Elm.Parser.Node.parser (Combine.map InfixExpose (Combine.parens (Combine.while ((/=) ')'))))


typeExpose : Parser State (Node TopLevelExpose)
typeExpose =
    Elm.Parser.Node.parser Elm.Parser.Tokens.typeName
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.andThen
            (\((Node typeRange typeValue) as tipe) ->
                Combine.oneOf
                    [ Elm.Parser.Node.parser (Combine.parens (Layout.maybeAroundBothSides (Combine.string "..")))
                        |> Combine.map
                            (\(Node openRange _) ->
                                Node
                                    { start = typeRange.start, end = openRange.end }
                                    (TypeExpose (ExposedType typeValue (Just openRange)))
                            )
                    , Combine.succeed (Node.map TypeOrAliasExpose tipe)
                    ]
            )


functionExpose : Parser State (Node TopLevelExpose)
functionExpose =
    Elm.Parser.Node.parser (Combine.map FunctionExpose Elm.Parser.Tokens.functionName)
