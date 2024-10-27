module Elm.Parser.Expose exposing (exposeDefinition)

import Combine exposing (Parser, maybe, oneOf, parens, sepBy1, string, succeed, while)
import Combine.Char exposing (char)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node
import Elm.Parser.Ranges as Ranges
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (exposingToken, functionName, typeName)
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node as Node exposing (Node(..))


exposeDefinition : Parser State Exposing
exposeDefinition =
    exposingToken
        |> Combine.continueWith (maybe Layout.layout)
        |> Combine.continueWith exposeListWith


exposeListWith : Parser State Exposing
exposeListWith =
    parens
        (Layout.optimisticLayout
            |> Combine.continueWith exposingListInner
            |> Combine.ignore Layout.optimisticLayout
        )


exposingListInner : Parser State Exposing
exposingListInner =
    Combine.oneOf
        [ Ranges.withRange (succeed All |> Combine.ignore (Layout.maybeAroundBothSides (string "..")))
        , Combine.map Explicit (sepBy1 (char ',') (Layout.maybeAroundBothSides exposable))
        ]


exposable : Parser State (Node TopLevelExpose)
exposable =
    oneOf
        [ typeExpose
        , infixExpose
        , functionExpose
        ]


infixExpose : Parser State (Node TopLevelExpose)
infixExpose =
    Elm.Parser.Node.parser (Combine.map InfixExpose (parens (while ((/=) ')'))))


typeExpose : Parser State (Node TopLevelExpose)
typeExpose =
    Elm.Parser.Node.parser typeName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen
            (\((Node typeRange typeValue) as tipe) ->
                Combine.oneOf
                    [ Elm.Parser.Node.parser (parens (Layout.maybeAroundBothSides (string "..")))
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
    Elm.Parser.Node.parser (Combine.map FunctionExpose functionName)
