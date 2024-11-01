module Elm.Parser.Layout exposing (LayoutStatus(..), layout, layoutStrict, maybeAroundBothSides, optimisticLayout, optimisticLayoutWith)

import Combine exposing (Parser)
import Elm.Parser.Comments as Comments
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Whitespace


anyComment : Combine.Parser State ()
anyComment =
    Combine.oneOf
        [ Comments.singleLineComment
        , Comments.multilineComment
        ]


layout : Parser State ()
layout =
    Combine.many1
        (Combine.oneOf
            [ anyComment
            , Combine.many1 Elm.Parser.Whitespace.realNewLine
                |> Combine.continueWith
                    (Combine.oneOf
                        [ Elm.Parser.Whitespace.many1Spaces
                        , anyComment
                        ]
                    )
            , Elm.Parser.Whitespace.many1Spaces
            ]
        )
        |> Combine.continueWith (verifyIndent (\stateIndent current -> stateIndent < current))


type LayoutStatus
    = Strict
    | Indented


optimisticLayoutWith : (() -> Parser State a) -> (() -> Parser State a) -> Parser State a
optimisticLayoutWith onStrict onIndented =
    optimisticLayout
        |> Combine.andThen
            (\ind ->
                case ind of
                    Strict ->
                        onStrict ()

                    Indented ->
                        onIndented ()
            )


optimisticLayout : Parser State LayoutStatus
optimisticLayout =
    Combine.many
        (Combine.oneOf
            [ anyComment
            , Combine.many1 Elm.Parser.Whitespace.realNewLine
                |> Combine.continueWith
                    (Combine.oneOf
                        [ Elm.Parser.Whitespace.many1Spaces
                        , anyComment
                        , Combine.succeed ()
                        ]
                    )
            , Elm.Parser.Whitespace.many1Spaces
            ]
        )
        |> Combine.continueWith compute


compute : Parser State LayoutStatus
compute =
    Combine.withState
        (\state ->
            Combine.withLocation
                (\l ->
                    if l.column == 1 || List.member (l.column - 1) (State.storedColumns state) then
                        Combine.succeed Strict

                    else
                        Combine.succeed Indented
                )
        )


layoutStrict : Parser State ()
layoutStrict =
    Combine.many1
        (Combine.oneOf
            [ anyComment
            , Combine.many1 Elm.Parser.Whitespace.realNewLine
                |> Combine.continueWith (Combine.succeed ())
            , Elm.Parser.Whitespace.many1Spaces
            ]
        )
        |> Combine.continueWith (verifyIndent (\stateIndent current -> stateIndent == current))


verifyIndent : (Int -> Int -> Bool) -> Parser State ()
verifyIndent f =
    Combine.withState
        (\s ->
            Combine.withLocation
                (\l ->
                    if f (State.expectedColumn s) l.column then
                        Combine.succeed ()

                    else
                        Combine.fail ("Expected higher indent than " ++ String.fromInt l.column)
                )
        )


maybeAroundBothSides : Parser State b -> Parser State b
maybeAroundBothSides x =
    Combine.maybe layout
        |> Combine.continueWith x
        |> Combine.ignore (Combine.maybe layout)
