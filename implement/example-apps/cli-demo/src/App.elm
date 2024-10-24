module App exposing (..)

import Bytes
import Bytes.Decode
import Bytes.Encode
import Platform.CommandLineApp


type alias State =
    { completedLines : List String
    , incompleteLine : Bytes.Bytes
    }


runRoot : Platform.CommandLineApp.CommandLineAppConfig State
runRoot =
    { init = init
    , subscriptions = subscriptions
    }


init : Platform.CommandLineApp.InitEnvironment -> ( State, Platform.CommandLineApp.EventResponse State )
init environment =
    ( { completedLines = []
      , incompleteLine = Bytes.Encode.encode (Bytes.Encode.sequence [])
      }
    , { commands =
            [ "Started with the following command line: "
                ++ environment.commandLine
                ++ "\n"
                |> Bytes.Encode.string
                |> Bytes.Encode.encode
                |> Platform.CommandLineApp.SendToStdOutCmd
            ]
      , exit = Nothing
      }
    )


subscriptions : State -> Platform.CommandLineApp.Subscriptions State
subscriptions _ =
    { stdIn = Just stdIn
    , posixTimeIsPast = Nothing
    }


stdIn : Bytes.Bytes -> State -> ( State, Platform.CommandLineApp.EventResponse State )
stdIn bytes stateBefore =
    let
        ( state, response ) =
            stdInWithoutLimits bytes stateBefore

        linesReceivedCount : Int
        linesReceivedCount =
            List.length state.completedLines
    in
    case response.exit of
        Just _ ->
            ( state, response )

        Nothing ->
            if 10 < List.length state.completedLines then
                ( state
                , { commands =
                        List.concat
                            [ response.commands
                            , [ Platform.CommandLineApp.SendToStdOutCmd
                                    (Bytes.Encode.string
                                        ("Exiting after " ++ String.fromInt linesReceivedCount ++ " lines received\n")
                                        |> Bytes.Encode.encode
                                    )
                              ]
                            ]
                  , exit = Just 0
                  }
                )

            else if 100 < Bytes.width state.incompleteLine then
                ( state
                , { commands =
                        List.concat
                            [ response.commands
                            , [ String.join "\n"
                                    [ "Exiting after "
                                        ++ String.fromInt (Bytes.width state.incompleteLine)
                                        ++ " bytes received on a single line:"
                                    , Bytes.Decode.decode
                                        (Bytes.Decode.string (Bytes.width state.incompleteLine))
                                        state.incompleteLine
                                        |> Maybe.withDefault "Failed decoding"
                                    ]
                                    |> Bytes.Encode.string
                                    |> Bytes.Encode.encode
                                    |> Platform.CommandLineApp.SendToStdOutCmd
                              ]
                            ]
                  , exit = Just 0
                  }
                )

            else
                ( state, response )


stdInWithoutLimits : Bytes.Bytes -> State -> ( State, Platform.CommandLineApp.EventResponse State )
stdInWithoutLimits bytes state =
    let
        aggregateBytes : Bytes.Bytes
        aggregateBytes =
            Bytes.Encode.encode
                (Bytes.Encode.sequence
                    [ Bytes.Encode.bytes state.incompleteLine
                    , Bytes.Encode.bytes bytes
                    ]
                )
    in
    case getCompletedLines aggregateBytes of
        Err err ->
            ( state
            , { commands =
                    [ ("Error: " ++ err)
                        |> Bytes.Encode.string
                        |> Bytes.Encode.encode
                        |> Platform.CommandLineApp.SendToStdErrCmd
                    ]
              , exit = Just 1
              }
            )

        Ok ( newCompletedLines, incompleteLine ) ->
            let
                responseCmds : List (Platform.CommandLineApp.Command State)
                responseCmds =
                    List.concat
                        [ bytes
                            |> Platform.CommandLineApp.SendToStdOutCmd
                            |> List.singleton
                        , newCompletedLines
                            |> List.map
                                (\line ->
                                    String.join "\n"
                                        [ "Received line:"
                                        , line
                                        , ""
                                        ]
                                        |> Bytes.Encode.string
                                        |> Bytes.Encode.encode
                                        |> Platform.CommandLineApp.SendToStdOutCmd
                                )
                        ]
            in
            ( { state
                | completedLines =
                    List.concat
                        [ state.completedLines
                        , newCompletedLines
                        ]
                , incompleteLine = incompleteLine
              }
            , { commands = responseCmds
              , exit = Nothing
              }
            )


getCompletedLines : Bytes.Bytes -> Result String ( List String, Bytes.Bytes )
getCompletedLines bytes =
    case Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes of
        Nothing ->
            Err ("Failed decoding " ++ String.fromInt (Bytes.width bytes) ++ " bytes to string")

        Just asString ->
            case List.reverse (String.split "\n" asString) of
                remainder :: linesReversed ->
                    Ok
                        ( List.reverse linesReversed
                        , Bytes.Encode.encode (Bytes.Encode.string remainder)
                        )

                _ ->
                    Ok ( [], bytes )
