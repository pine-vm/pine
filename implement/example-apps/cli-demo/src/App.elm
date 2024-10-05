module App exposing (..)

import Bytes
import Bytes.Encode
import Platform.CommandLineApp


type alias State =
    { inputCount : Int
    }


runRoot : Platform.CommandLineApp.CommandLineAppConfig State
runRoot =
    { init = init
    , subscriptions = subscriptions
    }


init : Platform.CommandLineApp.InitEnvironment -> ( State, Platform.CommandLineApp.EventResponse State )
init environment =
    ( { inputCount = 0
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
stdIn bytes state =
    let
        exit : Maybe Int
        exit =
            if state.inputCount < 10 then
                Nothing

            else
                Just 0

        exitResponseCmds : List (Platform.CommandLineApp.Command State)
        exitResponseCmds =
            case exit of
                Just _ ->
                    [ Platform.CommandLineApp.SendToStdOutCmd
                        (Bytes.Encode.string
                            ("Exiting after " ++ String.fromInt state.inputCount ++ " inputs\n")
                            |> Bytes.Encode.encode
                        )
                    ]

                Nothing ->
                    []

        response : Bytes.Bytes
        response =
            Bytes.Encode.sequence
                [ Bytes.Encode.string "Received: "
                , Bytes.Encode.bytes bytes
                , Bytes.Encode.string "\n"
                ]
                |> Bytes.Encode.encode
    in
    ( { state
        | inputCount = state.inputCount + 1
      }
    , { commands =
            List.concat
                [ [ Platform.CommandLineApp.SendToStdOutCmd response
                  ]
                , exitResponseCmds
                ]
      , exit = exit
      }
    )
