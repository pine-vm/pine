module Platform.CommandLineApp exposing (..)

{-| This module contains the types describing the command-line app configuration.
To build a command-line app in Elm, copy this module file into your project and add a declaration with the name `runRoot` using the type `CommandLineAppConfig` to an Elm module.

For the latest version of the documentation, see <https://pine-vm.org>

-}

import Bytes


{-| Use the type `CommandLineAppConfig` on a declaration named `runRoot` to declare a command-line program in an Elm module.
A command-line program can subscribe to incoming bytes from standard input and can write to standard output and standard error.
-}
type alias CommandLineAppConfig state =
    { init : InitEnvironment -> ( state, EventResponse state )
    , subscriptions : state -> Subscriptions state
    }


type alias InitEnvironment =
    { commandLine : String
    , environmentVariables : List ( String, String )
    }


type alias Subscriptions state =
    { stdIn : Maybe (Bytes.Bytes -> state -> ( state, EventResponse state ))
    , posixTimeIsPast :
        Maybe
            { minimumPosixTimeMilli : Int
            , update : { currentPosixTimeMilli : Int } -> state -> ( state, EventResponse state )
            }
    }


type alias EventResponse state =
    { commands : List (Command state)
    , exit : Maybe Int
    }


type Command state
    = SendToStdOutCmd Bytes.Bytes
    | SendToStdErrCmd Bytes.Bytes
