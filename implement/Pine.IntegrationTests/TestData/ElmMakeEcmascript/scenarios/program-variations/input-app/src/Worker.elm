module Worker exposing (main)

import Platform


main : Program () () Never
main =
    Platform.worker
        { init = \() -> ( (), Cmd.none )
        , update = \message _ -> never message
        , subscriptions = \_ -> Sub.none
        }
