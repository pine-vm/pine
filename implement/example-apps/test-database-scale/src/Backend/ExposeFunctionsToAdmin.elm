module Backend.ExposeFunctionsToAdmin exposing (..)

import Backend.State
import Base64
import Dict


setStoreEntryBase64 : { entryId : Int, entryBase64 : String } -> Backend.State.State -> Backend.State.State
setStoreEntryBase64 { entryId, entryBase64 } state =
    case Base64.toBytes entryBase64 of
        Nothing ->
            state

        Just bytes ->
            let
                store =
                    state.store
                        |> Dict.insert entryId { entryBytes = bytes }
            in
            { state
                | store = store
            }
